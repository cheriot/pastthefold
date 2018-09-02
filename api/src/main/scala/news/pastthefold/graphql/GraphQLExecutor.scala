package news.pastthefold.graphql

import cats._
import cats.data._
import cats.implicits._
import cats.effect._
import io.circe.Json
import io.circe.generic.auto._
import io.circe.jawn._
import io.circe.optics.JsonPath._
import io.circe.syntax._
import news.pastthefold.model.User
import org.http4s._
import org.http4s.circe._
import org.http4s.dsl.io._
import sangria.ast.Document
import sangria.execution.{ErrorWithResolver, QueryAnalysisError, _}
import sangria.marshalling.circe._
import sangria.parser.{QueryParser, SyntaxError}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.control.NonFatal
import scala.util.{Failure, Success}

class GraphQLExecutor {

  def httpGraphQL(query: String, userOpt: Option[User]): IO[Response[IO]] = {
    // This is actually not going to be json. runGraphQL will need to accept a query: String
    parse(query) match {
      case Right(json: Json) => httpGraphQL(json, userOpt)
      case Left(parsingFailure) => BadRequest(formatError(s"Error json parsing query. ${parsingFailure.message}"))
    }
  }

  def httpGraphQL(body: Json, userOpt: Option[User]): IO[Response[IO]] = {
    val result = IO {
      runGraphQL(body)
        .map(Ok(_))
        .recover {
          case error: QueryAnalysisError ⇒ BadRequest(error.resolveError)
          case error: ErrorWithResolver ⇒ InternalServerError(error.resolveError)
          case e: ParseFailure => BadRequest(formatError(s"Error json parsing query. ${e.message}"))
          case e: Throwable => BadRequest(formatError(e))
        }
    }

    IO.fromFuture(result).flatMap(a => a)
  }

  def runGraphQL(body: Json): Future[Json] = {
    val query = root.query.string.getOption(body)
    val operationName = root.operationName.string.getOption(body)
    val variablesJson = root.variables.string.getOption(body)

    query.map(QueryParser.parse(_)) match {
      case Some(Success(ast)) ⇒
        variablesJson.map(parse) match {
          case Some(Left(error)) ⇒ Future.failed(error)
          case Some(Right(json)) ⇒ executeGraphQL(ast, operationName, json)
          case None ⇒ executeGraphQL(ast, operationName, root.variables.json.getOption(body) getOrElse Json.obj())
        }
      case Some(Failure(error)) ⇒ Future.failed(error)
      case None ⇒ Future.failed(NoQueryException)
    }
  }

  object NoQueryException extends Throwable("No query to execute.")

  sealed trait ErrorMessage
  case class ThrowableLocation(line: Int, column: Int)
  case class ThrowableMessage(message: String, locations: Seq[ThrowableLocation]) extends ErrorMessage
  case class StringMessage(message: String) extends ErrorMessage
  case class ErrorResponse(errors: Seq[ErrorMessage])
  object ErrorResponse {
    def oneMessage(errorMessage: ErrorMessage): Json = ErrorResponse(errors = Seq(errorMessage)).asJson
  }

  private def formatError(error: Throwable): Json = error match {
    case syntaxError: SyntaxError => ErrorResponse.oneMessage(
      ThrowableMessage(
        message = syntaxError.getMessage(),
        locations = Seq[ThrowableLocation](ThrowableLocation(syntaxError.originalError.position.line, syntaxError.originalError.position.column))
      )
    )
    case NonFatal(e) => formatError(e.getMessage)
    case e => throw e
  }

  private def formatError(message: String): Json =
    ErrorResponse.oneMessage(StringMessage(message))

  private def executeGraphQL(query: Document, operationName: Option[String], variables: Json): Future[Json] =
    Executor.execute(
      SchemaDefinition.storylineSchemaDefinition.schema,
      query,
      QueryContext.buildContext,
      variables = if (variables.isNull) Json.obj() else variables,
      operationName = operationName,
      exceptionHandler = exceptionHandler,
      deferredResolver = SchemaDefinition.storylineSchemaDefinition.resolver)

  private val exceptionHandler = ExceptionHandler {
    case (_, e) ⇒ HandledException(e.getMessage)
  }
}

object GraphQLExecutor {
  def apply() =
    new GraphQLExecutor()
}

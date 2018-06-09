package news.pastthefold

import cats.effect._
import io.circe.Json
import io.circe.generic.auto._
import io.circe.jawn._
import io.circe.optics.JsonPath._
import io.circe.syntax._
import org.http4s._
import org.http4s.circe._
import org.http4s.dsl.io._
import sangria.ast.Document
import sangria.execution.deferred.DeferredResolver
import sangria.execution.{ErrorWithResolver, QueryAnalysisError, _}
import sangria.marshalling.circe._
import sangria.parser.{QueryParser, SyntaxError}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.control.NonFatal
import scala.util.{Failure, Success}

object GraphQLExecutor {

  def runGraphQL(queryStr: String): Future[IO[Response[IO]]] =
    parse(queryStr) match {
      case Right(json: Json) => runGraphQL(json)
      case Left(parsingFailure) => Future.successful(BadRequest(s"Error json parsing query. ${parsingFailure.message}"))
    }

  def runGraphQL(body: Json): Future[IO[Response[IO]]] = {
    val query = root.query.string.getOption(body)
    val operationName = root.operationName.string.getOption(body)
    val variablesJson = root.variables.string.getOption(body)

    query.map(QueryParser.parse(_)) match {
      case Some(Success(ast)) ⇒
        variablesJson.map(parse) match {
          case Some(Left(error)) ⇒ Future.successful(BadRequest(formatError(error)))
          case Some(Right(json)) ⇒ runGraphQL(ast, operationName, json)
          case None ⇒ runGraphQL(ast, operationName, root.variables.json.getOption(body) getOrElse Json.obj())
        }
      case Some(Failure(error)) ⇒ Future.successful(BadRequest(formatError(error)))
      case None ⇒ Future.successful(BadRequest(formatError("No query to execute")))
    }
  }

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

  private def runGraphQL(query: Document, operationName: Option[String], variables: Json) =
    executeGraphQL(query, operationName, variables)
      .map(Ok(_))
      .recover {
        case error: QueryAnalysisError ⇒ BadRequest(error.resolveError)
        case error: ErrorWithResolver ⇒ InternalServerError(error.resolveError)
      }

  private def executeGraphQL(query: Document, operationName: Option[String], variables: Json) =
    Executor.execute(
      SchemaDefinition.StarWarsSchema,
      query,
      new CharacterRepo,
      variables = if (variables.isNull) Json.obj() else variables,
      operationName = operationName,
      exceptionHandler = exceptionHandler,
      deferredResolver = DeferredResolver.fetchers(SchemaDefinition.characters))

  // private def executeAndPrintGraphQL(query: String) =
  //   QueryParser.parse(query) match {
  //     case Success(doc) ⇒
  //       println(Await.result(executeGraphQL(doc, None, Json.obj()), 10 seconds).spaces2)
  //     case Failure(error) ⇒
  //       Console.err.print(error.getMessage())
  //   }

  private val exceptionHandler = ExceptionHandler {
    case (_, e) ⇒ HandledException(e.getMessage)
  }
}

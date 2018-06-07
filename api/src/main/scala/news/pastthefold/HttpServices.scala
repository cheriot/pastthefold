package news.pastthefold

import cats.effect._
import org.http4s._
import io.circe._
import org.http4s.circe._
import org.http4s.dsl.io._
import sangria.ast.Document
import sangria.execution.{ErrorWithResolver, QueryAnalysisError}
import sangria.parser.{QueryParser, SyntaxError}
import sangria.marshalling.circe._
import cats.effect._
import cats.implicits._
import io.circe.Json
import io.circe.jawn._
import org.http4s._
import org.http4s.dsl.io._
import org.http4s.server.blaze._
import org.http4s.circe._
import io.circe.optics.JsonPath._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.control.NonFatal
import sangria.ast.Document
import sangria.execution.{ErrorWithResolver, QueryAnalysisError}
import sangria.parser.{QueryParser, SyntaxError}
import sangria.marshalling.circe._

import scala.util.{Failure, Success}

object HttpServices {
  val helloWorldService = HttpService[IO] {
    case GET -> Root / "hello" / name =>
      Ok(s"Hello, $name.")
  }

  val readerService = HttpService[IO] {
    // Articles's subscribe button.
    case req @ POST -> Root / "subscription" / articleId => ???
    // Unsubscribe links in email
    case req @ DELETE -> Root / "subscription" / articleId => ???
  }

  val newsRoomService = HttpService[IO] {
    case request @ POST -> Root  =>
      request.as[Json].flatMap { body =>
        val query = root.query.string.getOption(body)
        val operationName = root.operationName.string.getOption(body)
        val variablesStr = root.variables.string.getOption(body)

        def execute = query.map(QueryParser.parse(_)) match {
          case Some(Success(ast)) ⇒
            variablesStr.map(parse) match {
              case Some(Left(error)) ⇒ Future.successful(BadRequest(formatError(error)))
              case Some(Right(json)) ⇒ executeGraphQL(ast, operationName, json)
              case None ⇒ executeGraphQL(ast, operationName, root.variables.json.getOption(body) getOrElse Json.obj())
            }
          case Some(Failure(error)) ⇒ Future.successful(BadRequest(formatError(error)))
          case None ⇒ Future.successful(BadRequest(formatError("No query to execute")))
        }

        IO.fromFuture(IO(execute)).flatten
      }
  }

  def formatError(error: Throwable): Json = error match {
    case syntaxError: SyntaxError ⇒
      Json.obj("errors" → Json.arr(
        Json.obj(
          "message" → Json.fromString(syntaxError.getMessage),
          "locations" → Json.arr(Json.obj(
            "line" → Json.fromBigInt(syntaxError.originalError.position.line),
            "column" → Json.fromBigInt(syntaxError.originalError.position.column))))))
    case NonFatal(e) ⇒
      formatError(e.getMessage)
    case e ⇒
      throw e
  }

  def formatError(message: String): Json =
    Json.obj("errors" → Json.arr(Json.obj("message" → Json.fromString(message))))

  def executeGraphQL(query: Document, operationName: Option[String], variables: Json) =
    GraphQLUtil.executeGraphQL(query, operationName, variables)
      .map(Ok(_))
      .recover {
        case error: QueryAnalysisError ⇒ BadRequest(error.resolveError)
        case error: ErrorWithResolver ⇒ InternalServerError(error.resolveError)
      }

  object GraphQLUtil {
    import language.postfixOps
    import io.circe.Json
    import sangria.ast.Document
    import sangria.execution._
    import sangria.execution.deferred.DeferredResolver
    import sangria.parser.QueryParser
    import sangria.marshalling.circe._

    import scala.concurrent.ExecutionContext.Implicits.global
    import scala.concurrent.Await
    import scala.concurrent.duration._
    import scala.util.{Failure, Success}

    def executeGraphQL(query: Document, operationName: Option[String], variables: Json) =
      Executor.execute(SchemaDefinition.StarWarsSchema, query, new CharacterRepo,
        variables = if (variables.isNull) Json.obj() else variables,
        operationName = operationName,
        exceptionHandler = exceptionHandler,
        deferredResolver = DeferredResolver.fetchers(SchemaDefinition.characters))

    def executeAndPrintGraphQL(query: String) =
      QueryParser.parse(query) match {
        case Success(doc) ⇒
          println(Await.result(executeGraphQL(doc, None, Json.obj()), 10 seconds).spaces2)
        case Failure(error) ⇒
          Console.err.print(error.getMessage())
      }

    val exceptionHandler = ExceptionHandler {
      case (_, e) ⇒ HandledException(e.getMessage)
    }
  }

}

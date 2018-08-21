package news.pastthefold.http

import cats.effect._
import cats.implicits._
import io.circe.Json
import news.pastthefold.graphql.GraphQLExecutor
import org.http4s._
import org.http4s.circe._
import org.http4s.dsl.io._

object HttpServices {
  val helloWorldHttpService: HttpRoutes[IO] = HttpRoutes.of[IO] {
    case GET -> Root / "hello" / name =>
      Ok(s"Hello, $name.")
  }

  // val readerHttpService = HttpService[IO] {
  //   // Articles's subscribe button.
  //   case req @ POST -> Root / "subscription" / articleId => ???
  //   // Unsubscribe links in email
  //   case req @ DELETE -> Root / "subscription" / articleId => ???
  // }

  import org.http4s.headers.`Content-Type`
  object QueryParamMatcher extends QueryParamDecoderMatcher[String]("query")

  def contentType(request: Request[IO], str: String): Boolean =
    request.headers
      .get(`Content-Type`)
      .map(_.value)
      .filter(_ == str)
      .isDefined

  /** Extend to fully implement https://graphql.org/learn/serving-over-http/#http-methods-headers-and-body */
  val graphQLHttpService: HttpRoutes[IO] = HttpRoutes.of[IO] {

    case GET -> Root / "graphql" :? QueryParamMatcher(query) =>
      GraphQLExecutor.httpGraphQL(query)

    case request @ POST -> Root / "graphql" if contentType(request, "application/json") =>
      request.as[Json].flatMap { body =>
        GraphQLExecutor.httpGraphQL(body)
      }

    case request @ GET -> Root / "explore" =>
      StaticFile.fromResource("/graphiql.html", Some(request))
        .getOrElseF(NotFound("Can't find the graphiql html."))

    case (GET | POST) -> Root / "graphql" =>
      BadRequest("Invalid GraphQL query.")
  }

  val allHttpServices = helloWorldHttpService combineK graphQLHttpService

}

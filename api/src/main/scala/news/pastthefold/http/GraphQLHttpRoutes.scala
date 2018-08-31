package news.pastthefold.http

import cats.effect._
import io.circe.Json
import news.pastthefold.graphql.GraphQLExecutor
import org.http4s.{HttpService, Request, StaticFile}
import org.http4s.circe._
import org.http4s.dsl.Http4sDsl

class GraphQLHttpRoutes extends Http4sDsl[IO] {
  def helloWorldHttpService = HttpService[IO] {
    case GET -> Root / "hello" / name =>
      Ok(s"Hello, $name.")
  }

  import org.http4s.headers.`Content-Type`
  object QueryParamMatcher extends QueryParamDecoderMatcher[String]("query")

  def contentType(request: Request[IO], str: String): Boolean =
    request.headers
      .get(`Content-Type`)
      .map(_.value)
      .filter(_ == str)
      .isDefined

  /** Extend to fully implement https://graphql.org/learn/serving-over-http/#http-methods-headers-and-body */
  def endpoints(graphQLExecutor: GraphQLExecutor) = HttpService[IO] {
    case GET -> Root / "graphql" :? QueryParamMatcher(query) =>
      graphQLExecutor.httpGraphQL(query)

    case request @ POST -> Root / "graphql" if contentType(request, "application/json") =>
      request.as[Json].flatMap { body =>
        graphQLExecutor.httpGraphQL(body)
      }

    case request @ GET -> Root / "explore" =>
      StaticFile.fromResource("/graphiql.html", Some(request))
        .getOrElseF(NotFound("Can't find the graphiql html."))

    case (GET | POST) -> Root / "graphql" =>
      BadRequest("Invalid GraphQL query.")
  }

}

object GraphQLHttpRoutes {
  def endpoints(graphQLExecutor: GraphQLExecutor): HttpService[IO] =
    new GraphQLHttpRoutes().endpoints(graphQLExecutor)
}

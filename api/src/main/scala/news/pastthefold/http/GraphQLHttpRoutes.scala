package news.pastthefold.http

import cats.effect._
import io.circe.Json
import news.pastthefold.auth.SecureRequestService
import news.pastthefold.auth.SecureRequestService.AuthService
import news.pastthefold.graphql.GraphQLExecutor
import org.http4s.circe._
import org.http4s.dsl.Http4sDsl
import org.http4s.{HttpService, Request, StaticFile}
import tsec.authentication._

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

  // if contentType(request, "application/json")

  /** Extend to fully implement https://graphql.org/learn/serving-over-http/#http-methods-headers-and-body */
  def endpoints(graphQLExecutor: GraphQLExecutor): AuthService[IO] = TSecAuthService {
    case GET -> Root / "graphql" :? QueryParamMatcher(query) asAuthed user =>
      graphQLExecutor.httpGraphQL(query, Some(user))

    case authReq@POST -> Root / "graphql" asAuthed user =>
      authReq.request.as[Json].flatMap { body =>
        graphQLExecutor.httpGraphQL(body, Some(user))
      }

    case authReq@GET -> Root / "explore" asAuthed _ =>
      StaticFile.fromResource("/graphiql.html", Some(authReq.request))
        .getOrElseF(NotFound("Can't find the graphiql html."))

    case (GET | POST) -> Root / "graphql" asAuthed _ =>
      BadRequest("Invalid GraphQL query.")
  }

}

object GraphQLHttpRoutes {
  def endpoints(
                 graphQLExecutor: GraphQLExecutor,
                 secureRequestService: SecureRequestService[IO]
               ): HttpService[IO] =
    secureRequestService.liftService(
      new GraphQLHttpRoutes().endpoints(graphQLExecutor)
    )
}

package news.pastthefold

import java.util.UUID

import cats._
import cats.implicits._
import cats.effect._
import fs2.StreamApp.ExitCode
import fs2.{Stream, StreamApp}
import news.pastthefold.auth.{PasswordAuthService, PasswordHashingService, SecureRequestService}
import news.pastthefold.dao.MemoryUserAuthDAO
import news.pastthefold.graphql.GraphQLExecutor
import news.pastthefold.http.{AuthHttpRoutes, GraphQLHttpRoutes}
import org.http4s.HttpService
import org.http4s.server.blaze._
import tsec.authentication.{AuthenticatedCookie, BackingStore}
import tsec.mac.jca.HMACSHA256

import scala.concurrent.ExecutionContext.Implicits.global


object Main extends StreamApp[IO] {

  override def stream(args: List[String], requestShutdown: IO[Unit]): Stream[IO, ExitCode] = {
    // TODO this needs to be the same key in every instance.
    val key = HMACSHA256.generateKey[Id]
    // TODO non-memory
    val userAuthDAO = new MemoryUserAuthDAO[IO]
    // TODO memory
    val cookieBackingStore: BackingStore[IO, UUID, AuthenticatedCookie[HMACSHA256, Int]] = ???
    val passwordHashingService = PasswordHashingService[IO]
    val secureRequestService = SecureRequestService[IO](
      userAuthDAO,
      cookieBackingStore,
      key
    )
    val passwordAuthService = PasswordAuthService(
      userAuthDAO,
      passwordHashingService,
      secureRequestService
    )
    val authHttpEndpoints = AuthHttpRoutes.endpoints(passwordAuthService)
    val graphQLExecutor = GraphQLExecutor()
    val graphQLHttpEndpoints = GraphQLHttpRoutes.endpoints(graphQLExecutor)
    for {
      exitCode <- createStream[IO](authHttpEndpoints combineK graphQLHttpEndpoints)
    } yield exitCode
  }

  def createStream[F[_]: Effect](httpEndpoints: HttpService[F]): Stream[F, ExitCode] =
    BlazeBuilder[F]
      .bindHttp(9090, "localhost")
      .mountService(httpEndpoints, "/")
      .serve
}
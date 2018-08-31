package news.pastthefold

import java.util.UUID

import cats._
import cats.implicits._
import cats.effect._
import news.pastthefold.auth.{PasswordAuthService, PasswordHashingService, SecureRequestService}
import news.pastthefold.dao.MemoryUserAuthDAO
import news.pastthefold.graphql.GraphQLExecutor
import news.pastthefold.http.{AuthHttpRoutes, GraphQLHttpRoutes}
import tsec.authentication.{AuthenticatedCookie, BackingStore}
import tsec.mac.jca.HMACSHA256
import org.http4s.HttpService

class Registry {

  val allHttpEndpoints: HttpService[IO] = {
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
    authHttpEndpoints combineK graphQLHttpEndpoints
  }
}

object Registry {
  def apply() =
    new Registry
}

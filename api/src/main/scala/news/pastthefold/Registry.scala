package news.pastthefold

import cats.implicits._
import cats.effect._
import news.pastthefold.auth.{PasswordAuthService, PasswordHashingService, SecureRequestService}
import news.pastthefold.dao.{MemoryUserAuthDAO, SessionDAO}
import news.pastthefold.graphql.GraphQLExecutor
import news.pastthefold.http.{AuthHttpRoutes, GraphQLHttpRoutes}
import org.http4s.HttpService

class Registry {

  val allHttpEndpoints: HttpService[IO] = {
    val key = Config.cookieSigningKey
    // TODO non-memory
    val userAuthDAO = new MemoryUserAuthDAO[IO]
    // TODO non-memory
    val cookieBackingStore = SessionDAO[IO]()
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
    val authHttpEndpoints = AuthHttpRoutes.endpoints(passwordAuthService, secureRequestService)
    val graphQLExecutor = GraphQLExecutor()
    val graphQLHttpEndpoints = GraphQLHttpRoutes.endpoints(graphQLExecutor, secureRequestService)
    authHttpEndpoints combineK graphQLHttpEndpoints
  }
}

object Registry {
  def apply() =
    new Registry
}

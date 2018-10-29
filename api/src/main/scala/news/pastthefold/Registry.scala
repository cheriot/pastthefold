package news.pastthefold

import cats.implicits._
//import org.http4s.implicits._
import cats.effect._
import news.pastthefold.auth.{PasswordAuthService, PasswordHashingService, SecureRequestService}
import news.pastthefold.dao.{MemoryUserAuthDAO, SessionDAO}
import news.pastthefold.graphql.GraphQLExecutor
import news.pastthefold.http.{AuthHttpRoutes, GraphQLHttpRoutes}
import org.http4s.HttpService

class Registry {

  // TODO non-memory
  private lazy val userAuthDAO = new MemoryUserAuthDAO[IO]

  lazy val secureRequestService = {
    val key = Config.cookieSigningKey
    // TODO non-memory
    val cookieBackingStore = SessionDAO[IO]()
    SecureRequestService[IO](
      userAuthDAO,
      cookieBackingStore,
      key
    )
  }

  lazy val graphqlEndpoints: HttpService[IO] = {
    val graphQLExecutor = GraphQLExecutor()
    GraphQLHttpRoutes.endpoints(graphQLExecutor, secureRequestService)
  }

  lazy val authEndpoints: HttpService[IO] = {
    val passwordHashingService = PasswordHashingService[IO]
    val passwordAuthService = PasswordAuthService(
      userAuthDAO,
      passwordHashingService,
      secureRequestService
    )
    AuthHttpRoutes.endpoints(passwordAuthService, secureRequestService)
  }

  val allHttpEndpoints: HttpService[IO] = authEndpoints combineK graphqlEndpoints
}

object Registry {
  def apply() =
    new Registry
}

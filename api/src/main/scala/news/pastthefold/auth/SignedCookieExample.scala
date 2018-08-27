package news.pastthefold.auth

import java.util.UUID

import cats.Id
import cats.effect.IO
import cats.syntax.semigroupk._
import news.pastthefold.model.User
import org.http4s.HttpService
import org.http4s.dsl.io._
import tsec.authentication._
import tsec.mac.jca.{HMACSHA256, MacSigningKey}

import scala.concurrent.duration._

object SignedCookieExample {

  import ExampleAuthHelpers._

  type AuthService = TSecAuthService[User, AuthenticatedCookie[HMACSHA256, Int], IO]
  type AwareService = UserAwareService[User, AuthenticatedCookie[HMACSHA256, Int], IO]

  val cookieBackingStore: BackingStore[IO, UUID, AuthenticatedCookie[HMACSHA256, Int]] =
    dummyBackingStore[IO, UUID, AuthenticatedCookie[HMACSHA256, Int]](_.id)

  // We create a way to store our users. You can attach this to say, your doobie accessor
  val userStore: BackingStore[IO, Int, User] = dummyBackingStore[IO, Int, User](_.id)

  val settings: TSecCookieSettings = TSecCookieSettings(
    cookieName = "tsec-auth",
    secure = false, // Turn on in production
    expiryDuration = 365.days, // Absolute expiration time
    maxIdle = Some(48.hours) // Rolling window expiration. Set this to a FiniteDuration if you intend to have one
  )

  def businessLogic[T](t: T): T = {
    businessLogic(t)
    ???
  }

  //Our Signing key. Instantiate in a safe way using generateKey[F] where F[_]: Sync
  val key: MacSigningKey[HMACSHA256] = HMACSHA256.generateKey[Id]

  val cookieAuth =
    SignedCookieAuthenticator(
      settings,
      cookieBackingStore,
      userStore,
      key
    )

  val Auth =
    SecuredRequestHandler(cookieAuth)

  val service1: AuthService = TSecAuthService {
    //Where user is the case class User above
    case request @ GET -> Root / "api" asAuthed user =>
      /*
      Note: The request is of type: SecuredRequest, which carries:
      1. The request
      2. The Authenticator (i.e token)
      3. The identity (i.e in this case, User)
       */
      val r: SecuredRequest[IO, User, AuthenticatedCookie[HMACSHA256, Int]] = request
      businessLogic(r.identity)
      businessLogic(user)
      Ok()
  }

  val service2: AuthService = TSecAuthService {
    case request @ GET -> Root / "api2" asAuthed user =>
      val r: SecuredRequest[IO, User, AuthenticatedCookie[HMACSHA256, Int]] = request
      businessLogic(r)
      businessLogic(user)
      Ok()
  }

  val service3: AwareService = UserAwareService {
    case request @ GET -> Root / "login" asAware user =>
      val r: UserAwareRequest[IO, User, AuthenticatedCookie[HMACSHA256, Int]] = request
      val foo: (User, AuthenticatedCookie[HMACSHA256, Int]) = r.maybe.get
      val userId = 123
      val authedCookie: IO[AuthenticatedCookie[HMACSHA256, Int]] = Auth.authenticator.create(userId)
      businessLogic(r)
      businessLogic(foo)
      businessLogic(authedCookie)
      businessLogic(user)
      for {
        authedCookie <- Auth.authenticator.create(userId)
        response <- Ok()
      } yield Auth.authenticator.embed(response, authedCookie)
  }

  val liftedService3: HttpService[IO] = Auth.liftUserAware(service3)
  val liftedService1: HttpService[IO] = Auth.liftService(service1)
  val liftedComposed: HttpService[IO] = Auth.liftService(service1 <+> service2)

}

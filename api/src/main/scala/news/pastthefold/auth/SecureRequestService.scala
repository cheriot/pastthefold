package news.pastthefold.auth

import java.util.UUID

import cats.data.{Kleisli, OptionT}
import cats.effect.Sync
import cats.implicits._
import news.pastthefold.auth.SecureRequestService.{AuthCookie, AuthService}
import news.pastthefold.model.User
import org.http4s.{HttpService, Request, Response, Status}
import tsec.authentication.{AuthenticatedCookie, BackingStore, SecuredRequest, SecuredRequestHandler, SignedCookieAuthenticator, TSecAuthService, TSecCookieSettings, UserAwareMiddleware, UserAwareRequest, UserAwareService}
import tsec.mac.jca.{HMACSHA256, MacSigningKey}

import scala.concurrent.duration._

trait SecureRequestService[F[_]] {

  def auth: SecuredRequestHandler[F, Int, User, AuthenticatedCookie[HMACSHA256, Int]]

  def embedAuth(user: User, response: Response[F]): F[Response[F]]

  def liftUserAwareWithFallThrough(service: UserAwareService[User, AuthCookie, F]): HttpService[F]

  def liftServiceWithFallThrough(service: AuthService[F]): HttpService[F]
}

object SecureRequestService {

  type UserService[F[_]] = UserAwareService[User, AuthCookie, F]
  type AuthService[F[_]] = TSecAuthService[User, AuthCookie, F]
  type AuthCookie = AuthenticatedCookie[HMACSHA256, Int]

  def apply[F[_] : Sync](
                          userBackingStore: BackingStore[F, Int, User],
                          cookieBackingStore: BackingStore[F, UUID, AuthCookie],
                          key: MacSigningKey[HMACSHA256]
                        ): SecureRequestService[F] =
    new SignedCookieRequestHandler[F](
      userBackingStore,
      cookieBackingStore,
      key
    )
}

/**
  * @param userBackingStore
  * @param cookieBackingStore
  * @param key Our signing key. Must be the same value for every instance in an environment. Generate with Auth.generateKey
  * @tparam F
  */
class SignedCookieRequestHandler[F[_] : Sync](
                                               userBackingStore: BackingStore[F, Int, User],
                                               cookieBackingStore: BackingStore[F, UUID, AuthCookie],
                                               key: MacSigningKey[HMACSHA256]
                                             ) extends SecureRequestService[F] {

  private val settings =
    TSecCookieSettings(
      cookieName = "auth",
      secure = false, // Set to true in production
      expiryDuration = 365.days, // Absolute expiration time
      maxIdle = Some(48.hours) // Rolling window expiration. Set this to a FiniteDuration if you intend to have one
    )

  private val cookieAuth =
    SignedCookieAuthenticator(
      settings,
      cookieBackingStore,
      userBackingStore,
      key)

  private val cachedUnauthorized: Response[F] = Response[F](Status.Unauthorized)

  val auth = SecuredRequestHandler(cookieAuth)

  def embedAuth(user: User, response: Response[F]): F[Response[F]] =
    for {
      authCookie <- auth.authenticator.create(user.id)
    } yield auth.authenticator.embed(response, authCookie)

  private def liftUserAwareWithFallThrough(
                                            authedStuff: Kleisli[OptionT[F, ?], Request[F], SecuredRequest[F, User, AuthCookie]]
                                          ): UserAwareMiddleware[F, User, AuthCookie] =

    (service: UserAwareService[User, AuthCookie, F]) => {
      Kleisli { r: Request[F] =>
        val possibleResponse: F[Option[Response[F]]] = authedStuff
          .map(r => UserAwareRequest(r.request, Some((r.identity, r.authenticator))))
          .run(r)
          .getOrElse(UserAwareRequest(r, None))
          .flatMap((uar: UserAwareRequest[F, User, AuthCookie]) => {
            service.run(uar).value
          })

        OptionT(possibleResponse)
      }
    }

  def liftUserAwareWithFallThrough(
                                    service: UserAwareService[User, AuthCookie, F]
                                  ): HttpService[F] = {

    val middleware: UserAwareMiddleware[F, User, AuthenticatedCookie[HMACSHA256, Int]] =
      liftUserAwareWithFallThrough(Kleisli(cookieAuth.extractAndValidate))

    middleware(service)
      .handleErrorWith { e =>
        // TODO: fix log:
        // SecuredRequestHandler.logger.error(e)("Caught unhandled exception in authenticated service")
        println(s"Caught unhandled exception in authenticated service $e")
        Kleisli.liftF(OptionT.pure(cachedUnauthorized))
      }
  }

  override def liftServiceWithFallThrough(
                            service: TSecAuthService[User, AuthCookie, F]
                          ): HttpService[F] =
    auth.liftWithFallthrough(service)
}

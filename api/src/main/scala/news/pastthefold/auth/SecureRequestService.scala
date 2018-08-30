package news.pastthefold.auth

import java.util.UUID

import cats.effect.Sync
import cats.implicits._
import news.pastthefold.model.User
import org.http4s.Response
import tsec.authentication.{AuthenticatedCookie, BackingStore, SecuredRequestHandler, SignedCookieAuthenticator, TSecCookieSettings}
import tsec.mac.jca.{HMACSHA256, MacSigningKey}

import scala.concurrent.duration._

trait SecureRequestService[F[_]] {
  def embedAuth(user: User, response: Response[F]): F[Response[F]]
}

/**
  * @param userBackingStore
  * @param cookieBackingStore
  * @param key Our signing key. Must be the same value for every instance in an environment. Generate with Auth.generateKey
  * @tparam F
  */
class SignedCookieRequestHandler[F[_]: Sync](
                                              userBackingStore: BackingStore[F, Int, User],
                                              cookieBackingStore: BackingStore[F, UUID, AuthenticatedCookie[HMACSHA256, Int]],
                                              key: MacSigningKey[HMACSHA256]
                                            ) {

 val Auth = {
  val settings: TSecCookieSettings = TSecCookieSettings(
   cookieName = "auth",
   secure = false, // Set to true in production
   expiryDuration = 365.days, // Absolute expiration time
   maxIdle = Some(48.hours) // Rolling window expiration. Set this to a FiniteDuration if you intend to have one
  )

  val cookieAuth =
   SignedCookieAuthenticator(
    settings,
    cookieBackingStore,
    userBackingStore,
    key)

  SecuredRequestHandler(cookieAuth)
 }

 def embedAuth(user: User, response: Response[F]): F[Response[F]] =
  for {
   authCookie <- Auth.authenticator.create(user.id)
  } yield Auth.authenticator.embed(response, authCookie)

}

package news.pastthefold.auth

import java.util.UUID

import cats._
import cats.effect.Sync
import cats.implicits._
import news.pastthefold.dao.UserAuthDAO
import news.pastthefold.model.User
import org.http4s.Response
import tsec.authentication.{AuthenticatedCookie, BackingStore, SecuredRequestHandler, SignedCookieAuthenticator, TSecCookieSettings}
import tsec.mac.jca.{HMACSHA256, MacSigningKey}

import scala.concurrent.duration._

trait SecureRequestService[F[_]] {
  def embedAuth(user: User, response: Response[F]): F[Response[F]]
}

class SignedCookieRequestHandler[F[_]: Sync](
                                              userAuthDAO: UserAuthDAO[F],
                                              cookieBackingStore: BackingStore[F, UUID, AuthenticatedCookie[HMACSHA256, Int]]
                                            ) {

 val Auth = {
  val settings: TSecCookieSettings = TSecCookieSettings(
   cookieName = "auth",
   secure = false, // Set to true in production
   expiryDuration = 365.days, // Absolute expiration time
   maxIdle = Some(48.hours) // Rolling window expiration. Set this to a FiniteDuration if you intend to have one
  )

  //Our Signing key. Instantiate in a safe way using generateKey[F] where F[_]: Sync
  val key: MacSigningKey[HMACSHA256] = HMACSHA256.generateKey[Id]

  val cookieAuth =
   SignedCookieAuthenticator(
    settings,
    cookieBackingStore,
    userAuthDAO,
    key
   )

  SecuredRequestHandler(cookieAuth)
 }

 def embedAuth(user: User, response: Response[F]): F[Response[F]] =
  for {
   authedCookie <- Auth.authenticator.create(user.id)
  } yield Auth.authenticator.embed(response, authedCookie)

}


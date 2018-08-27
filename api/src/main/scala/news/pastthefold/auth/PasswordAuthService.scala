package news.pastthefold.auth

import java.util.UUID

import cats._
import cats.data._
import cats.effect.{Effect, Sync}
import cats.implicits._
import news.pastthefold.auth.PasswordEncryptionService.EncryptedPassword
import news.pastthefold.dao.UserAuthDAO
import news.pastthefold.model.{Salt, UntrustedPassword, User}
import org.bouncycastle.util.encoders.Hex
import org.http4s.Response
import tsec.authentication.{AuthenticatedCookie, BackingStore, SecuredRequestHandler, SignedCookieAuthenticator, TSecCookieSettings}
import tsec.common.{ManagedRandom, VerificationFailed, VerificationStatus, Verified}
import tsec.mac.jca.{HMACSHA256, MacSigningKey}
import tsec.passwordhashers.PasswordHash
import tsec.passwordhashers.jca._

import scala.concurrent.duration._

trait PasswordAuthService[F[_]] {

  def createPassword(password: String): F[Either[CreatePasswordError, EncryptedPassword]]

  def updatePassword(email: String, oldPassword: UntrustedPassword, nextPassword: String): F[Either[UpdatePasswordError, User]]

  def login(email: String, password: UntrustedPassword): F[Either[LoginError, User]]

  def embedAuth(user: User, response: Response[F]): F[Response[F]]
}

trait PasswordEncryptionService[F[_]] {

  def hashPassword(password: String): F[EncryptedPassword]

  def verifyPassword(salt: Salt, password: UntrustedPassword, passwordHash: PasswordHash[HardenedSCrypt]): F[VerificationStatus]
}

object PasswordEncryptionService {
  type EncryptedPassword = (Salt, PasswordHash[HardenedSCrypt])
}

class HardenedSCryptPasswordEncryptionService[F[_] : Sync] extends PasswordEncryptionService[F] with ManagedRandom {

  override def hashPassword(password: String): F[EncryptedPassword] =
    for {
      salt <- createSalt()
      saltedPassword = saltPassword(salt, password)
      passwordHash <- HardenedSCrypt.hashpw[F](saltedPassword)
    } yield (salt, passwordHash)

  override def verifyPassword(salt: Salt, untrustedPassword: UntrustedPassword, passwordHash: PasswordHash[HardenedSCrypt]): F[VerificationStatus] =
    HardenedSCrypt.checkpw[F](
      saltPassword(salt, untrustedPassword.value),
      passwordHash
    )

  private val saltSize = 32

  private def createSalt(): F[Salt] =
    Sync[F].pure {
      val saltBytes = new Array[Byte](saltSize)
      nextBytes(saltBytes)
      Salt(Hex.toHexString(saltBytes))
    }

  /** Changing this function will invalidate every password in the database. **/
  private def saltPassword(salt: Salt, password: String): String =
    salt.value + password
}

class DemoPasswordAuthService[F[_] : Effect](
                                              userAuthDAO: UserAuthDAO[F],
                                              passwordEncryptionService: PasswordEncryptionService[F]
                                            ) extends PasswordAuthService[F] {

  val Auth = {
    import ExampleAuthHelpers._

    val cookieBackingStore: BackingStore[F, UUID, AuthenticatedCookie[HMACSHA256, Int]] =
      dummyBackingStore[F, UUID, AuthenticatedCookie[HMACSHA256, Int]](_.id)

    // We create a way to store our users. You can attach this to say, your doobie accessor
    val userStore: BackingStore[F, Int, User] = dummyBackingStore[F, Int, User](_.id)
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
        userStore,
        key
      )
    SecuredRequestHandler(cookieAuth)
  }

  override def createPassword(password: String): F[Either[CreatePasswordError, EncryptedPassword]] =
    Either.cond(
      passwordRequirements(password),
      passwordEncryptionService.hashPassword(password),
      CreatePasswordError()
    ).sequence

  override def updatePassword(email: String, oldPassword: UntrustedPassword, nextPassword: String): F[Either[UpdatePasswordError, User]] = {

    val loginWithOldCredentials: EitherT[F, UpdatePasswordError, User] =
      EitherT(
        login(email, oldPassword)
      ).leftMap(e => InvalidCredentialsError(e))

    val encryptNextPassword: EitherT[F, UpdatePasswordError, EncryptedPassword] =
      EitherT(
        createPassword(nextPassword)
      ).leftMap(_ => PasswordRequirementsError())

    def updatePassword(user: User, salt: Salt, passwordHash: PasswordHash[HardenedSCrypt]): EitherT[F, UpdatePasswordError, User] =
      EitherT(
        userAuthDAO
          .updatePassword(user, salt, passwordHash)
          .map(Either.right)
      )

    // val eitherT = for {
    //   user <- loginWithOldCredentials
    //   encryptedPassword <- encryptNextPassword
    //   (salt, passwordHash) = encryptedPassword
    //   nextUser <- updatePassword(user, salt, passwordHash)
    // } yield nextUser
    // eitherT.value

    (loginWithOldCredentials, encryptNextPassword).mapN {
      case (user, (salt, passwordHash)) => updatePassword(user, salt, passwordHash)
    }.flatten.value
  }

  override def login(email: String, password: UntrustedPassword): F[Either[LoginError, User]] =
    for {
      user <- userAuthDAO.findByEmail(email)
      pwStatus <- passwordEncryptionService.verifyPassword(
        user.salt,
        password,
        user.passwordHash
      )
    } yield pwStatus match {
      case Verified => Right(user)
      case VerificationFailed => Left(WrongPasswordError)
    }

  override def embedAuth(user: User, response: Response[F]): F[Response[F]] =
    for {
      authedCookie <- Auth.authenticator.create(user.id)
    } yield Auth.authenticator.embed(response, authedCookie)

  private val minPasswordLength = 8

  private def passwordRequirements(password: String): Boolean =
    password.length > minPasswordLength
}

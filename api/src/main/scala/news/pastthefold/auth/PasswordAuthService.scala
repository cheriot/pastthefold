package news.pastthefold.auth

import cats.data._
import cats.effect.Effect
import cats.implicits._
import news.pastthefold.auth.PasswordHashingService.EncryptedPassword
import news.pastthefold.dao.UserAuthDAO
import news.pastthefold.model.{Salt, UntrustedPassword, User}
import org.http4s.Response
import tsec.common.{VerificationFailed, Verified}
import tsec.passwordhashers.PasswordHash
import tsec.passwordhashers.jca._

trait PasswordAuthService[F[_]] {

  def createPassword(password: String): F[Either[CreatePasswordError, EncryptedPassword]]

  def updatePassword(email: String, oldPassword: UntrustedPassword, nextPassword: String): F[Either[UpdatePasswordError, User]]

  def login(email: String, password: UntrustedPassword): F[Either[LoginError, User]]

  def embedAuth(user: User, response: Response[F]): F[Response[F]]
}

class PasswordAuthServiceImpl[F[_] : Effect](
                                              userAuthDAO: UserAuthDAO[F],
                                              passwordEncryptionService: PasswordHashingService[F],
                                              secureRequestService: SecureRequestService[F]
                                            ) extends PasswordAuthService[F] {

  private val minPasswordLength = 8

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
    secureRequestService.embedAuth(user, response)

  private def passwordRequirements(password: String): Boolean =
    password.length > minPasswordLength
}

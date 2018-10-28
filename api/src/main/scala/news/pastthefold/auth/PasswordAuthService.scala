package news.pastthefold.auth

import cats.data._
import cats.effect.Effect
import cats.implicits._
import news.pastthefold.auth.PasswordHashingService.EncryptedPassword
import news.pastthefold.dao.UserAuthDAO
import news.pastthefold.http.{CreateAccountForm, LoginForm, UpdatePasswordForm}
import news.pastthefold.model.{Salt, User}
import org.http4s.Response
import tsec.common.{VerificationFailed, Verified}
import tsec.passwordhashers.PasswordHash
import tsec.passwordhashers.jca._

trait PasswordAuthService[F[_]] {
  def createAccount(form: CreateAccountForm): F[Either[AuthError, User]]

  def updatePassword(form: UpdatePasswordForm): F[Either[AuthError, User]]

  def login(form: LoginForm): F[Either[AuthError, User]]

  def embedAuth(user: User, response: Response[F]): F[Response[F]]
}

object PasswordAuthService {
  def apply[F[_] : Effect](
                            userAuthDAO: UserAuthDAO[F],
                            passwordEncryptionService: PasswordHashingService[F],
                            secureRequestService: SecureRequestService[F]
                          ) =
    new PasswordAuthServiceImpl[F](userAuthDAO, passwordEncryptionService, secureRequestService)
}

class PasswordAuthServiceImpl[F[_] : Effect](
                                              userAuthDAO: UserAuthDAO[F],
                                              passwordEncryptionService: PasswordHashingService[F],
                                              secureRequestService: SecureRequestService[F]
                                            ) extends PasswordAuthService[F] {

  override def createAccount(form: CreateAccountForm): F[Either[AuthError, User]] = {
    val eitherT = for {
      encryptedPassword <- createPassword(form.password.value)

      // TODO: More specific error mapping
      user <- userAuthDAO.create(form.email, encryptedPassword)
          .leftMap[AuthError](_ => AccountDuplicateError)

    } yield user

    eitherT.value
  }

  private val minPasswordLength = 8

  def createPassword(password: String): EitherT[F, AuthError, EncryptedPassword] =
    EitherT(
      Either.cond[AuthError, F[EncryptedPassword]](
        passwordRequirements(password),
        passwordEncryptionService.hashPassword(password),
        PasswordRequirementsError
      ).sequence
    )

  override def updatePassword(form: UpdatePasswordForm): F[Either[AuthError, User]] = {

    val loginWithOldCredentials: EitherT[F, AuthError, User] =
      EitherT(
        login(form.toLoginForm)
      ).leftMap(e => new InvalidCredentialsError(e))

    val encryptNextPassword: EitherT[F, AuthError, EncryptedPassword] =
      createPassword(form.nextPassword)
        .leftMap(_ => PasswordRequirementsError)

    def updatePassword(user: User, salt: Salt, passwordHash: PasswordHash[HardenedSCrypt]): EitherT[F, AuthError, User] =
      EitherT(
        userAuthDAO
          .updatePassword(user, salt, passwordHash)
          .map(Either.right)
      )

    (loginWithOldCredentials, encryptNextPassword).mapN {
      case (user, (salt, passwordHash)) => updatePassword(user, salt, passwordHash)
    }.flatten.value
  }

  override def login(form: LoginForm): F[Either[AuthError, User]] = {
    def verify(user: User): F[Either[AuthError, User]] =
      for {
        pwStatus <- passwordEncryptionService.verifyPassword(
          user.salt,
          form.password,
          user.passwordHash)
      } yield pwStatus match {
        case Verified => Right(user)
        case VerificationFailed => Left(WrongPasswordError)
      }

    userAuthDAO
      .findByEmail(form.email)
      .flatMap {
        case Some(user) => verify(user)
        case None => Effect[F].pure(Left(EmailNotFoundError))
      }
  }

  override def embedAuth(user: User, response: Response[F]): F[Response[F]] =
    secureRequestService.embedAuth(user, response)

  private def passwordRequirements(password: String): Boolean =
    password.length > minPasswordLength
}

package news.pastthefold.auth

import news.pastthefold.auth.UserAuthService.LoginError
import news.pastthefold.model.User
import org.http4s.{Response, Status}
import org.http4s.Status._

trait UserAuthService[F[_]] {

  val login: (String, String) => F[Either[LoginError, User]]
  // val login: (String, String) => F[Either[LoginError, User]] = (email, password) =>
  //   for {
  //     user <- userAuthDAO.findByEmail(email)
  //     isAuthSuccess <- userAuthService.verifyPassword(user, password)
  //   } yield if (isAuthSuccess) Right(user) else Left(WrongPasswordError)

  def verifyPassword(user: User, password: String): F[Boolean]

  // authedCookie = SignedCookieExample.Auth.authenticator.create(user.id)
  // Auth.authenticator.embed(response, authedCookie)
  def embedAuth(user: User, response: Response[F]): Response[F]
}

object UserAuthService {
  sealed abstract class LoginError(val status: Status)
  case object NoEmailError extends LoginError(BadRequest)
  case object NoPasswordError extends LoginError(BadRequest)
  case object WrongPasswordError extends LoginError(Forbidden)
}

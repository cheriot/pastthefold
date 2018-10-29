package news.pastthefold.http

import cats.effect.{Effect, Sync}
import cats.implicits._
import fs2._
import news.pastthefold.auth.SecureRequestService.UserService
import news.pastthefold.auth._
import news.pastthefold.model.{UntrustedPassword, User}
import org.http4s._
import org.http4s.dsl.Http4sDsl
import tsec.authentication._

class AuthHttpRoutes[F[_] : Effect](
                                     userAuthService: PasswordAuthService[F]
                                   ) extends Http4sDsl[F] {

  def extractLoginForm(urlForm: UrlForm): Either[AuthError, LoginForm] =
    for {
      email <- Either.fromOption(urlForm.getFirst("email"), NoEmailError)
      password <- Either.fromOption(urlForm.getFirst("password"), NoPasswordError)
    } yield LoginForm(email, UntrustedPassword(password))

  def extractNewAccountForm(urlForm: UrlForm): Either[AuthError, CreateAccountForm] =
    for {
      email <- Either.fromOption(urlForm.getFirst("email"), AccountEmailError)
      password <- Either.fromOption(urlForm.getFirst("password"), AccountPasswordError)
    } yield CreateAccountForm(email, UntrustedPassword(password))


  def authenticatedOk(user: User, msg: String) = {
    val response = Response[F](body = Stream(msg).through(fs2.text.utf8Encode))
    userAuthService.embedAuth(user, response)
  }

  def unauthenticatedError(loginError: HttpStatusError): F[Response[F]] =
    Response[F](status = loginError.status)
      .withBody(s"Error: $loginError")

  def endpoints(): UserService[F] = UserAwareService {

    case awareReq@POST -> Root / "create" asAware _ =>
      awareReq.request.decode[UrlForm] { urlForm =>
        extractNewAccountForm(urlForm)
          .flatTraverse(userAuthService.createAccount)
          .flatMap {
            case Right(user) => authenticatedOk(user, s"Success. Acount created for ${user.email}")
            case Left(error) => unauthenticatedError(error)
          }
      }

    case awareReq@POST -> Root / "login" asAware _ =>
      awareReq.request.decode[UrlForm] { urlForm =>
        extractLoginForm(urlForm)
          .flatTraverse(userAuthService.login)
          .flatMap {
            case Right(user) => authenticatedOk(user, s"Success. You're logged in!")
            case Left(loginError) => unauthenticatedError(loginError)
          }
      }

    case POST -> Root / "logout" asAware userOpt =>
      userOpt match {
        case Some(_) => ???
        case None => Ok()
      }
  }

}

object AuthHttpRoutes {
  def endpoints[F[_] : Effect](
                                passwordAuthService: PasswordAuthService[F],
                                secureRequestService: SecureRequestService[F]
                              ): HttpService[F] =
    secureRequestService.liftUserAwareWithFallThrough(
      new AuthHttpRoutes[F](passwordAuthService).endpoints()
    )
}
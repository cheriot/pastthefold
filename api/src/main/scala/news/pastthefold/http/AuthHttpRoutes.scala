package news.pastthefold.http

import cats.effect.{Effect, Sync}
import cats.implicits._
import fs2._
import news.pastthefold.auth.SecureRequestService.AuthCookie
import news.pastthefold.auth._
import news.pastthefold.model.{UntrustedPassword, User}
import org.http4s._
import org.http4s.dsl.Http4sDsl
import tsec.authentication._

class AuthHttpRoutes[F[_] : Effect](
                                     userAuthService: PasswordAuthService[F],
                                     secureRequestService: SecureRequestService[F]
                                   ) extends Http4sDsl[F] {

  def extractLoginForm(urlForm: UrlForm): Either[LoginError, LoginForm] =
    for {
      email <- Either.fromOption(urlForm.getFirst("email"), NoEmailError)
      password <- Either.fromOption(urlForm.getFirst("password"), NoPasswordError)
    } yield LoginForm(email, UntrustedPassword(password))

  def authenticatedOk(user: User, msg: String) = {
    val response = Response[F](body = Stream(msg).through(fs2.text.utf8Encode))
    userAuthService.embedAuth(user, response)
  }

  def unauthenticatedError(loginError: LoginError): F[Response[F]] =
    Sync[F].pure(Response(status = loginError.status))

  def endpoints(): UserAwareService[User, AuthCookie, F] = UserAwareService {
    case awareReq@POST -> Root / "login" asAware userOpt =>
      awareReq.request.decode[UrlForm] { urlForm =>
        extractLoginForm(urlForm)
          .flatTraverse(userAuthService.login)
          .flatMap {
            case Right(user) => authenticatedOk(user, s"Success. You're logged in!")
            case Left(loginError) => unauthenticatedError(loginError)
          }
      }

    case req@POST -> Root / "logout" asAware userOpt =>
      userOpt match {
        case Some(user) => ???
        case None => Ok()
      }
  }

}

object AuthHttpRoutes {
  def endpoints[F[_] : Effect](
                                passwordAuthService: PasswordAuthService[F],
                                secureRequestService: SecureRequestService[F]
                              ): HttpService[F] =
    secureRequestService.liftUserAware(
      new AuthHttpRoutes[F](passwordAuthService, secureRequestService).endpoints()
    )
}
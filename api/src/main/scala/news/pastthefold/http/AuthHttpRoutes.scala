package news.pastthefold.http

import cats.effect.{Effect, Sync}
import cats.implicits._
import fs2._
import news.pastthefold.auth._
import news.pastthefold.model.{UntrustedPassword, User}
import org.http4s._
import org.http4s.dsl.io._

class AuthHttpRoutes[F[_] : Effect](userAuthService: PasswordAuthService[F]) {

  def extractLoginForm(urlForm: UrlForm): Either[LoginError, (String, UntrustedPassword)] =
    for {
      email <- Either.fromOption(urlForm.getFirst("email"), NoEmailError)
      password <- Either.fromOption(urlForm.getFirst("password"), NoPasswordError)
    } yield (email, UntrustedPassword(password))

  def authenticatedOk(user: User, msg: String) = {
    val response = Response[F](body = Stream(msg).through(fs2.text.utf8Encode))
    userAuthService.embedAuth(user, response)
  }

  def unauthenticatedError(loginError: LoginError): F[Response[F]] =
    Sync[F].pure(Response(status = loginError.status))

  def endpoints = HttpService[F] {
    case req@POST -> Root / "login" =>
      req.decode[UrlForm] { urlForm =>
        extractLoginForm(urlForm)
          .flatTraverse((userAuthService.login _).tupled)
          .flatMap {
            case Right(user) => authenticatedOk(user, s"Success. You're logged in!")
            case Left(loginError) => unauthenticatedError(loginError)
          }
      }

    case POST -> Root / "logout" => ???
  }

}

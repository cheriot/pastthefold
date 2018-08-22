package news.pastthefold.http

import cats.effect.Effect
import cats.implicits._
import fs2._
import news.pastthefold.auth.UserAuthService
import news.pastthefold.auth.UserAuthService._
import news.pastthefold.model.User
import org.http4s.dsl.io._
import org.http4s.{HttpRoutes, _}

class AuthHttpRoutes[F[_] : Effect](userAuthService: UserAuthService[F]) {

  def extractLoginForm(urlForm: UrlForm): Either[LoginError, (String, String)] =
    for {
      email <- Either.fromOption(urlForm.getFirst("email"), NoEmailError)
      password <- Either.fromOption(urlForm.getFirst("password"), NoPasswordError)
    } yield (email, password)

  def authenticatedOk(user: User, msg: String) = {
    val response = Response[F](body = Stream(msg).through(fs2.text.utf8Encode))
    userAuthService.embedAuth(user, response)
  }

  def endpoints: HttpRoutes[F] =
    HttpRoutes.of[F] {
      case req@POST -> Root / "login" =>
        req.decode[UrlForm] { urlForm =>
          extractLoginForm(urlForm).flatTraverse(userAuthService.login.tupled)
            .map {
              case Right(user) => authenticatedOk(user, s"Success. You're logged in!")
              case Left(loginError) => Response(status = loginError.status)
            }
        }

      case POST -> Root / "logout" => ???
    }

}

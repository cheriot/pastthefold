package news.pastthefold

import cats._
import cats.implicits._
import cats.effect.IO
import org.http4s.{Request, _}
import utest._

object AuthIntTest extends TestSuite {

  def requestWithAuth[F[_]: Functor](response: Response[F], request: Request[F]): Request[F] = {
    val authCookie = response.cookies.find(_.name == "auth").get
    request.addCookie(authCookie)
  }

  val tests = Tests {
    val endpoints = Registry().allHttpEndpoints
    "login" - {

      val response: Response[IO] = endpoints.run(
        Request(Method.POST, Uri(path = "/login"))
      ).value.unsafeRunSync().get

      response.cookies.find(_.name == "auth")
    }
  }
}
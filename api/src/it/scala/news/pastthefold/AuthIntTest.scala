package news.pastthefold

import cats._
import cats.implicits._
import cats.effect.IO
import fs2.Chunk
import org.http4s.{Request, _}
import utest._

object AuthIntTest extends TestSuite {

  def requestWithAuth[F[_]: Functor](response: Response[F], request: Request[F]): Request[F] = {
    val authCookie = response.cookies.find(_.name == "auth").get
    request.addCookie(authCookie)
  }

  val tests = Tests {
    val endpoints = Registry().authEndpoints

    "login" - {

      val form = UrlForm("email" -> "fake@fake.fake", "password" -> "notreal")
      val req = Request[IO](Method.POST, Uri(path = "/login"))
        .withBody(form)
        .unsafeRunSync()
      val response: Response[IO] = endpoints
        .run(req)
        .value
        .unsafeRunSync()
        .get

      assert(response.cookies.find(_.name == "auth").isDefined)

      requestWithAuth(response, Request(Method.POST, Uri(path = "/graphql")))
    }
  }
}
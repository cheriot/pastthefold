package news.pastthefold

import cats._
import cats.implicits._
import cats.effect.IO
import news.pastthefold.model.User
import org.http4s.{Request, _}
import utest._
import org.http4s.Method._
import tsec.authentication.{AuthenticatedCookie, SecuredRequestHandler, TSecAuthService, UserAwareService, asAware, asAuthed}
import tsec.mac.jca.HMACSHA256

object AuthIntTest extends TestSuite {

  val registry = Registry()
  val endpoints = registry.allHttpEndpoints

  def requestWithAuth[F[_] : Functor](response: Response[F], request: Request[F]): Request[F] = {
    val authCookie = response.cookies.find(_.name == "auth").get
    request.addCookie(authCookie)
  }

  def bodyStr(response: Response[IO]): String =
    new String(response.body.compile.toVector.unsafeRunSync().toArray)

  def fetch(request: IO[Request[IO]]): Response[IO] =
    request.map(r =>
      endpoints
        .run(r)
        .value
        .unsafeRunSync()
        .get
    ).unsafeRunSync()

  val tests = Tests {

    "login" - {

      val form = UrlForm("email" -> "fake@fake.fake", "password" -> "notreal1")

      val createAccountRequest =
        Request[IO](POST, Uri(path = "/create"))
          .withBody(form)

      val createAccountResponse = fetch(createAccountRequest)

      println(bodyStr(createAccountResponse))

      val loginRequest =
        Request[IO](POST, Uri(path = "/login"))
          .withBody(form)

      val loginResponse = fetch(loginRequest)
      println(bodyStr(loginResponse))
      assert(loginResponse.cookies.find(_.name == "auth").isDefined)

      val graphQlRequest = requestWithAuth(loginResponse, Request(POST, Uri(path = "/graphql")))
        .withBody(
          """
            | {
            |   "query": "{ storylines { id, title, articles { id } } }"
            | }
          """.stripMargin)

      val graphQlResponse = fetch(graphQlRequest)
      assert(graphQlResponse.status == Status.Ok)
      val jsonStr = bodyStr(graphQlResponse)
      println(jsonStr)
      assert(jsonStr.contains("title2"))
    }

    "HttpService, UserAwareService, and TSecAuthService can be combined" - {
      import cats.effect.Effect
      import org.http4s._
      import org.http4s.dsl.Http4sDsl

      class HttpRoutes[F[_] : Effect] extends Http4sDsl[F] {

        val userRequired: TSecAuthService[User, AuthenticatedCookie[HMACSHA256, Int], F] = TSecAuthService {
          case GET -> Root / "secret" asAuthed _ =>
            Response[F](status = Status.Ok)
              .withBody("The secret is...")
        }

        val userAware1: UserAwareService[User, AuthenticatedCookie[HMACSHA256, Int], F] = UserAwareService {
          case GET -> Root / "foo" asAware _ =>
            Response[F](status = Status.Ok)
              .withBody("Great!")
        }

        val plainService = HttpService[F] {
          case GET -> Root / "bar" =>
            Response[F](status = Status.Ok)
              .withBody("Good!")
        }

      }

      val httpRoutes = new HttpRoutes[IO]

      val auth: SecuredRequestHandler[IO, Int, User, AuthenticatedCookie[HMACSHA256, Int]] = registry.secureRequestService.auth

      val liftedAuthRequired: HttpService[IO] = auth.liftWithFallthrough(httpRoutes.userRequired)
      val liftedUserAware: HttpService[IO] = registry.secureRequestService.liftUserAwareWithFallThrough(httpRoutes.userAware1)

      val allHttp: HttpService[IO] = liftedAuthRequired combineK liftedUserAware combineK httpRoutes.plainService

      val requestFoo = Request[IO](GET, Uri(path = "/foo"))
      val requestBar = Request[IO](GET, Uri(path = "/bar"))
      val requestUnknown = Request[IO](GET, Uri(path = "/unknown"))

      val responseFoo: Response[IO] =
        allHttp
          .run(requestFoo)
          .value
          .unsafeRunSync()
          .get

      assert(responseFoo.status == Status.Ok)

      val responseBar: Response[IO] =
        allHttp
          .run(requestBar)
          .value
          .unsafeRunSync()
          .get
      assert(responseBar.status == Status.Ok)

      val responseUnknown: Option[Response[IO]] =
        allHttp
        .run(requestUnknown)
        .value
        .unsafeRunSync()
      assert(responseUnknown.isEmpty)
    }
  }
}
package news.pastthefold.auth

import java.util.UUID

import cats.effect.IO
import news.pastthefold.Config
import news.pastthefold.auth.SecureRequestService.AuthCookie
import news.pastthefold.model.User
import org.http4s.dsl.io._
import org.http4s.{Request, Response}
import tsec.authentication.BackingStore
import utest._


object SecureRequestServiceTest extends TestSuite {

  val key = Config.cookieSigningKey
  val user = FakeData.user
  val userBackingStore: BackingStore[IO, Int, User] = Mocks.userBackingStore
  userBackingStore.put(user)
  val cookieBackingStore: BackingStore[IO, UUID, AuthCookie] = Mocks.cookieBackingStore

  def build() = new SignedCookieRequestHandler[IO](
    userBackingStore,
    cookieBackingStore,
    key
  )

  override def tests = Tests {
    "embedAuth - create" - {
      val instance = build()
      val response = instance.embedAuth(user, Response(Ok)).unsafeRunSync()
      assert(response.cookies.length == 1)
      val cookie = response.cookies(0)
      assert(cookie.httpOnly == true)
      assert(cookie.name == "auth")
      val contentLength = cookie.content.length
      assert(contentLength == 161)
    }

    "checkAuth - create & read" - {
      val instance = build()
      val response = instance.embedAuth(user, Response(Ok)).unsafeRunSync()
      val authCookie = response.cookies(0)
      val request = Request[IO]().addCookie(authCookie)
      val Some(securedRequest) = instance.auth.authenticator.extractAndValidate(request).value.unsafeRunSync()
      assert(securedRequest.identity.id == user.id)
    }

    "checkAuth - create & read with different instances" - {
      val instanceA = build()
      val instanceB = build()
      val response = instanceA.embedAuth(user, Response(Ok)).unsafeRunSync()
      val authCookie = response.cookies(0)
      val request = Request[IO]().addCookie(authCookie)
      val Some(securedRequest) = instanceB.auth.authenticator.extractAndValidate(request).value.unsafeRunSync()
      assert(securedRequest.identity.id == user.id)
    }
  }
}

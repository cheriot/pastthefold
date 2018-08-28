package news.pastthefold.auth

import cats.effect.IO
import news.pastthefold.dao.UserAuthDAO
import news.pastthefold.model.{Salt, UntrustedPassword, User}
import tsec.common.{VerificationFailed, Verified}
import tsec.passwordhashers.PasswordHash
import utest._

object PasswordAuthServiceTest extends TestSuite {

  import Mocks._

  def build(
             userAuthDAO: UserAuthDAO[IO] = buildUserAuthDAO(),
             passwordHashingService: PasswordHashingService[IO] = buildPasswordHashingService()
           ) = new PasswordAuthServiceImpl[IO](
    userAuthDAO,
    passwordHashingService,
    buildSecureRequestService
  )

  val password = "supersecret"

  val tests = Tests {
    "createPassword valid" - {
      val instance = build()
      val Right((salt, passwordHash)) = instance.createPassword(password).unsafeRunSync()
      assert(salt == Salt("fake-salt"))
      assert(passwordHash == PasswordHash("hash-of-supersecret"))
    }

    "createPassword invalid" - {
      val instance = build()
      val Left(error) = instance.createPassword("short").unsafeRunSync()
      assert(error == CreatePasswordError())
    }

    "login valid" - {
      val user = User(4, "fake@email.com", PasswordHash("passwordHash"), Salt("salt"))
      val instance = build(
        userAuthDAO = buildUserAuthDAO(userOpt = Some(user)),
        buildPasswordHashingService(Verified)
      )
      val Right(foundUser) = instance.login("fake@email.com", UntrustedPassword(password)).unsafeRunSync()
      assert(foundUser == user)
    }

    "login fail password" - {
      val user = User(4, "fake@email.com", PasswordHash("passwordHash"), Salt("salt"))
      val instance = build(
        userAuthDAO = buildUserAuthDAO(userOpt = Some(user)),
        buildPasswordHashingService(VerificationFailed)
      )
      val Left(error) = instance.login("fake@email.com", UntrustedPassword(password)).unsafeRunSync()
      assert(error == WrongPasswordError)
    }

    "login fail no user" - {
      val instance = build(
        userAuthDAO = buildUserAuthDAO(userOpt = None)
      )
      // instance.login("fake@email.com", UntrustedPassword(password)).unsafeRunSync()
      // TODO capture this error properly when the UserDAO api is better.
    }

    "embedAuth" - {
      // TODO
    }
  }
}

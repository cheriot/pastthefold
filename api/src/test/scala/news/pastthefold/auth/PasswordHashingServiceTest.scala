package news.pastthefold.auth

import cats.effect.IO
import cats.implicits._
import news.pastthefold.model.UntrustedPassword
import tsec.common.{VerificationFailed, Verified}
import utest._

object PasswordHashingServiceTest extends TestSuite {

  val instance = new HardenedSCryptPasswordHashingService[IO]()
  val password = "supersecret"
  val (salt, passwordHash) = instance.hashPassword(password).unsafeRunSync()

  val tests = Tests {
    "passwordHash" - {
      assert(passwordHash.toString =!= password)
      assert(passwordHash.length == 80)
    }

    "salt" - {
      assert(salt.value =!= password)
      assert(salt.value =!= passwordHash.toString)
      assert(salt.value.length === 64)
    }

    "correct password succeeds" - {
      val result = instance
        .verifyPassword(salt, UntrustedPassword(password), passwordHash)
        .unsafeRunSync()
      assert(result == Verified)
    }

    "wrong password fails" - {
      val result = instance
        .verifyPassword(salt, UntrustedPassword(password + "foo"), passwordHash)
        .unsafeRunSync()
      assert(result == VerificationFailed)
    }
  }
}

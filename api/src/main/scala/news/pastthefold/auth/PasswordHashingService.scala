package news.pastthefold.auth

import cats._
import cats.effect.Sync
import cats.implicits._
import news.pastthefold.auth.PasswordHashingService.EncryptedPassword
import news.pastthefold.model.{Salt, UntrustedPassword}
import org.bouncycastle.util.encoders.Hex
import tsec.common.{ManagedRandom, VerificationStatus}
import tsec.passwordhashers.PasswordHash
import tsec.passwordhashers.jca._

trait PasswordHashingService[F[_]] {
  def hashPassword(password: String): F[EncryptedPassword]

  def verifyPassword(salt: Salt, password: UntrustedPassword, passwordHash: PasswordHash[HardenedSCrypt]): F[VerificationStatus]
}

object PasswordHashingService {
  type EncryptedPassword = (Salt, PasswordHash[HardenedSCrypt])
}

class HardenedSCryptPasswordHashingService[F[_] : Sync] extends PasswordHashingService[F] with ManagedRandom {

  override def hashPassword(password: String): F[EncryptedPassword] =
    for {
      salt <- createSalt()
      saltedPassword = saltPassword(salt, password)
      passwordHash <- HardenedSCrypt.hashpw[F](saltedPassword)
    } yield (salt, passwordHash)

  override def verifyPassword(salt: Salt, untrustedPassword: UntrustedPassword, passwordHash: PasswordHash[HardenedSCrypt]): F[VerificationStatus] =
    HardenedSCrypt.checkpw[F](
      saltPassword(salt, untrustedPassword.value),
      passwordHash
    )

  private val saltSize = 32

  private def createSalt(): F[Salt] =
    Applicative[F].pure {
      val saltBytes = new Array[Byte](saltSize)
      nextBytes(saltBytes)
      Salt(Hex.toHexString(saltBytes))
    }

  /** Changing this function will invalidate every password in the database. **/
  private def saltPassword(salt: Salt, password: String): String =
    salt.value + password
}


package news.pastthefold.dao

import news.pastthefold.model.{Salt, User}
import tsec.passwordhashers.PasswordHash
import tsec.passwordhashers.jca.HardenedSCrypt

trait UserAuthDAO[F[_]] {
  def findByEmail(email: String): F[User]

  def updatePassword(user: User, salt: Salt, passwordHash: PasswordHash[HardenedSCrypt]): F[User]
}

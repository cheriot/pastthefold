package news.pastthefold.dao

import news.pastthefold.model.User

trait UserAuthDAO[F[_]] {
  def findByEmail(email: String): F[User]
}

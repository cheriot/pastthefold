package news.pastthefold.auth

import news.pastthefold.model.{Salt, User}
import tsec.passwordhashers.PasswordHash

object FakeData {
  val user = User(4, "fake@email.com", PasswordHash("passwordHash"), Salt("salt"))
}

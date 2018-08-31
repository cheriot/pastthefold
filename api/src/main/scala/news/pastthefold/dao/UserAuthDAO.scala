package news.pastthefold.dao

import cats.data.OptionT
import cats.effect.Sync
import cats.implicits._
import news.pastthefold.model.{Salt, User}
import tsec.authentication.BackingStore
import tsec.passwordhashers.PasswordHash
import tsec.passwordhashers.jca.HardenedSCrypt

import scala.collection.mutable

trait UserAuthDAO[F[_]] extends BackingStore[F, Int, User] {
  def findByEmail(email: String): F[User]

  def updatePassword(user: User, salt: Salt, passwordHash: PasswordHash[HardenedSCrypt]): F[User]
}

class MemoryUserAuthDAO[F[_]: Sync] extends UserAuthDAO[F] {

  private val storageMap = mutable.HashMap.empty[Int, User]

  private val getId: User => Int = _.id

  override def findByEmail(email: String): F[User] =
    Sync[F].pure(
      storageMap.values.find(_.email === email).get
    )

  override def updatePassword(user: User, salt: Salt, passwordHash: PasswordHash[HardenedSCrypt]): F[User] =
    update(user.copy(salt = salt, passwordHash = passwordHash))

  def put(elem: User): F[User] = {
    val map = storageMap.put(getId(elem), elem)
    if (map.isEmpty)
      Sync[F].pure(elem)
    else
      Sync[F].raiseError(new IllegalArgumentException)
  }

  def get(id: Int): OptionT[F, User] =
    OptionT.fromOption[F](storageMap.get(id))

  def update(v: User): F[User] = {
    storageMap.update(getId(v), v)
    Sync[F].pure(v)
  }

  def delete(id: Int): F[Unit] =
    storageMap.remove(id) match {
      case Some(_) => Sync[F].unit
      case None    => Sync[F].raiseError(new IllegalArgumentException)
    }
}

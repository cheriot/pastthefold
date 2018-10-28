package news.pastthefold.dao

import cats.data._
import cats.effect.Sync
import cats.implicits._
import news.pastthefold.auth.PasswordHashingService.EncryptedPassword
import news.pastthefold.model.{Salt, User}
import tsec.authentication.BackingStore
import tsec.passwordhashers.PasswordHash
import tsec.passwordhashers.jca.HardenedSCrypt

import scala.collection.mutable

trait UserAuthDAO[F[_]] extends BackingStore[F, Int, User] {

  sealed trait UserAuthDaoError
  final object AccountAlreadyExistsError extends UserAuthDaoError

  def create(email: String, encryptedPassword: EncryptedPassword): EitherT[F, UserAuthDaoError, User]

  def findByEmail(email: String): F[Option[User]]

  def updatePassword(user: User, salt: Salt, passwordHash: PasswordHash[HardenedSCrypt]): F[User]
}

class MemoryUserAuthDAO[F[_] : Sync] extends UserAuthDAO[F] {

  private var nextId = 1
  private def allocateId(): Int = {
    val id = nextId
    nextId += 1
    id
  }

  private val storageMap = mutable.HashMap.empty[Int, User]

  private val getId: User => Int = _.id

  override def create(email: String, encryptedPassword: EncryptedPassword): EitherT[F, UserAuthDaoError, User] = {
    val (salt, passwordHash) = encryptedPassword

    val result = findByEmail(email)
      .flatMap {
        case None => {
          put(User(allocateId(), email, passwordHash, salt))
            .map(Either.right[UserAuthDaoError, User])
        }
        case Some(_) => {
          Sync[F].pure(Either.left[UserAuthDaoError, User](AccountAlreadyExistsError))
        }
      }

    EitherT(result)
  }

  override def findByEmail(email: String): F[Option[User]] =
    Sync[F].pure(
      storageMap.values.find(_.email === email)
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
      case None => Sync[F].raiseError(new IllegalArgumentException)
    }
}

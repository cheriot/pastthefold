package news.pastthefold.dao

import java.util.UUID

import cats._
import cats.data.OptionT
import cats.effect.Effect
import tsec.authentication.{AuthenticatedCookie, BackingStore}
import tsec.mac.jca.HMACSHA256

import scala.collection.mutable

trait SessionDAO[F[_]] extends BackingStore[F, UUID, AuthenticatedCookie[HMACSHA256, Int]] {
  type Cookie = AuthenticatedCookie[HMACSHA256, Int]
}

object SessionDAO {
  def apply[F[_]: Effect](): SessionDAO[F] =
    new MemorySessionDAO[F]
}

class MemorySessionDAO[F[_] : Effect] extends SessionDAO[F] {

  private val storageMap = mutable.HashMap.empty[UUID, Cookie]

  val getId: Cookie => UUID = _.id

  def put(elem: Cookie): F[Cookie] = {
    val map = storageMap.put(getId(elem), elem)
    if (map.isEmpty)
      Monad[F].pure(elem)
    else
      MonadError[F, Throwable].raiseError(new IllegalArgumentException)
  }

  def get(id: UUID): OptionT[F, Cookie] =
    OptionT.fromOption[F](storageMap.get(id))

  def update(v: Cookie): F[Cookie] = {
    storageMap.update(getId(v), v)
    Monad[F].pure(v)
  }

  def delete(id: UUID): F[Unit] =
    storageMap.remove(id) match {
      case Some(_) => Monad[F].unit
      case None    => MonadError[F, Throwable].raiseError(new IllegalArgumentException)
    }
}

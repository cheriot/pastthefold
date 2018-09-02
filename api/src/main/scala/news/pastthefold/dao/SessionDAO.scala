package news.pastthefold.dao

import java.util.UUID

import cats._
import cats.data.OptionT
import cats.effect.Effect
import news.pastthefold.auth.SecureRequestService.AuthCookie
import tsec.authentication.BackingStore

import scala.collection.mutable

trait SessionDAO[F[_]] extends BackingStore[F, UUID, AuthCookie] {
}

object SessionDAO {
  def apply[F[_]: Effect](): SessionDAO[F] =
    new MemorySessionDAO[F]
}

class MemorySessionDAO[F[_] : Effect] extends SessionDAO[F] {

  private val storageMap = mutable.HashMap.empty[UUID, AuthCookie]

  val getId: AuthCookie => UUID = _.id

  def put(elem: AuthCookie): F[AuthCookie] = {
    val map = storageMap.put(getId(elem), elem)
    if (map.isEmpty)
      Monad[F].pure(elem)
    else
      MonadError[F, Throwable].raiseError(new IllegalArgumentException)
  }

  def get(id: UUID): OptionT[F, AuthCookie] =
    OptionT.fromOption[F](storageMap.get(id))

  def update(v: AuthCookie): F[AuthCookie] = {
    storageMap.update(getId(v), v)
    Monad[F].pure(v)
  }

  def delete(id: UUID): F[Unit] =
    storageMap.remove(id) match {
      case Some(_) => Monad[F].unit
      case None    => MonadError[F, Throwable].raiseError(new IllegalArgumentException)
    }
}

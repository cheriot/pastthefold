package news.pastthefold.auth

import java.util.UUID

import cats.data.OptionT
import cats.effect.Sync
import tsec.authentication.{AuthenticatedCookie, BackingStore}
import tsec.mac.jca.HMACSHA256

import scala.collection.mutable


object ExampleAuthHelpers {
  def dummyBackingStore[F[_], I, V](getId: V => I)(implicit F: Sync[F]) = new BackingStore[F, I, V] {
    private val storageMap = mutable.HashMap.empty[I, V]

    def put(elem: V): F[V] = {
      val map = storageMap.put(getId(elem), elem)
      if (map.isEmpty)
        F.pure(elem)
      else
        F.raiseError(new IllegalArgumentException)
    }

    def get(id: I): OptionT[F, V] =
      OptionT.fromOption[F](storageMap.get(id))

    def update(v: V): F[V] = {
      storageMap.update(getId(v), v)
      F.pure(v)
    }

    def delete(id: I): F[Unit] =
      storageMap.remove(id) match {
        case Some(_) => F.unit
        case None    => F.raiseError(new IllegalArgumentException)
      }
  }

  def cookieBackingStore[F[_]: Sync]: BackingStore[F, UUID, AuthenticatedCookie[HMACSHA256, Int]] =
    dummyBackingStore[F, UUID, AuthenticatedCookie[HMACSHA256, Int]](_.id)

  // We create a way to store our users. You can attach this to say, your doobie accessor
  // def userStore[F[_]]: BackingStore[F, Int, User] =
  //   dummyBackingStore[F, Int, User](_.id)

}



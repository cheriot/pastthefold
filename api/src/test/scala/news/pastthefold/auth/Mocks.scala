package news.pastthefold.auth

import java.util.UUID

import cats.data.OptionT
import cats.effect.{IO, Sync}
import news.pastthefold.auth.SecureRequestService.AuthCookie
import news.pastthefold.dao.UserAuthDAO
import news.pastthefold.model.{Salt, UntrustedPassword, User}
import org.http4s.{HttpService, Response}
import tsec.authentication.{BackingStore, UserAwareService}
import tsec.common.{VerificationFailed, VerificationStatus}
import tsec.passwordhashers.PasswordHash
import tsec.passwordhashers.jca.HardenedSCrypt

import scala.collection.mutable

object Mocks {

  class MockUserAuthDAO extends UserAuthDAO[IO] {
    override def findByEmail(email: String): IO[User] = ???
    override def updatePassword(user: User, salt: Salt, passwordHash: PasswordHash[HardenedSCrypt]): IO[User] = ???
    override def put(elem: User): IO[User] = ???
    override def update(v: User): IO[User] = ???
    override def delete(id: Int): IO[Unit] = ???
    override def get(id: Int): OptionT[IO, User] = ???
  }

  def buildUserAuthDAO(userOpt: Option[User] = None) = new MockUserAuthDAO {
    override def findByEmail(email: String): IO[User] =
      if (userOpt.isDefined) IO { userOpt.get }
      else IO.raiseError(new Throwable("user not found"))

    override def get(id: Int): OptionT[IO, User] = OptionT(IO.pure(userOpt))
  }

  def buildPasswordHashingService(
                                   verifyResult: VerificationStatus = VerificationFailed
                                 ) = new PasswordHashingService[IO] {
    override def hashPassword(password: String): IO[(Salt, PasswordHash[HardenedSCrypt])] =
      IO.apply {
        (Salt("fake-salt"), PasswordHash(s"hash-of-$password"))
      }

    override def verifyPassword(salt: Salt, password: UntrustedPassword, passwordHash: PasswordHash[HardenedSCrypt]): IO[VerificationStatus] =
      IO.pure(verifyResult)
  }

  def buildSecureRequestService = new SecureRequestService[IO] {
    override def embedAuth(user: User, response: Response[IO]): IO[Response[IO]] = ???
    override def liftUserAware(service: UserAwareService[User, AuthCookie, IO]): HttpService[IO] = ???
  }

  class MockBackingStore[F[_], Id, Value] extends BackingStore [F, Id, Value] {
    override def put(elem: Value): F[Value] = ???
    override def update(v: Value): F[Value] = ???
    override def delete(id: Id): F[Unit] = ???
    override def get(id: Id): OptionT[F, Value] = ???
  }

  class MockCookieStore[F[_]] extends MockBackingStore[F, UUID, AuthCookie]

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

  def cookieBackingStore[F[_]: Sync]: BackingStore[F, UUID, AuthCookie] =
    dummyBackingStore[F, UUID, AuthCookie](_.id)

  def userBackingStore[F[_]: Sync]: BackingStore[F, Int, User] =
    dummyBackingStore[F, Int, User](_.id)

}

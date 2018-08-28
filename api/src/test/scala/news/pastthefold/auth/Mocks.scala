package news.pastthefold.auth

import cats.data.OptionT
import cats.effect.IO
import news.pastthefold.dao.UserAuthDAO
import news.pastthefold.model.{Salt, UntrustedPassword, User}
import org.http4s.Response
import tsec.common.{VerificationFailed, VerificationStatus}
import tsec.passwordhashers.PasswordHash
import tsec.passwordhashers.jca.HardenedSCrypt

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
  }
}

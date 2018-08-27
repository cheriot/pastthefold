package news.pastthefold.model

import cats.{Eq, MonadError}
import tsec.authorization.{AuthGroup, AuthorizationInfo, SimpleAuthEnum}
import tsec.passwordhashers.PasswordHash
import tsec.passwordhashers.jca.HardenedSCrypt

sealed case class Role(roleRepr: String)

object Role extends SimpleAuthEnum[Role, String] {

  val Administrator: Role = Role("Administrator")
  val Reader: Role        = Role("Reader")
  val Seller: Role        = Role("Seller")
  val CorruptedData       = Role("corrupted")


  implicit val E: Eq[Role] = Eq.fromUniversalEquals[Role]

  val getRepr: Role => String = _.roleRepr

  val orElse: Role = CorruptedData

  protected val values: AuthGroup[Role] = AuthGroup(Administrator, Reader, Seller)
}

case class Salt(val value: String) extends AnyVal

case class UntrustedPassword(val value: String) extends AnyVal

case class User(
                 id: Int,
                 email: String,
                 passwordHash: PasswordHash[HardenedSCrypt],
                 salt: Salt,
                 role: Role = Role.Reader
               )

object User {
  implicit def authRole[F[_]](implicit F: MonadError[F, Throwable]): AuthorizationInfo[F, Role, User] =
    new AuthorizationInfo[F, Role, User] {
      def fetchInfo(u: User): F[Role] = F.pure(u.role)
    }
}

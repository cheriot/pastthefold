package news.pastthefold.auth

import org.http4s.Status
import org.http4s.Status._

sealed abstract class LoginError(val status: Status)
case object NoEmailError extends LoginError(BadRequest)
case object NoPasswordError extends LoginError(BadRequest)
case object WrongPasswordError extends LoginError(Unauthorized)

sealed abstract class UpdatePasswordError(val status: Status)
case class InvalidCredentialsError(source: LoginError) extends UpdatePasswordError(Unauthorized)
case class PasswordRequirementsError() extends UpdatePasswordError(BadRequest)
case class UnknownError(source: Throwable) extends UpdatePasswordError(Forbidden)

sealed case class CreatePasswordError(val status: Status = BadRequest)

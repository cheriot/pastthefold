package news.pastthefold.auth

import org.http4s.Status
import org.http4s.Status._


trait HttpStatusError {
  def status: Status
}

sealed abstract class AuthError(val status: Status) extends HttpStatusError

final object AccountEmailError extends AuthError(BadRequest)
final object AccountPasswordError extends AuthError(BadRequest)
final object AccountDuplicateError extends AuthError(BadRequest)

final object NoEmailError extends AuthError(BadRequest)
final object NoPasswordError extends AuthError(BadRequest)
final object EmailNotFoundError extends AuthError(Unauthorized)
final object WrongPasswordError extends AuthError(Unauthorized)

final class InvalidCredentialsError(val source: AuthError) extends AuthError(Unauthorized)
final object PasswordRequirementsError extends AuthError(BadRequest)
final class UnknownError(val source: Throwable) extends AuthError(Forbidden)

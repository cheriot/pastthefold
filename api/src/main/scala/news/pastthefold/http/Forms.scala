package news.pastthefold.http

import news.pastthefold.model.UntrustedPassword

case class LoginForm(email: String, password: UntrustedPassword)

case class UpdatePasswordForm(email: String, oldPassword: UntrustedPassword, nextPassword: String) {
  def toLoginForm = LoginForm(email, oldPassword)
}

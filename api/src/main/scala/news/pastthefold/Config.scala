package news.pastthefold

import cats.Id
import com.typesafe.config._
import tsec.mac.jca.{HMACSHA256, MacSigningKey}
import org.bouncycastle.util.encoders.Hex
import scala.concurrent.duration._

object Config {

  private val secretsConfig = ConfigFactory.load("secrets")

  val cookieSigningKey: MacSigningKey[HMACSHA256] = {
    val hex = secretsConfig.getString("cookieSigningKey")
    val bytes: Array[Byte] = Hex.decode(hex)
    require(bytes.length == 32)
    HMACSHA256.buildKey[Id](bytes)
  }

  val idleTimeout = 2.seconds
}

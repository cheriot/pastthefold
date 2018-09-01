package news.pastthefold

import tsec.mac.jca.{HMACSHA256, MacSigningKey}
import utest._

object ConfigTest extends TestSuite {

  val tests = Tests {
    "reads secrets" - {
      val cookieSigningKey: MacSigningKey[HMACSHA256] = Config.cookieSigningKey
      val alg = cookieSigningKey.toJavaKey.getAlgorithm
      assert(alg == "HmacSHA256")
      val fmt = cookieSigningKey.toJavaKey.getFormat
      assert(fmt == "RAW")
    }
  }

}

package j2j

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class ConfigParserSpec extends AnyFreeSpec with Matchers {

  "Simple expression" - {

    "Json path" in {
      val input  = "$.hello['world'][*][2][-2][3:9][3:][:9][-5:-10].*"
      val output = $ / "hello" / "world" / * / 2 / -2 / (3, 9) / (3, *) / (*, 9) / (-5, -10) / *

      JsonPath.parse(input) shouldBe Right(output)
    }

  }

}

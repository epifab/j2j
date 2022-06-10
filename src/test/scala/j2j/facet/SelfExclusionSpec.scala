package j2j.facet

import org.scalatest.freespec.AnyFreeSpec

import scala.io.Source
import io.circe.parser.parse as parseJson
import j2j.facet.model.OutputEvent
import org.scalatest.matchers.should.Matchers

class SelfExclusionSpec extends AnyFreeSpec with Matchers {
  val inputStr          = Source.fromResource("excluded/in/indefinite.json").mkString
  val expectedOutputStr = Source.fromResource("excluded/out/indefinite.json").mkString

  "Indefinite" in {
    (for {
      inputJson <- parseJson(inputStr)
      output    <- SelfExclusion.extract(inputJson)
    } yield output) shouldBe parseJson(expectedOutputStr).flatMap(_.as[OutputEvent])
  }

}

package j2j

import io.circe.Json
import io.circe.parser.parse as parseJson
import j2j.EvaluationSyntax.*
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks

import java.time.Instant

class JsonTransformerSpec extends AnyFreeSpec with Matchers with TableDrivenPropertyChecks {

  "Full json transformation" in {

    val inputStr =
      """
        |{
        |  "this": "is",
        |  "a": [
        |    "test",
        |    7357,
        |    ["t", "e", "s", "t"],
        |    {
        |      "t": 7,
        |      "e": 3,
        |      "s": 5
        |    }
        |  ]
        |}
        |""".stripMargin

    val expectedOutput =
      """
        |{
        |  "v1": 1,
        |  "v2": 2,
        |  "processedBy": "j2j",
        |  "code-7357": 7357,
        |  "letters": ["t", "e", "s", "t"],
        |  "glossary": {
        |    "t": 7,
        |    "e": 3,
        |    "s": 5
        |  },
        |  "t": 7,
        |  "e": 3,
        |  "s": 5,
        |  "assertions": ["e=3"]
        |}
        |""".stripMargin

    val input  = parseJson(inputStr).getOrElse(fail("Invalid JSON in input"))
    val output = MyTransformer(input)

    output shouldBe parseJson(expectedOutput)

  }

}

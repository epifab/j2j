package j2j

import io.circe.Json
import io.circe.parser.parse as parseJson
import j2j.syntax.*
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
        |  "processedAt": "2021-03-14T15:09:00Z",
        |  "processedBy": "j2j",
        |  "test": 7357,
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

    val generate = new Template {

      override def apply(implicit json: Json): Either[String, Json] = {

        val a        = $ / "a"
        val test     = a / 0
        val code     = a / 1
        val letters  = a / 2 / *
        val glossary = a / 3
        val t        = glossary / "t"
        val e        = glossary / "e"
        val s        = glossary / "s"
        val assertions = ExpressionList(
          Value("e=3").when(e matches Value(3)),
          Value("e=4").when(e matches Value(4)),
        )

        val time   = Variable(Instant.now)
        val author = Value("Epifab")

        json"""
          |{
          |  "processedAt": $time,
          |  "processedBy": $author,
          |  $test: $code,
          |  "letters": $letters,
          |  "glossary": $glossary,
          |  "t": $t,
          |  "e": $e,
          |  "s": $s,
          |  "assertions": $assertions
          |}
          |"""
      }
    }

    val output = generate(parseJson(inputStr).getOrElse(fail("Invalid JSON in input")))
    output shouldBe parseJson(expectedOutput)

  }

}

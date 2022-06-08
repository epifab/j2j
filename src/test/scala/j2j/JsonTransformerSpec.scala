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

    val transformer: Transformer = new Transformer {

      private val a        = $ / "a"
      private val code     = a / 1
      private val letters  = a / 2 / *
      private val glossary = a / 3
      private val t        = glossary / "t"
      private val e        = glossary / "e"
      private val s        = glossary / "s"
      private val assertions = ExpressionList(
        Value("e=3").when(e matches Value(3)),
        Value("e=4").when(e matches Value(4)),
      )

      val test: Expression = for {
        tCode <- t.as[Int]
        eCode <- e.as[Int]
        sCode <- s.as[Int]
      } yield s"code-$tCode$eCode$sCode$tCode"

      private val versions = Iterator.from(0)
      private val version  = Variable(versions.next).as[Int].map(_ + 1)
      private val author   = Value("j2j")

      override def apply(implicit json: Json): Either[EvaluationError, Json] = {

        json"""
          |{
          |  "v1": $version,
          |  "v2": $version,
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

    val input  = parseJson(inputStr).getOrElse(fail("Invalid JSON in input"))
    val output = transformer(input)

    output shouldBe parseJson(expectedOutput)

  }

}

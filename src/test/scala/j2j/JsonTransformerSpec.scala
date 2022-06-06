package j2j

import io.circe.Json
import io.circe.parser.parse as parseJson
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks

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

    val mappingsStr =
      """
        |test = "$.a[0]"
        |code = "$.a[1]"
        |letters = "$.a[2].*"
        |glossary = "$.a[3]"
        |t = "%{glossary}.t"
        |e = "%{glossary}.e"
        |s = "%{glossary}.s"
        |""".stripMargin

    val templateStr =
      """
        |{
        |  "processedAt": "%{time}",
        |  "processedBy": "%{author}",
        |  "%{test}": "%{code}",
        |  "letters": "%{letters}",
        |  "glossary": "%{glossary}",
        |  "t": "%{t}",
        |  "e": "%{e}",
        |  "s": "%{s}"
        |}
        |""".stripMargin

    val superContext: PartialFunction[String, Json] = {
      case "time"   => Json.fromString("2021-03-14T15:09:00Z")
      case "author" => Json.fromString("j2j")
    }

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
        |  "s": 5
        |}
        |""".stripMargin

    val results = for {
      mappings <- NamedExpression.loadList(mappingsStr.split("\n").toList)
      template <- parseJson(templateStr)
      done     <- new JsonTransformer(mappings, template, superContext)(inputStr)
    } yield done

    results shouldBe parseJson(expectedOutput)

  }

}

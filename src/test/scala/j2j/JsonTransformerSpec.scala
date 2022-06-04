package j2j

import io.circe.Json
import io.circe.parser.parse as parseJson
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks
import pureconfig.ConfigSource

class JsonTransformerSpec extends AnyFreeSpec with Matchers with TableDrivenPropertyChecks {
  private val conf =
    """
      |foo = "%{foo}"
      |bar = "$.bar[0]"
      |baz = {
      |  value = "BAZ!"
      |  when = { src = "%{foo}", equals = "$.bar" }
      |}
      |""".stripMargin

  private val scenarios = Table(
    ("hint", "json", "context", "result"),
    (
      "Baz: true",
      """{"bar": [99]}""",
      Map("foo" -> Json.fromInt(99)),
      Map("foo" -> Json.fromInt(99), "bar" -> Json.fromInt(99), "baz" -> Json.fromString("BAZ!")),
    ),
    (
      "Baz: false",
      """{"bar": [100]}""",
      Map("foo" -> Json.fromInt(99)),
      Map("foo" -> Json.fromInt(99), "bar" -> Json.fromInt(100), "baz" -> Json.Null),
    ),
  )

  forAll(scenarios) { case (hint, jsonStr, context, expectedResult) =>
    hint in {
      val result = for {
        expressions <- ConfigSource.string(conf).load[Map[String, Expression]]
        json        <- parseJson(jsonStr)
      } yield JsonTransformer.lookup(expressions, json, context)

      result shouldBe Right(expectedResult)
    }
  }

  "Full json transformation" in {

    val inputJson =
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

    val mappings =
      """
        |test = "$.a[0]",
        |code = "$.a[1]",
        |letters = "$.a[2].*",
        |glossary = "$.a[3]",
        |t = "$.a[3].t",
        |e = "$.a[3].e",
        |s = "$.a[3].s"
        |""".stripMargin

    val templateJson =
      """
        |{
        |  "%{test}": "%{code}",
        |  "letters": "%{letters}",
        |  "glossary": "%{glossary}"
        |}
        |""".stripMargin

    val expectedOutput =
      """
        |{
        |  "test": 7357,
        |  "letters": ["t", "e", "s", "t"],
        |  "glossary": {
        |    "t": 7,
        |    "e": 3,
        |    "s": 5
        |  }
        |}
        |""".stripMargin

    JsonTransformer(
      inputJsonStr = inputJson,
      templateJsonStr = templateJson,
      expressionsStr = mappings,
    ) shouldBe parseJson(expectedOutput)

  }

}

package j2j

import io.circe.Json
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks
import pureconfig.ConfigSource

class ContextLookupSpec extends AnyFreeSpec with Matchers with TableDrivenPropertyChecks {
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

  forAll(scenarios) { case (hint, json, context, expectedResult) =>
    hint in {
      val result = for {
        evaluable <- ConfigSource.string(conf).load[Map[String, Expression]]
        variables <- ContextLookup(evaluable).lookup(json, context)
      } yield variables

      result shouldBe Right(expectedResult)
    }
  }

}

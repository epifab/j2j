package j2j

import io.circe.Json
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks
import pureconfig.ConfigSource

class ContextLookupSpec extends AnyFreeSpec with Matchers with TableDrivenPropertyChecks {

  private val scenarios = Table(
    ("hint", "config", "input", "expected output"),
    (
      "Evaluation of a constant (string)",
      """test = "A string"""",
      "{}",
      Map("test" -> Json.fromString("A string")),
    ),
    (
      "Evaluation of a constant (int)",
      """test = 123""",
      "{}",
      Map("test" -> Json.fromInt(123)),
    ),
    (
      "Evaluation of a constant (double)",
      """test = 1.23""",
      "{}",
      Map("test" -> Json.fromDoubleOrNull(1.23)),
    ),
    (
      "Evaluation of a constant (long)",
      """test = 12345678910""",
      "{}",
      Map("test" -> Json.fromLong(12345678910L)),
    ),
    (
      "Evaluation of a object property",
      """test = "$.foo"""",
      """{"foo": "bar"}""",
      Map("test" -> Json.fromString("bar")),
    ),
    (
      "Evaluation of a nested object property",
      """test = "$.foo.bar"""",
      """{"foo": {"bar": "baz"}}""",
      Map("test" -> Json.fromString("baz")),
    ),
    (
      "Evaluation of an array element",
      """test = "$[0]"""",
      """["bar"]""",
      Map("test" -> Json.fromString("bar")),
    ),
    (
      "Evaluation of a nested array element",
      """test = "$[1][0]"""",
      """[[1, 2], [3, 4]]""",
      Map("test" -> Json.fromInt(3)),
    ),
    (
      "Evaluation of an object property inside an array element",
      """test = "$[0].foo"""",
      """[{"foo": "bar"}]""",
      Map("test" -> Json.fromString("bar")),
    ),
    (
      "Evaluation of an array element inside an object property",
      """test = "$.foo[2]"""",
      """{"foo": [1, 2, 3]}""",
      Map("test" -> Json.fromInt(3)),
    ),
    (
      "Evaluation of an array element out of range",
      """test = "$.foo[4]"""",
      """{"foo": [1, 2, 3]}""",
      Map("test" -> Json.Null),
    ),
    (
      "Evaluation of an array element (negative index)",
      """test = "$.foo[-2]"""",
      """{"foo": [1, 2, 3]}""",
      Map("test" -> Json.fromInt(2)),
    ),
    (
      "Evaluation of an array element out of range (negative index)",
      """test = "$.foo[-4]"""",
      """{"foo": [1, 2, 3]}""",
      Map("test" -> Json.Null),
    ),
    (
      "Evaluation of a wildcard on an object",
      """test = "$.*.baz"""",
      """{"foo": {"baz": 1}, "bar": {"baz": 2, "qux": 3}}""",
      Map("test" -> Json.arr(Json.fromInt(1), Json.fromInt(2))),
    ),
    (
      "Evaluation of a wildcard on an array",
      """test = "$.*.baz"""",
      """[{"baz": 1}, {"baz": 2, "qux": 3}]""",
      Map("test" -> Json.arr(Json.fromInt(1), Json.fromInt(2))),
    ),
    (
      "Evaluation of a array range",
      """test = "$[1:2]"""",
      """[0,1,2,3]""",
      Map("test" -> Json.arr(Json.fromInt(1), Json.fromInt(2))),
    ),
    (
      "Evaluation of a array range (open end)",
      """test = "$[1:]"""",
      """[0,1,2,3]""",
      Map("test" -> Json.arr(Json.fromInt(1), Json.fromInt(2), Json.fromInt(3))),
    ),
    (
      "Evaluation of a array range (to: out of bound)",
      """test = "$[1:6]"""",
      """[0,1,2,3]""",
      Map("test" -> Json.arr(Json.fromInt(1), Json.fromInt(2), Json.fromInt(3))),
    ),
    (
      "Evaluation of a array range (to: negative)",
      """test = "$[1:-1]"""",
      """[0,1,2,3]""",
      Map("test" -> Json.arr(Json.fromInt(1), Json.fromInt(2))),
    ),
    (
      "Evaluation of a array range (to: negative, one element selected)",
      """test = "$[1:-2]"""",
      """[0,1,2,3]""",
      Map("test" -> Json.arr(Json.fromInt(1))),
    ),
    (
      "Evaluation of a array range (inverted from and to)",
      """test = "$[2:1]"""",
      """[0,1,2,3]""",
      Map("test" -> Json.arr()),
    ),
    (
      "Evaluation of a array range (from: negative)",
      """test = "$[-3:2]"""",
      """[0,1,2,3]""",
      Map("test" -> Json.arr(Json.fromInt(1), Json.fromInt(2))),
    ),
    (
      "Evaluation of a array range (negative)",
      """test = "$[-3:-1]"""",
      """[0,1,2,3]""",
      Map("test" -> Json.arr(Json.fromInt(1), Json.fromInt(2))),
    ),
    (
      "Evaluation of a array range (discard last element)",
      """test = "$[:-1]"""",
      """[0,1,2,3]""",
      Map("test" -> Json.arr(Json.fromInt(0), Json.fromInt(1), Json.fromInt(2))),
    ),
  )

  forAll(scenarios) { case (hint, conf, input, expectedOutput) =>
    hint in {
      val result = for {
        evaluable <- ConfigSource.string(conf).load[Map[String, Expression]]
        variables <- ContextLookup(evaluable).lookup(input)
      } yield variables

      result shouldBe Right(expectedOutput)
    }
  }
}

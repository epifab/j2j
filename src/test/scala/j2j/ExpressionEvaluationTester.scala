package j2j
import io.circe.parser.parse as parseJson
import io.circe.syntax.EncoderOps
import io.circe.{Encoder, Json}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks
import pureconfig.ConfigSource

case class Scenario private (
    hint: String,
    json: String,
    config: Expression,
    expectedOutput: Json,
)

object Scenario {
  def apply[A: Encoder](
      hint: String,
      json: String,
      expr: Expression,
      expectedOutput: A,
  ): Scenario =
    new Scenario(hint, json, expr, expectedOutput.asJson)
}

abstract class ExpressionEvaluationTester(scenarios: Scenario*) extends AnyFreeSpec with Matchers with TableDrivenPropertyChecks {

  forAll(Table("Scenario", scenarios*)) { case Scenario(hint, jsonString, expr, expectedOutput) =>
    hint in {
      val json = parseJson(jsonString).getOrElse(fail(s"invalid json: $jsonString"))
      new ExpressionEvaluator(json.hcursor).evaluateJson(expr) shouldBe expectedOutput
    }
  }

}

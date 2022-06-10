package j2j
import io.circe.parser.parse as parseJson
import io.circe.syntax.EncoderOps
import io.circe.{Encoder, Json}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks

case class Scenario private (
    hint: String,
    json: String,
    result: Json => Either[EvaluationError, Json],
    expectedOutput: Json,
)

object Scenario {
  def apply[A: Encoder](
      hint: String,
      json: String,
      expr: Expression[?],
      expectedOutput: A,
  ): Scenario =
    new Scenario(
      hint,
      json,
      json => expr.evalAsJson(json.hcursor),
      expectedOutput.asJson,
    )
}

abstract class ExpressionEvaluationTester(scenarios: Scenario*) extends AnyFreeSpec with Matchers with TableDrivenPropertyChecks {

  forAll(Table("Scenario", scenarios*)) { case Scenario(hint, jsonString, eval, expectedOutput) =>
    hint in {
      val json = parseJson(jsonString).getOrElse(fail(s"invalid json: $jsonString"))
      eval(json) shouldBe Right(expectedOutput)
    }
  }

}

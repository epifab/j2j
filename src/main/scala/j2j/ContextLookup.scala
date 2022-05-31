package j2j

import io.circe
import io.circe.Json
import io.circe.parser.{parse => parseJson}

class ContextLookup(evaluable: Map[String, Expression]) {
  def lookup(jsonText: String): Either[circe.Error, Map[String, Json]] =
    parseJson(jsonText).map(lookup)

  def lookup(json: Json): Map[String, Json] = {
    val evaluator = new ExpressionEvaluator(json.hcursor)
    evaluable.toVector.map { case (key, expression) =>
      key -> evaluator.evaluateJson(expression)
    }.toMap
  }
}

object ContextLookup {
  def apply(evaluable: Map[String, Expression]): ContextLookup = new ContextLookup(evaluable)
}

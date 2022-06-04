package j2j

import io.circe
import io.circe.Json
import io.circe.parser.parse as parseJson

class ContextLookup(evaluable: Map[String, Expression]) {
  def lookup(jsonText: String, context: Map[String, Json]): Either[circe.Error, Map[String, Json]] =
    parseJson(jsonText).map(lookup(_, context))

  def lookup(json: Json, context: Map[String, Json]): Map[String, Json] = {
    val evaluator = new ExpressionEvaluator(json.hcursor, context)
    evaluable.toVector.map { case (key, expression) =>
      key -> evaluator.evaluateJson(expression)
    }.toMap
  }
}

object ContextLookup {
  def apply(evaluable: Map[String, Expression]): ContextLookup = new ContextLookup(evaluable)
}

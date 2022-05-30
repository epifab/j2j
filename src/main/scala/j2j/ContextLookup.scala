package j2j

import cats.syntax.traverse.toTraverseOps
import io.circe
import io.circe.Json
import io.circe.parser.{parse => parseJson}

class ContextLookup(variables: Map[String, Expression[_]]) {
  def lookup(jsonText: String): Either[circe.Error, Map[String, Json]] = {
    for {
      json <- parseJson(jsonText)
      evaluator = new ExpressionEvaluator(json.hcursor)
      evaluated <- variables.toVector.traverse { case (key, expression) =>
        evaluator.evaluateJson(expression).map(key -> _)
      }
    } yield evaluated.toMap
  }
}

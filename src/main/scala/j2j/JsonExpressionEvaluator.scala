package j2j

import io.circe.{ACursor, Json}

trait JsonExpressionEvaluator {
  def evalAsJson(root: ACursor): Either[EvaluationError, Json]
}

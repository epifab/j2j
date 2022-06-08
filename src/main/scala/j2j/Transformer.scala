package j2j

import io.circe.Json

trait Transformer {
  def apply(implicit json: Json): Either[EvaluationError, Json]
}

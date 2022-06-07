package j2j

import io.circe.Json

trait Template {
  def apply(implicit json: Json): Either[String, Json]
}

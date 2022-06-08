package j2j

import io.circe.Json

import scala.reflect.runtime.universe.{TypeTag, typeTag}

sealed trait EvaluationError

case class ExtractionError[T: TypeTag](json: Json)
    extends RuntimeException(s"Cannot extract ${json.noSpaces} as ${typeTag[T].tpe}")
    with EvaluationError

case class TemplateError(jsonString: String, reason: String)
    extends RuntimeException(s"The template did not produce a valid JSON ($reason): $jsonString")
    with EvaluationError

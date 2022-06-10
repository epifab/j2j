package j2j

import io.circe.{ACursor, Json, Printer}

import scala.reflect.runtime.universe.{TypeTag, typeTag}

sealed trait EvaluationError

case class ExtractionError[T: TypeTag](json: Json, expr: Expression[?], root: ACursor)
    extends RuntimeException(
      s"Cannot extract ${json.noSpaces} as ${typeTag[T].tpe}" +
        s" while evaluating $expr" +
        s" on:\n${root.focus.map(_.printWith(Printer.spaces2)).getOrElse(Json.Null)}",
    )
    with EvaluationError

case class TemplateError(jsonString: String, reason: String)
    extends RuntimeException(s"The template did not produce a valid JSON ($reason): $jsonString")
    with EvaluationError

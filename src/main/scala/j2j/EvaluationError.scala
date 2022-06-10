package j2j

import io.circe.{ACursor, Json, Printer}

sealed trait EvaluationError

case class ExtractionError[T: TypeString](json: Json, expr: Expression[?], root: ACursor)
    extends RuntimeException(
      s"Cannot parse ${json.noSpaces} as ${TypeString.apply[T]}" +
        s" while evaluating:" +
        expr.printToNewLine(PrinterContext(indent = 0)) +
        s" on:\n${root.focus.map(_.printWith(Printer.spaces2)).getOrElse(Json.Null)}",
    )
    with EvaluationError

case class TemplateError(jsonString: String, reason: String)
    extends RuntimeException(s"The template did not produce a valid JSON ($reason): $jsonString")
    with EvaluationError

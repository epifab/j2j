package j2j

import io.circe.{ACursor, Decoder, Json}
import j2j.ConsoleOps.*

case class ConditionalExpression[T: Decoder: TypeString](
    value: Expression[T],
    when: Expression[Boolean],
    defaultTo: Expression[T],
) extends Expression[T] {

  override def evalAsJson(root: ACursor): Either[EvaluationError, Json] =
    when.evalT(root).flatMap {
      case true  => value.evalAsJson(root)
      case false => defaultTo.evalAsJson(root)
    }

  override def print(implicit context: PrinterContext): String =
    blue(bold("if")) +
      indent(when) +
      newLine(blue(bold("then"))) +
      indent(value) +
      newLine(blue(bold("else"))) +
      indent(defaultTo)

}

package j2j

import io.circe.{ACursor, Decoder, Json}
import j2j.ConsoleOps.*

case class CastedExpression[T: Decoder: TypeString](expression: Expression[?]) extends Expression[T] {

  override def evalAsJson(root: ACursor): Either[EvaluationError, Json] =
    expression.evalAsJson(root)

  override def print(implicit context: PrinterContext): String =
    blue(bold("cast")) +
      cyan(sqbr(TypeString.apply[T])) +
      indent(expression)

}

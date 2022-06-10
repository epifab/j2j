package j2j

import cats.implicits.toTraverseOps
import io.circe.{ACursor, Decoder, Json}
import j2j.ConsoleOps.*

case class FlatMappedExpression[T: Decoder: TypeString, U: Decoder: TypeString](expression: Expression[T], f: T => Expression[U])
    extends Expression[U] {

  override def evalAsJson(root: ACursor): Either[EvaluationError, Json] =
    expression
      .evalAsJson(root)
      .flatMap(eval(root, _))
      .flatMap(_.evalAsJson(root))

  private def eval(root: ACursor, json: Json) =
    json
      .as[T]
      .left
      .map(_ => ExtractionError[T](json, expression, root))
      .map(f)

  override def print(implicit context: PrinterContext): String =
    blue(bold("flatMap")) +
      cyan(sqbr(TypeString[T] + ", " + TypeString[U])) +
      indent(expression)

}

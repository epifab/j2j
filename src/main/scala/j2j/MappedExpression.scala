package j2j

import io.circe.{ACursor, Decoder, Encoder, Json}
import j2j.ConsoleOps.*

case class MappedExpression[T: TypeString: Decoder, U: TypeString: Encoder: Decoder](expression: Expression[T], f: T => U)
    extends Expression[U] {

  override def evalAsJson(root: ACursor): Either[EvaluationError, Json] =
    expression
      .evalAsJson(root)
      .flatMap(json => eval(root, json))

  private def eval(root: ACursor, json: Json) =
    json
      .as[T]
      .left
      .map(_ => ExtractionError[T](json, expression, root))
      .map(f)
      .map(Encoder[U].apply)

  override def print(implicit context: PrinterContext): String =
    blue(bold("map")) +
      cyan(sqbr(TypeString[T] + ", " + TypeString[U])) +
      indent(expression)

}

package j2j

import io.circe.{ACursor, Decoder, Encoder, Json}
import j2j.ConsoleOps.*
import j2j.props.Optionable

case class Value[T: Decoder: TypeString] private (encoded: Json) extends Expression[T] {

  override def evalAsJson(root: ACursor): Either[EvaluationError, Json] =
    Right(encoded)

  override def print(implicit context: PrinterContext): String =
    cyan(sqbr(TypeString[T])) + green(encoded.noSpaces)

}

object Value {

  def empty: Expression[Json] = new Value(Json.Null)

  def json(value: Json): Expression[Json] = new Value(value)

  def apply[T: Encoder: Decoder: TypeString](value: T): Expression[T] = Value(Encoder[T].apply(value))

}

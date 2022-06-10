package j2j

import io.circe.{ACursor, Decoder, Encoder, Json}
import j2j.ConsoleOps.*

case class Lambda[T: Decoder: TypeString] private (value: () => Json) extends Expression[T] {

  override def evalAsJson(root: ACursor): Either[EvaluationError, Json] =
    Value(value()).evalAsJson(root)

  override def print(implicit context: PrinterContext): String =
    cyan(sqbr(TypeString[T])) + magenta(bold("lambda()"))

}

object Lambda {
  def apply[T: Encoder: Decoder: TypeString](value: () => T): Lambda[T] = Lambda(() => Encoder[T].apply(value()))
}

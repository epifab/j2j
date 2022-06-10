package j2j

import cats.implicits.toTraverseOps
import io.circe.{ACursor, Decoder, Json}
import j2j.ConsoleOps.*

case class ExpressionVector[T: Decoder: TypeString](expressions: Vector[Expression[T]]) extends Expression[Vector[T]] {

  override def evalAsJson(root: ACursor): Either[EvaluationError, Json] =
    expressions.traverse(_.evalAsJson(root)).map(Json.arr)

  override def print(implicit context: PrinterContext): String =
    blue(bold("list")) + expressions.map(indent).mkString

}

object ExpressionVector {

  def apply[T: Decoder: TypeString](expressions: Expression[T]*): ExpressionVector[T] =
    new ExpressionVector(expressions.toVector)

}

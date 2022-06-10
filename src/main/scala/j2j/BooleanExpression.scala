package j2j

import io.circe.{ACursor, Json}
import j2j.BooleanExpression.*
import j2j.ConsoleOps.*
import j2j.props.Matcher

sealed trait BooleanExpression extends Expression[Boolean] {

  def &&(that: Expression[Boolean]): And = And(this, that)

  def ||(that: Expression[Boolean]): Or = Or(this, that)

  def unary_! : Not = Not(this)

  def evalBoolean(root: ACursor): Either[EvaluationError, Boolean]

  override def evalT(root: ACursor): Either[EvaluationError, Boolean] =
    evalBoolean(root)

  override def evalAsJson(root: ACursor): Either[EvaluationError, Json] =
    evalBoolean(root).map(Json.fromBoolean)

}

object BooleanExpression {

  sealed abstract class UnaryBooleanExpr(val name: String) extends BooleanExpression {
    def a: Expression[?]

    override def print(implicit context: PrinterContext): String =
      blue(bold(name)) + indent(a)
  }

  sealed abstract class BinaryBooleanExpr[A, B](val name: String) extends BooleanExpression {
    def a: Expression[?]
    def b: Expression[?]

    override def print(implicit context: PrinterContext): String =
      a.print + newLine(blue(bold(name))) + b.printToNewLine
  }

  case class Matches[A, B](a: Expression[A], b: Expression[B])(implicit cmp: Matcher[A, B]) extends BinaryBooleanExpr(cmp.name) {
    override def evalBoolean(root: ACursor): Either[EvaluationError, Boolean] =
      for {
        a <- a.evalT(root)
        b <- b.evalT(root)
      } yield cmp(a, b)
  }

  case class And(a: Expression[Boolean], b: Expression[Boolean]) extends BinaryBooleanExpr("and") {
    override def evalBoolean(root: ACursor): Either[EvaluationError, Boolean] =
      for {
        a <- a.evalT(root)
        b <- b.evalT(root)
      } yield a && b
  }

  case class Or(a: Expression[Boolean], b: Expression[Boolean]) extends BinaryBooleanExpr("or") {
    override def evalBoolean(root: ACursor): Either[EvaluationError, Boolean] =
      for {
        a <- a.evalT(root)
        b <- b.evalT(root)
      } yield a || b
  }

  case class Not(a: Expression[Boolean]) extends UnaryBooleanExpr("not") {
    override def evalBoolean(root: ACursor): Either[EvaluationError, Boolean] =
      a.evalT(root).map(!_)
  }

}

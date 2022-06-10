package j2j

import io.circe.Json

sealed trait BooleanExpression extends Printable {
  def and(that: BooleanExpression): BooleanExpression = BooleanExpression.And(this, that)
  def or(that: BooleanExpression): BooleanExpression  = BooleanExpression.Or(this, that)
  def unary_not: BooleanExpression                    = BooleanExpression.Not(this)
}

object BooleanExpression {

  sealed abstract class PrintableBooleanExpression2(name: String) extends BooleanExpression {
    def a: Printable
    def b: Printable

    override def prettyPrint(indent: Int): String =
      a.prettyPrint(indent + 1) +
        (newLine(indent) + Console.CYAN + Console.BOLD + name + Console.RESET) +
        (newLine(indent) + b.prettyPrint(indent + 1))
  }

  case class Matches(a: Expression[?], b: Expression[?])  extends PrintableBooleanExpression2("matches")
  case class Contains(a: Expression[?], b: Expression[?]) extends PrintableBooleanExpression2("contains")
  case class OneOf(a: Expression[?], b: Expression[?])    extends PrintableBooleanExpression2("oneOf")
  case class Overlaps(a: Expression[?], b: Expression[?]) extends PrintableBooleanExpression2("overlaps")
  case class NotNull(a: Expression[?]) extends PrintableBooleanExpression2("!=") { override val b: Value[Json] = Value.empty[Json] }
  case class And(a: BooleanExpression, b: BooleanExpression) extends PrintableBooleanExpression2("and")
  case class Or(a: BooleanExpression, b: BooleanExpression)  extends PrintableBooleanExpression2("or")

  case class Not(src: BooleanExpression) extends BooleanExpression {
    override def prettyPrint(indent: Int): String =
      (Console.CYAN + Console.BOLD + "!" + Console.RESET) + src.prettyPrint(indent + 1)
  }

}

package j2j

sealed trait BooleanExpression {
  def and(that: BooleanExpression): BooleanExpression = BooleanExpression.And(this, that)
  def or(that: BooleanExpression): BooleanExpression  = BooleanExpression.Or(this, that)
  def unary_not: BooleanExpression                    = BooleanExpression.Not(this)
}

object BooleanExpression {

  case class Equals(src: Expression, other: Expression)      extends BooleanExpression
  case class Includes(src: Expression, other: Expression)    extends BooleanExpression
  case class OneOf(src: Expression, other: Expression)       extends BooleanExpression
  case class Overlaps(src: Expression, other: Expression)    extends BooleanExpression
  case class NotNull(expression: Expression)                 extends BooleanExpression
  case class And(a: BooleanExpression, b: BooleanExpression) extends BooleanExpression
  case class Or(a: BooleanExpression, b: BooleanExpression)  extends BooleanExpression
  case class Not(src: BooleanExpression)                     extends BooleanExpression

}

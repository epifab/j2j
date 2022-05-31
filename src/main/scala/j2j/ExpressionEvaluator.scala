package j2j

import io.circe.{ACursor, Json}
import j2j.Expression.{Conditional, Const, Expressions, JsonPath}

class ExpressionEvaluator(cursor: ACursor) {

  def mapCursor(f: ACursor => ACursor): ExpressionEvaluator = new ExpressionEvaluator(f(cursor))

  def optional: Option[ExpressionEvaluator] = cursor.focus.map(j => new ExpressionEvaluator(j.hcursor))

  def evaluateJson(mapping: Expression): Json =
    mapping match {
      case Conditional(value, when, defaultTo) =>
        if (evaluateWhen(when)) evaluateJson(value)
        else defaultTo.map(evaluateJson).getOrElse(Json.Null)

      case jsonPath: JsonPath => JsonPathEvaluator.evaluate(cursor, jsonPath).asJson

      case const @ Const(_) => const.value

      case Expressions(expressions) => Json.arr(expressions.map(evaluateJson): _*)
    }

  def evaluateJsonArr(mapping: Expression): Vector[Json] =
    mapping match {
      case Conditional(value, when, defaultTo) =>
        if (evaluateWhen(when)) evaluateJsonArr(value)
        else defaultTo.map(evaluateJsonArr).getOrElse(Vector.empty)

      case jsonPath: JsonPath => JsonPathEvaluator.evaluate(cursor, jsonPath).asJsonArr

      case const @ Const(_) => Vector(const.value)

      case Expressions(expressions) => expressions.map(evaluateJson)
    }

  private def evaluateWhen(when: Option[BooleanExpression]): Boolean = when.forall(evaluateBoolean)

  private def evaluateBoolean(condition: BooleanExpression): Boolean = condition match {
    case BooleanExpression.And(expressions)     => expressions.map(evaluateBoolean).forall(identity)
    case BooleanExpression.Or(expressions)      => expressions.map(evaluateBoolean).exists(identity)
    case BooleanExpression.Equals(src, other)   => evaluateJsonArr(src).toSet == evaluateJsonArr(other).toSet
    case BooleanExpression.Includes(src, other) => evaluateJsonArr(src).forall(evaluateJsonArr(other).contains)
    case BooleanExpression.Defined(path)        => !evaluateJson(path).isNull
  }

}

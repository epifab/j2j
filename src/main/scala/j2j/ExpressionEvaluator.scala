package j2j

import cats.syntax.traverse.toTraverseOps
import io.circe.{ACursor, Decoder, DecodingFailure, Json}

class ExpressionEvaluator(cursor: ACursor) {

  def mapCursor(f: ACursor => ACursor): ExpressionEvaluator = new ExpressionEvaluator(f(cursor))

  def optional: Option[ExpressionEvaluator] = cursor.focus.map(j => new ExpressionEvaluator(j.hcursor))

  def evaluateJson(mapping: Expression): Json =
    mapping match {
      case ConditionalExpression(value, when, defaultTo) =>
        Option
          .when(evaluateBoolean(when))(evaluateJson(value))
          .getOrElse(evaluateJson(defaultTo))

      case jsonPath: JsonPath => JsonPathEvaluator.evaluate(cursor, jsonPath).asJson

      case Value(value) => value

      case Variable(f) => f()

      case ExpressionList(expressions) => Json.arr(expressions.toVector.flatMap(evaluateJsonArr)*)
    }

  def evaluateJsonArr(mapping: Expression): Vector[Json] =
    mapping match {
      case ConditionalExpression(value, when, defaultTo) =>
        if (evaluateBoolean(when)) evaluateJsonArr(value)
        else evaluateJsonArr(defaultTo)

      case jsonPath: JsonPath => JsonPathEvaluator.evaluate(cursor, jsonPath).asJsonArr

      case Value(value) => Vector(value)

      case Variable(f) => Vector(f())

      case ExpressionList(expressions) => expressions.toVector.map(evaluateJson)
    }

  def evaluate[A: Decoder](mapping: Expression): Either[DecodingFailure, A] =
    evaluateJson(mapping).as[A]

  def evaluateVector[A: Decoder](mapping: Expression): Either[DecodingFailure, Vector[A]] = {
    val json = evaluateJson(mapping)
    json.asArray
      .map(_.traverse(_.as[A]))
      .getOrElse(json.as[Option[A]].map(_.toVector))
  }

  private def evaluateBoolean(condition: BooleanExpression): Boolean = condition match {
    case BooleanExpression.And(a, b)            => evaluateBoolean(a) && evaluateBoolean(b)
    case BooleanExpression.Or(a, b)             => evaluateBoolean(a) || evaluateBoolean(b)
    case BooleanExpression.Not(expression)      => !evaluateBoolean(expression)
    case BooleanExpression.Equals(src, other)   => evaluateJsonArr(src).toSet == evaluateJsonArr(other).toSet
    case BooleanExpression.Includes(src, other) => evaluateJsonArr(src).contains(evaluateJson(other))
    case BooleanExpression.OneOf(src, other)    => evaluateJsonArr(other).contains(evaluateJson(src))
    case BooleanExpression.Overlaps(src, other) => evaluateJsonArr(src).toSet.intersect(evaluateJsonArr(other).toSet).nonEmpty
    case BooleanExpression.NotNull(path)        => evaluateJsonArr(path).nonEmpty
  }

}

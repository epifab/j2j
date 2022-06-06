package j2j

import cats.syntax.traverse.toTraverseOps
import j2j.Expression.{Conditional, Const, Expressions, JsonPath, Placeholder}
import io.circe.{ACursor, Decoder, DecodingFailure, Json}

class ExpressionEvaluator(cursor: ACursor, context: PartialFunction[String, Json]) {

  def mapCursor(f: ACursor => ACursor): ExpressionEvaluator = new ExpressionEvaluator(f(cursor), context)

  def optional: Option[ExpressionEvaluator] = cursor.focus.map(j => new ExpressionEvaluator(j.hcursor, context))

  def evaluateJson(mapping: Expression): Json =
    mapping match {
      case Conditional(value, when, defaultTo) =>
        Option
          .when(evaluateWhen(when))(evaluateJson(value))
          .filterNot(_.isNull)
          .orElse(defaultTo.map(evaluateJson))
          .getOrElse(Json.Null)

      case jsonPath: JsonPath => JsonPathEvaluator.evaluate(cursor, jsonPath).asJson

      case const @ Const(_) => const.value

      case Placeholder(key, path) if context.isDefinedAt(key) =>
        new ExpressionEvaluator(context(key).hcursor, PartialFunction.empty).evaluateJson(path)

      case Placeholder(_, _) => Json.Null

      case Expressions(expressions) => Json.arr(expressions.flatMap(evaluateJsonArr)*)
    }

  def evaluateJsonArr(mapping: Expression): Vector[Json] =
    mapping match {
      case Conditional(value, when, defaultTo) =>
        if (evaluateWhen(when)) evaluateJsonArr(value)
        else defaultTo.map(evaluateJsonArr).getOrElse(Vector.empty)

      case jsonPath: JsonPath => JsonPathEvaluator.evaluate(cursor, jsonPath).asJsonArr

      case const: Const => Vector(const.value)

      case placeholder: Placeholder => JsonPathEvaluator.evaluate(evaluateJson(placeholder).hcursor, JsonPath.Empty).asJsonArr

      case Expressions(expressions) => expressions.map(evaluateJson)
    }

  def evaluate[A: Decoder](mapping: Expression): Either[DecodingFailure, A] =
    evaluateJson(mapping).as[A]

  def evaluateVector[A: Decoder](mapping: Expression): Either[DecodingFailure, Vector[A]] = {
    val json = evaluateJson(mapping)
    json.asArray
      .map(_.traverse(_.as[A]))
      .getOrElse(json.as[Option[A]].map(_.toVector))
  }

  private def evaluateWhen(when: Option[BooleanExpression]): Boolean = when.forall(evaluateBoolean)

  private def evaluateBoolean(condition: BooleanExpression): Boolean = condition match {
    case BooleanExpression.And(expressions)     => expressions.map(evaluateBoolean).forall(identity)
    case BooleanExpression.Or(expressions)      => expressions.map(evaluateBoolean).exists(identity)
    case BooleanExpression.Equals(src, other)   => evaluateJsonArr(src).toSet == evaluateJsonArr(other).toSet
    case BooleanExpression.Includes(src, other) => evaluateJsonArr(src).contains(evaluateJson(other))
    case BooleanExpression.OneOf(src, other)    => evaluateJsonArr(other).contains(evaluateJson(src))
    case BooleanExpression.Overlaps(src, other) => evaluateJsonArr(src).toSet.intersect(evaluateJsonArr(other).toSet).nonEmpty
    case BooleanExpression.Defined(path)        => evaluateJsonArr(path).nonEmpty
  }

}

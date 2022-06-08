package j2j

import cats.syntax.traverse.toTraverseOps
import io.circe.{ACursor, Decoder, Json}

import scala.reflect.runtime.universe.TypeTag

class ExpressionEvaluator(cursor: ACursor) {

  def mapCursor(f: ACursor => ACursor): ExpressionEvaluator = new ExpressionEvaluator(f(cursor))

  def optional: Option[ExpressionEvaluator] = cursor.focus.map(j => new ExpressionEvaluator(j.hcursor))

  def evaluateJson(mapping: Expression): Either[EvaluationError, Json] =
    mapping match {
      case me @ MappedExpression(e, _) => evaluateJson(e).flatMap(me.apply)

      case fe @ FlatMappedExpression(e, _) => evaluateJson(e).flatMap(fe.apply).flatMap(evaluateJson)

      case TypedExpression(expression) => evaluateJson(expression)

      case jsonPath: JsonPath => Right(JsonPathEvaluator.evaluate(cursor, jsonPath).asJson)

      case Value(value) => Right(value)

      case Variable(f) => evaluateJson(Value(f()))

      case ConditionalExpression(value, when, defaultTo) =>
        for {
          cond   <- evaluateBoolean(when)
          result <- if (cond) evaluateJson(value) else evaluateJson(defaultTo)
        } yield result

      case ExpressionList(expressions) => expressions.toVector.flatTraverse(evaluateJsonArr).map(Json.arr(_*))
    }

  def evaluateJsonArr(mapping: Expression): Either[EvaluationError, Vector[Json]] =
    mapping match {
      case me @ MappedExpression(e, _) => evaluateJsonArr(e).flatMap(_.traverse(me.apply))

      case fe @ FlatMappedExpression(e, _) => evaluateJsonArr(e).flatMap(_.traverse(fe.apply)).flatMap(_.flatTraverse(evaluateJsonArr))

      case TypedExpression(expression) => evaluateJsonArr(expression)

      case jsonPath: JsonPath => Right(JsonPathEvaluator.evaluate(cursor, jsonPath).asJsonArr)

      case Value(value) => Right(value.asArray.getOrElse(Vector(value)).filterNot(_.isNull))

      case Variable(f) => evaluateJsonArr(Value(f()))

      case ConditionalExpression(value, when, defaultTo) =>
        for {
          cond   <- evaluateBoolean(when)
          result <- if (cond) evaluateJsonArr(value) else evaluateJsonArr(defaultTo)
        } yield result

      case ExpressionList(expressions) => expressions.toVector.flatTraverse(evaluateJsonArr)
    }

  def evaluate[A: Decoder: TypeTag](mapping: Expression): Either[EvaluationError, A] =
    evaluateJson(mapping)
      .flatMap(json => json.as[A].left.map(_ => ExtractionError[A](json)))

  def evaluateVector[A: Decoder: TypeTag](mapping: Expression): Either[EvaluationError, Vector[A]] = {
    evaluateJson(mapping).flatMap(json =>
      json.asArray
        .map(_.traverse(jsonItem => jsonItem.as[A].left.map(_ => ExtractionError[A](jsonItem))))
        .getOrElse(json.as[Option[A]].left.map(_ => ExtractionError[A](json)).map(_.toVector)),
    )
  }

  private def evaluateBoolean(condition: BooleanExpression): Either[EvaluationError, Boolean] = condition match {
    case BooleanExpression.And(a, b) =>
      for {
        aa <- evaluateBoolean(a)
        bb <- evaluateBoolean(b)
      } yield aa && bb

    case BooleanExpression.Or(a, b) =>
      for {
        aa <- evaluateBoolean(a)
        bb <- evaluateBoolean(b)
      } yield aa || bb

    case BooleanExpression.Not(expression) =>
      evaluateBoolean(expression).map(!_)

    case BooleanExpression.Matches(src, other) =>
      for {
        a <- evaluateJsonArr(src)
        b <- evaluateJsonArr(other)
      } yield a.toSet == b.toSet

    case BooleanExpression.Contains(src, other) =>
      for {
        a <- evaluateJsonArr(src)
        b <- evaluateJson(other)
      } yield a.contains(b)

    case BooleanExpression.OneOf(src, other) =>
      evaluateBoolean(BooleanExpression.Contains(other, src))

    case BooleanExpression.Overlaps(src, other) =>
      for {
        a <- evaluateJsonArr(src)
        b <- evaluateJsonArr(other)
      } yield a.exists(b.contains)

    case BooleanExpression.NotNull(path) =>
      evaluateJsonArr(path).map(_.nonEmpty)
  }

}

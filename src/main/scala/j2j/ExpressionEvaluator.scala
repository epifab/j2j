package j2j

import cats.syntax.traverse.toTraverseOps
import io.circe.{ACursor, Decoder, Json}

import scala.reflect.runtime.universe.TypeTag

trait ExpressionEvaluator {

  def evaluateAsJson(expression: Expression[?]): Either[EvaluationError, Json]

  def evaluate[A: Decoder: TypeTag](expression: Expression[A]): Either[EvaluationError, A]

  def evaluateVector[A: Decoder: TypeTag](expression: Expression[A]): Either[EvaluationError, Vector[A]]

  def evaluateOption[A: Decoder: TypeTag](expression: Expression[A]): Either[EvaluationError, Option[A]] =
    evaluate(expression.as[Option[A]])

}

object ExpressionEvaluator {

  def apply(json: Json): ExpressionEvaluator = new ExpressionEvaluatorImpl(json.hcursor)

  private class ExpressionEvaluatorImpl(root: ACursor) extends ExpressionEvaluator {

    def evaluateAsJson(expression: Expression[?]): Either[EvaluationError, Json] =
      expression match {
        case me @ MappedExpression(e, _) => evaluateAsJson(e).flatMap(me(root))

        case fe @ FlatMappedExpression(e, _) => evaluateAsJson(e).flatMap(fe(root)).flatMap(evaluateAsJson)

        case CastedExpression(expression) => evaluateAsJson(expression)

        case jsonPath: JsonPath => Right(JsonPathEvaluator.evaluate(root, jsonPath).asJson)

        case Value(value) => Right(value)

        case Variable(f) => evaluateAsJson(Value(f()))

        case ConditionalExpression(value, when, defaultTo) =>
          for {
            cond   <- evaluateBoolean(when)
            result <- if (cond) evaluateAsJson(value) else evaluateAsJson(defaultTo)
          } yield result

        case ExpressionList(expressions) => expressions.toVector.flatTraverse(evaluateAsJsonArr).map(Json.arr(_*))
      }

    def evaluateAsJsonArr(expression: Expression[?]): Either[EvaluationError, Vector[Json]] =
      expression match {
        case me @ MappedExpression(e, _) => evaluateAsJsonArr(e).flatMap(_.traverse(me(root)))

        case fe @ FlatMappedExpression(e, _) =>
          evaluateAsJsonArr(e).flatMap(_.traverse(fe(root))).flatMap(_.flatTraverse(evaluateAsJsonArr))

        case CastedExpression(expression) => evaluateAsJsonArr(expression)

        case jsonPath: JsonPath => Right(JsonPathEvaluator.evaluate(root, jsonPath).asJsonArr)

        case Value(value) => Right(value.asArray.getOrElse(Vector(value)).filterNot(_.isNull))

        case Variable(f) => evaluateAsJsonArr(Value(f()))

        case ConditionalExpression(value, when, defaultTo) =>
          for {
            cond   <- evaluateBoolean(when)
            result <- if (cond) evaluateAsJsonArr(value) else evaluateAsJsonArr(defaultTo)
          } yield result

        case ExpressionList(expressions) => expressions.toVector.flatTraverse(evaluateAsJsonArr)
      }

    def evaluate[A: Decoder: TypeTag](expression: Expression[A]): Either[EvaluationError, A] =
      evaluateAsJson(expression)
        .flatMap(json => json.as[A].left.map(_ => ExtractionError[A](json, expression, root)))

    def evaluateVector[A: Decoder: TypeTag](expression: Expression[A]): Either[EvaluationError, Vector[A]] = {
      evaluateAsJson(expression).flatMap(json =>
        json.asArray
          .map(_.traverse(jsonItem => jsonItem.as[A].left.map(_ => ExtractionError[A](jsonItem, expression, root))))
          .getOrElse(json.as[Option[A]].left.map(_ => ExtractionError[A](json, expression, root)).map(_.toVector)),
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
          a <- evaluateAsJsonArr(src)
          b <- evaluateAsJsonArr(other)
        } yield a.toSet == b.toSet

      case BooleanExpression.Contains(src, other) =>
        for {
          a <- evaluateAsJsonArr(src)
          b <- evaluateAsJson(other)
        } yield a.contains(b)

      case BooleanExpression.OneOf(src, other) =>
        evaluateBoolean(BooleanExpression.Contains(other, src))

      case BooleanExpression.Overlaps(src, other) =>
        for {
          a <- evaluateAsJsonArr(src)
          b <- evaluateAsJsonArr(other)
        } yield a.exists(b.contains)

      case BooleanExpression.NotNull(path) =>
        evaluateAsJsonArr(path).map(_.nonEmpty)
    }

  }

}

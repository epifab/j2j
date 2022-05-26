package j2j

import cats.syntax.traverse._
import j2j.JValue.JsonPath
import io.circe.{ACursor, Decoder, DecodingFailure, Json}
import j2j.Expression.Literal
import j2j.SeqSyntax.SeqExt

trait JsonSyntax {

  type OptionDecoder[T] = Decoder[Option[T]]

  implicit class CursorExt(cursor: ACursor) {
    def downPath(path: JsonPath): Vector[ACursor] = {
      path match {
        case JsonPath.NonEmpty(JsonPath.Property(key), tail) => cursor.downField(key).downPath(tail)

        case JsonPath.NonEmpty(JsonPath.ArrayElement(index), tail) => cursor.downN(index).downPath(tail)

        case JsonPath.NonEmpty(JsonPath.AllArrayElements, tail) =>
          cursor.focus
            .flatMap(_.asArray)
            .toVector
            .flatMap(_.flatMap(_.hcursor.downPath(tail)))

        case JsonPath.NonEmpty(JsonPath.ArrayRange(from, to), tail) =>
          cursor.focus
            .flatMap(_.asArray)
            .toVector
            .flatMap(_.slice(from, to).flatMap(_.hcursor.downPath(tail)))

        case JsonPath.Empty => Vector(cursor)
      }
    }

    private def evaluateAll[T: Decoder](mapping: JValue[T]): Either[DecodingFailure, Vector[T]] = mapping match {
      case jsonPath: JsonPath  => downPath(jsonPath).traverse(_.as[T])
      case JValue.Const(value) => Right(Vector(value))
    }

    private def evaluateFirst[T: OptionDecoder](mapping: JValue[T]): Either[DecodingFailure, Option[T]] = mapping match {
      case jsonPath: JsonPath  => downPath(jsonPath).traverseSome(_.as[Option[T]])
      case JValue.Const(value) => Right(Some(value))
    }

    def evaluateAll[T: Decoder: OptionDecoder](mapping: Expression[T]): Either[DecodingFailure, Vector[T]] =
      mapping match {
        case literal: Literal[T] =>
          for {
            unfiltered   <- evaluateWhen(literal.when)
            values       <- if (unfiltered) evaluateAll(literal.value) else Right(Vector.empty)
            withFallback <- if (values.isEmpty) literal.defaultTo.toVector.flatTraverse(evaluateAll[T]) else Right(values)
          } yield withFallback

        case Expression.Bucket(expressions) =>
          expressions.toVector.flatTraverse(evaluateAll[T])
      }

    def evaluateFirst[T: OptionDecoder](mapping: Expression[T]): Either[DecodingFailure, Option[T]] =
      mapping match {
        case literal: Literal[T] =>
          for {
            unfiltered   <- evaluateWhen(literal.when)
            values       <- if (unfiltered) evaluateFirst(literal.value) else Right(None)
            withFallback <- if (values.isEmpty) literal.defaultTo.flatTraverse(evaluateFirst[T]) else Right(values)
          } yield withFallback

        case Expression.Bucket(expressions) =>
          expressions.traverseSome(evaluateFirst[T])
      }

    def evaluateHead[T: OptionDecoder](mapping: Literal[T]): Either[DecodingFailure, T] =
      evaluateFirst(mapping).flatMap(_.toRight(DecodingFailure(s"Evaluation of $mapping yield no results", cursor.history)))

    private def evaluateWhen(condition: Option[BooleanExpr]): Either[DecodingFailure, Boolean] =
      condition.fold[Either[DecodingFailure, Boolean]](Right(true))(evaluateBoolean)

    private def evaluateBoolean(condition: BooleanExpr): Either[DecodingFailure, Boolean] = condition match {
      case BooleanExpr.All(expressions) =>
        expressions.traverse(evaluateBoolean).map(_.forall(identity))
      case BooleanExpr.Any(expressions) =>
        expressions.traverse(evaluateBoolean).map(_.exists(identity))
      case BooleanExpr.Equals(values) =>
        values.traverse(evaluateAll[Json]).map(_.flatten.distinct.size == 1)
      case BooleanExpr.Defined(path) =>
        println(cursor.downPath(path))
        Right(cursor.downPath(path).forall(_.focus.isDefined))
    }

  }

}

object JsonSyntax extends JsonSyntax

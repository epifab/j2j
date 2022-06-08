package j2j

import io.circe.parser.parse as parseJson
import io.circe.{Decoder, Encoder, Json as CirceJson}

import scala.reflect.runtime.universe.TypeTag

trait EvaluationSyntax {

  type Json = CirceJson

  implicit class ExpressionConversion[T](t: T) {
    def toExpression(implicit e: Encoder[T]): Expression = Value(t)
  }

  implicit class ExpressionListConversion(t: Seq[Expression]) {
    def toExpressionList: ExpressionList = ExpressionList(t*)
  }

  implicit class JsonEvaluationExtension(json: Json) extends ExpressionEvaluator {
    val evaluator: ExpressionEvaluator = ExpressionEvaluator(json)

    def evaluateAsJson(expression: Expression): Either[EvaluationError, Json] =
      evaluator.evaluateAsJson(expression)

    def evaluateAs[A: Decoder: TypeTag](expression: Expression): Either[EvaluationError, A] =
      evaluator.evaluateAs(expression)

    def evaluateAsVector[A: Decoder: TypeTag](expression: Expression): Either[EvaluationError, Vector[A]] =
      evaluator.evaluateAsVector(expression)

    def evaluateAsOption[A: Decoder: TypeTag](expression: Expression): Either[EvaluationError, Option[A]] =
      evaluator.evaluateAsOption(expression)
  }

  implicit class JInterpolation(sc: StringContext)(implicit context: Json) {
    def json(subs: Expression*): Either[EvaluationError, Json] = {

      val pit: Iterator[Either[EvaluationError, String]] = sc.parts.iterator.map(Right(_))
      val sit: Iterator[Either[EvaluationError, String]] = subs.iterator.map(context.evaluateAsJson).map(_.map(_.noSpaces))

      val reduceFunc: (Either[EvaluationError, String], Either[EvaluationError, String]) => Either[EvaluationError, String] = {
        case (a, b) =>
          for {
            s1 <- a
            s2 <- b
          } yield s1 + s2
      }

      val first = pit.next()

      (Iterator(first) ++ sit.zip(pit).map(reduceFunc.tupled))
        .reduce(reduceFunc)
        .map(_.stripMargin)
        .flatMap(s => parseJson(s).left.map(e => TemplateError(s, e.message)))
    }
  }

}

object EvaluationSyntax extends EvaluationSyntax

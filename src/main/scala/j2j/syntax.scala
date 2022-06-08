package j2j

import io.circe.Json
import io.circe.parser.parse as parseJson

object syntax {

  implicit class JInterpolation(sc: StringContext)(implicit context: Json) {
    def json(subs: Expression*): Either[EvaluationError, Json] = {
      val evaluator = new ExpressionEvaluator(context.hcursor)

      val pit: Iterator[Either[EvaluationError, String]] = sc.parts.iterator.map(Right(_))
      val sit: Iterator[Either[EvaluationError, String]] = subs.iterator.map(evaluator.evaluateJson).map(_.map(_.noSpaces))

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

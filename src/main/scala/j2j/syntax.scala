package j2j

import io.circe.{ACursor, Json}
import io.circe.parser.parse as parseJson

object syntax {

  implicit class JInterpolation(sc: StringContext)(implicit context: Json) {
    def json(subs: Expression*): Either[String, Json] = {
      val pit = sc.parts.iterator
      val sit = subs.iterator
      // Note parts.length == subs.length + 1
      val sb = new java.lang.StringBuilder(pit.next())
      while (sit.hasNext) {
        sb.append(new ExpressionEvaluator(context.hcursor).evaluateJson(sit.next()).noSpaces)
        sb.append(pit.next())
      }
      parseJson(sb.toString.stripMargin).left.map(_.message)
    }
  }

  implicit class CursorExt(json: ACursor) {
    def evaluateJson(expression: Expression): Json =
      new ExpressionEvaluator(json).evaluateJson(expression)

    def evaluateJsonArr(expression: Expression): Vector[Json] =
      new ExpressionEvaluator(json).evaluateJsonArr(expression)
  }

  implicit class JsonExt(json: Json) {
    def evaluateJson(expression: Expression): Json =
      json.hcursor.evaluateJson(expression)

    def evaluateJsonArr(expression: Expression): Vector[Json] =
      json.hcursor.evaluateJsonArr(expression)
  }

}

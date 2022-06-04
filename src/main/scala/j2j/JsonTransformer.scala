package j2j

import io.circe.parser.parse as parseJson
import io.circe.syntax.EncoderOps
import io.circe.{Json, ParsingFailure}
import j2j.Expression.Placeholder

sealed trait JsonTransformerError

case class JsonParseError(error: io.circe.Error) extends JsonTransformerError

case class ConfigParseError(error: pureconfig.error.ConfigReaderFailures) extends JsonTransformerError

class JsonTransformer(mappings: Map[String, Expression], template: Json, superContext: PartialFunction[String, Json]) {

  def apply(inputJsonStr: String): Either[ParsingFailure, Json] =
    parseJson(inputJsonStr).map(lookup).map(resolved => processTemplate(template)(resolved.orElse(superContext)))

  def lookup(json: Json): PartialFunction[String, Json] = {
    val evaluator = new ExpressionEvaluator(json.hcursor, superContext)
    mappings.toVector.map { case (key, expression) => key -> evaluator.evaluateJson(expression) }.toMap
  }

  def resolve(key: String, context: PartialFunction[String, Json]): Json =
    Placeholder.parse(key).fold(key.asJson)(new ExpressionEvaluator(Json.Null.hcursor, context).evaluateJson)

  def processTemplate(template: Json)(context: PartialFunction[String, Json]): Json =
    template.asString
      .map(resolve(_, context))
      .orElse(template.asArray.map(arr => arr.map(processTemplate(_)(context)).asJson))
      .orElse(
        template.asObject.map(_.toMap.flatMap { case (k, v) =>
          resolve(k, context).asString
            .map(_ -> processTemplate(v)(context))
        }.asJson),
      )
      .getOrElse(template)

}

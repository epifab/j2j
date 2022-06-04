package j2j

import io.circe.Json
import io.circe.parser.parse as parseJson
import io.circe.syntax.EncoderOps
import j2j.Expression.Placeholder
import pureconfig.ConfigSource

sealed trait JsonTransformerError

case class JsonParseError(error: io.circe.Error)                          extends JsonTransformerError
case class ConfigParseError(error: pureconfig.error.ConfigReaderFailures) extends JsonTransformerError

object JsonTransformer {

  def apply(
      inputJsonStr: String,
      templateJsonStr: String,
      expressionsStr: String,
      superContext: Map[String, Json] = Map.empty,
  ): Either[JsonTransformerError, Json] =
    for {
      inputJson    <- parseJson(inputJsonStr).left.map(JsonParseError)
      templateJson <- parseJson(templateJsonStr).left.map(JsonParseError)
      expressions  <- ConfigSource.string(expressionsStr).load[Map[String, Expression]].left.map(ConfigParseError)

      context = lookup(expressions, inputJson, superContext)
      done    = processTemplate(templateJson, superContext ++ context)
    } yield done

  def lookup(expressions: Map[String, Expression], json: Json, context: Map[String, Json]): Map[String, Json] = {
    val evaluator = new ExpressionEvaluator(json.hcursor, context)
    expressions.toVector.map { case (key, expression) =>
      key -> evaluator.evaluateJson(expression)
    }.toMap
  }

  def resolve(key: String, context: Map[String, Json]): Json =
    Placeholder.parse(key).fold(key.asJson)(new ExpressionEvaluator(Json.Null.hcursor, context).evaluateJson)

  def processTemplate(template: Json, context: Map[String, Json]): Json =
    template.asString
      .map(resolve(_, context))
      .orElse(template.asArray.map(arr => arr.map(a => processTemplate(a, context)).asJson))
      .orElse(
        template.asObject.map(_.toMap.flatMap { case (k, v) =>
          resolve(k, context).asString
            .map(_ -> processTemplate(v, context))
        }.asJson),
      )
      .getOrElse(template)

}

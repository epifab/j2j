package j2j

import io.circe.parser.parse as parseJson
import io.circe.syntax.EncoderOps
import io.circe.{Json, ParsingFailure}
import j2j.Expression.Placeholder

import scala.annotation.tailrec

sealed trait JsonTransformerError

case class JsonParseError(error: io.circe.Error) extends JsonTransformerError

case class ConfigParseError(error: pureconfig.error.ConfigReaderFailures) extends JsonTransformerError

class JsonTransformer(mappings: List[NamedExpression], template: Json, superContext: PartialFunction[String, Json]) {

  def apply(inputJsonStr: String): Either[ParsingFailure, Json] =
    parseJson(inputJsonStr)
      .map(buildContext)
      .map(processTemplate(template))

  def buildContext(json: Json): PartialFunction[String, Json] =
    buildContextR(superContext, mappings, json)

  @tailrec
  private def buildContextR(
      context: PartialFunction[String, Json],
      mappings: List[NamedExpression],
      json: Json,
  ): PartialFunction[String, Json] = {
    mappings match {
      case Nil => context
      case NamedExpression(key, expression) :: tail =>
        val evaluated = new ExpressionEvaluator(json.hcursor, context).evaluateJson(expression)
        buildContextR(context.orElse { case `key` => evaluated }, tail, json)
    }
  }

  def resolve(context: PartialFunction[String, Json])(s: String): Json =
    Placeholder.parse(s).fold(_ => s.asJson, new ExpressionEvaluator(Json.Null.hcursor, context).evaluateJson)

  def processTemplate(template: Json)(context: PartialFunction[String, Json]): Json =
    template.asString
      .map(resolve(context))
      .orElse(template.asArray.map(arr => arr.map(processTemplate(_)(context)).asJson))
      .orElse(
        template.asObject.map(_.toMap.flatMap { case (k, v) =>
          resolve(context)(k).asString
            .map(_ -> processTemplate(v)(context))
        }.asJson),
      )
      .getOrElse(template)

}

package j2j.facet

import j2j.*
import j2j.facet.model.{Data, NewValues, OutputAction, OutputBody, OutputEvent, PermissionDenial}

trait SimpleFacetTransformer extends EvaluationSyntax {

  protected val $body: JsonPath   = $ / "body"
  protected val $header: JsonPath = $ / "header"

  protected def $eventType: ConditionalExpression[String]
  protected def $universe: Expression[String]
  protected def $playerId: Expression[String]
  protected def $actions: List[Expression[OutputAction]]
  protected def $denials: Expression[Map[String, Vector[PermissionDenial]]]

  def active(json: Json): Either[EvaluationError, Boolean] =
    json.evaluateOption[String]($eventType).map(_.nonEmpty)

  private val $outputEvent = for {
    header    <- $header
    eventType <- $eventType
    universe  <- $universe
    playerId  <- $playerId
    denials   <- $denials
    actions   <- ExpressionList($actions*)
    _ = println(eventType)
    _ = println(universe)
    _ = println(playerId)
    _ = println(denials)
    _ = println(actions)
    _ = println("*" * 100)
  } yield OutputEvent(
    header,
    OutputBody(
      eventType,
      NewValues(
        playerId,
        universe,
        Data(
          permissionDenials = denials,
          actions = actions,
          context = Map.empty,
        ),
      ),
    ),
  )

  def extract(json: Json): Either[EvaluationError, OutputEvent] = json.evaluate($outputEvent)

}

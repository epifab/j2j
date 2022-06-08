package j2j.facet

import j2j.*

trait SimpleFacetTransformer extends EvaluationSyntax {

  protected val $body: JsonPath   = $ / "body"
  protected val $header: JsonPath = $ / "header"

  protected def $eventType: ConditionalExpression
  protected def $universe: Expression
  protected def $statuses: Expression
  protected def $playerId: Expression
  protected def $actions: List[Expression]

  def active(json: Json): Either[EvaluationError, Boolean] =
    json.evaluateAsOption[String]($eventType).map(_.nonEmpty)

  def extract(json: Json): Either[EvaluationError, Facet] = {
    val expr = for {
      eventType <- $eventType.as[String]
      universe  <- $universe.as[String]
      playerId  <- $playerId.as[String]
      statuses  <- $statuses.as[Vector[String]]
      actions   <- ExpressionList($actions*).as[Vector[Action]]
    } yield Facet(eventType, universe, playerId, statuses, actions)

    json.evaluateAs[Facet](expr)
  }

}

package j2j.facet

import io.circe.Json
import j2j.*
import j2j.facet.model.*

trait SimpleFacetTransformer {

  protected val $body: JsonPath   = $ / "body"
  protected val $header: JsonPath = $ / "header"

  protected def $eventType: Expression[Option[String]]
  protected def $universe: Expression[String]
  protected def $playerId: Expression[String]
  protected def $denials: Expression[Map[String, Vector[PermissionDenial]]]
  protected def $actions: Expression[Vector[OutputAction]]

  private def $outputEvent: Expression[Option[OutputEvent]] =
    $eventType.flatMap(_.fold(Value[Option[OutputEvent]](None)) { eventType =>
      for {
        header   <- $header
        playerId <- $playerId
        universe <- $universe
        denials  <- $denials
        actions  <- $actions
      } yield Some(
        OutputEvent(
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
        ),
      )
    })

  def extract(json: Json): Either[EvaluationError, Option[OutputEvent]] = $outputEvent.evalT(json.hcursor)

}

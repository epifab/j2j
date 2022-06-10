package j2j.facet

import j2j.*
import j2j.facet.model.{OutputAction, PermissionDenial}

import java.time.Instant

object SelfExclusion extends SimpleFacetTransformer {

  override protected val $eventType: ConditionalExpression[String] =
    Value("exclusion").when(($body / "type") matches Value("excluded"))

  override protected val $universe: Expression[String] = ($header / "universe").as[String]
  override protected val $playerId: Expression[String] = ($body / "newValues" / "playerId").as[String]

  private val $endTime: Expression[Option[Instant]] =
    ($body / "newValues" / "exclusion" / "end")
      .defaultTo($body / "newValues" / "exclusion" / "realizedEnd")
      .when(($body / "newValues" / "exclusion" / "type") overlaps Value(Vector("temporary", "timeout")))
      .as[Option[Instant]]

  private val $blockAccount: Expression[OutputAction] = {
    $endTime.map(endTime =>
      OutputAction(
        "blockAccount",
        List("canLogin", "canDeposit", "canBet"),
        "notification",
        endTime,
      ),
    )
  }.when($universe oneOf Value(List("wh-mga", "wh-eu-de", "wh-eu-dk")))

  println($blockAccount)

  override protected val $actions: List[Expression[OutputAction]] =
    List($blockAccount)

  override protected val $denials: Expression[Map[String, Vector[PermissionDenial]]] =
    $actions.toExpressionList
      .map(
        _.flatMap(action =>
          action.relatesToPermissions
            .map(permission =>
              permission -> Vector(
                PermissionDenial(
                  "self-excluded",
                  "Permission not granted because of self-exclusion",
                ),
              ),
            ),
        ).toMap,
      )

}

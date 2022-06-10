package j2j.facet

import j2j.*
import j2j.facet.model.{OutputAction, PermissionDenial}

import java.time.Instant

object SelfExclusion extends SimpleFacetTransformer {

  override protected val $eventType: Expression[Option[String]] = Value("excluded").when(($body / "type") equalsTo Value("excluded"))
  override protected val $universe: Expression[String]          = ($header / "universe").as[String]
  override protected val $playerId: Expression[String]          = ($body / "newValues" / "playerId").as[String]

  override protected val $actions: Expression[Vector[OutputAction]] =
    ExpressionVector($blockAccount).map(_.flatten)

  override protected val $denials: Expression[Map[String, Vector[PermissionDenial]]] =
    $actions.map(
      _.flatMap(_.relatesToPermissions)
        .map(permissionName =>
          permissionName -> Vector(
            PermissionDenial(
              "self-excluded",
              "Permission not granted because of self-exclusion",
            ),
          ),
        )
        .toMap,
    )

  private def $endTime: Expression[Option[Instant]] =
    ($body / "newValues" / "exclusion" / "end")
      .defaultTo($body / "newValues" / "exclusion" / "realizedEnd")
      .when(($body / "newValues" / "exclusion" / "type") oneOf Value(Vector("temporary", "timeout")))
      .as[Option[Instant]]

  private def $blockAccount: Expression[Option[OutputAction]] =
    $endTime.map(endTime =>
      OutputAction(
        "block-account",
        List("canLoginWithPassword", "canWithdraw", "canDeposit", "canBet", "canGame"),
        "notification",
        endTime,
      ),
    ).when($universe oneOf Value(Vector("wh-mga", "wh-eu-de", "wh-eu-dk")))

}

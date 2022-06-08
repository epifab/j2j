package j2j.facet

import j2j.*

object SelfExclusion extends SimpleFacetTransformer {

  override protected val $eventType: ConditionalExpression =
    Value("exclusion").when(($body / "type") matches Value("exclusion"))

  override protected val $universe: JsonPath = $header / "universe"
  override protected val $statuses: JsonPath = $body / "newValues" / "status"
  override protected val $playerId: JsonPath = $body / "newValues" / "id"

  private val exclusionReason =
    ($body / "newValues" / "blockReason").defaultTo(Value("self-excluded")).as[String]

  private val blockAccount =
    exclusionReason
      .map(reason =>
        Action(
          "blockAccount",
          reason = reason,
          revoked = Vector("canLogin", "canDeposit", "canBet"),
        ),
      )

  private val torturePerson =
    Action(
      "torturePerson",
      reason = "cruelty",
      granted = Vector("canDeposit"),
      revoked = Vector("canBet"),
    ).toExpression

  override protected val $actions: List[Expression] = {
    List(
      blockAccount when (($statuses contains Value("excluded")) and ($universe matches Value("wh-mga"))),
      torturePerson when (($statuses contains Value("excluded")) and ($universe matches Value("wh-eu-de"))),
    )
  }

}

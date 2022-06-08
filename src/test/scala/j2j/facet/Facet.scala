package j2j.facet

import io.circe.Codec
import io.circe.generic.semiauto.deriveCodec

case class Action(name: String, reason: String, revoked: Vector[String] = Vector.empty, granted: Vector[String] = Vector.empty)

object Action {
  implicit val codec: Codec[Action] = deriveCodec
}

case class Facet(eventType: String, universe: String, playerId: String, statuses: Vector[String], actions: Vector[Action])

object Facet {
  implicit val codec: Codec[Facet] = deriveCodec
}

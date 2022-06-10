package j2j.facet.model

import io.circe.generic.semiauto.deriveCodec
import io.circe.{Codec, Json}

import java.time.Instant

case class OutputEvent(header: Json, body: OutputBody)

object OutputEvent {
  implicit val codec: Codec[OutputEvent] = deriveCodec
}

case class OutputBody(
    `type`: String,
    newValues: NewValues,
)

object OutputBody {
  implicit val codec: Codec[OutputBody] = deriveCodec
}

case class NewValues(
    id: String,
    universe: String,
    data: Data,
)

object NewValues {
  implicit val codec: Codec[NewValues] = deriveCodec
}

case class Data(
    permissionDenials: Map[String, Vector[PermissionDenial]],
    actions: Vector[OutputAction],
    context: Map[String, String],
)

object Data {
  implicit val codec: Codec[Data] = deriveCodec
}

final case class PermissionDenial(
    reasonCode: String,
    description: String,
)

object PermissionDenial {
  implicit val codec: Codec[PermissionDenial] = deriveCodec
}

final case class OutputAction(name: String, relatesToPermissions: List[String], `type`: String, deadline: Option[Instant])

object OutputAction {
  implicit val codec: Codec[OutputAction] = deriveCodec
}

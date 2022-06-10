package j2j.props

import io.circe.{Encoder, Json}

import scala.annotation.implicitNotFound

@implicitNotFound("An expression of type ${A} cannot contain an expression of type ${B}")
trait CanContain[A, B] extends Matcher[A, B] {
  override def apply(as: A, b: B): Boolean
  override val name: String = "contains"
}

trait CanContainJson {
  implicit def jsonLeft[B: Encoder]: CanContain[Json, B] =
    (as: Json, b: B) => as.asArray.exists(_.contains(Encoder[B].apply(b)))
}

object CanContain extends CanContainJson {
  implicit def canonical[A, B](implicit cmp: AreComparable[A, B]): CanContain[Vector[A], B] =
    (as: Vector[A], b: B) => as.exists(a => cmp(a, b))
}

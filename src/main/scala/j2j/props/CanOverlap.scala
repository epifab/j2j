package j2j.props

import io.circe.{Encoder, Json}

import scala.annotation.implicitNotFound

@implicitNotFound("Expressions of type ${A} cannot overlaps with expression of type ${B}")
trait CanOverlap[A, B] extends Matcher[A, B] {
  override def apply(a: A, b: B): Boolean
  override val name: String = "overlaps"
}

trait LowPriorityOverlap {

  implicit def vectors[A, B](implicit cmp: AreComparable[A, B]): CanOverlap[Vector[A], Vector[B]] =
    (xs: Vector[A], ys: Vector[B]) => xs.exists(a => ys.exists(b => cmp(a, b)))

}

object CanOverlap extends LowPriorityOverlap {
  implicit def jsonLeft[B: Encoder]: CanOverlap[Json, Vector[B]] =
    (as: Json, bs: Vector[B]) => as.asArray.fold(false)(_.exists(a => bs.exists(b => a == Encoder[B].apply(b))))

  implicit def jsonRight[A: Encoder]: CanOverlap[Vector[A], Json] =
    (as: Vector[A], bs: Json) => bs.asArray.fold(false)(_.exists(b => as.exists(a => b == Encoder[A].apply(a))))

  implicit val json: CanOverlap[Json, Json] =
    (as: Json, bs: Json) =>
      (for {
        a <- as.asArray
        b <- bs.asArray
      } yield a == b).getOrElse(false)
}

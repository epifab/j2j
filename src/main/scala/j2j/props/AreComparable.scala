package j2j.props

import io.circe.{Encoder, Json}

import scala.annotation.implicitNotFound

@implicitNotFound("Expressions of type ${A} and ${B} are not comparable")
trait AreComparable[A, B] extends Matcher[A, B] {
  override def apply(a: A, b: B): Boolean
  override val name: String = "equals"
}

trait LowerPriorityComparisons {
  implicit def sameType[T]: AreComparable[T, T] =
    (a: T, b: T) => a == b
}

trait Any2JsonComparable extends LowerPriorityComparisons {
  implicit def jsonToAny[B: Encoder]: AreComparable[Json, B] =
    (a: Json, b: B) => AreComparable.jsonToJson(a, Encoder[B].apply(b))

  implicit def anyToJson[A: Encoder]: AreComparable[A, Json] =
    (a: A, b: Json) => AreComparable.jsonToJson(Encoder[A].apply(a), b)
}

object AreComparable extends Any2JsonComparable {

  implicit val jsonToJson: AreComparable[Json, Json] =
    (a: Json, b: Json) =>
      a.asArray.getOrElse(Vector(a)).filterNot(_.isNull).toSet ==
        b.asArray.getOrElse(Vector(b)).filterNot(_.isNull).toSet

  implicit def optionLeft[T]: AreComparable[Option[T], T] =
    (a: Option[T], b: T) => a.contains(b)

  implicit def optionRight[T]: AreComparable[T, Option[T]] =
    (a: T, b: Option[T]) => b.contains(a)

  implicit def comparableArraysRightOpt[T]: AreComparable[Vector[T], Vector[Option[T]]] =
    (t1: Vector[T], t2: Vector[Option[T]]) => t1.toSet == t2.flatten.toSet

  implicit def comparableArraysLeftOpt[T]: AreComparable[Vector[Option[T]], Vector[T]] =
    (t1: Vector[Option[T]], t2: Vector[T]) => t1.flatten.toSet == t2.toSet

  implicit def comparableArrays[T]: AreComparable[Vector[T], Vector[T]] =
    (t1: Vector[T], t2: Vector[T]) => t1.toSet == t2.toSet

}

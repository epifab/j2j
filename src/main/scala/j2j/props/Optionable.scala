package j2j.props

import scala.annotation.implicitNotFound

/** Works out the optional type for T
  *
  * @tparam T Any type T
  * @tparam U T if T is Option[?], Option[T] otherwise
  */
@implicitNotFound("${U} does not represent an optional ${T}")
trait Optionable[T, U] {
  def empty: U
  def lift(t: T): U
  def flatten(ts: Vector[U]): Vector[T]
}

object Optionable {
  implicit def nonOptional[T]: Optionable[T, Option[T]] = new Optionable[T, Option[T]] {
    override def empty: Option[T]                          = None
    override def lift(t: T): Option[T]                     = Some(t)
    override def flatten(ts: Vector[Option[T]]): Vector[T] = ts.flatten
  }

  implicit def optional[A]: Optionable[Option[A], Option[A]] = new Optionable[Option[A], Option[A]] {
    override def empty: Option[A]                                  = None
    override def lift(t: Option[A]): Option[A]                     = t
    override def flatten(ts: Vector[Option[A]]): Vector[Option[A]] = ts
  }
}

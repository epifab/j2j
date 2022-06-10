package j2j.props

import io.circe.Json

import scala.annotation.implicitNotFound

@implicitNotFound("Cannot determine the lowest common type between ${T} and ${U}")
trait LowestCommonType[A, B, C]

trait LowestCommonTypeLowPriority {
  implicit def optionalLeft[T, U](implicit opt: Optionable[T, U]): LowestCommonType[T, U, U]  = new LowestCommonType[T, U, U] {}
  implicit def optionalRight[T, U](implicit opt: Optionable[U, T]): LowestCommonType[T, U, T] = new LowestCommonType[T, U, T] {}

  implicit def jsonLeft[T]: LowestCommonType[Json, T, T]  = new LowestCommonType[Json, T, T] {}
  implicit def jsonRight[T]: LowestCommonType[T, Json, T] = new LowestCommonType[T, Json, T] {}
}

object LowestCommonType extends LowestCommonTypeLowPriority {
  implicit def sameType[T]: LowestCommonType[T, T, T] = new LowestCommonType[T, T, T] {}
}

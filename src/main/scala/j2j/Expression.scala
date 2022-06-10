package j2j

import io.circe.{ACursor, Decoder, Encoder, Json}
import j2j.BooleanExpression.*
import j2j.props.{AreComparable, CanContain, CanOverlap, LowestCommonType, Optionable}

abstract class Expression[T: Decoder: TypeString] extends JsonExpressionEvaluator with Printable {

  def evalT(root: ACursor): Either[EvaluationError, T] =
    evalAsJson(root).flatMap(json => json.as[T].left.map(_ => ExtractionError(json, this, root)))

  def when[U](cond: Expression[Boolean])(implicit
      op: Optionable[T, U],
      decoder: Decoder[U],
      encoder: Encoder[U],
      ts: TypeString[U],
  ): Expression[U] =
    ConditionalExpression(optional, cond, Value(op.empty))

  def when[U, V](booleanExpression: Expression[Boolean], default: Expression[U])(implicit
      lct: LowestCommonType[T, U, V],
      decoder: Decoder[V],
      typeString: TypeString[V],
  ): Expression[V] =
    ConditionalExpression(this.as[V], booleanExpression, default.as[V])

  def defaultTo[U, V](default: Expression[U])(implicit
      lct: LowestCommonType[T, U, V],
      decoder: Decoder[V],
      typeString: TypeString[V],
  ): Expression[V] =
    ConditionalExpression(as[V], notEmpty, default.as[V])

  def as[U: Decoder: TypeString]: Expression[U] =
    CastedExpression(this)

  def asJson: CastedExpression[Json] =
    CastedExpression(this)

  def map[U: Encoder: Decoder: TypeString](f: T => U): Expression[U] =
    MappedExpression(this, f)

  def flatMap[U: Decoder: TypeString](f: T => Expression[U]): Expression[U] =
    FlatMappedExpression(this, f)

  def equalsTo[U](that: Expression[U])(implicit cmp: AreComparable[T, U]): BooleanExpression = Matches(this, that)
  def contains[U](that: Expression[U])(implicit cmp: CanContain[T, U]): BooleanExpression    = Matches(this, that)
  def oneOf[U](that: Expression[U])(implicit cmp: CanContain[U, T]): BooleanExpression       = Matches(that, this)
  def overlaps[U](that: Expression[U])(implicit cmp: CanOverlap[T, U]): BooleanExpression    = Matches(this, that)

  def notEmpty: BooleanExpression = !(asJson equalsTo Value.empty)

  def optional[U](implicit
      op: Optionable[T, U],
      ts: TypeString[U],
      ec: Encoder[U],
      dc: Decoder[U],
  ): Expression[U] = map[U](op.lift)

}

package j2j

import io.circe.{Decoder, Encoder, Json}
import j2j.SeqSyntax.SeqExt

import scala.reflect.ClassTag
import scala.reflect.runtime.universe.TypeTag
import scala.util.matching.Regex
import scala.util.matching.Regex.Match

sealed trait Expression {

  def when(booleanExpression: BooleanExpression, default: Expression = Value.Empty): ConditionalExpression =
    ConditionalExpression(this, booleanExpression, default)

  def defaultTo(default: Expression): Expression =
    ConditionalExpression(this, notNull, default)

  def as[T: Decoder: TypeTag]: TypedExpression[T] =
    TypedExpression(this)

  def matches(that: Expression): BooleanExpression  = BooleanExpression.Matches(this, that)
  def contains(that: Expression): BooleanExpression = BooleanExpression.Contains(this, that)
  def oneOf(that: Expression): BooleanExpression    = BooleanExpression.OneOf(this, that)
  def overlaps(that: Expression): BooleanExpression = BooleanExpression.Overlaps(this, that)
  def notNull: BooleanExpression                    = BooleanExpression.NotNull(this)

}

case class TypedExpression[T: Decoder: TypeTag](expression: Expression) extends Expression {
  def map[U: Encoder](f: T => U): Expression =
    MappedExpression(this, f)

  def flatMap(f: T => Expression): Expression =
    FlatMappedExpression(this, f)
}

case class MappedExpression[T: Decoder: TypeTag, U: Encoder](expression: Expression, f: T => U) extends Expression {
  def apply(json: Json): Either[EvaluationError, Json] =
    json
      .as[T]
      .map(f)
      .map(Encoder[U].apply)
      .left
      .map(_ => ExtractionError[T](json))
}

case class FlatMappedExpression[T: Decoder: TypeTag](expression: Expression, f: T => Expression) extends Expression {
  def apply(json: Json): Either[EvaluationError, Expression] =
    json
      .as[T]
      .map(f)
      .left
      .map(_ => ExtractionError[T](json))
}

case class Value private (value: Json) extends Expression

object Value {
  val Empty = new Value(Json.Null)

  def apply[T: Encoder](value: T): Value = Value(Encoder[T].apply(value))
}

case class Variable private (value: () => Json) extends Expression

object Variable {
  def apply[T: Encoder](value: () => T): Variable = Variable(() => Encoder[T].apply(value()))
}

sealed trait Star
object * extends Star

object $ extends JsonPath.Empty

sealed trait JsonPath extends Expression {
  def /:(s: JsonPath.Segment): JsonPath = JsonPath.NonEmpty(s, this)
  def toList: List[JsonPath.Segment]
  def ++(j: JsonPath): JsonPath

  override def toString: String = s"$$${toList.mkString}"

  def /(s: String): JsonPath
  def /(i: Int): JsonPath
  def /(star: Star): JsonPath
  def /(range: (Int, Int)): JsonPath
  def /[X: ClassTag](range: (Star, Int)): JsonPath
  def /[X: ClassTag, Y: ClassTag](range: (Int, Star)): JsonPath
}

object JsonPath {
  trait Empty extends JsonPath {
    override val toList: List[Segment]     = Nil
    override def ++(j: JsonPath): JsonPath = j

    override def /(s: String): JsonPath                                    = NonEmpty(Property(s), this)
    override def /(i: Int): JsonPath                                       = NonEmpty(ArrayElement(i), this)
    override def /(star: Star): JsonPath                                   = NonEmpty(Wildcard, this)
    override def /(range: (Int, Int)): JsonPath                            = NonEmpty(ArrayRange(Some(range._1), Some(range._2)), this)
    override def /[X: ClassTag](range: (Star, Int)): JsonPath              = NonEmpty(ArrayRange(None, Some(range._2)), this)
    override def /[X: ClassTag, Y: ClassTag](range: (Int, Star)): JsonPath = NonEmpty(ArrayRange(Some(range._1), None), this)
  }

  case class NonEmpty(head: Segment, tail: JsonPath) extends JsonPath {
    override val toList: List[Segment]     = head :: tail.toList
    override def ++(j: JsonPath): JsonPath = NonEmpty(head, tail ++ j)

    override def /(s: String): JsonPath                                    = NonEmpty(head, tail / s)
    override def /(i: Int): JsonPath                                       = NonEmpty(head, tail / i)
    override def /(star: Star): JsonPath                                   = NonEmpty(head, tail / star)
    override def /(range: (Int, Int)): JsonPath                            = NonEmpty(head, tail / range)
    override def /[X: ClassTag](range: (Star, Int)): JsonPath              = NonEmpty(head, tail / [X] range)
    override def /[X: ClassTag, Y: ClassTag](range: (Int, Star)): JsonPath = NonEmpty(head, tail / [X, Y] range)
  }

  def apply(segments: Seq[Segment]): JsonPath = segments.foldRight[JsonPath]($)(_ /: _)

  sealed trait Segment {
    def toPath: JsonPath = NonEmpty(this, $)
  }

  case class Property(key: String) extends Segment {
    override def toString: String = s".$key"
  }

  case class ArrayElement(index: Int) extends Segment {
    override def toString: String = s"[$index]"
  }

  case class ArrayRange(from: Option[Int], to: Option[Int]) extends Segment {
    override def toString: String = s"[${from.getOrElse("")}:${to.getOrElse("")}]"
  }

  case object Wildcard extends Segment {
    override def toString: String = ".*"
  }

  private val propertyRegexDot      = "\\.([a-zA-Z\\d_-]+)".r
  private val propertyRegexBrackets = "\\['([a-zA-Z\\d_-]+)']".r
  private val arrayElementRegex     = "\\[(-?\\d+)]".r
  private val arrayRangeRegex       = "\\[(-?\\d+):(-?\\d+)]".r
  private val arrayRangeFromRegex   = "\\[(-?\\d+):]".r
  private val arrayRangeToRegex     = "\\[:(-?\\d+)]".r
  private val wildcardRegex         = "\\.\\*|\\[\\*]".r

  private def propertyMatch(m: Match): Property         = Property(m.group(1))
  private def arrayElementMatch(m: Match): ArrayElement = ArrayElement(m.group(1).toInt)
  private def arrayRangeMatch(m: Match): ArrayRange     = ArrayRange(Some(m.group(1).toInt), Some(m.group(2).toInt))
  private def arrayRangeFromMatch(m: Match): ArrayRange = ArrayRange(Some(m.group(1).toInt), None)
  private def arrayRangeToMatch(m: Match): ArrayRange   = ArrayRange(None, Some(m.group(1).toInt))

  private val segments: List[(Regex, Match => Segment)] = List(
    propertyRegexDot      -> propertyMatch,
    propertyRegexBrackets -> propertyMatch,
    arrayElementRegex     -> arrayElementMatch,
    arrayRangeRegex       -> arrayRangeMatch,
    arrayRangeFromRegex   -> arrayRangeFromMatch,
    arrayRangeToRegex     -> arrayRangeToMatch,
    wildcardRegex         -> (_ => Wildcard),
  )

  private def parseR(s: String): Either[String, List[Segment]] = {
    if (s.isEmpty) Right(Nil)
    else {
      segments
        .collectSome { case (regex, parser) => regex.findPrefixMatchOf(s).map(m => parser(m) -> m.source.toString.drop(m.end)) }
        .toRight(s"Invalid JSON path: $s")
        .flatMap { case (head, unmatched) => parseR(unmatched).map(head :: _) }
    }
  }

  def parse(s: String): Either[String, JsonPath] =
    if (s.startsWith("$")) parseR(s.drop(1)).map(JsonPath.apply)
    else Left(s"Invalid JSON path: $s")

}

case class ConditionalExpression(
    value: Expression,
    when: BooleanExpression,
    defaultTo: Expression,
) extends Expression

case class ExpressionList private (expressions: Vector[Expression]) extends Expression

object ExpressionList {
  def apply(expressions: Expression*): ExpressionList = new ExpressionList(expressions.toVector)
}

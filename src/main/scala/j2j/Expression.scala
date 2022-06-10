package j2j

import io.circe.{ACursor, Decoder, Encoder, Json, Printer}
import j2j.SeqSyntax.SeqExt

import scala.reflect.ClassTag
import scala.reflect.runtime.universe.{TypeTag, typeTag}
import scala.util.matching.Regex
import scala.util.matching.Regex.Match

sealed trait Expression[T] extends Printable {

  def when(booleanExpression: BooleanExpression, default: Expression[T] = Value.empty[T]): ConditionalExpression[T] =
    ConditionalExpression(this, booleanExpression, default)

  def defaultTo(default: Expression[T]): Expression[T] =
    ConditionalExpression(this, notNull, default)

  def as[U: Decoder: TypeTag]: CastedExpression[U] =
    CastedExpression(this)

  def asJson: CastedExpression[Json] =
    CastedExpression(this)

  def map[U: Encoder: TypeTag](f: T => U)(implicit d: Decoder[T], t: TypeTag[T]): Expression[U] =
    MappedExpression(this, f)

  def flatMap[U: TypeTag](f: T => Expression[U])(implicit tDec: Decoder[T], tTyp: TypeTag[T]): Expression[U] =
    FlatMappedExpression(this, f)

  def matches(that: Expression[?]): BooleanExpression  = BooleanExpression.Matches(this, that)
  def contains(that: Expression[?]): BooleanExpression = BooleanExpression.Contains(this, that)
  def oneOf(that: Expression[?]): BooleanExpression    = BooleanExpression.OneOf(this, that)
  def overlaps(that: Expression[?]): BooleanExpression = BooleanExpression.Overlaps(this, that)
  def notNull: BooleanExpression                       = BooleanExpression.NotNull(this)

}

case class CastedExpression[T: Decoder: TypeTag](expression: Expression[?]) extends Expression[T] {
  override def prettyPrint(indent: Int): String =
    (Console.BLUE + Console.BOLD + "cast" + Console.RESET) +
      (Console.CYAN + s"[${typeTag[T].tpe}]" + Console.RESET) +
      newLine(indent + 1) +
      expression.prettyPrint(indent + 1)
}

case class MappedExpression[T: Decoder: TypeTag, U: Encoder: TypeTag](expression: Expression[T], f: T => U) extends Expression[U] {
  override def prettyPrint(indent: Int): String =
    (Console.BLUE + Console.BOLD + "map" + Console.RESET) +
      (Console.CYAN + s"[${typeTag[U].tpe}]" + Console.RESET) +
      newLine(indent + 1) +
      expression.prettyPrint(indent + 1)

  def apply(root: ACursor)(json: Json): Either[EvaluationError, Json] =
    json
      .as[T]
      .map(f)
      .left
      .map(_ => ExtractionError[T](json, expression, root))
      .map(Encoder[U].apply)
}

case class FlatMappedExpression[T: Decoder: TypeTag, U: TypeTag](expression: Expression[T], f: T => Expression[U]) extends Expression[U] {
  override def prettyPrint(indent: Int): String =
    (Console.BLUE + Console.BOLD + "flatMap" + Console.RESET) +
      (Console.CYAN + s"[${typeTag[U].tpe}]" + Console.RESET) +
      newLine(indent + 1) +
      expression.prettyPrint(indent + 1)

  def apply(root: ACursor)(json: Json): Either[EvaluationError, Expression[U]] =
    json
      .as[T]
      .left
      .map(_ => ExtractionError[T](json, expression, root))
      .map(f)
}

case class Value[T] private (value: Json) extends Expression[T] {
  override def prettyPrint(indent: Int): String =
    value.printWith(Printer.noSpaces)
}

object Value {
  def empty[T]: Value[T] = new Value(Json.Null)

  def apply[T: Encoder](value: T): Value[T]   = Value(Encoder[T].apply(value))
  def json[T: Encoder](value: T): Value[Json] = Value(Encoder[T].apply(value))

}

case class Variable[T: TypeTag] private (value: () => Json) extends Expression[T] {
  override def prettyPrint(indent: Int): String =
    (Console.MAGENTA + "variable" + Console.RESET) +
      (Console.CYAN + s"[${typeTag[T].tpe}]" + Console.RESET)

}

object Variable {
  def apply[T: Encoder: TypeTag](value: () => T): Variable[T] = Variable(() => Encoder[T].apply(value()))
}

sealed trait Star
object * extends Star

object $ extends JsonPath.Empty

sealed trait JsonPath extends Expression[Json] {

  def /:(s: JsonPath.Segment): JsonPath = JsonPath.NonEmpty(s, this)
  def toList: List[JsonPath.Segment]
  def ++(j: JsonPath): JsonPath

  override def prettyPrint(indent: Int): String =
    Console.GREEN + Console.BOLD + s"$$${toList.mkString}" + Console.RESET

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

case class ConditionalExpression[T](
    value: Expression[T],
    when: BooleanExpression,
    defaultTo: Expression[T],
) extends Expression[T] {

  override def prettyPrint(indent: Int): String = {
    (Console.BLUE + Console.BOLD + "if" + Console.RESET) +
      newLine(indent + 1) +
      when.prettyPrint(indent + 1) +
      newLine(indent) +
      (Console.BLUE + Console.BOLD + "then" + Console.RESET) +
      newLine(indent + 1) +
      value.prettyPrint(indent + 1) +
      newLine(indent) +
      (Console.BLUE + Console.BOLD + "else" + Console.RESET) +
      newLine(indent + 1) +
      defaultTo.prettyPrint(indent + 1)
  }

}

case class ExpressionList[T] private (expressions: Vector[Expression[T]]) extends Expression[Vector[T]] {
  override def prettyPrint(indent: Int): String =
    expressions
      .map(_.prettyPrint(indent + 1))
      .map(p => newLine(indent + 1) + p)
      .mkString("[", ",", newLine(indent) + "]")
}

object ExpressionList {
  def apply[T](expressions: Expression[T]*): ExpressionList[T] = new ExpressionList(expressions.toVector)
}

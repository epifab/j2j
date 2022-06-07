package j2j

import cats.implicits.toTraverseOps
import io.circe.{Encoder, Json}
import j2j.Expression.JsonPath.Segment
import j2j.SeqSyntax.SeqExt
import pureconfig.ConfigReader
import pureconfig.error.{CannotConvert, ConfigReaderFailures, ConvertFailure, KeyNotFound}

import scala.reflect.ClassTag
import scala.util.matching.Regex
import scala.util.matching.Regex.Match

sealed trait Expression

object Expression {

  case class Const(value: Json) extends Expression

  object Const {
    def apply[T: Encoder](value: T): Const   = Const(Encoder[T].apply(value))
    implicit val reader: ConfigReader[Const] = JsonConfigReader.jsonReader.map(Const(_))
  }

  case class Placeholder(key: String, path: JsonPath) extends Expression

  object Placeholder {
    private val Regex = "%\\{([a-zA-Z\\d_-]+)}(.*)".r

    def parse(s: String): Either[String, Placeholder] = s match {
      case Regex(key, rest) => JsonPath.parse("$" + rest).map(path => Placeholder(key, path))
      case _                => Left("Not a placeholder")
    }

    implicit val reader: ConfigReader[Placeholder] =
      ConfigReader.stringConfigReader.emap(s => parse(s).left.map(CannotConvert(s, "Placeholder", _)))
  }

  sealed trait Star

  sealed trait JsonPath extends Expression {
    def /:(s: JsonPath.Segment): JsonPath = JsonPath.NonEmpty(s, this)
    def toList: List[Segment]
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

    case object * extends Star

    case object Empty extends JsonPath {
      override val toList: List[Segment]     = Nil
      override def ++(j: JsonPath): JsonPath = j

      override def /(s: String): JsonPath                                    = NonEmpty(Property(s), Empty)
      override def /(i: Int): JsonPath                                       = NonEmpty(ArrayElement(i), Empty)
      override def /(star: Star): JsonPath                                   = NonEmpty(Wildcard, Empty)
      override def /(range: (Int, Int)): JsonPath                            = NonEmpty(ArrayRange(Some(range._1), Some(range._2)), Empty)
      override def /[X: ClassTag](range: (Star, Int)): JsonPath              = NonEmpty(ArrayRange(None, Some(range._2)), Empty)
      override def /[X: ClassTag, Y: ClassTag](range: (Int, Star)): JsonPath = NonEmpty(ArrayRange(Some(range._1), None), Empty)
    }

    val $ : JsonPath = Empty

    case class NonEmpty(head: Segment, tail: JsonPath) extends JsonPath {
      override val toList: List[Segment]     = head :: tail.toList
      override def ++(j: JsonPath): JsonPath = NonEmpty(head, tail ++ j)

      override def /(s: String): JsonPath                                    = NonEmpty(head, tail / s)
      override def /(i: Int): JsonPath                                       = NonEmpty(head, tail / i)
      override def /(star: Star): JsonPath                                   = NonEmpty(head, tail / *)
      override def /(range: (Int, Int)): JsonPath                            = NonEmpty(head, tail / range)
      override def /[X: ClassTag](range: (Star, Int)): JsonPath              = NonEmpty(head, tail / range)
      override def /[X: ClassTag, Y: ClassTag](range: (Int, Star)): JsonPath = NonEmpty(head, tail / [X, Y] range)
    }

    def apply(segments: Seq[Segment]): JsonPath = segments.foldRight[JsonPath](Empty)(_ /: _)

    sealed trait Segment {
      def toPath: JsonPath = NonEmpty(this, Empty)
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

    implicit val reader: ConfigReader[JsonPath] =
      ConfigReader.stringConfigReader.emap(s => parse(s).left.map(reason => CannotConvert(s, "JsonPath", reason)))
  }

  case class Conditional(
      value: Expression,
      when: Option[BooleanExpression] = None,
      defaultTo: Option[Expression] = None,
  ) extends Expression

  object Conditional {
    implicit val reader: ConfigReader[Conditional] = {
      ConfigReader.fromCursor { cursor =>
        for {
          obj <- cursor.asMap
          value <- obj.get("value") match {
            case Some(valueCursor) => Expression.reader.from(valueCursor)
            case None              => Left(ConfigReaderFailures(ConvertFailure(KeyNotFound("value", obj.keys.toSet), cursor)))
          }
          when    <- obj.get("when").traverse(BooleanExpression.reader.from)
          default <- obj.get("default-to").traverse(Expression.reader.from)
        } yield Conditional(value, when, default)
      }
    }
  }

  case class Expressions(expressions: Vector[Expression]) extends Expression

  object Expressions {
    implicit val reader: ConfigReader[Expressions] =
      ConfigReader[Vector[Expression]].map(Expressions(_))
  }

  implicit val reader: ConfigReader[Expression] = {
    JsonPath.reader
      .orElse(Placeholder.reader)
      .orElse(Const.reader)
      .orElse(Conditional.reader)
      .orElse(Expressions.reader)
  }

}

package j2j

import cats.implicits.toTraverseOps
import io.circe.{Encoder, Json}
import j2j.Expression.JsonPath.Segment
import j2j.SeqSyntax.SeqExt
import pureconfig.ConfigReader
import pureconfig.error.{CannotConvert, ConfigReaderFailures, ConvertFailure, KeyNotFound}

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

  sealed trait JsonPath extends Expression {
    def /:(s: JsonPath.Segment): JsonPath = JsonPath.NonEmpty(s, this)
    def toList: List[Segment]

    override def toString: String = s"JsonPath($$${toList.mkString})"
  }

  object JsonPath {

    case object Empty extends JsonPath {
      override val toList: List[Segment] = Nil
    }

    case class NonEmpty(head: Segment, tail: JsonPath) extends JsonPath {
      override val toList: List[Segment] = head :: tail.toList
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

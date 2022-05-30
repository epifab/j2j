package j2j

import cats.implicits.toTraverseOps
import io.circe.{ACursor, DecodingFailure, Encoder, HCursor, Json}
import io.circe.syntax.EncoderOps
import j2j.SeqSyntax.SeqExt
import pureconfig.ConfigReader
import pureconfig.error.{CannotConvert, ConfigReaderFailures, ConvertFailure, KeyNotFound}

import scala.util.matching.Regex
import scala.util.matching.Regex.Match

sealed trait Expression[+T] {
  def asJson: Expression[Json]
}

object Expression {

  type Reader[T] = ConfigReader[Expression[T]]

  case class Const[+T: Encoder](value: T) extends Expression[T] {
    override def asJson: Const[Json] = Const(value.asJson)
  }

  object Const {
    implicit def reader[T: ConfigReader: Encoder]: ConfigReader[Const[T]] = ConfigReader[T].map(Const(_))
  }

  sealed trait JsonEvaluationResult {
    def toJson: Json
    def toJsonArr: Vector[Json]
  }

  object JsonEvaluationResult {
    case object Empty extends JsonEvaluationResult {
      override def toJson: Json            = Json.Null
      override def toJsonArr: Vector[Json] = Vector.empty
    }

    case class One(json: Json) extends JsonEvaluationResult {
      override def toJson: Json            = json
      override def toJsonArr: Vector[Json] = Vector(json)
    }

    case class Many(results: Vector[JsonEvaluationResult]) extends JsonEvaluationResult {
      override def toJson: Json            = Json.arr(results.flatMap(_.toJsonArr): _*)
      override def toJsonArr: Vector[Json] = results.flatMap(_.toJsonArr)
    }
  }

  sealed trait JsonPath extends Expression[Nothing] {
    def /:(s: JsonPath.Segment): JsonPath = JsonPath.NonEmpty(s, this)
    override def asJson: Expression[Json] = this
    def evaluate(cursor: ACursor): JsonEvaluationResult
  }

  object JsonPath {

    case object Empty extends JsonPath {
      override def evaluate(cursor: ACursor): JsonEvaluationResult =
        cursor.focus.fold[JsonEvaluationResult](JsonEvaluationResult.Empty)(JsonEvaluationResult.One)
    }

    case class NonEmpty(head: Segment, tail: JsonPath) extends JsonPath {
      private def indexFor[_](arr: Seq[_])(n: Int): Int = n match {
        case negative if negative < 0 => arr.length + negative
        case positive                 => positive
      }

      override def evaluate(cursor: ACursor): JsonEvaluationResult = head match {
        case Property(key) => tail.evaluate(cursor.downField(key))

        case Wildcard =>
          (for {
            c   <- cursor.focus
            arr <- c.asArray.orElse(c.asObject.map(_.values.toVector))
            results = JsonEvaluationResult.Many(arr.map(j => tail.evaluate(j.hcursor)))
          } yield results).getOrElse(JsonEvaluationResult.Empty)

        case ArrayElement(index) =>
          (for {
            c   <- cursor.focus
            arr <- c.asArray
            realIndex = indexFor(arr)(index)
            element <-
              try Some(arr.apply(realIndex))
              catch { case _: IndexOutOfBoundsException => None }
          } yield tail.evaluate(element.hcursor)).getOrElse(JsonEvaluationResult.Empty)

        case ArrayRange(from, to) =>
          (for {
            c   <- cursor.focus
            arr <- c.asArray

            actualFrom = from.map(indexFor(arr)).getOrElse(0)
            actualTo   = to.map(indexFor(arr)).getOrElse(arr.length)

            children =
              if (actualFrom > actualTo) arr.slice(actualTo, actualFrom + 1).reverse
              else arr.slice(actualFrom, actualTo + 1)

          } yield JsonEvaluationResult.Many(children.map(j => tail.evaluate(j.hcursor)))).getOrElse(JsonEvaluationResult.Empty)

      }
    }

    def apply(segments: Seq[Segment]): JsonPath = segments.foldRight[JsonPath](Empty)(_ /: _)

    sealed trait Segment {
      def toPath: JsonPath = NonEmpty(this, Empty)
    }

    case class Property(key: String)                          extends Segment
    case class ArrayElement(index: Int)                       extends Segment
    case class ArrayRange(from: Option[Int], to: Option[Int]) extends Segment
    case object Wildcard                                      extends Segment

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

  case class Conditional[+T](
      value: Expression[T],
      when: Option[BooleanExpression] = None,
      defaultTo: Option[Expression[T]] = None,
  ) extends Expression[T] {
    override def asJson: Expression[Json] = Conditional(value.asJson, when, defaultTo.map(_.asJson))
  }

  object Conditional {
    implicit def reader[T: ConfigReader: Encoder]: ConfigReader[Conditional[T]] = {
      ConfigReader.fromCursor { cursor =>
        for {
          obj <- cursor.asMap
          value <- obj.get("value") match {
            case Some(valueCursor) => Expression.reader[T].from(valueCursor)
            case None              => Left(ConfigReaderFailures(ConvertFailure(KeyNotFound("value", obj.keys.toSet), cursor)))
          }
          when    <- obj.get("when").traverse(BooleanExpression.reader.from)
          default <- obj.get("default-to").traverse(Expression.reader[T].from)
        } yield Conditional(value, when, default)
      }
    }
  }

  case class Expressions[+T](expressions: Vector[Expression[T]]) extends Expression[T] {
    override def asJson: Expression[Json] = Expressions(expressions.map(_.asJson))
  }

  object Expressions {
    implicit def reader[T: ConfigReader: Encoder]: ConfigReader[Expressions[T]] =
      ConfigReader[Vector[Expression[T]]].map(Expressions(_))
  }

  implicit def reader[T: ConfigReader: Encoder]: ConfigReader[Expression[T]] = {
    JsonPath.reader
      .orElse(Const.reader[T])
      .orElse(Conditional.reader[T])
      .orElse(Expressions.reader[T])
  }

}

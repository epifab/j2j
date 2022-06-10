package j2j

import io.circe.{ACursor, Json}
import j2j.ConsoleOps.*
import j2j.SeqSyntax.SeqExt

import scala.reflect.ClassTag
import scala.util.matching.Regex
import scala.util.matching.Regex.Match

sealed trait Star
object * extends Star

object $ extends JsonPath.Empty

sealed trait JsonPath extends Expression[Json] {

  def /:(s: JsonPath.Segment): JsonPath = JsonPath.NonEmpty(s, this)
  def segmentList: List[JsonPath.Segment]
  def ++(j: JsonPath): JsonPath

  def /(s: String): JsonPath
  def /(i: Int): JsonPath
  def /(star: Star): JsonPath
  def /(range: (Int, Int)): JsonPath
  def /[X: ClassTag](range: (Star, Int)): JsonPath
  def /[X: ClassTag, Y: ClassTag](range: (Int, Star)): JsonPath

  override def evalAsJson(root: ACursor): Either[EvaluationError, Json] =
    Right(JsonPathEvaluator.evaluate(root, this).asJson)

  override def print(implicit context: PrinterContext): String =
    green(bold(s"$$${segmentList.mkString}"))

}

object JsonPath {
  trait Empty extends JsonPath {
    override val segmentList: List[Segment] = Nil
    override def ++(j: JsonPath): JsonPath  = j

    override def /(s: String): JsonPath                                    = NonEmpty(Property(s), this)
    override def /(i: Int): JsonPath                                       = NonEmpty(ArrayElement(i), this)
    override def /(star: Star): JsonPath                                   = NonEmpty(Wildcard, this)
    override def /(range: (Int, Int)): JsonPath                            = NonEmpty(ArrayRange(Some(range._1), Some(range._2)), this)
    override def /[X: ClassTag](range: (Star, Int)): JsonPath              = NonEmpty(ArrayRange(None, Some(range._2)), this)
    override def /[X: ClassTag, Y: ClassTag](range: (Int, Star)): JsonPath = NonEmpty(ArrayRange(Some(range._1), None), this)
  }

  case class NonEmpty(head: Segment, tail: JsonPath) extends JsonPath {
    override val segmentList: List[Segment] = head :: tail.segmentList
    override def ++(j: JsonPath): JsonPath  = NonEmpty(head, tail ++ j)

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

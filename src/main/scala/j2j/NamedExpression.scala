package j2j

import cats.implicits.toTraverseOps
import pureconfig.ConfigSource

case class NamedExpression(key: String, value: Expression)

object NamedExpression {

  case class RawNamedExpression(key: String, expression: String)

  trait ParseStep {
    def next(n: RawNamedExpression): ParseStep
    def complete: Either[String, List[NamedExpression]]
  }

  case object InitialStep extends ParseStep {

    def next(n: RawNamedExpression): ParseStep = IncompleteParseStep(Nil, n)

    def complete: Either[String, List[NamedExpression]] = Right(Nil)

  }

  case class IncompleteParseStep(done: List[RawNamedExpression], current: RawNamedExpression) extends ParseStep {

    def next(n: RawNamedExpression): ParseStep =
      IncompleteParseStep(done :+ current, n)

    def add(s: String): IncompleteParseStep =
      IncompleteParseStep(done, current.copy(expression = current.expression + s))

    def complete: Either[String, List[NamedExpression]] = (done :+ current).traverse { case RawNamedExpression(key, value) =>
      ConfigSource
        .string(s"$key: $value")
        .load[Map[String, Expression]]
        .left
        .map(_.prettyPrint(2))
        .map(e => NamedExpression(key, e(key)))
    }
  }

  private val R = "([a-zA-Z_-]+)\\s*[=:]\\s*(.*)".r

  def loadList(lines: List[String]): Either[String, List[NamedExpression]] = {
    lines
      .filterNot(_.isEmpty)
      .filterNot(_.startsWith("#"))
      .foldLeft[Either[String, ParseStep]](Right(InitialStep)) { case (currentResult, s) =>
        currentResult.flatMap { (results: ParseStep) =>
          s match {
            case R(key, rest) => Right(results.next(RawNamedExpression(key, rest)))
            case rest =>
              results match {
                case InitialStep                     => Left(s"Unexpected: $s")
                case incomplete: IncompleteParseStep => Right(incomplete.add(rest))
              }
          }
        }
      }
      .flatMap(_.complete)
  }
}

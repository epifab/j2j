package j2j

import cats.syntax.traverse._
import pureconfig.ConfigReader
import pureconfig.error.{ConfigReaderFailures, ConvertFailure, KeyNotFound}

sealed trait Expression[+T]

object Expression {

  case class Literal[+T](
      value: JValue[T],
      when: Option[BooleanExpr] = None,
      defaultTo: Option[Literal[T]] = None,
  ) extends Expression[T]

  object Literal {
    implicit def reader[T](implicit mv: JValue.Reader[T]): ConfigReader[Literal[T]] = {
      mv.map(Literal(_))
        .orElse(ConfigReader.fromCursor { cursor =>
          for {
            obj <- cursor.asMap
            _if <- obj.get("if").traverse(BooleanExpr.reader.from)
            _then <- obj.get("then") match {
              case Some(valueCursor) => mv.from(valueCursor)
              case None              => Left(ConfigReaderFailures(ConvertFailure(KeyNotFound("value", obj.keys.toSet), cursor)))
            }
            _else <- obj.get("else").traverse(reader.from)
          } yield Literal(_then, _if, _else)
        })
    }
  }

  case class Bucket[+T](expressions: List[Expression[T]]) extends Expression[T]

  object Bucket {
    implicit def reader[T: ConfigReader]: ConfigReader[Bucket[T]] =
      ConfigReader[List[Literal[T]]].map(Bucket(_))
  }
  implicit def multipleReader[T: ConfigReader]: ConfigReader[Bucket[T]] =
    ConfigReader[List[Literal[T]]].map(Bucket(_))

  implicit def reader[T: ConfigReader]: ConfigReader[Expression[T]] =
    Bucket.reader[T].orElse(Literal.reader[T])

}

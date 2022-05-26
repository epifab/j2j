package j2j

import io.circe.Json
import pureconfig.ConfigReader
import pureconfig.error.{ConfigReaderFailures, ConvertFailure, KeyNotFound}

sealed trait BooleanExpr

object BooleanExpr {

  implicit val jsonReader: ConfigReader[Json] = ConfigReader.fromCursor { cursor =>
    cursor.asInt
      .map(Json.fromInt)
      .orElse(cursor.asLong.map(Json.fromLong))
      .orElse(cursor.asDouble.map(Json.fromDoubleOrString))
      .orElse(cursor.asBoolean.map(Json.fromBoolean))
      .orElse(cursor.asString.map(Json.fromString))
  }

  case class Equals(values: List[JValue[Json]])  extends BooleanExpr
  case class Defined(path: JValue.JsonPath)      extends BooleanExpr
  case class Any(expressions: List[BooleanExpr]) extends BooleanExpr
  case class All(expressions: List[BooleanExpr]) extends BooleanExpr

  private def mkReader(key: String, reader: ConfigReader[BooleanExpr]): ConfigReader[BooleanExpr] =
    ConfigReader.fromCursor(cursor =>
      for {
        obj        <- cursor.asMap
        dataCursor <- obj.get(key).toRight(ConfigReaderFailures(ConvertFailure(KeyNotFound(key, obj.keys.toSet), cursor)))
        expression <- reader.from(dataCursor)
      } yield expression,
    )

  val equalsReader: ConfigReader[BooleanExpr]  = mkReader("equals", ConfigReader[List[JValue[Json]]].map(Equals))
  val definedReader: ConfigReader[BooleanExpr] = mkReader("defined", JValue.JsonPath.reader.map(Defined))
  def andReader: ConfigReader[BooleanExpr]     = mkReader("all", ConfigReader[List[BooleanExpr]].map(All))
  def orReader: ConfigReader[BooleanExpr]      = mkReader("any", ConfigReader[List[BooleanExpr]].map(Any))

  implicit def reader: ConfigReader[BooleanExpr] =
    equalsReader.orElse(definedReader).orElse(andReader).orElse(orReader)
}

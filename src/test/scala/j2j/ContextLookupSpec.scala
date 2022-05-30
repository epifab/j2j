package j2j

import io.circe.Json
import pureconfig.ConfigSource

object ContextLookupSpec extends App {

  val config =
    """
      |header: "Context lookup works"
      |version: 0.1
      |test: "$.a[0]"
      |code: "$.a[1]"
      |letters: "$.a[2].*"
      |glossary: "$.a[3].*"
      |t: "$.a[3].t"
      |e: "$.a[3].e"
      |s: "$.a[3].s"
      |""".stripMargin

  val json =
    """
      |{
      |  "this": "is",
      |  "a": [
      |    "test",
      |    7357,
      |    ["t", "e", "s", "t"],
      |    {
      |      "t": 7,
      |      "e": 3,
      |      "s": 5
      |    }
      |  ]
      |}
      |""".stripMargin

  import JsonConfigReader.jsonReader

  println(new ContextLookup(ConfigSource.string(config).loadOrThrow[Map[String, Expression[Json]]]).lookup(json))

}

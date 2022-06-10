package j2j

import io.circe.Json
import j2j.EvaluationSyntax.JInterpolation

object MyTransformer {

  private val a        = $ / "a"
  private val code     = a / 1
  private val letters  = a / 2 / *
  private val glossary = a / 3
  private val t        = glossary / "t"
  private val e        = glossary / "e"
  private val s        = glossary / "s"
  private val assertions = ExpressionList(
    Value("e=3").when(e matches Value(3)),
    Value("e=4").when(e matches Value(4)),
  )

  val test: Expression[String] = for {
    tCode <- t.as[Int]
    eCode <- e.as[Int]
    sCode <- s.as[Int]
  } yield s"code-$tCode$eCode$sCode$tCode"

  private val versions = Iterator.from(0)
  private val version  = Variable(versions.next).as[Int].map(_ + 1)
  private val author   = Value("j2j")

  def apply(implicit json: Json): Either[EvaluationError, Json] = {

    json"""
          |{
          |  "v1": $version,
          |  "v2": $version,
          |  "processedBy": $author,
          |  $test: $code,
          |  "letters": $letters,
          |  "glossary": $glossary,
          |  "t": $t,
          |  "e": $e,
          |  "s": $s,
          |  "assertions": $assertions
          |}
          |"""
  }
}

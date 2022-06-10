package j2j

import io.circe.Json

object MyTransformer {

  private val $a        = $ / "a"
  private val $code     = $a / 1
  private val $letters  = $a / 2 / *
  private val $glossary = $a / 3
  private val $t        = $glossary / "t"
  private val $e        = $glossary / "e"
  private val $s        = $glossary / "s"
  private val $assertions = ExpressionVector(
    Value("e=3").when($e equalsTo Value(3)),
    Value("e=4").when($e equalsTo Value(4)),
  ).map(_.flatten)

  val $test: Expression[String] = for {
    tCode <- $t.as[Int]
    eCode <- $e.as[Int]
    sCode <- $s.as[Int]
  } yield s"code-$tCode$eCode$sCode$tCode"

  private val versions = Iterator.from(0)
  private val $version = Lambda(versions.next).as[Int].map(_ + 1)
  private val $author  = Value("j2j")

  val output: Expression[Json] = for {
    version1    <- $version.asJson
    version2    <- $version.asJson
    processedBy <- $author.asJson
    test        <- $test
    code        <- $code
    letters     <- $letters
    glossary    <- $glossary
    t           <- $t
    e           <- $e
    s           <- $s
    assertions  <- $assertions.asJson
  } yield Json.obj(
    "v1"          -> version1,
    "v2"          -> version2,
    "processedBy" -> processedBy,
    test          -> code,
    "letters"     -> letters,
    "glossary"    -> glossary,
    "t"           -> t,
    "e"           -> e,
    "s"           -> s,
    "assertions"  -> assertions,
  )

  def apply(implicit json: Json): Either[EvaluationError, Json] = output.evalT(json.hcursor)

}

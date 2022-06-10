package j2j.facet

import io.circe.Json
import j2j.*
import j2j.SeqSyntax.SeqExt
import j2j.facet.model.OutputEvent

class MultiFacetTransformer(facets: Seq[SimpleFacetTransformer]) {
  def apply(json: Json): Either[EvaluationError, Option[OutputEvent]] =
    facets.traverseSome(_.extract(json))

}

object MultiFacetTransformer {

  def prod = new MultiFacetTransformer(SelfExclusion :: Nil)

}

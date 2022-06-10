package j2j.facet

import j2j.*
import j2j.facet.model.OutputEvent

class MultiFacetTransformer(facets: Seq[SimpleFacetTransformer]) extends EvaluationSyntax {
  def apply(json: Json): Option[Either[EvaluationError, OutputEvent]] =
    facets
      .find(_.active(json) == Right(true))
      .map(_.extract(json))

}

object MultiFacetTransformer {

  def prod = new MultiFacetTransformer(
    SelfExclusion ::
      Nil,
  )

}

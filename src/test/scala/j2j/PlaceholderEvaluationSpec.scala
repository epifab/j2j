package j2j

class PlaceholderEvaluationSpec
    extends ExpressionEvaluationTester(
      Scenario(
        hint = "Placeholder can be found",
        json = "{}",
        context = Map("foo" -> "bar"),
        expr = """value = "%{foo}"""",
        expectedOutput = "bar",
      ),
      Scenario(
        hint = "Placeholder cannot be found",
        json = "{}",
        context = Map("baz" -> "bar"),
        expr = """value = "%{foo}"""",
        expectedOutput = None,
      ),
    )

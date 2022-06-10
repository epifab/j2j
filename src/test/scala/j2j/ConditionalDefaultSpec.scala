package j2j

class ConditionalDefaultSpec
    extends ExpressionEvaluationTester(
      Scenario(
        hint = "Default to constant for non-existing JSON property",
        json = """{}""",
        expr = ($ / "foo").defaultTo(Value.json("bar")),
        expectedOutput = "bar",
      ),
      Scenario(
        hint = "Default to property for non-existing JSON property",
        json = """{"qux": 123}""",
        expr = ($ / "foo").defaultTo($ / "qux"),
        expectedOutput = 123,
      ),
      Scenario(
        hint = "Default to non-existing JSON property",
        json = """{"qux": 123}""",
        expr = ($ / "foo").defaultTo($ / "bar"),
        expectedOutput = None,
      ),
      Scenario(
        hint = "Default when condition fails",
        json = """{}""",
        expr = Value(1).when(Value("a") matches Value("b"), Value(2)),
        expectedOutput = 2,
      ),
    )

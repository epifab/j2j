package j2j

class ConditionalContainsSpec
    extends ExpressionEvaluationTester(
      Scenario(
        hint = "Constant is included in JSON array",
        json = "[1, 2, 1936, 3]",
        expr = Value(true).when(($ / *) contains Value(1936)),
        expectedOutput = true,
      ),
      Scenario(
        hint = "Constant is NOT included in JSON array",
        json = "[1, 2, 3]",
        expr = Value(true).when(($ / *) contains Value(1936)),
        expectedOutput = None,
      ),
      Scenario(
        hint = "JSON number is included in constants",
        json = """{"foo": 1936}""",
        expr = Value(true).when(Value(Vector(1, 2, 1936, 3)) contains ($ / "foo")),
        expectedOutput = true,
      ),
      Scenario(
        hint = "JSON number is NOT included in constants",
        json = """{"foo": 1936}""",
        expr = Value(true).when(Value(Vector(1, 2, 3)) contains ($ / "foo")),
        expectedOutput = None,
      ),
      Scenario(
        hint = "[1, 2] is NOT included in [1, 2, 3]",
        json = "{}",
        expr = Value(true).when(Value(Vector(1, 2, 3)) contains Value(Vector(1, 2))),
        expectedOutput = None,
      ),
    )

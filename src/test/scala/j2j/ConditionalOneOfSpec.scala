package j2j

class ConditionalOneOfSpec
    extends ExpressionEvaluationTester(
      Scenario(
        hint = "Constant is one of JSON array",
        json = "[1, 2, 1936, 3]",
        expr = Value(true).when(Value(1936) oneOf ($ / *)),
        expectedOutput = true,
      ),
      Scenario(
        hint = "Constant is NOT one of JSON array",
        json = "[1, 2, 3]",
        expr = Value(true).when(Value(1936) oneOf ($ / *)),
        expectedOutput = None,
      ),
      Scenario(
        hint = "JSON number is one of constants",
        json = """{"foo": 1936}""",
        expr = Value(true).when(($ / "foo") oneOf Value(Vector(1, 2, 1936, 3))),
        expectedOutput = true,
      ),
      Scenario(
        hint = "JSON number is NOT one of constants",
        json = """{"foo": 1936}""",
        expr = Value(true).when(($ / "foo") oneOf Value(Vector(1, 2, 3))),
        expectedOutput = None,
      ),
      Scenario(
        hint = "[1, 2] is NOT one of [1, 2, 3]",
        json = "{}",
        expr = Value(true).when(Value(Vector(1, 2)) oneOf Value(Vector(1, 2, 1936, 3))),
        expectedOutput = None,
      ),
    )

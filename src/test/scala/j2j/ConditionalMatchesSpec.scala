package j2j

class ConditionalMatchesSpec
    extends ExpressionEvaluationTester(
      Scenario(
        hint = "JSON path matches expected value",
        json = """{"qux": 1936}""",
        expr = Value(true).when(($ / "qux") equalsTo Value(1936)),
        expectedOutput = true,
      ),
      Scenario(
        hint = "JSON path does not match expected value (same type)",
        json = """{"qux": 1936}""",
        expr = Value(true).when(($ / "qux") equalsTo Value(1937)),
        expectedOutput = None,
      ),
      Scenario(
        hint = "JSON path does not matches expected value (different types)",
        json = """{"qux": 1936}""",
        expr = Value(true).when(($ / "qux") equalsTo Value("wow")),
        expectedOutput = None,
      ),
      Scenario(
        hint = "JSON path matches expected value (implicit cast from String to Int)",
        json = """{"qux": 1936}""",
        expr = Value(true).when(($ / "qux") equalsTo Value(1936)),
        expectedOutput = true,
      ),
      Scenario(
        hint = "JSON path is missing",
        json = """{}""",
        expr = Value(true).when(($ / "qux") equalsTo Value(1936)),
        expectedOutput = None,
      ),
      Scenario(
        hint = "Matching all elements in array",
        json = """[{"qux": 1936}, {"qux": 1936}]""",
        expr = Value(true).when(($ / * / "qux") equalsTo Value(1936)),
        expectedOutput = true,
      ),
      Scenario(
        hint = "Matching all elements in array (part II)",
        json = """[{"qux": 1936}, {"qux": 1936}]""",
        expr = Value(true).when(($ / * / "qux") equalsTo Value(Vector(1936))),
        expectedOutput = true,
      ),
      Scenario(
        hint = "Matching all EXISTING elements in array",
        json = """[{"qux": 1936}, {"foo": "bar"}]""",
        expr = Value(true).when(($ / * / "qux") equalsTo Value(1936)),
        expectedOutput = true,
      ),
      Scenario(
        hint = "Matching only some elements in array",
        json = """[{"qux": 1936}, {"qux": 1937}]""",
        expr = Value(true).when(($ / * / "qux") equalsTo Value(1936)),
        expectedOutput = None,
      ),
      Scenario(
        hint = "Matching nothing (empty array)",
        json = """[{"foo": 1936}, {"foo": 1937}]""",
        expr = Value(true).when(($ / * / "qux") equalsTo Value(1936)),
        expectedOutput = None,
      ),
      Scenario(
        hint = "Comparing 2 identical arrays",
        json = """[{"qux": 1936}, {"qux": 1937}]""",
        expr = Value(true).when(($ / * / "qux") equalsTo Value(Vector(1936, 1937))),
        expectedOutput = true,
      ),
      Scenario(
        hint = "Comparing 2 arrays with the same elements in different order",
        json = """[{"qux": 1936}, {"qux": 1937}]""",
        expr = Value(true).when(($ / * / "qux") equalsTo Value(Vector(1936, 1937))),
        expectedOutput = true,
      ),
      Scenario(
        hint = "Comparing 2 sets with the same elements",
        json = """[{"qux": 1936}, {"qux": 1937}, {"qux": 1936}]""",
        expr = Value(true).when(($ / * / "qux") equalsTo Value(Vector(1936, 1937))),
        expectedOutput = true,
      ),
    )

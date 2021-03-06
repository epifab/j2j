package j2j

class ConditionalDefinedSpec
    extends ExpressionEvaluationTester(
      Scenario(
        hint = "Property is defined",
        json = """{"qux": 123}""",
        expr = Value(true).when(($ / "qux").notEmpty),
        expectedOutput = true,
      ),
      Scenario(
        hint = "Property not defined",
        json = """{}""",
        expr = Value(true).when(($ / "qux").notEmpty),
        expectedOutput = None,
      ),
      Scenario(
        hint = "Nested property is defined",
        json = """{"qux": {"baz": 123}}""",
        expr = Value(true).when(($ / "qux" / "baz").notEmpty),
        expectedOutput = true,
      ),
      Scenario(
        hint = "Nested property not defined",
        json = """{"foo": {}}""",
        expr = Value(true).when(($ / "foo" / "qux").notEmpty),
        expectedOutput = None,
      ),
      Scenario(
        hint = "Property as object is defined",
        json = """{"qux": {"baz": 123}}""",
        expr = Value(true).when(($ / "qux").notEmpty),
        expectedOutput = true,
      ),
      Scenario(
        hint = "Array element is defined",
        json = """[123]""",
        expr = Value(true).when(($ / 0).notEmpty),
        expectedOutput = true,
      ),
      Scenario(
        hint = "Array element is not defined",
        json = """[123]""",
        expr = Value(true).when(($ / 1).notEmpty),
        expectedOutput = None,
      ),
      Scenario(
        hint = "Property in array is defined for all elements",
        json = """[{"foo": 1}, {"foo": 2}]""",
        expr = Value(true).when(($ / * / "foo").notEmpty),
        expectedOutput = true,
      ),
      Scenario(
        hint = "Property in array is defined at least for some elements",
        json = """[{"foo": 1}, {"bar": 2}]""",
        expr = Value(true).when(($ / * / "foo").notEmpty),
        expectedOutput = true,
      ),
      Scenario(
        hint = "Property in array is undefined for all elements",
        json = """[{"foo": 1}, {"bar": 2}]""",
        expr = Value(true).when(($ / * / "baz").notEmpty),
        expectedOutput = None,
      ),
    )

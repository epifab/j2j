package j2j

class ConditionalDefinedSpec
    extends ExpressionEvaluationTester(
      Scenario(
        hint = "Property is defined",
        json = """{"qux": 123}""",
        expr = """{ value = true, when = { defined: "$.qux" } }""",
        expectedOutput = true,
      ),
      Scenario(
        hint = "Property not defined",
        json = """{}""",
        expr = """{ value = true, when = { defined: "$.qux" } }""",
        expectedOutput = None,
      ),
      Scenario(
        hint = "Nested property is defined",
        json = """{"qux": {"baz": 123}}""",
        expr = """{ value = true, when = { defined: "$.qux.baz" } }""",
        expectedOutput = true,
      ),
      Scenario(
        hint = "Nested property not defined",
        json = """{"foo": {}}""",
        expr = """{ value = true, when = { defined: "$.foo.qux" } }""",
        expectedOutput = None,
      ),
      Scenario(
        hint = "Property as object is defined",
        json = """{"qux": {"baz": 123}}""",
        expr = """{ value = true, when = { defined: "$.qux" } }""",
        expectedOutput = true,
      ),
      Scenario(
        hint = "Array element is defined",
        json = """[123]""",
        expr = """{ value = true, when = { defined: "$[0]" } }""",
        expectedOutput = true,
      ),
      Scenario(
        hint = "Array element is not defined",
        json = """[123]""",
        expr = """{ value = true, when = { defined: "$[1]" } }""",
        expectedOutput = None,
      ),
      Scenario(
        hint = "Property in array is defined for all elements",
        json = """[{"foo": 1}, {"foo": 2}]""",
        expr = """{ value = true, when = { defined: "$[*].foo" } }""",
        expectedOutput = true,
      ),
      Scenario(
        hint = "Property in array is defined at least for some elements",
        json = """[{"foo": 1}, {"bar": 2}]""",
        expr = """{ value = true, when = { defined: "$[*].foo" } }""",
        expectedOutput = true,
      ),
      Scenario(
        hint = "Property in array is undefined for all elements",
        json = """[{"foo": 1}, {"bar": 2}]""",
        expr = """{ value = true, when = { defined: "$[*].baz" } }""",
        expectedOutput = None,
      ),
    )

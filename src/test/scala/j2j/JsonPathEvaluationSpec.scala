package j2j

class JsonPathEvaluationSpec
    extends ExpressionEvaluationTester(
      Scenario(
        hint = "JSON path - property",
        json = """{"hello": "world"}""",
        expr = $ / "hello",
        expectedOutput = "world",
      ),
      Scenario(
        hint = "Missing property",
        json = """{}""",
        expr = $ / "hello",
        expectedOutput = None,
      ),
      Scenario(
        hint = "Querying all array elements (wildcard)",
        json = """[15, 16, 17]""",
        expr = $ / *,
        expectedOutput = Vector(15, 16, 17),
      ),
      Scenario(
        hint = "Querying specific array element (in range)",
        json = """[15, 16, 17]""",
        expr = $ / 1,
        expectedOutput = 16,
      ),
      Scenario(
        hint = "Querying specific array element (out of range)",
        json = """[15, 16, 17]""",
        expr = $ / 4,
        expectedOutput = None,
      ),
      Scenario(
        hint = "Querying specific array element (negative, in range)",
        json = """[15, 16, 17]""",
        expr = $ / -1,
        expectedOutput = 17,
      ),
      Scenario(
        hint = "Querying specific array element (negative, out of range)",
        json = """[15, 16, 17]""",
        expr = $ / -4,
        expectedOutput = None,
      ),
      Scenario(
        hint = "Querying array range",
        json = """[15, 16, 17, 18]""",
        expr = $ / (1, 2),
        expectedOutput = Vector(16, 17),
      ),
      Scenario(
        hint = "Querying array range (from: out of bound)",
        json = """[15, 16, 17, 18]""",
        expr = $ / (10, 12),
        expectedOutput = Vector.empty[Int],
      ),
      Scenario(
        hint = "Querying array range (to: out of bound)",
        json = """[15, 16, 17, 18]""",
        expr = $ / (1, 10),
        expectedOutput = Vector(16, 17, 18),
      ),
      Scenario(
        hint = "Querying array range (negative range)",
        json = """[15, 16, 17, 18]""",
        expr = $ / (-3, -1),
        expectedOutput = Vector(16, 17),
      ),
      Scenario(
        hint = "Querying array range (negative, out of bound)",
        json = """[15, 16, 17, 18]""",
        expr = $ / (-10, -5),
        expectedOutput = Vector.empty[Int],
      ),
      Scenario(
        hint = "Querying array range (negative, partially out of bound)",
        json = """[15, 16, 17, 18]""",
        expr = $ / (-10, -2),
        expectedOutput = Vector(15, 16),
      ),
      Scenario(
        hint = "Querying nested array range",
        json = """[[1, 2, 3], [4, 5, 6]]""",
        expr = $ / * / (1, *),
        expectedOutput = Vector(2, 3, 5, 6),
      ),
      Scenario(
        hint = "Querying deeply nested array range part I",
        json = """[[[1, 2], [3, 4]], [[5, 6], [], [7, 8, 9], [10]]]""",
        expr = $ / * / * / (1, *),
        expectedOutput = Vector(2, 4, 6, 8, 9),
      ),
      Scenario(
        hint = "Querying deeply nested array range part II",
        json = """[[[1, 2], [3, 4]], [[5, 6], [], [7, 8, 9], [10]]]""",
        expr = $ / * / (1, *),
        expectedOutput = Vector(3, 4, 7, 8, 9, 10),
      ),
      Scenario(
        hint = "Querying deeply nested array range part III",
        json = """["abc"]""",
        expr = $ / (*, 10),
        expectedOutput = Vector.empty[Int],
      ),
      Scenario(
        hint = "Wildcard selects all object properties values",
        json = """{"foo": [1, 2, 3], "bar": [4, 5, 6]}""",
        expr = $ / *,
        expectedOutput = List(1, 2, 3, 4, 5, 6),
      ),
      Scenario(
        hint = "Fetching and merging objects",
        json = """{"foo": {"bar": 1}, "baz": {"bar": 2, "qux": 3}}""",
        expr = $ / *,
        expectedOutput = List(Map("bar" -> 1), Map("bar" -> 2, "qux" -> 3)),
      ),
    )

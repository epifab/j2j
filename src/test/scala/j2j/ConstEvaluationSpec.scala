package j2j

class ConstEvaluationSpec
    extends ExpressionEvaluationTester(
      Scenario(
        hint = "Type: integer",
        json = "{}",
        expr = Value(123),
        expectedOutput = 123,
      ),
      Scenario(
        hint = "Type: long",
        json = "{}",
        expr = Value(123L),
        expectedOutput = 123L,
      ),
      Scenario(
        hint = "Type: double",
        json = "{}",
        expr = Value(1.23),
        expectedOutput = 1.23,
      ),
      Scenario(
        hint = "Type: string",
        json = "{}",
        expr = Value("hello"),
        expectedOutput = "hello",
      ),
      Scenario(
        hint = "Type: list of constants",
        json = "{}",
        expr = Value(Vector(1, 2, 3)),
        expectedOutput = Vector(1, 2, 3),
      ),
    )

package j2j

class ConditionalOrSpec
    extends ExpressionEvaluationTester(
      Scenario(
        hint = "Two expressions returning true",
        json = """{"foo": "bar"}""",
        expr = Value(true).when(($ / "foo").notEmpty || ($ / "foo").equalsTo(Value("bar"))),
        expectedOutput = true,
      ),
      Scenario(
        hint = "Two expressions returning true and false",
        json = """{"foo": "bar"}""",
        expr = Value(true).when(($ / "foo").notEmpty || ($ / "xxx").notEmpty),
        expectedOutput = true,
      ),
      Scenario(
        hint = "Two expressions returning false",
        json = """{"foo": "bar"}""",
        expr = Value(true).when(($ / "yyy").notEmpty || ($ / "xxx").notEmpty),
        expectedOutput = None,
      ),
    )

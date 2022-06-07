package j2j

class ConditionalOrSpec
    extends ExpressionEvaluationTester(
      Scenario(
        hint = "Two expressions returning true",
        json = """{"foo": "bar"}""",
        expr = Value(true).when(($ / "foo").notNull and ($ / "foo").matches(Value("bar"))),
        expectedOutput = true,
      ),
      Scenario(
        hint = "Two expressions returning true and false",
        json = """{"foo": "bar"}""",
        expr = Value(true).when(($ / "foo").notNull and ($ / "xxx").notNull),
        expectedOutput = true,
      ),
      Scenario(
        hint = "Two expressions returning false",
        json = """{"foo": "bar"}""",
        expr = Value(true).when(($ / "yyy").notNull and ($ / "xxx").notNull),
        expectedOutput = None,
      ),
    )

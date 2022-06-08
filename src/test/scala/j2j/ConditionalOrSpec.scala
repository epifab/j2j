package j2j

class ConditionalOrSpec
    extends ExpressionEvaluationTester(
      Scenario(
        hint = "Two expressions returning true",
        json = """{"foo": "bar"}""",
        expr = Value(true).when(($ / "foo").notNull or ($ / "foo").matches(Value("bar"))),
        expectedOutput = true,
      ),
      Scenario(
        hint = "Two expressions returning true and false",
        json = """{"foo": "bar"}""",
        expr = Value(true).when(($ / "foo").notNull or ($ / "xxx").notNull),
        expectedOutput = true,
      ),
      Scenario(
        hint = "Two expressions returning false",
        json = """{"foo": "bar"}""",
        expr = Value(true).when(($ / "yyy").notNull or ($ / "xxx").notNull),
        expectedOutput = None,
      ),
    )

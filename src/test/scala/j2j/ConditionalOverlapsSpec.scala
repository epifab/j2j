package j2j

class ConditionalOverlapsSpec
    extends ExpressionEvaluationTester(
      Scenario(
        hint = "Two lists are the same",
        json = "{}",
        expr = Value(true).when(Value(Vector(1, 2)) overlaps Value(Vector(1, 2))),
        expectedOutput = true,
      ),
      Scenario(
        hint = "Every element of src is contained in the other list",
        json = "{}",
        expr = Value(true).when(Value(Vector(1, 2)) overlaps Value(Vector(1, 2, 3))),
        expectedOutput = true,
      ),
      Scenario(
        hint = "Every element of the other list is contained in src",
        json = "{}",
        expr = Value(true).when(Value(Vector(1, 2, 3)) overlaps Value(Vector(1, 2))),
        expectedOutput = true,
      ),
      Scenario(
        hint = "Some common elements between the two lists",
        json = "{}",
        expr = Value(true).when(Value(Vector(1, 2)) overlaps Value(Vector(2, 3))),
        expectedOutput = true,
      ),
      Scenario(
        hint = "Two lists have nothing in common",
        json = "{}",
        expr = Value(true).when(Value(Vector(1, 2)) overlaps Value(Vector(3, 4))),
        expectedOutput = None,
      ),
      Scenario(
        hint = "Src element not in list",
        json = "{}",
        expr = Value(true).when(Value(Vector(1)) overlaps Value(Vector(3, 4))),
        expectedOutput = None,
      ),
    )

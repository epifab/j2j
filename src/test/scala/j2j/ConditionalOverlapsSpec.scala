package j2j

class ConditionalOverlapsSpec
    extends ExpressionEvaluationTester(
      Scenario(
        hint = "Two lists are the same",
        json = "{}",
        expr = """{ value: true, when: { src = [1, 2], overlaps = [1, 2] } }""",
        expectedOutput = true,
      ),
      Scenario(
        hint = "Every element of src is contained in the other list",
        json = "{}",
        expr = """{ value: true, when: { src = [1, 2], overlaps = [1, 2, 3] } }""",
        expectedOutput = true,
      ),
      Scenario(
        hint = "Every element of the other list is contained in src",
        json = "{}",
        expr = """{ value: true, when: { src = [1, 2, 3], overlaps = [1, 2] } }""",
        expectedOutput = true,
      ),
      Scenario(
        hint = "A single element is evaluated as a list",
        json = "{}",
        expr = """{ value: true, when: { src = 1, overlaps = [1, 2] } }""",
        expectedOutput = true,
      ),
      Scenario(
        hint = "Some common elements between the two lists",
        json = "{}",
        expr = """{ value: true, when: { src = [1, 2], overlaps = [2, 3] } }""",
        expectedOutput = true,
      ),
      Scenario(
        hint = "Two lists have nothing in common",
        json = "{}",
        expr = """{ value: true, when: { src = [1, 2], overlaps = [3, 4] } }""",
        expectedOutput = None,
      ),
      Scenario(
        hint = "Src element not in list",
        json = "{}",
        expr = """{ value: true, when: { src = 1, overlaps = [3, 4] } }""",
        expectedOutput = None,
      ),
    )
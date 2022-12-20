package ajr.scemplate

import utest._

object TestExpressions extends TestSuite with TestHelper {
  override def utestAfterAll() = {
    opCheck(195)
  }

  val tests = Tests {
    test("types") {
      test("string") { validateExpression(""""test"""", "test") }
      test("int") { validateExpression("1", 1) }
      test("double") { validateExpression("1.2", 1.2) }
      test("boolean") { validateExpression("true", true) }
    }
    test("basic") {
      test("addInt ") { validateExpression("OneInt + 2", 3) }
      test("addString ") { validateExpression("""OneString + " & only"""", "1 & only") }
    }
    test("badExpression") {
      test("empty") { expressionParseError("", "Error failed expecting") }
      test("intAndDouble") { intercept[BadTypeException] { validateExpression("1 == 1.2", "") } }
    }
  }
}

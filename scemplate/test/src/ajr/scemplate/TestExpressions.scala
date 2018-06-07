package ajr.scemplate

import utest._

object TestExpressions extends TestSuite with TestHelper {
  override def utestAfterAll() = {
    opCheck(236)
  }

  val tests = Tests {
    'types - {
      'string - validateExpression(""""test"""", "test")
      'int - validateExpression("1", 1)
      'double - validateExpression("1.2", 1.2)
      'boolean - validateExpression("true", true)
    }
    'basic - {
      'addInt  - validateExpression("OneInt + 2", 3)
      'addString  - validateExpression("""OneString + " & only"""", "1 & only")
    }
    'badExpression - {
      'empty - expressionParseError("", "Error failed expecting function | brackets | value")
      'intAndDouble - intercept[BadTypeException] { validateExpression("1 == 1.2", "") }
    }
  }
}

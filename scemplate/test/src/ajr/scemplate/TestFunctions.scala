package ajr.scemplate

import utest._
import implicits._

object TestFunctions extends TestSuite with TestHelper {
    val tests = Tests {
      test("len") {
        validate("${len(numbersMap)}", "3")
      }
      test("keys/value") {
        validate("${keys(numbersMap)}", "[1,2,3]")
        validate("${for key in keys(numbersMap)}$key:${value(numbersMap,key)} ${endfor}", "1:one 2:two 3:three ")
      }

      test("map iterate") {
        validate("""|${for item in numberTypes}
                    |${item.name}: ${item.age}
                    |${endfor}
                    |""".stripMargin,
                 """|andrew: 21
                    |sally: 39
                    |fred: 34
                    |""".stripMargin)
      }
    }
}
package ajr.scemplate

import utest._

object TestConstructs extends TestSuite with TestHelper {
  override def utestAfterAll() = {
    opCheck(1055)
  }

  val tests = Tests {
    test("constructs") {
      test("forLoop") {
        test("basic") {
          val forLoopTmpl =
            """
              |Header
              |${for i in range(0,2)}
              |Line $i
              |${endfor}
              |Footer
            """.stripMargin

          val expected =
            """
              |Header
              |Line 0
              |Line 1
              |Footer
            """.stripMargin

          validate(forLoopTmpl, expected)
        }
        test("lineEnding") {
          val forLoopTmpl =
            """
              |Header
              |${for i in range(0,2)}
              |
              |Line $i
              |${endfor}
              |Footer
            """.stripMargin

          val expected =
            """
              |Header
              |
              |Line 0
              |
              |Line 1
              |Footer
            """.stripMargin

          validate(forLoopTmpl, expected)
        }
        test("nested") {
          val forLoopTmpl =
            """
              |Header
              |${for i in range(0,2)}
              |Header: $i
              |${for j in range(0,2)}
              |Line $i,$j - ${i+j}
              |${endfor}
              |${endfor}
              |Footer
            """.stripMargin

          val expected =
            """
              |Header
              |Header: 0
              |Line 0,0 - 0
              |Line 0,1 - 1
              |Header: 1
              |Line 1,0 - 1
              |Line 1,1 - 2
              |Footer
            """.stripMargin

          validate(forLoopTmpl, expected)
        }
      }

      test("ifTheElse") {
        test("ifTrue") { validate("${if true}yes${endif}", "yes") }
        test("ifFalse") { validate("${if false}yes${endif}", "") }
        test("ifElseTrue") { validate("${if true}yes${else}no${endif}", "yes") }
        test("ifElseFalse") { validate("${if false}yes${else}no${endif}", "no") }
        test("ifExpression") { validate("${if age >= 18}adult${else}minor${endif}", "adult") }
        test("ifLineEnding") {
          val tmpl =
            """|${if true}
               |  A line
               |${endif}
               |Another
            """.stripMargin
          val expt =
            """|  A line
               |Another
            """.stripMargin
          test("unix") {
            validate(tmpl, expt)
          }
          test("windows") {
            validate(tmpl.replace("\n", "\r\n"), expt.replace("\n", "\r\n"))
          }
        }
      }
    }
  }
}

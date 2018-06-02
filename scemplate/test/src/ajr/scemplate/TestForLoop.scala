package ajr.scemplate

import utest._

object TestForLoop extends TestSuite with TestHelper {
  override def utestAfterAll() = {
    opCheck(1263)
  }

  val tests = Tests {
    'constructs - {
      'forLoop - {
        'basic - {
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
        'lineEnding - {
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
        'nested - {
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

      'ifTheElse - {
        'ifTrue - validate("${if true}yes${endif}", "yes")
        'ifFalse - validate("${if false}yes${endif}", "")
        'ifElseTrue - validate("${if true}yes${else}no${endif}", "yes")
        'ifElseFalse - validate("${if false}yes${else}no${endif}", "no")
        'ifExpression - validate("${if age >= 18}adult${else}minor${endif}", "adult")
        'ifLineEnding - {
          val tmpl =
            """
              |${if true}
              |A line
              |${endif}
              |Another
            """.stripMargin
          val expt =
            """
              |A line
              |Another
            """.stripMargin
          validate(tmpl, expt)
        }
      }
    }
  }
}

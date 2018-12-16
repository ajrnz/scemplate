package ajr.scemplate

import utest._

object TestMacro extends TestSuite with TestHelper {
  override def utestAfterAll() = {
    opCheck(1243)
  }

  val tests = Tests {
    'macros - {
      'define0 - {
        val tmpl = "${macro test0 ( ) }This is a macro${endmacro}"
         validate(tmpl, "")
      }
      'define1 - {
        val tmpl = "${macro test1(arg1)}Macro: $arg1${endmacro}"
        validate(tmpl, "")
      }
      'define2 - {
        val tmpl = "${macro test2(arg1, arg2)}Macro: $arg1, $arg2${endmacro}"
        validate(tmpl, "")
      }
      'eval0 - {
        val tmpl =
          """${macro copyright()}Copyright 2018 - Andrew Richards${endmacro}
            |${copyright()}
            |""".stripMargin
        validate(tmpl, "Copyright 2018 - Andrew Richards\n")
      }
      'eval1 - {
        val tmpl =
          """${macro bottles(num)}$num green bottles hanging on the wall${endmacro}
            |${bottles(3)}
            |${bottles(2)}
            |${bottles(1)}
          """.stripMargin
        val expt =
          """3 green bottles hanging on the wall
            |2 green bottles hanging on the wall
            |1 green bottles hanging on the wall
          """.stripMargin
        validate(tmpl, expt)
      }
      'eval2 - {
        val tmpl =
          """${macro times(r,c)}${r*c}${endmacro}
            |${for r in range(0,3)}
            |${for c in range(0,3)} ${times(c+1,r+1)}${endfor}
            |
            |${endfor}
            |""".stripMargin
        val expt =
          """ 1 2 3
            | 2 4 6
            | 3 6 9
            |""".stripMargin
        validate(tmpl, expt)
      }

      'scope1 - {
        val tmpl =
          """${for r in range(0,3)}
            |${macro times(r,c)}${r*c}${endmacro}
            |${endfor}
            |${times(2,5)}
            |""".stripMargin
        intercept[Exception] { validate(tmpl, "") }
      }
      'scope2 - {
        val tmpl =
          """${for n in range(1,6)}
            |${macro square(x)}${x*x}${endmacro}
            |${square(n)}
            |${endfor}
            |""".stripMargin
        validate(tmpl, "1\n4\n9\n16\n25\n")
      }
    }
  }
}

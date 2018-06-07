package ajr.scemplate

import utest._
import ajr.scemplate.implicits._

object TemplateTest extends TestSuite with TestHelper {

  override def utestAfterAll() = {
    opCheck(3924)
  }


  val tests = Tests{
    'notDefined1 - intercept[BadNameException] { validate("$foo", "") }
    'notDefined2 - intercept[BadNameException] { validate("${foo.bar}", "") }
    'basics - {
      'string - validate("test", "test")
      'dollar - validate("$$", "$")
      'dollarSpace - invalid("$ {}")
    }
    'types - {
      'literalString - validate("""${"string"}""", "string")
      'literalIntSignPos - validate("${+12}", "12")
      'literalIntSignNeg - validate("${-12}", "-12")
      'literalDouble - validate("${0.2}", "0.2")
      'literalDoubleNeg - validate("${-0.2}", "-0.2")
      'literalDoublePos - validate("${+0.2}", "0.2")
      'literalBooleanTrue - validate("${true}", "true")
      'literalBooleanFalse - validate("${false}", "false")
      'variableString - validate("$OneString", "1")
      'variableStringSimple - validate("$OneString.Another", "1.Another")
      'variableStringMissing - intercept[BadNameException] { validate("$Another.Thing", "") }
      'variableInt - validate("$OneInt", "1")
      'variableBooleanTrue - validate("$trueValue", "true")
      'variableBooleanTrue2 - validate("${trueValue}", "true")
      'variableBooleanFalse - validate("$falseValue", "false")
      'negationTrue - validate("${!true}", "false")
      'negationFalse - validate("${!false}", "true")
    }

    'misc - {
      'whitespace1 - validate("${ ( 1 + 2 ) * 3 - 4 }", "5")
      'whitespace2 - validate("${(1+2)*3-4 == 0}", "false")
      'quotes - validate("""${"test" + "fire"}""", "testfire")
      'quoteQuote - validate("""${"Quote \" here"}""", """Quote " here""")
      'quoteTab - validate("""${"Tab \t char"}""", "Tab \t char")
      'quoteUnicode - validate("""${"\u00a9"}""", "\u00a9")
      'stringEndsWhitespace - validate("""${" test "}""", " test ")
    }

    'idents - {
      invalid("$20one")
      invalid("${20one}")
      validate("${twenty_one}", "21")
    }

    'functions - {
      'oneParam - validate("${lowerCase(this)}", "this")
      'twoParam - validate("""${repeat("abc", 3)}""", "abcabcabc")
      'toFewParams - intercept[BadTypeException] { validate("${lowerCase()}", "this") }
    }

    'expressions - {
      'int - {
        'addition - validate("${1+2}", "3")
        'subtraction - validate("${5-2}", "3")
        'multiplication - validate("${5*2}", "10")
        'division - validate("${10/4}", "2")
        'modulus - validate("${10%3}", "1")
        'precedence - validate("${1+2*3-4}", "3")
        'brackets - validate("${(1+2)*3-4}", "5")
        'mixedTypes1 - validate("${10*0.5}", "5.0")
        'mixedTypes2 - validate("${0.5*10}", "5.0")
      }
      'double - {
        // no ideal comparing doubles like this, I know
        'addition - validate("${1.4+2.2}", "3.6")
        'subtraction - validate("${5.5-2.5}", "3.0")
        'multiplication - validate("${5.5*2.0}", "11.0")
        'division - validate("${10.0/4.0}", "2.5")
        'modulus - validate("${10.0%3.0}", "1.0")
        'mixedTypes1 - validate("${10*0.5}", "5.0")
        'mixedTypes2 - validate("${0.5*10}", "5.0")
      }
    }

    'conditions - {
      'equal - validate("${1 == 1}", "true")
      'equalNot - validate("${1 == 2}", "false")
      'lessThan - validate("${1 < 2}", "true")
      'lessThan2 - validate("${1 < 1}", "false")
      'and1 - validate("${true && true}", "true")
      'and2 - validate("${true && false}", "false")
      'or1 - validate("${true || true}", "true")
      'or2 - validate("${true || false}", "true")
      'or3 - validate("${false || false}", "false")
      'lessThanEqual1 - validate("${1 <= 1}", "true")
      'lessThanEqual2 - validate("${1 <= 2}", "true")
      'lessThanEqual3 - validate("${2 <= 1}", "false")
      'greaterThan1 - validate("${-1 > -2}", "true")
      'greaterThan2 - validate("${-2 > 2}", "false")
      'greaterThanEqual1 - validate("${1 >= 1}", "true")
      'greaterThanEqual2 - validate("${2 >= 1}", "true")
      'greaterThanEqual3 - validate("${1 >= 2}", "false")
      'diffTypes - intercept[BadTypeException] { validate("${2 == false}", "true") }
      'precedence - validate("${(1+2)*3-4 == 4-1*4+(4/2)}", "false")
      'precedence2 - validate("""${"test" == "test" && "it" == "it"}""", "true")
      'multiPrecedence - validate("${1 == 2 == false}", "true")
      'multiBrackets - validate("${false == (1 == 2)}", "true")
      'multiInvalid - intercept[BadTypeException] { validate("${false == 1 == 2}", "ex") }
    }

    'context - {
      'mapConversion - {
        val strs = Map("one" -> "1", "two" -> "2")
        val ints = Map("three" -> 3, "four" -> 4)
        val ctx = Context().withValues(strs).withValues(ints)
        validate("$one $two ${three * four}", "1 2 12")(ctx)
      }
    }

    'errorMessages - {
      'ifStart - parseError("${if true}blah", "Error failed expecting endif")
      //'unclosedString - parseError("""${OneString == "bad}""", """Error failed expecting "\"""")
    }
  }
}

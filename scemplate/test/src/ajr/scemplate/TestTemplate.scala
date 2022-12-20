package ajr.scemplate

import utest._
import ajr.scemplate.implicits._

object TemplateTest extends TestSuite with TestHelper {

  override def utestAfterAll() = {
    opCheck(3780)
  }


  val tests = Tests{
    test("notDefined1") { intercept[BadNameException] { validate("$foo", "") } }
    test("notDefined2") { intercept[BadNameException] { validate("${foo.bar}", "") } }
    test("basics") {
      test("string") { validate("test", "test") }
      test("dollar") { validate("$$", "$") }
      test("dollarSpace") { invalid("$ {}") }
    }
    test("types") {
      test("literalString") { validate("""${"string"}""", "string") }
      test("literalIntSignPos") { validate("${+12}", "12") }
      test("literalIntSignNeg") { validate("${-12}", "-12") }
      test("literalDouble") { validate("${0.2}", "0.2") }
      test("literalDoubleNeg") { validate("${-0.2}", "-0.2") }
      test("literalDoublePos") { validate("${+0.2}", "0.2") }
      test("literalBooleanTrue") { validate("${true}", "true") }
      test("literalBooleanFalse") { validate("${false}", "false") }
      test("variableString") { validate("$OneString", "1") }
      test("variableStringSimple") { validate("$OneString.Another", "1.Another") }
      test("variableStringMissing") { intercept[BadNameException] { validate("$Another.Thing", "") } }
      test("variableInt") { validate("$OneInt", "1") }
      test("variableBooleanTrue") { validate("$trueValue", "true") }
      test("variableBooleanTrue2") { validate("${trueValue}", "true") }
      test("variableBooleanFalse") { validate("$falseValue", "false") }
      test("negationTrue") { validate("${!true}", "false") }
      test("negationFalse") { validate("${!false}", "true") }
    }

    test("misc") {
      test("whitespace1") { validate("${ ( 1 + 2 ) * 3 - 4 }", "5") }
      test("whitespace2") { validate("${(1+2)*3-4 == 0}", "false") }
      test("quotes") { validate("""${"test" + "fire"}""", "testfire") }
      test("quoteQuote") { validate("""${"Quote \" here"}""", """Quote " here""") }
      test("quoteTab") { validate("""${"Tab \t char"}""", "Tab \t char") }
      test("quoteUnicode") { validate("""${"\u00a9"}""", "\u00a9") }
      test("stringEndsWhitespace") { validate("""${" test "}""", " test ") }
    }

    test("idents") {
      invalid("$20one")
      invalid("${20one}")
      validate("${twenty_one}", "21")
    }

    test("functions") {
      test("oneParam") { validate("${lowerCase(this)}", "this") }
      test("twoParam") { validate("""${repeat("abc", 3)}""", "abcabcabc") }
      test("toFewParams") { intercept[BadTypeException] { validate("${lowerCase()}", "this") } }
    }

    test("expressions") {
      test("int") {
        test("addition") { validate("${1+2}", "3") }
        test("subtraction") { validate("${5-2}", "3") }
        test("multiplication") { validate("${5*2}", "10") }
        test("division") { validate("${10/4}", "2") }
        test("modulus") { validate("${10%3}", "1") }
        test("precedence") { validate("${1+2*3-4}", "3") }
        test("brackets") { validate("${(1+2)*3-4}", "5") }
        test("mixedTypes1") { validate("${10*0.5}", "5.0") }
        test("mixedTypes2") { validate("${0.5*10}", "5.0") }
      }
      test("double") {
        // no ideal comparing doubles like this, I know
        test("addition") { validate("${1.4+2.2}", "3.6") }
        test("subtraction") { validate("${5.5-2.5}", "3.0") }
        test("multiplication") { validate("${5.5*2.0}", "11.0") }
        test("division") { validate("${10.0/4.0}", "2.5") }
        test("modulus") { validate("${10.0%3.0}", "1.0") }
        test("mixedTypes1") { validate("${10*0.5}", "5.0") }
        test("mixedTypes2") { validate("${0.5*10}", "5.0") }
      }
    }

    test("conditions") {
      test("equal") { validate("${1 == 1}", "true") }
      test("equalNot") { validate("${1 == 2}", "false") }
      test("lessThan") { validate("${1 < 2}", "true") }
      test("lessThan2") { validate("${1 < 1}", "false") }
      test("and1") { validate("${true && true}", "true") }
      test("and2") { validate("${true && false}", "false") }
      test("or1") { validate("${true || true}", "true") }
      test("or2") { validate("${true || false}", "true") }
      test("or3") { validate("${false || false}", "false") }
      test("lessThanEqual1") { validate("${1 <= 1}", "true") }
      test("lessThanEqual2") { validate("${1 <= 2}", "true") }
      test("lessThanEqual3") { validate("${2 <= 1}", "false") }
      test("greaterThan1") { validate("${-1 > -2}", "true") }
      test("greaterThan2") { validate("${-2 > 2}", "false") }
      test("greaterThanEqual1") { validate("${1 >= 1}", "true") }
      test("greaterThanEqual2") { validate("${2 >= 1}", "true") }
      test("greaterThanEqual3") { validate("${1 >= 2}", "false") }
      test("diffTypes") { intercept[BadTypeException] { validate("${2 == false}", "true") } }
      test("precedence") { validate("${(1+2)*3-4 == 4-1*4+(4/2)}", "false") }
      test("precedence2") { validate("""${"test" == "test" && "it" == "it"}""", "true") }
      test("multiPrecedence") { validate("${1 == 2 == false}", "true") }
      test("multiBrackets") { validate("${false == (1 == 2)}", "true") }
      test("multiInvalid") { intercept[BadTypeException] { validate("${false == 1 == 2}", "ex") } }
    }

    test("context") {
      test("mapConversion") {
        val strs = Map("one" -> "1", "two" -> "2")
        val ints = Map("three" -> 3, "four" -> 4)
        val ctx = Context().withMap(strs).withMap(ints)
        validate("$one $two ${three * four}", "1 2 12")(ctx)
      }
    }

    test("errorMessages") {
      //test("ifStart") { parseError("${if true}blah", "Error failed expecting endif") }
      //test("unclosedString") { parseError("""${OneString == "bad}""", """Error failed expecting "\"""") }
    }

    test("convenienceMethod") {
      Template.render("$OneString") ==> "1"
    }

    test("arrays") {
      test("apply") { validate("${oddNumbers(4)}", "9") }
      test("outOfBounds") { intercept[IndexOutOfBoundsException] { validate("${oddNumbers(5)}", "") } }
      test("multiDim") { intercept[BadTypeException] { validate("${oddNumbers(0,1)}", "") } }
    }

    test("map") {
      test("apply") { validate("""${abbrev("imo")}""", "in my opinion") }
      test("notFound") { intercept[NoSuchElementException] { validate("""${abbrev("rtfm")}""", "") } }
      test("notFoundType") { intercept[NoSuchElementException] { validate("""${abbrev(0)}""", "") } }
      test("multiDim") { intercept[BadTypeException] { validate("""${abbrev("a", "b")}""", "") } }
    }

    test("defined") {
      test("present") { validate("${defined(OneString)}", "true") }
      test("absent") { validate("${defined(NotHere)}", "false") }
      test("caseClassPresent1") { validate("${defined(user.person.name)}", "true") }
      test("caseClassPresent2") { validate("${defined(user)}", "true") }
      test("caseClassAbsent1") { validate("${defined(user.nothere.name)}", "false") }
      test("caseClassAbsent2") { validate("${defined(a.b.c)}", "false") }
    }
  }
}

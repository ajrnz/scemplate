package ajr.scemplate

import utest._

object TestCaseClasses extends TestSuite with TestHelper {
  override def utestAfterAll() = {
    opCheck(178)
  }

  val tests = Tests {
    'caseClasses - {
      'fieldInt - validate("${user.salary}", "80000")
      'subFieldString - validate("${user.person.name}", "Andrew")
      'subFieldInt - validate("${user.person.age}", "21")
      'invalidField - intercept[NoSuchElementException] { validate("${user.invalid}", "") }
    }

    'caseClassConversion - {
      val ccEnc = context.values.value("user")
      val expt = MapValue(Map(
        "person" -> MapValue(Map(
          "name" -> StringValue("Andrew"),
          "age" -> IntValue(21),
          "awards" -> ArrayValue(Vector(StringValue("superstar"), StringValue("humble"))))),
        "salary" -> IntValue(80000),
        "isManager" -> BooleanValue(true))
      )
      ccEnc ==> expt
    }
  }
}

package ajr.scemplate

import utest._

object TestCaseClasses extends TestSuite with TestHelper {
  override def utestAfterAll() = {
    opCheck(142)
  }

  val tests = Tests {
    test("caseClasses") {
      test("fieldInt") { validate("${user.salary}", "80000") }
      test("subFieldString") { validate("${user.person.name}", "Andrew") }
      test("subFieldInt") { validate("${user.person.age}", "21") }
      test("invalidField") { intercept[BadNameException] { validate("${user.invalid}", "") } }

    }

    test("caseClassConversion") {
      val ccEnc = testContext.values.value("user")
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

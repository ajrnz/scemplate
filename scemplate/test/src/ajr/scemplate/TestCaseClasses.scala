package ajr.scemplate

import utest._

object TestCaseClasses extends TestSuite with TestHelper {
  val tests = Tests {
    'caseClasses - {
      'fieldInt - validate("${user.salary}", "80000")
      'subFieldString - validate("${user.person.name}", "Andrew")
      'subFieldInt - validate("${user.person.age}", "21")
      'invalidField - intercept[NoSuchElementException] { validate("${user.invalid}", "") }
    }
  }
}

package ajr.scemplate
import utest._

import ajr.scemplate._
import ajr.scemplate.implicits._

case class Person(name: String, age: Int, awards: Seq[String] = Seq.empty[String])
case class Employee(person: Person, salary: Int, isManager: Boolean)
object Employee {
  implicit def toTV(value: Employee): TemplateValue = CaseClassEncoder.gen[Employee].encode(value)
}

class TemplateInst(tmpl: String, override val instrumentLevel: Int) extends Template(tmpl) {
  def parseOps = totalOps
}


trait TestHelper extends TestSuite {
  implicit val testContext = Context()
    .withValues(
      "OneString" -> "1",
      "OneInt" -> 1,
      "dollars" -> 1000,
      "twenty_one" -> 21,
      "trueValue" -> true,
      "falseValue" -> false,
      "age" -> 18,
      "this" -> "THIS",
      "that" -> "THAT",
      "contract.name.first" -> "Fred",
      "people" -> List("andrew","fred","jim","sally","brenda"),
      "oddNumbers" -> Seq(1,3,5,7,9),
      "titleString" -> "This is my title",
      "user" -> Employee(Person("Andrew", 21, Seq("superstar", "humble")), 80000, true)
    )
    .withFunctions(
      "lowerCase" ->      function(_.toStr.toLowerCase),
      "upperCase" ->      function(_.toStr.toLowerCase),
      "currencyCommas" -> function(_.toStr.reverse.grouped(3).mkString(",").reverse),
      "range" ->          function((s,e) => Range(s.toInt, e.toInt).toSeq),
      "repeat" ->         function((a,b) => a.toStr * b.toInt)
    )

  var totalOps = 0
  val instrumentLevel = 1

  def quoteWhiteSpace(str: String) = str.replaceAll("\n", "\\\\n").replaceAll("\t", "\\\\t")

  def validate(tmpl: String, expt: String)(implicit context: Context) = {
    val t = new TemplateInst(tmpl, instrumentLevel)
    totalOps += t.parseOps
    val err = t.error
    err ==> None
    val result = t.render(context)
    if (result != expt) {
      println(s"result\n$result")
      println(s"expected\n$expt")
      println(s"result:  >${quoteWhiteSpace(result)}<")
      println(s"expected:>${quoteWhiteSpace(expt)}<")
    }
    result ==> expt
  }

  def invalid(tmpl: String) = {
    val t = new TemplateInst(tmpl, instrumentLevel)
    totalOps += t.parseOps
    assert(t.error.isDefined)
  }

  def opCheck(expected: Int) = opDiff("Total", totalOps, expected)

  def opDiff(name: String, actual: Int, expected: Int) = {
    val percDiff = ((actual.toDouble - expected)/expected)*100
    println(f"$name ops: $actual (expected $expected) $percDiff%4.1f%%")
  }
}

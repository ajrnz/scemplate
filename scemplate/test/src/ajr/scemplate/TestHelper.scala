package ajr.scemplate
import utest._

import ajr.scemplate._
import ajr.scemplate.implicits._

case class Person(name: String, age: Int, awards: Seq[String] = Seq.empty[String])
case class Employee(person: Person, salary: Int, isManager: Boolean)
object Employee {
  implicit def toTV(value: Employee): TemplateValue = CaseClassEncoder.gen[Employee].encode(value)
}


trait TestHelper {
  val context = Context()
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

  val instrument = false

  def validate(tmpl: String, expt: String) = {
    val t = new Template(tmpl, instrument = instrument)
    val err = t.error
    assert(err == None)
    val result = t.render(context)
    assert(result == expt)
  }

  def invalid(tmpl: String) = {
    val t = new Template(tmpl, instrument = instrument)
    assert(t.error.isDefined)
  }
}

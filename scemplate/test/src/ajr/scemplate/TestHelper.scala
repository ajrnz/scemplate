package ajr.scemplate
import utest._

import ajr.scemplate._
import ajr.scemplate.implicits._

case class Person(name: String, age: Int, awards: Seq[String] = Seq.empty[String])
case class Employee(person: Person, salary: Int, isManager: Boolean) derives Encode
object Employee {
  implicit def toTV(value: Employee): TemplateValue = value.encode
}

class TemplateInst(tmpl: String, override val instrumentLevel: Int) extends Template(tmpl) {
  def parseOps = totalOps
}

class TemplateExpressionInst(tmpl: String, override val instrumentLevel: Int) extends TemplateExpression(tmpl) {
  def parseOps = totalOps
}


trait TestHelper extends TestSuite {
  implicit val testContext: Context = Context()
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
      "people" -> List("andrew","fred","jim","sally","brenda"),
      "abbrev" -> Map("lol" -> "laugh out loud", "imo" -> "in my opinion"),
      "numbersMap" -> MapValue(Map("1" -> "one", "2" -> "two", "3" -> "three")),
      "oddNumbers" -> Seq(1,3,5,7,9),
      "titleString" -> "This is my title",
      "user" -> Employee(Person("Andrew", 21, Seq("superstar", "humble")), 80000, true),
      "numberTypes" -> Seq(Map("name" -> "andrew", "age" -> "21"), Map("name" -> "sally", "age" -> "39"), Map("name" -> "fred", "age" -> "34"))
    )
    .withFunctions(Functions.stdlib: _*)
    .withFunctions(
      "lowerCase" ->      function(_.asString.toLowerCase),
      "upperCase" ->      function(_.asString.toLowerCase),
      "currencyCommas" -> function(_.asString.reverse.grouped(3).mkString(",").reverse),
      "range" ->          function((s,e) => Range(s.asInt, e.asInt).toSeq),
      "repeat" ->         function((a,b) => a.asString * b.asInt)
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

  def validateExpression(tmpl: String, expt: String)(implicit context: Context) = {
    evalExpression(tmpl, context).asString ==> expt
  }

  def validateExpression(tmpl: String, expt: Int)(implicit context: Context) = {
    evalExpression(tmpl, context).asInt ==> expt
  }

  def validateExpression(tmpl: String, expt: Boolean)(implicit context: Context) = {
    evalExpression(tmpl, context).asBoolean ==> expt
  }

  def validateExpression(tmpl: String, expt: Double)(implicit context: Context) = {
    evalExpression(tmpl, context).asDouble ==> expt
  }

  def evalExpression(tmpl: String, context: Context) = {
    val t = new TemplateExpressionInst(tmpl, instrumentLevel)
    totalOps += t.parseOps
    val err = t.error
    err ==> None
    t.eval(context)
  }

  def parseError(tmpl: String, expt: String)(implicit context: Context) = {
    val t = new TemplateInst(tmpl, instrumentLevel)
    totalOps += t.parseOps
    val err = t.error
    assert(err.nonEmpty && err.get.startsWith(expt))
  }

  def expressionParseError(tmpl: String, expt: String)(implicit context: Context) = {
    val t = new TemplateExpressionInst(tmpl, instrumentLevel)
    totalOps += t.parseOps
    val err = t.error
    assert(err.nonEmpty && err.get.startsWith(expt))
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

package ajr.scemplate
import utest._

import ajr.scemplate.TemplateBuilder._

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
      "titleString" -> "This is my title"
    )
    .withFunctions(
      "lowerCase" ->      function(_.toStr.toLowerCase),
      "upperCase" ->      function(_.toStr.toLowerCase),
      "currencyCommas" -> function(_.toStr.reverse.grouped(3).mkString(",").reverse),
      "range" ->          function((s,e) => ArrayValue(Range(s.toInt, e.toInt).map(IntValue))),
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

  def function(fcn: PrimitiveValue => PrimitiveValue): FunctionSpec =
    FunctionSpec(1, x => fcn(x(0)))

  def function(fcn: (PrimitiveValue,PrimitiveValue) => PrimitiveValue): FunctionSpec =
    FunctionSpec(2, x => fcn(x(0),x(1)))

  def function(fcn: (PrimitiveValue,PrimitiveValue,PrimitiveValue) => PrimitiveValue): FunctionSpec =
    FunctionSpec(3, x => fcn(x(0),x(1),x(2)))

  def function(fcn: (PrimitiveValue,PrimitiveValue,PrimitiveValue,PrimitiveValue) => PrimitiveValue): FunctionSpec =
    FunctionSpec(4, x => fcn(x(0),x(1),x(2),x(3)))

  def function(fcn: (PrimitiveValue,PrimitiveValue,PrimitiveValue,PrimitiveValue,PrimitiveValue) => PrimitiveValue): FunctionSpec =
    FunctionSpec(5, x => fcn(x(0),x(1),x(2),x(3),x(4)))

}

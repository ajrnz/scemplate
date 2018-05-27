package ajr.scemplate
import utest._

import ajr.scemplate.implicits._

trait TestHelper {
  val dict = Map[String,PrimitiveValue](
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
    "people" -> "andrew,fred,jim,sally,brenda",
    "titleString" -> "This is my title"
  ).withDefault(x => throw new Exception(s"Missing variable: $x"))

  val functions = Map(
    "lowerCase" -> FunctionSpec(1, x => StringValue(x(0).toStr.toLowerCase)),
    "upperCase" -> FunctionSpec(1, x => StringValue(x(0).toStr.toUpperCase)),
    "currencyCommas" -> FunctionSpec(1, x => StringValue(x(0).toStr.reverse.grouped(3).mkString(",").reverse)),
    "range" -> FunctionSpec(2, x => ArrayValue(Range(x(0).toInt, x(1).toInt).map(IntValue))),
    "repeat" -> FunctionSpec(2, x=> x(0).toStr * x(1).toInt)
  )

  val context = Context(dict, functions)

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

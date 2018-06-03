package ajr.scemplate


class TemplateException(msg: String, cause: Throwable = null) extends Exception(msg, cause)
class BadTypeException(msg: String, cause: Throwable = null) extends TemplateException(msg, cause)
class BadNameException(msg: String, cause: Throwable = null) extends TemplateException(msg, cause)

sealed trait TemplateExpr
case class Sequence(items: Seq[TemplateExpr]) extends TemplateExpr
case class Literal(string: String) extends TemplateExpr
object EmptyLiteral extends Literal("")
case class ForLoop(index: String, list: Value, expr: Sequence) extends TemplateExpr
case class IfThenElse(pred: Value, thenExpr: TemplateExpr, elseExpr: TemplateExpr) extends TemplateExpr

sealed trait Value extends TemplateExpr

case class MacroDef private (name: String, args: Seq[String], body: TemplateExpr) extends TemplateExpr

object MacroDef {
  def apply(name: String, args: Seq[String], body: Sequence) = {
    val newBody =
      body.items.lastOption match {
        case Some(Literal(str)) if str.endsWith("\n") =>
          Sequence(body.items.updated(body.items.size -1, Literal(str.substring(0, str.length-1))))
        case _ =>
          body
      }
    new MacroDef(name, args, newBody)
  }
}

sealed trait TemplateValue extends Value with Ordered[TemplateValue] {
  def toStr: String
  def toInt: Int = badType(s"$this cannot be converted to an integer")
  def toDouble: Double = badType(s"$this cannot be converted to a double")
  def toBoolean: BooleanValue = badType(s"$this cannot be converted to a boolean")
  def toArray: ArrayValue = badType(s"$this cannot be converted to an array")
  def toMap: MapValue = badType(s"$this cannot be converted to a map")

  def badType(message: String)= throw new BadTypeException(message)
}

case class StringValue(value: String) extends TemplateValue {
  def compare(that: TemplateValue): Int = that match {
    case StringValue(thatString) => value.compare(thatString)
    case _ => badType(s"Can't compare string $this with $that")
  }
  override def toStr = value
  override def toInt = value.toInt
  override def toDouble = value.toDouble
}

case class IntValue(value: Int) extends TemplateValue {
  def compare(that: TemplateValue): Int = that match {
    case IntValue(thatValue) =>
      value.compare(thatValue)
    case _ => badType(s"int: Can't compare int $this with $that")
  }
  override def toStr = value.toString
  override def toInt = value
  override def toDouble = value.toDouble
  override def toBoolean: BooleanValue = BooleanValue(value != 0)
}

case class DoubleValue(value: Double) extends TemplateValue {
  def compare(that: TemplateValue): Int = that match {
    case DoubleValue(thatValue) =>
      value.compare(thatValue)
    case _ => badType(s"int: Can't compare int $this with $that")
  }
  override def toStr = value.toString
  override def toInt = value.toInt
  override def toDouble = value
  override def toBoolean: BooleanValue = BooleanValue(value != 0.0)
}

case class BooleanValue(value: Boolean) extends TemplateValue {
  def compare(that: TemplateValue): Int = that match {
    case BooleanValue(thatValue) =>
      value.compare(thatValue)
    case _ => badType(s"Can't compare boolean $this with $that")
  }
  override def toStr = value.toString
  override def toBoolean: BooleanValue = this
}

object BooleanValue {
  val trueV = new BooleanValue(true)
  val falseV = new BooleanValue(false)
  def apply(value: Boolean): BooleanValue = if (value) trueV else falseV
}

case class ArrayValue(value: Seq[TemplateValue]) extends TemplateValue {
  def compare(that: TemplateValue): Int = badType(s"Can't compare array values")
  override def toStr = "[" + value.map(_.toStr).mkString(",") + "]"
  override def toArray = this
}

case class MapValue(value: Map[String,TemplateValue]) extends TemplateValue {
  def compare(that: TemplateValue): Int = badType(s"Can't compare map values")
  override def toStr = "{" + value.map{case(k,v) => s"k=${v.toStr}"}.mkString(",") +"}"
  override def toMap = this
  def apply(key: String) = value(key)
}

object MapValue {
  val empty = MapValue(Map.empty)
}

case class Variable(name: Seq[String]) extends Value
case class Function(name: String, params: Seq[Value]) extends Value

case class Negate(a: Value) extends Value
case class Add(a: Value, b: Value) extends Value
case class Subtract(a: Value, b: Value) extends Value
case class Multiply(a: Value, b: Value) extends Value
case class Divide(a: Value, b: Value) extends Value
case class Modulus(a: Value, b: Value) extends Value

sealed trait ConditionalExpr extends Value {
  val a: Value
  val b: Value
}

case class And(a: Value, b: Value) extends ConditionalExpr
case class Or(a: Value, b: Value) extends ConditionalExpr
case class Equals(a: Value, b: Value) extends ConditionalExpr
case class NotEqual(a: Value, b: Value) extends ConditionalExpr
case class GreaterThanEqual(a: Value, b: Value) extends ConditionalExpr
case class GreaterThan(a: Value, b: Value) extends ConditionalExpr
case class LessThanEqual(a: Value, b: Value) extends ConditionalExpr
case class LessThan(a: Value, b: Value) extends ConditionalExpr

object ConditionalExpr {
  def apply(a: Value, cond: String, b: Value): ConditionalExpr = cond match {
    case "==" => Equals(a, b)
    case "!=" => NotEqual(a, b)
    case ">"  => GreaterThan(a, b)
    case ">=" => GreaterThanEqual(a, b)
    case "<"  => LessThan(a, b)
    case "<=" => LessThanEqual(a, b)
  }
}

case class FunctionSpec(numParams: Int, function: Seq[TemplateValue] => TemplateValue)

case class Context(values: MapValue = MapValue.empty, functions: Map[String, FunctionSpec] = Map.empty) {
  def withValues(items: (String, TemplateValue)*): Context = {
    copy(values = MapValue(values.value ++ items))
  }
  def withFunctions(items: (String, FunctionSpec)*): Context = {
    copy(functions = functions ++ items)
  }
}

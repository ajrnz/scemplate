package ajr.scemplate

object implicits {
  implicit def toStringValue(value: String) = StringValue(value)
  implicit def toIntValue(value: Int) = IntValue(value)
  implicit def toBooleanValue(value: Boolean) = BooleanValue(value)
}

sealed trait TemplateExpr
case class Sequence(items: Seq[TemplateExpr]) extends TemplateExpr
case class Literal(string: String) extends TemplateExpr
object EmptyLiteral extends Literal("")
case class ForLoop(index: String, list: Value, expr: Sequence) extends TemplateExpr
case class IfThenElse(pred: Value, thenExpr: TemplateExpr, elseExpr: TemplateExpr) extends TemplateExpr

sealed trait Value extends TemplateExpr

sealed trait PrimitiveValue extends Value with Ordered[Value] {
  def toInt: Int = sys.error(s"$this cannot be converted to an integer")
  def toStr: String
  def toBoolean: BooleanValue = sys.error(s"$this cannot be converted to a boolean")
  def toArray: ArrayValue = sys.error(s"$this cannot be converted to an array")
  def toMap: MapValue = sys.error(s"$this cannot be converted to a map")
}

case class StringValue(value: String) extends PrimitiveValue {
  def compare(that: Value): Int = that match {
    case StringValue(thatString) => value.compare(thatString)
    case _ => throw new Exception(s"Can't compare string $this with $that")
  }
  override def toStr = value
}

case class IntValue(value: Int) extends PrimitiveValue {
  def compare(that: Value): Int = that match {
    case IntValue(thatValue) =>
      println(s"Int compare $value vs $thatValue")
      value - thatValue
    case _ => throw new Exception(s"int: Can't compare int $this with $that")
  }
  override def toStr = value.toString
  override def toInt = value
  override def toBoolean: BooleanValue = BooleanValue(value != 0)
}

case class BooleanValue(value: Boolean) extends PrimitiveValue {
  def compare(that: Value): Int = that match {
    case BooleanValue(thatValue) => if (value == thatValue) 0 else 1
    case _ => throw new Exception(s"Can't compare boolean $this with $that")
  }
  override def toStr = value.toString
  override def toBoolean: BooleanValue = this
}

object BooleanValue {
  val trueV = new BooleanValue(true)
  val falseV = new BooleanValue(false)
  def apply(value: Boolean): BooleanValue = if (value) trueV else falseV
}

case class ArrayValue(value: IndexedSeq[PrimitiveValue]) extends PrimitiveValue {
  def compare(that: Value): Int = throw new Exception(s"Can't compare array values")
  override def toStr = "[" + value.map(_.toStr).mkString(",") + "]"
  override def toArray = this
}

case class MapValue(value: Map[String,PrimitiveValue]) extends PrimitiveValue {
  def compare(that: Value): Int = throw new Exception(s"Can't compare map values")
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

case class FunctionSpec(numParams: Int, function: Seq[PrimitiveValue] => PrimitiveValue)

case class Context(values: MapValue = MapValue.empty, functions: Map[String, FunctionSpec] = Map.empty) {
  def withValues(items: (String, PrimitiveValue)*): Context = {
    copy(values = MapValue(values.value ++ items))
  }
  def withFunctions(items: (String, FunctionSpec)*): Context = {
    copy(functions = functions ++ items)
  }
}

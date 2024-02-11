package ajr.scemplate

import scala.language.implicitConversions


object implicits {
  implicit class SeqToSeqTemplateValue[X](s: Seq[X]) {
    def toArrayValue(implicit conv: X => TemplateValue): ArrayValue = ArrayValue(s.map(i => conv(i)).toIndexedSeq)
  }

  implicit class MapToMapTemplateValue[X](s: Map[String,X]) {
    def toMapValue(implicit conv: X => TemplateValue): MapValue = MapValue(s.map(i => i._1 -> (conv(i._2))))
  }

  implicit def toStringValue(value: String): TemplateValue = StringValue(value)
  implicit def toIntValue(value: Int): TemplateValue = IntValue(value)
  implicit def toIntValue(value: Boolean): TemplateValue = BooleanValue(value)
  implicit def toDoubleValue(value: Double): TemplateValue = DoubleValue(value)
  implicit def seqToArrayValue[T](items: Seq[T])(implicit conv: T => TemplateValue): TemplateValue = items.toArrayValue
  implicit def mapToMapValue[T](m: Map[String, T])(implicit conv: T => TemplateValue): TemplateValue = m.toMapValue


  implicit val showString: Encode[String] = new Encode[String] { extension (v: String) def encode: TemplateValue = StringValue(v) }
  implicit val showInt: Encode[Int] = new Encode[Int] { extension (v: Int) def encode: TemplateValue = IntValue(v) }
  implicit val showDouble: Encode[Double] = new Encode[Double] { extension (v: Double) def encode: TemplateValue = DoubleValue(v) }
  implicit val showBoolean: Encode[Boolean] = new Encode[Boolean] { extension (v: Boolean) def encode: TemplateValue = BooleanValue(v) }
  def showArray[X](implicit lift: X => TemplateValue): Encode[Seq[X]] =
    new Encode[Seq[X]] { extension (v: Seq[X]) def encode: TemplateValue = v.toArrayValue }
  implicit val showArrayString: Encode[Seq[String]] = showArray[String]


  def function(fcn: TemplateValue => TemplateValue): FunctionSpec =
    FunctionSpec(1, x => fcn(x(0)))

  def function(fcn: (TemplateValue,TemplateValue) => TemplateValue): FunctionSpec =
    FunctionSpec(2, x => fcn(x(0),x(1)))

  def function(fcn: (TemplateValue,TemplateValue,TemplateValue) => TemplateValue): FunctionSpec =
    FunctionSpec(3, x => fcn(x(0),x(1),x(2)))

  def function(fcn: (TemplateValue,TemplateValue,TemplateValue,TemplateValue) => TemplateValue): FunctionSpec =
    FunctionSpec(4, x => fcn(x(0),x(1),x(2),x(3)))

  def function(fcn: (TemplateValue,TemplateValue,TemplateValue,TemplateValue,TemplateValue) => TemplateValue): FunctionSpec =
    FunctionSpec(5, x => fcn(x(0),x(1),x(2),x(3),x(4)))
}

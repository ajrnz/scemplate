package ajr.scemplate

object implicits {
  implicit class SeqToSeqTemplateValue[X](s: Seq[X]) {
    def toArrayValue(implicit conv: X => TemplateValue): ArrayValue = ArrayValue(s.map(i => i: TemplateValue).toIndexedSeq)
  }

  implicit class MapToMapTemplateValue[X](s: Map[String,X]) {
    def toMapValue(implicit conv: X => TemplateValue): MapValue = MapValue(s.map(i => i._1 -> (i._2: TemplateValue)))
  }

  implicit def toStringValue(value: String) = StringValue(value)
  implicit def toIntValue(value: Int) = IntValue(value)
  implicit def toIntValue(value: Boolean) = BooleanValue(value)
  implicit def toDoubleValue(value: Double) = DoubleValue(value)
  implicit def seqToArrayValue[T](items: Seq[T])(implicit conv: T => TemplateValue) = items.toArrayValue
  implicit def mapToMapValue[T](m: Map[String, T])(implicit conv: T => TemplateValue) = m.toMapValue

  implicit val showString = new Encode[String] { def encode(v: String) = StringValue(v) }
  implicit val showInt = new Encode[Int] { def encode(v: Int) = IntValue(v) }
  implicit val showDouble = new Encode[Double] { def encode(v: Double) = DoubleValue(v) }
  implicit val showBoolean = new Encode[Boolean] { def encode(v: Boolean) = BooleanValue(v) }
  def showArray[X](implicit lift: X => TemplateValue): Encode[Seq[X]] =
    new Encode[Seq[X]] { def encode(v: Seq[X]) = v.toArrayValue }
  implicit val showArrayString = showArray[String]

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

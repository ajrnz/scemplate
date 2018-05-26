import $ivy.`com.lihaoyi::fastparse:1.0.0`
import $ivy.`com.lihaoyi::ammonite-ops:1.1.2`
import ammonite.ops._
import fastparse.WhitespaceApi
//import fastparse.WhitespaceApi

//import fastparse.core._
import fastparse.core.Parsed
//import StringTemplate.White._

sealed trait TemplateExpr
case class Sequence(items: Seq[TemplateExpr])extends TemplateExpr
case class Literal(string: String) extends TemplateExpr
object EmptyLiteral extends Literal("")
case class ForLoop(index: String, list: String, expr: TemplateExpr) extends TemplateExpr
case class IfThenElse(pred: ConditionalExpr, thenExpr: TemplateExpr, elseExpr: TemplateExpr) extends TemplateExpr

sealed trait Value extends TemplateExpr

sealed trait PrimitiveValue extends Value with Ordered[Value] {
  def toInt: Int = sys.error(s"$this cannot be converted to an int")
  def toStr: String
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
}
case class BooleanValue(value: Boolean) extends PrimitiveValue {
  def compare(that: Value): Int = that match {
    case BooleanValue(thatValue) => if (value == thatValue) 0 else 1
    case _ => throw new Exception(s"Can't compare boolean $this with $that")
  }
  override def toStr = value.toString
}
object BooleanValue {
  val trueV = new BooleanValue(true)
  val falseV = new BooleanValue(false)
  def apply(value: Boolean): BooleanValue = if (value) trueV else falseV
}

case class ArrayValue(seq: Seq[Value]) extends Value
case class Variable(name: String) extends Value
case class Function(name: String, params: Seq[Value]) extends Value

case class Addition(a: Value, b: Value) extends Value
case class Subtraction(a: Value, b: Value) extends Value
case class Multiplication(a: Value, b: Value) extends Value
case class Division(a: Value, b: Value) extends Value

sealed trait ConditionalExpr extends Value {
  val a: Value
  val b: Value
}
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

case class Context(dict: Map[String, PrimitiveValue], functions: Map[String, FunctionSpec])

object StringTemplate {
  val White = fastparse.WhitespaceApi.Wrapper{
    import fastparse.all._
    val ws = P(" " | "\t" | "\n").rep
    NoTrace(ws)
  }

  import fastparse.noApi._
  import White._


  val cc = P("}")
  val ccx = P("}" ~~ "\n".?)
  val reserved = Set("endfor", "endif", "else")
  val identStart = P(CharPred(x => x.isLetter || x == '_'))
  val ident = P((identStart ~~ CharsWhile(x => x.isLetterOrDigit || x == '_')).!).filter(!reserved(_))
  val dollar = P("$").map(_ => Literal("$"))

  val digit = P(CharPred(_.isDigit))

  val integer = P(("+" | "-").? ~~ digit.repX(min=1)).!.map(v => IntValue(v.toInt))
  val string = P("\"" ~~ CharsWhile(_ != '"').! ~~ "\"").map(StringValue)
  val boolean = P("true" | "false").map(v => BooleanValue(v == "true"))

  val literal = P(integer | string | boolean)
  val variable: P[Value] = P(ident.map(Variable))
  val value: P[Value] = P(variable | literal)
  val function = P((ident ~~ "(" ~ value.rep(sep = ",") ~ ")").map(Function.tupled))

  val brackets = P("(" ~/ expression ~ ")")
  val valueType = P(function | brackets | value)
//  val multiplication = P(valueType ~ "*" ~ valueType).map(Multiplication.tupled)
//  val division = P(valueType ~ "/" ~ valueType).map(Division.tupled)
  val addition = P(valueType ~ ("+" ~ addSub).?).map(v => (if (v._2.isDefined) Addition(v._1, v._2.get) else v._1))
  val subtraction = P(valueType ~ ("-" ~ addSub).?).map(v => (if (v._2.isDefined) Subtraction(v._1, v._2.get) else v._1))


//  val mulDiv = multiplication | division
  val addSub: P[Value] = P(addition | subtraction)

  val expression: P[Value] = P(addSub)
  val evalExpression: P[TemplateExpr] = P("{" ~ expression ~ cc)
  val conditional = P(expression ~ ("==" | "!=" | ">" | ">=" | "<" | "<=").! ~/ expression).map(x => ConditionalExpr(x._1, x._2, x._3)) // XXX add cut?
  val untilDollar = P(CharsWhile(_ != '$').!).map(Literal)
  def cmd(cmdName: String) = P("${" ~ cmdName ~ ccx)
  val forLoop = P(("{for " ~/ ident ~ "in" ~ ident ~ ccx ~/ mainText ~ cmd("endfor")).map(x => ForLoop(x._1, x._2, x._3)))
  val ifThenElse = P(("{if" ~/ conditional ~ ccx ~/ mainText ~ (cmd("else") ~/ mainText).? ~ cmd("endif"))
    .map(x => IfThenElse(x._1, x._2, x._3.getOrElse(EmptyLiteral))))
  val construct: P[TemplateExpr] = P(forLoop | ifThenElse)

  val dollarExpression: P[TemplateExpr] = P("$" ~~ (dollar | construct | variable | evalExpression))
  val mainText: P[Sequence] = P(dollarExpression | untilDollar).repX.map(Sequence)

  val mainDoc: P[Sequence] = P(Start ~~ mainText ~~ End)
}

class Template(templateText: String) {
  //import StringTemplate.White._
  private val instrumentFunction = (parser: fastparse.core.Parser[_,Char,String], index: Int, continuation: () => fastparse.core.Parsed[_, Char,String]) => {
    println(f"Call: ${parser.toString}%-20s $index%3d: ${templateText.drop(index).take(20).replaceAll("\n", "\\\\n")}")
  }

  def error = tree.left.toOption

  val tree: Either[String, TemplateExpr] = {
    val start = System.currentTimeMillis
    StringTemplate.mainDoc.parse(templateText, instrument = instrumentFunction) match {
      case Parsed.Success(output, _) =>
        val parseTime = System.currentTimeMillis - start
        println(s"ParseTime: $parseTime")
        Right(output)

      case err@Parsed.Failure(last, index, extra) =>
      System.err.println(extra.traced.stack.mkString("\n"))
      val currentContext = templateText.drop(index).take(20).replaceAll("\n", "\\\\n")
      Left(s"Error failed expecting $last at pos $index ${textPos(templateText,index)}: $currentContext...")
    }
  }

  def textPos(text: String, index: Int) = {
    val lines = text.take(index).split("\n")
    (lines.size, lines.last.length)
  }

  def render(context: Context): String =  {
    tree match {
      case Right(template) =>
        val sb = new StringBuilder
        render(template, context, sb)
        sb.result()

      case Left(err) =>
        throw new IllegalArgumentException(s"Cannot render invalid template: $err")
    }
  }

  private def render(template: TemplateExpr, context: Context, sb: StringBuilder): Unit = {
    template match {
      case Sequence(items) =>
        items.foreach(render(_, context, sb))

      case Literal(str) =>
        sb ++= str

      case value: Value =>
        sb ++= evalValue(value, context).toStr

      case ForLoop(index, list, expr) =>
        context.dict(list).toStr.split(",").foreach{ item =>
          render(expr, context.copy(dict = context.dict + (index -> StringValue(item))), sb)
        }

      case IfThenElse(pred, thenExpr, elseExpr) =>
        val predValue = evalValue(pred, context)
        val expr = if (predValue == BooleanValue.trueV) thenExpr else elseExpr
        render(expr, context, sb)
    }
  }

  def evalValue(value: Value, context: Context): PrimitiveValue = {
    value match  {
      case x: StringValue => x
      case x: IntValue => x
      case x: BooleanValue => x

      case Variable(name) =>
        context.dict(name)

      case Function(name, params) =>
        val paramValues = params.map(p => evalValue(p, context))
        context.functions(name).function(paramValues)

      case cond: ConditionalExpr =>
        val a = evalValue(cond.a, context)
        val b = evalValue(cond.b, context)
        val res = BooleanValue(cond match {
          case _: Equals =>           a == b
          case _: NotEqual =>         a != b
          case _: GreaterThanEqual => a >= b
          case _: GreaterThan =>      a >  b
          case _: LessThanEqual =>    a <= b
          case _: LessThan =>         a <  b
        })
        println(cond)
        println(res)
        res

      case add: Addition =>
        val a = evalValue(add.a, context)
        val b = evalValue(add.b, context)
        IntValue(a.toInt + b.toInt)
    }
  }
}



val num = 1
val fileName = s"test$num.tmpl"

val dict = Map[String,PrimitiveValue](
  "dollars" -> IntValue(1000),
  "age" -> IntValue(21),
  "this" -> StringValue("THIS"),
  "that" -> StringValue("THAT"),
  "contract.name.first" -> StringValue("Fred"),
  "people" -> StringValue("andrew,fred,jim,sally,brenda")
).withDefault(x => throw new Exception(s"Missing variable: $x"))

case class FunctionSpec(numParams: Int, function: Seq[PrimitiveValue] => PrimitiveValue)

val functions = Map(
  "lowerCase" -> FunctionSpec(1, x => StringValue(x(0).toStr.toLowerCase)),
  "upperCase" -> FunctionSpec(1, x => StringValue(x(0).toStr.toUpperCase)),
  "currencyCommas" -> FunctionSpec(1, x => StringValue(x(0).toStr.reverse.grouped(3).mkString(",").reverse))
)

val context = Context(dict, functions)

val text: String = read! pwd / fileName

println("Compiling template")
val tmpl = new Template(text)
println("Compiled.")
println(tmpl.tree)
tmpl.error match {
  case Some(err) =>
    println("Invalid template")
    println(err)

  case None =>
    println("Running")
    if (true) {
      val result = tmpl.render(context)
      println(result)
    }
    else {
      val secs = 60
      val start = System.currentTimeMillis

      val loopSize = 400000
      val maxMillis = secs * 1000
      var count = 0
      while((System.currentTimeMillis - start) < maxMillis) {
        val s = System.currentTimeMillis
        for(i <- 0 to loopSize) {
          val result = tmpl.render(context)
          //println(result)
        }
        val d = System.currentTimeMillis -s
        println(d)
        count += 1
      }
      val total = System.currentTimeMillis - start
      val perSecond = count.toDouble * loopSize / (total.toDouble / 1000.0)
      println(count * loopSize)
      println((perSecond * secs).toInt, "per second")
  }
}

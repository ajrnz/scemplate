package ajr.scemplate

import java.util.NoSuchElementException

import ammonite.ops._
import fastparse.WhitespaceApi
import fastparse.core.Parsed


object TemplateBuilder {
  implicit def toStringValue(value: String) = StringValue(value)
  implicit def toIntValue(value: Int) = IntValue(value)
  implicit def toIntValue(value: Boolean) = BooleanValue(value)
  implicit def seqToArrayValue(items: Seq[String]) = ArrayValue(items.toIndexedSeq.map(StringValue(_)))
  implicit def seqIntToArrayValue(items: Seq[Int]) = ArrayValue(items.toIndexedSeq.map(IntValue(_)))

  implicit val showString = new Show[String] { def encode(v: String) = StringValue(v) }
  implicit val showInt = new Show[Int] { def encode(v: Int) = IntValue(v) }
  implicit val showBoolean = new Show[Boolean] { def encode(v: Boolean) = BooleanValue(v) }
  def showArray[X](implicit lift: X => TemplateValue): Show[Seq[X]] =
    new Show[Seq[X]] { def encode(v: Seq[X]) = ArrayValue(v.map(x => lift(x)).toIndexedSeq) }
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

object Template {
  val White = fastparse.WhitespaceApi.Wrapper{
    import fastparse.all._
    val ws = P(" " | "\t" | "\n").rep
    NoTrace(ws)
  }

  import fastparse.noApi._
  import White._

  val reserved = Set("endfor", "endif", "else", "true", "false")

  val ccx: P[Unit] = "}" ~~ "\n".?
  val identStart = CharPred(x => x.isLetter || x == '_')
  val identChar = CharsWhile(x => x.isLetterOrDigit || x == '_')
  val ident = P((identStart ~~ identChar.?).!).filter(!reserved(_))
  val dollar = P("$").map(_ => Literal("$"))

  val digit = P(CharPred(_.isDigit))
  val integer = P(("+" | "-").? ~~ digit.repX(min=1)).!.map(v => IntValue(v.toInt))
  val string = P("\"" ~~ CharsWhile(_ != '"').! ~~ "\"").map(StringValue(_)) // XXX no way of escaping quotes
  val boolean = P(("true" | "false") ~ !identChar).!.map(v => BooleanValue(v == "true"))

  val literal = P(integer | string | boolean)
  val variable: P[Value] = P(ident ~ ("." ~ ident).rep).map(x => Variable(x._1 +: x._2))
  val value: P[Value] = P(literal | variable)
  val function = P((ident ~~ "(" ~ value.rep(sep = ",") ~ ")").map(Function.tupled))

  val brackets: P[Value] = P("(" ~/ expression ~ ")")
  val valueType: P[Value] = P("!".!.? ~ (function | brackets | value)).map{x=> x._1 match {
    case Some(_) => Negate(x._2)
    case None    => x._2
  }}

  val multiDivMod: P[Value] = P(valueType ~ (("*" | "/" | "%").! ~ valueType).rep).map{x =>
    x._2.foldLeft(x._1){case(c, (op, value)) => op match {
      case "*" => Multiply(c, value)
      case "/" => Divide(c, value)
      case "%" => Modulus(c, value)
    }}
  }
  val addSub: P[Value] = P(multiDivMod ~ (("+"|"-").! ~ multiDivMod).rep).map{x =>
    x._2.foldLeft(x._1){case(c, (op, value)) =>
      if (op == "+") Add(c, value) else Subtract(c, value)
    }
  }
  val andOr: P[Value] = P(addSub ~ (("&&"|"||").! ~ addSub).rep).map{x =>
    x._2.foldLeft(x._1){case(c, (op, value)) =>
      if (op == "&&") And(c, value) else Or(c, value)
    }
  }
  val conditional: P[Value] = P(andOr ~ (("==" | "!=" |  ">=" | ">" | "<=" | "<").! ~ andOr).rep).map{x =>
    x._2.foldLeft(x._1){case(c, (op, value)) =>
      ConditionalExpr(c, op, value)
    }
  }


  val expression: P[Value] = P(conditional)
  val evalExpression: P[TemplateExpr] = P("{" ~ expression ~ "}")
  def cmd(cmdName: String): P[Unit] = "${" ~ cmdName ~ ccx
  val forLoop = P(("{for " ~/ ident ~ "in" ~ expression ~ ccx ~~ mainText ~ cmd("endfor")).map(x => ForLoop(x._1, x._2, x._3)))
  val ifThenElse = P(("{if" ~/ conditional ~ ccx ~/ mainText ~ (cmd("else") ~/ mainText).? ~ cmd("endif"))
    .map(x => IfThenElse(x._1, x._2, x._3.getOrElse(EmptyLiteral))))
  val construct: P[TemplateExpr] = P(forLoop | ifThenElse)
  val untilDollar = P(CharsWhile(_ != '$').!).map(Literal)
  val dollarExpression: P[TemplateExpr] = P("$" ~~ (dollar | construct | variable | evalExpression))
  val mainText: P[Sequence] = P(dollarExpression | untilDollar).repX.map(Sequence)

  val mainDoc: P[Sequence] = P(Start ~~ mainText ~~ End)
}


class Template(templateText: String, val instrument: Boolean = false) {
  //import StringTemplate.White._
  var totalOps = 0
  private val instrumentFunction = (parser: fastparse.core.Parser[_,Char,String], index: Int, continuation: () => fastparse.core.Parsed[_, Char,String]) => {
    totalOps += 1
    println(f"Call: ${parser.toString}%-20s $index%3d: ${templateText.drop(index).take(20).replaceAll("\n", "\\\\n")}")
  }

  def error = tree.left.toOption

  val tree: Either[String, TemplateExpr] = {
    val start = System.currentTimeMillis
    Template.mainDoc.parse(templateText, instrument = if (instrument) instrumentFunction else null) match {
      case Parsed.Success(output, _) =>
        val parseTime = System.currentTimeMillis - start
        if (instrument) {
          println(s"ParseTime: $parseTime")
          println(output)
        }
        Right(output)

      case err@Parsed.Failure(last, index, extra) =>
      if (instrument) System.err.println(extra.traced.stack.mkString("\n"))
      val currentContext = templateText.drop(index).take(20).replaceAll("\n", "\\\\n")
      Left(s"Error failed expecting $last at pos $index ${textPos(templateText,index)}: $currentContext...")
    }
  }

  if (instrument) println(s"Total ops: $totalOps")

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

      case ForLoop(index, array, expr) =>
        val prim = evalValue(array, context)
        prim.toArray.value.foreach{ item =>
          render(expr, context.copy(values = MapValue(context.values.value + (index -> item))), sb)
        }

      case IfThenElse(pred, thenExpr, elseExpr) =>
        val predValue = evalValue(pred, context).toBoolean
        val expr = if (predValue == BooleanValue.trueV) thenExpr else elseExpr
        render(expr, context, sb)
    }
  }

  def evalValue(value: Value, context: Context): TemplateValue = {
    value match  {
      case x: StringValue => x
      case x: IntValue => x
      case x: BooleanValue => x

      case Variable(path) =>
        try {
          path.foldLeft[TemplateValue](context.values){ (ctx, key) =>
            ctx.toMap(key)
          }
        }
        catch {
          case _: NoSuchElementException =>
            throw new NoSuchElementException(s"Key not found ${path.mkString(".")}")
        }

      case Function(name, params) =>
        val paramValues = params.map(p => evalValue(p, context))
        val functionSpec = context.functions(name)
        if (paramValues.size != functionSpec.numParams)
          throw new Exception(s"Function $name(...) has ${functionSpec.numParams} parameters but ${paramValues.size} passed")
        functionSpec.function(paramValues)

      case cond: ConditionalExpr =>
        val a = evalValue(cond.a, context)
        val b = evalValue(cond.b, context)
        if (a.getClass != b.getClass) throw new Exception(s"Cannot compare $a and $b of different types")
        val res = BooleanValue(cond match {
          case _: Equals =>           a == b
          case _: NotEqual =>         a != b
          case _: GreaterThanEqual => a >= b
          case _: GreaterThan =>      a >  b
          case _: LessThanEqual =>    a <= b
          case _: LessThan =>         a <  b
          case _: And =>              a.toBoolean.value && b.toBoolean.value
          case _: Or =>               a.toBoolean.value || b.toBoolean.value
        })
        res

      case Negate(value) =>
        val v = evalValue(value, context).toBoolean
        BooleanValue(!v.value)

      case op: Add =>
        val a = evalValue(op.a, context)
        val b = evalValue(op.b, context)
        IntValue(a.toInt + b.toInt)

      case op: Subtract =>
        val a = evalValue(op.a, context)
        val b = evalValue(op.b, context)
        IntValue(a.toInt - b.toInt)

      case op: Multiply =>
        val a = evalValue(op.a, context)
        val b = evalValue(op.b, context)
        IntValue(a.toInt * b.toInt)

      case op: Divide =>
        val a = evalValue(op.a, context)
        val b = evalValue(op.b, context)
        IntValue(a.toInt / b.toInt)

      case op: Modulus =>
        val a = evalValue(op.a, context)
        val b = evalValue(op.b, context)
        IntValue(a.toInt % b.toInt)
    }
  }
}

import TemplateBuilder._

object TemplateApp {
  val num = 2
  val fileName = s"test$num.tmpl"

  val context = Context()
    .withValues(
      "dollars" -> 1000,
      "age" -> 21,
      "this" -> "THIS",
      "that" -> "THAT",
    )


  val functions = Map(
    "lowerCase" ->      function(_.toStr.toLowerCase),
    "upperCase" ->      function(_.toStr.toUpperCase),
    "currencyCommas" -> function(_.toStr.reverse.grouped(3).mkString(",").reverse),
    "repeat" ->         function((s,m) => s.toStr * m.toInt),
    "range" ->          function((s,e) => Range(s.toInt, e.toInt).toSeq)
  )

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
      if (false) {
        var c = 0L
        val s = System.currentTimeMillis
        for(x <- 0L to 1000000L*4) {
          tmpl.render(context)
          c+=1
        }
        val total = System.currentTimeMillis - s
        println(c, total.toDouble/1000)
      }
      else {
        val secs = 60
        val start = System.currentTimeMillis

        val loopSize = 400000
        val maxMillis = secs * 1000L
        var count = 0
        while((System.currentTimeMillis - start) < maxMillis) {
          val s = System.currentTimeMillis
          for(i <- 0 until loopSize) {
            val result = tmpl.render(context)
            count += 1
            //println(result)
          }
          val d = System.currentTimeMillis-s
          println(d)

        }
        val total = System.currentTimeMillis - start
        val perSecond = count.toDouble / (total.toDouble / 1000.0)
        println(s"count=$count, $perSecond p/s, total: $total")
    }
  }
}
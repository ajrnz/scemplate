package ajr.scemplate

import java.util.NoSuchElementException


private object TemplateParser {
  val White = fastparse.WhitespaceApi.Wrapper{
    import fastparse.all._
    val wsOpt = P(" " | "\t" | "\n").rep
    NoTrace(wsOpt)
  }

  import fastparse.noApi._
  import White._

  val reserved = Set("endfor", "endif", "endmacro", "else", "true", "false")

  val ws = P(" " | "\t" | "\n").rep(min=1)
  val ccx: P[Unit] = "}" ~~ "\n".?
  val identStart = CharPred(x => x.isLetter || x == '_')
  val identChar = CharsWhile(x => x.isLetterOrDigit || x == '_')
  val ident = P((identStart ~~ identChar.?).!).filter(!reserved(_))
  val dollar = P("$").map(_ => Literal("$"))

  val string = P("\"" ~~ CharsWhile(_ != '"').! ~~ "\"").map(StringValue(_)) // XXX no way of escaping quotes
  val digit = P(CharPred(_.isDigit))
  val integer = P(("+" | "-").? ~~ digit.repX(min=1)).!.map(v => IntValue(v.toInt))
  val double = P(("+" | "-").? ~~ digit.repX(min=1) ~~ "." ~~ digit.repX(min=1)).!.map(v => DoubleValue(v.toDouble))
  val boolean = P(("true" | "false") ~ !identChar).!.map(v => BooleanValue(v == "true"))

  val literal = P(double | integer | string | boolean)
  val variable: P[Value] = P(ident ~ ("." ~ ident).rep).map(x => Variable(x._1 +: x._2))
  val value: P[Value] = P(literal | variable)
  val function = P((ident ~~ "(" ~ expression.rep(sep = ",") ~ ")").map(Function.tupled))

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
  def cmd(cmdName: String): P[Unit] = P("${" ~ cmdName ~ ccx).opaque(s"$cmdName")
  val forLoop = P(("{" ~ "for" ~~ ws ~/ ident ~ "in" ~ expression ~ ccx ~~ mainText ~ cmd("endfor")).map(x => ForLoop(x._1, x._2, x._3)))
  val ifThenElse = P(("{" ~ "if" ~~ ws ~/ conditional ~ ccx ~/ mainText ~ (cmd("else") ~/ mainText).? ~ cmd("endif"))
    .map(x => IfThenElse(x._1, x._2, x._3.getOrElse(EmptyLiteral))))
  val macroTemplate = P(("{" ~ "macro" ~~ ws ~ ident ~ "(" ~ ident.rep(sep=",") ~ ")" ~ ccx ~/ mainText ~ cmd("endmacro")))
    .map(x=> MacroDef(x._1, x._2, x._3))
  val construct: P[TemplateExpr] = P(forLoop | ifThenElse | macroTemplate)
  val untilDollar = P(CharsWhile(_ != '$').!).map(Literal)
  val dollarExpression: P[TemplateExpr] = P("$" ~~ (dollar | construct | variable | evalExpression))
  val mainText: P[Sequence] = P(dollarExpression | untilDollar).repX.map(Sequence)

  val mainDoc: P[Sequence] = P(Start ~~ mainText ~~ End)
}



class Template(templateText: String, instrument: Boolean = false) {
  import fastparse.core.Parsed
  import fastparse.core.Parser

  private var totalOps = 0
  private val instrumentFunction = (parser: Parser[_,Char,String], index: Int, continuation: () => Parsed[_, Char,String]) => {
    totalOps += 1
    println(f"Call: ${parser.toString}%-20s $index%3d: ${templateText.drop(index).take(20).replaceAll("\n", "\\\\n")}")
  }

  def error = tree.left.toOption

  private val tree: Either[String, TemplateExpr] = {
    val start = System.currentTimeMillis
    TemplateParser.mainDoc.parse(templateText, instrument = if (instrument) instrumentFunction else null) match {
      case Parsed.Success(output, _) =>
        val parseTime = System.currentTimeMillis - start
        if (instrument) {
          println(s"ParseTime: $parseTime")
          println(s"Total ops: $totalOps")
          println(output)
        }
        Right(output)

      case err@Parsed.Failure(last, index, extra) =>
      if (instrument) System.err.println(extra.traced.stack.mkString("\n"))
      val currentContext = templateText.drop(index).take(20).replaceAll("\n", "\\\\n")
      Left(s"Error failed expecting $last at pos $index ${textPos(templateText,index)}: $currentContext...")
    }
  }

  private def textPos(text: String, index: Int) = {
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

  private def render(template: TemplateExpr, context: Context, sb: StringBuilder): Context = {
    template match {
      case MacroDef(name, args, body) =>
        context.withFunctions(name -> FunctionSpec(args.size, {
          argValExprs =>
            val argVals = argValExprs.map(argValExpr => evalValue(argValExpr, context))
            val newContext = context.withValues(args.zip(argVals): _*)
            val tsb = new StringBuilder
            render(body, newContext, tsb)
            StringValue(tsb.result())
        }))

      case Sequence(items) =>
        items.foldLeft(context)((ctx, item) => render(item, ctx, sb))
        context

      case Literal(str) =>
        sb ++= str
        context

      case value: Value =>
        sb ++= evalValue(value, context).toStr
        context

      case ForLoop(index, array, expr) =>
        val prim = evalValue(array, context)
        prim.toArray.value.foreach{ item =>
          render(expr, context.withValues(index -> item), sb)
        }
        context

      case IfThenElse(pred, thenExpr, elseExpr) =>
        val predValue = evalValue(pred, context).toBoolean
        val expr = if (predValue == BooleanValue.trueV) thenExpr else elseExpr
        render(expr, context, sb)
        context
    }
  }

  private def evalValue(value: Value, context: Context): TemplateValue = {
    value match  {
      case x: StringValue => x
      case x: IntValue => x
      case x: DoubleValue => x
      case x: BooleanValue => x
      case x: ArrayValue => x
      case x: MapValue => x

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
        val functionSpec = context.functions.get(name) match {
          case Some(func) => func
          case None => throw new Exception(s"Unknown function: $name")
        }
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
        (a,b) match {
          case (_: IntValue, _: IntValue) =>
            IntValue(a.toInt + b.toInt)
          case _ =>
            DoubleValue(a.toDouble + b.toDouble)
        }

      case op: Subtract =>
        val a = evalValue(op.a, context)
        val b = evalValue(op.b, context)
        (a,b)  match {
          case (_: IntValue, _: IntValue) =>
            IntValue(a.toInt - b.toInt)
          case _ =>
            DoubleValue(a.toDouble - b.toDouble)
        }

      case op: Multiply =>
        val a = evalValue(op.a, context)
        val b = evalValue(op.b, context)
        (a,b)  match {
          case (_: IntValue, _: IntValue) =>
            IntValue(a.toInt * b.toInt)
          case _ =>
            DoubleValue(a.toDouble * b.toDouble)
        }

      case op: Divide =>
        val a = evalValue(op.a, context)
        val b = evalValue(op.b, context)
        (a,b)  match {
          case (_: IntValue, _: IntValue) =>
            IntValue(a.toInt / b.toInt)
          case _ =>
            DoubleValue(a.toDouble / b.toDouble)
        }

      case op: Modulus =>
        val a = evalValue(op.a, context)
        val b = evalValue(op.b, context)
        (a,b)  match {
          case (_: IntValue, _: IntValue) =>
            IntValue(a.toInt % b.toInt)
          case _ =>
            DoubleValue(a.toDouble % b.toDouble)
        }
    }
  }
}

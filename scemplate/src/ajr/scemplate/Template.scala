package ajr.scemplate


private object TemplateParser {
  val White = fastparse.WhitespaceApi.Wrapper{
    import fastparse.all._
    val wsRep = P(CharIn(" \t\r\n").rep)
    NoTrace(wsRep)
  }

  import fastparse.noApi._
  import White._

  val reserved = Set("endfor", "endif", "endmacro", "else", "true", "false")

  val ws            = P(CharIn(" \t\r\n"))
  val ccx           = "}" ~~ "\n".?
  val identStart    = CharPred(x => x.isLetter || x == '_')
  val identChar     = CharsWhile(x => x.isLetterOrDigit || x == '_')
  val ident         = P((identStart ~~ identChar.?).!).filter(!reserved(_)).opaque("identifier")
  val dollar        = P("$").map(_ => Literal("$"))

  val hexDigit      = P(CharIn('0'to'9', 'a'to'f', 'A'to'F'))
  val unicodeEscape = P("u" ~~ (hexDigit ~~ hexDigit ~~ hexDigit ~~ hexDigit).!)
    .map(h => Character.valueOf(Integer.parseInt(h, 16).toChar).toString)
  val charEscape    = P(CharIn("tbnrf'\"\\").!.map(c => "\t\b\n\r\f\'\"\\"("tbnrf'\"\\".indexOf(c(0))).toString))
  val escape        = P( "\\" ~~ (charEscape | unicodeEscape) )
  val strChars      = P(CharsWhile(c => c != '"' && c != '\\').!)
  val string        = P("\"" ~~ (strChars | escape).repX ~~ "\"").map(x => StringValue(x.mkString))

  val digit         = P(CharPred(_.isDigit))
  val integer       = P(("+" | "-").? ~~ digit.repX(min=1)).!.map(v => IntValue(v.toInt))
  val double        = P(("+" | "-").? ~~ digit.repX(min=1) ~~ "." ~~ digit.repX(min=1)).!.map(v => DoubleValue(v.toDouble))
  val boolean       = P(StringIn("true", "false") ~ !identChar).!.map(v => BooleanValue(v == "true"))

  val literal       = P(double | integer | string | boolean)
  val variable      = P(ident.repX(min=1, sep=".")).map(x => Variable(x))
  val value         = P(literal | variable)
  val function      = P((ident ~ "(" ~ expression.rep(sep = ",") ~ ")").map(Function.tupled))

  val brackets      = P("(" ~/ expression ~ ")")

  val valueType: P[Value] = P("!".!.? ~ (function | brackets | value)).map{x=> x._1 match {
    case Some(_) => Negate(x._2)
    case None    => x._2
  }}

  val multiDivMod: P[Value] = P(valueType ~ (("*" | "/" | "%").! ~/ valueType).rep).map{x =>
    x._2.foldLeft(x._1){case(c, (op, value)) => op match {
      case "*" => Multiply(c, value)
      case "/" => Divide(c, value)
      case "%" => Modulus(c, value)
    }}
  }
  val addSub: P[Value] = P(multiDivMod ~ (("+"|"-").! ~/ multiDivMod).rep).map{x =>
    x._2.foldLeft(x._1){case(c, (op, value)) =>
      if (op == "+") Add(c, value) else Subtract(c, value)
    }
  }
  val andOr: P[Value] = P(addSub ~ (("&&"|"||").! ~/ addSub).rep).map{x =>
    x._2.foldLeft(x._1){case(c, (op, value)) =>
      if (op == "&&") And(c, value) else Or(c, value)
    }
  }
  val conditional: P[Value] = P(andOr ~ (("==" | "!=" |  ">=" | ">" | "<=" | "<").! ~/ andOr).rep).map{x =>
    x._2.foldLeft(x._1){case(c, (op, value)) =>
      ConditionalExpr(c, op, value)
    }
  }

  val expression:     P[Value] = P(conditional)
  val evalExpression: P[TemplateExpr] = P("{" ~ expression ~ "}")
  def cmd(cmdName: String): P[Unit] = P("${" ~ cmdName ~ ccx).opaque(s"$cmdName")
  val forLoop         = P(("{" ~ "for" ~~ ws ~/ ident ~ "in" ~ expression ~ ccx ~~ mainText ~ cmd("endfor")).map(x => ForLoop(x._1, x._2, x._3)))
  val ifThenElse      = P(("{" ~ "if" ~~ ws ~/ conditional ~ ccx ~/ mainText ~ (cmd("else") ~/ mainText).? ~ cmd("endif"))
    .map(x => IfThenElse(x._1, x._2, x._3.getOrElse(EmptyLiteral))))
  val macroTemplate   = P(("{" ~ "macro" ~~ ws ~/ ident ~ "(" ~ ident.rep(sep=",") ~ ")" ~ ccx ~/ mainText ~ cmd("endmacro")))
    .map(x=> MacroDef(x._1, x._2, x._3))
  val construct: P[TemplateExpr] = P(forLoop | ifThenElse | macroTemplate)
  val untilDollar     = P(CharsWhile(_ != '$').!).map(Literal)
  val dollarExpression: P[TemplateExpr] = P("$" ~~ (dollar | construct | variable | evalExpression))
  val mainText: P[Sequence] = P(dollarExpression | untilDollar).repX.map(Sequence)

  val mainDoc: P[Sequence] = P(Start ~~ mainText ~~ End)
}


class Template(templateText: String) {
  import fastparse.core.Parsed
  import fastparse.core.Parser

  protected val instrumentLevel: Int = 0
  protected var totalOps = 0

  private val instrumentFunction = (parser: Parser[_,Char,String], index: Int, continuation: () => Parsed[_, Char,String]) => {
    totalOps += 1
    if (instrumentLevel>1)
      println(f"Call: ${parser.toString}%-20s $index%3d: ${templateText.drop(index).take(20).replaceAll("\n", "\\\\n")}")
  }

  def error = tree.left.toOption

  private val tree: Either[String, TemplateExpr] = {
    val start = System.currentTimeMillis
    TemplateParser.mainDoc.parse(templateText, instrument = if (instrumentLevel>0) instrumentFunction else null) match {
      case Parsed.Success(output, _) =>
        val parseTime = System.currentTimeMillis - start
        if (instrumentLevel>1) {
          println(s"ParseTime: $parseTime")
          println(s"Total ops: $totalOps")
          println(output)
        }
        Right(output)

      case err@Parsed.Failure(last, index, extra) =>
        if (instrumentLevel>1) System.err.println(extra.traced.stack.mkString("\n"))
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
        throw new TemplateException(s"Cannot render invalid template: $err")
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
        path.foldLeft[Option[TemplateValue]](Some(context.values)){ (ctx, key) =>
          ctx.flatMap(_.toMap.value.get(key))
        }
        .getOrElse(throw new BadNameException(s"Key not found ${path.mkString(".")}"))

      case Function(name, params) =>
        val paramValues = params.map(p => evalValue(p, context))
        val functionSpec = context.functions.get(name) match {
          case Some(func) => func
          case None => throw new BadNameException(s"Unknown function: $name")
        }
        if (paramValues.size != functionSpec.numParams)
          throw new BadTypeException(s"Function $name(...) has ${functionSpec.numParams} parameters but ${paramValues.size} passed")
        functionSpec.function(paramValues)

      case cond: ConditionalExpr =>
        val a = evalValue(cond.a, context)
        val b = evalValue(cond.b, context)
        if (a.getClass != b.getClass) throw new BadTypeException(s"Cannot compare $a and $b of different types")
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

          case (_: StringValue, _: StringValue) =>
            StringValue(a.toStr + b.toStr)

          case (_: IntValue, _: IntValue) |
               (_: IntValue, _: DoubleValue) |
               (_: DoubleValue, _: IntValue) |
               (_: DoubleValue, _: DoubleValue) =>
            DoubleValue(a.toDouble + b.toDouble)

          case _ =>
            throw new BadNameException(s"Cannot add $a to $b")
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

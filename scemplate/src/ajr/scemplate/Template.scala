package ajr.scemplate

import fastparse._
import MultiLineWhitespace._
import fastparse.internal.Instrument


private object TemplateParser {
  val reserved = Set("endfor", "endif", "endmacro", "else", "true", "false")

  def ws           [$: P] = P(CharIn(" \t\r\n"))
  def newline      [$: P] = "\r\n" | "\n"
  def ccx          [$: P] = "}" ~~ newline.?
  def identStart   [$: P] = CharPred(x => x.isLetter || x == '_')
  def identChar    [$: P] = CharsWhile(x => x.isLetterOrDigit || x == '_')
  def ident        [$: P] = P((identStart ~~ identChar.?).!).filter(!reserved(_)).opaque("identifier")
  def dollar       [$: P] = P("$").map(_ => Literal("$"))

  def hexDigit     [$: P] = P(CharIn("0-9", "a-f", "A-F"))
  def unicodeEscape[$: P] = P("u" ~~ (hexDigit ~~ hexDigit ~~ hexDigit ~~ hexDigit).!)
    .map(h => Character.valueOf(Integer.parseInt(h, 16).toChar).toString)
  def charEscape   [$: P] = P(CharIn("tbnrf'\"\\\\").!.map(c => "\t\b\n\r\f\'\"\\"("tbnrf'\"\\".indexOf(c(0))).toString))
  def escape       [$: P] = P( "\\" ~~ (charEscape | unicodeEscape) )
  def strChars     [$: P] = P(CharsWhile(c => c != '"' && c != '\\').!)
  def string       [$: P] = P("\"" ~~ (strChars | escape).repX ~~ "\"").map(x => StringValue(x.mkString)) // XXX first ~~ should be ~~/

  def digit        [$: P] = P(CharPred(_.isDigit))
  def integer      [$: P] = P(("+" | "-").? ~~ digit.repX(1)).!.map(v => IntValue(v.toInt))
  def double       [$: P] = P(("+" | "-").? ~~ digit.repX(1) ~~ "." ~~ digit.repX(1)).!.map(v => DoubleValue(v.toDouble))
  def boolean      [$: P] = P(StringIn("true", "false") ~ !identChar).!.map(v => BooleanValue(v == "true"))
  def literal      [$: P] = P(double | integer | string | boolean)
  def variable     [$: P] = P(ident).map(x => Variable(Seq(x)))
  def variablePath [$: P] = P(ident.repX(min=1, sep=".")).map(Variable)
  def value        [$: P] = P(literal | variablePath)
  def defined      [$: P] = P(("defined" ~ "(" ~ variablePath ~ ")").map(Defined))
  def function     [$: P] = P((ident ~ "(" ~ expression.rep(sep = ",") ~ ")").map(Function.tupled))
  def brackets     [$: P] = P("(" ~/ expression ~ ")")

  def valueType[$: P]: P[Value] = P("!".!.? ~ (defined | function | brackets | value)).map{x=> x._1 match {
    case Some(_) => Negate(x._2)
    case None    => x._2
  }}

  def multiDivMod[$: P]: P[Value] = P(valueType ~ (("*" | "/" | "%").! ~/ valueType).rep).map{x =>
    x._2.foldLeft(x._1){case(c, (op, value)) => op match {
      case "*" => Multiply(c, value)
      case "/" => Divide(c, value)
      case "%" => Modulus(c, value)
    }}
  }
  def addSub[$: P]: P[Value] = P(multiDivMod ~ (("+"|"-").! ~/ multiDivMod).rep).map{x =>
    x._2.foldLeft(x._1){case(c, (op, value)) =>
      if (op == "+") Add(c, value) else Subtract(c, value)
    }
  }
  def conditional[$: P]: P[Value] = P(addSub ~ (("==" | "!=" |  ">=" | ">" | "<=" | "<").! ~/ addSub).rep).map{x =>
    x._2.foldLeft(x._1){case(c, (op, value)) =>
      ConditionalExpr(c, op, value)
    }
  }
  def andOr[$: P]: P[Value] = P(conditional ~ (("&&"|"||").! ~/ conditional).rep).map{x =>
    x._2.foldLeft(x._1){case(c, (op, value)) =>
      if (op == "&&") And(c, value) else Or(c, value)
    }
  }

  def expression      [$: P]: P[Value] = P(andOr)
  def evalExpression  [$: P]: P[TemplateExpr] = P("{" ~ expression ~ "}")
  def cmd             [$: P](cmdName: String): P[Unit] = P("${" ~ cmdName ~ ccx).opaque(s"$cmdName")
  def forLoop         [$: P] = P(("{" ~ "for" ~~ ws ~/ ident ~ "in" ~ expression ~ ccx ~~ mainText ~~ cmd("endfor")).map(x => ForLoop(x._1, x._2, x._3)))
  def ifThenElse      [$: P] = P(("{" ~ "if" ~~ ws ~/ conditional ~ ccx ~~ mainText ~~ (cmd("else") ~/ mainText).? ~~ cmd("endif"))
    .map(x => IfThenElse(x._1, x._2, x._3.getOrElse(EmptyLiteral))))
  def macroTemplate   [$: P] = P(("{" ~ "macro" ~/ ident ~ "(" ~ ident.rep(sep=",") ~ ")" ~ ccx ~~ mainText ~~ cmd("endmacro")))
    .map(x=> MacroDef(x._1, x._2, x._3))
  def construct       [$: P]: P[TemplateExpr] = P(forLoop | ifThenElse | macroTemplate)
  def untilDollar     [$: P]     = P(CharsWhile(_ != '$').!).map(Literal)
  def dollarExpression[$: P]: P[TemplateExpr] = P("$" ~~ (dollar | construct | variable | evalExpression))
  def mainText        [$: P]: P[Sequence] = P(dollarExpression | untilDollar).repX.map(Sequence)

  def mainDoc         [$: P]: P[Sequence] = P(Start ~~ mainText ~~ End)
}



trait TemplateBase[X] {
  protected val instrumentLevel: Int = 0
  protected var totalOps = 0

  protected def templateParser: P[_] => P[X]
  val templateText: String

  private val instrument = new Instrument {
    def beforeParse(parser: String, index: Int): Unit = {
      totalOps += 1
      if (instrumentLevel>1)
        println(f"Call: ${parser.toString}%-20s $index%3d: ${templateText.drop(index).take(20).replaceAll("\n", "\\\\n")}")
    }
    def afterParse(parser: String, index: Int, success: Boolean): Unit = ()
  }

  def error = tree.left.toOption

  protected val tree: Either[String, X] = {
    val start = System.currentTimeMillis
    parse(templateText, templateParser, instrument = if (instrumentLevel>0) instrument else null) match {
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

  private def evalVariable(context: Context, path: Seq[String]) = {
    path.foldLeft[Either[String, TemplateValue]](Right(context.values)) { (ctx, key) =>
      ctx match {
        case l@Left(_) => l

        case Right(MapValue(m)) =>
          m.get(key).toRight(s"$key not found")

        case Right(x) =>
          Left(s"$key not found")
      }
    }
  }


  protected def evalValue(value: Value, context: Context): TemplateValue = {
    value match  {
      case x: StringValue => x
      case x: IntValue => x
      case x: DoubleValue => x
      case x: BooleanValue => x
      case x: ArrayValue => x
      case x: MapValue => x

      case Variable(path) =>
        evalVariable(context, path) match {
          case Right(x) => x
          case Left(msg) =>
            throw new BadNameException(msg + (if (path.size>1) s" in ${path.mkString(".")}" else ""))
        }

      case Defined(variable) =>
        BooleanValue(evalVariable(context, variable.name).isRight)

      case Function(name, params) =>
        val paramValues = params.map(p => evalValue(p, context))
        // first try it as a variable, than as a function
        context.values.value.get(name) match {
          case Some(value) =>
            if (paramValues.length != 1)
              throw new BadTypeException(s"$name - too many parameters")
            else {
              value match {
                case ArrayValue(items) =>
                  items(paramValues.head.asInt)

                case MapValue(mapValue) =>
                  mapValue(paramValues.head.asString)

                case _ =>
                  throw new BadNameException(s"Cannot call apply() on $name which has value $value")
              }
            }

          case None =>
            val functionSpec = context.functions.get(name) match {
              case Some(func) => func
              case None => throw new BadNameException(s"Unknown function: $name")
            }
            if (paramValues.size != functionSpec.numParams)
              throw new BadTypeException(s"Function $name(...) has ${functionSpec.numParams} parameters but ${paramValues.size} passed")
            functionSpec.function(paramValues)
        }
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
          case _: And =>              a.asBoolean && b.asBoolean
          case _: Or =>               a.asBoolean || b.asBoolean
        })
        res

      case Negate(value) =>
        BooleanValue(!evalValue(value, context).asBoolean)

      case op: Add =>
        val a = evalValue(op.a, context)
        val b = evalValue(op.b, context)
        (a,b) match {
          case (_: IntValue, _: IntValue) =>
            IntValue(a.asInt + b.asInt)

          case (_: StringValue, _: StringValue) =>
            StringValue(a.asString + b.asString)

          case (_: IntValue, _: IntValue) |
               (_: IntValue, _: DoubleValue) |
               (_: DoubleValue, _: IntValue) |
               (_: DoubleValue, _: DoubleValue) =>
            DoubleValue(a.asDouble + b.asDouble)

          case _ =>
            throw new BadNameException(s"Cannot add $a to $b")
        }

      case op: Subtract =>
        val a = evalValue(op.a, context)
        val b = evalValue(op.b, context)
        (a,b)  match {
          case (_: IntValue, _: IntValue) =>
            IntValue(a.asInt - b.asInt)
          case _ =>
            DoubleValue(a.asDouble - b.asDouble)
        }

      case op: Multiply =>
        val a = evalValue(op.a, context)
        val b = evalValue(op.b, context)
        (a,b)  match {
          case (_: IntValue, _: IntValue) =>
            IntValue(a.asInt * b.asInt)
          case _ =>
            DoubleValue(a.asDouble * b.asDouble)
        }

      case op: Divide =>
        val a = evalValue(op.a, context)
        val b = evalValue(op.b, context)
        (a,b)  match {
          case (_: IntValue, _: IntValue) =>
            IntValue(a.asInt / b.asInt)
          case _ =>
            DoubleValue(a.asDouble / b.asDouble)
        }

      case op: Modulus =>
        val a = evalValue(op.a, context)
        val b = evalValue(op.b, context)
        (a,b)  match {
          case (_: IntValue, _: IntValue) =>
            IntValue(a.asInt % b.asInt)
          case _ =>
            DoubleValue(a.asDouble % b.asDouble)
        }
    }
  }
}
class TemplateExpression(val templateText: String) extends TemplateBase[Value] {
  protected def templateParser = TemplateParser.expression(_)

  def eval(context: Context): TemplateValue =  {
    tree match {
      case Right(value) =>
        evalValue(value, context)

      case Left(err) =>
        throw new TemplateException(s"Cannot render invalid expression: $err")
    }
  }
}

object Template {
  def render(template: String)(implicit context: Context): String = new Template(template).render(context)
}

class Template(val templateText: String) extends TemplateBase[Sequence] {
  override protected def templateParser = TemplateParser.mainDoc(_)

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
        sb ++= evalValue(value, context).asString
        context

      case ForLoop(index, array, expr) =>
        val prim = evalValue(array, context)
        prim.asSeq.foreach{ item =>
          render(expr, context.withValues(index -> item), sb)
        }
        context

      case IfThenElse(pred, thenExpr, elseExpr) =>
        val predValue = evalValue(pred, context).asBoolean
        val expr = if (predValue) thenExpr else elseExpr
        render(expr, context, sb)
        context
    }
  }

}


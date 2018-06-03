package ajr.test.scemplate

import java.time.LocalDate
import java.time.LocalDate.{parse => parseDate}
import java.time.format.DateTimeFormatter

import ajr.scemplate._
import ajr.scemplate.implicits._
import utest._

object TestExample extends TestHelper
{
  case class ReportInfo(title: String, page: Int, pages: Int, footer:String)
  case class Person(name: String, age: Int)

  implicit val dateToString = new Encode[LocalDate] {
    def encode(v: LocalDate) = v.format(DateTimeFormatter.ISO_DATE)
  }

  case class Transaction(date: LocalDate, description: String, value: Double)
  object Transaction {
    implicit def toTV(value: Transaction): TemplateValue = CaseClassEncoder.gen[Transaction].encode(value)
  }

  implicit val seqTran = new Encode[Seq[Transaction]] {
    def encode(v: Seq[Transaction]) = v.toArrayValue
  }

  case class Account(person: Person, balance: Double, active: Boolean, transactions: Seq[Transaction])
  object Account {
    implicit def toTV(value: Account): TemplateValue = CaseClassEncoder.gen[Account].encode(value)
  }


  val jack = Person("Jack", 37)
  val jill = Person("Jill", 32)

  val jackAcct = Account(jack, 1234.23, true, Seq(
    Transaction(parseDate("2018-05-22"), "Food", -110.22),
    Transaction(parseDate("2018-06-01"), "Rent", 1400),
    Transaction(parseDate("2018-07-19"), "Salary", 3200.00)
  ))

  val jillAcct = Account(jill, 0.0, false, Seq())
  val accounts = Seq(jackAcct, jillAcct)

  val length = function(v => v match {
    case v: StringValue => v.value.length
    case v: ArrayValue => v.value.size
    case v: MapValue => v.value.size
    case _ => throw new BadNameException(s"Cannot take length of $v")
  })

  val context = Context()
    .withValues(
      "title" -> "Bank Statement",
      "date" -> "17 March 2018",
      "page" -> 0,
      "branchId" -> 12008752,
      "accounts" -> accounts.toArrayValue
    )
    .withFunctions(
      "formatCurrency" -> function(v => java.text.NumberFormat.getInstance().format(v.toDouble)),
      "length" -> length
    )

  val templateText =
    """$title as at: $date  /  Branch: $branchId
      |Accounts
      |${for account in accounts}
      |
      |Name:    ${account.person.name} (${if account.active}Active${else}Inactive${endif})
      |Balance: ${formatCurrency(account.balance)}${if account.balance < 0.0} Overdrawn${endif}
      |
      |${if length(account.transactions) > 0}
      |Transactions
      |${for tran in account.transactions}
      |  ${tran.date} ${tran.description} ${formatCurrency(tran.value)}
      |${endfor}
      |${endif}
      |${endfor}
      |""".stripMargin

  val expected =
    """Bank Statement as at: 17 March 2018  /  Branch: 12008752
      |Accounts
      |
      |Name:    Jack (Active)
      |Balance: 1,234.23
      |Transactions
      |  2018-05-22 Food -110.22
      |  2018-06-01 Rent 1,400
      |  2018-07-19 Salary 3,200
      |
      |Name:    Jill (Inactive)
      |Balance: 0
      |""".stripMargin

  val tests = Tests {
    'example - {
      val template = new TemplateInst(templateText, 1)
      template.error ==> None
      val result = template.render(context)
      result ==> expected

      opDiff("Example", template.parseOps, 803)
    }

    'performance - {
      if (false) {
        val iterations = 100000
        val template = new Template(templateText)
        val start = System.currentTimeMillis
        var total = 0L
        for(i <- Range(0, iterations)) {
          val result = template.render(context)
          total += result.length
        }
        val time = System.currentTimeMillis - start
        val rate = iterations * 1000L / time
        println(s"Renders $iterations: $rate per second")
        assert(time < 2000)
      }
    }
  }
}

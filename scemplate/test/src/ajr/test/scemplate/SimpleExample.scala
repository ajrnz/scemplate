package ajr.test.scemplate

import java.text.NumberFormat
import java.time.LocalDate
import java.time.LocalDate.{parse => parseDate}
import java.time.format.DateTimeFormatter
import java.util.Locale

import ajr.scemplate._
import ajr.scemplate.implicits._
import utest._

object SimpleExample extends TestHelper
{
  case class Address(street: String, town: String, postCode: String)
  case class Person(name: String, age: Int, height: Double, email: String, address: Address)

  object Address {
    implicit def toTV(value: Address): TemplateValue = CaseClassEncoder.gen[Address].encode(value)
  }
  object Person {
    implicit def toTV(value: Person): TemplateValue = CaseClassEncoder.gen[Person].encode(value)
  }

  val address = Address("1 The Mall", "London", "SW1A 1AA")
  val person = Person("John Doe", 21, 1.76, "john@doe.com", address)

  implicit val context = Context()
    .withValues(
      "subject" -> "On offer this week...",
      "person" -> person,
      "cutoffAge" -> 30
    )
    .withFunctions(
      "firstName" -> function(n => n.asString.split(" ").head)
    )

  val templateText =
    """${person.address.street}
      |${person.address.town}
      |${person.address.postCode}
      |
      |Subject: $subject
      |
      |Dear ${firstName(person.name)},
      |
      |As a person ${if person.age>cutoffAge}over${else}under${endif} ${cutoffAge}....
      |""".stripMargin


  val expected =
    """1 The Mall
      |London
      |SW1A 1AA
      |
      |Subject: On offer this week...
      |
      |Dear John,
      |
      |As a person under 30....
      |""".stripMargin

  val tests = Tests {
    'example - {
      validate(templateText, expected)
      opDiff("Example", totalOps, 1111)
    }
  }
}

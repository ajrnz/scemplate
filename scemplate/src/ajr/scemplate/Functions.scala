package ajr.scemplate

import implicits._

object Functions {
  val stdlib = Seq(
    "len" -> function{ v =>
      v match {
        case v: StringValue => v.value.length
        case v: ArrayValue => v.value.length
        case v: MapValue => v.value.size
        case v => throw new Exception(s"len() not valid for $v")
      }
    },

    "keys" -> function{ v =>
      v match {
        case v: MapValue => v.value.keys.toSeq
        case v => throw new Exception(s"len() not valid for $v")
      }
    },
    "value" -> function{ (m,k) =>
      (m,k) match {
        case (v: MapValue, k: StringValue) => v.value(k.asString)
        case v => throw new Exception(s"value() not valid for $v")
      }
    },
    "replace" -> function{ (str, find, replace) => (str, find, replace) match {
      case (s: StringValue, f: StringValue, r: StringValue) => s.asString.replaceAll(f.asString, r.asString)
      case _ => throw new Exception(s"replace() not valid for ($str,$find,$replace)")
    }},
    "toLowerCase" -> function{ sv=> sv match {
      case (sv: StringValue) => sv.value.toLowerCase
      case _ => throw new Exception(s"toLowerCase() not valid for $sv")
    }},
    "toUpperCase" -> function{ sv=> sv match {
      case (sv: StringValue) => sv.value.toUpperCase
      case _ => throw new Exception(s"toUpperCase() not valid for $sv")
    }}  
  )
}

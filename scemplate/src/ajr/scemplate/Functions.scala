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
    }
  )
}

package ajr.scemplate

import language.experimental.macros, magnolia._

trait Show[T] { def encode(value: T): PrimitiveValue }

object CaseClassEncoder {
  type Typeclass[T] = Show[T]

  def combine[T](ctx: CaseClass[Show, T]): Show[T] = new Show[T] {
    println(s"combine: $ctx")
    def encode(value: T): PrimitiveValue = MapValue(ctx.parameters.map { p =>
      p.label -> p.typeclass.encode(p.dereference(value))
    }.toMap)
  }

  // required but not used
  def dispatch[T](ctx: SealedTrait[Show, T]): Show[T] = ???

  implicit def gen[T]: Show[T] = macro Magnolia.gen[T]
}

package ajr.scemplate

import language.experimental.macros, magnolia._

trait Encode[T] { def encode(value: T): TemplateValue }

object CaseClassEncoder {
  type Typeclass[T] = Encode[T]

  def combine[T](ctx: CaseClass[Encode, T]): Encode[T] = new Encode[T] {
    def encode(value: T): TemplateValue = MapValue(ctx.parameters.map { p =>
      p.label -> p.typeclass.encode(p.dereference(value))
    }.toMap)
  }

  // required but not used
  def dispatch[T](ctx: SealedTrait[Encode, T]): Encode[T] = ???

  implicit def gen[T]: Encode[T] = macro Magnolia.gen[T]
}

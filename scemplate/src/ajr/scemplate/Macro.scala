package ajr.scemplate

import language.experimental.macros, magnolia1._


trait Encode[T] {
  extension (x: T) def encode: TemplateValue
}

object Encode  extends AutoDerivation[Encode]:

  def join[T](ctx: CaseClass[Encode, T]): Encode[T] =  value =>
    MapValue(ctx.parameters.map { p =>
      p.label -> p.typeclass.encode(p.deref(value))
    }.toMap)


  override def split[T](ctx: SealedTrait[Encode, T]): Encode[T] = value =>
    ctx.choose(value) { sub => sub.typeclass.encode(sub.cast(value)) }


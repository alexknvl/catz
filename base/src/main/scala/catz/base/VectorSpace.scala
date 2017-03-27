package catz.base

abstract class OptionL[T <: Lifted] protected () extends Lifted {
  type Type = OptionL[T]
  type Value = Option[T#Value]
}
final class NoneL[T <: Lifted] extends OptionL[T]
final class SomeL[T <: Lifted] extends OptionL[T]

abstract class VectorSpace protected () extends TypeLevel {
  type Type = VectorSpace
  type Size <: OptionL[Nat]
}
object VectorSpace {
  final class D[V <: VectorSpace] extends VectorSpace
  final class :+:[V <: VectorSpace, U <: VectorSpace] extends VectorSpace
  final class :*:[V <: VectorSpace, U <: VectorSpace] extends VectorSpace

  type R[N <: Nat] = VectorSpace { type Size = SomeL[N] }
  type InfR = VectorSpace { type Size = NoneL[Nat] }
}

final case class Tensor[V <: VectorSpace]()
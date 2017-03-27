package catz.base

abstract class Fin protected () extends Lifted {
  type Type = Fin
  type Value = Int
  type Bound <: Nat
}
final class FZ[N <: Nat] extends Fin { type Bound = S[N] }
final class FS[F <: Fin] extends Fin { type Bound = S[F#Bound] }

object Fin {
  implicit def zValue[N <: Nat]: Value[FZ[N]] =
    Value[FZ[N]](0)
  implicit def sValue[N <: Fin](implicit N: Value[N]): Value[FS[N]] =
    Value[FS[N]](N.value + 1)
}
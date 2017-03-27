package catz.base

import catz.base.BasePackage.EqT

abstract class Nat protected () extends Lifted {
  type Value = Int
  type Type = Nat
}
final class Z extends Nat
final class S[N <: Nat] extends Nat

object Nat {
  final class +[N <: Nat, M <: Nat] extends Nat
  final class *[N <: Nat, M <: Nat] extends Nat

  object Evaluations {
    implicit val zValue: Value[Z] =
      Value[Z](0)
    implicit def sValue[N <: Nat](implicit N: Value[N]): Value[S[N]] =
      Value[S[N]](N.value + 1)
    implicit def plusValue[N <: Nat, M <: Nat](implicit N: Value[N], M: Value[M]): Value[N + M] =
      Value[N + M](N.value + M.value)
    implicit def multValue[N <: Nat, M <: Nat](implicit N: Value[N], M: Value[M]): Value[N * M] =
      Value[N * M](N.value * M.value)
  }

  object Reductions {
    implicit def case1r[M <: Nat]: Reduce.Aux[Z + M, M] =
      Reduce.byDef
    implicit def case2r[N <: Nat, M <: Nat]
    (implicit rec: Reduce[N + M]): Reduce.Aux[S[N] + M, S[rec.Result]] =
      Reduce.byDef
  }

  object Proofs {
    implicit def plusIsAssociative[A <: Nat, B <: Nat, C <: Nat]: EqT[Nat, A + (B + C), (A + B) + C] =
      EqT.unsafeForce[Nothing, Nat, A + (B + C), (A + B) + C]
    implicit def plusIsCommutative[A <: Nat, B <: Nat]: EqT[Nat, A + B, B + A] =
      EqT.unsafeForce[Nothing, Nat, A + B, B + A]
  }
}
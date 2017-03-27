package catz.bifunctor

import catz.base.BasePackage.Ξ
import catz.category.Category
import catz.base.Trivial

trait Bifunctor[P[_ <: Ξ, _ <: Ξ]] {
  type L0[_ <: Ξ]
  type L1[_ <: Ξ, _ <: Ξ]
  def L : Category.Aux[L1, L0]

  type R0[_ <: Ξ]
  type R1[_ <: Ξ, _ <: Ξ]
  def R : Category.Aux[R1, R0]

  type C0[_ <: Ξ]
  type C1[_ <: Ξ, _ <: Ξ]
  def C : Category.Aux[C1, C0]

  def bimap[LX <: Ξ, LY <: Ξ, RX <: Ξ, RY <: Ξ](left: L1[LX, LY], right: R1[RX, RY]): C1[P[LX, RX], P[LY, RY]]
}
object Bifunctor {
  trait Aux[P[_ <: Ξ, _ <: Ξ], L1_[_ <: Ξ, _ <: Ξ], L0_[_ <: Ξ], R1_[_ <: Ξ, _ <: Ξ], R0_[_ <: Ξ], C1_[_ <: Ξ, _ <: Ξ], C0_[_ <: Ξ]] extends Bifunctor[P] {
    type L0[A <: Ξ] = L0_[A]
    type L1[A <: Ξ, B <: Ξ] = L1_[A, B]

    type R0[A <: Ξ] = R0_[A]
    type R1[A <: Ξ, B <: Ξ] = R1_[A, B]

    type C0[A <: Ξ] = C0_[A]
    type C1[A <: Ξ, B <: Ξ] = C1_[A, B]
  }

  trait Aux1[P[_ <: Ξ, _ <: Ξ], C1[_ <: Ξ, _ <: Ξ], C0[_ <: Ξ]] extends Aux[P, C1, C0, C1, C0, C1, C0]
  trait Aux1T[P[_ <: Ξ, _ <: Ξ], C1[_ <: Ξ, _ <: Ξ]] extends Aux[P, C1, Trivial, C1, Trivial, C1, Trivial]
}

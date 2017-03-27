package catz.category

import catz.base.BasePackage._
import catz.bifunctor.Bifunctor

trait Associative[->[_ <: Ξ, _ <: Ξ], F[_ <: Ξ, _ <: Ξ]] {
  type C0[_ <: Ξ]
  type C1[A <: Ξ, B <: Ξ] = A -> B
  def C: Category.Aux[C1, C0]
  def bifunctor: Bifunctor.Aux1[F, C1, C0]

  def associate[X, Y, Z]: F[F[X, Y], Z] -> F[X, F[Y, Z]]
  def diassociate[X, Y, Z]: F[X, F[Y, Z]] -> F[F[X, Y], Z]
}
object Associative {
  trait Aux[C1_[_ <: Ξ, _ <: Ξ], C0_[_ <: Ξ], F[_ <: Ξ, _ <: Ξ]] extends Associative[C1_, F] {
    type C0[A <: Ξ] = C0_[A]
  }
}
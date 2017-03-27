package catz.base

import catz.base.BasePackage._

abstract class FunctionK[F[_ <: Ξ], G[_ <: Ξ]] extends Serializable { fg =>
  def apply[A <: Ξ](fa: F[A]): G[A]

  def andThen[H[_ <: Ξ]](gh: G ~> H): F ~> H = new (F ~> H) {
    override def apply[A <: Ξ](fa: F[A]): H[A] = gh.apply(fg.apply(fa))
  }

  def lower[A <: Ξ]: F[A] => G[A] = this.apply[A]

//  def or[H[_ <: Ξ]](hg: FunctionK[H, G]): λ[`α` => Coproduct[F, H, α]] ~> G =
//    new (λ[`α` => Coproduct[F, H, α]] ~> G) {
//      override def apply[A <: Ξ](fa: Coproduct[F, H, A]): G[A] =
//        fa.fold(fg.apply[A], hg.apply[A])
//    }
//
//  def and[H[_ <: Ξ]](fh: FunctionK[F, H]): F ~> λ[`α` => Product[G, H, α]] =
//    new (F ~> λ[`α` => Product[G, H, α]]) {
//      override def apply[A <: Ξ](fa: F[A]): Product[G, H, A] =
//        Product(fg.apply(fa), fh.apply(fa))
//    }
}
object FunctionK {
  def id[F[_ <: Ξ]]: FunctionK[F, F] = new FunctionK[F, F] {
    override def apply[A <: Ξ](fa: F[A]): F[A] = fa
  }
  def const[K](k: K): ∀[λ[`α[_ <: Ξ]` => α ~> λ[`β <: Ξ` => K]]] = new ∀[λ[`α[_ <: Ξ]` => α ~> λ[`β <: Ξ` => K]]] {
    override def apply[A[_ <: Ξ]]: A ~> λ[`β <: Ξ` => K] = new (A ~> λ[`β <: Ξ` => K]) {
      override def apply[B <: Ξ](fa: A[B]): K = k
    }
  }
}
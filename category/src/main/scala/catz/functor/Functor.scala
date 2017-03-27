package catz.functor

import catz.base.BasePackage._
import catz.category.Category
import catz.base.Trivial

trait Functor[F[_ <: Ξ] <: Ξ] {
  type C0[_ <: Ξ]
  type C1[_ <: Ξ, _ <: Ξ]
  def C : Category.Aux[C1, C0]

  type D0[_ <: Ξ]
  type D1[_ <: Ξ, _ <: Ξ]
  def D : Category.Aux[D1, D0]

  def fmap[A <: Ξ, B <: Ξ](f: C1[A, B]): D1[F[A], F[B]]
}
object Functor {
  trait Aux[F[_ <: Ξ] <: Ξ, C1_[_ <: Ξ, _ <: Ξ], C0_[_ <: Ξ], D1_[_ <: Ξ, _ <: Ξ], D0_[_ <: Ξ]] extends Functor[F] {
    type C0[A <: Ξ] = C0_[A]
    type C1[A <: Ξ, B <: Ξ] = C1_[A, B]
    type D0[A <: Ξ] = D0_[A]
    type D1[A <: Ξ, B <: Ξ] = D1_[A, B]
  }
  trait Endo[F[_ <: Ξ], C1[_ <: Ξ, _ <: Ξ], C0[_ <: Ξ]] extends Aux[F, C1, C0, C1, C0] {
    def D : Category.Aux[D1, D0] = C
  }
  trait EndoT[F[_ <: Ξ], C1[_ <: Ξ, _ <: Ξ]] extends Endo[F, C1, Trivial]

  trait EndoTF[F[_]] extends Endo[F, Function1, Trivial] {
    def C: Category.Aux[C1, C0] = Category.catzBaseFunction1Category

    def map[A, B](fa: F[A])(f: A => B): F[B] = fmap[A, B](f).apply(fa)
  }

  implicit val catzBaseOptionEndofunctor: EndoTF[Option] = new EndoTF[Option] {
    def fmap[A, B](f: A => B): Option[A] => Option[B] = _.map(f)
  }
}
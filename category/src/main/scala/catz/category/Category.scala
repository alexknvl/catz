package catz.category

import catz.base.BasePackage._
import catz.category.data.{Bicat, Dual, Phantom}
import catz.base.{Liskov, FunctionK, Trivial}

trait Category[->[_ <: Ξ, _ <: Ξ]] extends Semicategory[->] {
  type C0[A <: Ξ]
  def id[A <: Ξ](implicit A: C0[A]): A -> A
}
object Category {
  trait Aux[C1[_ <: Ξ, _ <: Ξ], C0_[_ <: Ξ]] extends Category[C1] { type C0[A <: Ξ] = C0_[A] }
  trait AuxT[C1[_ <: Ξ, _ <: Ξ]] extends Aux[C1, Trivial]

  implicit val catzBaseFunction1Category: Category.AuxT[Function1] = new Category.AuxT[Function1] {
    override def id[A](implicit A: C0[A]): A => A =
      identity[A]
    override def andThen[A, B, C](ab: A => B, bc: B => C): A => C =
      ab andThen bc
  }

  implicit val catzBaseFunctionKCategory: Category.AuxT[FunctionK] = new Category.AuxT[~>] {
    override def id[A[_ <: Ξ]](implicit A: C0[A]): A ~> A =
      FunctionK.id[A]
    override def andThen[A[_ <: Ξ], B[_ <: Ξ], C[_ <: Ξ]](ab: A ~> B, bc: B ~> C): A ~> C =
      ab andThen bc
  }

  implicit val catzBaseLiskovCategory: Category.AuxT[<~<] = new Category.AuxT[<~<] {
    override def id[A <: Ξ](implicit A: C0[A]): A <~< A =
      Liskov.id[A]
    override def andThen[A <: Ξ, B <: Ξ, C <: Ξ](ab: A <~< B, bc: B <~< C): A <~< C =
      ab andThen bc
  }

  implicit val catzCategoryPhantomCategory: Category.AuxT[Phantom] = new Category.AuxT[Phantom] {
    override def id[A <: Ξ](implicit A: C0[A]): A Phantom A =
      Phantom[A, A]()
    override def andThen[A <: Ξ, B <: Ξ, C <: Ξ](ab: A Phantom B, bc: B Phantom C): A Phantom C =
      Phantom[A, C]()
  }

  implicit def catzCategoryDualCategory[F[_ <: Ξ, _ <: Ξ], T[_ <: Ξ]](implicit C: Category.Aux[F, T]): Category.Aux[λ[(`α <: Ξ`, `β <: Ξ`) => Dual[F, α, β]], T] =
    new Category.Aux[λ[(`α <: Ξ`, `β <: Ξ`) => Dual[F, α, β]], T] {
      override def id[A <: Ξ](implicit A: T[A]): Dual[F, A, A] =
        Dual(C.id[A](A))
      override def andThen[A <: Ξ, B <: Ξ, C <: Ξ](ab: Dual[F, A, B], bc: Dual[F, B, C]): Dual[F, A, C] =
        Dual(C.andThen(bc.runDual, ab.runDual))
    }

  implicit def catzCategoryBicatCategory[F[_ <: Ξ, _ <: Ξ], T[_ <: Ξ]](implicit C: Category.Aux[F, T]): Category.Aux[λ[(`α <: Ξ`, `β <: Ξ`) => Bicat[F, α, β]], T] =
    new Category.Aux[λ[(`α <: Ξ`, `β <: Ξ`) => Bicat[F, α, β]], T] {
      override def id[A <: Ξ](implicit A: T[A]): Bicat[F, A, A] =
        Bicat(C.id[A](A), C.id[A](A))
      override def andThen[A <: Ξ, B <: Ξ, C <: Ξ](ab: Bicat[F, A, B], bc: Bicat[F, B, C]): Bicat[F, A, C] =
        Bicat(C.andThen(ab.forward, bc.forward), C.andThen(bc.backward, ab.backward))
    }
}
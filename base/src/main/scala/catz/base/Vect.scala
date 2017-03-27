package catz.base

import catz.base.BasePackage._

sealed trait Vect[N <: Nat, A] {
  type Size = N
}
object Vect {
  final case class Nil[A]() extends Vect[Z, A]
  final case class ::[N <: Nat, A](head: A, tail: Vect[N, A]) extends Vect[S[N], A]
}

sealed trait HVect {
  type Size <: Nat
}
object HVect {
  final class Nil extends HVect { type Size = Z }
  final class ::[Head <: Ξ, Tail <: HVect] extends HVect { type Size = S[Tail#Size] }

  final case class Index[L <: HVect, H <: Ξ](index: Int) extends AnyVal
  trait Index1 {
    implicit def case1[H <: Ξ, T <: HVect, X <: Ξ](implicit T: Index[T, X]): Index[H :: T, X] = Index(T.index)
  }
  object Index extends Index1 {
    implicit def case0[T <: HVect, X <: Ξ](implicit S: Value[T#Size]): Index[X :: T, X] = Index(S.value)
  }

  final case class IndexK[L <: HVect, H[_ <: Ξ]](index: Int) extends AnyVal
  trait IndexK1 {
    implicit def case1[H[_ <: Ξ], T <: HVect, X[_ <: Ξ]](implicit T: IndexK[T, X]): IndexK[H :: T, X] = IndexK(T.index)
  }
  object IndexK extends IndexK1 {
    implicit def case0[T <: HVect, X[_ <: Ξ]](implicit S: Value[T#Size]): IndexK[X :: T, X] = IndexK(S.value)
  }
}
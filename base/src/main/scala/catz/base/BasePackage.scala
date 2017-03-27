package catz.base

object BasePackage {
  type Ξ = scala.AnyKind

  type EqT[U, A <: U, B <: U] = BoundedLeibniz[Nothing, U, A, B]
  val EqT = BoundedLeibniz

  type ∀[F[_ <: Ξ]] = Forall[F]
  type ∃[F[_ <: Ξ]] = Exists[F]

  type ~>[A[_ <: Ξ], B[_ <: Ξ]] = FunctionK[A, B]

  type ===[A <: Ξ, B <: Ξ] = LeibnizK[A, B]
  type <~<[-A <: Ξ, +B <: Ξ] = Liskov[A, B]
  type >~>[+A <: Ξ, -B <: Ξ] = Liskov[B, A]
}

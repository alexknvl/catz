package catz.category.data

import catz.base.BasePackage._

final case class Bicat[F[_ <: Ξ, _ <: Ξ], A <: Ξ, B <: Ξ](forward: F[A, B], backward: F[B, A])
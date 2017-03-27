package catz.category.data

import catz.base.BasePackage._

final case class Dual[F[_ <: Ξ, _ <: Ξ], A <: Ξ, B <: Ξ](runDual: F[B, A])
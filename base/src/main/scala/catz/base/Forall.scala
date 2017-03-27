package catz.base

import catz.base.BasePackage.Ξ

trait Forall[F[_ <: Ξ]] extends Serializable {
  def apply[A <: Ξ]: F[A]
}

package catz.base

import catz.base.BasePackage.Ξ

trait Exists[F[_ <: Ξ]] extends Serializable { fa =>
  type A <: Ξ
  def apply: F[A]
}

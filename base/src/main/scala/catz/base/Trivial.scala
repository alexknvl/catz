package catz.base

import catz.base.BasePackage.Ξ

final class Trivial[A <: Ξ] private[Trivial]()
object Trivial {
  private[this] val anyTrivial: Trivial[Ξ] = new Trivial[Ξ]()
  def apply[A <: Ξ]: Trivial[A] = anyTrivial.asInstanceOf[Trivial[A]]
  implicit def catzBaseTrivialInstance[A <: Ξ]: Trivial[A] = apply[A]
}

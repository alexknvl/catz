package catz.category

import catz.base.BasePackage._

trait Semicategory[->[_ <: Ξ, _ <: Ξ]] {
  type C1[A <: Ξ, B <: Ξ] = ->[A, B]
  def andThen[A <: Ξ, B <: Ξ, C <: Ξ](ab: A -> B, bc: B -> C): A -> C
}
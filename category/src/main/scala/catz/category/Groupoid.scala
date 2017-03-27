package catz.category

import catz.base.BasePackage._
import catz.base.{LeibnizK, Trivial}

trait Groupoid[->[_ <: Ξ, _ <: Ξ]] extends Category[->] {
  def flip[A <: Ξ, B <: Ξ](f: A -> B): B -> A
}
object Groupoid {
  trait Aux[C1[_ <: Ξ, _ <: Ξ], C0[_ <: Ξ]] extends Groupoid[C1] with Category.Aux[C1, C0]
  trait AuxT[C1[_ <: Ξ, _ <: Ξ]] extends Aux[C1, Trivial]

  implicit val catzCategoryLeibnizKGroupoid: Groupoid.AuxT[===] = new Groupoid.AuxT[===] {
    override def id[A <: Ξ](implicit A: C0[A]): A === A =
      LeibnizK.refl[A]
    override def andThen[A <: Ξ, B <: Ξ, C <: Ξ](ab: A === B, bc: B === C): A === C =
      ab.andThen(bc)
    override def flip[A <: Ξ, B <: Ξ](ab: A === B): B === A =
      ab.flip
  }
}
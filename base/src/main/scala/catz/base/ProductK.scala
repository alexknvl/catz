package catz.base

import catz.base.BasePackage._
import catz.base.HVect.{Index, IndexK}

sealed abstract class Product[L <: HVect] extends Serializable {
  type Types = L
}
object Product {
  final case class View[L <: HVect](tag: Int, value: Any)
    extends Product[L]

  final case class Ops[L <: HVect, H](tag: Int) extends AnyVal {
  }
  object Ops {
    implicit def case0[L <: HVect, H](implicit I: Index[L, H]): Ops[L, H] =
      Ops(I.index)
  }
}

sealed class ProductK[L <: HVect, A <: Ξ] { type Constructors = L; type Argument = A }
object ProductK {
  final case class View[L <: HVect, A <: Ξ](values: Any) extends ProductK[L, A]

  final case class Ops[L <: HVect, H[_ <: Ξ]](tag: Int) {
  }
  object Ops {
    implicit def case0[L <: HVect, H[_ <: Ξ]](implicit I: IndexK[L, H]): Ops[L, H] =
      Ops(I.index)
  }
}
package catz.base

import HVect._
import catz.base.BasePackage._

sealed abstract class Coproduct[L <: HVect] extends Serializable {
  type Types = L
}
object Coproduct {
  final case class View[L <: HVect](tag: Int, value: Any)
    extends Coproduct[L]

  final case class Ops[L <: HVect, H](tag: Int) extends AnyVal {
    def inject(ha: H): Coproduct[L] = Coproduct.View(tag, ha)

    def project(coproduct: Coproduct[L]): Option[H] =
      coproduct match {
        case Coproduct.View(valueTag, value) if valueTag == tag =>
          Some(value.asInstanceOf[H])
        case _ => None
      }
  }
  object Ops {
    implicit def case0[L <: HVect, H](implicit I: Index[L, H]): Ops[L, H] =
      Ops(I.index)
  }
}

sealed abstract class CoproductK[L <: HVect, A <: Ξ] extends Serializable {
  type Constructors = L
  type Argument = A
}
object CoproductK {
  final case class View[L <: HVect, A <: Ξ](tag: Int, value: Any)
    extends CoproductK[L, A]

  final case class Ops[L <: HVect, H[_ <: Ξ]](tag: Int) extends AnyVal {
    def inject[A <: Ξ](ha: H[A]): CoproductK[L, A] =
      CoproductK.View(tag, ha)

    def project[A <: Ξ](coproduct: CoproductK[L, A]): Option[H[A]] =
      coproduct match {
        case CoproductK.View(valueTag, value) if valueTag == tag =>
          Some(value.asInstanceOf[H[A]])
        case _ => None
      }
  }
  object Ops {
    implicit def case0[L <: HVect, H[_ <: Ξ]](implicit I: IndexK[L, H]): Ops[L, H] =
      Ops(I.index)
  }
}

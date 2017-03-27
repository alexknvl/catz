package catz.base

import catz.base.BasePackage._

trait TypeLevel { self =>
  type Type >: self.type <: TypeLevel {
    type Type = self.Type
  }
}

trait Lifted extends TypeLevel { self =>
  type Type >: self.type <: Lifted {
    type Type = self.Type
    type Value = self.Value
  }
  type Value
}

final case class Value[N <: Lifted](value: N#Value) extends AnyVal { n =>
  def ==[M <: N#Type](m: Value[M])(implicit eq: Equiv[N#Value]): Option[EqT[N#Type, N, M]] =
    if (eq.equiv(n.value, m.value)) Some(BoundedLeibniz.unsafeForce[Nothing, N#Type, N, M])
    else None
}

trait Reduce[T <: TypeLevel] {
  type Result <: T#Type
  def proof: EqT[T#Type, T, Result]
}

object Reduce {
  trait Aux[T <: TypeLevel, R <: T#Type] extends Reduce[T] {
    type Result = R
  }

  def byDef[T <: TypeLevel, R <: T#Type]: Aux[T, R] =
    new Aux[T, R] { def proof = EqT.unsafeForce[Nothing, T#Type, T, R] }

  def apply[T <: TypeLevel](implicit r: Reduce[T]): EqT[T#Type, T, r.Result] =
    r.proof
}
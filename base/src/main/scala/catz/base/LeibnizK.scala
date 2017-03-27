package catz.base

import catz.base.BasePackage._
import LeibnizK._

/**
  * The data type `Leibniz` is the encoding of Leibnitz’ law which states that
  * if `a` and `b` are identical then they must have identical properties.
  * Leibnitz’ original definition reads as follows:
  *   a ≡ b = ∀ f .f a ⇔ f b
  * and can be proven to be equivalent to:
  *   a ≡ b = ∀ f .f a → f b
  *
  * The `Leibniz` data type encodes true type equality, since the identity
  * function is the only non-diverging conversion function that can be used
  * as an implementation of the `subst` method assuming that we do not break
  * parametricity. As the substitution function has to work for any `F[_]`, it
  * cannot make assumptions about the structure of `F[_]`, making it impossible
  * to construct a value of type `F[A]` or to access values of type `A` that
  * may be stored inside a value of type `F[A]`. Hence it is impossible for
  * a substitution function to alter the value it takes as argument.
  *
  * Not taking into account the partial functions that never terminate
  * (infinite loops), functions returning `null`, or throwing exceptions,
  * the identity function is the only function that can be used in place of
  * `subst` to construct a value of type `Leibniz[A, B]`.
  *
  * The existence of a value of type `Leibniz[A, B]` now implies that a ≡ b,
  * since the conversion function, that converts an `A` into a `B`, must be
  * the identity function.
  *
  * This technique was first used in
  * [[http://portal.acm.org/citation.cfm?id=583852.581494
  * Typing Dynamic Typing]] (Baars and Swierstra, ICFP 2002).
  *
  * @see [[===]] `A === B` is a type synonym to `Leibniz[A, B]`
  * @see [[http://typelevel.org/blog/2014/09/20/higher_leibniz.html
  *      Higher Leibniz]]
  */
sealed abstract class LeibnizK[A <: Ξ, B <: Ξ] private[LeibnizK]() { ab =>
  /**
    * To create an instance of `Leibniz[A, B]` you must show that for every
    * choice of `F[_]` you can convert `F[A]` to `F[B]`.
    */
  def subst[F[_ <: Ξ]](fa: F[A]): F[B]

  def substK[F[_ <: Ξ, _ <: Ξ]]: λ[`α <: Ξ` => F[A, α]] ~> λ[`α <: Ξ` => F[B, α]] = {
    type f[α <: Ξ] = λ[`β <: Ξ` => F[A, β]] ~> λ[`β <: Ξ` => F[α, β]]
    subst[f](FunctionK.id[λ[`α <: Ξ` => F[A, α]]])
  }

//  /**
//    * Substitution on identity brings about a direct coercion function.
//    */
//  final def coerce: A ~> B = {
//    type f[α <: Ξ] = A ~> α
//    subst[f](FunctionK.id[A])
//  }
//  /**
//    * Given `A =~= B` we can convert `X ~> A` into `X ~> B`.
//    */
//  final def onF[X[_ <: Ξ]](fa: X ~> A): X ~> B = {
//    type f[α <: Ξ] = X ~> α
//    subst[f](fa)
//  }

  /**
    * Equality is transitive relation and its witnesses can be composed
    * in a chain much like functions.
    *
    * @see [[compose]]
    */
  final def andThen[C <: Ξ](bc: B === C): A === C = {
    type f[α <: Ξ] = A === α
    bc.subst[f](ab)
  }

  /**
    * Equality is transitive relation and its witnesses can be composed
    * in a chain much like functions.
    *
    * @see [[andThen]]
    */
  final def compose[Z <: Ξ](za: Z === A): Z === B =
    za.andThen(ab)

  /**
    * Equality is symmetric relation and therefore can be flipped around.
    * Flipping is its own inverse, so `x.flip.flip == x`.
    */
  final def flip: B === A = {
    type f[α <: Ξ] = α === A
    ab.subst[f](refl)
  }

  /**
    * Given `A === B` we can prove that `F[A] === F[B]`.
    *
    * @see [[LeibnizK.lower]]
    * @see [[LeibnizK.lower2]]
    */
  final def lower[F[_ <: Ξ] <: Ξ]: F[A] === F[B] =
    LeibnizK.lower(ab)

  /**
    * Given `A === B` and `I === J` we can prove that `F[A, I] === F[B, J]`.
    *
    * @see [[LeibnizK.lower]]
    * @see [[LeibnizK.lower2]]
    * @see [[LeibnizK.lower3]]
    */
  final def lower2[F[_ <: Ξ, _ <: Ξ] <: Ξ]: PartiallyAppliedLower2[F] =
    new PartiallyAppliedLower2[F]
  final class PartiallyAppliedLower2[F[_ <: Ξ, _ <: Ξ] <: Ξ] {
    def apply[I <: Ξ, J <: Ξ](ij: I === J): F[A, I] === F[B, J] =
      LeibnizK.lower2(ab, ij)
  }

  /**
    * Given `A === B` we can prove that `F[A, ?] === F[B, ?]`.
    *
    * @see [[LeibnizK.lift]]
    * @see [[LeibnizK.lift2]]
    */
  final def lift[F[_ <: Ξ, _ <: Ξ] <: Ξ]: λ[`α <: Ξ` => F[A, α]] === λ[`α <: Ξ` => F[B, α]] =
    LeibnizK.lift(ab)

  /**
    * Given `A =~= B` and `I =~= J` we can prove that
    * `F[A, I, ?] =~= F[B, J, ?]`.
    *
    * @see [[LeibnizK.lift]]
    * @see [[LeibnizK.lift2]]
    * @see [[LeibnizK.lift3]]
    */
  final def lift2[F[_ <: Ξ, _ <: Ξ, _ <: Ξ] <: Ξ]: PartiallyAppliedLift2[F] =
    new PartiallyAppliedLift2[F]
  final class PartiallyAppliedLift2[F[_ <: Ξ, _ <: Ξ, _ <: Ξ] <: Ξ] {
    def apply[I <: Ξ, J <: Ξ](ij: I === J): λ[`α <: Ξ` => F[A, I, α]] === λ[`α <: Ξ` => F[B, J, α]] =
      LeibnizK.lift2(ab, ij)
  }
}

object LeibnizK {
  private[this] final case class Refl[A <: Ξ]() extends LeibnizK[A, A] {
    def subst[F[_ <: Ξ]](fa: F[A]): F[A] = fa
  }
  private[this] val anyRefl = Refl[Ξ]()

  /**
    * Unsafe coercion between types. `unsafeForce` abuses `asInstanceOf` to
    * explicitly coerce types. It is unsafe, but needed where Leibnizian
    * equality isn't sufficient.
    */
  @SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf"))
  def unsafeForce[A <: Ξ, B <: Ξ]: A === B =
    anyRefl.asInstanceOf[A === B]

  /**
    * Equality is reflexive relation.
    */
  def refl[A <: Ξ]: A === A = unsafeForce[A, A]

  /**
    * Given `A =~= B` we can prove that `F[A] === F[B]`.
    *
    * @see [[lower2]]
    * @see [[lower3]]
    */
  def lower[F[_ <: Ξ] <: Ξ, A <: Ξ, B <: Ξ]
  (ab: A === B): F[A] === F[B] = {
    type f[α <: Ξ] = F[A] === F[α]
    ab.subst[f](LeibnizK.refl[F[A]])
  }

  /**
    * Given `A === B` and `I === J` we can prove that
    * `F[A, I] === F[B, J]`.
    *
    * @see [[lower]]
    * @see [[lower3]]
    */
  def lower2[F[_ <: Ξ, _ <: Ξ] <: Ξ, A <: Ξ, B <: Ξ, I <: Ξ, J <: Ξ]
  (ab: A === B, ij: I === J): F[A, I] === F[B, J] = {
    type f1[α <: Ξ] = F[A, I] === F[α, I]
    type f2[α <: Ξ] = F[A, I] === F[B, α]
    ij.subst[f2](ab.subst[f1](LeibnizK.refl[F[A, I]]))
  }

  /**
    * Given `A === B`, `I === J`, and `M === N` we can prove that
    * `F[A, I] === F[B, J]`.
    *
    * @see [[lower]]
    * @see [[lower2]]
    */
  def lower3[F[_ <: Ξ, _ <: Ξ, _ <: Ξ] <: Ξ, A <: Ξ, B <: Ξ, I <: Ξ, J <: Ξ, M <: Ξ, N <: Ξ]
  (ab: A === B, ij: I === J, mn: M === N): F[A, I, M] === F[B, J, N] = {
    type f1[α <: Ξ] = F[A, I, M] === F[α, I, M]
    type f2[α <: Ξ] = F[A, I, M] === F[B, α, M]
    type f3[α <: Ξ] = F[A, I, M] === F[B, J, α]
    mn.subst[f3](ij.subst[f2](ab.subst[f1](LeibnizK.refl[F[A, I, M]])))
  }

  /**
    * Given `A === B` we can prove that `F[A, ?] === F[B, ?]`.
    *
    * @see [[lift2]]
    * @see [[lift3]]
    */
  def lift[F[_ <: Ξ, _ <: Ξ] <: Ξ, A <: Ξ, B <: Ξ]
  (ab: A === B): λ[`α <: Ξ` => F[A, α]] === λ[`α <: Ξ` => F[B, α]] = {
    type f[β <: Ξ] = λ[`α <: Ξ` => F[A, α]] === λ[`α <: Ξ` => F[β, α]]
    ab.subst[f](refl[λ[`α <: Ξ` => F[A, α]]])
  }

  /**
    * Given `A === B` and `I === J` we can prove that
    * `F[A, I, ?] === F[B, J, ?]`.
    *
    * @see [[lift]]
    * @see [[lift3]]
    */
  def lift2[F[_ <: Ξ, _ <: Ξ, _ <: Ξ] <: Ξ, A <: Ξ, B <: Ξ, I <: Ξ, J <: Ξ]
  (ab: A === B, ij: I === J): λ[`α <: Ξ` => F[A, I, α]] === λ[`α <: Ξ` => F[B, J, α]] = {
    type f1[β <: Ξ] = λ[`α <: Ξ` => F[A, I, α]] === λ[`α <: Ξ` => F[β, I, α]]
    type f2[β <: Ξ] = λ[`α <: Ξ` => F[A, I, α]] === λ[`α <: Ξ` => F[B, β, α]]
    ij.subst[f2](ab.subst[f1](refl[λ[`α <: Ξ` => F[A, I, α]]]))
  }

  /**
    * Given `A =~= B`, `I =~= J`, and `M =~= N` we can prove that
    * `F[A, I, M, ?] =~= F[B, J, N, ?]`.
    *
    * @see [[lift]]
    * @see [[lift2]]
    */
  def lift3[F[_ <: Ξ, _ <: Ξ, _ <: Ξ, _ <: Ξ] <: Ξ, A <: Ξ, B <: Ξ, I <: Ξ, J <: Ξ, M <: Ξ, N <: Ξ]
  (ab: A === B, ij: I === J, mn: M === N): λ[`α <: Ξ` => F[A, I, M, α]] === λ[`α <: Ξ` => F[B, J, N, α]] = {
    type f1[β <: Ξ] = λ[`α <: Ξ` => F[A, I, M, α]] === λ[`α <: Ξ` => F[β, I, M, α]]
    type f2[β <: Ξ] = λ[`α <: Ξ` => F[A, I, M, α]] === λ[`α <: Ξ` => F[B, β, M, α]]
    type f3[β <: Ξ] = λ[`α <: Ξ` => F[A, I, M, α]] === λ[`α <: Ξ` => F[B, J, β, α]]
    mn.subst[f3](ij.subst[f2](ab.subst[f1](refl[λ[`α <: Ξ` => F[A, I, M, α]]])))
  }
}
package util

// TODO: Improve naming
sealed trait Validation[+Value, Alternative] {

  import Validation._

  /**
   * The list of alternatives.
   */
  def alternatives: List[Alternative]

  /**
   * Returns `true` iff `this` validation has alternatives.
   */
  def hasAlternatives = alternatives.nonEmpty

  /**
   * Returns a [[Success]] containing the result of applying `f` to the value in this validation (if it exists).
   *
   * Preserves the alternatives.
   */
  @inline
  final def map[Output](f: Value => Output): Validation[Output, Alternative] = this match {
    case Success(value, alternatives) => Success(f(value), alternatives)
    case Failure(alternatives) => Failure(alternatives)
  }

  /**
   * Similar to `map` but does not wrap the result in a [[Success]].
   *
   * Preserves the alternatives.
   */
  @inline
  final def flatMap[Output](f: Value => Validation[Output, Alternative]): Validation[Output, Alternative] = this match {
    case Success(input, alternatives) => f(input) match {
      case Success(value, otherAlternatives) => Success(value, otherAlternatives ::: alternatives)
      case Failure(otherAlternatives) => Failure(otherAlternatives ::: alternatives)
    }
    case Failure(alternatives) => Failure(alternatives)
  }

  /**
   * Returns the result of the function `f` if `this` is a failure. Otherwise returns `this`.
   */
  @inline
  final def onFailure[A >: Value](f: => Validation[A, Alternative]): Validation[A, Alternative] = this match {
    case Success(value, alternatives) => Success(value, alternatives)
    case Failure(alternatives) => f match {
      case Success(value, otherAlternatives) => Success(value, otherAlternatives ::: alternatives)
      case Failure(otherAlternatives) => Failure(otherAlternatives ::: alternatives)
    }
  }

  /**
   * Returns `this` if it is successful. Otherwise returns a [[Success]] containing the result of `alt`.
   */
  @inline
  def orElse[A >: Value](alt: => A): Validation[A, Alternative] = this match {
    case Success(_, _) => this
    case Failure(alternatives) => Success(alt, alternatives)
  }

  /**
   * Returns `this` validation with an alternative appended.
   */
  @inline
  def withAlternative(alternative: Alternative): Validation[Value, Alternative] = this match {
    case Success(value, alternatives) => Success(value, alternatives ::: alternative :: Nil)
    case Failure(alternatives) => Failure(alternatives ::: alternative :: Nil)
  }

}

object Validation {

  /**
   * Folds the given function `f` over all elements `xs`.
   *
   * Returns a sequence of successful elements wrapped in [[Success]].
   */
  @inline
  def fold[In, Out, Alternative](xs: Seq[In], zero: Out)(f: (Out, In) => Validation[Out, Alternative]): Validation[Out, Alternative] = {
    xs.foldLeft(Success(zero, List.empty[Alternative]): Validation[Out, Alternative]) {
      case (acc, a) => acc flatMap {
        case value => f(value, a).orElse[Out](value)
      }
    }
  }

  /**
   * Flattens a sequence of validations into one validation. Alternatives are concatenated.
   *
   * Returns [[Success]] if every element in `xs` is a [[Success]].
   */
  @inline
  def flatten[Value, Alternative](xs: Seq[Validation[Value, Alternative]]): Validation[Seq[Value], Alternative] = {
    val zero = Success(List.empty[Value], List.empty[Alternative]): Validation[List[Value], Alternative]
    xs.foldLeft(zero) {
      case (Success(value, alternatives), Success(otherValue, otherAlternatives)) =>
        Success(otherValue :: value, otherAlternatives ::: alternatives)
      case (Success(value, alternatives), Failure(otherAlternatives)) =>
        Failure(otherAlternatives ::: alternatives)
      case (Failure(alternatives), Success(otherValue, otherAlternatives)) =>
        Failure(otherAlternatives ::: alternatives)
      case (Failure(alternatives), Failure(otherAlternatives)) =>
        Failure(otherAlternatives ::: alternatives)
    }
  }

  /**
   * Returns a sequence of values wrapped in a [[Success]] for every [[Success]] in `xs`. Alternatives are concatenated.
   */
  @inline
  def collect[Value, Alternative](xs: Seq[Validation[Value, Alternative]]): Validation[Seq[Value], Alternative] = {
    val zero = Success(List.empty[Value], List.empty[Alternative]): Validation[List[Value], Alternative]
    xs.foldLeft(zero) {
      case (Success(value, alternatives), Success(otherValue, otherAlternatives)) =>
        Success(otherValue :: value, otherAlternatives ::: alternatives)

      case (Success(value, alternatives), Failure(otherAlternatives)) =>
        Success(value, otherAlternatives ::: alternatives)

      case (Failure(alternatives), Success(otherValue, otherAlternatives)) =>
        Success(List(otherValue), otherAlternatives ::: alternatives)

      case (Failure(alternatives), Failure(otherAlternatives)) =>
        Success(List.empty, otherAlternatives ::: alternatives)
    }
  }

  /**
   * Merges 8 validations.
   */
  @inline
  def @@[A, B, C, D, E, F, G, H, X](a: Validation[A, X], b: Validation[B, X], c: Validation[C, X],
                                    d: Validation[D, X], e: Validation[E, X], f: Validation[F, X],
                                    g: Validation[G, X], h: Validation[H, X]): Validation[(A, B, C, D, E, F, G, H), X] =
    (@@(a, b, c, d, e, f, g), h) match {
      case (Success((valueA, valueB, valueC, valueD, valueE, valueF, valueG), altABCDEFG), Success(valueH, altH)) =>
        Success((valueA, valueB, valueC, valueD, valueE, valueF, valueG, valueH), altH ::: altABCDEFG)
      case (that, _) => Failure(h.alternatives ::: that.alternatives)
    }

  /**
   * Merges 7 validations.
   */
  @inline
  def @@[A, B, C, D, E, F, G, X](a: Validation[A, X], b: Validation[B, X], c: Validation[C, X],
                                 d: Validation[D, X], e: Validation[E, X], f: Validation[F, X],
                                 g: Validation[G, X]): Validation[(A, B, C, D, E, F, G), X] =
    (@@(a, b, c, d, e, f), g) match {
      case (Success((valueA, valueB, valueC, valueD, valueE, valueF), altABCDEF), Success(valueG, altG)) =>
        Success((valueA, valueB, valueC, valueD, valueE, valueF, valueG), altG ::: altABCDEF)
      case (that, _) => Failure(g.alternatives ::: that.alternatives)
    }

  /**
   * Merges 6 validations.
   */
  @inline
  def @@[A, B, C, D, E, F, X](a: Validation[A, X], b: Validation[B, X], c: Validation[C, X],
                              d: Validation[D, X], e: Validation[E, X], f: Validation[F, X]): Validation[(A, B, C, D, E, F), X] =
    (@@(a, b, c, d, e), f) match {
      case (Success((valueA, valueB, valueC, valueD, valueE), altABCDE), Success(valueF, altF)) =>
        Success((valueA, valueB, valueC, valueD, valueE, valueF), altF ::: altABCDE)
      case (that, _) => Failure(f.alternatives ::: that.alternatives)
    }

  /**
   * Merges 5 validations.
   */
  @inline
  def @@[A, B, C, D, E, X](a: Validation[A, X], b: Validation[B, X], c: Validation[C, X],
                           d: Validation[D, X], e: Validation[E, X]): Validation[(A, B, C, D, E), X] =
    (@@(a, b, c, d), e) match {
      case (Success((valueA, valueB, valueC, valueD), altABCD), Success(valueE, altE)) =>
        Success((valueA, valueB, valueC, valueD, valueE), altE ::: altABCD)
      case (that, _) => Failure(e.alternatives ::: that.alternatives)
    }

  /**
   * Merges 4 validations.
   */
  @inline
  def @@[A, B, C, D, X](a: Validation[A, X], b: Validation[B, X], c: Validation[C, X],
                        d: Validation[D, X]): Validation[(A, B, C, D), X] =
    (@@(a, b, c), d) match {
      case (Success((valueA, valueB, valueC), altABC), Success(valueD, altD)) =>
        Success((valueA, valueB, valueC, valueD), altD ::: altABC)
      case (that, _) => Failure(d.alternatives ::: that.alternatives)
    }

  /**
   * Merges 3 validations.
   */
  @inline
  def @@[A, B, C, X](a: Validation[A, X], b: Validation[B, X], c: Validation[C, X]): Validation[(A, B, C), X] =
    (@@(a, b), c) match {
      case (Success((valueA, valueB), altAB), Success(valueC, altC)) =>
        Success((valueA, valueB, valueC), altC ::: altAB)
      case (that, _) => Failure(c.alternatives ::: that.alternatives)
    }

  /**
   * Merges 2 validations.
   */
  @inline
  def @@[A, B, X](a: Validation[A, X], b: Validation[B, X]): Validation[(A, B), X] =
    (a, b) match {
      case (Success(valueA, altA), Success(valueB, altB)) =>
        Success((valueA, valueB), altB ::: altA)
      case _ => Failure(b.alternatives ::: a.alternatives)
    }

  /**
   * Add implicit `toVal` method.
   */
  implicit class ToValidation[+Value](val opt: Option[Value]) {
    def toSuccessOr[Alternative](alt: => Alternative): Validation[Value, Alternative] = opt match {
      case None => Failure(List(alt))
      case Some(value) => Success(value, List.empty)
    }
  }

  /**
   * Add implicit `toSuccess` method.
   */
  implicit class ToSuccess[+Value](val value: Value) {
    def toSuccess[R >: Value, Alternative]: Validation[R, Alternative] = Success(value, List.empty)
  }

  /**
   * Add implicit `toFailure` method.
   */
  implicit class ToFailure[+Alternative](val failure: Alternative) {
    def toFailure[Value, A >: Alternative]: Validation[Value, A] = Failure(List(failure))
  }

  /**
   * Represents a success `value` and `alternatives`.
   */
  case class Success[Value, Alternative](value: Value, alternatives: List[Alternative]) extends Validation[Value, Alternative]

  /**
   * Represents a failure with no value and `alternatives`.
   */
  case class Failure[Value, Alternative](alternatives: List[Alternative]) extends Validation[Value, Alternative]

}


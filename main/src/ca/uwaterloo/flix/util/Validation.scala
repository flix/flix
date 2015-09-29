package ca.uwaterloo.flix.util

// TODO: Improve naming
// TODO Incorporate back into uMonitor.
// TODO: Write test cases
sealed trait Validation[+Value, Error] {

  import Validation._

  /**
   * The list of errors.
   */
  def errors: List[Error]

  /**
   * Returns `true` iff `this` validation has errors.
   */
  // TODO: DOC
  def hasErrors = errors.nonEmpty

  // TODO: DOC
  def isSuccess: Boolean = this match {
    case v: Success[Value, Error] => true
    case _ => false
  }

  // TODO: DOC
  def isFailure: Boolean = !isSuccess

  // TODO: DOC
  def get: Value = this match {
    case Success(value, errors) => value
    case Failure(errors) => throw new RuntimeException()
  }


  /**
   * Returns a [[Success]] containing the result of applying `f` to the value in this validation (if it exists).
   *
   * Preserves the errors.
   */
  @inline
  final def map[Output](f: Value => Output): Validation[Output, Error] = this match {
    case Success(value, errors) => Success(f(value), errors)
    case Failure(errors) => Failure(errors)
  }

  /**
   * Similar to `map` but does not wrap the result in a [[Success]].
   *
   * Preserves the errors.
   */
  @inline
  final def flatMap[Output](f: Value => Validation[Output, Error]): Validation[Output, Error] = this match {
    case Success(input, errors) => f(input) match {
      case Success(value, otherAlternatives) => Success(value, otherAlternatives ::: errors)
      case Failure(otherAlternatives) => Failure(otherAlternatives ::: errors)
    }
    case Failure(errors) => Failure(errors)
  }

  /**
   * Returns the result of the function `f` if `this` is a failure. Otherwise returns `this`.
   */
  @inline
  final def onFailure[A >: Value](f: => Validation[A, Error]): Validation[A, Error] = this match {
    case Success(value, errors) => Success(value, errors)
    case Failure(errors) => f match {
      case Success(value, otherAlternatives) => Success(value, otherAlternatives ::: errors)
      case Failure(otherAlternatives) => Failure(otherAlternatives ::: errors)
    }
  }

  /**
   * Returns `this` if it is successful. Otherwise returns a [[Success]] containing the result of `alt`.
   */
  @inline
  def orElse[A >: Value](alt: => A): Validation[A, Error] = this match {
    case Success(_, _) => this
    case Failure(errors) => Success(alt, errors)
  }

  /**
   * Returns `this` validation with an error appended.
   */
  @inline
  def withAlternative(error: Error): Validation[Value, Error] = this match {
    case Success(value, errors) => Success(value, errors ::: error :: Nil)
    case Failure(errors) => Failure(errors ::: error :: Nil)
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
   * TODO: DOC
   */
  // TODO: Consider only folding over value?
  // TODO: Rename to map
  // TODO: need foldMap, foldMapKeys, foldMapValues
  @inline
  def fold[K, V, K2, V2, Alternative](m: Map[K, V])(f: (K, V) => Validation[(K2, V2), Alternative]): Validation[Map[K2, V2], Alternative] =
    m.foldLeft(Success(Map.empty[K2, V2], List.empty[Alternative]): Validation[Map[K2, V2], Alternative]) {
      case (macc, (k, v)) => macc flatMap {
        case ma => f(k ,v) map {
          case ko => ma + ko
        }
      }
    }

  /**
   * Flattens a sequence of validations into one validation. Alternatives are concatenated.
   *
   * Returns [[Success]] if every element in `xs` is a [[Success]].
   */
  // TODO: rename to @@?
  // TODO: Order is wrong
  // TODO: Prefer lists to Seq?
  @inline
  def @@[Value, Alternative](xs: Traversable[Validation[Value, Alternative]]): Validation[List[Value], Alternative] = {
    // TODO: Optimize
    val zero = Success(List.empty[Value], List.empty[Alternative]): Validation[List[Value], Alternative]
    xs.foldLeft(zero) {
      case (Success(value, errors), Success(otherValue, otherAlternatives)) =>
        Success(value ::: otherValue :: Nil, otherAlternatives ::: errors)
      case (Success(value, errors), Failure(otherAlternatives)) =>
        Failure(otherAlternatives ::: errors)
      case (Failure(errors), Success(otherValue, otherAlternatives)) =>
        Failure(otherAlternatives ::: errors)
      case (Failure(errors), Failure(otherAlternatives)) =>
        Failure(otherAlternatives ::: errors)
    }
  }

  /**
   * Returns a sequence of values wrapped in a [[Success]] for every [[Success]] in `xs`. Alternatives are concatenated.
   */
  @inline
  def collect[Value, Alternative](xs: Seq[Validation[Value, Alternative]]): Validation[Seq[Value], Alternative] = {
    val zero = Success(List.empty[Value], List.empty[Alternative]): Validation[List[Value], Alternative]
    xs.foldLeft(zero) {
      case (Success(value, errors), Success(otherValue, otherAlternatives)) =>
        Success(otherValue :: value, otherAlternatives ::: errors)

      case (Success(value, errors), Failure(otherAlternatives)) =>
        Success(value, otherAlternatives ::: errors)

      case (Failure(errors), Success(otherValue, otherAlternatives)) =>
        Success(List(otherValue), otherAlternatives ::: errors)

      case (Failure(errors), Failure(otherAlternatives)) =>
        Success(List.empty, otherAlternatives ::: errors)
    }
  }


  /**
   * Merges 2 validations.
   */
  @inline
  def @@[A, B, X](a: Validation[A, X], b: Validation[B, X]): Validation[(A, B), X] =
    (a, b) match {
      case (Success(valueA, altA), Success(valueB, altB)) =>
        Success((valueA, valueB), altB ::: altA)
      case _ => Failure(b.errors ::: a.errors)
    }

  /**
   * Merges 3 validations.
   */
  @inline
  def @@[A, B, C, X](a: Validation[A, X], b: Validation[B, X], c: Validation[C, X]): Validation[(A, B, C), X] =
    (@@(a, b), c) match {
      case (Success((valueA, valueB), altAB), Success(valueC, altC)) =>
        Success((valueA, valueB, valueC), altC ::: altAB)
      case (that, _) => Failure(c.errors ::: that.errors)
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
      case (that, _) => Failure(d.errors ::: that.errors)
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
      case (that, _) => Failure(e.errors ::: that.errors)
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
      case (that, _) => Failure(f.errors ::: that.errors)
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
      case (that, _) => Failure(g.errors ::: that.errors)
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
      case (that, _) => Failure(h.errors ::: that.errors)
    }

  /**
   * Merges two validations discarding the first value.
   */
  @inline
  def #@[A, B, X](a: Validation[A, X], b: Validation[B, X]): Validation[B, X] =
    (a, b) match {
      case (Success(valueA, altA), Success(valueB, altB)) =>
        Success(valueB, altB ::: altA)
      case _ => Failure(b.errors ::: a.errors)
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
   * Represents a success `value` and `errors`.
   */
  case class Success[Value, Alternative](value: Value, errors: List[Alternative]) extends Validation[Value, Alternative]

  /**
   * Represents a failure with no value and `errors`.
   */
  case class Failure[Value, Alternative](errors: List[Alternative]) extends Validation[Value, Alternative]

}


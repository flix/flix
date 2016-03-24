package ca.uwaterloo.flix.util

import scala.collection.mutable

// TODO: Improve naming
// TODO Incorporate back into uMonitor.
// TODO: Write test cases
// TODO: Consider optimizations for thisclass.
sealed trait Validation[+Value, +Error] {

  import Validation._

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
  final def flatMap[Output, A >: Error](f: Value => Validation[Output, A]): Validation[Output, A] = this match {
    case Success(input, errors) => f(input) match {
      case Success(value, thatErrors) => Success(value, errors ++: thatErrors)
      case Failure(thatErrors) => Failure(errors ++: thatErrors)
    }
    case Failure(errors) => Failure(errors)
  }

  /**
    * Returns the errors in this [[Success]] or [[Failure]] object.
    */
  def errors: Vector[Error]

  /**
    * Returns `true` iff this is a [[Success]] object.
    */
  def isSuccess: Boolean = this match {
    case v: Success[Value, Error] => true
    case _ => false
  }

  /**
    * Returns `true` iff this is a [[Failure]] object.
    */
  def isFailure: Boolean = !isSuccess

  /**
    * Returns the value inside `this` [[Success]] object.
    *
    * Throws an exception if `this` is a [[Failure]] object.
    */
  def get: Value = this match {
    case Success(value, errors) => value
    case Failure(errors) => throw new RuntimeException(s"Attempt to retrieve value from Failure. The errors are: ${errors.mkString(", ")}")
  }


}

object Validation {

  /**
    * Folds the given function `f` over all elements `xs`.
    *
    * Returns a sequence of successful elements wrapped in [[Success]].
    */
  @inline
  def fold[In, Out, Error](xs: Seq[In], zero: Out)(f: (Out, In) => Validation[Out, Error]): Validation[Out, Error] = {
    xs.foldLeft(Success(zero, Vector.empty[Error]): Validation[Out, Error]) {
      case (acc, a) => acc flatMap {
        case value => f(value, a)
      }
    }
  }

  /**
    * TODO: DOC
    */
  // TODO: need foldMap, foldMapKeys, foldMapValues
  @inline
  def fold[K, V, K2, V2, Error](m: Map[K, V])(f: (K, V) => Validation[(K2, V2), Error]): Validation[Map[K2, V2], Error] =
    m.foldLeft(Success(Map.empty[K2, V2], Vector.empty[Error]): Validation[Map[K2, V2], Error]) {
      case (macc, (k, v)) => macc flatMap {
        case ma => f(k, v) map {
          case ko => ma + ko
        }
      }
    }

  /**
    * TODO: DOC
    */
  @inline
  def @@[Value, Error](o: Option[Validation[Value, Error]]): Validation[Option[Value], Error] = o match {
    case None => Success(None, Vector.empty)
    case Some(Success(v, errors)) => Success(Some(v), errors)
    case Some(Failure(errors)) => Failure(errors)
  }

  /**
    * Flattens a sequence of validations into one validation. Errors are concatenated.
    *
    * Returns [[Success]] if every element in `xs` is a [[Success]].
    */
  @inline
  def @@[Value, Error](xs: Traversable[Validation[Value, Error]]): Validation[List[Value], Error] = {
    val zero = Success(List.empty[Value], Vector.empty[Error]): Validation[List[Value], Error]
    xs.foldRight(zero) {
      case (Success(curValue, curErrors), Success(accValue, accErrors)) =>
        Success(curValue :: accValue, curErrors ++: accErrors)
      case (Success(_, curErrors), Failure(accErrors)) =>
        Failure(curErrors ++: accErrors)
      case (Failure(curErrors), Success(_, accErrors)) =>
        Failure(curErrors ++: accErrors)
      case (Failure(curErrors), Failure(accErrors)) =>
        Failure(curErrors ++: accErrors)
    }
  }

  /**
    * Returns a sequence of values wrapped in a [[Success]] for every [[Success]] in `xs`. Errors are concatenated.
    */
  @inline
  def collect[Value, Error](xs: Seq[Validation[Value, Error]]): Validation[Seq[Value], Error] = {
    val zero = Success(List.empty[Value], Vector.empty[Error]): Validation[List[Value], Error]
    xs.foldRight(zero) {
      case (Success(value, errors), Success(accValue, accErrors)) =>
        Success(value :: accValue, errors ++: accErrors)
      case (Success(value, errors), Failure(accErrors)) =>
        Success(value :: Nil, errors ++: accErrors)
      case (Failure(errors), Success(accValue, accErrors)) =>
        Success(accValue, errors ++: accErrors)
      case (Failure(errors), Failure(accErrors)) =>
        Success(List.empty, errors ++: accErrors)
    }
  }


  /**
    * Merges 2 validations.
    */
  @inline
  def @@[A, B, X](a: Validation[A, X], b: Validation[B, X]): Validation[(A, B), X] =
    (a, b) match {
      case (Success(valueA, altA), Success(valueB, altB)) =>
        Success((valueA, valueB), altB ++: altA)
      case _ => Failure(b.errors ++: a.errors)
    }

  /**
    * Merges 3 validations.
    */
  @inline
  def @@[A, B, C, X](a: Validation[A, X], b: Validation[B, X], c: Validation[C, X]): Validation[(A, B, C), X] =
    (@@(a, b), c) match {
      case (Success((valueA, valueB), altAB), Success(valueC, altC)) =>
        Success((valueA, valueB, valueC), altC ++: altAB)
      case (that, _) => Failure(c.errors ++: that.errors)
    }

  /**
    * Merges 4 validations.
    */
  @inline
  def @@[A, B, C, D, X](a: Validation[A, X], b: Validation[B, X], c: Validation[C, X],
                        d: Validation[D, X]): Validation[(A, B, C, D), X] =
    (@@(a, b, c), d) match {
      case (Success((valueA, valueB, valueC), altABC), Success(valueD, altD)) =>
        Success((valueA, valueB, valueC, valueD), altD ++: altABC)
      case (that, _) => Failure(d.errors ++: that.errors)
    }

  /**
    * Merges 5 validations.
    */
  @inline
  def @@[A, B, C, D, E, X](a: Validation[A, X], b: Validation[B, X], c: Validation[C, X],
                           d: Validation[D, X], e: Validation[E, X]): Validation[(A, B, C, D, E), X] =
    (@@(a, b, c, d), e) match {
      case (Success((valueA, valueB, valueC, valueD), altABCD), Success(valueE, altE)) =>
        Success((valueA, valueB, valueC, valueD, valueE), altE ++: altABCD)
      case (that, _) => Failure(e.errors ++: that.errors)
    }

  /**
    * Merges 6 validations.
    */
  @inline
  def @@[A, B, C, D, E, F, X](a: Validation[A, X], b: Validation[B, X], c: Validation[C, X],
                              d: Validation[D, X], e: Validation[E, X], f: Validation[F, X]): Validation[(A, B, C, D, E, F), X] =
    (@@(a, b, c, d, e), f) match {
      case (Success((valueA, valueB, valueC, valueD, valueE), altABCDE), Success(valueF, altF)) =>
        Success((valueA, valueB, valueC, valueD, valueE, valueF), altF ++: altABCDE)
      case (that, _) => Failure(f.errors ++: that.errors)
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
        Success((valueA, valueB, valueC, valueD, valueE, valueF, valueG), altG ++: altABCDEF)
      case (that, _) => Failure(g.errors ++: that.errors)
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
        Success((valueA, valueB, valueC, valueD, valueE, valueF, valueG, valueH), altH ++: altABCDEFG)
      case (that, _) => Failure(h.errors ++: that.errors)
    }

  /**
    * Merges two validations discarding the first value.
    */
  @inline
  def #@[A, B, X](a: Validation[A, X], b: Validation[B, X]): Validation[B, X] =
    (a, b) match {
      case (Success(valueA, altA), Success(valueB, altB)) =>
        Success(valueB, altB ++: altA)
      case _ => Failure(b.errors ++: a.errors)
    }

  /**
    * Add implicit `toSuccess` method.
    */
  implicit class ToSuccess[+Value](val value: Value) {
    def toSuccess[V >: Value, Error]: Validation[V, Error] = Success(value, Vector.empty)
  }

  /**
    * Add implicit `toFailure` method.
    */
  implicit class ToFailure[+Error](val failure: Error) {
    def toFailure[Value, E >: Error]: Validation[Value, E] = Failure(Vector(failure))
  }

  /**
    * Represents a success `value` and `errors`.
    */
  case class Success[Value, Error](value: Value, errors: Vector[Error]) extends Validation[Value, Error]

  /**
    * Represents a failure with no value and `errors`.
    */
  case class Failure[Value, Error](errors: Vector[Error]) extends Validation[Value, Error]

}


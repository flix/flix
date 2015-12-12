package ca.uwaterloo.flix.language.ast

/**
  * A common super-type for operators.
  */
trait Operator

/**
  * A common super-type for unary operators.
  */
sealed trait UnaryOperator extends Operator

object UnaryOperator {

  /**
    * Unary negation.
    */
  case object Not extends UnaryOperator

  /**
    * Unary plus.
    */
  case object Plus extends UnaryOperator

  /**
    * Unary minus.
    */
  case object Minus extends UnaryOperator

  /**
    * Bitwise negate.
    */
  case object Negate extends UnaryOperator

  /**
    * A common super-type for unary set operations.
    */
  sealed trait SetOperator extends UnaryOperator

  // TODO: Option

  // TODO: List
  // head
  // last
  // tail, init
  // null
  // length
  // map
  // reverse
  //
  // -+ ,++
  // count
  // drop
  // dropRight
  // dropWhile
  // exists
  // filter
  // filterNot
  // fold
  // foldLeft
  // foldRight
  // reduce
  // head
  // tail
  // map
  // flatMap
  // all, every
  // toMap
  // take
  // drop
  // splitAt
  // takeWhile
  // dropWhile
  // isPrefixOf
  // isSuffixOf
  // in/elm/has
  // find
  // findIndex
  // zip
  // unzip

  // deleteBy
  // groupBy

  // TODO: Allow syntax for: variableName.length() --> length(variableName). "postfix call"
  // aSet.has(athing) --> has(aSet, athing).
  // aSet.getOrElse(xyz) --> getOrElse(aSet, xyz).
  //
  // foo::bar::baz(qux) <--> quux.foo::bar::baz.
  // aSet.Set::has(athing) --> Set::has(aSet, athing).
  // TODO: Allow postfix calls without ()?

  // namespace Set {
  //
  //    // TODO: Pattern matching can simplify this, e.g.:
  //
  //    match xs with {
  //      case #{} => // empty set
  //      case #{x} => // singleton with variable x
  //      case #{42} => singleton with literao 42
  //      case #{x, 42, y, rest...} => // set with two elements x and y, and 42, and rest...
  //    }
  //
  //    // queries
  //    fn null[A](xs: Set[A]): Bool = ...
  //    fn isEmpty[A](xs: Set[A]): Bool = ...
  //    fn isSingleton[A](xs: Set[A]): Bool = ...
  //    fn nonEmpty[A](xs: Set[A]): Bool = ...
  //
  //    head / tail, or conj.
  //
  //    fn size[A](xs: Set[A]): Int = ...
  //    fn in[A](a: A, Set[A]): Bool = ...
  //    fn memberOf[A](a: A, Set[A]): Bool = ...
  //    fn notMemberOf[A](a: A, Set[A]): Bool = ...
  //
  //    fn isSubsetOf[A](xs: Set[A], ys: Set[A]): Bool = ...
  //    fn isProperSubsetOf[A](xs: Set[A], ys: Set[A]): Bool = ...
  //    // TODO: super sets?
  //
  //    // construction
  //    fn empty(): Set[A] = ???
  //    fn singleton(a: A): Set[A] = ???
  //    fn insert(a: A, xs: Set[A]): Set[A] = ???
  //    fn delete(a: A, xs: Set[A]): Set[A] = ???
  //
  //    // combine
  //    fn union(xs:
  //    fn intersect
  //    fn diff
  //
  //    // filter
  //    fn filter
  //    fn partition
  //
  //    // map
  //    fn map
  //
  //    fn foldLeft
  //    fn foldRight
  //
  //    fn toList[A](xs: Set[A]): List[A] = ...
  //    fn toMap[A, B](xs: Set[(A, B)]: Map[A, B] = ...
  //    
  // }


  // TODO: Map
  // - getOrElse
  // - ++ (merge)
  // - removeKey
  // - foldValues
  // - isEmpty
  // - filterKeys/filterValues/exists/find/fold
  // - forall
  // - groupBy
  // - size
  // - member/notMembet
  // - lookup
  // - empty
  // - singleton
  // - insert
  // - delete
  // - update
  // - delete
  // - leftUnion
  // - difference
  // - intersection?
  // - map
  // - mapWithKey
  // - mapKeys
  // foldLeft/foldRigyht, foldLeftWithKey, foldRightWithKey
  // - elms
  // - keys
  // - toList
  // - fromList
  // filter/filterWithKey
  // paritition, partitionWithKey
  // isSubmapOf
  // isProperSubmapOf



  object Set {

    /**
      * A unary operator that returns `true` if its argument is the empty set.
      */
    case object IsEmpty extends UnaryOperator.SetOperator

    /**
      * A unary operator that returns `true` if its argument is a non-empty set.
      */
    case object NonEmpty extends UnaryOperator.SetOperator

    /**
      * A unary operator that returns `true` if its argument is a singleton set.
      */
    case object Singleton extends UnaryOperator.SetOperator

    /**
      * A unary operator that returns the size of a set.
      */
    case object Size extends UnaryOperator.SetOperator

  }

}

/**
  * A common super-type for binary operators.
  */
sealed trait BinaryOperator extends Operator

/**
  * A common super-type for arithmetic operators.
  */
sealed trait ArithmeticOperator extends BinaryOperator

/**
  * A common super-type for comparison operators.
  */
sealed trait ComparisonOperator extends BinaryOperator

/**
  * A common super-type for equality operators
  */
sealed trait EqualityOperator extends BinaryOperator

/**
  * A common super-type for logical operators.
  */
sealed trait LogicalOperator extends BinaryOperator

/**
 * A common super-type for bitwise operators.
 */
sealed trait BitwiseOperator extends BinaryOperator

object BinaryOperator {

  /**
    * Addition.
    */
  case object Plus extends ArithmeticOperator

  /**
    * Subtraction.
    */
  case object Minus extends ArithmeticOperator

  /**
    * Multiplication.
    */
  case object Times extends ArithmeticOperator

  /**
    * Division.
    */
  case object Divide extends ArithmeticOperator

  /**
    * Modulus.
    */
  case object Modulo extends ArithmeticOperator

  /**
    * Strictly less-than.
    */
  case object Less extends ComparisonOperator

  /**
    * Less than or equal.
    */
  case object LessEqual extends ComparisonOperator

  /**
    * Strictly greater.
    */
  case object Greater extends ComparisonOperator

  /**
    * Greater or equal.
    */
  case object GreaterEqual extends ComparisonOperator

  /**
    * Equality
    */
  case object Equal extends EqualityOperator

  /**
    * Inequality.
    */
  case object NotEqual extends EqualityOperator

  /**
    * Logical conjunction.
    */
  case object And extends LogicalOperator

  /**
    * Logical disjunction.
    */
  case object Or extends LogicalOperator

  /**
   * Bitwise and.
   */
  case object BitwiseAnd extends BitwiseOperator

  /**
   * Bitwise or.
   */
  case object BitwiseOr extends BitwiseOperator

  /**
   * Bitwise xor.
   */
  case object BitwiseXor extends BitwiseOperator

  /**
   * Bitwise left shift.
   */
  case object BitwiseLeftShift extends BitwiseOperator

  /**
   * Bitwise right shift.
   */
  case object BitwiseRightShift extends BitwiseOperator

  /**
    * A common super-type for binary set operators.
    */
  sealed trait SetOperator extends BinaryOperator

  object Set {

    /**
      * A binary operator that returns `true` if the left argument is a member of the right argument.
      */
    case object Member extends BinaryOperator.SetOperator

    /**
      * A binary operator that returns `true` if the left argument is a subset of the right argument.
      */
    case object SubsetOf extends BinaryOperator.SetOperator

    /**
      * A binary operator that returns `true` if the left argument is a proper subset of the right argument.
      */
    case object ProperSubsetOf extends BinaryOperator.SetOperator

    /**
      * A binary operator that returns the left argument with the right argument added.
      */
    case object Insert extends BinaryOperator.SetOperator

    /**
      * A binary operator that returns the left argument with the right argument removed.
      */
    case object Remove extends BinaryOperator.SetOperator

    /**
      * A binary operator that returns the set union of its two arguments.
      */
    case object Union extends BinaryOperator.SetOperator

    /**
      * A binary operator that returns the set intersection of its two arguments.
      */
    case object Intersection extends BinaryOperator.SetOperator

    /**
      * A binary operator that returns the set difference of its two arguments.
      */
    case object Difference extends BinaryOperator.SetOperator

  }


}

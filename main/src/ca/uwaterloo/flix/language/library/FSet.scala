package ca.uwaterloo.flix.language.library

object FSet {

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
  //      case #Set{} => // empty set
  //      case #Set{x} => // singleton with variable x
  //      case #Set{42} => singleton with literao 42
  //      case #Set{x, 42, y, rest...} => // set with two elements x and y, and 42, and rest...
  //    }
  //
  //    // queries
  //    fn null[A](xs: Set[A]): Bool = ...
  //    fn isEmpty[A](xs: Set[A]): Bool = ...
  //    fn isSingleton[A](xs: Set[A]): Bool = ...
  //    fn nonEmpty[A](xs: Set[A]): Bool = ...
  //
  //    fn head(xs: Set[A]): A
  //    fn tail(xs: Set[A]): A
  //
  //    fn conj(xs: Set[A]): (A, Set[A])
  //
  //    fn size[A](xs: Set[A]): Int = ...
  //    fn in[A](a: A, Set[A]): Bool = ...
  //    fn memberOf[A](a: A, Set[A]): Bool = ...
  //    fn notMemberOf[A](a: A, Set[A]): Bool = ...
  //
  //    fn isSubsetOf[A](xs: Set[A], ys: Set[A]): Bool = ...
  //    fn isProperSubsetOf[A](xs: Set[A], ys: Set[A]): Bool = ...
  //
  //    // construction
  //    fn empty(): Set[A] = ???
  //    fn singleton(a: A): Set[A] = ???
  //    fn insert(a: A, xs: Set[A]): Set[A] = ???
  //    fn delete(a: A, xs: Set[A]): Set[A] = ???
  //
  //    // combine
  //    fn union[A](xs: Set[A], ys: Set[A]): Set[A] = ...
  //    fn intersection[A](xs: Set[A], ys: Set[A]): Set[A] = ...
  //    fn difference[A](xs: Set[A], ys: Set[A]): Set[A] = ...
  //
  //    // filter/select/where?
  //    fn filter[A](xs: Set[A], f: A => Bool): Set[A] = ???
  //    fn partition[A](xs: Set[A], f: A => Bool): (Set[A], Set[A]) = ???
  //
  //    // map
  //    fn map[A, B](xs: Set[A], f: A => B): Set[B] = ???
  //
  //    fn foldLeft[A, B](xs: Set[A], b: B, f: (A, B => B)): B
  //    fn foldRight[A, B](xs: Set[A], b: B, f: (A, B => B)): B
  //
  //    fn toList[A](xs: Set[A]): List[A] = ...
  //    fn toMap[A, B](xs: Set[(A, B)]: Map[A, B] = ...
  //
  // }

}

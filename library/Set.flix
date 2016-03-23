//## Set Construction
//- **empty: Set[A]**
//    - The empty set.
//- **singleton(x: A): Set[A]**
//    - The singleton set of x.
//- **insert(x: A, xs: Set[A]): Set[A]**
//    - Adds x to the set xs.
//- **delete(x: A, xs: Set[A]): Set[A]**
//    - Removes x from the set xs.
//
//## Set Predicates
//- **null(xs: Set[A]): Bool**
//    - Returns true if the set is empty.
//- **memberOf(x: A, xs: Set[A]): Bool**
//    - Returns true if x is a member of the set xs.
//- **isSubsetOf(xs: Set[A], ys: Set[A]): Bool**
//    - Returns true if every element in xs appears in ys.
//- **isProperSubsetOf(xs: Set[A], ys: Set[A]): Bool**
//    - Returns true if every element in xs appears in ys and xs != ys.
//
//## Elementary Set Operations
//- **union(xs: Set[A], ys: Set[A]): Set[A]**
//    - Returns a new set of the union of xs and ys.
//- **intersection(xs: Set[A], ys: Set[A]): Set[A]**
//    - Returns a new set of the intersection of xs and ys.
//- **difference(xs: Set[A], ys: Set[A]): Set[A]**
//    - Returns a new set of the difference of xs and ys.
//- **subsets(xs: Set[A]): Set[Set[A]]**
//    - Returns the subsets of the set xs.
//
//## Set Transformation
//- **filter(f: A => Bool, xs: Set[A]): Set[A]**
//    - Returns the subset of xs where the elements satisfy the predicate f.
//- **map(f: A => B, xs: Set[A]): Set[B]**
//    - Returns the result of applying the function f to every element in the set.
//    - Note: The returned set may be smaller than the given set.
//- **flatMap(f: A => Set[B], xs: Set[A]): Set[B]**
//    - Returns the result of applying the function f to every element in the set and taking the union of the result.
//
//## Set Conversions
//- **toList(xs: Set[A]): List[A]**
//    - Alias for toAscList.
//- **toAscList(xs: Set[A]): List[A]**
//    - Returns the set as a list in ascending order (w.r.t. its natural order).
//- **toDescList(xs: Set[A]): List[A]**
//    - Returns the set as a list in descending order (w.r.t. its natural order).
//- **toMap(xs: Set[(A, B)]): Map[A, B]**
//    - Returns the set of pairs as a map. If the set contains the same mapping multiple times the one with the greatest key (w.r.t. the natural order) takes precedence. Functionally equivalent to List/toMap(Set/toList(xs)).
//
//## Order and Lattice Operations
//- **isAntiChain(xs: Set[A]): Bool**
//    - Returns true if the set xs is an anti chain. That is, if every element is incomparable every other element.
//- **join(xs: Set[A]): A**
//    - Returns the least upper bound of all element in xs.  The bottom element of A is returned if the set is empty.
//- **meet(xs: Set[A]): A**
//    - Similar to lub, but with the greatest lower bound and top element.
//- **widen(xs: Set[A]): A**
//    - Similar to lub, but with the widening operator.
//- **narrow(xs: Set[A]): A**
//    - Similar to lub, but with the narrowing operator.

// TODO: Forall???
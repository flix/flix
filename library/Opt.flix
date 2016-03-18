namespace Opt {

    /**
     * Returns `true` iff `o` is `None`.
     */
    external def null(o: Opt[A]): Bool

    /**
     * Returns `v` if `o` is `Some(v)`. Undefined semantics if `o` is `None`.
     */
    external def get(o: Opt[A]): A

//- **getWithDefault(o: Opt[A], a: A): A**
//    - Returns v if o is Some(v). Otherwise returns a.
//- **exists(f: A => Bool, o: Opt[A]): Bool**
//    - Returns the value of f(v) if o is Some(v). Otherwise false.
//- **forall(f: A => Bool, o: Opt[A]): Bool**
//    - Returns the value of f(v) if o is Some(v). Otherwise true.
//- **filter(f: A => Bool, o: Opt[A]): Opt[A]**
//    - Returns o if o is Some(v) and f(v) is true. Otherwise returns None.
//- **map(f: A => B, o: Opt[A]): Opt[B]**
//    - Returns Some(f(v)) if o is Some(v). Otherwise returns None.
//- **map2(f: (A, B) => C, o1: Opt[A], o2: Opt[B]): Opt[C]**
//    - Returns Some(f(v1, v2)) if o1 is Some(v1) and Some(v2).
//    - Otherwise returns None.
//- **flatMap(f: A => Opt[B], o: Opt[A]): Opt[B]**
//    - Returns f(v) if o is Some(v). Otherwise returns None.
//- **flatMap2(f: (A, B) => Opt[C], o1: Opt[A], o2: Opt[A]): Opt[C]**
//    - Returns f(v1, v2) if o1 is Some(v1) and o2 is Some(v2).
//    - Otherwise returns None.
//- **toList(o: Opt[A]): List[A]**
//    - Returns a one-element list of the value v if o is Some(v).
//    - Otherwise returns the empty list.
//- **toSet(o: Opt[A]): Set[A]**
//    - Returns a one-element set of the value v if o is Some(v).
//    - Otherwise returns the empty set.
//- **withDefault(o1: Opt[A], o2: Opt[A]): Opt[A]**
//    - Returns o1 if it is Some(v) otherwise returns o2.
//


}
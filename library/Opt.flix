// TODO: List[A] vs List[a] vs List[Color] ...

namespace Opt {

    /**
     * Returns `true` iff `o` is `None`.
     */
    def null(o: Opt[A]): Bool = match o with {
        case None => true
        case Some(v) => false
    }

    /**
     * Returns `v` if `o` is `Some(v)`. Undefined semantics if `o` is `None`.
     */
    def get(o: Opt[A]): A = match o with {
        case Some(v) => v
    }

    /**
     * Returns `v` if `o` is `Some(v).` Otherwise returns `a`.
     */
    def getWithDefault(o: Opt[A], a: A): A = match o with {
        case None => a
        case Some(v) => v
    }

    /**
     * Returns the value of `f(v)` if `o` is `Some(v)`. Otherwise returns `false`.
     */
    def exists(f: A -> Bool, o: Opt[A]): Bool = getWithDefault(map(f, o), false)

    /**
     * Returns the value of `f(v)` if `o` is `Some(v)`. Otherwise returns `true`.
     */
    def forall(f: A -> Bool, o: Opt[A]): Bool = getWithDefault(map(f, o), true)

    /**
     * Returns `o` if `o` is `Some(v)` and `f(v)` is true. Otherwise returns `None`.
     */
    def filter(f: A -> Bool, o: Opt[A]): Opt[A] = match o with {
        case None => None
        case Some(v) => if (f(v)) Some(v) else None
    }

    /**
     * Returns `Some(f(v))` if `o` is `Some(v)`. Otherwise returns `None`.
     */
    def map(f: A -> B, o: Opt[A]): Opt[B] = match o with {
        case None => None
        case Some(v) => Some(f(v))
    }

    /**
     * Returns `Some(f(v1, v2))` if `o1` is `Some(v1)` and `o2` is `Some(v2)`. Otherwise returns `None`.
     */
    def map2(f: (A, B) -> C, o1: Opt[A], o2: Opt[B]): Opt[C] = match (o1, o2) with {
        case (None, _) => None
        case (_, None) => None
        case (Some(v1), Some(v2)) => Some(f(v1, v2))
    }

    /**
     * Returns `f(v)` if `o` is `Some(v)`. Otherwise returns `None`.
     */
    def flatMap(f: A -> Opt[B], o: Opt[A]): Opt[B] = match o with {
        case None => None
        case Some(v) => f(v)
    }

    /**
     * Returns `f(v1, v2)` if `o1` is `Some(v1)` and `o2` is `Some(v2)`. Otherwise returns `None`.
     */
    def flatMap2(f: (A, B) -> Opt[C], o1: Opt[A], o2: Opt[A]): Opt[C] = match (o1, o2) with {
        case (None, _) => None
        case (_, None) => None
        case (Some(v1), Some(v2)) => f(v1, v2)
    }

    /**
     * Returns a one-element list of the value `v` if o is `Some(v)`. Otherwise returns the empty list.
     */
    def toList(o: Opt[A]): List[A] = match o with {
        case None => Nil
        case Some(v) => v :: Nil
    }

    /**
     * Returns a one-element set of the value `v` if `o` is `Some(v)`. Otherwise returns the empty set.
     */
    def toSet(o: Opt[A]): Set[A] = match o with {
        case None => #{}
        case Some(v) => #{v}
    }

    /**
     * Returns `o1` if it is `Some(v)`. Otherwise returns `o2`.
     */
    def withDefault(o1: Opt[A], o2: Opt[A]): Opt[A] = match o1 with {
        case None => o2
        case Some(v) => o1
    }

}
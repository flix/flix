
    ///
    /// Returns `true` iff `o` is `None`.
    ///
    def null[a](o: Option[a]): Bool = match o with {
        case None => true
        case Some(v) => false
    }

    ///
    /// Returns `v` if `o` is `Some(v)`. Undefined semantics if `o` is `None`.
    ///
    def get[a](o: Option[a]): a = match o with {
        case Some(v) => v
    }

    ///
    /// Returns `v` if `o` is `Some(v).` Otherwise returns `d`.
    ///
    def getWithDefault[a](o: Option[a], d: a): a = match o with {
        case None => d
        case Some(v) => v
    }

    ///
    /// Returns `true` if `o` is `Some(v)` and the predicate `f(v)` evaluates to `true`. Otherwise returns `false`.
    ///
    def exists[a](f: a -> Bool, o: Option[a]): Bool = getWithDefault(map(f, o), false)

    ///
    /// Returns `true` if `o` is `Some(v)` and the predicate `f(v)` evaluates to `true` or if `o` is `None`.
    /// Otherwise returns `false`.
    ///
    def forall[a](f: a -> Bool, o: Option[a]): Bool = getWithDefault(map(f, o), true)

    ///
    /// Returns `o` if `o` is `Some(v)` and the predicate `f(v)` is true. Otherwise returns `None`.
    ///
    def filter[a](f: a -> Bool, o: Option[a]): Option[a] = match o with {
        case None => None
        case Some(v) => if (f(v)) Some(v) else None
    }

    ///
    /// Returns `Some(f(v))` if `o` is `Some(v)`. Otherwise returns `None`.
    ///
    def map[a,b](f: a -> b, o: Option[a]): Option[b] = match o with {
        case None => None
        case Some(v) => Some(f(v))
    }

    ///
    /// Returns `Some(f(v1, v2))` if `o1` is `Some(v1)` and `o2` is `Some(v2)`. Otherwise returns `None`.
    ///
    def map2[a,b,c](f: (a, b) -> c, o1: Option[a], o2: Option[b]): Option[c] = match (o1, o2) with {
        case (None, _) => None
        case (_, None) => None
        case (Some(v1), Some(v2)) => Some(f(v1, v2))
    }

    ///
    /// Returns `f(v)` if `o` is `Some(v)`. Otherwise returns `None`.
    ///
    def flatMap[a,b](f: a -> Option[b], o: Option[a]): Option[b] = match o with {
        case None => None
        case Some(v) => f(v)
    }

    ///
    /// Returns `f(v1, v2)` if `o1` is `Some(v1)` and `o2` is `Some(v2)`. Otherwise returns `None`.
    ///
    def flatMap2[a,b,c](f: (a, b) -> Option[c], o1: Option[a], o2: Option[b]): Option[c] = match (o1, o2) with {
        case (None, _) => None
        case (_, None) => None
        case (Some(v1), Some(v2)) => f(v1, v2)
    }

    ///
    /// Returns a one-element list of the value `v` if o is `Some(v)`. Otherwise returns the empty list.
    ///
    def toList[a](o: Option[a]): List[a] = match o with {
        case None => Nil
        case Some(v) => v :: Nil
    }

    ///
    /// Returns a one-element set of the value `v` if `o` is `Some(v)`. Otherwise returns the empty set.
    ///
    def toSet[a](o: Option[a]): Set[a] = match o with {
        case None => #{}
        case Some(v) => #{v}
    }

    ///
    /// Returns `o1` if it is `Some(v)`. Otherwise returns `o2`.
    ///
    def withDefault[a](o1: Option[a], o2: Option[a]): Option[a] = match o1 with {
        case None => o2
        case Some(v) => o1
    }

    ///
    /// Returns `Ok(v)` if `o` is `Some(v)`. Otherwise returns `Err(e)`.
    ///
    def toResult[a,b](o: Option[a], e: b): Result[a,b] = match o with {
        case None => Err(e)
        case Some(v) => Ok(v)
    }

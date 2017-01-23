namespace Map {

    ///
    /// Returns the size of `m`.
    ///
    def size[k, v](m: Map[k, v]): Int32 =
        let Map(xs) = m;
            List.length(xs)

    //
    // ## Map Construction
    //

    ///
    /// Returns the empty map.
    ///
    def empty[k, v]: Map[k, v] = Map(Nil)

    ///
    /// Returns the singleton map where key `k` is mapped to value `v`.
    ///
    def singleton[k, v](k: k, v: v): Map[k, v] = Map((k, v) :: Nil)

    //
    // ## Basic Operations
    //

    ///
    /// Returns `true` if and only if `m` is the empty map, i.e. `Map(Nil)`.
    ///
    def null[k, v](m: Map[k, v]): Bool =
        let Map(xs) = m;
            List.isEmpty(xs)

    ///
    /// Returns `Some(v)` if `k -> v` is in `m`.
    /// Otherwise returns `None`.
    ///
    def get[k, v](k: k, m: Map[k, v]): Option[v] =
        let Map(xs) = m;
            List.findMap(match (key, v) -> if (key != k) None else Some(v), xs)

    ///
    /// Returns `v` if `k -> v` is in `m`.
    /// Otherwise, returns `d`.
    ///
    def getWithDefault[k, v](k: k, d: v, m: Map[k, v]): v = match m with {
        case Map(Nil) => d
        case Map((a, b) :: rs) => if (a == k) b else getWithDefault(k, d, Map(rs))
    }

    ///
    /// Returns `true` if and only if `m` contains the key `k`.
    ///
    def memberOf[k, v](k: k, m: Map[k, v]): Bool = match m with {
        case Map(Nil) => false
        case Map((a, b) :: rs) => if (a == k) true else memberOf(k, Map(rs))
    }

    ///
    /// Returns the keys of `m`.
    ///
    def keysOf[k, v](m: Map[k, v]): Set[k] =
        let Map(xs) = m;
            List.toSet(fst(List.unzip(xs)))

    ///
    /// Returns the values of `m`.
    ///
    def valuesOf[k, v](m: Map[k, v]): List[v] =
        let Map(xs) = m;
            snd(List.unzip(xs))

    //
    // ## Insert
    //

    ///
    /// Updates `m` with `k -> v`.
    ///
    def insert[k, v](k: k, v: v, m: Map[k, v]): Map[k, v] =
        insertWithKey((k1, v1, v2) -> v1, k, v, m)

    ///
    /// Updates `m` with `k -> f(v, v1)` if `k -> v1` is in `m`.
    /// Otherwise, updates `m` with `k -> v`.
    ///
    def insertWith[k, v](f: (v, v) -> v, k: k, v: v, m: Map[k, v]): Map[k, v] =
        insertWithKey((k1, v1, v2) -> f(v1, v2), k, v, m)

    ///
    /// Updates `m` with `k -> f(k, v, v1)` if `k -> v1` is in `m`.
    /// Otherwise, updates `m` with `k -> v`.
    ///
    def insertWithKey[k, v](f: (k, v, v) -> v, k: k, v: v, m: Map[k, v]): Map[k, v] =
        let Map(xs) = m;
            Map(insertWithKeyHelper(f, k, v, xs))

    ///
    /// Helper function for `insertWithKey`.
    ///
    @internal
    def insertWithKeyHelper[k, v](f: (k, v, v) -> v, k: k, v: v, xs: List[(k, v)]): List[(k, v)] = match xs with {
        case Nil => (k, v) :: Nil
        case (a, b) :: rs => if (k == a) (k, f(k, v, b)) :: rs else (a, b) :: insertWithKeyHelper(f, k, v, rs)
    }

    //
    // ## Update
    //

    ///
    /// Updates `m` with `k -> f(v)` if `k -> v` is in `m`.
    /// Otherwise, returns `m`.
    ///
    def adjust[k, v](f: v -> v, k: k, m: Map[k, v]): Map[k, v] =
        adjustWithKey((k1, v1) -> f(v1), k, m)

    ///
    /// Updates `m` with `k -> f(k, v)` if `k -> v` is in `m`.
    /// Otherwise, returns `m`.
    ///
    def adjustWithKey[k, v](f: (k, v) -> v, k: k, m: Map[k, v]): Map[k, v] =
        let Map(xs) = m;
            Map(List.map(match (key, v) -> if (key == k) (k, f(k, v)) else (key, v), xs))

    ///
    /// Updates `m` with `k -> v1` if `k -> v` is in `m` and `f(v) = Some(v1).
    /// Otherwise, returns `m`.
    ///
    def update[k, v](f: v -> Option[v], k: k, m: Map[k, v]): Map[k, v] =
        updateWithKey((k1, v1) -> f(v1), k, m)

    ///
    /// Updates `m` with `k -> v1` if `k -> v` is in `m` and `f(k, v) = Some(v1).
    /// Otherwise, returns `m`.
    ///
    def updateWithKey[k, v](f: (k, v) -> Option[v], k: k, m: Map[k, v]): Map[k, v] =
        let Map(xs) = m;
            Map(List.map(match (key, v) -> if (key == k) (k, Option.getWithDefault(f(k, v), v)) else (key, v), xs))

    //
    // ## Delete
    //

    ///
    ///
    ///
    def delete[k, v](k: k, m: Map[k, v]): Map[k, v] =
        let Map(xs) = m;
            Map(List.filter(match (key, v) -> key != k, xs))

    //
    // ## Map Predicates
    //

    ///
    /// Returns `true` if and only if all mappings in `m1` occur in `m2`.
    ///
    def isSubmapOf[k, v](m1: Map[k, v], m2: Map[k, v]): Bool =
        let Map(xs) = m1;
        let Map(ys) = m2;
            List.forall(x -> List.memberOf(x, ys), xs)

    ///
    /// Returns `true` if and only if all mappings in `m1` occur in `m2` and `m1 != m2`.
    ///
    def isProperSubmapOf[k, v](m1: Map[k, v], m2: Map[k, v]): Bool = isSubmapOf(m1, m2) && size(m1) != size(m2)

    //
    // ## Map Transformation
    //

    ///
    /// Returns a map of all mappings `k -> v` in `m` where `v` satisfies the predicate `f`.
    ///
    def filter[k, v](f: v -> Bool, m: Map[k, v]): Map[k, v] = filterWithKey((k, v) -> f(v), m)

    ///
    /// Returns a map of all mappings `k -> v` in `m` where `(k, v)` satisfies the predicate `f`.
    ///
    def filterWithKey[k, v](f: (k, v) -> Bool, m: Map[k, v]): Map[k, v] =
        let Map(xs) = m;
            Map(List.filter(x -> f(fst(x), snd(x)), xs))

    ///
    /// Returns a map with mappings `k -> f(v)` for every `k -> v` in `m`.
    ///
    def map[k, v1, v2](f: v1 -> v2, m: Map[k, v1]): Map[k, v2] = mapWithKey((k, v) -> f(v), m)

    ///
    /// Returns a map with mappings `k -> f(k, v)` for every `k -> v` in `m`.
    ///
    def mapWithKey[k, v1, v2](f: (k, v1) -> v2, m: Map[k, v1]): Map[k, v2] =
        let Map(xs) = m;
            Map(List.map(match (k, v) -> (k, f(k, v)), xs))

    //
    // ## Fold
    //

    ///
    /// Alias for `foldLeft`.
    ///
    def fold[k, v, b](f: (b, v) -> b, s: b, m: Map[k, v]): b = foldLeft(f, s, m)

    ///
    /// Alias for `foldLeftWithKey`.
    ///
    def foldWithKey[k, v, b](f: (k, b, v) -> b, s: b, m: Map[k, v]): b = foldLeftWithKey(f, s, m)

    ///
    /// Applies `f` to a start value `s` and all values in `m` going from left to right.
    /// That is, the result is of the form: `f(...f(f(s, v1), v2)..., vn)`.
    ///
    def foldLeft[k, v, b](f: (b, v) -> b, s: b, m: Map[k, v]): b = foldLeftWithKey((k, b, v) -> f(b, v), s, m)

    ///
    /// Applies `f` to a start value `s` and all key-value pairs in `m` going from left to right.
    /// That is, the result is of the form: `f(...f(k2, f(k1, s, v1), v2)..., vn)`.
    ///
    def foldLeftWithKey[k, v, b](f: (k, b, v) -> b, s: b, m: Map[k, v]): b =
        let Map(xs) = m;
            List.foldLeft((y, kv) -> f(fst(kv), y, snd(kv)), s, xs)

    ///
    /// Applies `f` to a start value `s` and all values in `m` going from right to left.
    /// That is, the result is of the form: `f(v1, ...f(vn-1, f(vn, s)))`.
    ///
    def foldRight[k, v, b](f: (v, b) -> b, s: b, m: Map[k, v]): b = foldRightWithKey((k, v, b) -> f(v, b), s, m)

    ///
    /// Applies `f` to a start value `s` and all values in `m` going from right to left.
    /// That is, the result is of the form: `f(v1, ...f(vn-1, f(vn, s)))`.
    ///
    def foldRightWithKey[k, v, b](f: (k, v, b) -> b, s: b, m: Map[k, v]): b =
        let Map(xs) = m;
            List.foldRight((kv, y) -> f(fst(kv), snd(kv), y), s, xs)

    //
    // ## Combine Two Maps
    //

    ///
    /// Returns the left-biased union of `m1` and `m2`.
    /// That is, key collisions are resolved by taking the mapping from `m1`.
    ///
    def union[k, v](m1: Map[k, v], m2: Map[k, v]): Map[k, v] = unionWithKey((k, v1, v2) -> v1, m1, m2)

    ///
    /// Returns the union of `m1` and `m2` where key collisions are resolved with the merge function `f`.
    ///
    def unionWith[k, v](f: (v, v) -> v, m1: Map[k, v], m2: Map[k, v]): Map[k, v] = unionWithKey((k, v1, v2) -> f(v1, v2), m1, m2)

    ///
    /// Returns the union of `m1` and `m2` where key collisions are resolved with the merge function `f`, taking both the key and values.
    ///
    def unionWithKey[k, v](f: (k, v, v) -> v, m1: Map[k, v], m2: Map[k, v]): Map[k, v] =
        foldRightWithKey((k, v, m) -> insertWithKey((key, v1, v2) -> f(key, v1, v2), k, v, m) , m2, m1)

    ///
    /// Returns the left-biased intersection of `m1` and `m2`.
    /// That is, key collisions are resolved by taking the mapping from `m1`.
    ///
    def intersection[k, v](m1: Map[k, v], m2: Map[k, v]): Map[k, v] = intersectionWithKey((k, v1, v2) -> v1, m1, m2)

    ///
    /// Returns the intersection of `m1` and `m2` where key collisions are resolved with the merge function `f`.
    ///
    def intersectionWith[k, v](f: (v, v) -> v, m1: Map[k, v], m2: Map[k, v]): Map[k, v] = intersectionWithKey((k, v1, v2) -> f(v1, v2), m1, m2)

    ///
    /// Returns the intersection of `m1` and `m2` where key collisions are resolved with the merge function `f`, taking both the key and values.
    ///
    def intersectionWithKey[k, v](f: (k, v, v) -> v, m1: Map[k, v], m2: Map[k, v]): Map[k, v] =
        let intersect = filterWithKey((k, v) -> memberOf(k, m1), m2);
        foldRightWithKey((k, v, m) -> adjustWithKey((key, v1) -> f(key, v, v1), k, m), intersect, m1)

    ///
    /// Returns the difference of `m1` and `m2`, i.e. `m1 - m2`.
    /// That is, returns the map `m1` with the keys removed that are in `m2`.
    ///
    def difference[k, v](m1: Map[k, v], m2: Map[k, v]): Map[k, v] = differenceWithKey((k, v1, v2) -> None, m1, m2)

    ///
    /// Returns the difference of `m1` and `m2`, i.e. `m1 - m2`.
    /// When a key `k` is in both `m1` and `m2`, the associated values are passed to the merge function `f`.
    /// If `f` returns `None` the mapping with `k` is thrown away (proper set difference).
    /// If `f` returns `Some(v)` the mapping `k -> v` is included in the result.
    ///
    def differenceWith[k, v](f: (v, v) -> Option[v], m1: Map[k, v], m2: Map[k, v]): Map[k, v] = differenceWithKey((k, v1, v2) -> f(v1, v2), m1, m2)

    ///
    /// Returns the difference of `m1` and `m2`, i.e. `m1 - m2`.
    /// When a key `k` is in both `m1` and `m2`, `k` and the associated values are passed to the merge function `f`.
    /// If `f` returns `None` the mapping with `k` is thrown away (proper set difference).
    /// If `f` returns `Some(v)` the mapping `k -> v` is included in the result.
    ///
    def differenceWithKey[k, v](f: (k, v, v) -> Option[v], m1: Map[k, v], m2: Map[k, v]): Map[k, v] =
        let diff = filterWithKey((k, v) -> !memberOf(k, m2), m1);
        let g = (k, v, m) -> if (memberOf(k, m1))
                                let Some(v1) = get(k, m1);
                                match f(k, v1, v) with {
                                    case None => m
                                    case Some(w) => insert(k, w, m)
                                }
                             else m;
        foldRightWithKey(g, diff, m2)

    //
    // ## Map Conversions
    //

    ///
    /// Returns the map `m` as a set of key-value pairs.
    ///
    def toSet[k, v](m: Map[k, v]): Set[(k, v)] =
        let Map(xs) = m;
            List.toSet(xs)

    // TODO:
    //- **toList(m: Map[K, V]): List[(K, V)]**
    //    - Alias for toListAsc.
    //- **toAscList(m: Map[K, V]): List[(K, V)]**
    //    - Returns the map as an association list of pairs ordered by the natural order on the keys in ascending order.
    //- **toDescList(m: Map[K, V]): List[(K, V)]**
    //    - Similar to toAscList, but the keys are ordered in descending order.

    //
    // ## Map Equality
    //

    ///
    /// Returns `true` if and only if `m1` and `m2` and equal, i.e. they have the same mappings.
    ///
    def eq[k, v](m1: Map[k, v], m2: Map[k, v]): Bool = isSubmapOf(m1, m2) && size(m1) == size(m2)
}

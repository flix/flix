///
/// An experimental Map type represented internally as a red-black tree.
///
enum Map[k, v] {
    case Map(RBTree[k, v])
}

namespace Map {

    ///
    /// Returns the size of `m`.
    ///
    pub def size(m: Map[k, v]): Int32 =
        let Map(xs) = m;
        RBTree.size(xs)

    ///
    /// Returns the empty map.
    ///
    /// `Map#{}` is syntactic sugar for `empty` (`Map#{} = empty()`).
    ///
    pub def empty(): Map[k, v] = Map(RBTree.empty())

    ///
    /// Returns the singleton map where key `k` is mapped to value `v`.
    ///
    /// `Map#{k -> v}` is syntactic sugar for `singleton` (`Map#{k -> v} = singleton(k, v)`).
    ///
    pub def singleton(k: k, v: v): Map[k, v] = insert(k, v, empty())

    ///
    /// Returns `true` if and only if `m` is the empty map, i.e. `Map(Nil)`.
    ///
    pub def isEmpty(m: Map[k, v]): Bool =
        let Map(xs) = m;
        RBTree.isEmpty(xs)

    ///
    /// Returns `Some(v)` if `k -> v` is in `m`.
    ///
    /// Otherwise returns `None`.
    ///
    pub def get(k: k, m: Map[k, v]): Option[v] =
        let Map(xs) = m;
        RBTree.findWithKey(k, xs)

    ///
    /// Returns `v` if `k -> v` is in `m`.
    ///
    /// Otherwise, returns `d`.
    ///
    pub def getWithDefault(k: k, d: v, m: Map[k, v]): v = match get(k, m) {
        case None    => d
        case Some(v) => v
    }

    ///
    /// Returns `true` if and only if `m` contains the key `k`.
    ///
    pub def memberOf(k: k, m: Map[k, v]): Bool =
        let Map(xs) = m;
        RBTree.contains(k, xs)

    ///
    /// Returns the keys of `m`.
    ///
    pub def keysOf(m: Map[k, v]): Set[k] = foldLeftWithKey((k, b, _) -> Set.insert(k, b), Set.empty(), m)

    ///
    /// Returns the values of `m`.
    ///
    pub def valuesOf(m: Map[k, v]): List[v] = foldLeftWithKey((k, b, _) -> k :: b, Nil, m)

    ///
    /// Updates `m` with `k -> v`.
    ///
    pub def insert(k: k, v: v, m: Map[k, v]): Map[k, v] = insertWithKey((_, _, v1) -> v1, k, v, m)

    ///
    /// Updates `m` with `k -> f(v, v1)` if `k -> v1` is in `m`.
    ///
    /// Otherwise, updates `m` with `k -> v`.
    ///
    pub def insertWith(f: (v, v) -> v & e, k: k, v: v, m: Map[k, v]): Map[k, v] & e =
        insertWithKey((_, v1, v2) -> f(v1, v2), k, v, m)

    ///
    /// Updates `m` with `k -> f(k, v, v1)` if `k -> v1` is in `m`.
    ///
    /// Otherwise, updates `m` with `k -> v`.
    ///
    pub def insertWithKey(f: (k, v, v) -> v & e, k: k, v: v, m: Map[k, v]): Map[k, v] & e =
        let Map(xs) = m;
        match get(k, m) {
            case None => Map(RBTree.insert(k, v, xs))
            case Some(v1) => Map(RBTree.insert(k, f(k, v, v1), xs))
        }

    ///
    /// Updates `m` with `k -> f(v)` if `k` is in `m`.
    ///
    /// Otherwise, returns `m`.
    ///
    pub def adjust(f: v -> v & e, k: k, m: Map[k, v]): Map[k, v] & e = adjustWithKey((_, v1) -> f(v1), k, m)

    ///
    /// Updates `m` with `k -> f(k, v)` if `k` is in `m`. Otherwise, returns `m`.
    ///
    pub def adjustWithKey(f: (k, v) -> v & e, k: k, m: Map[k, v]): Map[k, v] & e = match get(k, m) {
        case None => m
        case Some(v) => insert(k, f(k, v), m)
    }

    ///
    /// Updates `m` with `k -> v1` if `k` is in `m` and `f(v) = Some(v1)`. Otherwise, returns `m`.
    ///
    pub def update(f: v -> Option[v] & e, k: k, m: Map[k, v]): Map[k, v] & e = updateWithKey((_, v1) -> f(v1), k, m)

    ///
    /// Updates `m` with `k -> v1` if `k` is in `m` and `f(k, v) = Some(v1)`. Otherwise, returns `m`.
    ///
    pub def updateWithKey(f: (k, v) -> Option[v] & e, k: k, m: Map[k, v]): Map[k, v] & e = match get(k, m) {
        case None => m
        case Some(v) => match f(k, v) {
            case None => m
            case Some(v1) => insert(k, v1, m)
        }
    }





    ///
    /// Removes the mapping `k` from the map `m`.
    ///
    pub def delete(k: k, m: Map[k, v]): Map[k, v] =
        let Map(xs) = m;
        Map(RBTree.delete(k, xs))

    ///
    /// Returns `true` if and only if all mappings in `m1` occur in `m2`.
    ///
    pub def isSubmapOf(m1: Map[k, v], m2: Map[k, v]): Bool = forall((k, v) -> match get(k, m2) {
        case None => false
        case Some(v1) => v <=> v1 == 0
    }, m1)

    ///
    /// Returns `true` if and only if all mappings in `m1` occur in `m2` and `m1 != m2`.
    ///
    pub def isProperSubmapOf(m1: Map[k, v], m2: Map[k, v]): Bool = size(m1) != size(m2) && isSubmapOf(m1, m2)

    ///
    /// Alias for `findLeft`.
    ///
    /// The function `f` must be pure.
    ///
    pub def find(f: (k, v) -> Bool, m: Map[k, v]): Option[(k, v)] = findLeft(f, m)

    ///
    /// Optionally returns the first mapping of `m` that satisfies the predicate `f` when searching from left to right.
    ///
    /// The function `f` must be pure.
    ///
    pub def findLeft(f: (k, v) -> Bool, m: Map[k, v]): Option[(k, v)] =
        let Map(xs) = m;
        RBTree.findLeft(f, xs)

    ///
    /// Optionally returns the first mapping of `m` that satisfies the predicate `f` when searching from right to left.
    ///
    /// The function `f` must be pure.
    ///
    pub def findRight(f: (k, v) -> Bool, m: Map[k, v]): Option[(k, v)] =
        let Map(xs) = m;
        RBTree.findRight(f, xs)

    ///
    /// Returns a map of all mappings `k -> v` in `m` where `v` satisfies the predicate `f`.
    ///
    /// The function `f` must be pure.
    ///
    pub def filter(f: v -> Bool, m: Map[k, v]): Map[k, v] = filterWithKey((_, v) -> f(v), m)

    ///
    /// Returns a map of all mappings `k -> v` in `m` where `(k, v)` satisfies the predicate `f`.
    ///
    /// The function `f` must be pure.
    ///
    pub def filterWithKey(f: (k, v) -> Bool, m: Map[k, v]): Map[k, v] =
        foldLeftWithKey((k, b, v) -> if (f(k, v)) insert(k, v, b) else b, empty(), m)

    ///
    /// Returns a map with mappings `k -> f(v)` for every `k -> v` in `m`.
    ///
    pub def map(f: v1 -> v2 & e, m: Map[k, v1]): Map[k, v2] & e = mapWithKey((_, v) -> f(v), m)

    ///
    /// Returns a map with mappings `k -> f(k, v)` for every `k -> v` in `m`.
    ///
    pub def mapWithKey(f: (k, v1) -> v2 & e, m: Map[k, v1]): Map[k, v2] & e =
        foldLeftWithKey((k, b, v) -> insert(k, f(k, v), b), empty(), m)

    ///
    /// Alias for `foldLeft`.
    ///
    pub def fold(f: (b, v) -> b & e, s: b, m: Map[k, v]): b & e = foldLeft(f, s, m)

    ///
    /// Alias for `foldLeftWithKey`.
    ///
    pub def foldWithKey(f: (k, b, v) -> b & e, s: b, m: Map[k, v]): b & e = foldLeftWithKey(f, s, m)

    ///
    /// Applies `f` to a start value `s` and all values in `m` going from left to right.
    ///
    /// That is, the result is of the form: `f(...f(f(s, v1), v2)..., vn)`.
    ///
    pub def foldLeft(f: (b, v) -> b & e, s: b, m: Map[k, v]): b & e = foldLeftWithKey((_, b, v) -> f(b, v), s, m)

    ///
    /// Applies `f` to a start value `s` and all key-value pairs in `m` going from left to right.
    ///
    /// That is, the result is of the form: `f(...f(k2, f(k1, s, v1), v2)..., vn)`.
    ///
    pub def foldLeftWithKey(f: (k, b, v) -> b & e, s: b, m: Map[k, v]): b & e =
        let Map(xs) = m;
        RBTree.foldLeft(f, s, xs)

    ///
    /// Applies `f` to a start value `s` and all values in `m` going from right to left.
    ///
    /// That is, the result is of the form: `f(v1, ...f(vn-1, f(vn, s)))`.
    ///
    pub def foldRight(f: (v, b) -> b & e, s: b, m: Map[k, v]): b & e = foldRightWithKey((_, v, b) -> f(v, b), s, m)

    ///
    /// Applies `f` to a start value `s` and all key-value pairs in `m` going from right to left.
    ///
    /// That is, the result is of the form: `f(k1, v1, ...f(kn-1, vn-1, f(kn, vn, s)))`.
    ///
    pub def foldRightWithKey(f: (k, v, b) -> b & e, s: b, m: Map[k, v]): b & e =
        let Map(xs) = m;
        RBTree.foldRight(f, s, xs)

    ///
    /// Alias for `reduceLeft`.
    ///
    pub def reduce(f: (v, v) -> v & e, m: Map[k, v]): Option[v] & e = reduceLeft(f, m)

    ///
    /// Alias for `reduceLeftWithKey`.
    ///
    pub def reduceWithKey(f: (k, v, k, v) -> (k, v) & e, m: Map[k, v]): Option[(k, v)] = reduceLeftWithKey(f, m)

    ///
    /// Applies `f` to all values in `m` going from left to right until a single value `v` is obtained.  Returns `Some(v)`.
    ///
    /// That is, the result is of the form: `Some(f(...f(f(v1, v2), v3)..., vn))`
    ///
    /// Returns `None` if `m` is the empty Map.
    ///
    pub def reduceLeft(f: (v, v) -> v & e, m: Map[k, v]): Option[v] & e =
        match reduceLeftWithKey((k, v1, _, v2) -> (k, f(v1, v2)), m) {
            case None => None
            case Some(_, v) => Some(v)
        }

    ///
    /// Applies `f` to all mappings in `m` going from left to right until a single mapping `(k, v)` is obtained.  Returns `Some((k, v))`.
    ///
    /// That is, the result is of the form: `Some(f(...f(f(k1, v1, k2, v2), k3, v3)..., kn, vn))`
    ///
    /// Returns `None` if `m` is the empty Map.
    ///
    pub def reduceLeftWithKey(f: (k, v, k, v) -> (k, v) & e, m: Map[k, v]): Option[(k, v)] =
        let Map(xs) = m;
        RBTree.reduceLeft(f, xs)

    ///
    /// Applies `f` to all values in `m` going from right to left until a single value `v` is obtained.  Returns `Some(v)`.
    ///
    /// That is, the result is of the form: `Some(f(v1, ...f(vn-2, f(vn-1, vn))...))`
    ///
    /// Returns `None` if `m` is the empty Map.
    ///
    pub def reduceRight(f: (v, v) -> v & e, m: Map[k, v]): Option[v] & e =
        match reduceRightWithKey((k, v1, _, v2) -> (k, f(v1, v2)), m) {
            case None => None
            case Some((_, v)) => Some(v)
        }


    ///
    /// Applies `f` to all mappings in `m` going from right to left until a single mapping `(k, v)` is obtained.  Returns `Some((k, v))`.
    ///
    /// That is, the result is of the form: `Some(f(k1, v1, ...f(kn-2, vn-2, f(kn-1, vn-1, kn, vn))...))`
    ///
    /// Returns `None` if `m` is the empty Map.
    ///
    pub def reduceRightWithKey(f: (k, v, k, v) -> (k, v) & e, m: Map[k, v]): Option[(k, v)] =
        let Map(xs) = m;
        RBTree.reduceRight(f, xs)

    ///
    /// Returns the number of mappings in `m` that satisfy the predicate `f`.
    ///
    /// The function `f` must be pure.
    ///
    pub def count(f: (k, v) -> Bool, m: Map[k, v]): Int32 =
        foldLeftWithKey((k, b, v) -> if (f(k, v)) 1 + b else b, 0, m)

    ///
    /// Returns `true` if and only if at least one mapping in `m` satisfies the predicate `f`.
    ///
    /// Returns `false` if `m` is the empty Map.
    ///
    /// The function `f` must be pure.
    ///
    pub def exists(f: (k, v) -> Bool, m: Map[k, v]): Bool =
        let Map(xs) = m;
        RBTree.exists(f, xs)

    ///
    /// Returns `true` if and only if all mappings in `m` satisfy the predicate `f`.
    ///
    /// Returns `true` if `m` is the empty Map.
    ///
    /// The function `f` must be pure.
    ///
    pub def forall(f: (k, v) -> Bool, m: Map[k, v]): Bool =
        let Map(xs) = m;
        RBTree.forall(f, xs)

    ///
    /// Returns the left-biased union of `m1` and `m2`.
    ///
    /// That is, key collisions are resolved by taking the mapping from `m1`.
    ///
    pub def union(m1: Map[k, v], m2: Map[k, v]): Map[k, v] = unionWithKey((_, v1, _) -> v1, m1, m2)

    ///
    /// Returns the union of `m1` and `m2` where key collisions are resolved with the merge function `f`.
    ///
    pub def unionWith(f: (v, v) -> v & e, m1: Map[k, v], m2: Map[k, v]): Map[k, v] & e =
        unionWithKey((_, v1, v2) -> f(v1, v2), m1, m2)

    ///
    /// Returns the union of `m1` and `m2` where key collisions are resolved with the merge function `f`, taking both the key and values.
    ///
    pub def unionWithKey(f: (k, v, v) -> v & e, m1: Map[k, v], m2: Map[k, v]): Map[k, v] & e =
        foldRightWithKey((k, v, m) -> insertWithKey((key, v1, v2) -> f(key, v1, v2), k, v, m) , m2, m1)

    ///
    /// Returns the left-biased intersection of `m1` and `m2`.
    ///
    /// That is, key collisions are resolved by taking the mapping from `m1`.
    ///
    pub def intersection(m1: Map[k, v], m2: Map[k, v]): Map[k, v] = intersectionWithKey((_, v1, _) -> v1, m1, m2)

    ///
    /// Returns the intersection of `m1` and `m2` where key collisions are resolved with the merge function `f`.
    ///
    pub def intersectionWith(f: (v, v) -> v & e, m1: Map[k, v], m2: Map[k, v]): Map[k, v] & e =
        intersectionWithKey((_, v1, v2) -> f(v1, v2), m1, m2)

    ///
    /// Returns the intersection of `m1` and `m2` where key collisions are resolved with the merge function `f`, taking both the key and values.
    ///
    pub def intersectionWithKey(f: (k, v, v) -> v & e, m1: Map[k, v], m2: Map[k, v]): Map[k, v] & e =
        let intersect = filterWithKey((k, _) -> memberOf(k, m1), m2);
        foldRightWithKey((k, v, m) -> adjustWithKey((key, v1) -> f(key, v, v1), k, m), intersect, m1)

    ///
    /// Returns the difference of `m1` and `m2`, i.e. `m1 - m2`.
    ///
    /// That is, returns the Map `m1` with the keys removed that are in `m2`.
    ///
    pub def difference(m1: Map[k, v], m2: Map[k, v]): Map[k, v] = differenceWithKey((_, _, _) -> None, m1, m2)

    ///
    /// Returns the difference of `m1` and `m2`, i.e. `m1 - m2`.
    ///
    /// When a key `k` is in both `m1` and `m2`, the associated values are passed to the merge function `f`.
    /// If `f` returns `None` the mapping with `k` is thrown away (proper set difference).
    /// If `f` returns `Some(v)` the mapping `k -> v` is included in the result.
    ///
    pub def differenceWith(f: (v, v) -> Option[v] & e, m1: Map[k, v], m2: Map[k, v]): Map[k, v] & e =
        differenceWithKey((_, v1, v2) -> f(v1, v2), m1, m2)

    ///
    /// Returns the difference of `m1` and `m2`, i.e. `m1 - m2`.
    ///
    /// When a key `k` is in both `m1` and `m2`, `k` and the associated values are passed to the merge function `f`.
    /// If `f` returns `None` the mapping with `k` is thrown away (proper set difference).
    /// If `f` returns `Some(v)` the mapping `k -> v` is included in the result.
    ///
    pub def differenceWithKey(f: (k, v, v) -> Option[v] & e, m1: Map[k, v], m2: Map[k, v]): Map[k, v] & e =
        let diff = filterWithKey((k, _) -> !memberOf(k, m2), m1);
        let g = (k, v, m) -> if (memberOf(k, m1))
                                match get(k, m1) {
                                    case Some(v1) => match f(k, v1, v) {
                                        case None => m
                                        case Some(w) => insert(k, w, m)
                                    }
                                    case None => panic "unreachable"
                                }
                             else m;
        foldRightWithKey(g, diff, m2)





    ///
    /// Returns `m` as a mutable set.
    ///
    pub def toMutable(m: Map[k, v]): MutMap[k, v] & Impure =
        MutMap(ref m)

    ///
    /// Returns the Map `m` as a list of key-value pairs.
    ///
    pub def toList(m: Map[k, v]): List[(k, v)] = foldLeftWithKey((k, b, v) -> (k, v) :: b, Nil, m)

    ///
    /// Returns the Map `m` as a set of key-value pairs.
    ///
    pub def toSet(m: Map[k, v]): Set[(k, v)] = foldLeftWithKey((k, b, v) -> Set.insert((k, v), b), Set.empty(), m)

    ///
    /// Applies `f` to every element of `xs`.
    ///
    /// The function `f` must be impure.
    ///
    pub def foreach(f: (k, v) ~> Unit, m: Map[k, v]): Unit & Impure =
        let Map(xs) = m;
        RBTree.foreach(f, xs)
    ///
    /// Returns `true` if and only if `m1` and `m2` and equal, i.e. they have the same mappings.
    ///
    pub def __eq(m1: Map[k, v], m2: Map[k, v]): Bool = size(m1) == size(m2) && isSubmapOf(m1, m2)

}

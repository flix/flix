namespace Map {

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
    def get[k, v](k: k, m: Map[k, v]): Option[v] = match m with {
        case Map(Nil) => None
        case Map((a, b) :: rs) => if (a == k) Some(b) else get(k, Map(rs))
    }

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
            Set(fst(List.unzip(xs)))

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
        let Map(xs) = m;
            Map(insertHelper(k, v, xs))

    ///
    /// Helper function for `insert`.
    ///
    def insertHelper[k, v](k: k, v: v, xs: List[(k, v)]): List[(k, v)] = match xs with {
        case Nil => (k, v) :: Nil
        case (a, b) :: rs => if (k == a) (k, v) :: rs else (a, b) :: insertHelper(k, v, rs)
    }

    ///
    /// Updates `m` with `k -> f(v, v1)` if `k -> v1` is in `m`.
    /// Otherwise, updates `m` with `k -> v`.
    ///
    def insertWith[k, v](f: (v, v) -> v, k: k, v: v, m: Map[k, v]): Map[k, v] =
        let Map(xs) = m;
            Map(insertWithHelper(f, k, v, xs))

    ///
    /// Helper function for `insertWith`.
    ///
    def insertWithHelper[k, v](f: (v, v) -> v, k: k, v: v, xs: List[(k, v)]): List[(k, v)] = match xs with {
        case Nil => (k, v) :: Nil
        case (a, b) :: rs => if (k == a) (k, f(v, b)) :: rs else (a, b) :: insertWithHelper(f, k, v, rs)
    }

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
        let Map(xs) = m;
            Map(adjustHelper(f, k, xs))

    ///
    /// Helper function for `adjust`.
    ///
    def adjustHelper[k, v](f: v -> v, k: k, xs: List[(k, v)]): List[(k, v)] = match xs with {
        case Nil => Nil
        case (a, b) :: rs => if (k == a) (k, f(b)) :: rs else (a, b) :: adjustHelper(f, k, rs)
    }

    ///
    /// Updates `m` with `k -> f(k, v)` if `k -> v` is in `m`.
    /// Otherwise, returns `m`.
    ///
    def adjustWithKey[k, v](f: (k, v) -> v, k: k, m: Map[k, v]): Map[k, v] =
        let Map(xs) = m;
            Map(adjustWithKeyHelper(f, k, xs))

    ///
    /// Helper function for `adjustWithKey`.
    ///
    def adjustWithKeyHelper[k, v](f: (k, v) -> v, k: k, xs: List[(k, v)]): List[(k, v)] = match xs with {
        case Nil => Nil
        case (a, b) :: rs => if (k == a) (k, f(k, b)) :: rs else (a, b) :: adjustWithKeyHelper(f, k, rs)
    }

    ///
    /// Updates `m` with `k -> v1` if `k -> v` is in `m` and `f(v) = Some(v1).
    /// Otherwise, returns `m`.
    ///
    def update[k, v](f: v -> Option[v], k: k, m: Map[k, v]): Map[k, v] =
        let Map(xs) = m;
            Map(updateHelper(f, k, xs))

    ///
    /// Helper function for `update`.
    ///
    def updateHelper[k, v](f: v -> Option[v], k: k, xs: List[(k, v)]): List[(k, v)] = match xs with {
        case Nil => Nil
        case (a, b) :: rs => if (k == a) (k, Option.getWithDefault(f(b), b)) :: rs else (a, b) :: updateHelper(f, k, rs)
    }

    ///
    /// Updates `m` with `k -> v1` if `k -> v` is in `m` and `f(k, v) = Some(v1).
    /// Otherwise, returns `m`.
    ///
    def updateWithKey[k, v](f: (k, v) -> Option[v], k: k, m: Map[k, v]): Map[k, v] =
        let Map(xs) = m;
            Map(updateWithKeyHelper(f, k, xs))

    ///
    /// Helper function for `updateWithKey`.
    ///
    def updateWithKeyHelper[k, v](f: (k, v) -> Option[v], k: k, xs: List[(k, v)]): List[(k, v)] = match xs with {
        case Nil => Nil
        case (a, b) :: rs => if (k == a) (k, Option.getWithDefault(f(k, b), b)) :: rs else (a, b) :: updateWithKeyHelper(f, k, rs)
    }

    //
    // ## Delete
    //

    ///
    ///
    ///
    def delete[k, v](k: k, m: Map[k, v]): Map[k, v] =
        let Map(xs) = m;
            Map(deleteHelper(k, xs))

    ///
    /// Helper function for `delete`.
    ///
    def deleteHelper[k, v](k: k, xs: List[(k, v)]): List[(k, v)] = match xs with {
        case Nil => Nil
        case (a, b) :: rs => if (k == a) rs else (a, b) :: deleteHelper(k, rs)
    }

  //## Map Predicates
  //- **isSubmapOf(m1: Map[K, V], m2: Map[K, V]): Map[K, V]**
  //    - Returns true if all (key, value) mappings in m1 occur in m2.
  //- **isProperSubmapOf(m1: Map[K, V], m2: Map[K, V]): Map[K, V]**
  //    - Returns true if all (key, value) mappings in m1 occur in m2 and m1 is not equal to m2.
  //
  //## Map Transformation
   //- **filter(f: V => Bool, m: Map[K, V]): Map[K, V]**
   //    - Returns a map built from m where the values satisfy the predicate f.
   //- **filterWithKey(f: (K, V) => Bool, m: Map[K, V]): Map[K, V]**
   //    - Similar to filter but also passes the key to the predicate function.
   //- **map(f: A => B, m: Map[K, A]): Map[K, B]**
   //    - Returns a map with the function f applied to all values in m.
   //- **mapWithKey(f: (K, A) => B, m: Map[K, A]): Map[K, B]**
   //    - Similar to map but also passes the key to the map function.
   //
   //## Fold
    //- **fold(f: (B, A) => B, b: B, m: Map[K, A]): B**
    //    - Alias for foldLeft.
    //- **foldWithKey(f: (K, B, A) => B, b: B, m: Map[K, A]): B**
    //    - Alias for foldLeftWithKey.
    //- **foldLeft(f: (B, A) => B, b: B, m: Map[K, A]): B**
    //    - Left fold of the values in the map m with the fold function f.
    //- **foldWithKey(f: (K, B, A) => B, b: B, m: Map[K, A]): B**
    //    - Similar to foldLeft but passes the key to the fold function.
    //- **foldRight(f: (A, B) => B, b: B, m: Map[K, A]): B**
    //    - Right fold of the values in the map m with the fold function f.
    //- **foldRightWithKey(f: (K, A, B) => B, b: B, m: Map[K, A]): B**
    //    - Similar to foldRight but passes the key to the fold function.
    //

    //## Combine Two Maps
     //- **union(m1: Map[K, V], m2: Map[K, V]): Map[K, V]**
     //    - Returns the left-biased union of m1 and m2. That is, a map with all mappings taken from m1 and m2 and where key collisions are resolved by always taking the mapping from m1.
     //- **unionWith(f: (V, V) => V, m1: Map[K, V], m2: Map[K, V]): Map[K, V]**
     //    - Returns the union of the maps m1 and m2 where key collisions are resolved by the merge function f.
     //- **unionWithKey(f: (K, V, V) => V, m1: Map[K, V], m2: Map[K, V]): Map[K, V]**
     //    - Similar to unionWith but also passes the key to the merge function.
     //- **intersection(m1: Map[K, V], m2: Map[K, V]): Map[K, V]**
     //    - Returns the left-biased intersection of m1 and m2. That is, a map with all mappings taken from m1 where the keys also exist in m2.
     //- **intersectionWith(f: (V, V) => V,m1: Map[K, V], m2: Map[K, V]): Map[K, V]**
     //    - Returns the intersection of m1 and m2 using the merge function f to combine values.
     //- **intersectionWithKey(f: (K, V, V) => V, m1: Map[K, V], m2: Map[K, V]): Map[K, V]**
     //    - Similar to intersectionWith but also passes the key to the merge function.
     //- **difference(m1: Map[K, V], m2: Map[K, V]): Map[K, V]**
     //    - Returns the map m1 with the keys removed that are in m2.
     //- **differenceWith(f: (V, V) => Opt[V], m1: Map[K, V], m2: Map[K, V]): Map[K, V]**
     //    - Returns a map with the keys (and values) from m1 that are not in m2. When a key is in both m1 and m2 they are passed to the merge function. If the function returns None the mapping is thrown away.
     //- **differenceWithKey(f: (K, V, V) => Opt[V], m1: Map[K, V], m2: Map[K, V]): Map[K, V]**
     //    - Similar to differenceWith but also pases the key to the merge function.
     
     //## Map Conversions
      //- **toList(m: Map[K, V]): List[(K, V)]**
      //    - Alias for toListAsc.
      //- **toAscList(m: Map[K, V]): List[(K, V)]**
      //    - Returns the map as an association list of pairs ordered by the natural order on the keys in ascending order.
      //- **toDescList(m: Map[K, V]): List[(K, V)]**
      //    - Similar to toAscList, but the keys are ordered in descending order.
      //- **toSet(m: Map[K, V]): Set[(K, V)]**
      //    - Returns the map as a set of pairs.
}

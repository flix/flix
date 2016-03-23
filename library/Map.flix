//# Map
//
//## Map Construction
//- **empty: Map[K, V]**
//    - The empty map.
//- **singleton(k: K, v: V): Map[K, V]**
//    - The singleton map where the key k is mapped to the value v.
//
//## Basic Operations
//- **null(m: Map[K, V]): Bool**
//    - Returns true iff m is the empty map.
//- **get(k: K, m: Map[K, V]): Opt[V]**
//    - Optionally returns the value the key k is mapped to in the map m.
//    - Returns None if m has no mapping for k.
//- **getWithDefault(k: K, v: V, m: Map[K, V]): V**
//    - Returns the value the key k is mapped to in the map m. Returns v if the key has no mapping.
//- **memberOf(k: K, m: Map[K, V]): Bool**
//    - Returns true iff k is key of the map m.
//- **keysOf(m: Map[K, V]): Set[K]**
//    - Returns the key of the map.
//- **valuesOf(m: Map[K, V]): List[V]**
//    - Returns the values of the map.
//
//## Insert
//- **insert(k: K, v: V, m: Map[K, V]): Map[K, V]**
//    - Returns the map m updated with the key k mapped to the value v.
//- **insertWith(f: (V, V) => V, k: K, v: V, m: Map[K, V]): Map[K, V]**
//    - Returns the map m updated with the key k mapped to the value v. If the key already exists in the map and is mapped to v2, then the map is updated with the value of f(v, v2).
//- **insertWithKey(f: (K, V, V) => V, k: K, v: V, m: Map[K, V]): Map[K, V]**
//    - Returns the map m updated with the key k mapped to the value v. If the key already exists in the map and is mapped to v2, then the map is updated with the value of f(k, v, v2).
//
//## Update
//- **adjust(f: V => V, k: K, m: Map[K, V]): Map[K, V]**
//    - Returns m adjusted with f.
//    - Returns the original map if the key k is not in the map.
//- **adjustWithKey(f: (K, V) => V, k: K, m: Map[K, V]): Map[K, V]**
//    - Similar to adjust but also passes the key to the function.
//- **update(f: V => Opt[V], k: K, m: Map[K, V]): Map[K, V]**
//    - Updates the map m with a new mapping if f(v) returns Some.
//- **updateWithKey(f: V => Opt[V], k: K, m: Map[K, V]): Map[K, V]**
//    - Similar to update but also passes the key to the function.
//
//## Delete
//- **delete(k: K, m: Map[K, V]): Map[K, V]**
//- Deletes the mapping for the key k in the map m.
//
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
//
//## Map Conversions
//- **toList(m: Map[K, V]): List[(K, V)]**
//    - Alias for toListAsc.
//- **toAscList(m: Map[K, V]): List[(K, V)]**
//    - Returns the map as an association list of pairs ordered by the natural order on the keys in ascending order.
//- **toDescList(m: Map[K, V]): List[(K, V)]**
//    - Similar to toAscList, but the keys are ordered in descending order.
//- **toSet(m: Map[K, V]): Set[(K, V)]**
//    - Returns the map as a set of pairs.
//
//## Order and Lattice Operations
//- **join(m1: Map[K, V], m2: Map[K, V]): Map[K, V]**
//    - Returns the least upper bound of the two maps.
//- **meet(m1: Map[K, V], m2: Map[K, V]): Map[K, V]**
//    - Returns the greatest upper bound of the two maps.

///
/// Portable `BPlusTree` overlay.
///
/// This implementation preserves the public semantics used by `Fixpoint3`, but it is
/// intentionally single-threaded and uses a simple sorted mutable list internally.
///
pub struct BPlusTree[k, v, r] {
    rc: Region[r],
    arity: Int32,
    search: BPlusTree.Search,
    mut entries: MutList[(k, v), r]
}

instance ForEach[BPlusTree[k, v, r]] {
    type Elm = (k, v)
    type Aef = r
    pub def forEach(f: ((k, v)) -> Unit \ ef, m: BPlusTree[k, v, r]): Unit \ ef + r =
        BPlusTree.forEach(k -> v -> f((k, v)), m)
}

pub mod BPlusTree {

    pub type alias Search = Vector[Int32]

    def defaultSearch(): Search = Vector#{-1}

    pub def empty(rc: Region[r]): BPlusTree[k, v, r] \ r =
        emptyWithSearch(rc, defaultSearch())

    pub def emptyWithArity(rc: Region[r], m: Int32): BPlusTree[k, v, r] \ r =
        emptyWithArityAndSearch(rc, Int32.max(m, 3), defaultSearch())

    pub def emptyWithArityAndSearch(rc: Region[r], m: Int32, search: Search): BPlusTree[k, v, r] \ r =
        new BPlusTree @ rc {
            rc = rc,
            arity = Int32.max(m, 3),
            search = search,
            entries = MutList.empty(rc)
        }

    pub def emptyWithSearch(rc: Region[r], search: Search): BPlusTree[k, v, r] \ r =
        emptyWithArityAndSearch(rc, 64, search)

    pub def arity(t: BPlusTree[k, v, r]): Int32 = t->arity

    pub def rc(t: BPlusTree[k, v, r]): Region[r] = t->rc

    pub def search(t: BPlusTree[k, v, r]): Search = t->search

    pub def size(t: BPlusTree[k, v, r]): Int32 \ r = MutList.size(t->entries)

    pub def isEmpty(t: BPlusTree[k, v, r]): Bool \ r = MutList.isEmpty(t->entries)

    pub def nonEmpty(t: BPlusTree[k, v, r]): Bool \ r = not isEmpty(t)

    pub def toList(t: BPlusTree[k, v, r]): List[(k, v)] \ r =
        MutList.toList(t->entries)

    pub def toVector(t: BPlusTree[k, v, r]): Vector[(k, v)] \ r =
        MutList.toVector(t->entries)

    pub def minimumKey(t: BPlusTree[k, v, r]): Option[(k, v)] \ r =
        MutList.head(t->entries)

    pub def forEach(f: k -> v -> Unit \ ef, t: BPlusTree[k, v, r]): Unit \ ef + r =
        let entries = t->entries;
        def loop(i: Int32): Unit \ ef + r =
            match MutList.nth(i, entries) {
                case None => ()
                case Some((k, v)) =>
                    f(k, v);
                    loop(i + 1)
            };
        loop(0)

    @Parallel
    pub def parForEach(f: k -> v -> Unit \ r, t: BPlusTree[k, v, r]): Unit \ r =
        forEach(f, t)

    @Parallel
    pub def parForEachWhen(f1: k -> v -> Unit \ r, f2: k -> v -> Unit \ r, parLimit: Int32, t: BPlusTree[k, v, r]): Unit \ r =
        if (size(t) < parLimit) forEach(f1, t) else forEach(f2, t)

    pub def get(k: k, t: BPlusTree[k, v, r]): Option[v] \ r with Order[k] =
        match findIndex(k, t) {
            case Some(i) => MutList.nth(i, t->entries) |> Option.map(match (_, v) -> v)
            case None => None
        }

    pub def getKeyAndValue(k: k, t: BPlusTree[k, v, r]): Option[(k, v)] \ r with Order[k] =
        match findIndex(k, t) {
            case Some(i) => MutList.nth(i, t->entries)
            case None => None
        }

    pub def getWithDefault(k: k, d: v, t: BPlusTree[k, v, r]): v \ r with Order[k] =
        match get(k, t) {
            case Some(v) => v
            case None => d
        }

    pub def memberOf(k: k, t: BPlusTree[k, v, r]): Bool \ r with Order[k] =
        match findIndex(k, t) {
            case Some(_) => true
            case None => false
        }

    pub def memberOfPair(k: k, v: v, t: BPlusTree[k, v, r]): Bool \ r with Order[k], Eq[v] =
        match get(k, t) {
            case Some(v0) => v0 == v
            case None => false
        }

    pub def put(k: k, v: v, t: BPlusTree[k, v, r]): Unit \ r with Order[k] =
        match findIndexOrInsertion(k, t) {
            case (true, i) =>
                MutList.remove(i, t->entries);
                MutList.insert((k, v), i, t->entries)
            case (false, i) => MutList.insert((k, v), i, t->entries)
        }

    pub def putIf(decider: k -> v -> k -> v -> Bool, k: k, v: v, t: BPlusTree[k, v, r]): Unit \ r with Order[k] =
        match findIndexOrInsertion(k, t) {
            case (true, i) =>
                match MutList.nth(i, t->entries) {
                    case Some((k0, v0)) if decider(k, v, k0, v0) =>
                        MutList.remove(i, t->entries);
                        MutList.insert((k, v), i, t->entries)
                    case _ => ()
                }
            case (false, i) =>
                MutList.insert((k, v), i, t->entries)
        }

    pub def putWith(f: v -> v -> v \ ef, k: k, v: v, t: BPlusTree[k, v, r]): Unit \ ef + r with Order[k] =
        match findIndexOrInsertion(k, t) {
            case (true, i) =>
                match MutList.nth(i, t->entries) {
                    case Some((_, v0)) =>
                        MutList.remove(i, t->entries);
                        MutList.insert((k, f(v, v0)), i, t->entries)
                    case None => unreachable!()
                }
            case (false, i) =>
                MutList.insert((k, v), i, t->entries)
        }

    pub def computeIfAbsent(f: Unit -> v \ ef, k: k, t: BPlusTree[k, v, r]): v \ ef + r with Order[k] =
        match findIndexOrInsertion(k, t) {
            case (true, i) =>
                match MutList.nth(i, t->entries) {
                    case Some((_, v)) => v
                    case None => unreachable!()
                }
            case (false, i) =>
                let v = f();
                MutList.insert((k, v), i, t->entries);
                v
        }

    pub def merge(src: BPlusTree[k, v, r], dst: BPlusTree[k, v, r]): Unit \ r with Order[k] =
        forEach(k -> v -> put(k, v, dst), src)

    pub def mergeWith(f: v -> v -> v \ ef, src: BPlusTree[k, v, r], dst: BPlusTree[k, v, r]): Unit \ r + ef with Order[k] =
        forEach(k -> v -> putWith(f, k, v, dst), src)

    pub def rangeQueryWith(f: k -> v -> Unit \ ef, min: k, max: k, t: BPlusTree[k, v, r]): Unit \ r + ef with Order[k] =
        let entries = t->entries;
        let s = t->search;
        def loop(i: Int32): Unit \ ef + r =
            match MutList.nth(i, entries) {
                case None => ()
                case Some((k, v)) =>
                    if (compareKeys(k, max, s) > 0) {
                        ()
                    } else {
                        if (compareKeys(k, min, s) >= 0) {
                            f(k, v)
                        } else {
                            ()
                        };
                        loop(i + 1)
                    }
            };
        loop(0)

    pub def joinWith(f: k -> v -> String \ ef, sep: String, t: BPlusTree[k, v, r]): String \ r + ef =
        toList(t)
            |> List.map(match (k, v) -> f(k, v))
            |> List.join(sep)

    pub def joinKeys(sep: String, t: BPlusTree[k, v, r]): String \ r with ToString[k] =
        joinWith(k -> _ -> ToString.toString(k), sep, t)

    pub def joinValues(sep: String, t: BPlusTree[k, v, r]): String \ r with ToString[v] =
        joinWith(_ -> v -> ToString.toString(v), sep, t)

    pub def toString(t: BPlusTree[k, v, r]): String \ r with ToString[k], ToString[v] =
        let searchStr = if (isDefaultSearch(t->search)) "<default>" else Vector.join(" < ", t->search);
        "Search: ${searchStr}\nTree:\n${joinWith(k -> v -> "${k}: ${v}", "\n", t)}"

    def findIndex(k: k, t: BPlusTree[k, v, r]): Option[Int32] \ r with Order[k] =
        match findIndexOrInsertion(k, t) {
            case (true, i) => Some(i)
            case (false, _) => None
        }

    def findIndexOrInsertion(k: k, t: BPlusTree[k, v, r]): (Bool, Int32) \ r with Order[k] =
        let entries = t->entries;
        let s = t->search;
        def loop(i: Int32): (Bool, Int32) \ r =
            match MutList.nth(i, entries) {
                case None => (false, i)
                case Some((k0, _)) =>
                    let cmp = compareKeys(k, k0, s);
                    if (cmp == 0) (true, i)
                    else if (cmp < 0) (false, i)
                    else loop(i + 1)
            };
        loop(0)

    def compareKeys(k1: k, k2: k, s: Search): Int32 with Order[k] =
        if (isDefaultSearch(s)) {
            match k1 <=> k2 {
                case Comparison.LessThan => -1
                case Comparison.EqualTo => 0
                case Comparison.GreaterThan => 1
            }
        } else {
            compareVectors(
                unchecked_cast(k1 as Vector[Int64]),
                unchecked_cast(k2 as Vector[Int64]),
                0,
                s
            )
        }

    def isDefaultSearch(s: Search): Bool =
        match Vector.head(s) {
            case Some(-1) => true
            case _ => false
        }

    def compareVectors(v1: Vector[Int64], v2: Vector[Int64], i: Int32, s: Search): Int32 =
        if (i >= Vector.length(s)) {
            0
        } else {
            let idx = Vector.get(i, s);
            let x1 = Vector.get(idx, v1);
            let x2 = Vector.get(idx, v2);
            if (x1 < x2) -1
            else if (x1 > x2) 1
            else compareVectors(v1, v2, i + 1, s)
        }

}

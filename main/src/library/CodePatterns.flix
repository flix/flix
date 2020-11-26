namespace Option {

    @lint
    def isEmptyCount(): Bool =
      forall [a] (o: Option[a], f: a -> Bool).
        Option.isEmpty(o) `implies` (Option.count(f, o) == 0)

    @lint
    def isEmptyExists(): Bool =
      forall [a] (o: Option[a], f: a -> Bool).
        Option.isEmpty(o) `implies` (Option.exists(f, o) == false)

    @lint
    def isEmptyFilter(): Bool =
      forall [a] (o: Option[a], f: a -> Bool).
        Option.isEmpty(o) `implies` (Option.filter(f, o) == None)

    @lint
    def isEmptyFind(): Bool =
      forall [a] (o: Option[a], f: a -> Bool).
        Option.isEmpty(o) `implies` (Option.find(f, o) == None)

    @lint
    def isEmptyFlatMap(): Bool =
      forall [a] (o: Option[a], f: a -> Option[a]).
        Option.isEmpty(o) `implies` (Option.flatMap(f, o) == None)

    @lint
    def isEmptyFlatMap2a(): Bool =
      forall [a, b] (o1: Option[a], o2: Option[a], f: (a, b) -> Option[a]).
        Option.isEmpty(o1) `implies` (Option.flatMap2(f, o1, o2) == None)

    @lint
    def isEmptyFlatMap2b(): Bool =
      forall [a, b] (o1: Option[a], o2: Option[a], f: (a, b) -> Option[a]).
        Option.isEmpty(o2) `implies` (Option.flatMap2(f, o1, o2) == None)

    @lint
    def isEmptyFlatten(): Bool =
      forall [a] (o: Option[Option[a]]).
        Option.isEmpty(o) `implies` (Option.flatten(o) == None)

    @lint
    def isEmptyFoldLeft(): Bool =
      forall [a, b] (o: Option[a], f: (b, a) -> b, i: b).
        Option.isEmpty(o) `implies` (Option.foldLeft(f, i, o) == i)

//    @lint
//    def isEmptyFoldRight(): Bool =
//      forall [a, b] (o: Option[a], f: (a, b) -> b, i: b).
//        Option.isEmpty(o) `implies` (Option.foldLeft(f, o, i) == i)

    @lint
    def isEmptyForall(): Bool =
      forall [a] (o: Option[a], f: a -> Bool).
        Option.isEmpty(o) `implies` (Option.forall(f, o) == true)

    @lint
    def isEmptyGetWithDefault(): Bool =
      forall [a] (o: Option[a], d: a).
        Option.isEmpty(o) `implies` (Option.getWithDefault(o, d) == d)

    @lint
    def isEmptyIsEmpty(): Bool =
      forall [a] (o: Option[a]).
        Option.isEmpty(o) `implies` (Option.isEmpty(o) == true)

    @lint
    def isEmptyMap(): Bool =
      forall [a, b] (o: Option[a], f: a -> b).
        Option.isEmpty(o) `implies` (Option.map(f, o) == None)

    @lint
    def isEmptyMap2a(): Bool =
      forall [a, b, c] (o1: Option[a], o2: Option[b], f: (a, b) -> c).
        Option.isEmpty(o1) `implies` (Option.map2(f, o1, o2) == None)

    @lint
    def isEmptyMap2b(): Bool =
      forall [a, b, c] (o1: Option[a], o2: Option[b], f: (a, b) -> c).
        Option.isEmpty(o2) `implies` (Option.map2(f, o1, o2) == None)

    @lint
    def isEmptyReplace(): Bool =
      forall [a, b] (o: Option[a], x: a, y: b).
        Option.isEmpty(o) `implies` (Option.replace(x, y, o) == None)

    @lint
    def isEmptyUnzip(): Bool =
      forall [a, b] (o: Option[(a, b)]).
        Option.isEmpty(o) `implies` (Option.unzip(o) == (None, None))

    @lint
    def isEmptyWithDefault(): Bool =
      forall [a] (o1: Option[a], o2: Option[b]).
        Option.isEmpty(o1) `implies` (Option.withDefault(o1, o2) == o2)

    @lint
    def isEmptyZip1(): Bool =
      forall [a] (o1: Option[a], o2: Option[b]).
        Option.isEmpty(o1) `implies` (Option.zip(o1, o2) == None)

    @lint
    def isEmptyZip2(): Bool =
      forall [a] (o1: Option[a], o2: Option[b]).
        Option.isEmpty(o2) `implies` (Option.zip(o1, o2) == None)

    @lint
    def existsIsEmpty(): Bool =
      forall [a] (f: a -> Bool, o: Option[a]).
        Option.exists(f, o) `implies` (Option.isEmpty(o) == false)

}

namespace List {

    // TODO: Bug: Why does this not type check?
    //@lint
    //def mapZip2(): Bool =
    //  forall [a, b, c] (f: (a, b) -> c, xs: List[a], ys: List[b]).
    //    List.map(f, List.zip(xs, ys)) == List.zipWith(f, xs, ys)

    @lint
    def memberOfIsEmpty(): Bool =
      forall [a] (x: a, xs: List[a]).
        List.memberOf(x, xs) `implies` (List.isEmpty(xs) == false)      // TODO: Use eq or not?

    @lint
    def memberOfReplace(): Bool =
        forall [a] (x: a, y: a, xs: List[a]).
            not List.memberOf(x, xs) `implies` (List.replace(x, y, xs) == xs)      // TODO: What about negation here?

    @lint
    def toSetFlatten(): Bool =
        forall [a] (xs: List[Set[a]]).
            Set.flatten(List.toSet(xs)) === List.foldLeft((x, y) -> Set.union(x, y), Set.empty(), xs)

    @lint
    def foldConjFalse(): Bool =
        forall [a] (xs: List[a]).
            List.foldLeft((x, y) -> x and y, false, xs) === false

    @lint
    def foldDisjTrue(): Bool =
        forall [a] (xs: List[a]).
            List.foldLeft((x, y) -> x or y, true, xs) === true

    @lint
    def foldForall(): Bool =
        forall [a] (f: x -> Bool, xs: List[a]).
            List.foldLeft((x, y) -> x and y, true, List.map(f, xs)) === List.forall(f, xs)

    @lint
    def foldExists(): Bool =
        forall [a] (f: x -> Bool, xs: List[a]).
            List.foldLeft((x, y) -> x or y, false, List.map(f, xs)) === List.exists(f, xs)

    @lint
    def mapAndToSet(): Bool =
        forall [a, b] (f: a -> b, xs: List[a]).
            List.toSet(List.map(f, xs)) == List.foldLeft((x, y) -> Set.insert(f(y), x), Set.empty(), xs)

    @lint
    def mapAndFlatten(): Bool =
        forall [a, b] (f: a -> List[b], xs: List[List[a]]).
            List.flatten(List.map(f, xs)) == List.flatMap(f, xs)

    // TODO: filterMap, findMap, map and fold.

    // TODO: fst(List.unzip(xs)) => // then typically both project should be used.
    // TODO: let x = List.unzip(xs); fst(x) // these should be the only usages of x, if it escapes then that is okay.

    // TODO: Do we want some first-class notion of "belief". Otherwise we have to write a million variants about when a list is empty then ...

// TODO: Compiler error.
//    @lint
//    def useToMap(): Bool =
//        forall [a, b] (xs: List[(a, b)]).
//            List.foldLeft(match (acc, (k, v)) -> Map.insert(k, v, acc), Map.empty(), xs) == List.toMap(xs)

}


///
/// Set Patterns.
///
namespace Set {

    // TODO: contains, add

    @lint
    def memberOfInsert(): Bool =
        forall [a] (s: Set[a], x: a).
            Set.memberOf(x, s) `implies` (Set.insert(x, s) == s)

}


///
/// Map Patterns.
///
namespace Map {

    ///
    /// Simplifications.
    ///
    @lint
    def keysOfMemberOf(): Bool =
        forall [k] (k: k, m: Map[k, v]).
            Set.memberOf(k, Map.keysOf(m)) == Map.memberOf(k, m)

    ///
    /// Use Cases.
    ///

    @lint
    def useValuesOf(): Bool =
        forall [k, v] (m: Map[k, v]).
            Map.foldLeft((acc, v) -> v :: acc, Nil, m) == Map.valuesOf(m) // TODO: foldLeft or foldRight?

    ///
    /// Beliefs.
    ///


    // TODO: size etc.
    // TODOL Union and fold.

    // TODO: How can we support pattern that detects a use of Map.filterWithKey where the key is unused?
    // TODO: @lint
    // TODO: def filterWithKeyNoKey(): Bool =
    // TODO:    forall [k, v](f: (k, v) -> Bool, m: Map[k, v]).
    // TODO:        Map.filterWithKey((_, v) -> ???) -> Map.filter(???)

}


///
/// Vector Patterns.
///
namespace Vec {

    @lint
    def clearPop(): Bool =
        forall [a] (v: mut Vec[a]).
            Vec.clear(v) `impliesWith` (Vec.pop(v) === None)

    @lint
    def newPop(): Bool =
        Vec.pop(Vec.new()) == None // TODO: How to express?

    @lint
    def pushClear(): Bool =
        forall [a] (v: mut Vec[a], x: a).
            Vec.push(v, x) `uselessLeft` Vec.clear(v) == ()

    @lint
    def isEmptyMemberOf(): Bool =
        forall [a] (v: mut Vec[a], x: a).
            Vec.isEmpty(v) `implies` Vec.memberOf(x, v) == false

    @lint
    def zeroLengthIsEmpty(): Bool =
        forall [a] (v: mut Vec[a]).
            (Vec.length(v) == 0) `implies` Vec.isEmpty(v) // TODO: What about its negation?

    // TODO: What about the dual?
    @lint
    def popNoneIsEmpty(): Bool =
        forall [a] (v: mut Vec[a]).
            (Vec.pop(v) == None) `implies` Vec.isEmpty(v)

    @lint
    def insertAtLength(): Bool =
        forall [a] (v: mut Vec[a], x: a).
            Vec.insert(v, x, Vec.length(v)) == Vec.push(v, x)

//    @lint
//    def removeAtLength(): Bool =
//        forall [a] (v: mut Vec[a]).
//            Vec.remove(v, Vec.length(v)) === Vec.pop(v)

    @lint
    def appendLeftEmpty(): Bool =
        forall [a] (v: mut Vec[a]).
            Vec.append(Vec.new(), v) == ??? // TODO: How to express that this is useless, when append does not return?

    @lint
    def appendRightEmpty(): Bool =
        forall [a] (v: mut Vec[a]).
            Vec.append(v, Vec.new()) == ??? // TODO: How to express that this is useless, when append does not return?

//    @lint
//    def newFollowedByReserve(): Bool =
//        forall (n: Int32).
//            Vec.reserve(Vec.new(), n) == Vec.newWithCapacity(n)

    // TODO: removeAll?
    //    @lint
    //    def retainVecNew(): Bool =
    //        forall [a] (v: mut Vec[a]).
    //            Vec.retain(v, Vec.new()) == v

    // TODO: v.push(x) -> v.pop == Some(x)
    // TODO: replace, contains
    // TODO: truncate(zero)
    // TODO: new followed by reserve -> newWithCapacity
    // TODO: head == none -> not exists, etc.
    // TODO: last = none -> not exists, isEmpty, etc.
    // TODO: find == some -> nonEmpty
    // TODO: ditty for indexOf
    // TODO: push -> nonEmpty

    // TODO: Meta properties?

}


// TODO: How to define this extra linguistic predicate?
pub def uselessLeft(x: a, y: b): b = ???

// TODO: How to define this extra linguistic predicate?
pub def implies(x: Bool, y: Bool): Bool = ???

// TODO: How to define this extra linguistic predicate?
pub def impliesWith(x: a, y: b): Bool = ??? // TODO: Add argument

    // TODO: Changing an if to an expression change semantics to be eager.


// TODO: min, max?

// TODO: List.flatMap(...).toSet -> replace by fold.

// TODO: How should we attach some documentation?
// TODO: What should the naming convention be?
// TODO: functions to add: List.sum, List.catMaybes , mapMaybe , traverse? sequence?
/// TODO: Introduce an annotation @unsafe or @unchecked to allow an expression that matches a code pattern.
/// TODO: It would be nice if we could use |> in patterns.
/// TODO: Do we want to normalize the AST somehow? For example if (not c) e1 else e2 could become if (c) e2 else e1?
// TODO: Add more arithmetic identities with shift and less commonly used operations.

///
/// TODO: Currently not possible to express:
///

/// x != 'a' or x != 'b' === false      // Problem: x is universally quantified, so are 'a' and 'b' but they must be distinct.
/// x == x or x != x                    // Problem: x is a variable, not a constant.
/// (x + 1) / (x + 1)                   // Problem: x is a variable, not a constant.
/// a[x] = 1; a[x] = 1                  // Problem: x is a variable and the array should not be accessed in between. Maybe too hard?

// TODO MultipMap, MultiSet, EqualityRelation?

// TODO: Add sequence and traverse to Result.
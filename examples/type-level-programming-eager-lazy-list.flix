/// A type that allows reflection on a type-level Boolean.
enum BoolProxy[_: Bool] {
    case BoolProxy
}

/// A lazy-eager list. The type-level Bool determines if it is eager.
enum LEList[a: Type, e: Bool] {
    case Nil
    case Cons(a, Lazy[LEList[a, e]])
}

type alias EagerEval = true
type alias LazyEval = false

/// An eager zip function.
def zipE(l1: LEList[a, e1], l2: LEList[b, e2]): LEList[(a, b), EagerEval] =
    match (l1, l2) {
        case (LEList.Nil, _) => LEList.Nil
        case (_, LEList.Nil) => LEList.Nil
        case (LEList.Cons(x, xs), LEList.Cons(y, ys)) =>
            // Force evaluation of the tail.
            let tl = zipE(force xs, force ys);
            LEList.Cons((x, y), lazy tl)
    }

/// A lazy zip function.
def zipL(l1: LEList[a, e1], l2: LEList[b, e2]): LEList[(a, b), LazyEval] =
    match (l1, l2) {
        case (LEList.Nil, _) => LEList.Nil
        case (_, LEList.Nil) => LEList.Nil
        case (LEList.Cons(x, xs), LEList.Cons(y, ys)) =>
            LEList.Cons((x, y), lazy zipL(force xs, force ys))
    }

/// A hybrid zip function which is eager if both arguments are eager.
/// If either argument (or both) is lazy then the list is lazy.
def zip(l1: LEList[a, e1], l2: LEList[b, e2]): LEList[(a, b), e1 and e2] =
    typematch (BoolProxy.BoolProxy : BoolProxy[e1 and e2]) {
        case _: (BoolProxy[true]) =>
            unchecked_cast(zipE(l1, l2) as LEList[(a, b), e1 and e2])
        case _: _                 =>
            unchecked_cast(zipL(l1, l2) as LEList[(a, b), e1 and e2])
    }

/// We can use type-level booleans to track whether a list can be empty.
enum ENList[a: Type, _e: Bool] {
    case Nil,
    case Cons(a, ENList[a, true])
}

/// Two type aliases that we will use to represent emptiness.
type alias Empty    = true
type alias NonEmpty = false

/// Creates an empty list of type `a`.
def _nil(): ENList[a, Empty] = ENList.Nil

/// Returns a nonempty list with a newly attached element.
def _cons(x: a, xs: ENList[a, e]): ENList[a, NonEmpty] =
    ENList.Cons(x, unchecked_cast(xs as ENList[a, true]))

/// Returns the head of the list. Note that it is a type error to call this
/// function on an empty list.
def _head(l: ENList[a, NonEmpty]): a = match l {
    case ENList.Nil        => bug!("Impossible")
    case ENList.Cons(x, _) => x
}

/// Returns the concatenation of two lists. Note that the resulting list
/// will only be empty if this function is called on two empty lists.
def _append(l1: ENList[a, e1], l2: ENList[a, e2]): ENList[a, e1 and e2] =
    match l1 {
        case ENList.Nil         =>
            unchecked_cast(l2 as ENList[a, e1 and e2])
        case ENList.Cons(x, xs) =>
            ENList.Cons(x, unchecked_cast(_append(xs, l2) as ENList[a, true]))
    }

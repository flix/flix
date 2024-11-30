/// A list of type `a` that uses a Boolean at the type level to track
/// whether its length is even or odd.
enum EOList[a: Type, e: Bool] {
    case Nil
    case Cons(a, EOList[a, not e])
}

/// Returns an empty list of type `a` with even length (which is 0).
def _mkNil(): EOList[a, true] = EOList.Nil

/// Attaches an element to the front of the list.
def _cons(x: a, l: EOList[a, e]): EOList[a, not e] =
    EOList.Cons(x, l)

/// Returns the head of an odd list, which must be nonempty.
def _head(l: EOList[a, false]): a = match l {
    case EOList.Nil        => bug!("Empty input not allowed")
    case EOList.Cons(x, _) => x
}

/// Returns the tail of the list.
/// Note how this changes the evenness of the list.
def _tail(l: EOList[a, e]): EOList[a, not e] = match l {
    case EOList.Nil         => EOList.Nil
    case EOList.Cons(_, xs) => xs
}

/// Applies a function `f` to each element of a list.
/// Note how evenness of the list is preserved.
def _map(f: a -> b, l: EOList[a, e]): EOList[b, e] = match l {
    case EOList.Nil         => EOList.Nil
    case EOList.Cons(x, xs) => EOList.Cons(f(x), _map(f, xs))
}

/// Returns the concatenation of `l1` and `l2`.
/// Note that the result is even if both lists are even or both are odd (i.e. (e1 and e2)
/// or (not e1 and not e2).
/// which is equivalent to not (e1 xor e2).
def _append(l1: EOList[a, e1], l2: EOList[a, e2]): EOList[a, not (e1 xor e2)] = match l1 {
     case EOList.Nil         =>
        // We have to insert unchecked casts because the Flix type system does not
        // refine the type of l1 when we pattern match on it, i.e. there no GADTs.
        unchecked_cast(l2 as EOList[a, not (e1 xor e2)])
     case EOList.Cons(x, xs) =>
        unchecked_cast(EOList.Cons(x, _append(xs, l2)) as EOList[a, not (e1 xor e2)])
}

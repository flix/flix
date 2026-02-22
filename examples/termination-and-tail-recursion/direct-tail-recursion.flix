/// A function is tail recursive if the recursive
/// call is the last operation in the function.

/// Returns true if `l1` is a prefix of `l2`.
@Terminates @Tailrec
pub def isPrefixOf(l1: List[a], l2: List[a]): Bool with Eq[a] = match (l1, l2) {
    case (Nil, _)           => true
    case (_, Nil)           => false
    case (x :: xs, y :: ys) => if (x == y) isPrefixOf(xs, ys) else false
}

def main(): Unit \ IO =
    isPrefixOf(1 :: 2 :: Nil, 1 :: 2 :: 3 :: Nil) |> println

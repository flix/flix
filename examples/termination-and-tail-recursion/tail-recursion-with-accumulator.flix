/// We can use the `@Terminates` annotation to have the compiler
/// enforce that `unzip` is guaranteed to terminate.

/// Returns the first and second elements of each pair in separate lists.
@Terminates
pub def unzip(l: List[(a, b)]): (List[a], List[b]) =
    def loop(ll, acc1, acc2) = match ll {
        case Nil             => (acc1, acc2)
        case (x1, x2) :: xs  => loop(xs, x1 :: acc1, x2 :: acc2)
    };
    let (l1, l2) = loop(l, Nil, Nil);
    (List.reverse(l1), List.reverse(l2))

def main(): Unit \ IO =
    unzip((1, 'a') :: (2, 'b') :: (3, 'c') :: Nil) |> println

/// The `@Terminates` annotation on `unzip` asks the compiler to
/// verify that the function is guaranteed to terminate.
///
/// When a top-level function is marked `@Terminates`, all its inner
/// local defs are implicitly checked for termination as well.
/// The `@Tailrec` annotation on the local def `loop` additionally
/// verifies that every self-recursive call is in tail position.

@Terminates
pub def unzip(l: List[(a, b)]): (List[a], List[b]) =
    @Tailrec
    def loop(ll, acc1, acc2) = match ll {
        case Nil             => (acc1, acc2)
        case (x1, x2) :: xs => loop(xs, x1 :: acc1, x2 :: acc2)
    };
    let (l1, l2) = loop(l, Nil, Nil);
    (List.reverse(l1), List.reverse(l2))

def main(): Unit \ IO =
    unzip((1, 'a') :: (2, 'b') :: (3, 'c') :: Nil) |> println

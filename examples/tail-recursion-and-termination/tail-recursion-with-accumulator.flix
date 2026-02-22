/// The `@Terminates` annotation on `unzip` asks the compiler to
/// verify that the function is guaranteed to terminate.
///
/// The `@Terminates` and `@Tailrec` annotations on the local def
/// `loop` verify that it terminates via structural recursion and
/// that every self-recursive call is in tail position.
/// The external call `loop(l, Nil, Nil)` appears inside a
/// let-binding (a non-tail position), but this is allowed because
/// it is not a self-recursive call of `loop`.

/// Returns the first and second elements of each pair in separate lists.
@Terminates
pub def unzip(l: List[(a, b)]): (List[a], List[b]) =
    @Terminates @Tailrec
    def loop(ll, acc1, acc2) = match ll {
        case Nil             => (acc1, acc2)
        case (x1, x2) :: xs => loop(xs, x1 :: acc1, x2 :: acc2)
    };
    let (l1, l2) = loop(l, Nil, Nil);
    (List.reverse(l1), List.reverse(l2))

def main(): Unit \ IO =
    unzip((1, 'a') :: (2, 'b') :: (3, 'c') :: Nil) |> println

/// The `@Terminates` annotation on `map` asks the compiler to verify
/// that the function is guaranteed to terminate.
///
/// The `@Terminates` and `@Tailrec` annotations on the local def
/// `loop` independently verify that the inner loop terminates via
/// structural recursion and that every self-recursive call is in
/// tail position. The external call `loop(l, Nil)` is not a
/// self-recursive call, so it is not subject to the tail-call check.

@Terminates
pub def map(f: a -> b \ ef, l: List[a]): List[b] \ ef =
    @Terminates @Tailrec
    def loop(ll, acc) = match ll {
        case Nil     => acc
        case x :: xs => loop(xs, f(x) :: acc)
    };
    List.reverse(loop(l, Nil))

def main(): Unit \ IO =
    map(x -> x + 1, 1 :: 2 :: 3 :: Nil) |> println

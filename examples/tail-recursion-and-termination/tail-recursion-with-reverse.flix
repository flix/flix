/// The `@Terminates` annotation on `map` asks the compiler to verify
/// that the function is guaranteed to terminate.
///
/// When a top-level function is marked `@Terminates`, all its inner
/// local defs are implicitly checked for termination as well.
/// The `@Tailrec` annotation on the local def `loop` additionally
/// verifies that every self-recursive call is in tail position.

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

/// We can use the `@Terminates` annotation to have the compiler
/// enforce that `map` is guaranteed to terminate.

@Terminates
pub def map(f: a -> b \ ef, l: List[a]): List[b] \ ef =
    def loop(ll, acc) = match ll {
        case Nil     => acc
        case x :: xs => loop(xs, f(x) :: acc)
    };
    List.reverse(loop(l, Nil))

def main(): Unit \ IO =
    map(x -> x + 1, 1 :: 2 :: 3 :: Nil) |> println

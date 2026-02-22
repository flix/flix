/// A common tail-recursive pattern is to build
/// a list in reverse order and then reverse it.

/// Returns the result of applying `f` to every element in `l`.
@Terminates @Tailrec
pub def map(f: a -> b \ ef, l: List[a]): List[b] \ ef =
    def loop(ll, acc) = match ll {
        case Nil     => acc
        case x :: xs => loop(xs, f(x) :: acc)
    };
    List.reverse(loop(l, Nil))

def main(): Unit \ IO =
    map(x -> x + 1, 1 :: 2 :: 3 :: Nil) |> println

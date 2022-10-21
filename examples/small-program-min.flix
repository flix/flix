///
/// The List type.
///
/// A list is either the empty list represented by `Nil`, or
/// an element `v` followed by a list `vs` represented by `v :: vs`.
///
pub enum List[t] {
    case Nil,
    case Cons(t, List[t])
}

namespace List {

    ///
    /// Alias for `scanLeft`.
    ///
    pub def scan(f: (b, a) -> b \ ef, s: b, l: List[a]): List[b] \ ef = scanLeft(f, s, l)

    ///
    /// Accumulates the result of applying `f` to `l` going left to right.
    ///
    /// That is, the result is of the form: `s :: f(s, x1) :: f(f(s, x1), x2)  ...`.
    ///
    pub def scanLeft(f: (b, a) -> b \ ef, s: b, l: List[a]): List[b] \ ef =
        def loop(ll, k, acc) = match ll {
            case Nil     => k(Nil)
            case x :: xs =>
                let y = f(acc, x);
                loop(xs, ks -> k(y :: ks), y)
        };
        loop(l, ks -> s :: ks, s)

}


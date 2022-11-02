
pub def identity(x: a): a = x

pub def upcast(f: a -> b): a -> b \ ef =
    x -> f(x) as \ ef

pub enum List[t] {
    case Nil,
    case Cons(t, List[t])
}

namespace List {

    pub def foldRight(f: (a, b) -> b \ ef, s: b, l: List[a]): b \ ef =
        def loop(ll, k) = match ll {
            case Nil     => k(s)
            case x :: xs => loop(xs, ks -> k(f(x, ks)))
        };
        loop(l, upcast(identity))

}

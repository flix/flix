
pub def identity(x: a): a = x

pub def upcast(f: a -> b): a -> b \ ef =
    x -> f(x) as \ ef

pub enum List[t] {
    case Nil,
    case Cons(t, List[t])
}

namespace List {

    pub def breaks(f: Int32 -> Int32 \ ef, s: Int32, l: List[Int32]): Int32 \ ef =
        def loop(ll, k) = match ll {
            case Nil     => k(s)
            case x :: _ => f(x)
        };
        loop(l, upcast(identity))

    pub def works_inlined(f: Int32 -> Int32 \ ef, s: Int32, l: List[Int32]): Int32 \ ef =
        match l {
            case Nil => upcast(identity)(s)
            case x :: _ => f(x)
        }

    pub def works_no_f(s: Int32, l: List[Int32]): Int32 \ ef =
        def loop(ll, k) = match ll {
            case Nil     => k(s)
            case x :: _ => k(x)
        };
        loop(l, upcast(identity))

}

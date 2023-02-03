restrictable enum Seq[i] {
    case Nil
    case One(Int32)
    case Cons(Int32, Seq[i])
}

type alias MOption = Seq[<Seq.Nil, Seq.One>]
type alias MList   = Seq[<Seq.Nil, Seq.Cons>]
type alias MNel    = Seq[<Seq.One, Seq.Cons>]

def last(s: Seq[i -- <Seq.Nil>]): Int32 =
    choose s {
        case Seq.One(x)      => x
        case Seq.Cons(_, xs) => last(xs)
    }

def forall(f: Int32 -> Bool, s: Seq[i]): Bool =
    choose s {
        case Seq.Nil         => true
        case Seq.One(x)      => f(x)
        case Seq.Cons(x, xs) => f(x) and forall(f, xs)
    }

def map(f: Int32 -> Int32, s: Seq[i]): Seq[i] =
    choose* s {
        case Seq.Nil         => Seq.Nil
        case Seq.One(x)      => Seq.One(f(x))
        case Seq.Cons(x, xs) => open Seq.Cons(f(x), map(f, xs))
}

def append(w: Int32, s: Seq[i]): Seq[<Seq.One, Seq.Cons>] =
    choose* s {
        case Seq.Nil         => Seq.One(w)
        case Seq.One(x)      => open Seq.Cons(x, Seq.One(x))
        case Seq.Cons(x, xs) => open Seq.Cons(x, append(w, xs))
    }

def reverse(s: Seq[i]): Seq[i ++ <Seq.One>] =
    choose* s {
        case Seq.Nil         => Seq.Nil
        case Seq.One(x)      => Seq.One(x)
        case Seq.Cons(x, xs) => append(x, reverse(xs))
    }


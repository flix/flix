restrictable enum Seq[i][j: Seq] {
    case Nil
    case Cons(Int32, Seq[j][j])
}

type alias MOption = Seq[<Seq.Nil, Seq.Cons>][<Seq.Nil>]
type alias MList   = Seq[<Seq.Nil, Seq.Cons>][<Seq.Nil, Seq.Cons>]
type alias MNel    = Seq[<Seq.Cons>][<Seq.Nil, Seq.Cons>]

def last(s: Seq[i -- <Seq.Nil>][j]): Int32 =
    choose s {
        case Seq.Cons(x, xs) => choose xs {
            case Seq.Nil => x
            case Seq.Cons(y, ys) => last(Seq.Cons(y, ys))
        }
    }

def forall(f: Int32 -> Bool, s: Seq[i][j]): Bool =
    choose s {
        case Seq.Nil         => true
        case Seq.Cons(x, xs) => f(x) and forall(f, xs)
    }

def map(f: Int32 -> Int32, s: Seq[i][j]): Seq[i][j] =
    choose* s {
        case Seq.Nil         => Seq.Nil
        case Seq.Cons(x, xs) => Seq.Cons(f(x), map(f, xs))
}

def append(w: Int32, s: Seq[i][j]): Seq[<Seq.Cons>][j ++ <Seq.Nil>] = {
    choose s {
        case Seq.Nil         => Seq.Cons(w, ???)
        case Seq.Cons(x, xs) => Seq.Cons(x, nestedOpen(append(w, xs)))
    }
}

def nestedOpen(s: Seq[i][j]): Seq[i ++ a][j ++ b] = choose* s {
    case Seq.Nil => Seq.Nil
    case Seq.Cons(x, xs) => Seq.Cons(x, nestedOpen(xs))
}

def auxAppend(w: Int32, s: Seq[i][i]): Seq[<Seq.Cons, Seq.Nil>][<Seq.Cons, Seq.Nil>] =
    choose* s {
        case Seq.Nil => Seq.Cons(w, open Seq.Nil)
        case Seq.Cons(x, xs) => Seq.Cons(x, auxAppend(w, xs))
    }

def reverse(s: Seq[i][j]): Seq[i][<Seq.Cons, Seq.Nil>] =
    choose* s {
        case Seq.Nil         => Seq.Nil
        case Seq.Cons(x, xs) => append(x, reverse(xs))
    }


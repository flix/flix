namespace Test/Restrictable/List {

    // infinite/normal/empty list
    // List[Nil]         : Nil
    // List[Cons]        : Cons(_, Cons(_, Cons(_, ...)))
    // List[{Cons, Nil}] : Any
    restrictable enum List[s][a] {
        case Nil
        case Cons(a, List[s][a])
    }

    // Shallow s, tail must have both?
    // List[Nil]         : Nil
    // List[Cons]        : Cons(_, Any)
    // List[{Cons, Nil}] : Any
    restrictable enum List[s][a] {
        case Nil
        case Cons(a, List[{s + Cons + Nil}][a])
    }

    // can Typer see that `s - Nil + Cons` = `Cons`
    def snoc(a: a, l: List[s][a]): List[{s - Nil + Cons}] = choose* l {
        case Nil => Cons(a, Nil)
        case Cons(x, rest) => Cons(x, snoc(a, rest): List[{Const}][a] /* ERR */)
    }

    // Shallow s, tail must have both?
    // List[Nil][_]                   : Nil
    // List[Cons][Nil]                : Cons(_, Nil)
    // List[Cons][Cons]               : Cons(_, Cons(_, Cons(_, ...)))
    // List[Cons][{Cons, Nil}]        : Cons(_, Any)
    // List[{Cons, Nil}][Nil]         : Nil | Cons(_, Nil)
    // List[{Cons, Nil}][Cons]        : Nil | Cons(_, Cons(_, Cons(_, ...)))
    // List[{Cons, Nil}][{Cons, Nil}] : Any
    restrictable enum List[s][d, a] {
        case Nil
        case Cons(a, List[d][d, a])
    }



}

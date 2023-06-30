enum BoolProxy[_: Bool] {
    case BoolProxy
}

enum HList[a: Type, eager: Bool] {
    case Nil
    case ECons(a, HList[a, eager])
    case LCons(Lazy[a], HList[a, eager])
}

def map(f: a -> b, l: HList[a, e]): HList[b, e] = ???

def zip(l1: HList[a, e1], l2: HList[b, e2]): HList[(a, b), e1 and e2] =
    typematch (l1, l2) {
        case _: (HList[_, true], HList[_, true]) => zipE(l1, l2)
        case _: _                                => zipL(l1, l2)
    }

def zipE(l1: HList[a, e1], l2: HList[b, e2]): HList[(a, b), e1 and e2] =
    match (l1, l2) {
        case _ => ???
    }

def zipL(l1: HList[a, e1], l2: HList[b, e2]): HList[(a, b), e1 and e2] =
    match (l1, l2) {
        case _ => ???
    }

enum EnhancedLazy[t: Type, _b: Bool] {
    case EnhancedLazy(Lazy[t])
}

// eager -> eager, nothing to do
// eager -> lazy (?)
// lazy -> eager (force)
// lazy -> lazy (nothing to do?)
def eval(e: EnhancedLazy[t, b1]): EnhancedLazy[t, b2] = ??? //


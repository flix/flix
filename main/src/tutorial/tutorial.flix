//
// Welcome to the Flix Tutorial!
//

rel LitExp(r: Str, c: Int)
rel AddExp(r: Str, x: Str, y: Str)
rel DivExp(r: Str, x: Str, y: Str)

lat LocalVar(k: Str, v: Constant)

enum Constant {
    case Top,
    case Cst(Int),
    case Bot
}

def leq(e1: Constant, e2: Constant): Bool
    = match (e1, e2) with {
        case (Constant.Bot, _) => true
        case (Constant.Cst(n1), Constant.Cst(n2)) => n1 == n2
        case (_, Constant.Top) => true
        case _ => false
    }

let Constant<> = (Constant.Bot, Constant.Top, leq, lub, glb)


LocalVar(r, lift(c)) :- LitExp(r, c).

def lift(i: Int): Constant = Constant.Cst(i)

LocalVar(r, sum(v1, v2)) :- AddExp(r, x, y),
                            LocalVar(x, v1),
                            LocalVar(y, v2).

LocalVar(r, div(v1, v2)) :- DivExp(r, x, y),
                            LocalVar(x, v1),
                            LocalVar(y, v2).

def sum(e1: Constant, e2: Constant): Constant
    = match (e1, e2) with {
        case (_, Constant.Bot) => Constant.Bot
        case (Constant.Bot, _) => Constant.Bot
        case (Constant.Cst(n1), Constant.Cst(n2)) => Constant.Cst(n1 + n2)
        case _ => Constant.Top
    }

def div(e1: Constant, e2: Constant): Constant
    = match (e1, e2) with {
        case (_, Constant.Bot) => Constant.Bot
        case (Constant.Bot, _) => Constant.Bot
        // TODO: zero case
        case (Constant.Cst(n1), Constant.Cst(n2)) => Constant.Cst(n1 / n2)
        case _ => Constant.Top
    }

rel ArithmeticError(r: Str)

ArithmeticError(r) :- isMaybeZero(y),
                      DivExp(r, n, d),
                      LocalVar(d, y).

def isMaybeZero(e: Constant): Bool
    = match e with {
        case Constant.Bot => false
        case Constant.Cst(n) => n == 0
        case Constant.Top => true
    }


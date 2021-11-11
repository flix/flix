/*
 * Copyright 2021 Magnus Madsen, Ondřej Lhoták
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

/// 
/// A fully polymorphic implementation of the IDE framework in Flix.
///
/// Written by Magnus Madsen, Ondřej Lhoták
///

///
/// We have now defined the IDE framework.
/// 
/// Next, we instantiate it with the constant propagation lattice.
///

///
/// We begin by defining the lattice and its associated operations.
///



enum Transformer with Eq, Order, ToString {
    case BotTrans,
    case NonBotTrans(Int,Int,Const)
}


instance LowerBound[Transformer] {
    pub def minValue(): Transformer = Transformer.BotTrans
}

instance PartialOrder[Transformer] {
    pub def lessEqual(e1: Transformer, e2: Transformer): Bool = e2 == JoinLattice.leastUpperBound(e1, e2)
}

instance JoinLattice[Transformer] {
    pub def leastUpperBound(t1: Transformer, t2: Transformer): Transformer = match (t1, t2) {
        case (Transformer.BotTrans, _) => t2
        case (_, Transformer.BotTrans) => t1
        case (Transformer.NonBotTrans(a1, b1, c1), Transformer.NonBotTrans(a2, b2, c2)) =>
            if (a1 == a2 and b1 == b2)
                Transformer.NonBotTrans(a1, b1, JoinLattice.leastUpperBound(c1, c2))
            else if(c1 == c2)
                if((a2-a1) != 0 and 0==(b1-b2)rem(a2-a1))
                //                // is divisible
                Transformer.NonBotTrans(a1, b2, JoinLattice.leastUpperBound(Const.Cst(a1*(b1-b2)/(a2-a1)+b1), JoinLattice.leastUpperBound(c1, c2)))
                //                // is not divisible
                else Transformer.NonBotTrans(1, 0, Const.Top)
            else ???: Transformer
    }
}

instance MeetLattice[Transformer] {
    pub def greatestLowerBound(x: Transformer, y: Transformer): Transformer = ???
}





lawless class Trans[a] {
    pub def id(): a
    pub def compose(x: a, y: a): a

    pub def apply(x: a, l: Const): Const // TODO: Signature
}

instance Trans[Transformer] {
  pub def id(): Transformer = Transformer.NonBotTrans(1,0,Const.Bot)

    pub def compose(t1: Transformer, t2: Transformer): Transformer = match (t1, t2) {
        case (_,Transformer.BotTrans)                                 => Transformer.BotTrans
        case (Transformer.BotTrans, Transformer.NonBotTrans(_, _, c)) => match c {
            case Const.Bot => Transformer.BotTrans
            case Const.Top => Transformer.NonBotTrans(0,0,Const.Top)
            case Const.Cst(cc) => Transformer.NonBotTrans(0,cc,c)
        }
        case (Transformer.NonBotTrans(a2, b2, c2), Transformer.NonBotTrans(a1, b1, c1)) =>
            Transformer.NonBotTrans(a1*a2, (a1*b2)+b1, JoinLattice.leastUpperBound(sum(mul(c2, lift(a1)), lift(b1)), c1))
    }

    // from paper: f = \l. a*l+b meet c; f(Top) = Top
    // paper is upside-down, so: f = \l. a*l+b join c; f(Bot) = Bot
    pub def apply(t: Transformer, l: Const): Const = match t {
        case Transformer.BotTrans => Const.Bot
        case Transformer.NonBotTrans(a, b, c) => if(l==Const.Bot) Const.Bot else JoinLattice.leastUpperBound(sum(mul(l, lift(a)), lift(b)),c)
    }

}

// Rules
def runIDE(cfg: List[(String, String)],
           callGraph: List[(String, String)],
           startNodes: List[(String, String)],
           endNodes: List[(String, String)]): Int32 & Impure =
    use Trans.{id, compose, apply};

    let p = #{
        // nodes in a given procedure
        InProc(p,start) :- StartNode(p, start).
        InProc(p,m) :- InProc(p,n), CFG(n,m).

        // intraproc
        JumpFn(d1,m,d3;compose(long,short)) :-
            CFG(n,m),
            JumpFn(d1,n,d2;long),
            EshIntra(n,d2,d3;short).

        // use summary
        JumpFn(d1,m,d3;compose(caller,summary)) :-
            CFG(n,m),
            JumpFn(d1,n,d2;caller),
            SummaryFn(n,d2,d3;summary).

        JumpFn(d3,start,d3;id()) :-
            JumpFn(_d1,call,d2;nonbottom1),
            CallGraph(call,target),
            EshCallStart(call,d2,target,d3;nonbottom2),
            StartNode(target,start),
            if (nonbottom1 != LowerBound.minValue() and
                nonbottom2 != LowerBound.minValue()). // TODO: Ondrej: Why are these checks here?

        // compute summary
        SummaryFn(call,d4,d5;compose(compose(cs,se),er)) :-
            CallGraph(call,target),
            StartNode(target,_start),
            EndNode(target,end),
            EshCallStart(call,d4,target,d1;cs),
            JumpFn(d1,end,d2;se),
            EshEndReturn(target,d2,call,d5;er).

        // tabulate result
        Results(n, d; apply(fn, vp)) :-
            ResultProc(proc, dp; vp),
            InProc(proc, n),
            JumpFn(dp, n, d; fn).

        ResultProc(proc,dp;apply(cs,v)) :-
            Results(call,d;v),
            EshCallStart(call,d,proc,dp;cs).
    };

    let f1 = project cfg, callGraph, startNodes, endNodes into CFG, CallGraph, StartNode, EndNode;

    // TODO: Move more facts as arguments.
    let f = #{
        EshIntra(n,"zero","zero";id()) :- CFG(n,_).

        EshIntra("smain","zero","x";Transformer.BotTrans).
        EshIntra("n2","x","x";id()).
        EshIntra("n3","x","x";id()).

        EshIntra("sp","a","a";id()).
        EshIntra("sp","x","x";id()).
        EshIntra("n4","a","a";id()).
        EshIntra("n4","x","x";id()).
        EshIntra("n5","a","a";Transformer.NonBotTrans(1,-2,Const.Bot)).
        EshIntra("n5","x","x";id()).
        EshIntra("n6","a","a";id()).
        EshIntra("n7","a","a";id()).
        EshIntra("n7","x","x";id()).
        EshIntra("n8","a","a";Transformer.NonBotTrans(1,2,Const.Bot)).
        EshIntra("n8","x","x";id()).
        EshIntra("n9","a","a";id()).
        EshIntra("n9","a","x";Transformer.NonBotTrans(-2,5,Const.Bot)).

        EshCallStart(call,"zero",target,"zero";id()) :- CallGraph(call,target).
        EshEndReturn(target,"zero",call,"zero";id()) :- CallGraph(call,target).

        EshCallStart("n1","zero","p","a";Transformer.NonBotTrans(0,7,Const.Bot)).
        EshCallStart("n1","x","p","x";id()).
        EshEndReturn("p","x","n1","x";id()).

        EshCallStart("n6","a","p","a";id()).
        EshCallStart("n6","x","p","x";id()).
        EshEndReturn("p","x","n6","x";id()).

        // Entrypoint
        JumpFn("zero","smain","zero";id()).
        ResultProc("main","zero";Const.Top).
    };
    let m = solve p, f, f1;
    (m |> Fixpoint.unsafePrint) as & Impure;
    0



///            Top
///
///   ... -2 -1 0 1 2 ...
///
///            Bot
///
enum Const with Eq, Order, ToString {
    case Bot,
    case Cst(Int),
    case Top
}

///
/// The Bottom Element of `Const`.
///
instance LowerBound[Const] {
    pub def minValue(): Const = Const.Bot
}

///
/// The Partial Order on `Const`.
///
instance PartialOrder[Const] {
    pub def lessEqual(x: Const, y: Const): Bool = match (x, y) {
        case (Const.Bot, _)                 => true
        case (Const.Cst(n1), Const.Cst(n2)) => n1 == n2
        case (_, Const.Top)                 => true
        case _                              => false
    }
}

///
/// The Least Upper Bound on `Const`.
///
instance JoinLattice[Const] {
    pub def leastUpperBound(x: Const, y: Const): Const = match (x, y) {
        case (Const.Bot, _)                               => y
        case (_, Const.Bot)                               => x
        case (Const.Cst(n1), Const.Cst(n2)) if (n1 == n2) => Const.Cst(n1)
        case _                                            => Const.Top
    }
}

///
/// The Greatest Lower Bound on `Const`.
///
instance MeetLattice[Const] {
    pub def greatestLowerBound(x: Const, y: Const): Const = match (x, y) {
        case (Const.Top, _)                               => y
        case (_, Const.Top)                               => x
        case (Const.Cst(n1), Const.Cst(n2)) if (n1 == n2) => Const.Cst(n1)
        case _                                            => Const.Bot
    }
}

///
/// We define a function to lift a concrete element into the constant propagation lattice.
///
def lift(n: Int32): Const = Const.Cst(n)

// TODO: Use Add and Mul type classes?

///
/// We define abstract addintion:
///
def sum(x: Const, y: Const): Const = match (x, y) {
    case (Const.Bot, _)                 => Const.Bot
    case (_, Const.Bot)                 => Const.Bot
    case (Const.Cst(n1), Const.Cst(n2)) => Const.Cst(n1 + n2)
    case _                              => Const.Top
}

///
/// And we define abstract multiplication:
///
def mul(x: Const, y: Const): Const = match (x, y) {
    case (Const.Bot, _)                 => Const.Bot
    case (_, Const.Bot)                 => Const.Bot
    case (Const.Cst(_), Const.Cst(0))   => Const.Cst(0) // TODO: Ondrej: Why is it like this?>
    case (Const.Cst(n1), Const.Cst(n2)) => Const.Cst(n1 * n2)
    case _                              => Const.Top
}


///
/// Run the example.
///
def main(_: Array[String]): Int32 & Impure =
    println("Running IDE");

    // TODO: Magnus: Use sets?

    let cfg =
        ("smain","n1") ::
        ("n1","n2") ::
        ("n2","n3") ::
        ("n3","emain") ::

        ("sp","n4") ::
        ("n4","n5") ::
        ("n4","n9") ::
        ("n5","n6") ::
        ("n6","n7") ::
        ("n7","n8") ::
        ("n8","n9") ::
        ("n9","ep") :: Nil;

    let callGraph =
        ("n1","p") ::
        ("n6","p") :: Nil;

    let startNodes =
        ("main","smain") ::
        ("p","sp") :: Nil;

    let endNodes =
        ("main","emain") ::
        ("p","ep") :: Nil;

    runIDE(cfg, callGraph, startNodes, endNodes);
    0

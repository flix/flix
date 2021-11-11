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

def compose(t1: Transformer, t2: Transformer): Transformer = match (t1, t2) {
case (_,Transformer.BotTrans) => Transformer.BotTrans
case (Transformer.BotTrans, Transformer.NonBotTrans(_, _, c)) => match c {
  case Const.Bot => Transformer.BotTrans
  case Const.Top => Transformer.NonBotTrans(0,0,Const.Top)
  case Const.Cst(cc) => Transformer.NonBotTrans(0,cc,c)
}
case (Transformer.NonBotTrans(a2, b2, c2), Transformer.NonBotTrans(a1, b1, c1)) =>
  Transformer.NonBotTrans(a1*a2, (a1*b2)+b1, JoinLattice.leastUpperBound(sum(prod(c2, a1), b1), c1))
}

// from paper: f = \l. a*l+b meet c; f(Top) = Top
// paper is upside-down, so: f = \l. a*l+b join c; f(Bot) = Bot
def apply(t: Transformer, l: Const): Const = match t {
    case Transformer.BotTrans => Const.Bot
    case Transformer.NonBotTrans(a, b, c) => if(l==Const.Bot) Const.Bot else JoinLattice.leastUpperBound(sum(prod(l,a),b),c)
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

// Inputs
lat EshIntra(node: String, infact: String, outfact: String, transformer: Transformer)
lat EshCallStart(node: String, infact: String, proc: String, outfact: String, transformer: Transformer)
lat EshEndReturn(proc: String, infact: String, node: String, outfact: String, transformer: Transformer)

// Call-to-Return edges should be included above in EshIntra
rel CFG(src: String, dst: String)
rel CallGraph(callsite: String, proc: String)
rel StartNode(proc: String, node: String)
rel EndNode(proc: String, node: String)

// Internal lattices
lat JumpFn(infact: String, node: String, outfact: String, transformer: Transformer)
lat SummaryFn(node: String, infact: String, outfact: String, transformer: Transformer)
lat ResultProc(proc: String, fact: String, value: Const)
rel InProc(proc: String, node: String)

// Output
lat Results(node: String, fact: String, value: Const)

// Rules for Example (constant propagation) from IDE paper
def id(): Transformer = Transformer.NonBotTrans(1,0,Const.Bot)

// call-to-start
def nonbottom(t: Transformer): Bool = t != Transformer.BotTrans

// Rules
def main(_: Array[String]): Int32 & Impure =
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
            nonbottom(nonbottom1),
            nonbottom(nonbottom2).

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

        CFG("smain","n1").
        CFG("n1","n2").
        CFG("n2","n3").
        CFG("n3","emain").

        CFG("sp","n4").
        CFG("n4","n5").
        CFG("n4","n9").
        CFG("n5","n6").
        CFG("n6","n7").
        CFG("n7","n8").
        CFG("n8","n9").
        CFG("n9","ep").

        StartNode("main","smain").
        StartNode("p","sp").
        EndNode("main","emain").
        EndNode("p","ep").

        CallGraph("n1","p").
        CallGraph("n6","p").

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
    let m = solve p;
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


def sum(e1: Const, e2: Int): Const = match e1 {
    case Const.Cst(n) => Const.Cst(n+e2)
    case _ => e1
}

def prod(e1: Const, e2: Int): Const = match (e1, e2) {
    case (Const.Cst(n), _) => Const.Cst(n*e2)
    case (Const.Bot, _) => Const.Bot
    case (_, zero) => if(zero == 0) Const.Cst(0) else e1
    case _ => e1
}
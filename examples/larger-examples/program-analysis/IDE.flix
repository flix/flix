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
/// A fully polymorphic implementation of the IDE framework written in Flix.
///
/// Written by Magnus Madsen, Ondřej Lhoták.
///
mod IDE {

    ///
    /// An IDE instance where:
    ///
    /// - p is the type of procedures.
    /// - n is the type of program point.
    /// - d is the type of analysis data elements.
    ///
    type alias IDE[p, n, d, f, l] = {
        zero            = d,
        main            = p,
        cfg             = List[(n, n)],
        startNodes      = List[(p, n)],            // TODO: Ondrej: Should this also be a function?
        endNodes        = List[(p, n)],
        callGraph       = List[(n, p)],
        eshIntra        = List[(n, d, d, f)],      // TODO: Ought to be a function: (n, d)      -> (d, f)
        eshCallStart    = List[(n, d, p, d, f)],   // TODO: Ought to be a function: (n, d, p)   -> (d, f)
        eshEndReturn    = List[(p, d, n, d, f)],   // TODO: Ought to be a function: (p, d, n)   -> (d, f)
        id              = f,
        apply           = (f, l) -> l,
        compose         = (f, f) -> f
    }

    ///
    /// Solves the given IDE instances.
    ///
    pub def run(ide: IDE[p, n, d, f, l]): List[(n, d, l)]
        with LowerBound[f], JoinLattice[f], MeetLattice[f],
             LowerBound[l], JoinLattice[l], MeetLattice[l],
             Order[n], Order[d], Order[p], Order[f],  Order[l] =

        // Extract main.
        let main = ide.main;

        // Extract apply and compose.
        let apply = ide.apply;
        let compose = ide.compose;

        ///
        /// The core IDE Framework.
        ///
        let p = #{
            // nodes in a given procedure
            InProc(p, start) :- StartNode(p, start).
            InProc(p, m) :- InProc(p, n), CFG(n, m).

            // intraproc
            JumpFn(d1, m, d3; compose(long, short)) :-
                CFG(n, m),
                JumpFn(d1, n, d2; long),
                EshIntra(n, d2, d3, short).

            // use summary
            JumpFn(d1, m, d3; compose(caller, summary)) :-
                CFG(n, m),
                JumpFn(d1, n, d2; caller),
                SummaryFn(n, d2, d3; summary).

            JumpFn(d3, start, d3; ide.id) :-
                JumpFn(_d1, call, d2; nonbottom1),
                CallGraph(call, target),
                EshCallStart(call, d2, target, d3, nonbottom2),
                StartNode(target, start),
                if (nonbottom1 != LowerBound.minValue() and
                    nonbottom2 != LowerBound.minValue()). // TODO: Ondrej: Why are these checks here?

            // compute summary
            SummaryFn(call, d4, d5; compose(compose(cs, se), er)) :-
                CallGraph(call, target),
                StartNode(target, _start),
                EndNode(target, end),
                EshCallStart(call, d4, target, d1, cs),
                JumpFn(d1, end, d2; se),
                EshEndReturn(target, d2, call, d5, er).

            // tabulate result
            Results(n, d; apply(fn, vp)) :-
                ResultProc(proc, dp; vp),
                InProc(proc, n),
                JumpFn(dp, n, d; fn).

            // TODO: Ondrej: Add comment.
            ResultProc(proc, dp; apply(cs,v)) :-
                Results(call, d; v),
                EshCallStart(call, d, proc, dp, cs).

            // TODO: Ondrej: Add comment.
            EshIntra(n, ide.zero, ide.zero, ide.id) :- CFG(n,_).

            // TODO: Ondrej: Add comment.
            EshCallStart(call, ide.zero, target, ide.zero, ide.id) :- CallGraph(call, target).

            // TODO: Ondrej: Add comment.
            EshEndReturn(target, ide.zero, call, ide.zero, ide.id) :- CallGraph(call, target).

            // TODO: Ondrej: Add comment.
            ResultProc(ide.main, ide.zero; LowerBound.minValue()).

            // TODO: Ondrej: Add comment.
            JumpFn(ide.zero, n, ide.zero; ide.id) :- StartNode(main, n).

        };


        let f1 = inject ide.cfg, ide.callGraph, ide.startNodes, ide.endNodes into CFG, CallGraph, StartNode, EndNode;
        let f2 = inject ide.eshIntra, ide.eshCallStart, ide.eshEndReturn into EshIntra, EshCallStart, EshEndReturn;

        query p, f1, f2 select (n, d, l) from Results(n, d; l)

}


///
/// We have now defined the IDE framework.
///
/// Next, we instantiate it with the constant propagation lattice.
///

///
/// We begin by defining the lattice and its associated operations.
///
mod ConstantProp {

    ///            Top
    ///
    ///   ... -2 -1 0 1 2 ...
    ///
    ///            Bot
    ///
    pub enum Const with Eq, Order, ToString {
        case Bot,
        case Cst(Int32),
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
            case (Const.Bot, _)                    => y
            case (_, Const.Bot)                    => x
            case (Const.Cst(n1), Const.Cst(n2)) => if (n1 == n2) Const.Cst(n1) else Const.Top
            case _                                 => Const.Top
        }
    }

    ///
    /// The Greatest Lower Bound on `Const`.
    ///
    instance MeetLattice[Const] {
        pub def greatestLowerBound(x: Const, y: Const): Const = match (x, y) {
            case (Const.Top, _)                 => y
            case (_, Const.Top)                 => x
            case (Const.Cst(n1), Const.Cst(n2)) => if (n1 == n2) Const.Cst(n1) else Const.Bot
            case _                              => Const.Bot
        }
    }

    ///
    /// We define a function to lift a concrete element into the constant propagation lattice.
    ///
    pub def lift(n: Int32): Const = Const.Cst(n)

    ///
    /// We define abstract addintion:
    ///
    pub def sum(x: Const, y: Const): Const = match (x, y) {
        case (Const.Bot, _)                 => Const.Bot
        case (_, Const.Bot)                 => Const.Bot
        case (Const.Cst(n1), Const.Cst(n2)) => Const.Cst(n1 + n2)
        case _                              => Const.Top
    }

    ///
    /// And we define abstract multiplication:
    ///
    pub def mul(x: Const, y: Const): Const = match (x, y) {
        case (Const.Bot, _)                 => Const.Bot
        case (_, Const.Bot)                 => Const.Bot
        case (Const.Cst(0), _)              => Const.Cst(0)
        case (_, Const.Cst(0))              => Const.Cst(0)
        case (Const.Cst(n1), Const.Cst(n2)) => Const.Cst(n1 * n2)
        case _                              => Const.Top
    }

    ///
    /// We can now define the micro functions.
    ///
    pub enum MicroFunction with Eq, Order, ToString {
        case Bot,
        case NonBot(Int32, Int32, ConstantProp.Const)
    }

    ///
    /// The Bottom Element.
    ///
    instance LowerBound[MicroFunction] {
        pub def minValue(): MicroFunction = MicroFunction.Bot
    }

    ///
    /// The Partial Order.
    ///
    instance PartialOrder[MicroFunction] {
        // TODO: Ondrej: This seems a bit lazy. Can we do better? :)
        pub def lessEqual(x: MicroFunction, y: MicroFunction): Bool = y == JoinLattice.leastUpperBound(x, y)
    }

    ///
    /// The Least Upper Bound.
    ///
    instance JoinLattice[MicroFunction] {
        pub def leastUpperBound(x: MicroFunction, y: MicroFunction): MicroFunction =
            use JoinLattice.{leastUpperBound => lub};
            match (x, y) {
                case (MicroFunction.Bot, _) => y
                case (_, MicroFunction.Bot) => x
                case (MicroFunction.NonBot(a1, b1, c1), MicroFunction.NonBot(a2, b2, c2)) =>
                    if (a1 == a2 and b1 == b2)
                        MicroFunction.NonBot(a1, b1, lub(c1, c2))
                    else if (c1 == c2)
                        if((a2-a1) != 0 and 0 == (b1 - b2) `Int32.rem` (a2 - a1)) {
                            // Divisible.
                            MicroFunction.NonBot(a1, b2, lub(Const.Cst(a1 * (b1 - b2) / (a2 - a1) + b1), lub(c1, c2)))
                        } else {
                            // Indivisible.
                            MicroFunction.NonBot(1, 0, Const.Top)
                        }
                    else {
                        unreachable!()
                    }
            }
    }

    ///
    /// The Greatest Lower Bound (required but not used).
    ///
    instance MeetLattice[MicroFunction] {
        pub def greatestLowerBound(_x: MicroFunction, _y: MicroFunction): MicroFunction = bug!("Not Implemented")
    }

    ///
    /// Returns the identity micro function.
    ///
    pub def id(): MicroFunction = MicroFunction.NonBot(1, 0, Const.Bot)

    ///
    /// Returns the composition of the two micro functions `f1` and `f2`.
    ///
    pub def compose(f1: MicroFunction, f2: MicroFunction): MicroFunction = match (f1, f2) {
        case (_, MicroFunction.Bot) => MicroFunction.Bot
        case (MicroFunction.Bot, MicroFunction.NonBot(_, _, c)) => match c {
            case Const.Bot     => MicroFunction.Bot
            case Const.Top     => MicroFunction.NonBot(0, 0, Const.Top)
            case Const.Cst(cc) => MicroFunction.NonBot(0, cc, c)
        }
        case (MicroFunction.NonBot(a2, b2, c2), MicroFunction.NonBot(a1, b1, c1)) =>
            use JoinLattice.{leastUpperBound => lub};
            MicroFunction.NonBot(a1 * a2, (a1 * b2) + b1, lub(sum(mul(c2, lift(a1)), lift(b1)), c1))
    }

    ///
    /// Returns the result of applying the micro function `f` to the lattice element `l`.
    ///
    pub def apply(f: MicroFunction, l: ConstantProp.Const): ConstantProp.Const = match f {
        case MicroFunction.Bot             => Const.Bot
        case MicroFunction.NonBot(a, b, c) => match l {
            case Const.Bot => Const.Bot
            case _         =>
                // TODO: Ondrej corporate this somewhere:
                // from paper: f = \l. a*l+b meet c; f(Top) = Top
                // paper is upside-down, so: f = \l. a*l+b join c; f(Bot) = Bot
                JoinLattice.leastUpperBound(sum(mul(l, lift(a)), lift(b)),c)
        }
    }
}

///
/// Run the example.
///
def main(): Unit \ IO =
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

    let eshIntra =
        ("smain","zero","x", ConstantProp.MicroFunction.Bot) ::
        ("n2","x","x", ConstantProp.id()) ::
        ("n3","x","x", ConstantProp.id()) ::

        ("sp","a","a", ConstantProp.id()) ::
        ("sp","x","x", ConstantProp.id()) ::
        ("n4","a","a", ConstantProp.id()) ::
        ("n4","x","x", ConstantProp.id()) ::
        ("n5","a","a", ConstantProp.MicroFunction.NonBot(1,-2, ConstantProp.Const.Bot)) ::
        ("n5","x","x", ConstantProp.id()) ::
        ("n6","a","a", ConstantProp.id()) ::
        ("n7","a","a", ConstantProp.id()) ::
        ("n7","x","x", ConstantProp.id()) ::
        ("n8","a","a", ConstantProp.MicroFunction.NonBot(1,2, ConstantProp.Const.Bot)) ::
        ("n8","x","x", ConstantProp.id()) ::
        ("n9","a","a", ConstantProp.id()) ::
        ("n9","a","x", ConstantProp.MicroFunction.NonBot(-2,5, ConstantProp.Const.Bot)) :: Nil;

    let eshCallStart =
        ("n1","zero","p","a",ConstantProp.MicroFunction.NonBot(0,7,ConstantProp.Const.Bot)) ::
        ("n1","x","p","x",ConstantProp.id()) ::
        ("n6","a","p","a",ConstantProp.id()) ::
        ("n6","x","p","x",ConstantProp.id()) :: Nil;

    let eshEndReturn =
        ("p", "x", "n1", "x", ConstantProp.id()) ::
        ("p", "x", "n6", "x", ConstantProp.id()) :: Nil;

    let result = IDE.run({
        zero         = "zero",
        main         = "main",
        cfg          = cfg,
        startNodes   = startNodes,
        endNodes     = endNodes,
        callGraph    = callGraph,
        eshIntra     = eshIntra,
        eshCallStart = eshCallStart,
        eshEndReturn = eshEndReturn,
        id           = ConstantProp.id(),
        apply        = ConstantProp.apply,
        compose      = ConstantProp.compose
        });

    println(result)

/*
 * Copyright 2021 Magnus Madsen
 * Copyright 2021 Benjamin Dahse
 * Copyright 2025 Casper Dalgaard Nielsen
 *                Adam Yasser Tallouzi
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
pub mod Fixpoint3.Ast.Shared {

    use Fixpoint3.Boxable
    use Fixpoint3.Boxed

    /////////////////////////////////////////////////////////////////////////////
    // Denotation                                                              //
    /////////////////////////////////////////////////////////////////////////////

    ///
    /// Represents the denotation of a predicate symbol.
    ///
    /// A predicate symbol either has relational or lattice semantics.
    ///
    /// If it has lattice semantics it is equipped with:
    ///
    /// - A bottom element.
    /// - A partial order (a leq function).
    /// - a least upper bound (a lub function).
    /// - a greatest lower bound (a glb function).
    ///
    pub enum Denotation[v] {
        case Relational
        case Latticenal(v, v -> v -> Bool, v -> v -> v, v -> v -> v)
    }

    pub type alias BoxedDenotation = Denotation[Boxed]

    ///
    /// Returns `true` if the given denotation is relational.
    ///
    pub def isRelational(den: Denotation[v]): Bool = match den {
        case Denotation.Relational => true
        case _                     => false
    }

    ///
    /// Returns a latticenal denotation associated with the type `t`.
    ///
    @LoweringTargetDatalog
    pub def lattice(): Denotation[v] with LowerBound[v], JoinLattice[v], MeetLattice[v] =
        Denotation.Latticenal(
            LowerBound.minValue(),
            (x, y) -> PartialOrder.lessEqual(x, y),
            (x, y) -> JoinLattice.leastUpperBound(x, y),
            (x, y) -> MeetLattice.greatestLowerBound(x, y)
        )

    ///
    /// Boxes the lattice components inside the given denotation `d`.
    ///
    @LoweringTargetDatalog
    pub def box(d: Denotation[t]): Denotation[Boxed] with Order[t] = match d {
        case Denotation.Relational => Denotation.Relational
        case Denotation.Latticenal(bot, leq, lub, glb) =>
            Denotation.Latticenal(
                Boxable.box(bot),
                Boxable.lift2b(leq),
                Boxable.lift2(lub),
                Boxable.lift2(glb)
            )
    }

    /////////////////////////////////////////////////////////////////////////////
    // PredSym                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    pub enum PredSym with Eq, Order {
        case PredSym(String, Int64)
    }

    instance ToString[PredSym] {
        pub def toString(predSym: PredSym): String = match predSym {
            case PredSym.PredSym(name, id) => "${name}%${id}"
        }
    }

}

/*
 * Copyright 2021 Magnus Madsen
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
mod Fixpoint.Ast {

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
    @Internal
    pub enum Denotation[v] {
        case Relational
        case Latticenal(v, v -> v -> Bool, v -> v -> v, v -> v -> v)
    }

    ///
    /// Returns `true` if the given denotation is relational.
    ///
    @Internal
    pub def isRelational(den: Denotation[v]): Bool = match den {
        case Denotation.Relational => true
        case _          => false
    }

    ///
    /// Returns a latticenal denotation associated with the type `t`.
    ///
    @Internal
    pub def lattice(): Denotation[t] with LowerBound[t], JoinLattice[t], MeetLattice[t] =
        // TODO: Why are these type annotations necessary?
        Denotation.Latticenal(
            LowerBound.minValue() : t,
            (x: t, y: t) -> PartialOrder.lessEqual(x: t, y: t),
            (x: t, y: t) -> JoinLattice.leastUpperBound(x: t, y: t),
            (x: t, y: t) -> MeetLattice.greatestLowerBound(x: t, y:t)
        )

    ///
    /// Boxes the lattice components inside the given denotation `d`.
    ///
    @Internal
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

}

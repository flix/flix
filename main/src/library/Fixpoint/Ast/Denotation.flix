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
namespace Fixpoint/Ast {
    pub enum Denotation[v] {
        case Relational
        case Latticenal(Unit -> v, v -> v -> Bool, v -> v -> v, v -> v -> v)
    }

    ///
    /// Returns a latticenal denotation with the given type.
    ///
    pub def lattice(): Denotation[t] with LowerBound[t], PartialOrder[t], JoinLattice[t], MeetLattice[t] =
        // TODO: Why are these type annotations necessary?
        Latticenal(
            () -> LowerBound.minValue() : t,
            (x: t, y: t) -> PartialOrder.partialCompare(x: t, y: t),
            (x: t, y: t) -> JoinLattice.leastUpperBound(x: t, y: t),
            (x: t, y: t) -> MeetLattice.greatestLowerBound(x: t, y:t)
        )

    ///
    /// Boxes the lattice components inside the given denotation.
    ///
    pub def box(d: Denotation[t]): Denotation[Boxed] with Boxable[t] = match d {
        case Denotation.Relational => Denotation.Relational
        case Denotation.Latticenal(bot, leq, lub, glb) =>
            Latticenal(
                () -> Boxable.box(bot()),
                Boxable.lift2b(leq),
                Boxable.lift2(lub),
                Boxable.lift2(glb)
            )
    }

}

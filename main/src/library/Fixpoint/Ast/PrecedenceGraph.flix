/*
 * Copyright 2021 Benjamin Dahse
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
    pub opaque type PrecedenceGraph = Set[PrecedenceEdge]

    pub enum PrecedenceEdge {
        case PositiveEdge(PredSym, PredSym)
        case NegativeEdge(PredSym, PredSym)
    }

    instance Monoid[PrecedenceGraph] {
        pub def empty(): PrecedenceGraph = PrecedenceGraph(Set.empty())

        pub def combine(x: PrecedenceGraph, y: PrecedenceGraph): PrecedenceGraph = match (x, y) {
            case (PrecedenceGraph(xs), PrecedenceGraph(ys)) => PrecedenceGraph(Set.union(xs, ys))
        }
    }

    instance Eq[PrecedenceEdge] {
        pub def eq(a: PrecedenceEdge, b: PrecedenceEdge): Bool = Fixpoint/Ast/PrecedenceEdge.compare(a, b) == EqualTo
    }

    instance Order[PrecedenceEdge] {
        pub def compare(a: PrecedenceEdge, b: PrecedenceEdge): Comparison = Fixpoint/Ast/PrecedenceEdge.compare(a, b)
    }

    namespace PrecedenceEdge {
        use Fixpoint/Ast.PrecedenceEdge;
        use Fixpoint/Ast.PrecedenceEdge.{PositiveEdge, NegativeEdge};

        pub def compare(a: PrecedenceEdge, b: PrecedenceEdge): Comparison = match (a, b) {
            case (PositiveEdge(x), PositiveEdge(y)) => x <=> y
            case (NegativeEdge(x), NegativeEdge(y)) => x <=> y
            case (NegativeEdge(_), PositiveEdge(_)) => LessThan
            case (PositiveEdge(_), NegativeEdge(_)) => GreaterThan
        }
    }
}

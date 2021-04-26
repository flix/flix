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

    instance Order[PrecedenceEdge] {
        pub def compare(a: PrecedenceEdge, b: PrecedenceEdge): Comparison = match (a, b) {
            case (PositiveEdge(x), PositiveEdge(y)) => x <=> y
            case (NegativeEdge(x), NegativeEdge(y)) => x <=> y
            case (NegativeEdge(_), PositiveEdge(_)) => LessThan
            case (PositiveEdge(_), NegativeEdge(_)) => GreaterThan
        }
    }

    pub def mkDepGraph(d: Datalog[v]): PrecedenceGraph = match d {
        case Datalog(_, rules) => Array.fold(Array.map(precedenceHelper, rules)) as & Pure
    }

    def precedenceHelper(cnst: Constraint[v]): PrecedenceGraph = match cnst {
        case Constraint(head, body) => Array.fold(Array.map(mkDepEdge(head), body)) as & Pure
    }

    def mkDepEdge(dst: HeadPredicate[v], src: BodyPredicate[v]): PrecedenceGraph = match (dst, src) {
        case (HeadAtom(headSym, _, _, _), BodyAtom(bodySym, _, Positive, _, _)) =>
            PrecedenceGraph(Set#{PositiveEdge(bodySym, headSym)})
        case (HeadAtom(headSym, _, _, _), BodyAtom(bodySym, _, Negative, _, _)) =>
            PrecedenceGraph(Set#{NegativeEdge(bodySym, headSym)})
        case _ => PrecedenceGraph(Set#{})
    }
}
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
    pub opaque type PrecedenceGraph[v] = Set[PrecedenceEdge[v]]

    pub enum PrecedenceEdge[v] {
        case PositiveEdge(BodyPredicate[v], HeadPredicate[v]),
        case NegativeEdge(BodyPredicate[v], HeadPredicate[v])
    }

    pub def precedence(cs: Datalog[v]): PrecedenceGraph[v] = match cs {
        case Datalog(_, rules) =>
            Array.fold(Array.map(precedenceHelper, rules)) as & Pure
    }

    def precedenceHelper(cnst: Constraint[v]): PrecedenceGraph[v] = match cnst {
        case Constraint(head, body, _) => Array.fold(Array.map(mkDepEdge(head), body)) as & Pure
    }

    def mkDepEdge(dst: HeadPredicate[v], src: BodyPredicate[v]): PrecedenceGraph[v] = match src {
        case BodyAtom(_, Positive, _, _) => PrecedenceGraph(Set#{PositiveEdge(src, dst)})
        case BodyAtom(_, Negative, _, _) => PrecedenceGraph(Set#{NegativeEdge(src, dst)})
        case GuardAtom(_) => PrecedenceGraph(Set#{})
    }

    instance Monoid[PrecedenceGraph[v]] {
        pub def empty(): PrecedenceGraph[v] = PrecedenceGraph(Set.empty())

        pub def combine(x: PrecedenceGraph[v], y: PrecedenceGraph[v]): PrecedenceGraph[v] = match (x, y) {
            case (PrecedenceGraph(xs), PrecedenceGraph(ys)) => PrecedenceGraph(Set.union(xs, ys))
        }
    }

    instance Order[PrecedenceEdge[v]] {

        pub def less(a: PrecedenceEdge[v], b: PrecedenceEdge[v]): Bool = match a <=> b {
            case LessThan => true
            case _ => false
        }

        pub def lessEqual(a: PrecedenceEdge[v], b: PrecedenceEdge[v]): Bool = match a <=> b {
            case LessThan => true
            case EqualTo => true
            case _ => false
        }

        pub def greater(a: PrecedenceEdge[v], b: PrecedenceEdge[v]): Bool = match a <=> b {
            case GreaterThan => true
            case _ => false
        }

        pub def greaterEqual(a: PrecedenceEdge[v], b: PrecedenceEdge[v]): Bool = match a <=> b {
            case GreaterThan => true
            case EqualTo => true
            case _ => false
        }

        pub def compare(a: PrecedenceEdge[v], b: PrecedenceEdge[v]): Comparison = match (a,b) {
            case (PositiveEdge(BodyAtom(s1, _, _, _), HeadAtom(s2, _, _)),
                    PositiveEdge(BodyAtom(s3, _, _, _), HeadAtom(s4, _, _))) =>
                let cmp = s1 <=> s3;
                if (cmp == EqualTo)
                s2 <=> s4
                else
                cmp
            case (NegativeEdge(BodyAtom(s1, _, _, _), HeadAtom(s2, _, _)),
                    NegativeEdge(BodyAtom(s3, _, _, _), HeadAtom(s4, _, _))) =>
                let cmp = s1 <=> s3;
                if (cmp == EqualTo)
                s2 <=> s4
                else
                cmp
            case (NegativeEdge(_, _), PositiveEdge(_, _)) => LessThan
            case (PositiveEdge(_, _), NegativeEdge(_, _)) => GreaterThan
        }

    }
}
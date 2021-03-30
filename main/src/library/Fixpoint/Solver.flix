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

use Fixpoint/Ast.{Datalog, Constraint, HeadTerm, PredSym, Denotation};
use Fixpoint/Ast.Datalog.Datalog;
use Fixpoint/Ast.Constraint.Constraint;
use Fixpoint/Ast.HeadPredicate.HeadAtom;
use Fixpoint/Ast.PredSym.PredSym;
use Fixpoint/Ram.RelSym;

// TODO: The following tests have been commented and should be fixed an incommented.
// Test.Dec.RelAlias
// Test.Exp.Fixpoint.Fold
// Test.Predicate.Filter
// Test.Predicate.Guard
// Test.Predicate.Union
// Test.Term.Apply
// Test.Term.Var.CapturedVar
// Test.Term.Var.WildVar
// fixpoint-computations-on-lattices
// TestConstant
// IFDS
// IDE
// SUOpt
// FloydWarshall
// TestIntroduction

namespace Fixpoint {
    pub def solve[v : Order : Eq : ToString](d: Datalog[v]): Datalog[v] =
        stratify(d) |>
        solveWithStratification(d)

    pub def solveWithStratification[v : Order : Eq : ToString](d: Datalog[v], stf: Map[PredSym, Int32]): Datalog[v] =
        let _ = notifyPreSolve(d, stf);
        let model = (compile(d, stf) |>
         queryStmt |>
         lowerStmt |>
         interpret |>
         toAst) as & Pure;
         let _ = notifyPostSolve(model);
         model

    pub def union(d1: Datalog[v], d2: Datalog[v]): Datalog[v] = match (d1, d2) {
        case (Datalog(edb1, idb1), Datalog(edb2, idb2)) =>
            Datalog(Array.append(edb1, edb2), Array.append(idb1, idb2)) as & Pure
    }

    pub def isSubsetOf[v : Order](d1: Datalog[v], d2: Datalog[v]): Bool = match (d1, d2) {
        case (Datalog(edb1, _), Datalog(edb2, _)) =>
            let db1 = toDatabase(edb1);
            let db2 = predicatesOf(edb1) |> restrict(edb2) |> toDatabase;
            let lookupD2 = k -> Map.getWithDefault(k, Set#{}, db2);
            let isSupersetOf = xs -> ys -> Set.isSubsetOf(ys, xs);
            Map.forall(lookupD2 >> isSupersetOf, db1)
    }

    pub def project(p: PredSym, d: Datalog[v]): Datalog[v] = match d {
        case Datalog(facts, rules) => Datalog(restrict(facts, Set#{p}), rules)
    }

    pub def foldRight[v : Order, b](d: Datalog[v], p: PredSym, i: b, f: (Array[v], b) -> b): b =
        let Datalog(facts, _) = project(p, d);
        Array.foldRight(factToTuple >> f, i, facts) as & Pure

    ///
    /// Unsafely casts and prints the given Datalog program `d`.
    ///
    pub def unsafePrint(d: a): a =
        let _ = Unsafe.println(unsafeCast(d));
        d

    ///
    /// Unsafely casts the given value `d` to a Datalog program.
    ///
    pub def unsafeCast(v: a): Datalog[Boxed] = v as Datalog[Boxed]


    def factToTuple(fact: Constraint[v]): Array[v] = match fact {
        case Constraint(HeadAtom(_, _, terms, _), _, _) => Array.map(t -> match t {
            case HeadTerm.Lit(v, _) => v
            case _ => ?bug
        }, terms) as & Pure
    }

    def tupleToFact(relSym: RelSym[v], tuple: Array[v], lat: v): Constraint[v] & Impure = match relSym {
        case RelSym.Full(relName, denotation) =>
            use Fixpoint/Ast.SourceLocation.Unknown;
            let terms = match denotation {
                case Denotation.Relational => Array.map(v -> HeadTerm.Lit(v, Unknown), tuple)
                case Denotation.Latticenal(_) => Array.init(i -> {
                    if (i < Array.length(tuple))
                        HeadTerm.Lit(tuple[i], Unknown)
                    else
                        HeadTerm.Lit(lat, Unknown)
                }, Array.length(tuple) + 1)
            };
            Constraint(HeadAtom(PredSym(relName, Unknown), denotation, terms, Unknown), [], Unknown)
        case _ => ?bug
    }

    def toDatabase[v : Order](facts: Array[Constraint[v]]): Map[String, Set[Array[v]]] =
        Array.foldRight(c -> match c {
            case Constraint(HeadAtom(PredSym(pred, _), _, _, _), _, _) =>
                Map.insertWith(Set.union, pred, Set#{factToTuple(c)})
        }, Map#{}, facts) as & Pure

    def toAst(db: Database[v]): Datalog[v] & Impure =
        let facts = MutList.new();
        MutMap.foreach(relSym -> rel -> match relSym {
            case RelSym.Full(_) => MutMap.foreach(t -> l -> {
                MutList.push!(tupleToFact(relSym, t, l), facts)
            }, rel)
            case _ => ()
        }, db);
        Datalog(MutList.toArray(facts), [])
}
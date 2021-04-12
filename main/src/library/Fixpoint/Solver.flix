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

use Boxable.unbox;
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
    pub def solve(d: Datalog[v]): Datalog[v] with Order[v], Eq[v], ToString[v] =
        stratify(d) |>
        solveWithStratification(d)

    pub def solveWithStratification(d: Datalog[v], stf: Map[PredSym, Int32]): Datalog[v] with Order[v], Eq[v], ToString[v] =
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

    pub def isSubsetOf(d1: Datalog[v], d2: Datalog[v]): Bool with Order[v], Eq[v], ToString[v] = match (d1, d2) {
        case (Datalog(edb1, _), Datalog(edb2, _)) =>
            use Fixpoint/Ram.toDenotation;
            let db1 = toDatabase(edb1);
            let db2 = predicatesOf(edb1) |> restrict(edb2) |> toDatabase;
            Map.forall(relSym -> rel1 -> match toDenotation(relSym) {
                case Denotation.Relational =>
                    let rel2 = Map.getWithDefault(relSym, Map#{}, db2);
                    Map.forall(t -> _ -> Map.memberOf(t, rel2), rel1)
                case Denotation.Latticenal(_, leq, _, _) =>
                    let rel2 = Map.getWithDefault(relSym, Map#{}, db2);
                    Map.forall(k -> l1 -> match Map.get(k, rel2) {
                        case Some(l2) => l1 `leq` l2
                        case None => false
                    }, rel1)
            }, db1)
    }

    pub def project(p: PredSym, d: Datalog[v]): Datalog[v] = match d {
        case Datalog(facts, rules) => Datalog(restrict(facts, Set#{p}), rules)
    }

    pub def foldRight(d: Datalog[v], p: PredSym, i: b, f: (Array[v], b) -> b): b =
        let Datalog(facts, _) = project(p, d);
        Array.foldRight(factToTuple >> f, i, facts) as & Pure

    ///
    /// Returns all facts in `d` associated with the predicate symbol `p`.
    ///
    pub def facts1(p: PredSym, d: Datalog[Boxed]): Array[v] with Boxable[v] =
        let f = terms -> unbox(terms[0]);
        (factsOf(p, d) |> Array.map(f)) as & Pure

    ///
    /// Returns all facts in `d` associated with the predicate symbol `p`.
    ///
    pub def facts2(p: PredSym, d: Datalog[Boxed]): Array[(t1, t2)] with Boxable[t1], Boxable[t2] =
        let f = terms -> (unbox(terms[0]), unbox(terms[1]));
        (factsOf(p, d) |> Array.map(f)) as & Pure

    ///
    /// Returns all facts in `d` associated with the predicate symbol `p`.
    ///
    pub def facts3(p: PredSym, d: Datalog[Boxed]): Array[(t1, t2, t3)] with Boxable[t1], Boxable[t2], Boxable[t3] =
        let f = terms -> (unbox(terms[0]), unbox(terms[1]), unbox(terms[2]));
        (factsOf(p, d) |> Array.map(f)) as & Pure

    ///
    /// Returns all facts in `d` associated with the predicate symbol `p`.
    ///
    pub def facts4(p: PredSym, d: Datalog[Boxed]): Array[(t1, t2, t3, t4)] with Boxable[t1], Boxable[t2], Boxable[t3], Boxable[t4] =
        let f = terms -> (unbox(terms[0]), unbox(terms[1]), unbox(terms[2]), unbox(terms[3]));
        (factsOf(p, d) |> Array.map(f)) as & Pure

    ///
    /// Returns all facts in `d` associated with the predicate symbol `p`.
    ///
    pub def facts5(p: PredSym, d: Datalog[Boxed]): Array[(t1, t2, t3, t4, t5)] with Boxable[t1], Boxable[t2], Boxable[t3], Boxable[t4], Boxable[t5] =
        let f = terms -> (unbox(terms[0]), unbox(terms[1]), unbox(terms[2]), unbox(terms[3]), unbox(terms[4]));
        (factsOf(p, d) |> Array.map(f)) as & Pure

    ///
    /// Returns an array of facts associated with the given predicate symbol `p` in the given Datalog program `d`.
    ///
    def factsOf(p: PredSym, d: Datalog[v]): Array[Array[v]] & Impure =
        let Datalog(facts, _) = d;
        restrict(facts, Set#{p}) |>
        Array.map(match Constraint(head, _, _) -> head) |>
        Array.map(match HeadAtom(_, _, terms, _) -> terms) |>
        Array.map(terms -> terms |> Array.map(headTermValue))

    ///
    /// Returns the value of the given head term `v`.
    ///
    /// Note the term must have been fully evaluated to a literal value.
    ///
    def headTermValue(t: HeadTerm[v]): v = match t {
        case HeadTerm.Lit(v, _) => v
        case _                  => bug!("Unexpected non-value term.")
    }

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

    def toDatabase(facts: Array[Constraint[v]]): Map[RelSym[v], Map[Array[v], v]] with Order[v] =
        Array.foldRight(c -> match c {
            case Constraint(HeadAtom(PredSym(pred, _), denotation, terms, _), _, _) =>
                let relSym = RelSym.Full(pred, denotation);
                match denotation {
                    case Denotation.Relational =>
                        Map.singleton(factToTuple(c), default) |>
                        Map.insertWith(Map.union, relSym)
                    case Denotation.Latticenal(_, _, lub, _) =>
                        let key = Array.init(i -> match terms[i] {
                            case HeadTerm.Lit(v, _) => v
                            case _ => ?bug
                        }, Array.length(terms) - 1);
                        let lat = match Array.last(terms) {
                            case Some(HeadTerm.Lit(v, _)) => v
                            case _ => ?bug
                        };
                        Map.singleton(key, lat) |>
                        Map.insertWith(Map.unionWith(lub), relSym)
                }
        } as & Pure, Map#{}, facts) as & Pure

    def toAst(db: Database[v]): Datalog[v] & Impure =
        // TODO: don't eagerly convert back to AST. Keep it as a database as long as possible.
        let facts = MutList.new();
        MutMap.foreach(relSym -> rel -> match relSym {
            case RelSym.Full(_) => MutMap.foreach(t -> l -> {
                MutList.push!(tupleToFact(relSym, t, l), facts)
            }, rel)
            case _ => ()
        }, db);
        Datalog(MutList.toArray(facts), [])
}
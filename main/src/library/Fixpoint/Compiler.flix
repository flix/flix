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

use Fixpoint/Ram.{RamStmt, RelOp, RelSym, RamTerm, BoolExp, RowVar};
use Fixpoint/Ast.{Datalog, Constraint, BodyPredicate, BodyTerm, HeadTerm, Polarity, PredSym, VarSym};
use Fixpoint/Ast.Datalog.Datalog;
use Fixpoint/Ast.Constraint.Constraint;
use Fixpoint/Ast.HeadPredicate.HeadAtom;
use Fixpoint/Ast.BodyPredicate.{BodyAtom, Guard0, Guard1, Guard2, Guard3, Guard4, Guard5};
use Fixpoint/Ast.PredSym.PredSym;

namespace Fixpoint {
    pub def compile[v : ToString](cs: Datalog[v], strat: Map[PredSym, Int32]): RamStmt[v] = match cs {
        case Datalog(facts, rules) =>
            let evalFacts = Array.fold(Array.map(compileRule, facts)) as & Pure;
            let evalStrata =
                Map.invert(strat) |>
                Map.map(restrict(rules) >> compileStratum) |>
                Map.valuesOf |>
                List.fold;
            List.fold(evalFacts :: evalStrata :: Nil)
    }

    /// Compile all rules to be computed in a stratum to RAM
    /// by using semi-naive evaluation as a code generator.
    def compileStratum[v : ToString](stratum: Array[Constraint[v]]): RamStmt[v] =
        let fold = f -> xs -> Set.foldRight(x -> Monoid.combine(f(x)), Monoid.empty(), xs);
        let idb = predicatesOf(stratum);
        let eval = Array.fold(Array.map(compileRule, stratum)) as & Pure;
        let mergeDelta = fold(match PredSym(relName, _) -> {
            RamStmt.Merge(RelSym.Full(relName), RelSym.Delta(relName))
        }, idb);
        let purge = fold(match PredSym(relName, _) -> {
            RamStmt.Purge(RelSym.New(relName))
        }, idb);
        let evalIncr = Array.fold(Array.map(compileRuleIncr, stratum)) as & Pure;
        let mergeNew = fold(match PredSym(relName, _) -> {
            RamStmt.Merge(RelSym.New(relName), RelSym.Full(relName))
        }, idb);
        let assign = fold(match PredSym(relName, _) -> {
            RamStmt.Assign(RelSym.Delta(relName), RelSym.New(relName))
        }, idb);
        let loopTest = Set.foldRight(match PredSym(relName, _) -> acc -> {
            BoolExp.Empty(RelSym.Delta(relName)) :: acc
        }, Nil, idb);
        let loopBody = List.fold(purge :: evalIncr :: mergeNew :: assign :: Nil);
        let fixpoint = RamStmt.Until(loopTest, loopBody);
        List.fold(eval :: mergeDelta :: fixpoint :: Nil)

    /// Compile rule into a join loop.
    def compileRule[v : ToString](rule: Constraint[v]): RamStmt[v] = match rule {
        case Constraint(HeadAtom(PredSym(headPred, _), headTerms, _), body, _) =>
            let augBody = augmentBody(body);
            let env = unifyVars(augBody);
            let join = joinTerms(env, augBody);
            let ramTerms = Array.map(compileHeadTerm(env), headTerms) as & Pure;
            let projection = RelOp.Project(ramTerms, RelSym.Full(headPred));
            let insert =
                Array.foldRight(match (atom, rowVar) -> match (isLast, acc) -> match atom {
                    case BodyAtom(PredSym(pred, _), Polarity.Positive, _, _) =>
                        if (isLast)
                            (false, RelOp.Search(rowVar, RelSym.Full(pred), join, acc))
                        else
                            (false, RelOp.Search(rowVar, RelSym.Full(pred), Nil, acc))
                    case _ => (isLast, acc)
                }, (true, projection), augBody) as & Pure |>
                snd |>
                RamStmt.Insert;
            let comment = RamStmt.Comment(ToString.toString(rule));
            List.fold(comment :: insert :: Nil)
    }

    def compileRuleIncr[v : ToString](rule: Constraint[v]): RamStmt[v] = match rule {
        case Constraint(HeadAtom(PredSym(headPred, _), headTerms, _), body, _) =>
            let augBody = augmentBody(body);
            let env = unifyVars(augBody);
            let join = joinTerms(env, augBody);
            let ramTerms = Array.map(compileHeadTerm(env), headTerms) as & Pure;
            let projection = RelOp.Project(ramTerms, RelSym.New(headPred));
            /// Compile rule into an incremental join loop where delta designates the focused atom.
            let compile = delta ->
                Array.foldRight(match (atom, rowVar) -> match (isLast, acc) -> match atom {
                    case BodyAtom(PredSym(pred, _), Polarity.Positive, _, _) =>
                        let relSym = if (rowVar == delta) {
                            RelSym.Delta(pred)
                        } else {
                            RelSym.Full(pred)
                        };
                        let where = if (isLast) {
                            BoolExp.NotMemberOf(ramTerms, RelSym.Full(headPred)) :: join
                        } else {
                            Nil
                        };
                        (false, RelOp.Search(rowVar, relSym, where, acc))
                    case _ => (isLast, acc)
                }, (true, projection), augBody) as & Pure |>
                snd |>
                RamStmt.Insert;
            let positiveAtoms = Array.filter(match (atom, _) -> match atom {
                case BodyAtom(_, Polarity.Positive, _, _) => true
                case _ => false
            }, augBody) as & Pure;
            let deltas = Array.fold(Array.map(match (_, rowVar) -> compile(rowVar), positiveAtoms)) as & Pure;
            let comment = RamStmt.Comment(ToString.toString(rule));
            List.fold(comment :: deltas :: Nil)
    }

    def compileHeadTerm(env: Map[VarSym, RamTerm[v]], term: HeadTerm[v]): RamTerm[v] = match term {
        case HeadTerm.Var(var, _) => unwrap(Map.get(var, env))
        case HeadTerm.Lit(v, _) => RamTerm.Lit(v)
        case HeadTerm.App0(f, _) => RamTerm.App0(f)
        case HeadTerm.App1(f, v, _) =>
            let t = unwrap(Map.get(v, env));
            RamTerm.App1(f, t)
        case HeadTerm.App2(f, v1, v2, _) =>
            let t1 = unwrap(Map.get(v1, env));
            let t2 = unwrap(Map.get(v2, env));
            RamTerm.App2(f, t1, t2)
        case HeadTerm.App3(f, v1, v2, v3, _) =>
            let t1 = unwrap(Map.get(v1, env));
            let t2 = unwrap(Map.get(v2, env));
            let t3 = unwrap(Map.get(v3, env));
            RamTerm.App3(f, t1, t2, t3)
        case HeadTerm.App4(f, v1, v2, v3, v4, _) =>
            let t1 = unwrap(Map.get(v1, env));
            let t2 = unwrap(Map.get(v2, env));
            let t3 = unwrap(Map.get(v3, env));
            let t4 = unwrap(Map.get(v4, env));
            RamTerm.App4(f, t1, t2, t3, t4)
        case HeadTerm.App5(f, v1, v2, v3, v4, v5, _) =>
            let t1 = unwrap(Map.get(v1, env));
            let t2 = unwrap(Map.get(v2, env));
            let t3 = unwrap(Map.get(v3, env));
            let t4 = unwrap(Map.get(v4, env));
            let t5 = unwrap(Map.get(v5, env));
            RamTerm.App5(f, t1, t2, t3, t4, t5)
    }

    /// Augment body atoms with row variables.
    def augmentBody(body: Array[BodyPredicate[v]]): Array[(BodyPredicate[v], RowVar)] =
        Array.mapWithIndex(atom -> i -> match atom {
            case BodyAtom(predSym, Polarity.Positive, _, _) => (atom, RowVar.Named("${predSym}$${i}"))
            case _ => (atom, RowVar.Named("IfYouSeeThisYouFoundABug"))
        }, body) as & Pure

    /// Unifies each variable that occurs in a rule body with an attribute
    /// corresponding to a positive atom where the variable is a term.
    def unifyVars(body: Array[(BodyPredicate[v], RowVar)]): Map[VarSym, RamTerm[v]] =
        Array.foldLeft(acc -> match (atom, rowVar) -> match atom {
            case BodyAtom(_, Polarity.Positive, terms, _) =>
                Array.mapWithIndex(term -> j -> match term {
                    case BodyTerm.Var(var, _) => Map.singleton(var, RamTerm.Attribute(rowVar, j))
                    case BodyTerm.Lit(_, _) => Map#{}
                    case BodyTerm.Wild(_) => Map#{}
                }, terms) |>
                Array.foldLeft(Map.union, acc)
            case _ => acc
        }, Map#{}, body) as & Pure

    /// Equates every term in a positive body atom with an attribute
    /// and declares that terms in negative body atoms are not in the corresponding relations.
    def joinTerms(env: Map[VarSym, RamTerm[v]], body: Array[(BodyPredicate[v], RowVar)]): List[BoolExp[v]] =
        Array.foldRight(match (atom, rowVar) -> acc ->
            let compileBodyTerm = term -> j -> match term {
                case BodyTerm.Var(var, _) => unwrap(Map.get(var, env))
                case BodyTerm.Lit(v, _) => RamTerm.Lit(v)
                case BodyTerm.Wild(_) => RamTerm.Attribute(rowVar, j)
            };
            match atom {
                case BodyAtom(_, Polarity.Positive, terms, _) =>
                    Array.mapWithIndex(term -> j -> {
                        let ramTerm = compileBodyTerm(term, j);
                        BoolExp.Eq(RamTerm.Attribute(rowVar, j), ramTerm)
                    }, terms) |>
                    Array.foldRight(x -> xs -> x :: xs, acc)
                case BodyAtom(PredSym(pred, _), Polarity.Negative, terms, _) =>
                    let ramTerms = Array.mapWithIndex(compileBodyTerm, terms);
                    BoolExp.NotMemberOf(ramTerms, RelSym.Full(pred)) :: acc
                case Guard0(f, _) =>
                    BoolExp.Guard0(f) :: acc
                case Guard1(f, v, _) =>
                    let t = unwrap(Map.get(v, env));
                    BoolExp.Guard1(f, t) :: acc
                case Guard2(f, v1, v2, _) =>
                    let t1 = unwrap(Map.get(v1, env));
                    let t2 = unwrap(Map.get(v2, env));
                    BoolExp.Guard2(f, t1, t2) :: acc
                case Guard3(f, v1, v2, v3, _) =>
                    let t1 = unwrap(Map.get(v1, env));
                    let t2 = unwrap(Map.get(v2, env));
                    let t3 = unwrap(Map.get(v3, env));
                    BoolExp.Guard3(f, t1, t2, t3) :: acc
                case Guard4(f, v1, v2, v3, v4, _) =>
                    let t1 = unwrap(Map.get(v1, env));
                    let t2 = unwrap(Map.get(v2, env));
                    let t3 = unwrap(Map.get(v3, env));
                    let t4 = unwrap(Map.get(v4, env));
                    BoolExp.Guard4(f, t1, t2, t3, t4) :: acc
                case Guard5(f, v1, v2, v3, v4, v5, _) =>
                    let t1 = unwrap(Map.get(v1, env));
                    let t2 = unwrap(Map.get(v2, env));
                    let t3 = unwrap(Map.get(v3, env));
                    let t4 = unwrap(Map.get(v4, env));
                    let t5 = unwrap(Map.get(v5, env));
                    BoolExp.Guard5(f, t1, t2, t3, t4, t5) :: acc
            }, Nil, body) as & Pure

    /// Restrict a constraint system to those constraints whose head predicate belongs to the given domain.
    def restrict(cs: Array[Constraint[v]], domain: Set[PredSym]): Array[Constraint[v]] =
        let headPred = match Constraint(HeadAtom(headSym, _, _), _, _) -> headSym;
        let memberOf = xs -> x -> Set.memberOf(x, xs);
        Array.filter(headPred >> memberOf(domain), cs) as & Pure

    /// Return the set of predicates of the given constraint system.
    def predicatesOf(cs: Array[Constraint[v]]): Set[PredSym] =
        Array.foldRight(match Constraint(HeadAtom(headPred, _, _), _, _) -> Set.insert(headPred), Set#{}, cs) as & Pure

    def unwrap(o: Option[a]): a = match o {
        case Some(a) => a
        case None => ???
    }
}

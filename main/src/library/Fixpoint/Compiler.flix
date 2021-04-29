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

use Fixpoint/Ram.{RamStmt, RelOp, RamSym, RamTerm, BoolExp, RowVar};
use Fixpoint/Ast.{Datalog, Constraint, BodyPredicate, BodyTerm, HeadTerm, Denotation, Polarity, PredSym, VarSym};
use Fixpoint/Ast.Datalog.Datalog;
use Fixpoint/Ast.Constraint.Constraint;
use Fixpoint/Ast.HeadPredicate.HeadAtom;
use Fixpoint/Ast.BodyPredicate.{BodyAtom, Guard0, Guard1, Guard2, Guard3, Guard4, Guard5};
use Fixpoint/Ast.PredSym.PredSym;

namespace Fixpoint {
    ///
    /// Compile the given Datalog program `cs` to RAM.
    /// First compile all facts in `cs` to project-statements.
    /// For example, the fact `Path(1, 2).` becomes `project (1, 2) into Path`.
    /// Second compile all rules in `cs` according to the given stratification `strat`.
    /// This means to compile all rules in stratum 0, then stratum 1 and so on.
    /// A rule belongs to the same stratum as its head predicate.
    ///
    def compile(cs: Datalog[v], strat: Map[PredSym, Int32]): RamStmt[v] with ToString[v] = match cs {
        case Datalog(facts, rules) =>
            let evalFacts = Array.fold(Array.map(compileRule, facts)) as & Pure;
            let evalStrata =
                Map.invert(strat) |>
                Map.map(restrict(rules) >> compileStratum) |>
                Map.valuesOf |>
                List.fold;
            List.fold(evalFacts :: evalStrata :: Nil)
    }

    ///
    /// Compile all rules to be evaluated in `stratum` to RAM
    /// by using semi-naive evaluation as a code generator.
    /// The steps are:
    ///     1. For each rule Ri in `stratum` with head Pi:
    ///     Pi = Pi ∪ Eval-Rule(Ri);
    ///     merge Pi into ΔPi;
    ///     2. Generate fixpoint loop:
    ///     until(⋃Pi == ∅) do
    ///     2.1. For each rule Ri with Pi in its head:
    ///         purge ΔPi';
    ///         ΔPi' = ΔPi' ∪ Eval-Rule-Incr(Ri);
    ///         merge ΔPi' into Pi;
    ///         ΔPi = ΔPi';
    ///     end
    /// Note that Eval-Rule is the code emitted by `compileRule`
    /// and Eval-Rule-Incr is the code emitted by `compileRuleIncr`.
    ///
    def compileStratum(stratum: Array[Constraint[v]]): RamStmt[v] with ToString[v] =
        let idb = Array.foldRight(match Constraint(HeadAtom(pred, den, _), _) -> {
            Map.insert(pred, den)
        }, Map#{}, stratum) as & Pure;
        let eval = Array.fold(Array.map(compileRule, stratum)) as & Pure;
        let mergeDelta =
            Map.mapWithKey(match PredSym(relName) -> den -> {
                RamStmt.Merge(RamSym.Full(relName, den), RamSym.Delta(relName, den))
            }, idb) |>
            Map.valuesOf |>
            List.fold;
        let purge =
            Map.mapWithKey(match PredSym(relName) -> den -> {
                RamStmt.Purge(RamSym.New(relName, den))
            }, idb) |>
            Map.valuesOf |>
            List.fold;
        let evalIncr = Array.fold(Array.map(compileRuleIncr, stratum)) as & Pure;
        let mergeNew =
            Map.mapWithKey(match PredSym(relName) -> den -> {
                RamStmt.Merge(RamSym.New(relName, den), RamSym.Full(relName, den))
            }, idb) |>
            Map.valuesOf |>
            List.fold;
        let assign =
            Map.mapWithKey(match PredSym(relName) -> den -> {
                RamStmt.Assign(RamSym.Delta(relName, den), RamSym.New(relName, den))
            }, idb) |>
            Map.valuesOf |>
            List.fold;
        let loopTest =
            Map.mapWithKey(match PredSym(relName) -> den -> {
                BoolExp.Empty(RamSym.Delta(relName, den))
            }, idb) |>
            Map.valuesOf;
        let loopBody = List.fold(purge :: evalIncr :: mergeNew :: assign :: Nil);
        let fixpoint = RamStmt.Until(loopTest, loopBody);
        List.fold(eval :: mergeDelta :: fixpoint :: Nil)

    ///
    /// Compile `rule` into a join loop. A join loop is a nested loop.
    /// Each positive body atom in `rule` becomes part of the nested loop.
    /// The head atom in `rule` is compiled to an insertion into the corresponding relation.
    /// Negative atoms are compiled into if-statements.
    /// The rule `R(x) :- not A(x), B(x),  C(x).` compiles to:
    ///     search B$1 ∈ B do
    ///         search C$2 C do
    ///             if (B$1[0] == C$2[0] ∧ (B$1[0]) ∉ A) then
    ///                 project (B$1[0]) into R
    ///             end
    ///         end
    ///     end
    ///
    def compileRule(rule: Constraint[v]): RamStmt[v] with ToString[v] = match rule {
        case Constraint(HeadAtom(PredSym(headPred), headDen, headTerms), body) =>
            let augBody = augmentBody(body);
            let env = unifyVars(augBody);
            let ramTerms = Array.map(compileHeadTerm(env), headTerms) as & Pure;
            let projection = RelOp.Project(ramTerms, RamSym.Full(headPred, headDen));
            let join = compileBody(env, augBody);
            let loopBody = RelOp.If(join, projection);
            let insert =
                Array.foldRight(match (atom, rowVar) -> acc -> match atom {
                    case BodyAtom(PredSym(pred), den, Polarity.Positive, _) =>
                        RelOp.Search(rowVar, RamSym.Full(pred, den), acc)
                    case _ => acc
                }, loopBody, augBody) as & Pure |>
                RamStmt.Insert;
            if (enableDebugging()) {
                let comment = RamStmt.Comment(ToString.toString(rule));
                List.fold(comment :: insert :: Nil)
            } else {
                insert
            }
    }

    ///
    /// Compile `rule` into an incremental join loop. A join loop is a nested loop.
    /// Each positive body atom in `rule` becomes part of the nested loop.
    /// The head atom in `rule` is compiled to an insertion into the corresponding relation.
    /// Incremental means there is a join loop for every positive atom.
    /// Each join loop "focuses" on a particular atom.
    /// Negative atoms are compiled into if-statements.
    /// The rule `R(x) :- not A(x), B(x),  C(x).` compiles to:
    ///     search B$1 ∈ ΔB do
    ///         search C$2 C do
    ///             if (B$1[0] == C$2[0] ∧ (B$1[0]) ∉ A ∧ (B$1[0]) ∉ R) then
    ///                 project (B$1[0]) into ΔR'
    ///             end
    ///         end
    ///     end
    ///     search B$1 ∈ B do
    ///         search C$2 ΔC do
    ///             if (B$1[0] == C$2[0] ∧ (B$1[0]) ∉ A ∧ (B$1[0]) ∉ R) then
    ///                 project (B$1[0]) into ΔR'
    ///             end
    ///         end
    ///     end
    ///
    /// Note that there are two join loops, because there are two positive atoms.
    /// Also note how in the first loop, `B` is the focused atom and `C` is focused in the second loop.
    ///
    def compileRuleIncr(rule: Constraint[v]): RamStmt[v] with ToString[v] = match rule {
        case Constraint(HeadAtom(PredSym(headPred), headDen, headTerms), body) =>
            let augBody = augmentBody(body);
            let env = unifyVars(augBody);
            let ramTerms = Array.map(compileHeadTerm(env), headTerms) as & Pure;
            let projection = RelOp.Project(ramTerms, RamSym.New(headPred, headDen));
            let join = compileBody(env, augBody);
            // TODO: make sure membership tests are checked last.
            let loopBody = RelOp.If(BoolExp.NotMemberOf(ramTerms, RamSym.Full(headPred, headDen)) :: join, projection);
            let compile = delta -> // `delta` designates the focused atom.
                Array.foldRight(match (atom, rowVar) -> acc -> match atom {
                    case BodyAtom(PredSym(pred), den, Polarity.Positive, _) =>
                        let ramSym = if (rowVar == delta) {
                            RamSym.Delta(pred, den)
                        } else {
                            RamSym.Full(pred, den)
                        };
                        RelOp.Search(rowVar, ramSym, acc)
                    case _ => acc
                }, loopBody, augBody) as & Pure |>
                RamStmt.Insert;
            let positiveAtoms = Array.filter(match (atom, _) -> match atom {
                case BodyAtom(_, _, Polarity.Positive, _) => true
                case _ => false
            }, augBody) as & Pure;
            let deltas = Array.fold(Array.map(match (_, rowVar) -> compile(rowVar), positiveAtoms)) as & Pure;
            if (enableDebugging()) {
                let comment = RamStmt.Comment(ToString.toString(rule));
                List.fold(comment :: deltas :: Nil)
            } else {
                deltas
            }
    }

    ///
    /// Substitute Datalog head term `term` for a RAM term according to the given substitution `env`.
    ///
    def compileHeadTerm(env: Map[VarSym, RamTerm[v]], term: HeadTerm[v]): RamTerm[v] = match term {
        case HeadTerm.Var(var) => unwrap(Map.get(var, env))
        case HeadTerm.Lit(v) => RamTerm.Lit(v)
        case HeadTerm.App0(f) => RamTerm.App0(f)
        case HeadTerm.App1(f, v) =>
            let t = unwrap(Map.get(v, env));
            RamTerm.App1(f, t)
        case HeadTerm.App2(f, v1, v2) =>
            let t1 = unwrap(Map.get(v1, env));
            let t2 = unwrap(Map.get(v2, env));
            RamTerm.App2(f, t1, t2)
        case HeadTerm.App3(f, v1, v2, v3) =>
            let t1 = unwrap(Map.get(v1, env));
            let t2 = unwrap(Map.get(v2, env));
            let t3 = unwrap(Map.get(v3, env));
            RamTerm.App3(f, t1, t2, t3)
        case HeadTerm.App4(f, v1, v2, v3, v4) =>
            let t1 = unwrap(Map.get(v1, env));
            let t2 = unwrap(Map.get(v2, env));
            let t3 = unwrap(Map.get(v3, env));
            let t4 = unwrap(Map.get(v4, env));
            RamTerm.App4(f, t1, t2, t3, t4)
        case HeadTerm.App5(f, v1, v2, v3, v4, v5) =>
            let t1 = unwrap(Map.get(v1, env));
            let t2 = unwrap(Map.get(v2, env));
            let t3 = unwrap(Map.get(v3, env));
            let t4 = unwrap(Map.get(v4, env));
            let t5 = unwrap(Map.get(v5, env));
            RamTerm.App5(f, t1, t2, t3, t4, t5)
    }

    ///
    /// Augment body atoms with row variables.
    /// Every atom is thus identified by a unique row variable, regardless of its position in the body.
    ///
    /// Note that row variables not unique across rules.
    ///
    def augmentBody(body: Array[BodyPredicate[v]]): Array[(BodyPredicate[v], RowVar)] =
        Array.mapWithIndex(atom -> i -> match atom {
            case BodyAtom(predSym, _, Polarity.Positive, _) => (atom, RowVar.Named("${predSym}$${i}"))
            case _ => (atom, RowVar.Named("IfYouSeeThisYouFoundABug"))
        }, body) as & Pure

    ///
    /// Unifies each relational variable that occurs in a rule body with an attribute
    /// corresponding to a positive atom where the variable is a term.
    /// Latticenal variables are unified with a "meet" term.
    /// For example, a body like `not A(x), B(x; s),  C(x; s).` returns the substitution:
    ///     `Map#{x -> B$1[0], s -> B$1[1] ⊓ C$2[1]}`
    /// `x` is mapped to B$1[0] because `x` occurs positively in the second atom.
    /// `s` is mapped to the glb of all its positive occurences because it is latticenal.
    ///
    def unifyVars(body: Array[(BodyPredicate[v], RowVar)]): Map[VarSym, RamTerm[v]] =
        Array.foldLeft(acc -> match (atom, rowVar) -> match atom {
            case BodyAtom(_, denotation, Polarity.Positive, terms) =>
                Array.mapWithIndex(term -> i -> (term, i), terms) |>
                Array.foldRight(match (term, i) -> match term {
                    case BodyTerm.Var(var) => match denotation {
                        case Denotation.Relational => Map.insert(var, RamTerm.RowLoad(rowVar, i))
                        case Denotation.Latticenal(_, _, _, glb) =>
                            if (i < Array.length(terms) - 1)
                                Map.insert(var, RamTerm.RowLoad(rowVar, i))
                            else
                                Map.insertWith(t1 -> t2 -> RamTerm.Meet(glb, t1, t2), var, RamTerm.LoadLatVar(rowVar))
                    }
                    case _ => x -> x
                }, acc)
            case _ => acc
        }, Map#{}, body) as & Pure

    ///
    /// Equates every term in a positive body atom with a RAM term corresponding to an attribute
    /// and declares that terms in negative body atoms are not in the corresponding relations.
    /// Latticenal terms are handled slightly differently: they are compared with the partial
    /// order on the lattice instead of with equality.
    /// Guard atoms are compiled to function calls.
    /// For example, a body like `not A(x), B(x), if x > 0.` is compiled to the following expressions:
    /// 1.    B$1[0] = B$1[0]
    /// 2.    (B$1[0]) ∉ A
    /// 3.    <clo>(x)
    /// Intuitively, the variable `x` has been unified with the term B$1[0]
    /// because `x` occurs positively in the second atom.
    /// All positive occurences of `x` is therefore equated with B$1[0], generating (1).
    /// (2) comes from the negative atom `not A(x)`.
    /// (3) is a function call that computes the expression `x > 0`.
    ///
    def compileBody(env: Map[VarSym, RamTerm[v]], body: Array[(BodyPredicate[v], RowVar)]): List[BoolExp[v]] =
        Array.foldRight(match (atom, rowVar) -> acc ->
            let compileBodyTerm = term -> j -> match term {
                case BodyTerm.Wild      => RamTerm.RowLoad(rowVar, j)
                case BodyTerm.Var(var)  => unwrap(Map.get(var, env))
                case BodyTerm.Lit(v)    => RamTerm.Lit(v)
            };
            match atom {
                case BodyAtom(_, denotation, Polarity.Positive, terms) =>
                    Array.mapWithIndex(compileBodyTerm, terms) |>
                    Array.mapWithIndex(t -> i -> (t, i)) |>
                    Array.foldRight(match (t, i) -> ts ->
                        let checkRamTerm = term -> match term {
                            case RamTerm.RowLoad(rVar, index) => 
                            if (rVar == rowVar and index == i) 
                                ts 
                            else BoolExp.Eq(RamTerm.RowLoad(rowVar, i), t) :: ts
                            case _ => BoolExp.Eq(RamTerm.RowLoad(rowVar, i), t) :: ts
                        };
                        match denotation {
                            case Denotation.Relational =>
                                checkRamTerm(t)
                            case Denotation.Latticenal(_, leq, _, _) =>
                                if (i < Array.length(terms) - 1)
                                    checkRamTerm(t)
                                else
                                    match t {
                                        case RamTerm.LoadLatVar(rVar) => if (rVar == rowVar) ts else BoolExp.Leq(leq, t, RamTerm.LoadLatVar(rowVar)) :: ts
                                        case _ => BoolExp.Leq(leq, t, RamTerm.LoadLatVar(rowVar)) :: ts
                                    }
                    }, acc)
                case BodyAtom(PredSym(pred), denotation, Polarity.Negative, terms) =>
                    let ramTerms = Array.mapWithIndex(compileBodyTerm, terms);
                    BoolExp.NotMemberOf(ramTerms, RamSym.Full(pred, denotation)) :: acc
                case Guard0(f) =>
                    BoolExp.Guard0(f) :: acc
                case Guard1(f, v) =>
                    let t = unwrap(Map.get(v, env));
                    BoolExp.Guard1(f, t) :: acc
                case Guard2(f, v1, v2) =>
                    let t1 = unwrap(Map.get(v1, env));
                    let t2 = unwrap(Map.get(v2, env));
                    BoolExp.Guard2(f, t1, t2) :: acc
                case Guard3(f, v1, v2, v3) =>
                    let t1 = unwrap(Map.get(v1, env));
                    let t2 = unwrap(Map.get(v2, env));
                    let t3 = unwrap(Map.get(v3, env));
                    BoolExp.Guard3(f, t1, t2, t3) :: acc
                case Guard4(f, v1, v2, v3, v4) =>
                    let t1 = unwrap(Map.get(v1, env));
                    let t2 = unwrap(Map.get(v2, env));
                    let t3 = unwrap(Map.get(v3, env));
                    let t4 = unwrap(Map.get(v4, env));
                    BoolExp.Guard4(f, t1, t2, t3, t4) :: acc
                case Guard5(f, v1, v2, v3, v4, v5) =>
                    let t1 = unwrap(Map.get(v1, env));
                    let t2 = unwrap(Map.get(v2, env));
                    let t3 = unwrap(Map.get(v3, env));
                    let t4 = unwrap(Map.get(v4, env));
                    let t5 = unwrap(Map.get(v5, env));
                    BoolExp.Guard5(f, t1, t2, t3, t4, t5) :: acc
            }, Nil, body) as & Pure

    ///
    /// Restrict a constraint system to those constraints whose head predicate belongs to the given domain.
    ///
    def restrict(cs: Array[Constraint[v]], domain: Set[PredSym]): Array[Constraint[v]] =
        let headPred = match Constraint(HeadAtom(headSym, _, _), _) -> headSym;
        let memberOf = xs -> x -> Set.memberOf(x, xs);
        Array.filter(headPred >> memberOf(domain), cs) as & Pure

    ///
    /// Return the set of predicates of the given constraint system.
    ///
    def predicatesOf(cs: Array[Constraint[v]]): Set[PredSym] =
        Array.foldRight(match Constraint(HeadAtom(headPred, _, _), _) -> Set.insert(headPred), Set#{}, cs) as & Pure

    def unwrap(o: Option[a]): a = match o {
        case Some(a) => a
        case None => bug!("Unexpected None")
    }
}

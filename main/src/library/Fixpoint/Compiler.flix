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

mod Fixpoint {

    use Fixpoint.Ram.{RamStmt, RelOp, RamSym, RamTerm, BoolExp, RowVar}
    use Fixpoint.Ast.{Datalog, Constraint, BodyPredicate, BodyTerm, HeadTerm, Denotation, Polarity, VarSym}
    use Fixpoint.Ast.Datalog.Datalog
    use Fixpoint.Ast.Constraint.Constraint
    use Fixpoint.Ast.HeadPredicate.HeadAtom
    use Fixpoint.Ast.BodyPredicate.{BodyAtom}
    use Fixpoint.Ast.BodyPredicate.{Guard0, Guard1, Guard2, Guard3, Guard4, Guard5}
    use Fixpoint.Ast.BodyPredicate.{Loop}
    use Fixpoint.Ast.isRelational
    use Fixpoint.Shared.PredSym
    use Fixpoint.Shared.PredSym.PredSym

    ///
    /// Compile the given Datalog program `d` to RAM.
    /// First compile all facts in `d` to project-statements.
    /// For example, the fact `Path(1, 2).` becomes `project (1, 2) into Path`.
    /// Second compile all rules in `d` according to the given stratification `strat`.
    /// This means to compile all rules in stratum 0, then stratum 1 and so on.
    /// A rule belongs to the same stratum as its head predicate.
    ///
    def compile(d: Datalog[v], strat: Map[PredSym, Int32]): RamStmt[v] = match d {
        case Datalog(facts, rules) => region r {
            let stmts = MutList.new(r);
            Vector.forEach(compileRule(stmts), facts);
            Vector.foldRight(rule -> match rule {
                case Constraint(HeadAtom(p, _, _), _) =>
                    let ruleStratum = Map.getWithDefault(p, 0, strat);
                    Map.insertWith(List.append, ruleStratum, rule :: Nil)
            }, Map#{}, rules) |>
            Map.forEach((_, s) -> compileStratum(stmts, s));
            RamStmt.Seq(MutList.toList(stmts))
        }
        case _ => bug!("Datalog normalization bug")
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
    def compileStratum(stmts: MutList[RamStmt[v], r], stratum: List[Constraint[v]]): Unit \ Write(r) = region r {
        let idb = List.foldRight(match Constraint(HeadAtom(pred, den, terms), _) -> {
            let arity = Vector.length(terms);
            Map.insert(pred, (arity, den))
        }, Map#{}, stratum);
        let loopBody = MutList.new(r);
        List.forEach(compileRule(stmts), stratum);
        Map.forEach(predSym -> match (arity, den) -> {
            let full = RamSym.Full(predSym, arity, den);
            let delta = RamSym.Delta(predSym, arity, den);
            let mergeDelta = RamStmt.Merge(full, delta);
            MutList.push!(mergeDelta, stmts)
        }, idb);
        Map.forEach(predSym -> match (arity, den) -> {
            let purge = RamStmt.Purge(RamSym.New(predSym, arity, den));
            MutList.push!(purge, loopBody)
        }, idb);
        List.forEach(compileRuleIncr(loopBody), stratum);
        Map.forEach(predSym -> match (arity, den) -> {
            let new = RamSym.New(predSym, arity, den);
            let full = RamSym.Full(predSym, arity, den);
            let mergeNew = RamStmt.Merge(new, full);
            MutList.push!(mergeNew, loopBody)
        }, idb);
        Map.forEach(predSym -> match (arity, den) -> {
            let delta = RamSym.Delta(predSym, arity, den);
            let new = RamSym.New(predSym, arity, den);
            let assign = RamStmt.Assign(delta, new);
            MutList.push!(assign, loopBody)
        }, idb);
        let loopTest =
            Map.mapWithKey(predSym -> match (arity, den) -> {
                BoolExp.Empty(RamSym.Delta(predSym, arity, den))
            }, idb) |>
            Map.valuesOf;
        let untilBody = RamStmt.Seq(MutList.toList(loopBody));
        let fixpoint = RamStmt.Until(loopTest, untilBody);
        MutList.push!(fixpoint, stmts)
    }

    ///
    /// Compile `rule` into a join loop. A join loop is a nested loop.
    /// Each positive body atom in `rule` becomes part of the nested loop.
    /// The head atom in `rule` is compiled to an insertion into the corresponding relation.
    /// Negative atoms are compiled into if-statements.
    /// The rule `R(x) :- not A(x), B(x),  C(x).` compiles to:
    ///     search B$1 ∈ B do
    ///         search C$2 ∈ C do
    ///             if (B$1[0] == C$2[0] ∧ (B$1[0]) ∉ A) then
    ///                 project (B$1[0]) into R
    ///             end
    ///         end
    ///     end
    ///
    def compileRule(stmts: MutList[RamStmt[v], r], rule: Constraint[v]): Unit \ Write(r) = match rule {
        case Constraint(HeadAtom(headSym, headDen, headTerms), body) =>
            let augBody = augmentBody(body);
            let env = unifyVars(augBody);
            let ramTerms = Vector.map(compileHeadTerm(env), headTerms);
            let arity = Vector.length(ramTerms);
            let projection = RelOp.Project(ramTerms, RamSym.Full(headSym, arity, headDen));
            let join = compileBody(env, augBody);
            let loopBody = RelOp.If(join, projection);
            let insert =
                Vector.foldRight(match (atom, rowVar) -> acc -> match atom {
                    case BodyAtom(bodySym, den, Polarity.Positive, _, terms) =>
                        let arity1 = Vector.length(terms);
                        RelOp.Search(rowVar, RamSym.Full(bodySym, arity1, den), acc)
                    case _ => acc
                }, loopBody, augBody) |>
                RamStmt.Insert;
            if (enableDebugging()) {
                let comment = RamStmt.Comment(ToString.toString(rule));
                MutList.push!(comment, stmts);
                MutList.push!(insert, stmts)
            } else {
                MutList.push!(insert, stmts)
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
    ///         search C$2 ∈ C do
    ///             if (B$1[0] == C$2[0] ∧ (B$1[0]) ∉ A ∧ (B$1[0]) ∉ R) then
    ///                 project (B$1[0]) into ΔR'
    ///             end
    ///         end
    ///     end
    ///     search B$1 ∈ B do
    ///         search C$2 ∈ ΔC do
    ///             if (B$1[0] == C$2[0] ∧ (B$1[0]) ∉ A ∧ (B$1[0]) ∉ R) then
    ///                 project (B$1[0]) into ΔR'
    ///             end
    ///         end
    ///     end
    ///
    /// Note that there are two join loops, because there are two positive atoms.
    /// Also note how in the first loop, `B` is the focused atom and `C` is focused in the second loop.
    ///
    def compileRuleIncr(stmts: MutList[RamStmt[v], r], rule: Constraint[v]): Unit \ Write(r) = match rule {
        case Constraint(HeadAtom(headSym, headDen, headTerms), body) =>
            let augBody = augmentBody(body);
            let env = unifyVars(augBody);
            let ramTerms = Vector.map(compileHeadTerm(env), headTerms);
            let arity = Vector.length(ramTerms);
            let projection = RelOp.Project(ramTerms, RamSym.New(headSym, arity, headDen));
            let join = compileBody(env, augBody);
            let loopBody = RelOp.If(Vector.append(Vector.singleton(BoolExp.NotMemberOf(ramTerms, RamSym.Full(headSym, arity, headDen))), join), projection);
            let compile = delta -> { // `delta` designates the focused atom.
                let insert =
                    Vector.foldRight(match (atom, rowVar) -> acc -> match atom {
                        case BodyAtom(bodySym, den, Polarity.Positive, _, terms) =>
                            let arity1 = Vector.length(terms);
                            let ramSym = if (rowVar == delta and isRelational(den)) {
                                RamSym.Delta(bodySym, arity1, den)
                            } else {
                                RamSym.Full(bodySym, arity1, den)
                            };
                            RelOp.Search(rowVar, ramSym, acc)
                        case _ => acc
                    }, loopBody, augBody) |>
                    RamStmt.Insert;
                MutList.push!(insert, stmts)
            };

            let positiveAtoms = Vector.filter(match (atom, _) -> match atom {
                case BodyAtom(_, _, Polarity.Positive, _, _) => true
                case _ => false
            }, augBody);
            if (enableDebugging()) {
                let comment = RamStmt.Comment(ToString.toString(rule));
                MutList.push!(comment, stmts);
                Vector.forEach(match (_, rowVar) -> compile(rowVar), positiveAtoms)
            } else {
                Vector.forEach(match (_, rowVar) -> compile(rowVar), positiveAtoms)
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
    def augmentBody(body: Vector[BodyPredicate[v]]): Vector[(BodyPredicate[v], RowVar)] =
        Vector.mapWithIndex(i -> atom -> match atom {
            case BodyAtom(predSym, _, Polarity.Positive, _, _) => (atom, RowVar.Named("${predSym}$${i}"))
            case _ => (atom, RowVar.Named("IfYouSeeThisYouFoundABug"))
        }, body)

    ///
    /// Unifies each relational variable that occurs in a rule body with an attribute
    /// corresponding to a positive atom where the variable is a term.
    /// Latticenal variables are unified with a "meet" term.
    /// For example, a body like `not A(x), B(x; s),  C(x; s).` returns the substitution:
    ///     `Map#{x -> B$1[0], s -> B$1[1] ⊓ C$2[1]}`
    /// `x` is mapped to B$1[0] because `x` occurs positively in the second atom.
    /// `s` is mapped to the glb of all its positive occurences because it is latticenal.
    ///
    def unifyVars(body: Vector[(BodyPredicate[v], RowVar)]): Map[VarSym, RamTerm[v]] =
        Vector.foldLeft(acc -> match (atom, rowVar) -> match atom {
            case BodyAtom(_, denotation, Polarity.Positive, _, terms) =>
                Vector.mapWithIndex(i -> term -> (term, i), terms) |>
                Vector.foldRight(match (term, i) -> match term {
                    case BodyTerm.Var(var) => match denotation {
                        case Denotation.Relational =>
                            Map.insertWith(_ -> t -> t, var, RamTerm.RowLoad(rowVar, i))
                        case Denotation.Latticenal(_, _, _, glb) =>
                            if (i < Vector.length(terms) - 1)
                                Map.insertWith(_ -> t -> t, var, RamTerm.RowLoad(rowVar, i))
                            else
                                let f = t1 -> t2 -> RamTerm.Meet(glb, t1, t2);
                                Map.insertWith(f, var, RamTerm.LoadLatVar(rowVar))
                    }
                    case _ => x -> x
                }, acc)
            case _ => acc
        }, Map#{}, body)

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
    def compileBody(env: Map[VarSym, RamTerm[v]], body: Vector[(BodyPredicate[v], RowVar)]): Vector[BoolExp[v]] =
        Vector.foldRight(match (atom, rowVar) -> acc ->
            let compileBodyTerm = j -> term -> match term {
                case BodyTerm.Wild      => RamTerm.RowLoad(rowVar, j)
                case BodyTerm.Var(var)  => unwrap(Map.get(var, env))
                case BodyTerm.Lit(v)    => RamTerm.Lit(v)
            };
            match atom {
                case BodyAtom(_, denotation, Polarity.Positive, _, terms) =>
                    Vector.mapWithIndex(compileBodyTerm, terms) |>
                    Vector.mapWithIndex(i -> t -> match denotation {
                        case Denotation.Relational => BoolExp.Eq(RamTerm.RowLoad(rowVar, i), t)
                        case Denotation.Latticenal(_, leq, _, _) =>
                            if (i < Vector.length(terms) - 1)
                                BoolExp.Eq(RamTerm.RowLoad(rowVar, i), t)
                            else
                                BoolExp.Leq(leq, t, RamTerm.LoadLatVar(rowVar))
                    }) |>
                    Vector.foldRight(x -> xs -> Vector.append(Vector.singleton(x), xs), acc)
                case BodyAtom(bodySym, denotation, Polarity.Negative, _, terms) =>
                    let ramTerms = Vector.mapWithIndex(compileBodyTerm, terms);
                    let arity = Vector.length(ramTerms);
                    Vector.append(
                        Vector.singleton(BoolExp.NotMemberOf(ramTerms, RamSym.Full(bodySym, arity, denotation))),
                        acc
                    )
                case Guard0(f) =>
                    Vector.append(Vector.singleton(BoolExp.Guard0(f)), acc)
                case Guard1(f, v) =>
                    let t = unwrap(Map.get(v, env));
                    Vector.append(Vector.singleton(BoolExp.Guard1(f, t)), acc)
                case Guard2(f, v1, v2) =>
                    let t1 = unwrap(Map.get(v1, env));
                    let t2 = unwrap(Map.get(v2, env));
                    Vector.append(Vector.singleton(BoolExp.Guard2(f, t1, t2)), acc)
                case Guard3(f, v1, v2, v3) =>
                    let t1 = unwrap(Map.get(v1, env));
                    let t2 = unwrap(Map.get(v2, env));
                    let t3 = unwrap(Map.get(v3, env));
                    Vector.append(Vector.singleton(BoolExp.Guard3(f, t1, t2, t3)), acc)
                case Guard4(f, v1, v2, v3, v4) =>
                    let t1 = unwrap(Map.get(v1, env));
                    let t2 = unwrap(Map.get(v2, env));
                    let t3 = unwrap(Map.get(v3, env));
                    let t4 = unwrap(Map.get(v4, env));
                    Vector.append(Vector.singleton(BoolExp.Guard4(f, t1, t2, t3, t4)), acc)
                case Guard5(f, v1, v2, v3, v4, v5) =>
                    let t1 = unwrap(Map.get(v1, env));
                    let t2 = unwrap(Map.get(v2, env));
                    let t3 = unwrap(Map.get(v3, env));
                    let t4 = unwrap(Map.get(v4, env));
                    let t5 = unwrap(Map.get(v5, env));
                    Vector.append(Vector.singleton(BoolExp.Guard5(f, t1, t2, t3, t4, t5)), acc)
                case Loop(_) =>
                    ??? // TODO
            }, Vector.empty(), body)

    def unwrap(o: Option[a]): a = match o {
        case Some(a) => a
        case None => bug!("Unexpected None")
    }
}

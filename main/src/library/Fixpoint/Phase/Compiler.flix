/*
 * Copyright 2021 Benjamin Dahse
 * Copyright 2025 Casper Dalgaard Nielsen
 *                Adam Yasser Tallouzi
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

mod Fixpoint.Phase.Compiler {
    use Fixpoint.Ast.Ram.{RamStmt, RelOp, RelSym, RamTerm, BoolExp, RowVar, Facts, PredicateState, RamProgram, RamId, arityOfNonLat}
    use Fixpoint.Ast.Datalog.{Datalog, Constraint, BodyPredicate, BodyTerm, HeadTerm, Polarity, Fixity, VarSym}
    use Fixpoint.Ast.Datalog.Datalog.{Datalog}
    use Fixpoint.Ast.Datalog.Constraint.Constraint
    use Fixpoint.Ast.Datalog.HeadPredicate.HeadAtom
    use Fixpoint.Ast.Datalog.BodyPredicate.{Guard0, Guard1, Guard2, Guard3, Guard4, Guard5, Functional, BodyAtom}
    use Fixpoint.Ast.Shared.Denotation
    use Fixpoint.Ast.Shared.isRelational
    use Fixpoint.Ast.Shared.PredSym
    use Fixpoint.Ast.Shared.PredSym.PredSym
    use Fixpoint.Boxed
    use Fixpoint.Counter
    use Fixpoint.Predicates
    use Fixpoint.Predicates.{PredSymMapInternal}
    use Fixpoint.Predicates.PredType.{Full, Delta, New}

    // This should (hopefully) be fine. The symbol should not be used for anything.
    pub def functionalRelSym(arity: Int32): RelSym = RelSym.Symbol(PredSym.PredSym("", -1i64), arity, Denotation.Relational)
    ///
    /// Compile the given Datalog program `d` to RAM.
    /// First compile all facts in `d` to project-statements.
    /// For example, the fact `Path(1, 2).` becomes `project (1, 2) into Path`.
    /// Second compile all rules in `d` according to the given stratification `strat`.
    /// This means to compile all rules in stratum 0, then stratum 1 and so on.
    /// A rule belongs to the same stratum as its head predicate.
    ///
    @Internal
    pub def compile(d: Datalog, db: Map[RelSym, Map[Vector[Boxed], Boxed]], strat: Map[PredSym, Int32]): RamProgram = match d {
        case Datalog(facts, rules) => region rc {
            let predSymMaps = Predicates.initialize(d, db);
            let stmts = MutList.empty(rc);
            let compiledFacts = compileFacts(facts, db, predSymMaps);
            // Create map of stratum to list of rules in stratum
            let counter = Counter.fresh(rc);
            Vector.foldRight(rule -> match rule {
                case Constraint(HeadAtom(p, _, _), _) =>
                    let ruleStratum = Map.getWithDefault(p, 0, strat);
                    Map.insertWith(List.append, ruleStratum, rule :: Nil)
            }, Map#{}, rules) |>
            Map.forEach((_, s) -> compileStratum(stmts, s, predSymMaps, counter));
            RamProgram.Program(RamStmt.Seq(MutList.toVector(stmts)), compiledFacts, predSymMaps, (Map.empty(), Map.empty()))
        }
        case _ => bug!("Datalog Boxing bug")
    }

    ///
    /// Collect all facts by their predicate
    ///
    def compileFacts(facts: Vector[Constraint], db: Map[RelSym, Map[Vector[Boxed], Boxed]], predSymMaps: PredSymMapInternal): Facts =
        let startingPoint = Map.mapWithKey(match RelSym.Symbol(_, _, den) -> values -> match isRelational(den) {
            case true => Map.foldWithKey(acc -> k -> _ -> k :: acc, Nil, values)
            case false => Map.foldWithKey(acc -> k -> v -> Vector.append(k, Vector#{v}) :: acc, Nil, values)
        }, db);

        let test =
            Vector.foldRight(a -> b -> {
                match a {
                    case Constraint(HeadAtom(predSym, _, headTerms), _) => 
                        let x = Vector.map(getFactTerm, headTerms);
                        let sym = Predicates.getNewRel(predSymMaps, predSym, Full);
                        Map.insertWith(List.append, sym, x :: Nil, b)
                }
            }, startingPoint, facts);
        Map.map(List.toVector, test)


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
    def compileStratum(stmts: MutList[RamStmt, r1], stratum: List[Constraint], predSymMaps: PredSymMapInternal, counter: Counter.Counter[r2]): Unit \ r1 + r2 = region rc {
        let idb = List.foldRight(match Constraint(HeadAtom(pred, den, terms), _) -> {
            let arity = Vector.length(terms);
            Map.insert(pred, (arity, den))
        }, Map#{}, stratum);
        let idbIds = List.foldRight(match Constraint(HeadAtom(PredSym(_, id), _, _), _) -> {
            Set.insert(id)
        }, Set#{}, stratum);

        // Construct the inserts for step 1.
        let inserts = MutList.empty(rc);
        List.forEach(x -> compileRule(inserts, x, predSymMaps, counter), stratum);
        let par1 = 
            if(MutList.size(inserts) > 1) {
                RamStmt.Par(MutList.toVector(inserts))
            } else if (MutList.size(inserts) == 1){
                unwrap(MutList.head(inserts))
            } else {
                bug!("Bug in Fixpoint.Compiler.compileStratum: Stratum had no PredSyms")
            };
        MutList.push(par1, stmts);

        // Construct the merges for step 1.
        MutList.push(constructMerges(idb, predSymMaps, true), stmts);

        // Construct the purges for step 1.
        let loopBody = MutList.empty(rc);
        Map.forEach(predSym -> match (_arity, _den) -> {
            let purge = RamStmt.Purge(Predicates.getNewRel(predSymMaps, predSym, New));
            MutList.push(purge, loopBody)
        }, idb);

        let loopInserts = MutList.empty(rc);
        List.forEach(x -> compileRuleIncr(loopInserts, x, idbIds, predSymMaps, counter), stratum);
        let loopParOpt = 
            if (MutList.size(loopInserts) > 1) {
                Some(RamStmt.Par(MutList.toVector(loopInserts)))
            } else {
                MutList.head(loopInserts)
            };
        match loopParOpt {
            case None => ()
            case Some(loopPar) => 
                MutList.push(loopPar, loopBody);
                MutList.push(constructMerges(idb, predSymMaps, false), loopBody);
                Map.forEach(predSym -> match (_arity, _den) -> {
                    let delta = Predicates.getNewRel(predSymMaps, predSym, Delta);
                    let newr = Predicates.getNewRel(predSymMaps, predSym, New);
                    let swap = RamStmt.Swap(newr, delta);
                    MutList.push(swap, loopBody)
                }, idb);

                let loopTest =
                    idb |> Map.mapWithKey(predSym -> match (_arity, _den) ->
                        BoolExp.IsEmpty(Predicates.getNewRel(predSymMaps, predSym, Delta))
                    ) |>
                    Map.valuesOf |>
                    List.toVector;
                let untilBody = RamStmt.Seq(MutList.toVector(loopBody));
                let fixpoint = RamStmt.Until(loopTest, untilBody);
                MutList.push(fixpoint, stmts)
        }
    }

    def constructMerges(idb: Map[PredSym, (Int32, Denotation)], predSymMaps: PredSymMapInternal, mergeIntoDelta: Bool): RamStmt = region rc {
        let merges = MutList.empty(rc);
        Map.forEach(predSym -> match (_arity, _den) -> {
            let newRel = Predicates.getNewRel(predSymMaps, predSym, New);
            let deltaRel = Predicates.getNewRel(predSymMaps, predSym, Delta);
            let fullRel = Predicates.getNewRel(predSymMaps, predSym, Full);
            let mergeFull = RamStmt.Merge(newRel, fullRel);
            MutList.push(mergeFull, merges);
            if(mergeIntoDelta) {
            let mergeDelta = RamStmt.Merge(newRel, deltaRel);
                MutList.push(mergeDelta, merges)
            } else ()
        }, idb);
        if(MutList.size(merges) > 1)
            RamStmt.Par(MutList.toVector(merges))
        else if (MutList.size(merges) == 1)
            unwrap(MutList.head(merges))
        else
            bug!("Bug in Fixpoint.Compiler: Stratum error. Stratum had no PredSyms")
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
    def compileRule(stmts: MutList[RamStmt, r1], rule: Constraint, predSymMaps: PredSymMapInternal, counter: Counter.Counter[r2]): Unit \ r1 + r2 = match rule {
        case Constraint(HeadAtom(headSym, _, headTerms), body) =>
            let augBody = augmentBody(body, counter);
            let env = unifyVars(predSymMaps, counter, augBody);
            let ramTerms = Vector.map(compileHeadTerm(env, counter), headTerms);
            let projection = RelOp.Project(ramTerms, Predicates.getNewRel(predSymMaps, headSym, New));
            let join = compileBody(env, augBody, predSymMaps, counter);
            let loopBody = RelOp.If(join, projection);
            let insert =
                Vector.foldRight(match (atom, rowVar) -> acc -> match atom {
                    case BodyAtom(bodySym, _, Polarity.Positive, _, _) =>
                        RelOp.Search(rowVar, Predicates.getNewRel(predSymMaps, bodySym, Full), acc)
                    case Functional(boundVars, f, freeVars) =>
                        let terms = Vector.map(v -> unwrap(Map.get(v, env)), freeVars);
                        RelOp.Functional(rowVar, f, terms, acc, Vector.length(boundVars))
                    case _ => acc
                }, loopBody, augBody) |>
                RamStmt.Insert;
            if (Fixpoint.Options.enableDebugging()) {
                let comment = RamStmt.Comment(ToString.toString(rule));
                MutList.push(comment, stmts);
                MutList.push(insert, stmts)
            } else {
                MutList.push(insert, stmts)
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



    // Note: This has been modified to only generate using the delta-relations for the current strata.
    // Reasoning: The older delta-relations will all be empty so it is a simple optimization that can also allow optimizations for index selection (of the delta relations).

    def compileRuleIncr(stmts: MutList[RamStmt, r1], rule: Constraint, idbSet: Set[Int64], predSymMaps: PredSymMapInternal, counter: Counter.Counter[r2]): Unit \ r1 + r2 = match rule {
        case Constraint(HeadAtom(headSym, _, headTerms), body) =>
            let compileSomething = deltaIndex -> { // `delta` designates the focused atom.
                let augBody = augmentBody(body, counter);
                let delta = snd(Vector.get(deltaIndex, augBody));
                let env = unifyVars(predSymMaps, counter, augBody);
                let ramTerms = Vector.map(compileHeadTerm(env, counter), headTerms);
                let projection = RelOp.Project(ramTerms, Predicates.getNewRel(predSymMaps, headSym, New));
                let join = compileBody(env, augBody, predSymMaps, counter);
                let relSym = Predicates.getNewRel(predSymMaps, headSym, Full);
                let loopBody = RelOp.If(
                    Vector.append(Vector#{BoolExp.NotMemberOf(ramTerms, relSym)}, join), 
                    projection
                );
                let insert =
                    Vector.foldRight(match (atom, rowVar) -> acc -> match atom {
                        case BodyAtom(bodySym, den, Polarity.Positive, _, _) =>
                            let ramSym = if (rowVar == delta and isRelational(den)) {
                                Predicates.getNewRel(predSymMaps, bodySym, Delta)
                            } else {
                                Predicates.getNewRel(predSymMaps, bodySym, Full)
                            };
                            RelOp.Search(rowVar, ramSym, acc)
                        case Functional(boundVars, f, freeVars) =>
                            let terms = Vector.map(v -> unwrap(Map.get(v, env)), freeVars);
                            RelOp.Functional(rowVar, f, terms, acc, Vector.length(boundVars))
                        case _ => acc
                    }, loopBody, augBody) |>
                    RamStmt.Insert;
                MutList.push(insert, stmts)
            };

            let positiveIDBAtoms = Vector.mapWithIndex(i -> atom -> match atom {
                case BodyAtom(PredSym(_, id), _, Polarity.Positive, Fixity.Loose, _) =>
                    if(Set.memberOf(id, idbSet)) {
                        i
                    } else {
                        -1
                    }
                case _ => -1
            }, body) |> 
            Vector.filter(i -> i != -1);

            if (Fixpoint.Options.enableDebugging()) {
                let comment = RamStmt.Comment(ToString.toString(rule));
                MutList.push(comment, stmts);
                Vector.forEach(match rowVar -> compileSomething(rowVar), positiveIDBAtoms)
            } else {
                Vector.forEach(match rowVar -> compileSomething(rowVar), positiveIDBAtoms)
            }
    }

    ///
    /// Substitute Datalog head term `term` for a RAM term according to the given substitution `env`.
    ///
    def compileHeadTerm(env: Map[VarSym, RamTerm], counter: Counter.Counter[r], term: HeadTerm): RamTerm \ r = match term {
        case HeadTerm.Var(var) => unwrap(Map.get(var, env))
        case HeadTerm.Lit(v) => RamTerm.Lit(v, RamId.Id(Counter.getNewID(counter)))
        case HeadTerm.App0(f) => RamTerm.App0(f, RamId.Id(Counter.getNewID(counter)))
        case HeadTerm.App1(f, v) =>
            let t = unwrap(Map.get(v, env));
            RamTerm.App1(f, t, RamId.Id(Counter.getNewID(counter)))
        case HeadTerm.App2(f, v1, v2) =>
            let t1 = unwrap(Map.get(v1, env));
            let t2 = unwrap(Map.get(v2, env));
            RamTerm.App2(f, t1, t2, RamId.Id(Counter.getNewID(counter)))
        case HeadTerm.App3(f, v1, v2, v3) =>
            let t1 = unwrap(Map.get(v1, env));
            let t2 = unwrap(Map.get(v2, env));
            let t3 = unwrap(Map.get(v3, env));
            RamTerm.App3(f, t1, t2, t3, RamId.Id(Counter.getNewID(counter)))
        case HeadTerm.App4(f, v1, v2, v3, v4) =>
            let t1 = unwrap(Map.get(v1, env));
            let t2 = unwrap(Map.get(v2, env));
            let t3 = unwrap(Map.get(v3, env));
            let t4 = unwrap(Map.get(v4, env));
            RamTerm.App4(f, t1, t2, t3, t4, RamId.Id(Counter.getNewID(counter)))
        case HeadTerm.App5(f, v1, v2, v3, v4, v5) =>
            let t1 = unwrap(Map.get(v1, env));
            let t2 = unwrap(Map.get(v2, env));
            let t3 = unwrap(Map.get(v3, env));
            let t4 = unwrap(Map.get(v4, env));
            let t5 = unwrap(Map.get(v5, env));
            RamTerm.App5(f, t1, t2, t3, t4, t5, RamId.Id(Counter.getNewID(counter)))
    }

    ///
    /// Augment body atoms with RowVars.
    /// Every atom is thus identified by a unique RowVar, regardless of its position in the body.
    ///
    def augmentBody(body: Vector[BodyPredicate], counter: Counter.Counter[r]): Vector[(BodyPredicate, RowVar)] \ r =
        Vector.map(atom -> match atom {
            case BodyAtom(_, _, Polarity.Positive, _, _) => (atom, RowVar.Named(Counter.getNewID(counter)))
            case Functional(_, _, _)                     => (atom, RowVar.Named(Counter.getNewID(counter)))
            case _ => (atom, RowVar.Named(-1))
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
    def unifyVars(predSymMaps: PredSymMapInternal, counter: Counter.Counter[r], body: Vector[(BodyPredicate, RowVar)]): Map[VarSym, RamTerm] \ r =
        let toFullSym = sym -> (Predicates.getNewRel(predSymMaps, sym, Full));
        let innerFunc = {
            acc -> atomVar -> 
            let (atom, rowVar) = atomVar;
            match atom {
            case BodyAtom(bodySym, denotation, Polarity.Positive, _, terms) =>
                Vector.mapWithIndex(i -> term -> (term, i), terms) |>
                Vector.foldRight(termI -> acc1 ->
                let (term, i) = termI;
                match term {
                    case BodyTerm.Var(var) => match denotation {
                        case Denotation.Relational =>
                            Map.insertWith(_ -> t -> t, var, RamTerm.RowLoad(rowVar, i, toFullSym(bodySym)), acc1)
                        case Denotation.Latticenal(_, _, _, glb) =>
                            if (i < Vector.length(terms) - 1)
                                Map.insertWith(_ -> t -> t, var, RamTerm.RowLoad(rowVar, i, toFullSym(bodySym)), acc1)
                            else
                                let f = _ -> t2 -> RamTerm.Meet(glb, t2, (rowVar, toFullSym(bodySym)), RamId.Id(Counter.getNewID(counter)));
                                Map.insertWith(f, var, RamTerm.RowLoad(rowVar, i, toFullSym(bodySym)), acc1)
                    }
                    case _ => acc1
                }, acc)
            case BodyAtom(_, _, Polarity.Negative, _, _) => acc
            case Functional(boundVars, _, freeVars) =>
                let fffff = Vector.mapWithIndex(i -> var -> (var, i), boundVars);
                let res = Vector.foldRight(match (var, i) -> Map.insertWith(_ -> t -> t, var, RamTerm.RowLoad(rowVar, i, functionalRelSym(Vector.length(freeVars)))), acc, fffff);
                res
            case Guard0(_)                               => acc
            case Guard1(_, _)                            => acc
            case Guard2(_, _, _)                         => acc
            case Guard3(_, _, _, _)                      => acc
            case Guard4(_, _, _, _, _)                   => acc
            case Guard5(_, _, _, _, _, _)                => acc
            case _ => acc
        }};
        Vector.foldLeft(innerFunc, Map#{}, body)

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
    def compileBody(env: Map[VarSym, RamTerm], body: Vector[(BodyPredicate, RowVar)], predSymMaps: PredSymMapInternal, counter: Counter.Counter[r]): Vector[BoolExp] \ r =
        let toFullSym = sym -> (Predicates.getNewRel(predSymMaps, sym, Full));
        Vector.flatMap(match (atom, rowVar) ->
            let compileBodyTerm = oldSym -> j -> term -> match term {
                case BodyTerm.Wild      => RamTerm.RowLoad(rowVar, j, toFullSym(oldSym))
                case BodyTerm.Var(var)  => unwrap(Map.get(var, env))
                case BodyTerm.Lit(v)    => RamTerm.Lit(v, RamId.Id(Counter.getNewID(counter)))
            };
            match atom {
                case BodyAtom(bodySym, denotation, Polarity.Positive, _, terms) =>
                    Vector.mapWithIndex(compileBodyTerm(bodySym), terms) |>
                    Vector.zip(Vector.range(0, Vector.length(terms))) |>
                    Vector.map(match (i, t) -> match denotation {
                        case Denotation.Relational => 
                            BoolExp.Eq(RamTerm.RowLoad(rowVar, i, toFullSym(bodySym)), t)
                        case Denotation.Latticenal(bot, leq, _, _) =>
                            if (i < Vector.length(terms) - 1)
                                BoolExp.Eq(RamTerm.RowLoad(rowVar, i, toFullSym(bodySym)), t)
                            else
                            // 2 cases: Either we have P(;x), Q(;x) in which case we ensure x is not bottom.
                            // Otherwise we have P(;12) in which case we ensure that the read value is less than or equal to 12.
                                compileBoolForLatticeenal(t, leq, bot, rowVar, toFullSym(bodySym))
                                // BoolExp.NotBot(t, leq, bot)
                    })
                case BodyAtom(bodySym, _, Polarity.Negative, _, terms) =>
                    let ramTerms = Vector.mapWithIndex(compileBodyTerm(bodySym), terms);
                    Vector#{BoolExp.NotMemberOf(ramTerms, Predicates.getNewRel(predSymMaps, bodySym, Full))}
                case Functional(_, _, _) => Vector.empty()
                case Guard0(f) =>
                    Vector#{BoolExp.Guard0(f)}
                case Guard1(f, v) =>
                    let t = unwrap(Map.get(v, env));
                    Vector#{BoolExp.Guard1(f, t)}
                case Guard2(f, v1, v2) =>
                    let t1 = unwrap(Map.get(v1, env));
                    let t2 = unwrap(Map.get(v2, env));
                    Vector#{BoolExp.Guard2(f, t1, t2)}
                case Guard3(f, v1, v2, v3) =>
                    let t1 = unwrap(Map.get(v1, env));
                    let t2 = unwrap(Map.get(v2, env));
                    let t3 = unwrap(Map.get(v3, env));
                    Vector#{BoolExp.Guard3(f, t1, t2, t3)}
                case Guard4(f, v1, v2, v3, v4) =>
                    let t1 = unwrap(Map.get(v1, env));
                    let t2 = unwrap(Map.get(v2, env));
                    let t3 = unwrap(Map.get(v3, env));
                    let t4 = unwrap(Map.get(v4, env));
                    Vector#{BoolExp.Guard4(f, t1, t2, t3, t4)}
                case Guard5(f, v1, v2, v3, v4, v5) =>
                    let t1 = unwrap(Map.get(v1, env));
                    let t2 = unwrap(Map.get(v2, env));
                    let t3 = unwrap(Map.get(v3, env));
                    let t4 = unwrap(Map.get(v4, env));
                    let t5 = unwrap(Map.get(v5, env));
                    Vector#{BoolExp.Guard5(f, t1, t2, t3, t4, t5)}
            }, body)

    // Compile the RamTerm `t` to either a Leq or NotBot depending on usage.
    def compileBoolForLatticeenal(t: RamTerm, leq: Boxed -> Boxed -> Bool, bot: Boxed, rv: RowVar, relSym: RelSym): BoolExp = match t {
        case RamTerm.Lit(val, _) =>
            BoolExp.Leq(val, rv, relSym)
        case _ => BoolExp.NotBot(t, leq, bot)
    }

    def unwrap(o: Option[a]): a = match o {
        case Some(a) => a
        case None => bug!("Unexpected None")
    }

    def getFactTerm(term: HeadTerm): Boxed = match term {
            case HeadTerm.Lit(v) => v
            case _ => bug!("Facts should only consist of literals")
        }
    
}

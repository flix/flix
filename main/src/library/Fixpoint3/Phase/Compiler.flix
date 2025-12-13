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

pub mod Fixpoint3.Phase.Compiler {
    use Fixpoint3.Ast.Ram.{arityOfNonLat, BoolExp, Facts, Predicates, RamProgram, RamStmt, RamTerm, RelOp, RelSym, RowVar}
    use Fixpoint3.Ast.Datalog.{BodyPredicate, BodyTerm, Constraint, Datalog, Fixity, HeadTerm, Polarity, VarSym}
    use Fixpoint3.Ast.Datalog.Datalog.{Datalog}
    use Fixpoint3.Ast.Datalog.Constraint.Constraint
    use Fixpoint3.Ast.Datalog.HeadPredicate.HeadAtom
    use Fixpoint3.Ast.Datalog.BodyPredicate.{BodyAtom, Functional, Guard0, Guard1, Guard2, Guard3, Guard4, Guard5}
    use Fixpoint3.Ast.Shared.{BoxedDenotation => Denotation}
    use Fixpoint3.Ast.Shared.Denotation.{Relational, Latticenal}
    use Fixpoint3.Ast.Shared.isRelational
    use Fixpoint3.Ast.Shared.PredSym
    use Fixpoint3.Ast.Shared.PredSym.PredSym
    use Fixpoint3.Boxed
    use Fixpoint3.Counter
    use Fixpoint3.Options.usedArity
    use Fixpoint3.Predicate
    use Fixpoint3.Predicate.PredType.{Full, Delta, New}
    use Fixpoint3.TypeInfo.{getType, getTypeOf, Type, typeDatalog, TypeInformation}
    use Fixpoint3.Util.getOrCrash

    ///
    /// Compile the given Datalog program `d` to RAM.
    /// First insert all facts in `d` into the set of facts.
    /// Second compile all rules in `d` according to the given stratification `strat`.
    /// This means to compile all rules in stratum 0, then stratum 1 and so on.
    /// A rule belongs to the same stratum as its head predicate.
    ///
    pub def compile(d: Datalog, db: Map[RelSym, BPlusTree[Vector[Boxed], Boxed, Static]], strat: Map[PredSym, (Int32, Int32)], withProv: Bool): RamProgram = match d {
        case Datalog(facts, _) => region rc {
            let predicates = Predicate.initialize(d, db);
            let stmts = MutList.empty(rc);
            let compiledFacts = unsafe IO { compileFacts(facts, db, predicates) };
            // Create map of stratum to list of rules in stratum
            let counter = Counter.fresh(rc);
            let rulesWithId = enumerateRules(d);
            let pseudoStratums = MutMap.empty(rc);
            foreach ((num, rule) <- rulesWithId) {
                match rule {
                    case Constraint(HeadAtom(p, _, _), _) =>
                        let (pseudoStratum, stratum) = Map.getWithDefault(p, (0, 0), strat);
                        let pseudoMap = MutMap.getOrElsePut(pseudoStratum, MutMap.empty(rc), pseudoStratums);
                        MutMap.putWith(List.append, stratum, (num, rule) :: Nil, pseudoMap)
                }
            };
            let typeInfo = typeDatalog(d, pseudoStratums, predicates, withProv);
            let functionalCounter = Counter.fresh(rc);
            Counter.set(getOrCrash(Int64.tryToInt32(Predicate.getMax(predicates))), functionalCounter);
            foreach ((_, pseudStrat) <- pseudoStratums) {
                    let runInParallel = (Nil, pseudStrat) ||>
                        MutMap.foldLeft(acc -> enumeratedRules -> {
                            let stmtsForPseudo = MutList.empty(rc);
                            let (stratarulesWithId, strataRules) = List.unzip(enumeratedRules);
                            compileStratum(stmtsForPseudo, strataRules, stratarulesWithId, predicates, counter, functionalCounter, typeInfo);
                            RamStmt.Seq(MutList.toVector(stmtsForPseudo)) :: acc
                        });
                    if (List.size(runInParallel) > 1) {
                        let codeBlock = RamStmt.Par(List.toVector(runInParallel));
                        MutList.push(codeBlock, stmts)
                    } else match List.head(runInParallel) {
                        case Some(v) => MutList.push(v, stmts)
                        case None    => ()
                    }
            };
            let stmt = RamStmt.Seq(MutList.toVector(stmts));
            RamProgram.Program(stmt, compiledFacts, predicates, (Map#{}, Map#{}), typeInfo)
        }
        case _ => bug!("Datalog normalization bug")
    }

    ///
    /// Collect all rules in `d` and assign them a number, deterministically.
    ///
    pub def enumerateRules(d: Datalog): Vector[(Int32, Constraint)] = match d {
        case Datalog(_, rules) => Vector.mapWithIndex(i -> r -> (i, r), rules)
        case _                 => unreachable!()
    }

    ///
    /// Functional relations should be treated as `Relational`, but they do not posses any `PredSym`, indicated
    /// by assigning them -1. `functionalRelSym` should only be placed in `RowLoads`. Only `Lowering.flix` uses
    /// the `RelSym` part and does not use the `PredSym`.
    ///
    pub def functionalRelSym(arity: Int32): RelSym = RelSym.Symbol(PredSym.PredSym("", -1i64), arity, Relational)

    ///
    /// Insert all facts in `facts` in `db` and return the result.
    ///
    def compileFacts(facts: Vector[Constraint], db: Map[RelSym, BPlusTree[Vector[Boxed], Boxed, Static]], predicates: Predicates): (Facts, Facts) \ IO = region rc {
        let newFacts = MutMap.empty(rc);
        foreach (Constraint(headAtom, _) <- facts) {
            let HeadAtom(_, den, headTerms) = headAtom;
            let x = Vector.map(getLitOrCrash, headTerms);
            match den {
                case Relational =>
                    let sym = Predicate.headAtomToRelSym(headAtom, Full, predicates);
                    let newFactsTree = MutMap.getOrElsePut(sym, BPlusTree.emptyWithArity(Static, usedArity()), newFacts);
                    BPlusTree.put(x, Boxed.NoValue, newFactsTree)
                case Latticenal(bot, leq, lub, _) =>
                    // Remove p(;bot) from the facts
                    let latVal = Vector.get(Vector.length(x) - 1, x);
                    if (latVal `leq` bot) {
                        ()
                    } else {
                        let sym = Predicate.headAtomToRelSym(headAtom, Full, predicates);
                        let newFactsTree = MutMap.getOrElsePut(sym, BPlusTree.emptyWithArity(Static, usedArity()), newFacts);
                        BPlusTree.putWith(lub, Vector.dropRight(1, x), latVal, newFactsTree)
                    }
            }
        };
        (db, MutMap.toMap(newFacts))
    }

    ///
    /// Compile all rules to be evaluated in `stratum` to RAM
    /// by using semi-naive evaluation as a code generator.
    /// The steps are:
    ///     1. For each rule Ri in `stratum` with head Pi:
    ///     ΔPi' = Pi ∪ Eval-Rule(Ri);
    ///     merge ΔPi' into Pi;
    ///     ΔPi = ΔPi';
    ///     purge ΔPi';
    ///     2. Generate fixpoint loop:
    ///     until(⋃Pi == ∅) do
    ///     2.1. For each rule Ri with Pi in its head:
    ///         ΔPi' = ΔPi' ∪ Eval-Rule-Incr(Ri);
    ///         merge ΔPi' into Pi;
    ///         ΔPi = ΔPi';
    ///         purge ΔPi'
    ///     end
    /// Note that Eval-Rule is the code emitted by `compileRule`
    /// and Eval-Rule-Incr is the code emitted by `compileRuleIncr`.
    ///
    def compileStratum(stmts: MutList[RamStmt, r1], stratum: List[Constraint], rulesWithId: List[Int32], predicates: Predicates, counter: Counter[r2], functionalCounter: Counter[r2], typeInfo: TypeInformation): Unit \ r1 + r2 = region rc {
        let functionalCountBackUp = Counter.get(functionalCounter);
        let idb = (Set#{}, stratum) ||>
            List.foldRight(match Constraint(head, _) -> {
                Set.insert(Predicate.headAtomToRelSym(head, Full, predicates))
            });
        let idbIds = (Set#{}, stratum) ||>
            List.foldRight(match Constraint(HeadAtom(PredSym(_, id), _, _), _) -> {
                Set.insert(id)
            });

        // Construct the inserts for step 1.
        let inserts = MutList.empty(rc);
        foreach ((rule, ruleNum) <- ForEach.withZip(stratum, rulesWithId)) {
            compileRule(inserts, rule, ruleNum, predicates, counter, functionalCounter, typeInfo)
        };
        let optPar =
            if (MutList.size(inserts) > 1) {
                Some(RamStmt.Par(MutList.toVector(inserts)))
            } else {
                MutList.head(inserts)
            };
        match optPar {
            case None       => ()
            case Some(par1) =>
                MutList.push(par1, stmts);

                // Construct the merges, swaps and purges for step 1.
                MutList.push(constructMerges(idb, predicates), stmts);

                let loopBody = MutList.empty(rc);

                let loopInserts = MutList.empty(rc);

                let finalFunctionalCounter =  Counter.get(functionalCounter);
                Counter.set(functionalCountBackUp, functionalCounter);
                foreach ((rule, ruleNum) <- ForEach.withZip(stratum, rulesWithId)) {
                    compileRuleIncr(loopInserts, rule, ruleNum, idbIds, predicates, counter, functionalCounter, typeInfo)
                };
                Counter.set(finalFunctionalCounter, functionalCounter);
                let loopParOpt =
                    if (MutList.size(loopInserts) > 1) {
                        Some(RamStmt.Par(MutList.toVector(loopInserts)))
                    } else {
                        MutList.head(loopInserts)
                    };
                match loopParOpt {
                    case None          => ()
                    case Some(loopPar) =>
                        MutList.push(loopPar, loopBody);
                        MutList.push(constructMerges(idb, predicates), loopBody);
                        let loopTest =
                            idb
                                |> Set.toVector
                                |> Vector.map(relSym ->
                                    BoolExp.IsEmpty(Predicate.getRelSymAsType(relSym, Delta, predicates))
                                );
                        let untilBody = RamStmt.Seq(MutList.toVector(loopBody));
                        let fixpoint = RamStmt.Until(loopTest, untilBody);
                        MutList.push(fixpoint, stmts)
                }
        }
    }

    ///
    /// Construct merges for the given `RelSym`s in `idb`. See the description for `compileStratum`
    ///
    def constructMerges(idb: Set[RelSym], predicates: Predicates): RamStmt = region rc {
        let merges = MutList.empty(rc);
        let swaps = MutList.empty(rc);
        let purges = MutList.empty(rc);
        foreach (relSym <- idb) {
            let newRel = Predicate.getRelSymAsType(relSym, New, predicates);
            let deltaRel = Predicate.getRelSymAsType(relSym, Delta, predicates);
            let fullRel = Predicate.getRelSymAsType(relSym, Full, predicates);
            let mergeFull = RamStmt.MergeInto(newRel, fullRel);
            let swapDelta = RamStmt.Swap(newRel, deltaRel);
            let purgeNew = RamStmt.Purge(newRel);
            MutList.push(mergeFull, merges);
            MutList.push(swapDelta, swaps);
            MutList.push(purgeNew, purges)
        };
        let swapsAsRam = if (MutList.length(swaps) > 1) {
            RamStmt.Seq(MutList.toVector(swaps))
        } else if (MutList.size(swaps) == 1) {
            getOrCrash(MutList.head(swaps))
        } else {
            bug!("Bug in Fixpoint.Compiler: Stratum error. Stratum had no PredSyms")
        };
        let purgesAsRam = if (MutList.length(purges) > 1) {
            RamStmt.Seq(MutList.toVector(purges))
        } else if (MutList.size(purges) == 1) {
            getOrCrash(MutList.head(purges))
        } else {
            bug!("Bug in Fixpoint.Compiler: Stratum error. Stratum had no PredSyms")
        };
        let mergesAsRam = if (MutList.size(merges) > 1)
            RamStmt.Par(MutList.toVector(merges))
        else if (MutList.size(merges) == 1)
            getOrCrash(MutList.head(merges))
        else
            bug!("Bug in Fixpoint.Compiler: Stratum error. Stratum had no PredSyms");
        RamStmt.Seq(Vector#{mergesAsRam, swapsAsRam, purgesAsRam})
    }

    ///
    /// Compile `rule` into a join loop. A join loop is a nested loop.
    /// Each positive body atom in `rule` becomes part of the nested loop.
    /// The head atom in `rule` is compiled to an insertion into the corresponding relation.
    /// Negative atoms are compiled into if-statements.
    /// The rule `R(x) :- not A(x), B(x), C(x).` compiles to:
    ///     search B$1 ∈ B do
    ///         search C$2 ∈ C do
    ///             if (B$1[0] == C$2[0] ∧ (B$1[0]) ∉ A) then
    ///                 project (B$1[0]) into R
    ///             end
    ///         end
    ///     end
    ///
    def compileRule(stmts: MutList[RamStmt, r1], rule: Constraint, ruleNum: Int32, predicates: Predicates, counter: Counter[r2], functionalCounter: Counter[r2], typeInfo: TypeInformation): Unit \ r1 + r2 = match rule {
        case Constraint(headAtom, body) =>
            let HeadAtom(PredSym(_, headId), _, headTerms) = headAtom;
            let augBody = augmentBody(body, counter);
            let env = unifyVars(predicates, functionalCounter, typeInfo, augBody);
            let ramTerms = Vector.mapWithIndex(i -> atom -> compileHeadTerm(env, getType(headId, i, typeInfo), atom), headTerms);
            let projection = RelOp.Project(ramTerms, Predicate.headAtomToRelSym(headAtom, New, predicates), ruleNum);
            match compileBody(env, augBody, predicates, typeInfo) {
                case None       => ()
                case Some(join) =>
                    let loopBody = RelOp.If(join, projection);
                    let insert = (loopBody, augBody)
                        ||> Vector.foldRight(match (atom, rowVar) -> acc -> match atom {
                            case BodyAtom(PredSym(_, id), _, Polarity.Positive, _, _) =>
                                RelOp.Search(rowVar, Predicate.bodyAtomToRelSym(atom, Full, predicates), getTypeOf(id, typeInfo), acc)
                            case Functional(boundVars, f, freeVars) =>
                                let terms = Vector.map(v -> getOrCrash(Map.get(v, env)), freeVars);
                                let t = getTypeOf(Int32.toInt64(Counter.getAndIncrement(functionalCounter)), typeInfo);
                                RelOp.Functional(rowVar, f, terms, acc, Vector.length(boundVars), t)
                            case _ => acc
                        })
                        |> RamStmt.Insert;
                    if (Fixpoint3.Options.enableDebugging()) {
                        let comment = RamStmt.Comment(ToString.toString(rule));
                        MutList.push(comment, stmts);
                        MutList.push(insert, stmts)
                    } else {
                        MutList.push(insert, stmts)
                    }
            }
    }

    ///
    /// Compile `rule` into an incremental join loop. A join loop is a nested loop.
    /// Each positive body atom in `rule` becomes part of the nested loop.
    /// The head atom in `rule` is compiled to an insertion into the corresponding relation.
    /// Incremental means there is a join loop for every positive atom.
    /// Each join loop "focuses" on a particular atom.
    /// Negative atoms are compiled into if-statements.
    /// The rule `R(x) :- not A(x), B(x), C(x).` compiles to:
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
    /// Note that only relations in `idbSet` are used as Δ-relations.
    ///
    /// `ruleNum` is the rules assigned number and `counter` is used to create unique `RowVar`s.
    ///
    def compileRuleIncr(stmts: MutList[RamStmt, r1], rule: Constraint, ruleNum: Int32, idbSet: Set[Int64], predicates: Predicates, counter: Counter[r2], functionalCounter: Counter[r2], typeInfo: TypeInformation): Unit \ r1 + r2 = match rule {
        case Constraint(headAtom, body) =>
            let HeadAtom(PredSym(_, headId), _, headTerms) = headAtom;
            let compileSomething = deltaIndex -> { // `delta` designates the focused atom.
                let augBody = augmentBody(body, counter);
                let delta = snd(Vector.get(deltaIndex, augBody));
                let env = unifyVars(predicates, functionalCounter, typeInfo, augBody);
                let ramTerms = Vector.mapWithIndex(i -> atom -> compileHeadTerm(env, getType(headId, i, typeInfo), atom), headTerms);
                let projection = RelOp.Project(ramTerms, Predicate.headAtomToRelSym(headAtom, New, predicates), ruleNum);
                match compileBody(env, augBody, predicates, typeInfo) {
                    case None       => ()
                    case Some(join) =>
                        let relSym = Predicate.headAtomToRelSym(headAtom, Full, predicates);
                        let loopBody = RelOp.If(
                            Vector#{BoolExp.NotMemberOf(ramTerms, relSym, false)} ++ join,
                            projection
                        );
                        let insert = (loopBody, augBody)
                            ||> Vector.foldRight(match (atom, rowVar) -> acc -> match atom {
                                case BodyAtom(PredSym(_, id), den, Polarity.Positive, _, _) =>
                                    let ramSym = if (rowVar == delta and isRelational(den)) {
                                        Predicate.bodyAtomToRelSym(atom, Delta, predicates)
                                    } else {
                                        Predicate.bodyAtomToRelSym(atom, Full, predicates)
                                    };
                                    RelOp.Search(rowVar, ramSym, getTypeOf(id, typeInfo), acc)
                                case Functional(boundVars, f, freeVars) =>
                                    let terms = Vector.map(v -> getOrCrash(Map.get(v, env)), freeVars);
                                    let t = getTypeOf(Int32.toInt64(Counter.getAndIncrement(functionalCounter)), typeInfo);
                                    RelOp.Functional(rowVar, f, terms, acc, Vector.length(boundVars), t)
                                case _ => acc
                            })
                            |> RamStmt.Insert;
                        MutList.push(insert, stmts)
                }
            };

            let positiveIDBAtoms = body
                |> Vector.mapWithIndex(i -> atom -> match atom {
                    case BodyAtom(PredSym(_, id), _, Polarity.Positive, Fixity.Loose, _) =>
                        if (Set.memberOf(id, idbSet)) {
                            i
                        } else {
                            -1
                        }
                    case _ => -1
                })
                |> Vector.filter(i -> i != -1);

            let count = Counter.get(functionalCounter);
            if (Fixpoint3.Options.enableDebugging()) {
                let comment = RamStmt.Comment(ToString.toString(rule));
                MutList.push(comment, stmts);
                Vector.forEach(i -> {Counter.set(count, functionalCounter); compileSomething(i)}, positiveIDBAtoms)
            } else {
                Vector.forEach(i -> {Counter.set(count, functionalCounter); compileSomething(i)}, positiveIDBAtoms)
            }
    }

    ///
    /// Substitute Datalog head term `term` for a RAM term according to the given substitution `env`.
    ///
    def compileHeadTerm(env: Map[VarSym, RamTerm], ty: Type, term: HeadTerm): RamTerm = match term {
        case HeadTerm.Var(var)   => getOrCrash(Map.get(var, env))
        case HeadTerm.Lit(v)     => RamTerm.Lit(v, ty)
        case HeadTerm.App1(f, v) =>
            let t = getOrCrash(Map.get(v, env));
            RamTerm.App1(f, t, ty)
        case HeadTerm.App2(f, v1, v2) =>
            let t1 = getOrCrash(Map.get(v1, env));
            let t2 = getOrCrash(Map.get(v2, env));
            RamTerm.App2(f, t1, t2, ty)
        case HeadTerm.App3(f, v1, v2, v3) =>
            let t1 = getOrCrash(Map.get(v1, env));
            let t2 = getOrCrash(Map.get(v2, env));
            let t3 = getOrCrash(Map.get(v3, env));
            RamTerm.App3(f, t1, t2, t3, ty)
        case HeadTerm.App4(f, v1, v2, v3, v4) =>
            let t1 = getOrCrash(Map.get(v1, env));
            let t2 = getOrCrash(Map.get(v2, env));
            let t3 = getOrCrash(Map.get(v3, env));
            let t4 = getOrCrash(Map.get(v4, env));
            RamTerm.App4(f, t1, t2, t3, t4, ty)
        case HeadTerm.App5(f, v1, v2, v3, v4, v5) =>
            let t1 = getOrCrash(Map.get(v1, env));
            let t2 = getOrCrash(Map.get(v2, env));
            let t3 = getOrCrash(Map.get(v3, env));
            let t4 = getOrCrash(Map.get(v4, env));
            let t5 = getOrCrash(Map.get(v5, env));
            RamTerm.App5(f, t1, t2, t3, t4, t5, ty)
    }

    ///
    /// Augment body atoms with RowVars.
    /// Every atom is thus identified by a unique RowVar, regardless of its position in the body.
    ///
    def augmentBody(body: Vector[BodyPredicate], counter: Counter[r]): Vector[(BodyPredicate, RowVar)] \ r =
        Vector.map(atom -> match atom {
            case BodyAtom(_, _, Polarity.Positive, _, _) => (atom, RowVar.Named(Counter.getAndIncrement(counter)))
            case Functional(_, _, _)                     => (atom, RowVar.Named(Counter.getAndIncrement(counter)))
            case _ => (atom, RowVar.Named(-1))
        }, body)

    ///
    /// Unifies each relational variable that occurs in a rule body with an attribute
    /// corresponding to a positive atom where the variable is a term.
    /// Latticenal variables are unified with a "meet" term.
    /// For example, a body like `not A(x), B(x; s), C(x; s).` returns the substitution:
    ///     `Map#{x -> B$1[0], s -> B$1[1] ⊓ C$2[1]}`
    /// `x` is mapped to B$1[0] because `x` occurs positively in the second atom.
    /// `s` is mapped to the glb of all its positive occurences because it is latticenal.
    ///
    /// Does not change `functionalCounter`
    ///
    def unifyVars(predicates: Predicates, functionalCounter: Counter[r], typeInfo: TypeInformation, body: Vector[(BodyPredicate, RowVar)]): Map[VarSym, RamTerm] \ r =
        let functionalCountBackUp = Counter.get(functionalCounter);
        let toFullSym = bodyAtom -> (Predicate.bodyAtomToRelSym(bodyAtom, Full, predicates));
        let innerFunc = {
            acc -> atomVar ->
            let (atom, rowVar) = atomVar;
            match atom {
            case BodyAtom(PredSym(_, id), denotation, Polarity.Positive, _, terms) =>
                let termsWithIndexes = Vector.mapWithIndex(i -> term -> (term, i), terms);
                (acc, termsWithIndexes) ||>
                    Vector.foldRight(termI -> acc1 ->
                        let (term, i) = termI;
                        let sType = getType(id, i, typeInfo);
                        match term {
                            case BodyTerm.Var(var) => match denotation {
                                case Relational =>
                                    Map.insertWith(_ -> t -> t, var, RamTerm.RowLoad(rowVar, i, sType, toFullSym(atom)), acc1)
                                case Latticenal(_, _, _, glb) =>
                                    if (i < Vector.length(terms) - 1)
                                        Map.insertWith(_ -> t -> t, var, RamTerm.RowLoad(rowVar, i, sType, toFullSym(atom)), acc1)
                                    else
                                        let f = _ -> t2 -> RamTerm.Meet(glb, t2, (rowVar, toFullSym(atom)), getType(id, i, typeInfo));
                                        Map.insertWith(f, var, RamTerm.RowLoad(rowVar, i, sType, toFullSym(atom)), acc1)
                            }
                            case _ => acc1
                    })
            case BodyAtom(_, _, Polarity.Negative, _, _) => acc
            case Functional(boundVars, _, freeVars) =>
                let typeId = Int32.toInt64(Counter.getAndIncrement(functionalCounter));
                let funcRelSym = functionalRelSym(Vector.length(freeVars));
                let varsWithIndexes = Vector.mapWithIndex(i -> var -> (var, i), boundVars);
                (acc, varsWithIndexes) ||>
                    Vector.foldRight(match (var, i) -> Map.insertWith(_ -> old -> old, var, RamTerm.RowLoad(rowVar, i, getType(typeId, i, typeInfo), funcRelSym)))
            case Guard0(_)                => acc
            case Guard1(_, _)             => acc
            case Guard2(_, _, _)          => acc
            case Guard3(_, _, _, _)       => acc
            case Guard4(_, _, _, _, _)    => acc
            case Guard5(_, _, _, _, _, _) => acc
            case _                        => acc
        }};
        let res = Vector.foldLeft(innerFunc, Map#{}, body);
        Counter.set(functionalCountBackUp, functionalCounter);
        res

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
    def compileBody(env: Map[VarSym, RamTerm], body: Vector[(BodyPredicate, RowVar)], predicates: Predicates, typeInfo: TypeInformation): Option[Vector[BoolExp]] =
        if (isImpossible(body)) None
        else Some(compileBodyWithoutGuard0(env, body, predicates, typeInfo))

    ///
    /// Returns true if a rule is impossible to satisfy. Used for removing `Guard0()` from the AST.
    ///
    def isImpossible(body: Vector[(BodyPredicate, RowVar)]): Bool =
        Vector.exists(match (atom, _) ->
            match atom {
                case BodyAtom(_, _, _, _, _)  => false
                case BodyAtom(_, _, _, _, _)  => false
                case Functional(_, _, _)      => false
                case Guard0(f)                =>
                    if (f()) false
                    else true
                case Guard1(_, _)             => false
                case Guard2(_, _, _)          => false
                case Guard3(_, _, _, _)       => false
                case Guard4(_, _, _, _, _)    => false
                case Guard5(_, _, _, _, _, _) => false
            }, body)


    ///
    /// Compiles `body` under substitution `env`, assuming any `Guard0` is true.
    ///
    def compileBodyWithoutGuard0(env: Map[VarSym, RamTerm], body: Vector[(BodyPredicate, RowVar)], predicates: Predicates, typeInfo: TypeInformation): Vector[BoolExp] =
        let toFullSym = bodyAtom -> (Predicate.bodyAtomToRelSym(bodyAtom, Full, predicates));
        body |>
            Vector.flatMap(match (atom, rowVar) ->
                let compileBodyTerm = id -> j -> term -> match term {
                    case BodyTerm.Wild      => RamTerm.RowLoad(rowVar, j, getType(id, j, typeInfo), toFullSym(atom))
                    case BodyTerm.Var(var)  => getOrCrash(Map.get(var, env))
                    case BodyTerm.Lit(v)    => RamTerm.Lit(v, getType(id, j, typeInfo))
                };
                match atom {
                    case BodyAtom(PredSym(_, id), denotation, Polarity.Positive, _, terms) =>
                        let full = toFullSym(atom);
                        Vector.mapWithIndex(compileBodyTerm(id), terms)
                            |> Vector.zip(Vector.range(0, Vector.length(terms)))
                            |> Vector.map(match (i, t) -> match denotation {
                                case Relational =>
                                    BoolExp.Eq(RamTerm.RowLoad(rowVar, i, getType(id, i, typeInfo), full), t)
                                case Latticenal(bot, leq, _, _) =>
                                    if (i < Vector.length(terms) - 1)
                                        BoolExp.Eq(RamTerm.RowLoad(rowVar, i, getType(id, i, typeInfo), full), t)
                                    else
                                        compileBoolForLatticeenal(t, leq, bot, rowVar, full)
                            })
                    case BodyAtom(PredSym(_, id), _, Polarity.Negative, _, terms) =>
                        let ramTerms = Vector.mapWithIndex(compileBodyTerm(id), terms);
                        Vector#{BoolExp.NotMemberOf(ramTerms, Predicate.bodyAtomToRelSym(atom, Full, predicates), true)}
                    case Functional(_, _, _) => Vector.empty()
                    case Guard0(_) =>
                        Vector#{}
                    case Guard1(f, v) =>
                        let t = getOrCrash(Map.get(v, env));
                        Vector#{BoolExp.Guard1(f, t)}
                    case Guard2(f, v1, v2) =>
                        let t1 = getOrCrash(Map.get(v1, env));
                        let t2 = getOrCrash(Map.get(v2, env));
                        Vector#{BoolExp.Guard2(f, t1, t2)}
                    case Guard3(f, v1, v2, v3) =>
                        let t1 = getOrCrash(Map.get(v1, env));
                        let t2 = getOrCrash(Map.get(v2, env));
                        let t3 = getOrCrash(Map.get(v3, env));
                        Vector#{BoolExp.Guard3(f, t1, t2, t3)}
                    case Guard4(f, v1, v2, v3, v4) =>
                        let t1 = getOrCrash(Map.get(v1, env));
                        let t2 = getOrCrash(Map.get(v2, env));
                        let t3 = getOrCrash(Map.get(v3, env));
                        let t4 = getOrCrash(Map.get(v4, env));
                        Vector#{BoolExp.Guard4(f, t1, t2, t3, t4)}
                    case Guard5(f, v1, v2, v3, v4, v5) =>
                        let t1 = getOrCrash(Map.get(v1, env));
                        let t2 = getOrCrash(Map.get(v2, env));
                        let t3 = getOrCrash(Map.get(v3, env));
                        let t4 = getOrCrash(Map.get(v4, env));
                        let t5 = getOrCrash(Map.get(v5, env));
                        Vector#{BoolExp.Guard5(f, t1, t2, t3, t4, t5)}
            })

    ///
    /// Compile the RamTerm `t` to either a Leq or NotBot depending on whether the atom was `Pi(;x)`, `Pi(;_)`, or `P(;1)`.
    ///
    def compileBoolForLatticeenal(t: RamTerm, leq: Boxed -> Boxed -> Bool, bot: Boxed, rv: RowVar, relSym: RelSym): BoolExp = match t {
        case RamTerm.Lit(val, _) =>
            BoolExp.Leq(val, rv, relSym)
        case _ => BoolExp.NotBot(t, leq, bot)
    }

    ///
    /// Extract the `Lit` in `term` or crash.
    ///
    def getLitOrCrash(term: HeadTerm): Boxed = match term {
            case HeadTerm.Lit(v) => v
            case _               => bug!("Facts should only consist of literals")
        }

}

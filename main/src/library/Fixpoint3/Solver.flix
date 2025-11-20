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

mod Fixpoint3.Solver {
    import dev.flix.runtime.Global

    use Fixpoint3.{Boxed, Debugging, Interpreter, Options, Phase, Predicate, PredSymsOf, SubstitutePredSym}
    use Fixpoint3.Ast.Datalog.{Datalog, Constraint, HeadTerm}
    use Fixpoint3.Ast.Datalog.Datalog.{Datalog, Join, Model, Provenance}
    use Fixpoint3.Ast.Datalog.Constraint.Constraint
    use Fixpoint3.Ast.Datalog.HeadPredicate.HeadAtom
    use Fixpoint3.Ast.Ram
    use Fixpoint3.Ast.Ram.{Predicates, RelSym, toDenotation}
    use Fixpoint3.Ast.ExecutableRam
    use Fixpoint3.Ast.Shared.{Denotation, PredSym}
    use Fixpoint3.Ast.Shared.Denotation.{Relational, Latticenal}
    use Fixpoint3.Ast.Shared.PredSym.PredSym
    use Fixpoint3.Boxable.{box, unbox}
    use Fixpoint3.BoxingType.Boxing
    use Fixpoint3.Interpreter.Database
    use Fixpoint3.Util.getOrCrash

    ///
    /// Returns the minimal model of the given Datalog program `d`.
    /// The minimal model is given by the model semantics for Datalog programs with stratified negation.
    /// The stratification is computed automatically by the solver.
    ///
    @Internal
    @LoweringTarget
    pub def runSolver(d: Datalog): Datalog = runSolverInternal(false, d)

    ///
    /// Returns a provenance model of the given Datalog program `d`.
    /// The provenance model can later be used for provenance queries.
    ///
    @Internal
    @LoweringTarget
    pub def runSolverWithProvenance(d: Datalog): Datalog = runSolverInternal(true, d)

    def runSolverInternal(withProv: Bool, d: Datalog): Datalog = region rc {
        let (fixed, fixInfo) = Phase.RenamePredSyms.assignUniqueIdentifiersToPredSyms(rc, d);
        let strat = Phase.Stratifier.stratify(fixed);
        Phase.RenamePredSyms.renamePredSyms(runWithStratification(fixed, strat, withProv), fixInfo)
    }

    ///
    /// Returns the minimal model of the given Datalog program `d`.
    /// The minimal model is given by the model semantics for Datalog programs with stratified negation.
    /// A stratification of `d` is given by `stf`.
    ///
    def runWithStratification(d0: Datalog, stf: Map[PredSym, (Int32, Int32)], withProv: Bool): Datalog = region rc {
        let d = Debugging.notifyPreSolve(d0);
        def compiler(cs: Datalog, db: Ram.Facts): (ExecutableRam.RamProgram[rc], Boxing[rc]) \ rc = {
            Phase.Compiler.compile(cs, db, stf, withProv)
                |> Debugging.notifyPreLowering("Compiler")
                |> Phase.Simplifier.simplifyProgram
                |> Debugging.notifyPreLowering("Simplifier")
                |> Phase.Hoisting.hoistProgram
                |> Debugging.notifyPreLowering("Hoisting")
                |> Phase.IndexSelection.indexProgram
                |> Debugging.notifyPreLowering("Index Selection")
                |> Phase.ProvenanceAugment.augmentProgram(withProv)
                |> Phase.Lowering.lowerProgram(rc, withProv)
        };
        let model = match d {
            case Datalog(_, r) =>
                compiler(d, Map.empty())
                    |> Debugging.notifyPreInterpret
                    |> Interpreter.interpret(rc)
                    |> toModelOnlyFull(withProv, r)
            case Model(_) =>
                d
            case Provenance(_, _) =>
                d
            case Join(Model(m), Datalog(f, r)) =>
                let cs = Datalog(f, r);
                compiler(cs, m)
                    |> Debugging.notifyPreInterpret
                    |> Interpreter.interpret(rc)
                    |> toModelOnlyFull(withProv, r)
            case Join(Provenance(_, model), Datalog(f, r)) =>
                let cs = Datalog(f, r);
                // Provenance is destroyed upon joining. Becomes a model.
                let m = provenanceModeltoModel(model);
                compiler(cs, m)
                    |> Debugging.notifyPreInterpret
                    |> Interpreter.interpret(rc)
                    |> toModelOnlyFull(withProv, r)
            case _ => bug!("Datalog Boxing bug")
        };
        let m = Debugging.notifyPostSolve(model);
        m
    }

    ///
    /// Returns the pairwise union of `d1` and `d2`.
    /// I.e. the facts of the union is the union of the facts and likewise for rules.
    /// A fact or rule may occur twice in the Datalog program. This has no effect on its semantics.
    ///
    @Internal
    @LoweringTarget
    pub def union(d1: Datalog, d2: Datalog): Datalog = match (d1, d2) {
        case (Datalog(edb1, idb1), Datalog(edb2, idb2)) =>
            Datalog(edb1 ++ edb2, idb1 ++ idb2)
        case (Model(db1), Model(db2)) => unsafely IO run {
            let union = ramSym -> v1 -> v2 -> {
                let newTree = BPlusTree.empty(Static);

                match toDenotation(ramSym) {
                    case Relational =>
                        BPlusTree.merge(v1, newTree);
                        BPlusTree.merge(v2, newTree)
                    case Latticenal(_, _, lub, _) =>
                        BPlusTree.merge(v1, newTree);
                        BPlusTree.mergeWith(lub, v2, newTree)
                };
                newTree
            };
            Model(Map.unionWithKey(union, db1, db2))
        }
        case (Model(_), Datalog(_, _))             => Join(d1, d2)
        case (Datalog(_, _), Model(_))             => Join(d2, d1)
        case (Join(model, datalog), Datalog(_, _)) => Join(model, union(datalog, d2))
        case (Datalog(_, _), Join(model, datalog)) => Join(model, union(datalog, d1))
        case (Join(model, datalog), Model(_))      => Join(union(model, d2), datalog)
        case (Model(_), Join(model, datalog))      => Join(union(model, d1), datalog)
        case (Join(x, y), Join(z, w))              => Join(union(x, z), union(y, w))
        case (Provenance(_, _), _)                 => union(provenanceToModel(d1), d2)
        case (_, Provenance(_, _))                 => union(d1, provenanceToModel(d2))
    }

    ///
    /// Returns the provenance of fact `f` of relation `p` given Datalog program `d`.
    ///
    @Internal
    @LoweringTarget
    pub def provenanceOf(p: PredSym, f: Vector[Boxed], withh: Vector[PredSym], mkExtVar: PredSym -> Vector[Boxed] -> t, d: Datalog): Vector[t] = match d {
        case Datalog(_, _) => provenanceOf(p, f, withh, mkExtVar, runSolverWithProvenance(d))
        case Model(db) => unsafely IO run {
            // This is a dirty hack based on the implementation of Order on RelSym (it only depends on PredSym).
            let fakeRelSym = Ram.makeFakeRelSym(p);
            match Map.get(fakeRelSym, db) {
                case None => // Temporary
                    let termsStr = Vector.join(", ", f);
                    bug!("${p}(${termsStr}) is not a fact")
                case Some(facts) => match BPlusTree.memberOf(f, facts) {
                    case false => // Temporary
                        let termsStr = Vector.join(", ", f);
                        bug!("${p}(${termsStr}) is not a fact")
                    case true => Vector#{mkExtVar(p, f)}
                }
            }
        }
        case Provenance(_, _) => match Fixpoint3.ProvenanceReconstruct.provQuery(p, f, withh, d) {
            case None =>
                let termsStr = Vector.join(", ", f);
                bug!("${p}(${termsStr}) is not a fact")
            case Some(v) => Vector.map(match (pred, fact) -> mkExtVar(pred, fact), v)
        }
        case Join(_, _) => provenanceOf(p, f, withh, mkExtVar, runSolverWithProvenance(d))
    }

    ///
    /// Returns the `Provenance` program `d` as a `Model`.
    ///
    /// Crashes if `d` is not a `Provenance`.
    ///
    def provenanceToModel(d: Datalog): Datalog = match d {
        case Provenance(_, facts) => Model(provenanceModeltoModel(facts))
        case _                    => unreachable!()
    }

    ///
    /// Returns the given provenance model `model` as a Datalog value.
    ///
    def provenanceModeltoModel(model: Map[RelSym, BPlusTree[Vector[Boxed], (Int64, Int32), IO]]): Map[RelSym, BPlusTree[Vector[Boxed], Boxed, IO]] = unsafely IO run {
        model |>
            Map.map(old -> {
                let newTree = BPlusTree.empty(Static);
                BPlusTree.forEach(k -> _ -> BPlusTree.put(k, Boxed.NoValue, newTree), old);
                newTree
            })
    }

    ///
    /// Project all facts in the given Datalog program `d` that has `p` in its head.
    ///
    @Internal
    @LoweringTarget
    pub def projectSym(p: PredSym, d: Datalog): Datalog = match d {
        case Datalog(facts, _) =>
            let pFacts = Vector.filter(match Constraint(HeadAtom(head, _, _), _) -> head == p, facts);
            Datalog(pFacts, Vector.empty())
        case Model(db) =>
            Map.rangeQuery(ramSym -> match ramSym {
                case RelSym.Symbol(predSym, _, _) => match predSym <=> p {
                    case Comparison.EqualTo => Comparison.EqualTo
                    case cmp => cmp
                }
                case _ => Comparison.LessThan
            }, db)
                |> List.toMap
                |> Model
        case Provenance(rules, db) =>
            let fakeRelSym = Ram.makeFakeRelSym(p);
            match Map.get(fakeRelSym, db) {
                case None    => Provenance(rules, Map#{})
                case Some(v) => Provenance(rules, Map#{fakeRelSym => v})
            }
        case Join(d1, d2) => union(projectSym(p, d1), projectSym(p, d2))
    }

    ///
    /// Renames every predicate symbol in `d` with a fresh name, except for those in `preds`.
    ///
    @Internal
    @LoweringTarget
    pub def rename(preds: List[PredSym], d: Datalog): Datalog = {
        // Determine all the predicate symbols in `d`.
        let predSyms = PredSymsOf.predSymsOf(d);

        // Compute a substitition that assignes fresh names to all predicates except those in `preds`.
        def f(macc, predSym) = {
            if (List.memberOf(predSym, preds)) {
                // Case 1: Do not rename. Do not extend the map.
                macc
            } else {
                // Case 2: Introduce a fresh name (index) for the predicate symbol.
                let PredSym(name, _) = predSym;
                let nextId = unchecked_cast((Global.newId() + 1i64) as _ \ {});  // We add one because the counter starts at zero.
                let newPredSym = PredSym(name, nextId);
                Map.insert(predSym, newPredSym, macc)
            }
        };
        let s = Set.foldLeft(f, Map.empty(), predSyms);

        // Apply the computed substitution to the Datalog program `d`.
        SubstitutePredSym.substitute(d, s)
    }

    ///
    /// Insert all facts in the given sequence `f` into the given relation `p`.
    ///
    /// I.e. returns a Datalog program corresponding to `#{p(t1). p(t1').}`
    /// given the sequence `(t1), (t1')`.
    ///
    @Internal
    @LoweringTarget
    pub def injectInto1(p: PredSym, ts: f[t1]):
        Datalog \ Foldable.Aef[f] with Order[t1], Foldable[f] =
        injectIntoX(match (v1) -> Vector#{
            box(v1)
        }
        , p, ts)

    ///
    /// Insert all facts in the given sequence `f` into the given relation `p`.
    ///
    /// I.e. returns a Datalog program corresponding to `#{p(t1, t2). p(t1', t2').}`
    /// given the sequence `(t1, t2), (t1', t2')`.
    ///
    @Internal
    @LoweringTarget
    pub def injectInto2(p: PredSym, ts: f[(t1, t2)]):
        Datalog \ Foldable.Aef[f] with Order[t1], Order[t2], Foldable[f] =
        injectIntoX(match (v1, v2) -> Vector#{
            box(v1),
            box(v2)
        }
        , p, ts)

    ///
    /// Insert all facts in the given sequence `f` into the given relation `p`.
    ///
    /// I.e. returns a Datalog program corresponding to `#{p(t1, t2, t3). p(t1', t2', t3').}`
    /// given the sequence `(t1, t2, t3), (t1', t2', t3')`.
    ///
    @Internal
    @LoweringTarget
    pub def injectInto3(p: PredSym, ts: f[(t1, t2, t3)]):
        Datalog \ Foldable.Aef[f] with Order[t1], Order[t2], Order[t3], Foldable[f] =
        injectIntoX(match (v1, v2, v3) -> Vector#{
            box(v1),
            box(v2),
            box(v3)
        }
        , p, ts)

    ///
    /// Insert all facts in the given sequence `f` into the given relation `p`.
    ///
    /// I.e. returns a Datalog program corresponding to
    /// `#{p(t1, t2, t3, t4). p(t1', t2', t3', t4').}`
    /// given the sequence `(t1, t2, t3, t4), (t1', t2', t3', t4')`.
    ///
    @Internal
    @LoweringTarget
    pub def injectInto4(p: PredSym, ts: f[(t1, t2, t3, t4)]):
        Datalog \ Foldable.Aef[f] with Order[t1], Order[t2], Order[t3], Order[t4], Foldable[f] =
        injectIntoX(match (v1, v2, v3, v4) -> Vector#{
            box(v1),
            box(v2),
            box(v3),
            box(v4)
        }
        , p, ts)

    ///
    /// Insert all facts in the given sequence `f` into the given relation `p`.
    ///
    /// I.e. returns a Datalog program corresponding to
    /// `#{p(t1, t2, t3, t4, t5). p(t1', t2', t3', t4', t5').}`
    /// given the sequence `(t1, t2, t3, t4, t5), (t1', t2', t3', t4', t5')`.
    ///
    @Internal
    @LoweringTarget
    pub def injectInto5(p: PredSym, ts: f[(t1, t2, t3, t4, t5)]):
        Datalog \ Foldable.Aef[f] with Order[t1], Order[t2], Order[t3], Order[t4], Order[t5], Foldable[f] =
        injectIntoX(match (v1, v2, v3, v4, v5) -> Vector#{
            box(v1),
            box(v2),
            box(v3),
            box(v4),
            box(v5)
        }
        , p, ts)

    ///
    /// Insert all facts in the given sequence `f` into the given relation `p`.
    ///
    /// I.e. returns a Datalog program corresponding to
    /// `#{p(t1, t2, t3, t4, t5, t6). p(t1', t2', t3', t4', t5', t6').}`
    /// given the sequence `(t1, t2, t3, t4, t5, t6), (t1', t2', t3', t4', t5', t6')`.
    ///
    @Internal
    @LoweringTarget
    pub def injectInto6(p: PredSym, ts: f[(t1, t2, t3, t4, t5, t6)]):
        Datalog \ Foldable.Aef[f] with Order[t1], Order[t2], Order[t3], Order[t4], Order[t5], Order[t6], Foldable[f] =
        injectIntoX(match (v1, v2, v3, v4, v5, v6) -> Vector#{
            box(v1),
            box(v2),
            box(v3),
            box(v4),
            box(v5),
            box(v6)
        }
        , p, ts)

    ///
    /// Insert all facts in the given sequence `f` into the given relation `p`.
    ///
    /// I.e. returns a Datalog program corresponding to
    /// `#{p(t1, t2, t3, t4, t5, t6, t7). p(t1', t2', t3', t4', t5', t6', t7').}`
    /// given the sequence `(t1, t2, t3, t4, t5, t6, t7), (t1', t2', t3', t4', t5', t6', t7')`.
    ///
    @Internal
    @LoweringTarget
    pub def injectInto7(p: PredSym, ts: f[(t1, t2, t3, t4, t5, t6, t7)]):
        Datalog \ Foldable.Aef[f] with Order[t1], Order[t2], Order[t3], Order[t4], Order[t5], Order[t6], Order[t7], Foldable[f] =
        injectIntoX(match (v1, v2, v3, v4, v5, v6, v7) -> Vector#{
            box(v1),
            box(v2),
            box(v3),
            box(v4),
            box(v5),
            box(v6),
            box(v7)
        }
        , p, ts)

    ///
    /// Insert all facts in the given sequence `f` into the given relation `p`.
    ///
    /// I.e. returns a Datalog program corresponding to
    /// `#{p(t1, t2, t3, t4, t5, t6, t7, t8). p(t1', t2', t3', t4', t5', t6', t7', t8').}`
    /// given the sequence
    /// `(t1, t2, t3, t4, t5, t6, t7, t8), (t1', t2', t3', t4', t5', t6', t7', t8')`.
    ///
    @Internal
    @LoweringTarget
    pub def injectInto8(p: PredSym, ts: f[(t1, t2, t3, t4, t5, t6, t7, t8)]):
        Datalog \ Foldable.Aef[f] with Order[t1], Order[t2], Order[t3], Order[t4], Order[t5], Order[t6], Order[t7], Order[t8], Foldable[f] =
        injectIntoX(match (v1, v2, v3, v4, v5, v6, v7, v8) -> Vector#{
            box(v1),
            box(v2),
            box(v3),
            box(v4),
            box(v5),
            box(v6),
            box(v7),
            box(v8)
        }
        , p, ts)

    ///
    /// Insert all facts in the given sequence `f` into the given relation `p`.
    ///
    /// I.e. returns a Datalog program corresponding to
    /// `#{p(t1, t2, t3, t4, t5, t6, t7, t8, t9). p(t1', t2', t3', t4', t5', t6', t7', t8', t9').}`
    /// given the sequence
    /// `(t1, t2, t3, t4, t5, t6, t7, t8, t9), (t1', t2', t3', t4', t5', t6', t7', t8', t9')`.
    ///
    @Internal
    @LoweringTarget
    pub def injectInto9(p: PredSym, ts: f[(t1, t2, t3, t4, t5, t6, t7, t8, t9)]):
        Datalog \ Foldable.Aef[f] with Order[t1], Order[t2], Order[t3], Order[t4], Order[t5], Order[t6], Order[t7], Order[t8], Order[t9], Foldable[f] =
        injectIntoX(match (v1, v2, v3, v4, v5, v6, v7, v8, v9) -> Vector#{
            box(v1),
            box(v2),
            box(v3),
            box(v4),
            box(v5),
            box(v6),
            box(v7),
            box(v8),
            box(v9)
        }
        , p, ts)

    ///
    /// Insert all facts in the given sequence `f` into the given relation `p`.
    ///
    /// I.e. returns a Datalog program corresponding to
    /// `#{p(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10).
    ///    p(t1', t2', t3', t4', t5', t6', t7', t8', t9', t10').}`
    /// given the sequence
    /// `(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10),
    ///  (t1', t2', t3', t4', t5', t6', t7', t8', t9', t10')`.
    ///
    @Internal
    @LoweringTarget
    pub def injectInto10(p: PredSym, ts: f[(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10)]):
        Datalog \ Foldable.Aef[f] with Order[t1], Order[t2], Order[t3], Order[t4], Order[t5], Order[t6], Order[t7], Order[t8], Order[t9], Order[t10], Foldable[f] =
        injectIntoX(match (v1, v2, v3, v4, v5, v6, v7, v8, v9, v10) -> Vector#{
            box(v1),
            box(v2),
            box(v3),
            box(v4),
            box(v5),
            box(v6),
            box(v7),
            box(v8),
            box(v9),
            box(v10)
        }
        , p, ts)

    ///
    /// Insert all facts in the given sequence `f` into the given relation `p`.
    ///
    /// I.e. returns a Datalog program corresponding to
    /// `#{p(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11).
    ///    p(t1', t2', t3', t4', t5', t6', t7', t8', t9', t10', t11').}`
    /// given the sequence
    /// `(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11),
    ///  (t1', t2', t3', t4', t5', t6', t7', t8', t9', t10', t11')`.
    ///
    @Internal
    @LoweringTarget
    pub def injectInto11(p: PredSym, ts: f[(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11)]):
        Datalog \ Foldable.Aef[f] with Order[t1], Order[t2], Order[t3], Order[t4], Order[t5], Order[t6], Order[t7], Order[t8], Order[t9], Order[t10], Order[t11], Foldable[f] =
        injectIntoX(match (v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11) -> Vector#{
            box(v1),
            box(v2),
            box(v3),
            box(v4),
            box(v5),
            box(v6),
            box(v7),
            box(v8),
            box(v9),
            box(v10),
            box(v11)
        }
        , p, ts)

    ///
    /// Insert all facts in the given sequence `f` into the given relation `p`.
    ///
    /// I.e. returns a Datalog program corresponding to
    /// `#{p(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12).
    ///    p(t1', t2', t3', t4', t5', t6', t7', t8', t9', t10', t11', t12').}`
    /// given the sequence
    /// `(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12),
    ///  (t1', t2', t3', t4', t5', t6', t7', t8', t9', t10', t11', t12')`.
    ///
    @Internal
    @LoweringTarget
    pub def injectInto12(p: PredSym, ts: f[(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12)]):
        Datalog \ Foldable.Aef[f] with Order[t1], Order[t2], Order[t3], Order[t4], Order[t5], Order[t6], Order[t7], Order[t8], Order[t9], Order[t10], Order[t11], Order[t12], Foldable[f] =
        injectIntoX(match (v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12) -> Vector#{
            box(v1),
            box(v2),
            box(v3),
            box(v4),
            box(v5),
            box(v6),
            box(v7),
            box(v8),
            box(v9),
            box(v10),
            box(v11),
            box(v12)
        }
        , p, ts)

    ///
    /// Insert all facts in the given sequence `f` into the given relation `p`.
    ///
    /// I.e. returns a Datalog program corresponding to
    /// `#{p(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13).
    ///    p(t1', t2', t3', t4', t5', t6', t7', t8', t9', t10', t11', t12', t13').}`
    /// given the sequence
    /// `(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13),
    ///  (t1', t2', t3', t4', t5', t6', t7', t8', t9', t10', t11', t12', t13')`.
    ///
    @Internal
    @LoweringTarget
    pub def injectInto13(p: PredSym, ts: f[(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13)]):
        Datalog \ Foldable.Aef[f] with Order[t1], Order[t2], Order[t3], Order[t4], Order[t5], Order[t6], Order[t7], Order[t8], Order[t9], Order[t10], Order[t11], Order[t12], Order[t13], Foldable[f] =
        injectIntoX(match (v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13) -> Vector#{
            box(v1),
            box(v2),
            box(v3),
            box(v4),
            box(v5),
            box(v6),
            box(v7),
            box(v8),
            box(v9),
            box(v10),
            box(v11),
            box(v12),
            box(v13)
        }
        , p, ts)

    ///
    /// Insert all facts in the given sequence `f` into the given relation `p`.
    ///
    /// I.e. returns a Datalog program corresponding to
    /// `#{p(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14).
    ///    p(t1', t2', t3', t4', t5', t6', t7', t8', t9', t10', t11', t12', t13', t14').}`
    /// given the sequence
    /// `(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14),
    ///  (t1', t2', t3', t4', t5', t6', t7', t8', t9', t10', t11', t12', t13', t14')`.
    ///
    @Internal
    @LoweringTarget
    pub def injectInto14(p: PredSym, ts: f[(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14)]):
        Datalog \ Foldable.Aef[f] with Order[t1], Order[t2], Order[t3], Order[t4], Order[t5], Order[t6], Order[t7], Order[t8], Order[t9], Order[t10], Order[t11], Order[t12], Order[t13], Order[t14], Foldable[f] =
        injectIntoX(match (v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14) -> Vector#{
            box(v1),
            box(v2),
            box(v3),
            box(v4),
            box(v5),
            box(v6),
            box(v7),
            box(v8),
            box(v9),
            box(v10),
            box(v11),
            box(v12),
            box(v13),
            box(v14)
        }
        , p, ts)

    ///
    /// Insert all facts in the given sequence `f` into the given relation `p`.
    ///
    /// I.e. returns a Datalog program corresponding to
    /// `#{p(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15).
    ///    p(t1', t2', t3', t4', t5', t6', t7', t8', t9', t10', t11', t12', t13', t14', t15').}`
    /// given the sequence
    /// `(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15),
    ///  (t1', t2', t3', t4', t5', t6', t7', t8', t9', t10', t11', t12', t13', t14', t15')`.
    ///
    @Internal
    @LoweringTarget
    pub def injectInto15(p: PredSym, ts: f[(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15)]):
        Datalog \ Foldable.Aef[f] with Order[t1], Order[t2], Order[t3], Order[t4], Order[t5], Order[t6], Order[t7], Order[t8], Order[t9], Order[t10], Order[t11], Order[t12], Order[t13], Order[t14], Order[t15], Foldable[f] =
        injectIntoX(match (v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15) -> Vector#{
            box(v1),
            box(v2),
            box(v3),
            box(v4),
            box(v5),
            box(v6),
            box(v7),
            box(v8),
            box(v9),
            box(v10),
            box(v11),
            box(v12),
            box(v13),
            box(v14),
            box(v15)
        }
        , p, ts)

    def injectIntoX(f: t -> Vector[Boxed] \ ef, p: PredSym, ts: f[t]): Datalog \ (ef + Foldable.Aef[f]) with Foldable[f] =
        unchecked_cast(({region rc {
            let db = MutMap.empty(rc);
            Foldable.foldLeft(() -> t -> {
                let vs = f(t);
                let arity = Vector.length(vs);
                let ramSym = RelSym.Symbol(p, arity, Relational);
                let rel = MutMap.getOrElsePut(ramSym, BPlusTree.empty(Static), db);
                BPlusTree.put(vs, Boxed.NoValue, rel)
            }, (), ts);
            toModel(db)
        // Remove the `IO` effect from dealing with `Static`.
        }}: _ \ IO + ef + Foldable.Aef[f]) as _ \ ef + Foldable.Aef[f])

    ///
    /// Returns all facts in `d` associated with the predicate symbol `p`.
    ///
    @Internal
    @LoweringTarget
    pub def facts0(p: PredSym, d: Datalog): Vector[t] with Order[t] =
        // Unsafe cast is required. If a predicate is nullary then its values must be Unit.
        factsOf(_ -> unchecked_cast(() as t), p, d)

    ///
    /// Returns all facts in `d` associated with the predicate symbol `p`.
    ///
    @Internal
    @LoweringTarget
    pub def facts1(p: PredSym, d: Datalog): Vector[t] with Order[t] =
        let f = terms -> (
            unbox(Vector.get(0, terms))
        );
        factsOf(f, p, d)

    ///
    /// Returns all facts in `d` associated with the predicate symbol `p`.
    ///
    @Internal
    @LoweringTarget
    pub def facts2(p: PredSym, d: Datalog): Vector[(t1, t2)] with Order[t1], Order[t2] =
        let f = terms -> (
            unbox(Vector.get(0, terms)),
            unbox(Vector.get(1, terms))
        );
        factsOf(f, p, d)

    ///
    /// Returns all facts in `d` associated with the predicate symbol `p`.
    ///
    @Internal
    @LoweringTarget
    pub def facts3(p: PredSym, d: Datalog): Vector[(t1, t2, t3)] with Order[t1], Order[t2], Order[t3] =
        let f = terms -> (
            unbox(Vector.get(0, terms)),
            unbox(Vector.get(1, terms)),
            unbox(Vector.get(2, terms))
        );
        factsOf(f, p, d)

    ///
    /// Returns all facts in `d` associated with the predicate symbol `p`.
    ///
    @Internal
    @LoweringTarget
    pub def facts4(p: PredSym, d: Datalog): Vector[(t1, t2, t3, t4)] with Order[t1], Order[t2], Order[t3], Order[t4] =
        let f = terms -> (
            unbox(Vector.get(0, terms)),
            unbox(Vector.get(1, terms)),
            unbox(Vector.get(2, terms)),
            unbox(Vector.get(3, terms))
        );
        factsOf(f, p, d)

    ///
    /// Returns all facts in `d` associated with the predicate symbol `p`.
    ///
    @Internal
    @LoweringTarget
    pub def facts5(p: PredSym, d: Datalog): Vector[(t1, t2, t3, t4, t5)] with Order[t1], Order[t2], Order[t3], Order[t4], Order[t5] =
        let f = terms -> (
            unbox(Vector.get(0, terms)),
            unbox(Vector.get(1, terms)),
            unbox(Vector.get(2, terms)),
            unbox(Vector.get(3, terms)),
            unbox(Vector.get(4, terms))
        );
        factsOf(f, p, d)

    ///
    /// Returns all facts in `d` associated with the predicate symbol `p`.
    ///
    @Internal
    @LoweringTarget
    pub def facts6(p: PredSym, d: Datalog): Vector[(t1, t2, t3, t4, t5, t6)] with Order[t1], Order[t2], Order[t3], Order[t4], Order[t5], Order[t6] =
        let f = terms -> (
            unbox(Vector.get(0, terms)),
            unbox(Vector.get(1, terms)),
            unbox(Vector.get(2, terms)),
            unbox(Vector.get(3, terms)),
            unbox(Vector.get(4, terms)),
            unbox(Vector.get(5, terms))
        );
        factsOf(f, p, d)

    ///
    /// Returns all facts in `d` associated with the predicate symbol `p`.
    ///
    @Internal
    @LoweringTarget
    pub def facts7(p: PredSym, d: Datalog): Vector[(t1, t2, t3, t4, t5, t6, t7)] with Order[t1], Order[t2], Order[t3], Order[t4], Order[t5], Order[t6], Order[t7] =
        let f = terms -> (
            unbox(Vector.get(0, terms)),
            unbox(Vector.get(1, terms)),
            unbox(Vector.get(2, terms)),
            unbox(Vector.get(3, terms)),
            unbox(Vector.get(4, terms)),
            unbox(Vector.get(5, terms)),
            unbox(Vector.get(6, terms))
        );
        factsOf(f, p, d)

    ///
    /// Returns all facts in `d` associated with the predicate symbol `p`.
    ///
    @Internal
    @LoweringTarget
    pub def facts8(p: PredSym, d: Datalog): Vector[(t1, t2, t3, t4, t5, t6, t7, t8)] with Order[t1], Order[t2], Order[t3], Order[t4], Order[t5], Order[t6], Order[t7], Order[t8] =
        let f = terms -> (
            unbox(Vector.get(0, terms)),
            unbox(Vector.get(1, terms)),
            unbox(Vector.get(2, terms)),
            unbox(Vector.get(3, terms)),
            unbox(Vector.get(4, terms)),
            unbox(Vector.get(5, terms)),
            unbox(Vector.get(6, terms)),
            unbox(Vector.get(7, terms))
        );
        factsOf(f, p, d)

    ///
    /// Returns all facts in `d` associated with the predicate symbol `p`.
    ///
    @Internal
    @LoweringTarget
    pub def facts9(p: PredSym, d: Datalog): Vector[(t1, t2, t3, t4, t5, t6, t7, t8, t9)] with Order[t1], Order[t2], Order[t3], Order[t4], Order[t5], Order[t6], Order[t7], Order[t8], Order[t9] =
        let f = terms -> (
            unbox(Vector.get(0, terms)),
            unbox(Vector.get(1, terms)),
            unbox(Vector.get(2, terms)),
            unbox(Vector.get(3, terms)),
            unbox(Vector.get(4, terms)),
            unbox(Vector.get(5, terms)),
            unbox(Vector.get(6, terms)),
            unbox(Vector.get(7, terms)),
            unbox(Vector.get(8, terms))
        );
        factsOf(f, p, d)

    ///
    /// Returns all facts in `d` associated with the predicate symbol `p`.
    ///
    @Internal
    @LoweringTarget
    pub def facts10(p: PredSym, d: Datalog): Vector[(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10)] with Order[t1], Order[t2], Order[t3], Order[t4], Order[t5], Order[t6], Order[t7], Order[t8], Order[t9], Order[t10] =
        let f = terms -> (
            unbox(Vector.get(0, terms)),
            unbox(Vector.get(1, terms)),
            unbox(Vector.get(2, terms)),
            unbox(Vector.get(3, terms)),
            unbox(Vector.get(4, terms)),
            unbox(Vector.get(5, terms)),
            unbox(Vector.get(6, terms)),
            unbox(Vector.get(7, terms)),
            unbox(Vector.get(8, terms)),
            unbox(Vector.get(9, terms))
        );
        factsOf(f, p, d)

    ///
    /// Returns all facts in `d` associated with the predicate symbol `p`.
    ///
    @Internal
    @LoweringTarget
    pub def facts11(p: PredSym, d: Datalog): Vector[(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11)] with Order[t1], Order[t2], Order[t3], Order[t4], Order[t5], Order[t6], Order[t7], Order[t8], Order[t9], Order[t10], Order[t11] =
        let f = terms -> (
            unbox(Vector.get(0, terms)),
            unbox(Vector.get(1, terms)),
            unbox(Vector.get(2, terms)),
            unbox(Vector.get(3, terms)),
            unbox(Vector.get(4, terms)),
            unbox(Vector.get(5, terms)),
            unbox(Vector.get(6, terms)),
            unbox(Vector.get(7, terms)),
            unbox(Vector.get(8, terms)),
            unbox(Vector.get(9, terms)),
            unbox(Vector.get(10, terms))
        );
        factsOf(f, p, d)

    ///
    /// Returns all facts in `d` associated with the predicate symbol `p`.
    ///
    @Internal
    @LoweringTarget
    pub def facts12(p: PredSym, d: Datalog): Vector[(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12)] with Order[t1], Order[t2], Order[t3], Order[t4], Order[t5], Order[t6], Order[t7], Order[t8], Order[t9], Order[t10], Order[t11], Order[t12] =
        let f = terms -> (
            unbox(Vector.get(0, terms)),
            unbox(Vector.get(1, terms)),
            unbox(Vector.get(2, terms)),
            unbox(Vector.get(3, terms)),
            unbox(Vector.get(4, terms)),
            unbox(Vector.get(5, terms)),
            unbox(Vector.get(6, terms)),
            unbox(Vector.get(7, terms)),
            unbox(Vector.get(8, terms)),
            unbox(Vector.get(9, terms)),
            unbox(Vector.get(10, terms)),
            unbox(Vector.get(11, terms))
        );
        factsOf(f, p, d)

    ///
    /// Returns all facts in `d` associated with the predicate symbol `p`.
    ///
    @Internal
    @LoweringTarget
    pub def facts13(p: PredSym, d: Datalog): Vector[(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13)] with Order[t1], Order[t2], Order[t3], Order[t4], Order[t5], Order[t6], Order[t7], Order[t8], Order[t9], Order[t10], Order[t11], Order[t12], Order[t13] =
        let f = terms -> (
            unbox(Vector.get(0, terms)),
            unbox(Vector.get(1, terms)),
            unbox(Vector.get(2, terms)),
            unbox(Vector.get(3, terms)),
            unbox(Vector.get(4, terms)),
            unbox(Vector.get(5, terms)),
            unbox(Vector.get(6, terms)),
            unbox(Vector.get(7, terms)),
            unbox(Vector.get(8, terms)),
            unbox(Vector.get(9, terms)),
            unbox(Vector.get(10, terms)),
            unbox(Vector.get(11, terms)),
            unbox(Vector.get(12, terms))
        );
        factsOf(f, p, d)

    ///
    /// Returns all facts in `d` associated with the predicate symbol `p`.
    ///
    @Internal
    @LoweringTarget
    pub def facts14(p: PredSym, d: Datalog): Vector[(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14)] with Order[t1], Order[t2], Order[t3], Order[t4], Order[t5], Order[t6], Order[t7], Order[t8], Order[t9], Order[t10], Order[t11], Order[t12], Order[t13], Order[t14] =
        let f = terms -> (
            unbox(Vector.get(0, terms)),
            unbox(Vector.get(1, terms)),
            unbox(Vector.get(2, terms)),
            unbox(Vector.get(3, terms)),
            unbox(Vector.get(4, terms)),
            unbox(Vector.get(5, terms)),
            unbox(Vector.get(6, terms)),
            unbox(Vector.get(7, terms)),
            unbox(Vector.get(8, terms)),
            unbox(Vector.get(9, terms)),
            unbox(Vector.get(10, terms)),
            unbox(Vector.get(11, terms)),
            unbox(Vector.get(12, terms)),
            unbox(Vector.get(13, terms))
        );
        factsOf(f, p, d)

    ///
    /// Returns all facts in `d` associated with the predicate symbol `p`.
    ///
    @Internal
    @LoweringTarget
    pub def facts15(p: PredSym, d: Datalog): Vector[(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15)] with Order[t1], Order[t2], Order[t3], Order[t4], Order[t5], Order[t6], Order[t7], Order[t8], Order[t9], Order[t10], Order[t11], Order[t12], Order[t13], Order[t14], Order[t15] =
        let f = terms -> (
            unbox(Vector.get(0, terms)),
            unbox(Vector.get(1, terms)),
            unbox(Vector.get(2, terms)),
            unbox(Vector.get(3, terms)),
            unbox(Vector.get(4, terms)),
            unbox(Vector.get(5, terms)),
            unbox(Vector.get(6, terms)),
            unbox(Vector.get(7, terms)),
            unbox(Vector.get(8, terms)),
            unbox(Vector.get(9, terms)),
            unbox(Vector.get(10, terms)),
            unbox(Vector.get(11, terms)),
            unbox(Vector.get(12, terms)),
            unbox(Vector.get(13, terms)),
            unbox(Vector.get(14, terms))
        );
        factsOf(f, p, d)

    ///
    /// Returns an array of facts associated with the given predicate symbol `p` in the given Datalog program `d`.
    ///
    def factsOf(f: Vector[Boxed] -> t \ ef, p: PredSym, d: Datalog): Vector[t] \ ef =
        unchecked_cast(factsOfInternal(f, p, d) as _ \ ef)

    ///
    /// Returns an array of facts associated with the given predicate symbol `p` in the given Datalog program `d`.
    ///
    def factsOfInternal(f: Vector[Boxed] -> t \ ef, p: PredSym, d: Datalog): Vector[t] \ ef + IO = match d {
        case Datalog(_, cs) => region rc {
            let pFacts = MutList.empty(rc);
            foreach (Constraint(HeadAtom(headPred, _, terms), body) <- cs) {
                if (headPred == p and Vector.isEmpty(body)) {
                    let vs = Vector.map(headTermValue, terms);
                    MutList.push(f(vs), pFacts)
                }
            };
            MutList.toVector(pFacts)
        }
        case Model(db) => region rc {
            let pFacts = MutList.empty(rc);
            let qquery = ramSym -> match ramSym {
                case RelSym.Symbol(predSym, _, _) => match predSym <=> p {
                    case Comparison.EqualTo => Comparison.EqualTo
                    case cmp => cmp
                }
                case _ => Comparison.LessThan
            };
            db |>
                Map.rangeQueryWith(qquery, ramSym -> rel -> match toDenotation(ramSym) {
                    case Relational =>
                        BPlusTree.forEach(tuple -> _ -> MutList.push(f(tuple), pFacts), rel)
                    case Latticenal(bot, leq, _, _) =>
                        BPlusTree.forEach(tuple -> lat -> {
                            if (not leq(lat, bot)) {
                                let arity = Vector.length(tuple);
                                let vs = Vector.init(i -> {
                                    if (i < arity)
                                        Vector.get(i, tuple)
                                    else
                                        lat
                                }, arity + 1);
                                MutList.push(f(vs), pFacts)
                            }
                        }, rel)
                });
            MutList.toVector(pFacts)
        }
        case Join(d1, d2)      => factsOf(f, p, d1) ++ factsOf(f, p, d2)
        case Provenance(_, db) => match Map.get(Ram.makeFakeRelSym(p), db) {
            case None        => Vector#{}
            case Some(facts) => region rc {
                let acc = MutList.empty(rc);
                BPlusTree.forEach(k -> _ -> MutList.push(f(k), acc), facts);
                unchecked_cast((MutList.toArray(rc, acc): Array[t, rc]) as Vector[t])
            }
        }
    }

    ///
    /// Returns the value of the given head term `v`.
    ///
    /// Note the term must have been fully evaluated to a literal value.
    ///
    def headTermValue(t: HeadTerm): Boxed = match t {
        case HeadTerm.Lit(v) => v
        case _               => bug!("Unexpected non-value term.")
    }

    ///
    /// Returns the given database `db` as a Datalog value.
    ///
    def toModelOnlyFull(withProv: Bool, rules: Vector[Constraint], dbMeta: (Database[r], Predicates)): Datalog \ r = {
        let (db, meta) = dbMeta;
        let dbOnlyFullRels = MutMap.toMap(db) |>
            Map.filterWithKey(k -> facts -> match k {
                case RelSym.Symbol(PredSym(_, id), _, _) => Predicate.isFullId(id, meta) and not (unsafely IO run BPlusTree.isEmpty(facts))
            });
        if (withProv) {
            toProvenance(rules, dbOnlyFullRels)
        } else {
            Model(dbOnlyFullRels)
        }
    }

    ///
    /// Returns the given database `db` augmented with provenance as a Provenance value.
    ///
    def toProvenance(rules: Vector[Constraint], db: Map[RelSym, BPlusTree[Vector[Boxed], Boxed, Static]]): Datalog = unsafely IO run {
        (Map#{}, db)
            ||> Map.foldLeftWithKey(acc -> relSym -> facts -> {
                let len = Vector.length(fst(getOrCrash(BPlusTree.minimumKey(facts))));
                let newTree = BPlusTree.empty(Static);
                foreach ((fact, _) <- facts) {
                    let (rawFact, annotation) = Vector.splitAt(len - 2, fact);
                    newTree |> BPlusTree.put(rawFact, (unbox(Vector.get(0, annotation)), getOrCrash(Int64.tryToInt32(unbox(Vector.get(1, annotation))))))
                };
                Map.insert(relSym, newTree, acc)
            })
            |> Provenance(rules)
    }

    ///
    /// Returns the given database `db` as a Datalog value.
    ///
    def toModel(db: Database[r]): Datalog \ r =
        Model(MutMap.toMap(db))

}

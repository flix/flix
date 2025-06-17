mod Fixpoint.SolverApi {

    import dev.flix.runtime.Global

    use Fixpoint.Ast.Datalog.Datalog
    use Fixpoint.Ast.Shared.PredSym
    use Fixpoint.Ast.Shared.PredSym.PredSym
    use Fixpoint.Toggle.useNewSolver
    use Fixpoint.Boxed

    ///
    /// Returns the minimal model of the given Datalog program `d`.
    /// The minimal model is given by the model semantics for Datalog programs with stratified negation.
    /// The stratification is computed automatically by the solver.
    ///
    @Internal
    pub def runSolver(d: Datalog): Datalog =
        if(useNewSolver()) {
            Fixpoint3.Solver.runSolver(d)
        } else {
            Fixpoint.Solver.runSolver(d)
        }

    ///
    /// Returns the pairwise union of `d1` and `d2`.
    /// I.e. the facts of the union is the union of the facts and likewise for rules.
    /// A fact or rule may occur twice in the Datalog program. This has no effect on its semantics.
    ///
    @Internal
    pub def union(d1: Datalog, d2: Datalog): Datalog =
        if(useNewSolver()) {
            Fixpoint3.Solver.union(d1, d2)
        } else {
            Fixpoint.Solver.union(d1, d2)
        }

    ///
    /// Project all facts in the given Datalog program `d` that has `p` in its head.
    ///
    @Internal
    pub def projectSym(p: PredSym, d: Datalog): Datalog = 
        if(useNewSolver()) {
            Fixpoint3.Solver.projectSym(p, d)
        } else {
            Fixpoint.Solver.projectSym(p, d)
        }

    ///
    /// Renames every predicate symbol in `d` with a fresh name, except for those in `preds`.
    ///
    @Internal
    pub def rename(preds: List[PredSym], d: Datalog): Datalog = 
        if(useNewSolver()) {
            Fixpoint3.Solver.rename(preds, d)
        } else {
            Fixpoint.Solver.rename(preds, d)
        }


    ///
    /// Insert all facts in the given sequence `f` into the given relation `p`.
    ///
    /// I.e. returns a Datalog program corresponding to `#{p(t1). p(t1').}`
    /// given the sequence `(t1), (t1')`.
    ///
    @Internal
    pub def injectInto1(p: PredSym, ts: f[t1]):
        Datalog \ Foldable.Aef[f] with Order[t1], Foldable[f] =
        if(useNewSolver()) {
            Fixpoint3.Solver.injectInto1(p, ts)
        } else {
            Fixpoint.Solver.injectInto1(p, ts)
        }

    ///
    /// Insert all facts in the given sequence `f` into the given relation `p`.
    ///
    /// I.e. returns a Datalog program corresponding to `#{p(t1, t2). p(t1', t2').}`
    /// given the sequence `(t1, t2), (t1', t2')`.
    ///
    @Internal
    pub def injectInto2(p: PredSym, ts: f[(t1, t2)]):
        Datalog \ Foldable.Aef[f] with Order[t1], Order[t2], Foldable[f] =
        if(useNewSolver()) {
            Fixpoint3.Solver.injectInto2(p, ts)
        } else {
            Fixpoint.Solver.injectInto2(p, ts)
        }

    ///
    /// Insert all facts in the given sequence `f` into the given relation `p`.
    ///
    /// I.e. returns a Datalog program corresponding to `#{p(t1, t2, t3). p(t1', t2', t3').}`
    /// given the sequence `(t1, t2, t3), (t1', t2', t3')`.
    ///
    @Internal
    pub def injectInto3(p: PredSym, ts: f[(t1, t2, t3)]):
        Datalog \ Foldable.Aef[f] with Order[t1], Order[t2], Order[t3], Foldable[f] =
        if(useNewSolver()) {
            Fixpoint3.Solver.injectInto3(p, ts)
        } else {
            Fixpoint.Solver.injectInto3(p, ts)
        }

    ///
    /// Insert all facts in the given sequence `f` into the given relation `p`.
    ///
    /// I.e. returns a Datalog program corresponding to
    /// `#{p(t1, t2, t3, t4). p(t1', t2', t3', t4').}`
    /// given the sequence `(t1, t2, t3, t4), (t1', t2', t3', t4')`.
    ///
    @Internal
    pub def injectInto4(p: PredSym, ts: f[(t1, t2, t3, t4)]):
        Datalog \ Foldable.Aef[f] with Order[t1], Order[t2], Order[t3], Order[t4], Foldable[f] =
        if(useNewSolver()) {
            Fixpoint3.Solver.injectInto4(p, ts)
        } else {
            Fixpoint.Solver.injectInto4(p, ts)
        }

    ///
    /// Insert all facts in the given sequence `f` into the given relation `p`.
    ///
    /// I.e. returns a Datalog program corresponding to
    /// `#{p(t1, t2, t3, t4, t5). p(t1', t2', t3', t4', t5').}`
    /// given the sequence `(t1, t2, t3, t4, t5), (t1', t2', t3', t4', t5')`.
    ///
    @Internal
    pub def injectInto5(p: PredSym, ts: f[(t1, t2, t3, t4, t5)]):
        Datalog \ Foldable.Aef[f] with Order[t1], Order[t2], Order[t3], Order[t4], Order[t5], Foldable[f] =
        if(useNewSolver()) {
            Fixpoint3.Solver.injectInto5(p, ts)
        } else {
            Fixpoint.Solver.injectInto5(p, ts)
        }

    ///
    /// Insert all facts in the given sequence `f` into the given relation `p`.
    ///
    /// I.e. returns a Datalog program corresponding to
    /// `#{p(t1, t2, t3, t4, t5, t6). p(t1', t2', t3', t4', t5', t6').}`
    /// given the sequence `(t1, t2, t3, t4, t5, t6), (t1', t2', t3', t4', t5', t6')`.
    ///
    @Internal
    pub def injectInto6(p: PredSym, ts: f[(t1, t2, t3, t4, t5, t6)]):
        Datalog \ Foldable.Aef[f] with Order[t1], Order[t2], Order[t3], Order[t4], Order[t5], Order[t6], Foldable[f] =
        if(useNewSolver()) {
            Fixpoint3.Solver.injectInto6(p, ts)
        } else {
            Fixpoint.Solver.injectInto6(p, ts)
        }

    ///
    /// Insert all facts in the given sequence `f` into the given relation `p`.
    ///
    /// I.e. returns a Datalog program corresponding to
    /// `#{p(t1, t2, t3, t4, t5, t6, t7). p(t1', t2', t3', t4', t5', t6', t7').}`
    /// given the sequence `(t1, t2, t3, t4, t5, t6, t7), (t1', t2', t3', t4', t5', t6', t7')`.
    ///
    @Internal
    pub def injectInto7(p: PredSym, ts: f[(t1, t2, t3, t4, t5, t6, t7)]):
        Datalog \ Foldable.Aef[f] with Order[t1], Order[t2], Order[t3], Order[t4], Order[t5], Order[t6], Order[t7], Foldable[f] =
        if(useNewSolver()) {
            Fixpoint3.Solver.injectInto7(p, ts)
        } else {
            Fixpoint.Solver.injectInto7(p, ts)
        }

    ///
    /// Insert all facts in the given sequence `f` into the given relation `p`.
    ///
    /// I.e. returns a Datalog program corresponding to
    /// `#{p(t1, t2, t3, t4, t5, t6, t7, t8). p(t1', t2', t3', t4', t5', t6', t7', t8').}`
    /// given the sequence
    /// `(t1, t2, t3, t4, t5, t6, t7, t8), (t1', t2', t3', t4', t5', t6', t7', t8')`.
    ///
    @Internal
    pub def injectInto8(p: PredSym, ts: f[(t1, t2, t3, t4, t5, t6, t7, t8)]):
        Datalog \ Foldable.Aef[f] with Order[t1], Order[t2], Order[t3], Order[t4], Order[t5], Order[t6], Order[t7], Order[t8], Foldable[f] =
        if(useNewSolver()) {
            Fixpoint3.Solver.injectInto8(p, ts)
        } else {
            Fixpoint.Solver.injectInto8(p, ts)
        }

    ///
    /// Insert all facts in the given sequence `f` into the given relation `p`.
    ///
    /// I.e. returns a Datalog program corresponding to
    /// `#{p(t1, t2, t3, t4, t5, t6, t7, t8, t9). p(t1', t2', t3', t4', t5', t6', t7', t8', t9').}`
    /// given the sequence
    /// `(t1, t2, t3, t4, t5, t6, t7, t8, t9), (t1', t2', t3', t4', t5', t6', t7', t8', t9')`.
    ///
    @Internal
    pub def injectInto9(p: PredSym, ts: f[(t1, t2, t3, t4, t5, t6, t7, t8, t9)]):
        Datalog \ Foldable.Aef[f] with Order[t1], Order[t2], Order[t3], Order[t4], Order[t5], Order[t6], Order[t7], Order[t8], Order[t9], Foldable[f] =
        if(useNewSolver()) {
            Fixpoint3.Solver.injectInto9(p, ts)
        } else {
            Fixpoint.Solver.injectInto9(p, ts)
        }

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
    pub def injectInto10(p: PredSym, ts: f[(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10)]):
        Datalog \ Foldable.Aef[f] with Order[t1], Order[t2], Order[t3], Order[t4], Order[t5], Order[t6], Order[t7], Order[t8], Order[t9], Order[t10], Foldable[f] =
        if(useNewSolver()) {
            Fixpoint3.Solver.injectInto10(p, ts)
        } else {
            Fixpoint.Solver.injectInto10(p, ts)
        }

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
    pub def injectInto11(p: PredSym, ts: f[(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11)]):
        Datalog \ Foldable.Aef[f] with Order[t1], Order[t2], Order[t3], Order[t4], Order[t5], Order[t6], Order[t7], Order[t8], Order[t9], Order[t10], Order[t11], Foldable[f] =
        if(useNewSolver()) {
            Fixpoint3.Solver.injectInto11(p, ts)
        } else {
            Fixpoint.Solver.injectInto11(p, ts)
        }

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
    pub def injectInto12(p: PredSym, ts: f[(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12)]):
        Datalog \ Foldable.Aef[f] with Order[t1], Order[t2], Order[t3], Order[t4], Order[t5], Order[t6], Order[t7], Order[t8], Order[t9], Order[t10], Order[t11], Order[t12], Foldable[f] =
        if(useNewSolver()) {
            Fixpoint3.Solver.injectInto12(p, ts)
        } else {
            Fixpoint.Solver.injectInto12(p, ts)
        }

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
    pub def injectInto13(p: PredSym, ts: f[(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13)]):
        Datalog \ Foldable.Aef[f] with Order[t1], Order[t2], Order[t3], Order[t4], Order[t5], Order[t6], Order[t7], Order[t8], Order[t9], Order[t10], Order[t11], Order[t12], Order[t13], Foldable[f] =
        if(useNewSolver()) {
            Fixpoint3.Solver.injectInto13(p, ts)
        } else {
            Fixpoint.Solver.injectInto13(p, ts)
        }

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
    pub def injectInto14(p: PredSym, ts: f[(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14)]):
        Datalog \ Foldable.Aef[f] with Order[t1], Order[t2], Order[t3], Order[t4], Order[t5], Order[t6], Order[t7], Order[t8], Order[t9], Order[t10], Order[t11], Order[t12], Order[t13], Order[t14], Foldable[f] =
        if(useNewSolver()) {
            Fixpoint3.Solver.injectInto14(p, ts)
        } else {
            Fixpoint.Solver.injectInto14(p, ts)
        }

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
    pub def injectInto15(p: PredSym, ts: f[(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15)]):
        Datalog \ Foldable.Aef[f] with Order[t1], Order[t2], Order[t3], Order[t4], Order[t5], Order[t6], Order[t7], Order[t8], Order[t9], Order[t10], Order[t11], Order[t12], Order[t13], Order[t14], Order[t15], Foldable[f] =
        if(useNewSolver()) {
            Fixpoint3.Solver.injectInto15(p, ts)
        } else {
            Fixpoint.Solver.injectInto15(p, ts)
        }

    // @Internal
    // pub def injectIntoX(f: t -> List[Boxed] \ ef, p: PredSym, ts: f[t]): Datalog \ (ef + Foldable.Aef[f]) with Foldable[f] =
    //     if(useNewSolver()) {
    //         Fixpoint3.Solver.injectIntoX(f, p, ts)
    //     } else {
    //         Fixpoint.Solver.injectIntoX(f, p, ts)
    //     }

    ///
    /// Returns all facts in `d` associated with the predicate symbol `p`.
    ///
    @Internal
    pub def facts0(p: PredSym, d: Datalog): Vector[t] with Order[t] =
        if(useNewSolver()) {
            Fixpoint3.Solver.facts0(p, d)
        } else {
            Fixpoint.Solver.facts0(p, d)
        }

    ///
    /// Returns all facts in `d` associated with the predicate symbol `p`.
    ///
    @Internal
    pub def facts1(p: PredSym, d: Datalog): Vector[t] with Order[t] =
        if(useNewSolver()) {
            Fixpoint3.Solver.facts1(p, d)
        } else {
            Fixpoint.Solver.facts1(p, d)
        }

    ///
    /// Returns all facts in `d` associated with the predicate symbol `p`.
    ///
    @Internal
    pub def facts2(p: PredSym, d: Datalog): Vector[(t1, t2)] with Order[t1], Order[t2] =
        if(useNewSolver()) {
            Fixpoint3.Solver.facts2(p, d)
        } else {
            Fixpoint.Solver.facts2(p, d)
        }

    ///
    /// Returns all facts in `d` associated with the predicate symbol `p`.
    ///
    @Internal
    pub def facts3(p: PredSym, d: Datalog): Vector[(t1, t2, t3)] with Order[t1], Order[t2], Order[t3] =
        if(useNewSolver()) {
            Fixpoint3.Solver.facts3(p, d)
        } else {
            Fixpoint.Solver.facts3(p, d)
        }

    ///
    /// Returns all facts in `d` associated with the predicate symbol `p`.
    ///
    @Internal
    pub def facts4(p: PredSym, d: Datalog): Vector[(t1, t2, t3, t4)] with Order[t1], Order[t2], Order[t3], Order[t4] =
        if(useNewSolver()) {
            Fixpoint3.Solver.facts4(p, d)
        } else {
            Fixpoint.Solver.facts4(p, d)
        }

    ///
    /// Returns all facts in `d` associated with the predicate symbol `p`.
    ///
    @Internal
    pub def facts5(p: PredSym, d: Datalog): Vector[(t1, t2, t3, t4, t5)] with Order[t1], Order[t2], Order[t3], Order[t4], Order[t5] =
        if(useNewSolver()) {
            Fixpoint3.Solver.facts5(p, d)
        } else {
            Fixpoint.Solver.facts5(p, d)
        }

    ///
    /// Returns all facts in `d` associated with the predicate symbol `p`.
    ///
    @Internal
    pub def facts6(p: PredSym, d: Datalog): Vector[(t1, t2, t3, t4, t5, t6)] with Order[t1], Order[t2], Order[t3], Order[t4], Order[t5], Order[t6] =
        if(useNewSolver()) {
            Fixpoint3.Solver.facts6(p, d)
        } else {
            Fixpoint.Solver.facts6(p, d)
        }

    ///
    /// Returns all facts in `d` associated with the predicate symbol `p`.
    ///
    @Internal
    pub def facts7(p: PredSym, d: Datalog): Vector[(t1, t2, t3, t4, t5, t6, t7)] with Order[t1], Order[t2], Order[t3], Order[t4], Order[t5], Order[t6], Order[t7] =
        if(useNewSolver()) {
            Fixpoint3.Solver.facts7(p, d)
        } else {
            Fixpoint.Solver.facts7(p, d)
        }

    ///
    /// Returns all facts in `d` associated with the predicate symbol `p`.
    ///
    @Internal
    pub def facts8(p: PredSym, d: Datalog): Vector[(t1, t2, t3, t4, t5, t6, t7, t8)] with Order[t1], Order[t2], Order[t3], Order[t4], Order[t5], Order[t6], Order[t7], Order[t8] =
        if(useNewSolver()) {
            Fixpoint3.Solver.facts8(p, d)
        } else {
            Fixpoint.Solver.facts8(p, d)
        }

    ///
    /// Returns all facts in `d` associated with the predicate symbol `p`.
    ///
    @Internal
    pub def facts9(p: PredSym, d: Datalog): Vector[(t1, t2, t3, t4, t5, t6, t7, t8, t9)] with Order[t1], Order[t2], Order[t3], Order[t4], Order[t5], Order[t6], Order[t7], Order[t8], Order[t9] =
        if(useNewSolver()) {
            Fixpoint3.Solver.facts9(p, d)
        } else {
            Fixpoint.Solver.facts9(p, d)
        }

    ///
    /// Returns all facts in `d` associated with the predicate symbol `p`.
    ///
    @Internal
    pub def facts10(p: PredSym, d: Datalog): Vector[(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10)] with Order[t1], Order[t2], Order[t3], Order[t4], Order[t5], Order[t6], Order[t7], Order[t8], Order[t9], Order[t10] =
        if(useNewSolver()) {
            Fixpoint3.Solver.facts10(p, d)
        } else {
            Fixpoint.Solver.facts10(p, d)
        }

    ///
    /// Returns all facts in `d` associated with the predicate symbol `p`.
    ///
    @Internal
    pub def facts11(p: PredSym, d: Datalog): Vector[(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11)] with Order[t1], Order[t2], Order[t3], Order[t4], Order[t5], Order[t6], Order[t7], Order[t8], Order[t9], Order[t10], Order[t11] =
        if(useNewSolver()) {
            Fixpoint3.Solver.facts11(p, d)
        } else {
            Fixpoint.Solver.facts11(p, d)
        }

    ///
    /// Returns all facts in `d` associated with the predicate symbol `p`.
    ///
    @Internal
    pub def facts12(p: PredSym, d: Datalog): Vector[(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12)] with Order[t1], Order[t2], Order[t3], Order[t4], Order[t5], Order[t6], Order[t7], Order[t8], Order[t9], Order[t10], Order[t11], Order[t12] =
        if(useNewSolver()) {
            Fixpoint3.Solver.facts12(p, d)
        } else {
            Fixpoint.Solver.facts12(p, d)
        }

    ///
    /// Returns all facts in `d` associated with the predicate symbol `p`.
    ///
    @Internal
    pub def facts13(p: PredSym, d: Datalog): Vector[(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13)] with Order[t1], Order[t2], Order[t3], Order[t4], Order[t5], Order[t6], Order[t7], Order[t8], Order[t9], Order[t10], Order[t11], Order[t12], Order[t13] =
        if(useNewSolver()) {
            Fixpoint3.Solver.facts13(p, d)
        } else {
            Fixpoint.Solver.facts13(p, d)
        }

    ///
    /// Returns all facts in `d` associated with the predicate symbol `p`.
    ///
    @Internal
    pub def facts14(p: PredSym, d: Datalog): Vector[(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14)] with Order[t1], Order[t2], Order[t3], Order[t4], Order[t5], Order[t6], Order[t7], Order[t8], Order[t9], Order[t10], Order[t11], Order[t12], Order[t13], Order[t14] =
        if(useNewSolver()) {
            Fixpoint3.Solver.facts14(p, d)
        } else {
            Fixpoint.Solver.facts14(p, d)
        }

    ///
    /// Returns all facts in `d` associated with the predicate symbol `p`.
    ///
    @Internal
    pub def facts15(p: PredSym, d: Datalog): Vector[(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15)] with Order[t1], Order[t2], Order[t3], Order[t4], Order[t5], Order[t6], Order[t7], Order[t8], Order[t9], Order[t10], Order[t11], Order[t12], Order[t13], Order[t14], Order[t15] =
        if(useNewSolver()) {
            Fixpoint3.Solver.facts15(p, d)
        } else {
            Fixpoint.Solver.facts15(p, d)
        }
}

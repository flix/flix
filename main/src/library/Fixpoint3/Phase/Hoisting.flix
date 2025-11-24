/*
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


///
/// The purpose of this phase is to hoist `If` ops as much as possible and merge them
/// with `Search` into `Query` when possible/relevant. Additionally, guards are added
/// at the top of `Insert` statements that only perform the operations if none of the
/// involved relations are empty. For example, the following
/// ```
/// search a ∈ A do
///     search b ∈ B do
///         search c ∈ C do
///             if (b[0] = a[0] ∧ c[0] = a[0]) then
///                 project (a[0]) into R
///             end
///         end
///     end
/// end
/// ```
/// is hoisted to
/// ```
/// if (not (A = ∅) ∧ not (B = ∅) not (C = ∅)) then
///     search a ∈ A do
///         query {b ∈ B | b[0] = a[0]} do
///             query {c ∈ C | c[0] = a[0]} do
///                 project (a[0]) into R
///             end
///         end
///     end
/// end
/// ```
///
/// One hard dependency of the current implementation is that for `BoolExp.Eq(x, y)`, `x`
/// and `y` must either be `Lit` or `RowLoad(_ ,_, _)`.
///
/// The hoisting in general happens with a `MutDisjointSets`, called `equalitySets`,
/// which is populated by `BoolExp.Eq(RowLoad, RowLoad)`. In this way, all `RowLoad`'s
/// which are equal will be treated as one.
///
/// `termMap` contains a map from the representing `RowLoad`, with respect to `equalitySets`,
/// to the `RamTerm` highest in the AST where the value of `Rep` is known.
///
/// For example, given:
/// ```
/// search a ∈ A do
///     search b ∈ B do
///         search c ∈ C do
///             if (b[0] = a[0] ∧ c[0] = a[0], b[1] = 1, b[2] = c[1]) then
///                 project (a[0]) into R
///             end
///         end
///     end
/// end
/// ```
/// `equalitySets` could be `{a[0] => a[0], b[0] => a[0], c[0] => a[0], b[1] => b[1], b[2] => c[1], c[1] => c[1]}`.
///
/// Since `equalitySets` is only used to get representatives for loads, the exact value of the representative
/// load is irrelevant. As long as `a[0]`, `b[0]` and `c[0]` get the same representative, the actual value
/// could be any of them. Also note that `b[1]` only points to itself, not `1`. The fact that `b[1] = 1` would
/// be maintained by `termMap`.
///
/// `termMap` will be initialized with `{b[1] => 1}`.
///
/// When arriving at `search b ∈ B`, `termMap` will be `{b[1] => 1, a[0] => a[0]}`.
///
/// When arriving at `search c ∈ C`, `termMap` will be `{b[1] => 1, a[0] => a[0], c[1] => b[2]}`.
///
/// When arriving at `if (...)` and `project(...)` `termMap` will remain unchanged.
///
mod Fixpoint3.Phase.Hoisting {
    use Fixpoint3.Ast.Ram.{arityOf, arityOfNonLat, BoolExp, RamProgram, RamStmt, RamTerm, RelOp, RelSym, RowVar}
    use Fixpoint3.Boxed
    use Fixpoint3.Phase.Compiler.functionalRelSym
    use Fixpoint3.Util.getOrCrash
    use Fixpoint3.TypeInfo.Type

    ///
    /// Hoists `program` as described above.
    ///
    @Internal
    pub def hoistProgram(program: RamProgram): RamProgram = match program {
        case RamProgram.Program(stmt, facts, meta, index, typeInfo) => region rc {
            let equalitySets = MutDisjointSets.empty(rc);
            let constEqualities = MutMap.empty(rc);
            // Compute transitive equality
            unifyEqualitiesStmt(equalitySets, constEqualities, stmt);
            let impossible = MutSet.empty(rc);
            // For all `x[i] = constant`, register this requirement in `termMap`. If 2 such requirements
            // exists for `constant1!=constant2`, register `x[i]` as impossible to satisfy.
            // Note that with the above unification, we do not really care about `x[i]`, but some representative.
            // We might have `x[i] = x[j] = y[k]` and we view these as the same.
            let termMap = (Map.empty(), constEqualities) ||>
                MutMap.foldLeftWithKey(acc -> load -> listOfPairs ->
                    (acc, listOfPairs) ||>
                        List.foldLeft(innerAcc -> match (val, ty) -> {
                            // `load` must have been registered by `unifyEqualitiesStmt`.
                            let rep = getOrCrash(MutDisjointSets.find(load, equalitySets));
                            match Map.get(rep, innerAcc) {
                                case Some(RamTerm.Lit(v, _)) => {
                                        if (v != val) { MutSet.add(fst(rep), impossible) }
                                    };
                                    innerAcc
                                case Some(_) => bug!("In Fixpoint.Phase.Hoisting.hoistProgram: termMap contains non-literals before hoistStmt!")
                                case None    => Map.insert(rep, RamTerm.Lit(val, ty), innerAcc)
                            }
                        })
                );
            match hoistStmt(rc, termMap, equalitySets, MutSet.toSet(impossible), stmt) {
                case None => RamProgram.Program(RamStmt.Comment("No rules are satisfiable"), facts, meta, index, typeInfo)
                case Some(hoistedStmt) => RamProgram.Program(hoistedStmt, facts, meta, index, typeInfo)
            }
        }
    }

    ///
    /// Hoist `If` ops in `stmt` and merge them with `Search` ops when relevant.
    ///
    /// `termMap` is used to look up which term a given `(RowVar, Int32)` is represented by and
    /// whether they are currently considered ground. A load is considered ground if its representative
    /// is a key in `termMap`.
    ///
    /// `equalitySets` is used to find representative terms, given the seen equalities.
    ///
    /// `impossible` contains the set of `RowVar`'s (rules) which have been determined impossible to satisfy.
    ///
    /// `None` is returned when a statement is determined to be irrelevant, with respect to `impossible`,
    /// or if all its children are.
    ///
    def hoistStmt(
        rc: Region[r],
        termMap: Map[(RowVar, Int32), RamTerm],
        equalitySets: MutDisjointSets[(RowVar, Int32), r],
        impossible: Set[RowVar],
        stmt: RamStmt
    ): Option[RamStmt] \ r = match stmt {
        case RamStmt.Insert(op) =>
            // independent is a list of BoolExp which do not depend on any `RowVar`s.
            let independent = MutList.empty(rc);
            let optNewOp = hoistOp(rc, equalitySets, impossible, MutMap.empty(rc), independent, termMap, op);
            match optNewOp {
                case None => None
                case Some(newOp) =>
                    // Create a guard ensuring that no `RelSym` in the expression is empty. Note that this
                    // only include RelSym in `Search` and `Query`, not negative dependencies.
                    let isEmptyBools = relSymsOfStmt(stmt)
                        |> List.map(relSym -> BoolExp.Not(BoolExp.IsEmpty(relSym)))
                        |> List.toVector;
                    let newStmt =
                        if (MutList.size(independent) == 0) {
                            RamStmt.Insert(RelOp.If(isEmptyBools, newOp))
                        } else {
                            let bools = MutList.toVector(independent) ++ isEmptyBools;
                            RamStmt.Insert(RelOp.If(bools, newOp))
                        };
                    Some(newStmt)
            }
        case RamStmt.MergeInto(_, _) => Some(stmt)
        case RamStmt.Swap(_, _) => Some(stmt)
        case RamStmt.Purge(_) => Some(stmt)
        case RamStmt.Seq(xs) =>
            let children =
                Vector.filterMap(hoistStmt(rc, termMap, equalitySets, impossible), xs);
            match Vector.length(children) {
                case 0 => None
                case 1 => Vector.head(children)
                case _ => Some(RamStmt.Seq(children))
            }
        case RamStmt.Par(xs) =>
            let children =
                Vector.filterMap(hoistStmt(rc, termMap, equalitySets, impossible), xs);
            match Vector.length(children) {
                case 0 => None
                case 1 => Vector.head(children)
                case _ => Some(RamStmt.Par(children))
            }
        case RamStmt.Until(test, body) =>
            match hoistStmt(rc, termMap, equalitySets, impossible, body) {
                case None => None
                case Some(v) => Some(RamStmt.Until(test, v))
            }
        case RamStmt.Comment(_) => Some(stmt)
    }

    ///
    /// Returns a list of the `RelSym`'s appearing in `stmt` (`RamStmt.Insert`).
    ///
    /// Only includes non-negative dependencies.
    ///
    def relSymsOfStmt(stmt: RamStmt): List[RelSym] = match stmt {
        case RamStmt.Insert(op) => relSymsOfOp(op)
        case _ => unreachable!()
    }

    ///
    /// Returns a list of the `RelSym`'s appearing positively in `op`.
    ///
    /// Only includes non-negative dependencies.
    ///
    def relSymsOfOp(op: RelOp): List[RelSym] = match op {
        case RelOp.Search(_, relSym, _, body)      => relSym :: relSymsOfOp(body)
        case RelOp.Query(_, relSym, _, _, _, body) => relSym :: relSymsOfOp(body)
        case RelOp.Functional(_, _, _, body, _, _) => relSymsOfOp(body)
        case RelOp.Project(_, _, _)                => Nil
        case RelOp.If(_, body)                     => relSymsOfOp(body)
    }

    ///
    /// Hoists statements in `op` as far as possible. See `hoistStmt`.
    ///
    /// `termMap` maps loads (e.g. `x[0]`) to a literals or other loads bound higher in the AST.
    /// More specifically the representing load for `x[0]` will be used, where representing stems from
    /// `equalitySets`.
    ///
    /// `equalitySets` maps loads (e.g. `x[0]`) to a load that represents a transitively equivalent `RowLoad`.
    /// If we have x[0] = x[1] = y[4], then looking up any of these will consistently return the same value/load.
    ///
    /// `impossible` is a set of `RowVar` that are involved in unsatisfying conditions. If they are encountered,
    /// `None` will be returned immediately.
    ///
    /// `idToBool` maps `RowVar`s to a list of `BoolExp`s which the `RowVar` occurs in.
    ///
    /// `independentBool` is a list of `BoolExp`s which do not depend on any `RowVar`s.
    ///
    /// Returns `None` if a `RowVar` in `impossible` is met.
    ///
    /// Returns `Some(v)` where `v` is a hoisted version of `op`.
    ///
    def hoistOp(
        rc: Region[r],
        equalitySets: MutDisjointSets[(RowVar, Int32), r],
        impossible: Set[RowVar],
        idToBool: MutMap[RowVar, MutList[BoolExp, r], r],
        independentBool: MutList[BoolExp, r],
        termMap: Map[(RowVar, Int32), RamTerm],
        op: RelOp
    ): Option[RelOp] \ r =
        let hoistOpRec = hoistOp(rc, equalitySets, impossible, idToBool, independentBool, termMap);
        let hoistOpRecNewTermMap = hoistOp(rc, equalitySets, impossible, idToBool, independentBool);
        match op {
            // We here move every `BoolExp` into `Query` even though IndexSelection can only handle `Eq`.
            // This is done in case a newer index selection scheme makes use of those `BoolExp`.
            case RelOp.Search(rowVar, relSym, t, body) =>
                if (Set.memberOf(rowVar, impossible))
                    None
                else {
                    // Register all values `rowVar[i]`, for `0 <= i < arity`, as known when hoisting `body`.
                    let termMapWithCur = registerNewTerms(rowVar, termMap, equalitySets, t, relSym);
                    let optHoistedBody = hoistOpRecNewTermMap(termMapWithCur, body);
                    match optHoistedBody {
                        case None => None
                        case Some(hoistedBody) =>
                            let (intraEqualities, knownEqualities) = constructEqualityConstraints(rowVar, relSym, t, termMapWithCur, equalitySets);
                            // Expressions are hoisted to this point if they are ground with `rowVar` bound,
                            // but not without `rowVar`.
                            let hoistedToHere = getBoolsHoistedToHere(rowVar, termMap, termMapWithCur, equalitySets, idToBool);
                            let combined = List.toVector(knownEqualities ::: intraEqualities ::: hoistedToHere);
                            let newOp =
                                if (Vector.length(combined) != 0)
                                    RelOp.Query(rowVar, relSym, combined, -1, t, hoistedBody)
                                else RelOp.Search(rowVar, relSym, t, hoistedBody);
                            Some(newOp)
                    }
            }
            case RelOp.Query(_, _, _, _, _, _) =>
                bug!("In Fixpoint.Phase.Hoisting.hoistOp: Query should not exists when introducing Query")
            case RelOp.Functional(rowVar, func, terms, body, arity, t) =>
                let fakeRelSym = functionalRelSym(arity);
                let termMapWithCur = registerNewTerms(rowVar, termMap, equalitySets, t, fakeRelSym);
                match hoistOpRec(body) {
                    case None => None
                    case Some(v) => {
                        // The 'output' variables from `Functional` may not have been bound earlier.
                        // Forceful removal from `termMap` is therefore safe.
                        let hoistedToHere = getBoolsHoistedToHere(rowVar, termMap, termMapWithCur, equalitySets, idToBool);

                        if (List.nonEmpty(hoistedToHere)) {
                            let newBody = RelOp.If(List.toVector(hoistedToHere) |> Vector.map(replaceByRepBoolExp(termMapWithCur, equalitySets)), v);
                            Some(RelOp.Functional(rowVar, func, terms, newBody, arity, t))
                        } else {
                            Some(RelOp.Functional(rowVar, func, terms, v, arity, t))
                        }
                    }
                }
            case RelOp.Project(_, _, _) => Some(op)
            case RelOp.If(boolExps, body) =>
                // Just note all `boolExps`. They will be placed as high as possible in the AST by other cases.
                // If `boolExps` contains no variables, push it all the way to the top, otherwise register it for
                // all variables occurring in `boolExp`.
                foreach (b <- boolExps) {
                    match b {
                        // Don't collect Eq's: they are handled separately to ensure we refer to the earliest evaluated expression.
                        case BoolExp.Eq(_, _) => ()
                        case _ =>
                            let vars = boolVariablesOf(termMap, equalitySets, b);
                            if (Set.size(vars) == 0)
                                MutList.push(b, independentBool)
                            else {
                                foreach (v <- vars) {
                                    let boolList = MutMap.getOrElsePut(v, MutList.empty(rc), idToBool);
                                    MutList.push(b, boolList)
                                }
                            }
                    }
                };
                hoistOpRec(body)
        }

    ///
    /// Based on `termMapCopy`, for each `rowVar[i]`, test whether its value is known/ground.
    /// Its value is known if `termMapCopy` maps to a value which is not a
    /// `RowLoad(rowVar, _ , _)`.
    ///
    /// If the value is known, generate a constraint of
    /// `Eq(RowLoad(rowVar, i, relSym), termMap[rep(rowVar, i)])`.
    ///
    /// If `termMapCopy` returns `RowLoad(rowVar, j, _)` for `i != j`, this generates a
    /// constraint of `Eq(RowLoad(rowVar, i, relSym), RowLoad(rowVar, j, relSym))`.
    ///
    /// Returns a pair of lists where the first list contains the first kind of constraints
    /// and the second list contains the second kind of constraints.
    ///
    def constructEqualityConstraints(
        rowVar: RowVar,
        relSym: RelSym,
        types: Vector[Int32],
        termMapCopy: Map[(RowVar, Int32), RamTerm],
        equalitySets: MutDisjointSets[(RowVar, Int32), r]
    ): (List[BoolExp], List[BoolExp]) \ r = region rc {
        let intraEqualities = MutList.empty(rc);
        let knownEqualities = MutList.empty(rc);
        foreach (i <- Vector.range(0, arityOf(relSym))) {
            match MutDisjointSets.find((rowVar, i), equalitySets) {
                case None => ()
                case Some(rep) => match getOrCrash(Map.get(rep, termMapCopy)) {
                    // If `rep => rowVar[_]`, then this is the place `rep` was considered bound
                    // and we unbind it by removing it from `termMap`.
                    // Furthermore, if `rep => rowVar[i1]`, where `i1 != i`, then `rowVar[i] == rowVar[i1]`.
                    // We cannot currently use this for index selection, but need to keep this invariant, so it is pushed to `intraEqualities`
                    case RamTerm.RowLoad(rv, i1, _, _) if (rv == rowVar and i1 == i) => ()
                    case RamTerm.RowLoad(rv, i1, t, rel) if (rv == rowVar) =>
                        intraEqualities |> MutList.push(BoolExp.Eq(RamTerm.RowLoad(rv, i, t, rel), RamTerm.RowLoad(rv, i1, t, rel)))
                    case value =>
                        let t = Vector.get(i, types);
                        MutList.push(BoolExp.Eq(RamTerm.RowLoad(rowVar, i, t, relSym), value), knownEqualities)
            }}
        };
        (MutList.toList(knownEqualities), MutList.toList(intraEqualities))
    }

    ///
    /// Registers all terms `rv[i]` in `termMap`.
    ///
    /// If `rep(rv[i])` already exists as a key in `termMap`, nothing is changed. Otherwise,
    /// `termMap[rep(rv[i])] = rep(rv[i])` afterwards.
    ///
    def registerNewTerms(
        rv: RowVar,
        termMap: Map[(RowVar, Int32), RamTerm],
        equalitySets: MutDisjointSets[(RowVar, Int32), r],
        types: Vector[Int32],
        relSym: RelSym
    ): Map[(RowVar, Int32), RamTerm] \ r = {
        // Register all values `rowVar[i]`, for `0 <= i < arity`, as known when hoisting `body`.
        (termMap, Vector.range(0, arityOf(relSym))) ||>
            Vector.foldLeft(acc -> i -> {
                match MutDisjointSets.find((rv, i), equalitySets) {
                    case Some(rep) => Map.insertWith(_ -> v -> v, rep, RamTerm.RowLoad(rv, i, Vector.get(i, types), relSym), acc)
                    case None => bug!("In Fixpoint.Phase.Hoisting: Everything should be in the equalitySet")
                }
            })
    }

    ///
    /// Returns the list ground of `BoolExp`s in `termMap[rv]`.
    ///
    def keepGroundBools(
        rv: RowVar,
        termMap: Map[(RowVar, Int32), RamTerm],
        equalitySets: MutDisjointSets[(RowVar, Int32), r],
        idToBool: MutMap[RowVar, MutList[BoolExp, r], r]
    ): List[BoolExp] \ r = {
        match MutMap.get(rv, idToBool) {
            case None => Nil
            case Some(v) =>
                MutList.toList(v) |> List.filter(isBoolGround(termMap, equalitySets))
        }
    }

    ///
    /// Returns a list of `BoolExp` that can be hoisted no higher than to after `rv` is
    /// bound.
    ///
    /// Concretely, it returns all bools which are ground with respect to `termMapCopy`,
    /// but not with respect to `termMap`.
    ///
    def getBoolsHoistedToHere(
        rv: RowVar,
        termMap: Map[(RowVar, Int32), RamTerm],
        termMapCopy: Map[(RowVar, Int32), RamTerm],
        equalitySets: MutDisjointSets[(RowVar, Int32), r],
        idToBool: MutMap[RowVar, MutList[BoolExp, r], r]
    ): List[BoolExp] \ r = {
        keepGroundBools(rv, termMapCopy, equalitySets, idToBool)
            |> List.filter(x -> not isBoolGround(termMap, equalitySets, x))
            |> List.map(replaceByRepBoolExp(termMapCopy, equalitySets))
    }

    ///
    /// Returns the set of `RowVar` in `bool` which are mapped to other `RowVar` with respect to `termMap`.
    ///
    /// Ignores `Eq` as `Eq`'s are seen as a special case and will always be a comparison between loads and
    /// loads/constants.
    ///
    /// As an example, `Guard2(x[0], y[3])` will return `{x}` if `termMap` is e.g. `[y[3] => 8]`.
    /// If `termMap` contained `y[3]=> z[3]`, then `{x, z}` will be returned.
    ///
    /// The purpose is to return the `RowVar`'s in the AST whose `RelOp`'s are possible
    /// places `bool` can be hoisted to.
    ///
    def boolVariablesOf(
        termMap: Map[(RowVar, Int32), RamTerm],
        equalitySets: MutDisjointSets[(RowVar, Int32), r],
        bexp: BoolExp
    ): Set[RowVar] \ r =
        let termVarsOf = termVariablesOf(termMap, equalitySets);
        match bexp {
            case BoolExp.Not(boolExp) => boolVariablesOf(termMap, equalitySets, boolExp)
            case BoolExp.IsEmpty(_) => Set#{}
            case BoolExp.NotMemberOf(terms, _) => Vector.foldMap(termVarsOf, terms)
            case BoolExp.NotBot(t1, _, _) => termVarsOf(t1)
            case BoolExp.Leq(_, rv, relSym) => getRepRowVar(termMap, equalitySets, rv, arityOfNonLat(relSym))
            case BoolExp.Eq(_, _) => Set#{}
            case BoolExp.Guard1(_, t1) => termVarsOf(t1)
            case BoolExp.Guard2(_, t1, t2) => Set.union(termVarsOf(t1), termVarsOf(t2))
            case BoolExp.Guard3(_, t1, t2, t3) =>
                Set.union(Set.union(termVarsOf(t1), termVarsOf(t2)), termVarsOf(t3))
            case BoolExp.Guard4(_, t1, t2, t3, t4) =>
                Set.union(Set.union(Set.union(termVarsOf(t1), termVarsOf(t2)), termVarsOf(t3)), termVarsOf(t4))
            case BoolExp.Guard5(_, t1, t2, t3, t4, t5) =>
                Set.union(Set.union(Set.union(Set.union(termVarsOf(t1), termVarsOf(t2)), termVarsOf(t3)), termVarsOf(t4)), termVarsOf(t5))
    }

    ///
    /// Returns the set of `RowVar` in `term`. See `boolVariablesOf`.
    ///
    def termVariablesOf(
        termMap: Map[(RowVar, Int32), RamTerm],
        equalitySets: MutDisjointSets[(RowVar, Int32), r],
        term: RamTerm
    ): Set[RowVar] \ r =
        let termVarsOf = termVariablesOf(termMap, equalitySets);
        let getRepVar = getRepRowVar(termMap, equalitySets);
        match term {
            case RamTerm.Lit(_, _)                    => Set#{}
            case RamTerm.ProvMax(vec)                 => Vector.foldLeft(acc -> match (rv, _) -> Set.insert(rv, acc), Set#{}, vec)
            case RamTerm.RowLoad(rv, i, _, _)         => getRepVar(rv, i)
            case RamTerm.Meet(_, t1, (rv, relSym), _) => Set.union(termVarsOf(t1), getRepVar(rv, arityOfNonLat(relSym)))
            case RamTerm.App1(_, t1, _)               => termVarsOf(t1)
            case RamTerm.App2(_, t1, t2, _) =>
                Vector#{t1, t2} |>
                    Vector.foldRight(t -> acc -> termVarsOf(t) `Set.union` acc, Set#{})
            case RamTerm.App3(_, t1, t2, t3, _) =>
                Vector#{t1, t2, t3} |>
                    Vector.foldRight(t -> acc -> termVarsOf(t) `Set.union` acc, Set#{})
            case RamTerm.App4(_, t1, t2, t3, t4, _) =>
                Vector#{t1, t2, t3, t4} |>
                    Vector.foldRight(t -> acc -> termVarsOf(t) `Set.union` acc, Set#{})
            case RamTerm.App5(_, t1, t2, t3, t4, t5, _) =>
                Vector#{t1, t2, t3, t4, t5} |>
                    Vector.foldRight(t -> acc -> termVarsOf(t) `Set.union` acc, Set#{})
        }

    ///
    /// Returns the (set of) `RowVar` that represents `term` with respect to `termMap`.
    ///
    /// See `boolVariablesOf`.
    ///
    def getRepRowVar(
        termMap: Map[(RowVar, Int32), RamTerm],
        equalitySets: MutDisjointSets[(RowVar, Int32), r],
        rv: RowVar,
        index: Int32
    ): Set[RowVar] \ r =
        let repLoad = getOrCrash(MutDisjointSets.find((rv, index), equalitySets));
        match Map.get(repLoad, termMap) {
            case None => Set#{rv}
            case Some(rep) => match rep {
                case RamTerm.RowLoad(rv1, _, _, _) => Set#{rv1}
                case RamTerm.Lit(_, _) => Set#{}
                case _ => unreachable!()
            }
    }

    ///
    /// Replace terms in `bool` by their representative.
    ///
    /// In particular, `RowLoad(rv, i, _)` will be replaced by termMap[equalitySet[(rv, i)]].
    /// This ensures that we all loads refer to variables as far up in the AST as possible,
    /// and therefore variables which are still bound after hoisting.
    ///
    /// `Meet` and `ProvMax` are excluded from this treatment. `ProvMax` should not exist
    /// at this point. `Meet` is simply used to ensure that for `A(;x), B(;x)`, we will
    /// meet `A[0]` and `B[0]`. Even though `A[0] = B[0]` should never exist we would not
    /// want to replace `Meet(A[0], B[0])` by `Meet(A[0], A[0])`.
    ///
    def replaceByRepBoolExp(
        termMap: Map[(RowVar, Int32), RamTerm],
        equalitySets: MutDisjointSets[(RowVar, Int32), r],
        bexp: BoolExp
    ): BoolExp \ r =
        let replaceRec = replaceByRepBoolExp(termMap, equalitySets);
        let replaceTerm = replaceByRepRamTerm(termMap, equalitySets);
        match bexp {
            case BoolExp.Not(boolExp) => BoolExp.Not(replaceRec(boolExp))
            case BoolExp.IsEmpty(_) => bexp
            case BoolExp.NotMemberOf(terms, rel) => BoolExp.NotMemberOf(Vector.map(replaceTerm, terms), rel)
            case BoolExp.NotBot(t1, f, bot) => BoolExp.NotBot(replaceTerm(t1), f, bot)
            case BoolExp.Leq(_, _, _) => bexp
            case BoolExp.Eq(t1, t2) => BoolExp.Eq(replaceTerm(t1), replaceTerm(t2))
            case BoolExp.Guard1(f, t1) => BoolExp.Guard1(f, replaceTerm(t1))
            case BoolExp.Guard2(f, t1, t2) => BoolExp.Guard2(f, replaceTerm(t1), replaceTerm(t2))
            case BoolExp.Guard3(f, t1, t2, t3) => BoolExp.Guard3(f, replaceTerm(t1), replaceTerm(t2), replaceTerm(t3))
            case BoolExp.Guard4(f, t1, t2, t3, t4) => BoolExp.Guard4(f, replaceTerm(t1), replaceTerm(t2), replaceTerm(t3), replaceTerm(t4))
            case BoolExp.Guard5(f, t1, t2, t3, t4, t5) => BoolExp.Guard5(f, replaceTerm(t1), replaceTerm(t2), replaceTerm(t3), replaceTerm(t4), replaceTerm(t5))
    }

    ///
    /// Replace terms in `ramTerm` by their representative. See `replaceByRepBoolExp`
    ///
    def replaceByRepRamTerm(
        termMap: Map[(RowVar, Int32), RamTerm],
        equalitySets: MutDisjointSets[(RowVar, Int32), r],
        ramTerm: RamTerm
    ): RamTerm \ r =
        let getRep = term -> match term {
            case RamTerm.RowLoad(rv, i, _, _) =>
                let load = getOrCrash(MutDisjointSets.find((rv, i), equalitySets));
                match Map.get(load, termMap) {
                    case None => term
                    case Some(rep) => rep
                }
            case _ => term
        };
        let replaceRec = replaceByRepRamTerm(termMap, equalitySets);
        match ramTerm {
            case RamTerm.Lit(_, _)                       => ramTerm
            case RamTerm.ProvMax(_)                      => ramTerm
            case RamTerm.RowLoad(_, _, _, _)             => getRep(ramTerm)
            case RamTerm.Meet(_, _, _, _)                => ramTerm
            case RamTerm.App1(f, t1, ty)                 => RamTerm.App1(f, replaceRec(t1), ty)
            case RamTerm.App2(f, t1, t2, ty)             => RamTerm.App2(f, replaceRec(t1), replaceRec(t2), ty)
            case RamTerm.App3(f, t1, t2, t3, ty)         => RamTerm.App3(f, replaceRec(t1), replaceRec(t2), replaceRec(t3), ty)
            case RamTerm.App4(f, t1, t2, t3, t4, ty)     => RamTerm.App4(f, replaceRec(t1), replaceRec(t2), replaceRec(t3), replaceRec(t4), ty)
            case RamTerm.App5(f, t1, t2, t3, t4, t5, ty) => RamTerm.App5(f, replaceRec(t1), replaceRec(t2), replaceRec(t3), replaceRec(t4), replaceRec(t5), ty)
    }


    ///
    /// Returns `true` if `bool` is ground with respect to `termMap` and `equalitySets`.
    ///
    /// See `isTermGround`.
    ///
    def isBoolGround(
        termMap: Map[(RowVar, Int32), RamTerm],
        equalitySets: MutDisjointSets[(RowVar, Int32), r],
        bexp: BoolExp
    ): Bool \ r =
        let termGround = isTermGround(termMap, equalitySets);
        match bexp {
            case BoolExp.Not(boolExp) => isBoolGround(termMap, equalitySets, boolExp)
            case BoolExp.IsEmpty(_) => unreachable!()
            case BoolExp.NotMemberOf(terms, _) => Vector.forAll(termGround, terms)
            case BoolExp.NotBot(t1, _, _) => termGround(t1)
            case BoolExp.Leq(_, rv, relSym) =>
                Map.memberOf(getOrCrash(MutDisjointSets.find((rv, arityOfNonLat(relSym)), equalitySets)), termMap)
            case BoolExp.Eq(t1, t2) => termGround(t1) and termGround(t2)
            case BoolExp.Guard1(_, t1) => termGround(t1)
            case BoolExp.Guard2(_, t1, t2) => termGround(t1) and termGround(t2)
            case BoolExp.Guard3(_, t1, t2, t3) => termGround(t1) and termGround(t2) and termGround(t3)
            case BoolExp.Guard4(_, t1, t2, t3, t4) => termGround(t1) and termGround(t2) and termGround(t3) and termGround(t4)
            case BoolExp.Guard5(_, t1, t2, t3, t4, t5) => termGround(t1) and termGround(t2) and termGround(t3) and termGround(t4) and termGround(t5)
    }

    ///
    /// Returns `true` if `term` is ground with respect to `termMap` and `equalitySets`.
    ///
    /// A `RowLoad` is ground if its representing `(rv, index)` with respect to `equalitySets`
    /// is in `termMap`.
    ///
    def isTermGround(
        termMap: Map[(RowVar, Int32), RamTerm],
        equalitySets: MutDisjointSets[(RowVar, Int32), r],
        term: RamTerm
    ): Bool \ r =
        let termGround = isTermGround(termMap, equalitySets);
        match term {
            case RamTerm.Lit(_, _) => true
            case RamTerm.ProvMax(loads) =>
                loads |>
                    Vector.forAll(load -> {
                        Map.memberOf(getOrCrash(MutDisjointSets.find(load, equalitySets)), termMap)
                    })
            case RamTerm.RowLoad(rv, i, _, _) =>
                Map.memberOf(getOrCrash(MutDisjointSets.find((rv, i), equalitySets)), termMap)
            case RamTerm.Meet(_, t1, (rv, relSym), _) => termGround(t1) and Map.memberOf(getOrCrash(MutDisjointSets.find((rv, arityOfNonLat(relSym)), equalitySets)), termMap)
            case RamTerm.App1(_, t1, _) => termGround(t1)
            case RamTerm.App2(_, t1, t2, _) => termGround(t1) and termGround(t2)
            case RamTerm.App3(_, t1, t2, t3, _) => termGround(t1) and termGround(t2) and termGround(t3)
            case RamTerm.App4(_, t1, t2, t3, t4, _) => termGround(t1) and termGround(t2) and termGround(t3) and termGround(t4)
            case RamTerm.App5(_, t1, t2, t3, t4, t5, _) => termGround(t1) and termGround(t2) and termGround(t3) and termGround(t4) and termGround(t5)
        }

    ///
    /// Collects equality information in `stmt` and save it in `equalitySets` and `constEqualities`.
    /// Equalities between 2 `RowLoad`s are added to `equalitySets` and equalities between `RowLoad`
    /// and `Lit` are added to `constEqualities`.
    ///
    /// In `equalitySets`, we unify `(rv1, i1)` with `(rv2, i2)` if they occur together
    /// in an equality statement.
    ///
    /// In `constEqualities`, we save a list of which literals a `(rv, i)` were equal to.
    ///
    @Internal
    pub def unifyEqualitiesStmt(
        equalitySets: MutDisjointSets[(RowVar, Int32), r],
        constEqualities: MutMap[(RowVar, Int32), List[(Boxed, Type)], r],
        stmt: RamStmt
    ): Unit \ r = match stmt {
        case RamStmt.Seq(xs) => Vector.forEach(unifyEqualitiesStmt(equalitySets, constEqualities), xs)
        case RamStmt.Insert(op) => unifyEqualitiesOp(equalitySets, constEqualities, op)
        case RamStmt.Par(xs) => Vector.forEach(unifyEqualitiesStmt(equalitySets, constEqualities), xs)
        case RamStmt.Until(boolExps, body) =>
            Vector.forEach(unifyBoolExp(equalitySets, constEqualities), boolExps);
            unifyEqualitiesStmt(equalitySets, constEqualities, body)
        case RamStmt.MergeInto(_, _) => ()
        case RamStmt.Swap(_, _) => ()
        case RamStmt.Purge(_) => ()
        case RamStmt.Comment(_) => ()
    }

    ///
    /// Collects equality information in `op`.
    ///
    /// See `unifyEqualitiesStmt`.
    ///
    @Internal
    pub def unifyEqualitiesOp(
        equalitySets: MutDisjointSets[(RowVar, Int32), r],
        constEqualities: MutMap[(RowVar, Int32), List[(Boxed, Type)], r],
        op: RelOp
    ): Unit \ r = match op {
        case RelOp.Search(rowVar, RelSym.Symbol(_, arity, _), _, body) =>
            loadsIntoOf(rowVar, arity, equalitySets);
            unifyEqualitiesOp(equalitySets, constEqualities, body)
        case RelOp.Query(_, _, _, _, _, _) =>
            bug!("In Fixpoint.Phase.Hoisting.unifyEqualitiesOp: Query should not exist at this point!")
        case RelOp.Functional(rowVar, _, _, body, arity, _) =>
            loadsIntoOf(rowVar, arity, equalitySets);
            unifyEqualitiesOp(equalitySets, constEqualities, body)
        case RelOp.Project(_, _, _) => ()
        case RelOp.If(boolExps, body) =>
            Vector.forEach(unifyBoolExp(equalitySets, constEqualities), boolExps);
            unifyEqualitiesOp(equalitySets, constEqualities, body)
    }

    ///
    /// Collects equality information in `boolExp` and save it in `equalitySets` and `constEqualities`.
    ///
    /// See `unifyEqualitiesOp`.
    ///
    def unifyBoolExp(
        equalitySets: MutDisjointSets[(RowVar, Int32), r],
        constEqualities: MutMap[(RowVar, Int32), List[(Boxed, Type)], r],
        boolExp: BoolExp
    ): Unit \ r = match boolExp {
        case BoolExp.Eq(RamTerm.RowLoad(rv1, i1, _, _), RamTerm.RowLoad(rv2, i2, _, _)) =>
            MutDisjointSets.makeSet((rv1, i1), equalitySets);
            MutDisjointSets.makeSet((rv2, i2), equalitySets);
            MutDisjointSets.union((rv1, i1), (rv2, i2), equalitySets)
        case BoolExp.Eq(RamTerm.RowLoad(rv, i, _, _), RamTerm.Lit(val, ty)) =>
            MutMap.putWith(_ -> list -> (val, ty) :: list, (rv, i), (val, ty) :: Nil, constEqualities)
        case BoolExp.Eq(RamTerm.Lit(val, ty), RamTerm.RowLoad(rv, i, _, _)) =>
            MutMap.putWith(_ -> list -> (val, ty) :: list, (rv, i), (val, ty) :: Nil, constEqualities)
        case _ => ()
    }

    ///
    /// Add `(rowVar, i)`, for all `i` between `0` (inclusive) and `arity` (exlusive),
    /// to `equalitySets`.
    ///
    def loadsIntoOf(rowVar: RowVar, arity: Int32, equalitySets: MutDisjointSets[(RowVar, Int32), r]): Unit \ r =
        Vector.forEach(i -> MutDisjointSets.makeSet((rowVar, i), equalitySets), Vector.range(0, arity))

}

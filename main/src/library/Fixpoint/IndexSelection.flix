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

    use Fixpoint.Ast.Ram.{RamStmt, RelOp, RamSym, RamTerm, BoolExp, RowVar}

    ///
    /// Hoist and query optimize `stmt`.
    ///
    /// This is an optimization phase and can be omitted with no semantic effect.
    ///
    /// The purpose of this phase is to:
    /// 1) Hoist if-statements from inner loops to outer loops.
    /// An if-statement can be hoisted from its enclosing loop if it only reads variables
    /// that are bound in an outer loop.
    /// 2) Rewrite searches on relations to queries on indices, when possible.
    /// It is possible to rewrite a search when it searches on attributes that form a prefix
    /// of the attribute sequence used to lexicographically define the index.
    /// Consider the following example:
    /// search x ∈ B do
    ///     search (y, z, w) ∈ R do
    ///         search u ∈ C do
    ///             if (x = y ∧ x = w ∧ z = u ∧ x ∉ A) then
    ///                 project x into A
    /// After step 1 (hoisting):
    /// search x ∈ B do
    ///     if (x ∉ A) then
    ///         search (y, z, w) ∈ R do
    ///             if (x = y ∧ x = w) then
    ///                 search u ∈ C do
    ///                     if (z = u) then
    ///                         project x into A
    /// After step 2 (query rewriting):
    /// search x ∈ B do
    ///     if (x ∉ A) then
    ///         query {(y, z, w) ∈ R | x = y} do
    ///             if (x = w) then
    ///                 query {u ∈ C | z = u} do
    ///                     project x into A
    ///
    /// Note how the search `x = y` is optimized, but `x = w` is not.
    /// The index for R is defined by the attribute sequence Y < Z < W.
    /// Therefore the search `x = y` is a prefix search.
    /// But `x = w` is not part of any prefix and hence cannot be optimized.
    ///
    /// Step 1 and Step 2 are implemented as one pass.
    ///
    @Internal
    pub def queryStmt(stmt: RamStmt[v]): RamStmt[v] = match stmt {
        case RamStmt.Insert(op) =>
            let (innerOp, ground) = queryOp(op, Set#{});
            if (Vector.isEmpty(ground))
                RamStmt.Insert(innerOp)
            else
                RamStmt.Insert(RelOp.If(ground, innerOp))
        case RamStmt.Merge(_, _) => stmt
        case RamStmt.Assign(_, _) => stmt
        case RamStmt.Purge(_) => stmt
        case RamStmt.Seq(xs) => RamStmt.Seq(Vector.map(queryStmt, xs))
        case RamStmt.Until(test, body) => RamStmt.Until(test, queryStmt(body))
        case RamStmt.Comment(_) => stmt
    }

    /// Hoist and query optimize `op`.
    ///
    /// `freeVars` is the set of variables bound by an outer loop.
    /// Returns the optimized op and the conditions that occur in `op` that have to be hoisted.
    ///
    def queryOp(op: RelOp[v], freeVars: Set[RowVar]): (RelOp[v], Vector[BoolExp[v]]) = match op {
        case RelOp.Search(var, ramSym, body) =>
            use Fixpoint.Ast.Ram.BoolExp.Eq;
            use Fixpoint.Ast.Ram.RamTerm.{RowLoad, Lit};
            let (innerOp, innerGround) = queryOp(body, Set.insert(var, freeVars));
            let (ground, notGround) = Vector.partition(isExpGround(freeVars), innerGround);
            let (varQuery, rest1) =
                // Make sure `var` is on the lhs of all equalities.
                Vector.map(exp -> match exp {
                    case Eq(RowLoad(row1, i), RowLoad(row2, j)) =>
                        if (row2 == var)
                            Eq(RowLoad(row2, j), RowLoad(row1, i))
                        else
                            exp
                    case Eq(Lit(v), RowLoad(row, i)) => Eq(RowLoad(row, i), Lit(v))
                    case _ => exp
                }, notGround) |>
                // Partition into those equalities that have `var` on the lhs and those that don't.
                Vector.partition(exp -> match exp {
                    case Eq(RowLoad(row1, _), RowLoad(row2, _)) => row1 != row2 and row1 == var
                    case Eq(RowLoad(row, _), Lit(_)) => row == var
                    case _ => false
                });
            let (prefixQuery, rest2) = longestPrefixQuery(varQuery);
            let test = Vector.append(rest1, rest2);
            if (Vector.isEmpty(prefixQuery))
                if (Vector.isEmpty(test))
                    let search = RelOp.Search(var, ramSym, innerOp);
                    (search, ground)
                else
                    let search = RelOp.Search(var, ramSym, RelOp.If(test, innerOp));
                    (search, ground)
            else
                let query =
                    Vector.map(x -> match x {
                        case Eq(RamTerm.RowLoad(_, j), rhs) => (j, rhs)
                        case _ => ???
                    }, prefixQuery);
                if (Vector.isEmpty(test))
                    let search = RelOp.Query(var, ramSym, query, innerOp);
                    (search, ground)
                else
                    let search = RelOp.Query(var, ramSym, query, RelOp.If(test, innerOp));
                    (search, ground)
        case RelOp.Query(_) => (op, Vector.empty())
        case RelOp.Functional(_) => (op, Vector.empty())
        case RelOp.Project(_) => (op, Vector.empty())
        case RelOp.If(test, then) =>
            let (innerOp, innerGround) = queryOp(then, freeVars);
            (innerOp, Vector.append(test, innerGround))
    }

    def longestPrefixQuery(varQuery: Vector[BoolExp[v]]): (Vector[BoolExp[v]], Vector[BoolExp[v]]) =
        use Fixpoint.Ast.Ram.BoolExp.Eq;
        use Fixpoint.Ast.Ram.RamTerm.{RowLoad, Lit};
        // Sort equalities of the form `var[i] = rhs` ascending on `i`.
        Vector.sortWith(x -> y -> match (x, y) {
            case (Eq(RowLoad(_, index1), _), Eq(RowLoad(_, index2), _)) => Comparison.fromInt32(index1 - index2)
            case _ => ???
        }, varQuery) |>
        // Group `var[i] = rhs` by `i`.
        Vector.groupBy(x -> y -> match (x, y) {
            case (Eq(RowLoad(_, index1), _), Eq(RowLoad(_, index2), _)) => index1 == index2
            case _ => ???
        }) |>
        Vector.foldLeft(match (i, prefix, rest) -> e -> {
                let (p, r) = Vector.partition(be -> match be {
                    case BoolExp.Eq(RamTerm.RowLoad(_, j), _) => i == j
                    case _ => ???
                }, e);
                (i + 1, Vector.append(prefix, p), Vector.append(rest, r))
            }, (0, Vector.empty(), Vector.empty())) |>
        match (_, prefix, rest) ->
            (prefix, rest)

    ///
    /// An expression is ground if all its terms are ground.
    ///
    def isExpGround(freeVars: Set[RowVar], exp: BoolExp[v]): Bool = match exp {
        case BoolExp.Empty(_) => true
        case BoolExp.NotMemberOf(terms, _) => Vector.forAll(isTermGround(freeVars), terms)
        case BoolExp.Eq(lhs, rhs) => isTermGround(freeVars, lhs) and isTermGround(freeVars, rhs)
        case BoolExp.Leq(_, lhs, rhs) => isTermGround(freeVars, lhs) and isTermGround(freeVars, rhs)
        case BoolExp.Guard0(_) => true
        case BoolExp.Guard1(_, t) => isTermGround(freeVars, t)
        case BoolExp.Guard2(_, t1, t2) =>
            isTermGround(freeVars, t1) and
            isTermGround(freeVars, t2)
        case BoolExp.Guard3(_, t1, t2, t3) =>
            isTermGround(freeVars, t1) and
            isTermGround(freeVars, t2) and
            isTermGround(freeVars, t3)
        case BoolExp.Guard4(_, t1, t2, t3, t4) =>
            isTermGround(freeVars, t1) and
            isTermGround(freeVars, t2) and
            isTermGround(freeVars, t3) and
            isTermGround(freeVars, t4)
        case BoolExp.Guard5(_, t1, t2, t3, t4, t5) =>
            isTermGround(freeVars, t1) and
            isTermGround(freeVars, t2) and
            isTermGround(freeVars, t3) and
            isTermGround(freeVars, t4) and
            isTermGround(freeVars, t5)
    }

    ///
    /// A term is ground if it is a literal or a free variable.
    ///
    def isTermGround(freeVars: Set[RowVar], term: RamTerm[v]): Bool = match term {
        case RamTerm.Lit(_) => true
        case RamTerm.RowLoad(var, _) => Set.memberOf(var, freeVars)
        case RamTerm.LoadLatVar(var) => Set.memberOf(var, freeVars)
        case RamTerm.Meet(_, t1, t2) =>
            isTermGround(freeVars, t1) and
            isTermGround(freeVars, t2)
        case RamTerm.App0(_) => true
        case RamTerm.App1(_, t) => isTermGround(freeVars, t)
        case RamTerm.App2(_, t1, t2) =>
            isTermGround(freeVars, t1) and
            isTermGround(freeVars, t2)
        case RamTerm.App3(_, t1, t2, t3) =>
            isTermGround(freeVars, t1) and
            isTermGround(freeVars, t2) and
            isTermGround(freeVars, t3)
        case RamTerm.App4(_, t1, t2, t3, t4) =>
            isTermGround(freeVars, t1) and
            isTermGround(freeVars, t2) and
            isTermGround(freeVars, t3) and
            isTermGround(freeVars, t4)
        case RamTerm.App5(_, t1, t2, t3, t4, t5) =>
            isTermGround(freeVars, t1) and
            isTermGround(freeVars, t2) and
            isTermGround(freeVars, t3) and
            isTermGround(freeVars, t4) and
            isTermGround(freeVars, t5)
    }
}

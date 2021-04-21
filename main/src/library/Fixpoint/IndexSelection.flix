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

namespace Fixpoint {
    ///
    /// Hoist and query optimize `stmt`.
    ///
    /// The purpose of this phase is to:
    /// 1) Hoist if-statements from inner loops to outer loops.
    /// An if-statement can be hoisted from its enclosing loop if it only reads variables
    /// that are bound in an outer loop.
    /// 2) Rewrite searches on relations to queries on indices, when possible.
    /// It is possible to rewrite a searc when it searches on attributes that form a prefix
    /// of the attribute sequence used to lexicographically define the index.
    /// Consider the following example:
    /// search x ∈ b do
    ///     search (y, z, w) ∈ r do
    ///         search u ∈ c do
    ///             if (x = y ∧ x = w ∧ z = u ∧ x ∉ a) then
    ///                 project x into a
    /// After step 1:h
    /// search x ∈ b do
    ///     if (x ∉ a) then
    ///         search (y, z, w) ∈ r do
    ///             if (x = y ∧ x = w) then
    ///                 search u ∈ c do
    ///                     if (z = u) then
    ///                         project x into a
    /// After step 2:
    /// search x ∈ b do
    ///     if (x ∉ a) then
    ///         query {(y, z, w) ∈ r | x = y} do
    ///             if (x = w) then
    ///                 query {u ∈ c | z = u} do
    ///                     project x into a
    ///
    /// Step 1 and Step 2 are implemented as one pass.
    ///
    pub def queryStmt(stmt: RamStmt[v]): RamStmt[v] = match stmt {
        case RamStmt.Insert(op) =>
            let (innerOp, ground) = queryOp(op, Set#{});
            if (List.isEmpty(ground))
                RamStmt.Insert(innerOp)
            else
                RamStmt.Insert(RelOp.If(ground, innerOp))
        case RamStmt.Merge(_, _) => stmt
        case RamStmt.Assign(_, _) => stmt
        case RamStmt.Purge(_) => stmt
        case RamStmt.Seq(xs) => RamStmt.Seq(List.map(queryStmt, xs))
        case RamStmt.Until(test, body) => RamStmt.Until(test, queryStmt(body))
        case RamStmt.Comment(_) => stmt
    }

    /// Hoist and query optimize `op`.
    ///
    /// `freeVars` is the set of variables bound by an outer loop.
    /// Returns the optimized op and the conditions that occur in `op` that have to be hoisted.
    ///
    def queryOp(op: RelOp[v], freeVars: Set[RowVar]): (RelOp[v], List[BoolExp[v]]) = match op {
        case RelOp.Search(var, ramSym, body) =>
            use Fixpoint/Ram.BoolExp.Eq;
            use Fixpoint/Ram.RamTerm.{RowLoad, Lit};
            let (innerOp, innerGround) = queryOp(body, Set.insert(var, freeVars));
            let (ground, notGround) = List.partition(isExpGround(freeVars), innerGround);
            let (varQuery, rest1) =
                // Make sure `var` is on the lhs of all equalities.
                List.map(exp -> match exp {
                    case Eq(RowLoad(row1, i), RowLoad(row2, j)) =>
                        if (row2 == var)
                            Eq(RowLoad(row2, j), RowLoad(row1, i))
                        else
                            exp
                    case Eq(Lit(v), RowLoad(row, i)) => Eq(RowLoad(row, i), Lit(v))
                    case _ => exp
                }, notGround) |>
                // Partition into those equalities that have `var` on the lhs and those that don't.
                List.partition(exp -> match exp {
                    case Eq(RowLoad(row1, _), RowLoad(row2, _)) => row1 != row2 and row1 == var
                    case Eq(RowLoad(row, _), Lit(_)) => row == var
                    case _ => false
                });
            let (prefixQuery, rest2) =
                // Sort equalities of the form `var[i] = rhs` ascending on `i`.
                List.sortWith(x -> y -> match (x, y) {
                    case (Eq(RowLoad(_, index1), _), Eq(RowLoad(_, index2), _)) =>
                        index1 - index2
                }, varQuery) |>
                // Tag `var[i] = rhs` with its "expected" / prefix index.
                List.mapWithIndex(x -> i -> (x, i)) |>
                // Partition into those equalities that form a prefix on `var` and those that don't.
                List.partition(match (Eq(RowLoad(_, index), _rhs), i) -> index == i) |>
                // Finally project away the left-hand side and the index.
                match (query, rest) ->
                    (List.map(match (Eq(_, rhs), _) -> rhs, query), List.map(fst, rest));
            let test = rest1 ::: rest2;
            if (List.isEmpty(prefixQuery))
                if (List.isEmpty(test))
                    let search = RelOp.Search(var, ramSym, innerOp);
                    (search, ground)
                else
                    let search = RelOp.Search(var, ramSym, RelOp.If(test, innerOp));
                    (search, ground)
            else
                let query = List.toArray(prefixQuery) as & Pure;
                if (List.isEmpty(test))
                    let search = RelOp.Query(var, ramSym, query, innerOp);
                    (search, ground)
                else
                    let search = RelOp.Query(var, ramSym, query, RelOp.If(test, innerOp));
                    (search, ground)
        case RelOp.Query(_) => (op, Nil)
        case RelOp.Project(_) => (op, Nil)
        case RelOp.If(test, then) =>
            let (innerOp, innerGround) = queryOp(then, freeVars);
            (innerOp, test ::: innerGround)
    }

    ///
    /// An expression is ground if all its terms are ground.
    ///
    def isExpGround(freeVars: Set[RowVar], exp: BoolExp[v]): Bool = match exp {
        case BoolExp.Empty(_) => true
        case BoolExp.NotMemberOf(terms, _) => Array.forall(isTermGround(freeVars), terms) as & Pure
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
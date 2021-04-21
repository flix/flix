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

// TODO: rewrite docs. This is out of date.
/// The purpose of this phase is to:
/// 1) Hoist inner search conditions to outer search conditions.
/// 2) Rewrite searches on relations to queries on indices, when possible.
/// It is possible to rewrite a search when it searches on attributes that form a prefix
/// of the attribute sequence used to lexicographically define the index.
/// Consider the following example:
/// search x ∈ b do
///     search (y, z, w) ∈ r do
///         search u ∈ c where
///             x = y /\ x = w /\ z = u /\ x ∉ a
///         do
///             project x into a
/// After step 1:
/// search x ∈ b where x ∉ a
/// do
///     search (y, z, w) ∈ r where x = y /\ x = w
///     do
///         search u ∈ c where z = u
///         do
///             project x into a
/// After step 2:
/// search x ∈ b where x ∉ a
/// do
///     query (y, z, w) ∈ {(y, z, w) ∈ r | x = y} where x = w
///     do
///         query u ∈ {u ∈ c | z = u}
///         do
///             project x into a
namespace Fixpoint {
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

    def queryOp(op: RelOp[v], freeVars: Set[RowVar]): (RelOp[v], List[BoolExp[v]]) = match op {
        case RelOp.Search(var, ramSym, body) =>
            use Fixpoint/Ram.BoolExp.Eq;
            use Fixpoint/Ram.RamTerm.{RowLoad, Lit};
            let (innerOp, innerGround) = queryOp(body, Set.insert(var, freeVars));
            let (ground, notGround) = List.partition(isExpGround(freeVars), innerGround);
            let (attributeQuery, rest1) =
                List.partition(exp -> match exp {
                    case Eq(RowLoad(row1, _), RowLoad(row2, _)) =>
                        // TODO: consider that the order of attributes can have an impact
                        // on query optimization.
                        // E.g: a[1] = b[0] cannot be rewritten to a query on `a` but
                        // b[0] = a[1] *can* be rewritten to a query on `b`.
                        row1 != row2 and row1 == var
                    case Eq(RowLoad(row, _), Lit(_)) => row == var
                    case _ => false
                }, notGround);
            let (prefixQuery, rest2) =
                List.sortWith(x -> y -> match (x, y) {
                    case (Eq(RowLoad(_, index1), _), Eq(RowLoad(_, index2), _)) =>
                        index1 - index2
                }, attributeQuery) |>
                List.mapWithIndex(x -> i -> (x, i)) |>
                List.partition(match (Eq(RowLoad(_, index), _), i) -> index == i) |>
                // finally project away the left-hand side and the index.
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
        case RelOp.If(test, body) =>
            let (innerOp, innerGround) = queryOp(body, freeVars);
            (innerOp, test ::: innerGround)
    }

    /// An expression is ground if all its terms are ground.
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
        case BoolExp.Guard6(_, t1, t2, t3, t4, t5, t6) =>
            isTermGround(freeVars, t1) and
            isTermGround(freeVars, t2) and
            isTermGround(freeVars, t3) and
            isTermGround(freeVars, t4) and
            isTermGround(freeVars, t5) and
            isTermGround(freeVars, t6)
        case BoolExp.Guard7(_, t1, t2, t3, t4, t5, t6, t7) =>
            isTermGround(freeVars, t1) and
            isTermGround(freeVars, t2) and
            isTermGround(freeVars, t3) and
            isTermGround(freeVars, t4) and
            isTermGround(freeVars, t5) and
            isTermGround(freeVars, t6) and
            isTermGround(freeVars, t7)
        case BoolExp.Guard8(_, t1, t2, t3, t4, t5, t6, t7, t8) =>
            isTermGround(freeVars, t1) and
            isTermGround(freeVars, t2) and
            isTermGround(freeVars, t3) and
            isTermGround(freeVars, t4) and
            isTermGround(freeVars, t5) and
            isTermGround(freeVars, t6) and
            isTermGround(freeVars, t7) and
            isTermGround(freeVars, t8)
        case BoolExp.Guard9(_, t1, t2, t3, t4, t5, t6, t7, t8, t9) =>
            isTermGround(freeVars, t1) and
            isTermGround(freeVars, t2) and
            isTermGround(freeVars, t3) and
            isTermGround(freeVars, t4) and
            isTermGround(freeVars, t5) and
            isTermGround(freeVars, t6) and
            isTermGround(freeVars, t7) and
            isTermGround(freeVars, t8) and
            isTermGround(freeVars, t9)
        case BoolExp.Guard10(_, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10) =>
            isTermGround(freeVars, t1) and
            isTermGround(freeVars, t2) and
            isTermGround(freeVars, t3) and
            isTermGround(freeVars, t4) and
            isTermGround(freeVars, t5) and
            isTermGround(freeVars, t6) and
            isTermGround(freeVars, t7) and
            isTermGround(freeVars, t8) and
            isTermGround(freeVars, t9) and
            isTermGround(freeVars, t10)
        case BoolExp.Guard11(_, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11) =>
            isTermGround(freeVars, t1) and
            isTermGround(freeVars, t2) and
            isTermGround(freeVars, t3) and
            isTermGround(freeVars, t4) and
            isTermGround(freeVars, t5) and
            isTermGround(freeVars, t6) and
            isTermGround(freeVars, t7) and
            isTermGround(freeVars, t8) and
            isTermGround(freeVars, t9) and
            isTermGround(freeVars, t10) and
            isTermGround(freeVars, t11)
        case BoolExp.Guard12(_, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12) =>
            isTermGround(freeVars, t1) and
            isTermGround(freeVars, t2) and
            isTermGround(freeVars, t3) and
            isTermGround(freeVars, t4) and
            isTermGround(freeVars, t5) and
            isTermGround(freeVars, t6) and
            isTermGround(freeVars, t7) and
            isTermGround(freeVars, t8) and
            isTermGround(freeVars, t9) and
            isTermGround(freeVars, t10) and
            isTermGround(freeVars, t11) and
            isTermGround(freeVars, t12)
        case BoolExp.Guard13(_, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13) =>
            isTermGround(freeVars, t1) and
            isTermGround(freeVars, t2) and
            isTermGround(freeVars, t3) and
            isTermGround(freeVars, t4) and
            isTermGround(freeVars, t5) and
            isTermGround(freeVars, t6) and
            isTermGround(freeVars, t7) and
            isTermGround(freeVars, t8) and
            isTermGround(freeVars, t9) and
            isTermGround(freeVars, t10) and
            isTermGround(freeVars, t11) and
            isTermGround(freeVars, t12) and
            isTermGround(freeVars, t13)
        case BoolExp.Guard14(_, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14) =>
            isTermGround(freeVars, t1) and
            isTermGround(freeVars, t2) and
            isTermGround(freeVars, t3) and
            isTermGround(freeVars, t4) and
            isTermGround(freeVars, t5) and
            isTermGround(freeVars, t6) and
            isTermGround(freeVars, t7) and
            isTermGround(freeVars, t8) and
            isTermGround(freeVars, t9) and
            isTermGround(freeVars, t10) and
            isTermGround(freeVars, t11) and
            isTermGround(freeVars, t12) and
            isTermGround(freeVars, t13) and
            isTermGround(freeVars, t14)
        case BoolExp.Guard15(_, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15) =>
            isTermGround(freeVars, t1) and
            isTermGround(freeVars, t2) and
            isTermGround(freeVars, t3) and
            isTermGround(freeVars, t4) and
            isTermGround(freeVars, t5) and
            isTermGround(freeVars, t6) and
            isTermGround(freeVars, t7) and
            isTermGround(freeVars, t8) and
            isTermGround(freeVars, t9) and
            isTermGround(freeVars, t10) and
            isTermGround(freeVars, t11) and
            isTermGround(freeVars, t12) and
            isTermGround(freeVars, t13) and
            isTermGround(freeVars, t14) and
            isTermGround(freeVars, t15)
    }

    /// A term is ground if it is a literal or a free variable.
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
        case RamTerm.App6(_, t1, t2, t3, t4, t5, t6) =>
            isTermGround(freeVars, t1) and
            isTermGround(freeVars, t2) and
            isTermGround(freeVars, t3) and
            isTermGround(freeVars, t4) and
            isTermGround(freeVars, t5) and
            isTermGround(freeVars, t6)
        case RamTerm.App7(_, t1, t2, t3, t4, t5, t6, t7) =>
            isTermGround(freeVars, t1) and
            isTermGround(freeVars, t2) and
            isTermGround(freeVars, t3) and
            isTermGround(freeVars, t4) and
            isTermGround(freeVars, t5) and
            isTermGround(freeVars, t6) and
            isTermGround(freeVars, t7)
        case RamTerm.App8(_, t1, t2, t3, t4, t5, t6, t7, t8) =>
            isTermGround(freeVars, t1) and
            isTermGround(freeVars, t2) and
            isTermGround(freeVars, t3) and
            isTermGround(freeVars, t4) and
            isTermGround(freeVars, t5) and
            isTermGround(freeVars, t6) and
            isTermGround(freeVars, t7) and
            isTermGround(freeVars, t8)
        case RamTerm.App9(_, t1, t2, t3, t4, t5, t6, t7, t8, t9) =>
            isTermGround(freeVars, t1) and
            isTermGround(freeVars, t2) and
            isTermGround(freeVars, t3) and
            isTermGround(freeVars, t4) and
            isTermGround(freeVars, t5) and
            isTermGround(freeVars, t6) and
            isTermGround(freeVars, t7) and
            isTermGround(freeVars, t8) and
            isTermGround(freeVars, t9)
        case RamTerm.App10(_, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10) =>
            isTermGround(freeVars, t1) and
            isTermGround(freeVars, t2) and
            isTermGround(freeVars, t3) and
            isTermGround(freeVars, t4) and
            isTermGround(freeVars, t5) and
            isTermGround(freeVars, t6) and
            isTermGround(freeVars, t7) and
            isTermGround(freeVars, t8) and
            isTermGround(freeVars, t9) and
            isTermGround(freeVars, t10)
        case RamTerm.App11(_, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11) =>
            isTermGround(freeVars, t1) and
            isTermGround(freeVars, t2) and
            isTermGround(freeVars, t3) and
            isTermGround(freeVars, t4) and
            isTermGround(freeVars, t5) and
            isTermGround(freeVars, t6) and
            isTermGround(freeVars, t7) and
            isTermGround(freeVars, t8) and
            isTermGround(freeVars, t9) and
            isTermGround(freeVars, t10) and
            isTermGround(freeVars, t11)
        case RamTerm.App12(_, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12) =>
            isTermGround(freeVars, t1) and
            isTermGround(freeVars, t2) and
            isTermGround(freeVars, t3) and
            isTermGround(freeVars, t4) and
            isTermGround(freeVars, t5) and
            isTermGround(freeVars, t6) and
            isTermGround(freeVars, t7) and
            isTermGround(freeVars, t8) and
            isTermGround(freeVars, t9) and
            isTermGround(freeVars, t10) and
            isTermGround(freeVars, t11) and
            isTermGround(freeVars, t12)
        case RamTerm.App13(_, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13) =>
            isTermGround(freeVars, t1) and
            isTermGround(freeVars, t2) and
            isTermGround(freeVars, t3) and
            isTermGround(freeVars, t4) and
            isTermGround(freeVars, t5) and
            isTermGround(freeVars, t6) and
            isTermGround(freeVars, t7) and
            isTermGround(freeVars, t8) and
            isTermGround(freeVars, t9) and
            isTermGround(freeVars, t10) and
            isTermGround(freeVars, t11) and
            isTermGround(freeVars, t12) and
            isTermGround(freeVars, t13)
        case RamTerm.App14(_, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14) =>
            isTermGround(freeVars, t1) and
            isTermGround(freeVars, t2) and
            isTermGround(freeVars, t3) and
            isTermGround(freeVars, t4) and
            isTermGround(freeVars, t5) and
            isTermGround(freeVars, t6) and
            isTermGround(freeVars, t7) and
            isTermGround(freeVars, t8) and
            isTermGround(freeVars, t9) and
            isTermGround(freeVars, t10) and
            isTermGround(freeVars, t11) and
            isTermGround(freeVars, t12) and
            isTermGround(freeVars, t13) and
            isTermGround(freeVars, t14)
        case RamTerm.App15(_, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15) =>
            isTermGround(freeVars, t1) and
            isTermGround(freeVars, t2) and
            isTermGround(freeVars, t3) and
            isTermGround(freeVars, t4) and
            isTermGround(freeVars, t5) and
            isTermGround(freeVars, t6) and
            isTermGround(freeVars, t7) and
            isTermGround(freeVars, t8) and
            isTermGround(freeVars, t9) and
            isTermGround(freeVars, t10) and
            isTermGround(freeVars, t11) and
            isTermGround(freeVars, t12) and
            isTermGround(freeVars, t13) and
            isTermGround(freeVars, t14) and
            isTermGround(freeVars, t15)
    }
}
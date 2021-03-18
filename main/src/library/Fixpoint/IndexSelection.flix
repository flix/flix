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

use Fixpoint/Ram.{RamStmt, RelOp, RelSym, RamTerm, BoolExp, RowVar};

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
    pub def hoistStmt(stmt: RamStmt[v]): RamStmt[v] = match stmt {
        case RamStmt.Insert(op) => hoistOp(op, Set#{}) |> fst |> RamStmt.Insert
        case RamStmt.Merge(_, _) => stmt
        case RamStmt.Assign(_, _) => stmt
        case RamStmt.Purge(_) => stmt
        case RamStmt.Seq(xs) => RamStmt.Seq(List.map(hoistStmt, xs))
        case RamStmt.Until(test, body) => RamStmt.Until(test, hoistStmt(body))
        case RamStmt.Comment(_) => stmt
    }

    def hoistOp(op: RelOp[v], freeVars: Set[RowVar]): (RelOp[v], List[BoolExp[v]]) = match op {
        case RelOp.Search(var, relSym, where, body) =>
            let (innerOp, innerGround) = hoistOp(body, Set.insert(var, freeVars));
            let (ground, rest) = List.partition(isExpGround(freeVars), where ::: innerGround);
            let search = RelOp.Search(var, relSym, rest, innerOp);
            (search, ground)
        case RelOp.Query(_, _, _, _, _) => (op, Nil)
        case RelOp.Project(_, _) => (op, Nil)
    }

    pub def queryStmt[v : ToString](stmt: RamStmt[v]): RamStmt[v] = match stmt {
        case RamStmt.Insert(op) => RamStmt.Insert(queryOp(op))
        case RamStmt.Merge(_, _) => stmt
        case RamStmt.Assign(_, _) => stmt
        case RamStmt.Purge(_) => stmt
        case RamStmt.Seq(xs) => RamStmt.Seq(List.map(queryStmt, xs))
        case RamStmt.Until(test, body) => RamStmt.Until(test, queryStmt(body))
        case RamStmt.Comment(_) => stmt
    }

    def queryOp[v : ToString](op: RelOp[v]): RelOp[v] = match op {
        case RelOp.Search(var, relSym, where, body) =>
            use Fixpoint/Ram.BoolExp.Eq;
            use Fixpoint/Ram.RamTerm.{Attribute, Lit};
            let (attributeQuery, rest1) =
                List.partition(exp -> match exp {
                    case Eq(Attribute(row1, _), Attribute(row2, _)) =>
                        row1 != row2 and row1 == var
                    case Eq(Attribute(row, _), Lit(_)) => row == var
                    case _ => false
                }, where);
            let (prefixQuery, rest2) =
                List.sortWith(x -> y -> match (x, y) {
                    case (Eq(Attribute(_, index1), _), Eq(Attribute(_, index2), _)) =>
                        index1 - index2
                }, attributeQuery) |>
                List.mapWithIndex(x -> i -> (x, i)) |>
                List.partition(match (Eq(Attribute(_, index), _), i) -> index == i) |>
                match (prefixQuery, rest) ->
                    (List.map(match (Eq(_, rhs), _) -> rhs, prefixQuery), List.map(fst, rest));
            if (List.isEmpty(prefixQuery))
                RelOp.Search(var, relSym, where, queryOp(body))
            else
                let query = List.toArray(prefixQuery) as & Pure;
                RelOp.Query(var, relSym, query, rest1 ::: rest2, queryOp(body))
        case RelOp.Query(_, _, _, _, _) => op
        case RelOp.Project(_, _) => op
    }

    /// An expression is ground if all its terms are ground.
    def isExpGround(freeVars: Set[RowVar], exp: BoolExp[v]): Bool = match exp {
        case BoolExp.Empty(_) => true
        case BoolExp.NotMemberOf(terms, _) => Array.forall(isTermGround(freeVars), terms) as & Pure
        case BoolExp.Guard(_, terms) => Array.forall(isTermGround(freeVars), terms) as & Pure
        case BoolExp.Eq(lhs, rhs) => isTermGround(freeVars, lhs) and isTermGround(freeVars, rhs)
    }

    /// A term is ground if it is a literal or a free variable.
    def isTermGround(freeVars: Set[RowVar], term: RamTerm[v]): Bool = match term {
        case RamTerm.Lit(_) => true
        case RamTerm.Attribute(var, _) => Set.memberOf(var, freeVars)
        case RamTerm.App(_, args) => Array.forall(isTermGround(freeVars), args) as & Pure
    }
}
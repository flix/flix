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

/// The purpose of this phase is to lower row variables in searches and queries to array indices.
/// This implies substituting named references to row variables with indices
/// in where clauses, range queries and project operations.
namespace Fixpoint {
    pub def lowerStmt(stmt: RamStmt[v]): RamStmt[v] = match stmt {
        case RamStmt.Insert(op) => RamStmt.Insert(lowerOp(op, Map#{}, 0))
        case RamStmt.Merge(_, _) => stmt
        case RamStmt.Assign(_, _) => stmt
        case RamStmt.Purge(_) => stmt
        case RamStmt.Seq(xs) => RamStmt.Seq(List.map(lowerStmt, xs))
        case RamStmt.Until(test, body) => RamStmt.Until(test, lowerStmt(body))
        case RamStmt.Comment(_) => stmt
    }

    def lowerOp(op: RelOp[v], rowVars: Map[RowVar, RowVar], depth: Int32): RelOp[v] =
        match op {
            case RelOp.Search(var, relSym, where, body) =>
                let newVar = RowVar.Index(depth);
                let newVars = Map.insert(var, newVar, rowVars);
                let newWhere = List.map(lowerExp(newVars), where);
                RelOp.Search(newVar, relSym, newWhere, lowerOp(body, newVars, depth + 1))
            case RelOp.Query(var, relSym, prefixQuery, where, body) =>
                let newVar = RowVar.Index(depth);
                let newVars = Map.insert(var, newVar, rowVars);
                let newQuery = Array.map(lowerTerm(rowVars), prefixQuery) as & Pure;
                let newWhere = List.map(lowerExp(newVars), where);
                RelOp.Query(newVar, relSym, newQuery, newWhere, lowerOp(body, newVars, depth + 1))
            case RelOp.Project(terms, relSym) =>
                 RelOp.Project(Array.map(lowerTerm(rowVars), terms), relSym) as & Pure
        }

    def lowerExp(rowVars: Map[RowVar, RowVar], exp: BoolExp[v]): BoolExp[v] =
        match exp {
            case BoolExp.Empty(_) => exp
            case BoolExp.NotMemberOf(terms, relSym) =>
                 BoolExp.NotMemberOf(Array.map(lowerTerm(rowVars), terms), relSym) as & Pure
            case BoolExp.Guard(f, terms) =>
                 BoolExp.Guard(f, Array.map(lowerTerm(rowVars), terms)) as & Pure
            case BoolExp.Eq(lhs, rhs) =>
                 BoolExp.Eq(lowerTerm(rowVars, lhs), lowerTerm(rowVars, rhs))
        }

    def lowerTerm(rowVars: Map[RowVar, RowVar], term: RamTerm[v]): RamTerm[v] = match term {
        case RamTerm.Lit(_) => term
        case RamTerm.Attribute(rowVar, index) =>
             RamTerm.Attribute(Map.getWithDefault(rowVar, rowVar, rowVars), index)
        case RamTerm.App(f, terms) =>
             RamTerm.App(f, Array.map(lowerTerm(rowVars), terms)) as & Pure
    }
}
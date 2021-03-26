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
            case BoolExp.Eq(lhs, rhs) =>
                 BoolExp.Eq(lowerTerm(rowVars, lhs), lowerTerm(rowVars, rhs))
            case BoolExp.Guard0(_) => exp
            case BoolExp.Guard1(f, v) =>
                let t = lowerTerm(rowVars, v);
                BoolExp.Guard1(f, t)
            case BoolExp.Guard2(f, v1, v2) =>
                let t1 = lowerTerm(rowVars, v1);
                let t2 = lowerTerm(rowVars, v2);
                BoolExp.Guard2(f, t1, t2)
            case BoolExp.Guard3(f, v1, v2, v3) =>
                let t1 = lowerTerm(rowVars, v1);
                let t2 = lowerTerm(rowVars, v2);
                let t3 = lowerTerm(rowVars, v3);
                BoolExp.Guard3(f, t1, t2, t3)
            case BoolExp.Guard4(f, v1, v2, v3, v4) =>
                let t1 = lowerTerm(rowVars, v1);
                let t2 = lowerTerm(rowVars, v2);
                let t3 = lowerTerm(rowVars, v3);
                let t4 = lowerTerm(rowVars, v4);
                BoolExp.Guard4(f, t1, t2, t3, t4)
            case BoolExp.Guard5(f, v1, v2, v3, v4, v5) =>
                let t1 = lowerTerm(rowVars, v1);
                let t2 = lowerTerm(rowVars, v2);
                let t3 = lowerTerm(rowVars, v3);
                let t4 = lowerTerm(rowVars, v4);
                let t5 = lowerTerm(rowVars, v5);
                BoolExp.Guard5(f, t1, t2, t3, t4, t5)
        }

    def lowerTerm(rowVars: Map[RowVar, RowVar], term: RamTerm[v]): RamTerm[v] = match term {
        case RamTerm.Lit(_) => term
        case RamTerm.Attribute(rowVar, index) => RamTerm.Attribute(Map.getWithDefault(rowVar, rowVar, rowVars), index)
        case RamTerm.App0(_) => term
        case RamTerm.App1(f, v) =>
            let t = lowerTerm(rowVars, v);
            RamTerm.App1(f, t)
        case RamTerm.App2(f, v1, v2) =>
            let t1 = lowerTerm(rowVars, v1);
            let t2 = lowerTerm(rowVars, v2);
            RamTerm.App2(f, t1, t2)
        case RamTerm.App3(f, v1, v2, v3) =>
            let t1 = lowerTerm(rowVars, v1);
            let t2 = lowerTerm(rowVars, v2);
            let t3 = lowerTerm(rowVars, v3);
            RamTerm.App3(f, t1, t2, t3)
        case RamTerm.App4(f, v1, v2, v3, v4) =>
            let t1 = lowerTerm(rowVars, v1);
            let t2 = lowerTerm(rowVars, v2);
            let t3 = lowerTerm(rowVars, v3);
            let t4 = lowerTerm(rowVars, v4);
            RamTerm.App4(f, t1, t2, t3, t4)
        case RamTerm.App5(f, v1, v2, v3, v4, v5) =>
            let t1 = lowerTerm(rowVars, v1);
            let t2 = lowerTerm(rowVars, v2);
            let t3 = lowerTerm(rowVars, v3);
            let t4 = lowerTerm(rowVars, v4);
            let t5 = lowerTerm(rowVars, v5);
            RamTerm.App5(f, t1, t2, t3, t4, t5)
    }
}
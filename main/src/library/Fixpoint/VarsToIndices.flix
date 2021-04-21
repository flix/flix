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
            case RelOp.Search(var, ramSym, body) =>
                let newVar = RowVar.Index(depth);
                let newVars = Map.insert(var, newVar, rowVars);
                RelOp.Search(newVar, ramSym, lowerOp(body, newVars, depth + 1))
            case RelOp.Query(var, ramSym, prefixQuery, body) =>
                let newVar = RowVar.Index(depth);
                let newVars = Map.insert(var, newVar, rowVars);
                let newQuery = Array.map(lowerTerm(rowVars), prefixQuery) as & Pure;
                RelOp.Query(newVar, ramSym, newQuery, lowerOp(body, newVars, depth + 1))
            case RelOp.Project(terms, ramSym) =>
                 RelOp.Project(Array.map(lowerTerm(rowVars), terms), ramSym) as & Pure
            case RelOp.If(test, then) =>
                 RelOp.If(List.map(lowerExp(rowVars), test), lowerOp(then, rowVars, depth))
        }

    def lowerExp(rowVars: Map[RowVar, RowVar], exp: BoolExp[v]): BoolExp[v] =
        match exp {
            case BoolExp.Empty(_) => exp
            case BoolExp.NotMemberOf(terms, ramSym) =>
                 BoolExp.NotMemberOf(Array.map(lowerTerm(rowVars), terms), ramSym) as & Pure
            case BoolExp.Eq(lhs, rhs) =>
                 BoolExp.Eq(lowerTerm(rowVars, lhs), lowerTerm(rowVars, rhs))
            case BoolExp.Leq(f, lhs, rhs) =>
                 BoolExp.Leq(f, lowerTerm(rowVars, lhs), lowerTerm(rowVars, rhs))
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
            case BoolExp.Guard6(f, v1, v2, v3, v4, v5, v6) =>
                let t1 = lowerTerm(rowVars, v1);
                let t2 = lowerTerm(rowVars, v2);
                let t3 = lowerTerm(rowVars, v3);
                let t4 = lowerTerm(rowVars, v4);
                let t5 = lowerTerm(rowVars, v5);
                let t6 = lowerTerm(rowVars, v6);
                BoolExp.Guard6(f, t1, t2, t3, t4, t5, t6)                
            case BoolExp.Guard7(f, v1, v2, v3, v4, v5, v6, v7) =>
                let t1 = lowerTerm(rowVars, v1);
                let t2 = lowerTerm(rowVars, v2);
                let t3 = lowerTerm(rowVars, v3);
                let t4 = lowerTerm(rowVars, v4);
                let t5 = lowerTerm(rowVars, v5);
                let t6 = lowerTerm(rowVars, v6);
                let t7 = lowerTerm(rowVars, v7);
                BoolExp.Guard7(f, t1, t2, t3, t4, t5, t6, t7)
            case BoolExp.Guard8(f, v1, v2, v3, v4, v5, v6, v7, v8) =>
                let t1 = lowerTerm(rowVars, v1);
                let t2 = lowerTerm(rowVars, v2);
                let t3 = lowerTerm(rowVars, v3);
                let t4 = lowerTerm(rowVars, v4);
                let t5 = lowerTerm(rowVars, v5);
                let t6 = lowerTerm(rowVars, v6);
                let t7 = lowerTerm(rowVars, v7);
                let t8 = lowerTerm(rowVars, v8);
                BoolExp.Guard8(f, t1, t2, t3, t4, t5, t6, t7, t8)
            case BoolExp.Guard9(f, v1, v2, v3, v4, v5, v6, v7, v8, v9) =>
                let t1 = lowerTerm(rowVars, v1);
                let t2 = lowerTerm(rowVars, v2);
                let t3 = lowerTerm(rowVars, v3);
                let t4 = lowerTerm(rowVars, v4);
                let t5 = lowerTerm(rowVars, v5);
                let t6 = lowerTerm(rowVars, v6);
                let t7 = lowerTerm(rowVars, v7);
                let t8 = lowerTerm(rowVars, v8);
                let t9 = lowerTerm(rowVars, v9);
                BoolExp.Guard9(f, t1, t2, t3, t4, t5, t6, t7, t8, t9)
            case BoolExp.Guard10(f, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10) =>
                let t1 = lowerTerm(rowVars, v1);
                let t2 = lowerTerm(rowVars, v2);
                let t3 = lowerTerm(rowVars, v3);
                let t4 = lowerTerm(rowVars, v4);
                let t5 = lowerTerm(rowVars, v5);
                let t6 = lowerTerm(rowVars, v6);
                let t7 = lowerTerm(rowVars, v7);
                let t8 = lowerTerm(rowVars, v8);
                let t9 = lowerTerm(rowVars, v9);
                let t10 = lowerTerm(rowVars, v10);
                BoolExp.Guard10(f, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10)
            case BoolExp.Guard11(f, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11) =>
                let t1 = lowerTerm(rowVars, v1);
                let t2 = lowerTerm(rowVars, v2);
                let t3 = lowerTerm(rowVars, v3);
                let t4 = lowerTerm(rowVars, v4);
                let t5 = lowerTerm(rowVars, v5);
                let t6 = lowerTerm(rowVars, v6);
                let t7 = lowerTerm(rowVars, v7);
                let t8 = lowerTerm(rowVars, v8);
                let t9 = lowerTerm(rowVars, v9);
                let t10 = lowerTerm(rowVars, v10);
                let t11 = lowerTerm(rowVars, v11);
                BoolExp.Guard11(f, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11)
            case BoolExp.Guard12(f, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12) =>
                let t1 = lowerTerm(rowVars, v1);
                let t2 = lowerTerm(rowVars, v2);
                let t3 = lowerTerm(rowVars, v3);
                let t4 = lowerTerm(rowVars, v4);
                let t5 = lowerTerm(rowVars, v5);
                let t6 = lowerTerm(rowVars, v6);
                let t7 = lowerTerm(rowVars, v7);
                let t8 = lowerTerm(rowVars, v8);
                let t9 = lowerTerm(rowVars, v9);
                let t10 = lowerTerm(rowVars, v10);
                let t11 = lowerTerm(rowVars, v11);
                let t12 = lowerTerm(rowVars, v12);
                BoolExp.Guard12(f, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12)
            case BoolExp.Guard13(f, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13) =>
                let t1 = lowerTerm(rowVars, v1);
                let t2 = lowerTerm(rowVars, v2);
                let t3 = lowerTerm(rowVars, v3);
                let t4 = lowerTerm(rowVars, v4);
                let t5 = lowerTerm(rowVars, v5);
                let t6 = lowerTerm(rowVars, v6);
                let t7 = lowerTerm(rowVars, v7);
                let t8 = lowerTerm(rowVars, v8);
                let t9 = lowerTerm(rowVars, v9);
                let t10 = lowerTerm(rowVars, v10);
                let t11 = lowerTerm(rowVars, v11);
                let t12 = lowerTerm(rowVars, v12);
                let t13 = lowerTerm(rowVars, v13);
                BoolExp.Guard13(f, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13)
            case BoolExp.Guard14(f, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14) =>
                let t1 = lowerTerm(rowVars, v1);
                let t2 = lowerTerm(rowVars, v2);
                let t3 = lowerTerm(rowVars, v3);
                let t4 = lowerTerm(rowVars, v4);
                let t5 = lowerTerm(rowVars, v5);
                let t6 = lowerTerm(rowVars, v6);
                let t7 = lowerTerm(rowVars, v7);
                let t8 = lowerTerm(rowVars, v8);
                let t9 = lowerTerm(rowVars, v9);
                let t10 = lowerTerm(rowVars, v10);
                let t11 = lowerTerm(rowVars, v11);
                let t12 = lowerTerm(rowVars, v12);
                let t13 = lowerTerm(rowVars, v13);
                let t14 = lowerTerm(rowVars, v14);
                BoolExp.Guard14(f, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14)
            case BoolExp.Guard15(f, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15) =>
                let t1 = lowerTerm(rowVars, v1);
                let t2 = lowerTerm(rowVars, v2);
                let t3 = lowerTerm(rowVars, v3);
                let t4 = lowerTerm(rowVars, v4);
                let t5 = lowerTerm(rowVars, v5);
                let t6 = lowerTerm(rowVars, v6);
                let t7 = lowerTerm(rowVars, v7);
                let t8 = lowerTerm(rowVars, v8);
                let t9 = lowerTerm(rowVars, v9);
                let t10 = lowerTerm(rowVars, v10);
                let t11 = lowerTerm(rowVars, v11);
                let t12 = lowerTerm(rowVars, v12);
                let t13 = lowerTerm(rowVars, v13);
                let t14 = lowerTerm(rowVars, v14);
                let t15 = lowerTerm(rowVars, v15);
                BoolExp.Guard15(f, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15)
        }

    def lowerTerm(rowVars: Map[RowVar, RowVar], term: RamTerm[v]): RamTerm[v] = match term {
        case RamTerm.Lit(_) => term
        case RamTerm.RowLoad(rowVar, index) =>
             RamTerm.RowLoad(Map.getWithDefault(rowVar, rowVar, rowVars), index)
        case RamTerm.LoadLatVar(rowVar) =>
             RamTerm.LoadLatVar(Map.getWithDefault(rowVar, rowVar, rowVars))
        case RamTerm.Meet(f, v1, v2) =>
            let t1 = lowerTerm(rowVars, v1);
            let t2 = lowerTerm(rowVars, v2);
            RamTerm.Meet(f, t1, t2)
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
        case RamTerm.App6(f, v1, v2, v3, v4, v5, v6) =>
            let t1 = lowerTerm(rowVars, v1);
            let t2 = lowerTerm(rowVars, v2);
            let t3 = lowerTerm(rowVars, v3);
            let t4 = lowerTerm(rowVars, v4);
            let t5 = lowerTerm(rowVars, v5);
            let t6 = lowerTerm(rowVars, v6);
            RamTerm.App6(f, t1, t2, t3, t4, t5, t6)
        case RamTerm.App7(f, v1, v2, v3, v4, v5, v6, v7) =>
            let t1 = lowerTerm(rowVars, v1);
            let t2 = lowerTerm(rowVars, v2);
            let t3 = lowerTerm(rowVars, v3);
            let t4 = lowerTerm(rowVars, v4);
            let t5 = lowerTerm(rowVars, v5);
            let t6 = lowerTerm(rowVars, v6);
            let t7 = lowerTerm(rowVars, v7);
            RamTerm.App7(f, t1, t2, t3, t4, t5, t6, t7)
        case RamTerm.App8(f, v1, v2, v3, v4, v5, v6, v7, v8) =>
            let t1 = lowerTerm(rowVars, v1);
            let t2 = lowerTerm(rowVars, v2);
            let t3 = lowerTerm(rowVars, v3);
            let t4 = lowerTerm(rowVars, v4);
            let t5 = lowerTerm(rowVars, v5);
            let t6 = lowerTerm(rowVars, v6);
            let t7 = lowerTerm(rowVars, v7);
            let t8 = lowerTerm(rowVars, v8);
            RamTerm.App8(f, t1, t2, t3, t4, t5, t6, t7, t8)
        case RamTerm.App9(f, v1, v2, v3, v4, v5, v6, v7, v8, v9) =>
            let t1 = lowerTerm(rowVars, v1);
            let t2 = lowerTerm(rowVars, v2);
            let t3 = lowerTerm(rowVars, v3);
            let t4 = lowerTerm(rowVars, v4);
            let t5 = lowerTerm(rowVars, v5);
            let t6 = lowerTerm(rowVars, v6);
            let t7 = lowerTerm(rowVars, v7);
            let t8 = lowerTerm(rowVars, v8);
            let t9 = lowerTerm(rowVars, v9);
            RamTerm.App9(f, t1, t2, t3, t4, t5, t6, t7, t8, t9)
        case RamTerm.App10(f, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10) =>
            let t1 = lowerTerm(rowVars, v1);
            let t2 = lowerTerm(rowVars, v2);
            let t3 = lowerTerm(rowVars, v3);
            let t4 = lowerTerm(rowVars, v4);
            let t5 = lowerTerm(rowVars, v5);
            let t6 = lowerTerm(rowVars, v6);
            let t7 = lowerTerm(rowVars, v7);
            let t8 = lowerTerm(rowVars, v8);
            let t9 = lowerTerm(rowVars, v9);
            let t10 = lowerTerm(rowVars, v10);
            RamTerm.App10(f, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10)
        case RamTerm.App11(f, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11) =>
            let t1 = lowerTerm(rowVars, v1);
            let t2 = lowerTerm(rowVars, v2);
            let t3 = lowerTerm(rowVars, v3);
            let t4 = lowerTerm(rowVars, v4);
            let t5 = lowerTerm(rowVars, v5);
            let t6 = lowerTerm(rowVars, v6);
            let t7 = lowerTerm(rowVars, v7);
            let t8 = lowerTerm(rowVars, v8);
            let t9 = lowerTerm(rowVars, v9);
            let t10 = lowerTerm(rowVars, v10);
            let t11 = lowerTerm(rowVars, v11);
            RamTerm.App11(f, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11)
        case RamTerm.App12(f, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12) =>
            let t1 = lowerTerm(rowVars, v1);
            let t2 = lowerTerm(rowVars, v2);
            let t3 = lowerTerm(rowVars, v3);
            let t4 = lowerTerm(rowVars, v4);
            let t5 = lowerTerm(rowVars, v5);
            let t6 = lowerTerm(rowVars, v6);
            let t7 = lowerTerm(rowVars, v7);
            let t8 = lowerTerm(rowVars, v8);
            let t9 = lowerTerm(rowVars, v9);
            let t10 = lowerTerm(rowVars, v10);
            let t11 = lowerTerm(rowVars, v11);
            let t12 = lowerTerm(rowVars, v12);
            RamTerm.App12(f, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12)
        case RamTerm.App13(f, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13) =>
            let t1 = lowerTerm(rowVars, v1);
            let t2 = lowerTerm(rowVars, v2);
            let t3 = lowerTerm(rowVars, v3);
            let t4 = lowerTerm(rowVars, v4);
            let t5 = lowerTerm(rowVars, v5);
            let t6 = lowerTerm(rowVars, v6);
            let t7 = lowerTerm(rowVars, v7);
            let t8 = lowerTerm(rowVars, v8);
            let t9 = lowerTerm(rowVars, v9);
            let t10 = lowerTerm(rowVars, v10);
            let t11 = lowerTerm(rowVars, v11);
            let t12 = lowerTerm(rowVars, v12);
            let t13 = lowerTerm(rowVars, v13);
            RamTerm.App13(f, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13)
        case RamTerm.App14(f, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14) =>
            let t1 = lowerTerm(rowVars, v1);
            let t2 = lowerTerm(rowVars, v2);
            let t3 = lowerTerm(rowVars, v3);
            let t4 = lowerTerm(rowVars, v4);
            let t5 = lowerTerm(rowVars, v5);
            let t6 = lowerTerm(rowVars, v6);
            let t7 = lowerTerm(rowVars, v7);
            let t8 = lowerTerm(rowVars, v8);
            let t9 = lowerTerm(rowVars, v9);
            let t10 = lowerTerm(rowVars, v10);
            let t11 = lowerTerm(rowVars, v11);
            let t12 = lowerTerm(rowVars, v12);
            let t13 = lowerTerm(rowVars, v13);
            let t14 = lowerTerm(rowVars, v14);
            RamTerm.App14(f, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14)
        case RamTerm.App15(f, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15) =>
            let t1 = lowerTerm(rowVars, v1);
            let t2 = lowerTerm(rowVars, v2);
            let t3 = lowerTerm(rowVars, v3);
            let t4 = lowerTerm(rowVars, v4);
            let t5 = lowerTerm(rowVars, v5);
            let t6 = lowerTerm(rowVars, v6);
            let t7 = lowerTerm(rowVars, v7);
            let t8 = lowerTerm(rowVars, v8);
            let t9 = lowerTerm(rowVars, v9);
            let t10 = lowerTerm(rowVars, v10);
            let t11 = lowerTerm(rowVars, v11);
            let t12 = lowerTerm(rowVars, v12);
            let t13 = lowerTerm(rowVars, v13);
            let t14 = lowerTerm(rowVars, v14);
            let t15 = lowerTerm(rowVars, v15);
            RamTerm.App15(f, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15)
    }
}
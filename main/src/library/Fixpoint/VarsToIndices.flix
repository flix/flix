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

/// The purpose of this phase is to lower row variables in searches and queries to array indices.
/// This implies substituting named references to row variables with indices
/// in where clauses, range queries and project operations.
mod Fixpoint {

    use Fixpoint.Ast.Ram.{RamStmt, RelOp, RamSym, RamTerm, BoolExp, RowVar}

    @Internal
    pub def lowerStmt(stmt: RamStmt[v]): RamStmt[v] = match stmt {
        case RamStmt.Insert(op) => RamStmt.Insert(lowerOp(op, Map#{}, 0))
        case RamStmt.Merge(_, _) => stmt
        case RamStmt.Assign(_, _) => stmt
        case RamStmt.Purge(_) => stmt
        case RamStmt.Seq(xs) => RamStmt.Seq(Vector.map(lowerStmt, xs))
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
                let newQuery = Vector.map(match (j, t) -> (j, lowerTerm(rowVars, t)), prefixQuery);
                RelOp.Query(newVar, ramSym, newQuery, lowerOp(body, newVars, depth + 1))
            case RelOp.Functional(rowVar, f, terms, body) =>
                let newVar = RowVar.Index(depth);
                let newVars = Map.insert(rowVar, newVar, rowVars);
                let newTerms = terms |> Vector.map(lowerTerm(rowVars));
                let newBody = lowerOp(body, newVars, depth + 1);
                RelOp.Functional(newVar, f, newTerms, newBody)
            case RelOp.Project(terms, ramSym) =>
                 RelOp.Project(Vector.map(lowerTerm(rowVars), terms), ramSym)
            case RelOp.If(test, then) =>
                 RelOp.If(Vector.map(lowerExp(rowVars), test), lowerOp(then, rowVars, depth))
        }

    def lowerExp(rowVars: Map[RowVar, RowVar], exp: BoolExp[v]): BoolExp[v] =
        match exp {
            case BoolExp.Empty(_) => exp
            case BoolExp.NotMemberOf(terms, ramSym) =>
                 BoolExp.NotMemberOf(Vector.map(lowerTerm(rowVars), terms), ramSym)
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
    }
}

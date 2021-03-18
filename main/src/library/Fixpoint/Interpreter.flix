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
use Fixpoint/Ast.PredSym;
use Fixpoint/Ast.PredSym.PredSym;
use Fixpoint.Tuple;

instance Order[Tuple[v]] with [v : Order] {
    pub def less(a: Tuple[v], b: Tuple[v]): Bool = match a <=> b {
        case LessThan => true
        case _ => false
    }

    pub def lessEqual(a: Tuple[v], b: Tuple[v]): Bool = match a <=> b {
        case LessThan => true
        case EqualTo => true
        case _ => false
    }

    pub def greater(a: Tuple[v], b: Tuple[v]): Bool = match a <=> b {
        case GreaterThan => true
        case _ => false
    }

    pub def greaterEqual(a: Tuple[v], b: Tuple[v]): Bool = match a <=> b {
        case GreaterThan => true
        case EqualTo => true
        case _ => false
    }

    pub def compare(a: Tuple[v], b: Tuple[v]): Comparison = compareHelper(a, b, 0) as & Pure
}

// Unused def. unless this is public. Bug?
pub def compareHelper[v : Order](a: Array[v], b: Array[v], i: Int32): Comparison & Impure =
    let alen = Array.length(a);
    let blen = Array.length(b);
    if (i >= alen and i >= blen)
        EqualTo
    else if (i >= alen)
        LessThan
    else if (i >= blen)
        GreaterThan
    else match a[i] <=> b[i] {
        case EqualTo => compareHelper(a, b, i+1)
        case cmp => cmp
    }


namespace Fixpoint {
    pub type alias Tuple[v] = Array[v]
    type alias Database[v] = MutMap[RelSym, MutSet[Tuple[v]]]
    type alias SearchEnv[v] = Array[Tuple[v]]

    pub def interpret[v : Order : Eq](stmt: RamStmt[v]): Map[PredSym, Set[Tuple[v]]] =
        let db = MutMap.empty() as & Pure;
        let _ = evalStmt(db, stmt) as & Pure;
        toImmutable(db) as & Pure

    def toImmutable(db: Database[v]): Map[PredSym, Set[Tuple[v]]] & Impure =
        MutMap.foldLeftWithKey(acc -> k -> v -> match k {
            case RelSym.Full(sym) => Map.insert(PredSym(sym), MutSet.toImmutable(v), acc)
            case _ => acc
        }, Map#{}, db)

    def evalStmt[v : Order : Eq](db: Database[v], stmt: RamStmt[v]): Unit & Impure = match stmt {
        case RamStmt.Insert(relOp) => evalOp(db, allocEnv(0, relOp), relOp)
        case RamStmt.Merge(src, dst) =>
            db |>
            MutMap.getOrElsePut!(dst, MutSet.empty()) |>
            MutSet.addAll!(MutMap.getWithDefault(src, MutSet.empty(), db))
        case RamStmt.Assign(lhs, rhs) =>
            db |>
            MutMap.put!(lhs, MutMap.getWithDefault(rhs, MutSet.empty(), db))
        case RamStmt.Purge(relSym) => MutMap.remove!(relSym, db)
        case RamStmt.Seq(stmts) => List.foreach(evalStmt(db), stmts)
        case RamStmt.Until(test, body) =>
            if (List.forall(evalBoolExp(db, []) as BoolExp[v] -> Bool, test)) {
                ()
            } else {
                evalStmt(db, body);
                evalStmt(db, stmt)
            }
        case RamStmt.Comment(_) => ()
    }

    def allocEnv(depth: Int32, relOp: RelOp[v]): SearchEnv[v] & Impure = match relOp {
        case RelOp.Search(_, _, _, body) => allocEnv(depth + 1, body)
        case RelOp.Query(_, _, _, _, body) => allocEnv(depth + 1, body)
        case RelOp.Project(_) => Array.new([], depth)
    }

    def evalOp[v : Order : Eq](db: Database[v], env: SearchEnv[v], op: RelOp[v]): Unit & Impure =
        match op {
            case RelOp.Search(RowVar.Index(i), relSym, where, body) =>
                MutMap.getWithDefault(relSym, MutSet.empty(), db) |>
                MutSet.foreach(row -> {
                    env[i] = row;
                    if (List.forall(evalBoolExp(db, env) as BoolExp[v] -> Bool, where)) {
                        evalOp(db, env, body)
                    } else {
                        ()
                    }
                })
            case RelOp.Query(RowVar.Index(i), relSym, query, where, body) =>
                MutMap.getWithDefault(relSym, MutSet.empty(), db) |>
                MutSet.query(evalQuery(env, 0, query)) |>
                List.foreach(row -> {
                    env[i] = row;
                    if (List.forall(evalBoolExp(db, env) as BoolExp[v] -> Bool, where)) {
                        evalOp(db, env, body)
                    } else {
                        ()
                    }
                })
            case RelOp.Project(terms, relSym) =>
                let tuple = Array.map(evalTerm(env), terms);
                MutMap.getOrElsePut!(relSym, MutSet.empty(), db) |>
                MutSet.add!(tuple)
            case _ => ()
        }

    def evalQuery[v : Order](env: SearchEnv[v], i: Int32, query: Array[RamTerm[v]], tuple: Tuple[v]): Comparison =
        if (i >= Array.length(query))
            EqualTo
        else match (tuple[i] as & Pure) <=> evalTerm(env, query[i] as & Pure) {
            case EqualTo => evalQuery(env, i+1, query, tuple)
            case cmp => cmp
        }

    def evalBoolExp[v : Order : Eq](db: Database[v], env: SearchEnv[v], exp: BoolExp[v]): Bool & Impure =
        match exp {
            case BoolExp.Empty(relSym) =>
                MutMap.getWithDefault(relSym, MutSet.empty(), db) |> MutSet.isEmpty
            case BoolExp.NotMemberOf(terms, relSym) => match MutMap.get(relSym, db) {
                case Some(table) =>
                    let tuple = Array.map(evalTerm(env), terms);
                    not MutSet.memberOf(tuple, table)
                case None => true
            }
            case BoolExp.Guard(f, terms) =>
                Array.map(evalTerm(env), terms) |> f
            case BoolExp.Eq(lhs, rhs) =>
                evalTerm(env, lhs) == evalTerm(env, rhs)
        }

    def evalTerm(env: SearchEnv[v], term: RamTerm[v]): v = match term {
        case RamTerm.Lit(v) => v
        case RamTerm.Attribute(RowVar.Index(i), index) => env[i][index] as & Pure
        case RamTerm.App(f, args) => (Array.map(evalTerm(env), args) |> f) as & Pure
        case _ => ???
    }
}
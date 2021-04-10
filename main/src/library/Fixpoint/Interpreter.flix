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

use Fixpoint/Ram.{RamStmt, RelOp, RelSym, RamTerm, BoolExp, RowVar, toDenotation};
use Fixpoint/Ast.Denotation;

namespace Fixpoint {
    type alias Tuple[v] = Array[v]
    type alias Database[v] = MutMap[RelSym[v], MutMap[Tuple[v], v]]
    type alias SearchEnv[v] = (Array[Tuple[v]], Array[v])

    def interpret(stmt: RamStmt[v]): Database[v] & Impure with Order[v], Eq[v], ToString[v] =
        let _ = notifyPreInterpret(stmt);
        let db = MutMap.empty();
        evalStmt(db, stmt);
        db

    def evalStmt(db: Database[v], stmt: RamStmt[v]): Unit & Impure with Order[v], Eq[v], ToString[v] = match stmt {
        case RamStmt.Insert(relOp) => evalOp(db, allocEnv(0, relOp), relOp)
        case RamStmt.Merge(srcSym, dstSym) =>
            let dst = MutMap.getOrElsePut!(dstSym, MutMap.empty(), db);
            match toDenotation(srcSym) {
                case Denotation.Relational =>
                    MutMap.merge!(MutMap.getWithDefault(srcSym, MutMap.empty(), db), dst)
                case Denotation.Latticenal(_, _, lub, _) =>
                    MutMap.mergeWith!(lub, MutMap.getWithDefault(srcSym, MutMap.empty(), db), dst)
            }
        case RamStmt.Assign(lhs, rhs) =>
            MutMap.put!(lhs, MutMap.getWithDefault(rhs, MutMap.empty(), db), db)
        case RamStmt.Purge(relSym) => MutMap.remove!(relSym, db)
        case RamStmt.Seq(stmts) => List.foreach(evalStmt(db), stmts)
        case RamStmt.Until(test, body) =>
            if (evalBoolExp(db, ([], []), test)) {
                ()
            } else {
                evalStmt(db, body);
                evalStmt(db, stmt)
            }
        case RamStmt.Comment(_) => ()
    }

    def allocEnv(depth: Int32, relOp: RelOp[v]): SearchEnv[v] & Impure = match relOp {
        case RelOp.Search(_, _, body) => allocEnv(depth + 1, body)
        case RelOp.Query(_, _, _, body) => allocEnv(depth + 1, body)
        case RelOp.Project(_) => (Array.new([], depth), Array.new(default, depth))
        case RelOp.If(_, then) => allocEnv(depth, then)
    }

    def evalOp(db: Database[v], env: SearchEnv[v], op: RelOp[v]): Unit & Impure with Order[v], Eq[v], ToString[v] =
        match op {
            case RelOp.Search(RowVar.Index(i), relSym, body) =>
                let (tupleEnv, latEnv) = env;
                MutMap.foreach(t -> l -> {
                    tupleEnv[i] = t;
                    latEnv[i] = l;
                    evalOp(db, env, body)
                }, MutMap.getWithDefault(relSym, MutMap.empty(), db))
            case RelOp.Query(RowVar.Index(i), relSym, query, body) =>
                let (tupleEnv, latEnv) = env;
                MutMap.queryWith(evalQuery(env, 0, query), t -> l -> {
                    tupleEnv[i] = t;
                    latEnv[i] = l;
                    evalOp(db, env, body)
                }, MutMap.getWithDefault(relSym, MutMap.empty(), db))
            case RelOp.Project(terms, relSym) =>
                let rel = MutMap.getOrElsePut!(relSym, MutMap.empty(), db);
                match toDenotation(relSym) {
                    case Denotation.Relational =>
                        let tuple = Array.map(evalTerm(env), terms);
                        MutMap.put!(tuple, default, rel)
                    case Denotation.Latticenal(bot, leq, lub, _) =>
                        let len = Array.length(terms);
                        let key = Array.init(i -> evalTerm(env, terms[i]), len - 1);
                        let lat = evalTerm(env, terms[len - 1]);
                        if (lat `leq` bot())
                            ()
                        else
                            MutMap.putWith!(lub, key, lat, rel)
                }
            case RelOp.If(test, then) =>
                if (evalBoolExp(db, env, test)) {
                    evalOp(db, env, then)
                } else {
                    ()
                }
            case _ => ()
        }

    def evalQuery(env: SearchEnv[v], i: Int32, query: Array[RamTerm[v]], tuple: Tuple[v]): Comparison with Order[v], ToString[v] =
        if (i >= Array.length(query))
            EqualTo
        else match (tuple[i] as & Pure) <=> evalTerm(env, query[i] as & Pure) {
            case EqualTo => evalQuery(env, i+1, query, tuple)
            case cmp => cmp
        }

    def evalBoolExp(db: Database[v], env: SearchEnv[v], es: List[BoolExp[v]]): Bool with Order[v], Eq[v], ToString[v] =
        List.forall(exp -> match exp {
            case BoolExp.Empty(relSym) =>
                MutMap.isEmpty(MutMap.getWithDefault(relSym, MutMap.empty(), db))
            case BoolExp.NotMemberOf(terms, relSym) =>
                let rel = MutMap.getWithDefault(relSym, MutMap.empty(), db);
                match toDenotation(relSym) {
                    case Denotation.Relational =>
                        let tuple = Array.map(evalTerm(env), terms);
                        not MutMap.memberOf(tuple, rel)
                    case Denotation.Latticenal(bot, leq, _, _) =>
                        let len = Array.length(terms);
                        let key = Array.init(i -> evalTerm(env, terms[i]), len - 1);
                        let lat = evalTerm(env, terms[len - 1]);
                        not (lat `leq` MutMap.getWithDefault(key, bot(), rel))
                }
            case BoolExp.Eq(lhs, rhs) =>
                evalTerm(env, lhs) == evalTerm(env, rhs)
            case BoolExp.Leq(leq, lhs, rhs) =>
                evalTerm(env, lhs) `leq` evalTerm(env, rhs)
            case BoolExp.Guard0(f) =>
                f()
            case BoolExp.Guard1(f, t) =>
                let v = evalTerm(env, t);
                f(v)
            case BoolExp.Guard2(f, t1, t2) =>
                let v1 = evalTerm(env, t1);
                let v2 = evalTerm(env, t2);
                f(v1)(v2)
            case BoolExp.Guard3(f, t1, t2, t3) =>
                let v1 = evalTerm(env, t1);
                let v2 = evalTerm(env, t2);
                let v3 = evalTerm(env, t3);
                f(v1)(v2)(v3)
            case BoolExp.Guard4(f, t1, t2, t3, t4) =>
                let v1 = evalTerm(env, t1);
                let v2 = evalTerm(env, t2);
                let v3 = evalTerm(env, t3);
                let v4 = evalTerm(env, t4);
                f(v1)(v2)(v3)(v4)
            case BoolExp.Guard5(f, t1, t2, t3, t4, t5) =>
                let v1 = evalTerm(env, t1);
                let v2 = evalTerm(env, t2);
                let v3 = evalTerm(env, t3);
                let v4 = evalTerm(env, t4);
                let v5 = evalTerm(env, t5);
                f(v1)(v2)(v3)(v4)(v5)
        } as & Pure, es)

    def evalTerm(env: SearchEnv[v], term: RamTerm[v]): v with ToString[v] = match term {
        case RamTerm.Lit(v) => v
        case RamTerm.Attribute(RowVar.Index(i), index) =>
            let (tupleEnv, _) = env;
            tupleEnv[i][index] as & Pure
        case RamTerm.Lat(RowVar.Index(i)) =>
            let (_, latEnv) = env;
            latEnv[i] as & Pure
        case RamTerm.Meet(cap, lhs, rhs) =>
            evalTerm(env, lhs) `cap` evalTerm(env, rhs)
        case RamTerm.App0(f) =>
            f()
        case RamTerm.App1(f, t) =>
            let v = evalTerm(env, t);
            f(v)
        case RamTerm.App2(f, t1, t2) =>
            let v1 = evalTerm(env, t1);
            let v2 = evalTerm(env, t2);
            f(v1)(v2)
        case RamTerm.App3(f, t1, t2, t3) =>
            let v1 = evalTerm(env, t1);
            let v2 = evalTerm(env, t2);
            let v3 = evalTerm(env, t3);
            f(v1)(v2)(v3)
        case RamTerm.App4(f, t1, t2, t3, t4) =>
            let v1 = evalTerm(env, t1);
            let v2 = evalTerm(env, t2);
            let v3 = evalTerm(env, t3);
            let v4 = evalTerm(env, t4);
            f(v1)(v2)(v3)(v4)
        case RamTerm.App5(f, t1, t2, t3, t4, t5) =>
            let v1 = evalTerm(env, t1);
            let v2 = evalTerm(env, t2);
            let v3 = evalTerm(env, t3);
            let v4 = evalTerm(env, t4);
            let v5 = evalTerm(env, t5);
            f(v1)(v2)(v3)(v4)(v5)
        case _ => bug!("Illegal term ${term}")
    }
}
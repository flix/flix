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

use Fixpoint/Ram.{RamStmt, RelOp, RamSym, RamTerm, BoolExp, RowVar, toDenotation};
use Fixpoint/Ast.Denotation;

namespace Fixpoint {
    type alias Tuple[v] = Array[v]
    type alias Database[v] = MutMap[RamSym[v], MutMap[Tuple[v], v]]
    type alias SearchEnv[v] = (Array[Tuple[v]], Array[v])

    def interpret(stmt: RamStmt[v]): Database[v] & Impure with Order[v], Eq[v], ToString[v] =
        interpretWithDatabase(MutMap.empty(), stmt)

    def interpretWithDatabase(db: Database[v], stmt: RamStmt[v]): Database[v] & Impure with Order[v], Eq[v], ToString[v] =
        let _ = notifyPreInterpret(stmt);
        evalStmt(db, stmt);
        db

    def evalStmt(db: Database[v], stmt: RamStmt[v]): Unit & Impure with Order[v], Eq[v], ToString[v] = match stmt {
        case RamStmt.Facts(fs) => // TODO: consider removing this.
            use Fixpoint/Ast.HeadPredicate.HeadAtom;
            use Fixpoint/Ast.Constraint.Constraint;
            use Fixpoint/Ast.PredSym.PredSym;
            Array.foreach(match Constraint(HeadAtom(PredSym(headSym), denotation, terms), _) -> {
                let ramSym = RamSym.Full(headSym, denotation);
                let rel = MutMap.getOrElsePut!(ramSym, MutMap.empty(), db);
                match denotation {
                    case Denotation.Relational =>
                        let tuple = Array.map(headTermValue, terms);
                        MutMap.put!(tuple, default, rel)
                    case Denotation.Latticenal(bot, leq, lub, _) =>
                        let len = Array.length(terms);
                        let key = Array.init(i -> headTermValue(terms[i]), len - 1);
                        let lat = headTermValue(terms[len - 1]);
                        if (lat `leq` bot)
                            ()
                        else
                            MutMap.putWith!(lub, key, lat, rel)
                }
            }, fs)
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
        case RamStmt.Purge(ramSym) => MutMap.remove!(ramSym, db)
        case RamStmt.Seq(stmts) => Array.foreach(evalStmt(db), stmts)
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
            case RelOp.Search(RowVar.Index(i), ramSym, body) =>
                let (tupleEnv, latEnv) = env;
                MutMap.foreach(t -> l -> {
                    tupleEnv[i] = t;
                    latEnv[i] = l;
                    evalOp(db, env, body)
                }, MutMap.getWithDefault(ramSym, MutMap.empty(), db))
            case RelOp.Query(RowVar.Index(i), ramSym, query, body) =>
                let (tupleEnv, latEnv) = env;
                MutMap.queryWith(evalQuery(env, 0, query), t -> l -> {
                    tupleEnv[i] = t;
                    latEnv[i] = l;
                    evalOp(db, env, body)
                }, MutMap.getWithDefault(ramSym, MutMap.empty(), db))
            case RelOp.Project(terms, ramSym) =>
                let rel = MutMap.getOrElsePut!(ramSym, MutMap.empty(), db);
                match toDenotation(ramSym) {
                    case Denotation.Relational =>
                        let tuple = Array.map(evalTerm(env), terms);
                        MutMap.put!(tuple, default, rel)
                    case Denotation.Latticenal(bot, leq, lub, _) =>
                        let len = Array.length(terms);
                        let key = Array.init(i -> evalTerm(env, terms[i]), len - 1);
                        let lat = evalTerm(env, terms[len - 1]);
                        if (lat `leq` bot)
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

    def evalQuery(env: SearchEnv[v], i: Int32, query: Array[(Int32, RamTerm[v])], tuple: Tuple[v]): Comparison with Order[v], ToString[v] =
        if (i >= Array.length(query))
            EqualTo
        else {
            let (j, t) = query[i] as & Pure;
            match (tuple[j] as & Pure) <=> evalTerm(env, t) {
                case EqualTo => evalQuery(env, i+1, query, tuple)
                case cmp => cmp
            }
        }

    def evalBoolExp(db: Database[v], env: SearchEnv[v], es: List[BoolExp[v]]): Bool with Order[v], Eq[v], ToString[v] =
        List.forall(exp -> match exp {
            case BoolExp.Empty(ramSym) =>
                MutMap.isEmpty(MutMap.getWithDefault(ramSym, MutMap.empty(), db))
            case BoolExp.NotMemberOf(terms, ramSym) =>
                let rel = MutMap.getWithDefault(ramSym, MutMap.empty(), db);
                match toDenotation(ramSym) {
                    case Denotation.Relational =>
                        let tuple = Array.map(evalTerm(env), terms);
                        not MutMap.memberOf(tuple, rel)
                    case Denotation.Latticenal(bot, leq, _, _) =>
                        let len = Array.length(terms);
                        let key = Array.init(i -> evalTerm(env, terms[i]), len - 1);
                        let lat = evalTerm(env, terms[len - 1]);
                        not (lat `leq` MutMap.getWithDefault(key, bot, rel))
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
        case RamTerm.RowLoad(RowVar.Index(i), index) =>
            let (tupleEnv, _) = env;
            tupleEnv[i][index] as & Pure
        case RamTerm.LoadLatVar(RowVar.Index(i)) =>
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
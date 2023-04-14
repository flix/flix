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

namespace Fixpoint {

    use Fixpoint/Ram.{RamStmt, RelOp, RamSym, RamTerm, BoolExp, RowVar, toDenotation}
    use Fixpoint/Ast.Denotation

    type alias Database[v: Type, r: Region] = MutMap[RamSym[v], MutMap[Vector[v], v, r], r]
    type alias SearchEnv[v: Type, r: Region] = (Array[Vector[v], r], Array[v, r])

    def interpret(r: Region[r], stmt: RamStmt[v]): Database[v, r] \ Write(r) with Order[v] =
        interpretWithDatabase(r, MutMap.new(r), stmt)

    def interpretWithDatabase(r: Region[r], db: Database[v, r], stmt: RamStmt[v]): Database[v, r] \ { Read(r), Write(r) } with Order[v] =
        masked_cast(notifyPreInterpret(stmt));
        evalStmt(r, db, stmt);
        db

    def evalStmt(r: Region[r], db: Database[v, r], stmt: RamStmt[v]): Unit \ { Read(r), Write(r) } with Order[v] =
        match stmt {
            case RamStmt.Insert(relOp) => evalOp(r, db, allocEnv(r, 0, relOp), relOp)
            case RamStmt.Merge(srcSym, dstSym) =>
                let dst = MutMap.getOrElsePut!(dstSym, MutMap.new(r), db);
                match toDenotation(srcSym) {
                    case Denotation.Relational =>
                        MutMap.merge!(MutMap.getWithDefault(srcSym, MutMap.new(r), db), dst)
                    case Denotation.Latticenal(_, _, lub, _) =>
                        MutMap.mergeWith!(lub, MutMap.getWithDefault(srcSym, MutMap.new(r), db), dst)
                }
            case RamStmt.Assign(lhs, rhs) =>
                MutMap.put!(lhs, MutMap.getWithDefault(rhs, MutMap.new(r), db), db)
            case RamStmt.Purge(ramSym) => MutMap.remove!(ramSym, db)
            case RamStmt.Seq(stmts) => List.forEach(evalStmt(r, db), stmts)
            case RamStmt.Until(test, body) =>
                if (evalBoolExp(r, db, (Array#{} @ r, Array#{} @ r), List.toVector(test))) {
                    ()
                } else {
                    evalStmt(r, db, body);
                    evalStmt(r, db, stmt)
                }
            case RamStmt.Comment(_) => ()
        }

    def allocEnv(r: Region[r], depth: Int32, relOp: RelOp[v]): SearchEnv[v, r] \ Write(r) = match relOp {
        case RelOp.Search(_, _, body) => allocEnv(r, depth + 1, body)
        case RelOp.Query(_, _, _, body) => allocEnv(r, depth + 1, body)
        case RelOp.Project(_) => (Array.repeat(r, depth, Vector.empty()), Array.new(r, depth))
        case RelOp.If(_, then) => allocEnv(r, depth, then)
    }

    def evalOp(r1: Region[r1], db: Database[v, r1], env: SearchEnv[v, r2], op: RelOp[v]): Unit \ { Read(r1), Write(r1), Read(r2), Write(r2) } with Order[v] =
        match op {
            case RelOp.Search(RowVar.Index(i), ramSym, body) =>
                let (tupleEnv, latEnv) = env;
                MutMap.forEach(t -> l -> {
                    Array.put(t, i, tupleEnv);
                    Array.put(l, i, latEnv);
                    evalOp(r1, db, env, body)
                }, MutMap.getWithDefault(ramSym, MutMap.new(r1), db))
            case RelOp.Query(RowVar.Index(i), ramSym, query, body) =>
                let (tupleEnv, latEnv) = env;
                MutMap.queryWith(evalQuery(env, query), t -> l -> {
                    Array.put(t, i, tupleEnv);
                    Array.put(l, i, latEnv);
                    evalOp(r1, db, env, body)
                }, MutMap.getWithDefault(ramSym, MutMap.new(r1), db))
            case RelOp.Project(terms, ramSym) =>
                let rel = MutMap.getOrElsePut!(ramSym, MutMap.new(r1), db);
                match toDenotation(ramSym) {
                    case Denotation.Relational =>
                        let tuple = Vector.map(evalTerm(env), terms);
                        MutMap.put!(tuple, Reflect.default(), rel)
                    case Denotation.Latticenal(bot, leq, lub, _) =>
                        // assume that length(terms) > 0
                        let len = Vector.length(terms);
                        let keyList = terms |> Vector.map(evalTerm(env));
                        let (relKeys, latValList) = Vector.splitAt(len-1, keyList);
                        let latVal = match Vector.head(latValList) {
                            case None => bug!("Found predicate without terms")
                            case Some(k) => k
                        };
                        if (latVal `leq` bot) ()
                        else MutMap.putWith!(lub, relKeys, latVal, rel)
                }
            case RelOp.If(test, then) =>
                if (evalBoolExp(r1, db, env, test)) {
                    evalOp(r1, db, env, then)
                } else {
                    ()
                }
            case _ => ()
        }

    def evalQuery(env: SearchEnv[v, r], query: Vector[(Int32, RamTerm[v])], tuple: Vector[v]): Comparison \ Read(r) with Order[v] =
        match Vector.head(query) {
            case None => Comparison.EqualTo
            case Some((j, t)) => {
                let tl = Vector.slice(start = 1, end = Vector.length(query), query);
                match Vector.get(j, tuple) <=> evalTerm(env, t) {
                    case Comparison.EqualTo => evalQuery(env, tl, tuple)
                    case cmp => cmp
                }
            }
        }

    def evalBoolExp(r1: Region[r1], db: Database[v, r1], env: SearchEnv[v, r2], es: Vector[BoolExp[v]]): Bool \ { Read(r1), Read(r2) } with Order[v] =
        Vector.forAll(exp -> match exp {
            case BoolExp.Empty(ramSym) =>
                MutMap.isEmpty(MutMap.getWithDefault(ramSym, MutMap.new(r1), db))
            case BoolExp.NotMemberOf(terms, ramSym) =>
                let rel = MutMap.getWithDefault(ramSym, MutMap.new(r1), db);
                match toDenotation(ramSym) {
                    case Denotation.Relational =>
                        let tuple = Vector.map(evalTerm(env), terms);
                        not MutMap.memberOf(tuple, rel)
                    case Denotation.Latticenal(bot, leq, _, _) =>
                        let len = Vector.length(terms);
                        let (keyTerms, latTermList) = terms |> Vector.map(evalTerm(env)) |> Vector.splitAt(len - 1);
                        let latTerm = match Vector.head(latTermList) {
                            case None => bug!("Found predicate without terms")
                            case Some(hd) => hd
                        };
                        not (latTerm `leq` MutMap.getWithDefault(keyTerms, bot, rel))
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
        }, es)

    def evalTerm(env: SearchEnv[v, r], term: RamTerm[v]): v \ Read(r) = match term {
        case RamTerm.Lit(v) => v
        case RamTerm.RowLoad(RowVar.Index(i), index) =>
            let (tupleEnv, _) = env;
            Vector.get(index, Array.get(i, tupleEnv))
        case RamTerm.LoadLatVar(RowVar.Index(i)) =>
            let (_, latEnv) = env;
            Array.get(i, latEnv)
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

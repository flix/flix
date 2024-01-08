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

mod Fixpoint.Interpreter {

    use Fixpoint.Ast.Ram.{RamStmt, RelOp, RamSym, RamTerm, BoolExp, RowVar, toDenotation}
    use Fixpoint.Ast.Shared.Denotation

    pub type alias Database[v: Type, r: Region] = MutMap[RamSym[v], MutMap[Vector[v], v, r], r]

    pub type alias SearchEnv[v: Type, r: Region] = (Array[Vector[v], r], Array[v, r])

    @Internal
    pub def interpret(rc: Region[r], stmt: RamStmt[v]): Database[v, r] \ r with Order[v] =
        interpretWithDatabase(rc, MutMap.new(rc), stmt)

    @Internal
    pub def interpretWithDatabase(rc: Region[r], db: Database[v, r], stmt: RamStmt[v]): Database[v, r] \ r with Order[v] =
        masked_cast(Fixpoint.Debugging.notifyPreInterpret(stmt));
        evalStmt(rc, db, stmt);
        db

    def evalStmt(rc: Region[r], db: Database[v, r], stmt: RamStmt[v]): Unit \ r with Order[v] =
        match stmt {
            case RamStmt.Insert(relOp) => evalOp(rc, db, allocEnv(rc, 0, relOp), relOp)
            case RamStmt.Merge(srcSym, dstSym) =>
                let dst = MutMap.getOrElsePut!(dstSym, MutMap.new(rc), db);
                match toDenotation(srcSym) {
                    case Denotation.Relational =>
                        MutMap.merge!(MutMap.getWithDefault(srcSym, MutMap.new(rc), db), dst)
                    case Denotation.Latticenal(_, _, lub, _) =>
                        MutMap.mergeWith!(lub, MutMap.getWithDefault(srcSym, MutMap.new(rc), db), dst)
                }
            case RamStmt.Assign(lhs, rhs) =>
                MutMap.put!(lhs, MutMap.getWithDefault(rhs, MutMap.new(rc), db), db)
            case RamStmt.Purge(ramSym) => MutMap.remove!(ramSym, db)
            case RamStmt.Seq(stmts) => Vector.forEach(evalStmt(rc, db), stmts)
            case RamStmt.Until(test, body) =>
                if (evalBoolExp(rc, db, (Array#{} @ rc, Array#{} @ rc), test)) {
                    ()
                } else {
                    evalStmt(rc, db, body);
                    evalStmt(rc, db, stmt)
                }
            case RamStmt.Comment(_) => ()
        }

    def allocEnv(rc: Region[r], depth: Int32, relOp: RelOp[v]): SearchEnv[v, r] \ r = match relOp {
        case RelOp.Search(_, _, body)           => allocEnv(rc, depth + 1, body)
        case RelOp.Query(_, _, _, body)         => allocEnv(rc, depth + 1, body)
        case RelOp.Functional(_, _, _, body)    => allocEnv(rc, depth + 1, body)
        case RelOp.Project(_)                   => (Array.repeat(rc, depth, Vector.empty()), Array.new(rc, depth))
        case RelOp.If(_, then)                  => allocEnv(rc, depth, then)
    }

    def evalOp(rc1: Region[r1], db: Database[v, r1], env: SearchEnv[v, r2], op: RelOp[v]): Unit \ { r1, r2 } with Order[v] =
        match op {
            case RelOp.Search(RowVar.Index(i), ramSym, body) =>
                let (tupleEnv, latEnv) = env;
                MutMap.forEach(t -> l -> {
                    Array.put(t, i, tupleEnv);
                    Array.put(l, i, latEnv);
                    evalOp(rc1, db, env, body)
                }, MutMap.getWithDefault(ramSym, MutMap.new(rc1), db))
            case RelOp.Query(RowVar.Index(i), ramSym, query, body) =>
                let (tupleEnv, latEnv) = env;
                MutMap.queryWith(evalQuery(env, Vector.toList(query)), t -> l -> {
                    Array.put(t, i, tupleEnv);
                    Array.put(l, i, latEnv);
                    evalOp(rc1, db, env, body)
                }, MutMap.getWithDefault(ramSym, MutMap.new(rc1), db))
            case RelOp.Functional(RowVar.Index(i), f, terms, body) =>
                let args = terms |> Vector.map(evalTerm(env));
                let result = f(args): Vector[Vector[v]];

                let (tupleEnv, _latEnv) = env; // TODO: Do we ever need to use latEnv?
                foreach (t <- result) {
                    Array.put(t, i, tupleEnv);
                    evalOp(rc1, db, env, body)
                }
            case RelOp.Project(terms, ramSym) =>
                let rel = MutMap.getOrElsePut!(ramSym, MutMap.new(rc1), db);
                match toDenotation(ramSym) {
                    case Denotation.Relational =>
                        let tuple = Vector.map(evalTerm(env), terms);
                        MutMap.put!(tuple, Reflect.default(), rel)
                    case Denotation.Latticenal(bot, leq, lub, _) =>
                        // assume that length(terms) > 0
                        let len = Vector.length(terms);
                        let keyList = terms |> Vector.map(evalTerm(env));
                        let (key, latValList) = Vector.splitAt(len - 1, keyList);
                        let latVal = match Vector.head(latValList) {
                            case None => bug!("Found predicate without terms")
                            case Some(k) => k
                        };
                        if (latVal `leq` bot) ()
                        else MutMap.putWith!(lub, key, latVal, rel)
                }
            case RelOp.If(test, then) =>
                if (evalBoolExp(rc1, db, env, test)) {
                    evalOp(rc1, db, env, then)
                } else {
                    ()
                }
            case _ => ()
        }

    def evalQuery(env: SearchEnv[v, r], query: List[(Int32, RamTerm[v])], tuple: Vector[v]): Comparison \ r with Order[v] =
        match query {
            case Nil => Comparison.EqualTo
            case (j, t) :: tl => match Vector.get(j, tuple) <=> evalTerm(env, t) {
                case Comparison.EqualTo => evalQuery(env, tl, tuple)
                case cmp => cmp
            }
        }

    def evalBoolExp(rc1: Region[r1], db: Database[v, r1], env: SearchEnv[v, r2], es: Vector[BoolExp[v]]): Bool \ { r1, r2 } with Order[v] =
        Vector.forAll(exp -> match exp {
            case BoolExp.Empty(ramSym) =>
                MutMap.isEmpty(MutMap.getWithDefault(ramSym, MutMap.new(rc1), db))
            case BoolExp.NotMemberOf(terms, ramSym) =>
                let rel = MutMap.getWithDefault(ramSym, MutMap.new(rc1), db);
                match toDenotation(ramSym) {
                    case Denotation.Relational =>
                        let tuple = Vector.map(evalTerm(env), terms);
                        not MutMap.memberOf(tuple, rel)
                    case Denotation.Latticenal(bot, leq, _, _) =>
                        let len = Vector.length(terms);
                        let evalTerms = Vector.map(evalTerm(env), terms);
                        let key = Vector.take(len - 1, evalTerms);
                        let latTerms = Vector.drop(len - 1, evalTerms);
                        let latTerm = match Vector.head(latTerms) {
                            case None => bug!("Found predicate without terms")
                            case Some(hd) => hd
                        };
                        not (latTerm `leq` MutMap.getWithDefault(key, bot, rel))
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

    def evalTerm(env: SearchEnv[v, r], term: RamTerm[v]): v \ r = match term {
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
        case RamTerm.RowLoad(rowVar, index) => bug!("Illegal RowLoad with ${rowVar} ${index}")
        case _ => bug!("Illegal term ${term}")
    }
}

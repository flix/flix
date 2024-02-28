/*
 * Copyright 2015-2016 Magnus Madsen
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
package ca.uwaterloo.flix

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.Ast.Constant
import ca.uwaterloo.flix.language.ast.SemanticOp.{BoolOp, CharOp, Float32Op, Float64Op, Int16Op, Int32Op, Int64Op, Int8Op, StringOp}
import ca.uwaterloo.flix.language.ast.{Ast, SemanticOp, Symbol, TypedAst}
import ca.uwaterloo.flix.language.ast.TypedAst.Expr
import ca.uwaterloo.flix.tools.Tester
import ca.uwaterloo.flix.util._
import org.json4s.reflect.fail

import java.io.File
import scala.::
import scala.util.Random

object MutationTester {
    def run(flix: Flix, tester: String, testee: String): Unit = {
        val root = flix.check().unsafeGet
        val start = System.nanoTime()
        val root1 = mutateRoot(root, testee)
        val end = System.nanoTime() - start
        val timeSec = end.toFloat / 1_000_000_000.0
        println(s"time to generate mutations: $timeSec")
        runMutations(flix, root, root1)
        /**
        val result = root1.map(r => flix.codeGen(r).unsafeGet)
        val tests = result.map(res => res.getTests)
        */
    }

    private def runMutations(flix: Flix, root: TypedAst.Root, mutatedDefs: List[(Symbol.DefnSym, List[TypedAst.Def])]): Unit = {
        val totalStartTime = System.nanoTime()
        var timeTemp = 0.0
        var survivorCount = 0
        var mutantCounter = 0
        mutatedDefs.foreach(mut => {
            val defName = mut._1.toString
            val mutationAmount = mut._2.length
            println(s"testing $defName with $mutationAmount mutations")
            val defs = root.defs
            mut._2.foreach(mDef => {
                mutantCounter += 1
                val start = System.nanoTime()
                val n = defs + (mut._1 -> mDef)
                println(s"mutation: $mDef")
                val newRoot = root.copy(defs = n)
                val cRes = flix.codeGen(newRoot).unsafeGet
                val testResults = cRes.getTests.values.forall(c =>
                    try {
                        c.run() match {
                            case java.lang.Boolean.TRUE => true
                            case _ => false
                        }
                    } catch {
                        case _: Throwable => false
                    }
                )
                timeTemp += (System.nanoTime() - start).toFloat / 1_000_000_000
                if (testResults) {
                    survivorCount += 1
                    val sym = mDef.sym.toString
                    println(s"mutation in $sym survived")
                }
            })
        })
        val totalEndTime = System.nanoTime() - totalStartTime
        println(s"there where $survivorCount surviving mutations, out of $mutantCounter mutations")
        val average = timeTemp/mutantCounter
        println(s"average time to test a mutant:  $average sec")
        val time = totalEndTime.toFloat/1_000_000_000
        println(s"total time to test all mutants: $time sec")
    }
    private def mutateRoot(root: TypedAst.Root, testee: String): List[(Symbol.DefnSym, List[TypedAst.Def])] = {
        val defs = root.defs
        val defSyms = root.modules.filter(pair => (pair._1.toString.equals(testee))).values.toList.flatten
        mutateDefs(defs, defSyms).flatten
    }


    private def mutateDefs(defs: Map[Symbol.DefnSym, TypedAst.Def], defSyms: List[Symbol]) = {
        defs.toList.map(d => (d._1, d._2) match {
            case (s, fun) =>
                if (defSyms.contains(s)) {
                    val mutExps = mutateExpr(fun.exp)
                    val mutDefs = mutExps.map(mexp => fun.copy(exp = mexp))
                    Some(d._1 -> mutDefs)
                } else None
            case _ => None
        })
    }


    private def mutationPermutations(exps: List[TypedAst.Expr]): List[List[TypedAst.Expr]] = {
        def mutate(exps: List[TypedAst.Expr], index: Int) : List[TypedAst.Expr] = (exps, index) match {
            case (x::Nil, 0) => mutateExpr(x)
            case (x::xs, 0) => mutateExpr(x)
            case (x::xs, n) => mutate(xs, n-1)
            case (_, _) => Nil
        }

        def replace(index: Int, mutation: TypedAst.Expr, exps: List[TypedAst.Expr]): List[TypedAst.Expr] =
            (index, exps) match {
                case (0, _::Nil) => mutation :: Nil
                case (0, x::xs) => mutation::xs
                case (n, x::xs) => x :: replace(n-1, mutation, xs)
                case (_,_) => Nil
        }

        var perms: List[List[TypedAst.Expr]] = Nil

        for (i <- exps.indices) {
            val mutations = mutate(exps, i)
            for(m <- mutations){
                perms = replace(i, m, exps) :: perms
            }

        }
        perms

    }
    /**
        // var doesn't contain a subtree, and we don't mutate them so we don't need to return them
        case Expr.Var(_, _, _) => Nil
      */
    private def mutateExpr(e: TypedAst.Expr): List[TypedAst.Expr] = e match {
        case Expr.Cst(cst, tpe, loc) =>
            mutateCst(cst).map(m => Expr.Cst(m, tpe, loc))
        case original@Expr.Var(_, _, _) => original :: Nil
        case original@Expr.Def(sym, tpe, loc) => original :: Nil
        case original@Expr.Sig(sym, tpe, loc) => original :: Nil
        case original@Expr.Hole(sym, tpe, loc) => original :: Nil
        case original@Expr.HoleWithExp(exp, tpe, eff, loc) => mutateExpr(exp).map(m => original.copy(exp = m))
        case original@Expr.OpenAs(symUse, exp, tpe, loc) => mutateExpr(exp).map(m => original.copy(exp = m))
        case original@Expr.Use(sym, alias, exp, loc) => mutateExpr(exp).map(m => original.copy(exp = m))
        case original@Expr.Lambda(fparam, exp, _, _) =>
            mutateExpr(exp).map(m => original.copy(exp = m))

        case original@Expr.Apply(exp, exps, tpe, eff, loc) =>
            mutationPermutations(exps).map(mp => {
                println(mp)
                original.copy(exps = mp)
            })

        case original@Expr.Unary(sop, exp, tpe, eff, loc) =>
            val mut1 = Expr.Unary(sop, original, tpe, eff, loc)
            mut1 :: mutateExpr(exp).map(m => original.copy(exp = m))

        case original@Expr.Binary(sop, exp1, exp2, _, _, _) =>
            val mut1 = mutateSop(sop).map(m => original.copy(sop = m))
            val mut2 = mutateExpr(exp1).map(m => original.copy(exp1 = m))
            val mut3 = mutateExpr(exp2).map(m => original.copy(exp2 = m))
            mut1:::mut2:::mut3

        case original@Expr.Let(sym, mod, exp1, exp2, tpe, eff, loc) =>
            mutateExpr(exp2).map(m => original.copy(exp2 = m))
        case original@Expr.LetRec(sym, ann, mod, exp1, exp2, tpe, eff, loc) => original :: Nil
        case original@Expr.Region(tpe, loc) => original :: Nil
        case original@Expr.Scope(sym, regionVar, exp, tpe, eff, loc) => mutateExpr(exp).map(m => original.copy(exp = m))
        case original@Expr.IfThenElse(exp1, exp2, exp3, _, _, _) =>
            val mut1 = mutateExpr(exp1).map(m => original.copy(exp1 = m))
            val mut2 = mutateExpr(exp2).map(m => original.copy(exp2 = m))
            val mut3 = mutateExpr(exp3).map(m => original.copy(exp3 = m))
            mut1:::mut2:::mut3
        case original@Expr.Stm(exp1, exp2, _, _, _) =>
            val mut1 = mutateExpr(exp1).map(m => original.copy(exp1 = m))
            val mut2 = mutateExpr(exp2).map(m => original.copy(exp2 = m))
            mut1 ::: mut2
        case original@Expr.Discard(exp, eff, loc) => original :: Nil
        case original@Expr.Match(_, rules, _, _, _) =>
            // refactor to permutation
            val permutations = rules.permutations.toList
            permutations.flatMap(p => p.map(r => mutateMatchrule(r)).map(m => original.copy(rules = m)))
        case original@Expr.TypeMatch(exp, rules, tpe, eff, loc) => original :: Nil
        case original@Expr.RestrictableChoose(star, exp, rules, tpe, eff, loc) => original :: Nil
        case original@Expr.Tag(sym, exp, tpe, eff, loc) => original :: Nil
        case original@Expr.RestrictableTag(sym, exp, tpe, eff, loc) => original :: Nil
        case original@Expr.Tuple(elms, _, _, _) =>
            val mutateElms = elms.map(e => mutateExpr(e))
            mutateElms.map(m => original.copy(elms = m))
        case original@Expr.RecordEmpty(tpe, loc) => original :: Nil
        case original@Expr.RecordSelect(exp, label, tpe, eff, loc) => original :: Nil
        case original@Expr.RecordExtend(label, exp1, exp2, tpe, eff, loc) => original :: Nil
        case original@Expr.RecordRestrict(label, exp, tpe, eff, loc) => original :: Nil
        case original@Expr.ArrayLit(exps, exp, _, _, _) =>
            val mut = mutateExpr(exp).map(m => original.copy (exp = m))
            val mutateExps = exps.map(e => mutateExpr(e))
            mutateExps.map(m => original.copy(exps = m)) ::: mut
        case original@Expr.ArrayNew(exp1, exp2, exp3, _, _, _) =>
            val mut1 = mutateExpr (exp1).map (m => original.copy (exp1 = m) )
            val mut2 = mutateExpr (exp2).map (m => original.copy (exp2 = m) )
            val mut3 = mutateExpr (exp3).map (m => original.copy (exp3 = m) )
            mut1 ::: mut2 ::: mut3
        case original@Expr.ArrayLoad(exp1, exp2, _, _, _) =>
            val mut1 = mutateExpr(exp1).map(m => original.copy(exp1 = m))
            val mut2 = mutateExpr(exp2).map(m => original.copy(exp2 = m))
            mut1 ::: mut2
        case original@Expr.ArrayLength(exp, eff, loc) =>
            mutateExpr(exp).map(m => original.copy(exp = m))
        case original@Expr.ArrayStore(exp1, exp2, exp3, _, _) =>
            val mut1 = mutateExpr(exp1).map(m => original.copy(exp1 = m))
            val mut2 = mutateExpr(exp2).map(m => original.copy(exp2 = m))
            val mut3 = mutateExpr(exp3).map(m => original.copy(exp3 = m))
            mut1 ::: mut2 ::: mut3
        case original@Expr.VectorLit(exps, _, _, _) =>
            val mutateExps = exps.map(e => mutateExpr(e))
            mutateExps.map(m => original.copy(exps = m))
        case original@Expr.VectorLoad(exp1, exp2, _, _, _) =>
            val mut1 = mutateExpr(exp1).map(m => original.copy(exp1 = m))
            val mut2 = mutateExpr(exp2).map(m => original.copy(exp2 = m))
            mut1 ::: mut2
        case original@Expr.VectorLength(exp, _) =>
            mutateExpr(exp).map(m => original.copy(exp = m))
        case original@Expr.Ref(exp1, exp2, _, _, _) =>
            val mut1 = mutateExpr(exp1).map(m => original.copy(exp1 = m))
            val mut2 = mutateExpr(exp2).map(m => original.copy(exp2 = m))
            mut1 ::: mut2
        case original@Expr.Deref(exp, tpe, eff, loc) =>
            mutateExpr(exp).map(m => original.copy(exp = m))
        case original@Expr.Assign(exp1, exp2, _, _, _) =>
            val mut1 = mutateExpr(exp1).map(m => original.copy(exp1 = m))
            val mut2 = mutateExpr(exp2).map(m => original.copy(exp2 = m))
            mut1:::mut2
        case original@Expr.Ascribe(exp, _, _, _) =>
            mutateExpr(exp).map(m => original.copy(exp = m))
        case original@Expr.InstanceOf(exp, clazz, loc) => mutateExpr(exp).map(m => original.copy(exp = m))
        case original@Expr.CheckedCast(cast, exp, tpe, eff, loc) => mutateExpr(exp).map(m => original.copy(exp = m))
        case original@Expr.UncheckedCast(exp, declaredType, declaredEff, tpe, eff, loc) => mutateExpr(exp).map(m => original.copy(exp = m))
        case original@Expr.UncheckedMaskingCast(exp, tpe, eff, loc) => mutateExpr(exp).map(m => original.copy(exp = m))
        case original@Expr.Without(exp, effUse, tpe, eff, loc) => mutateExpr(exp).map(m => original.copy(exp = m))
        case original@Expr.TryCatch(exp, rules, tpe, eff, loc) => mutateExpr(exp).map(m => original.copy(exp = m))
        case original@Expr.TryWith(exp, effUse, rules, tpe, eff, loc) => mutateExpr(exp).map(m => original.copy(exp = m))
        case original@Expr.Do(op, exps, tpe, eff, loc) =>
            val mutateExps = exps.map(e => mutateExpr(e))
            mutateExps.map(m => original.copy(exps = m))
        case original@Expr.InvokeConstructor(constructor, exps, tpe, eff, loc) =>
            val mutateExps = exps.map(e => mutateExpr(e))
            mutateExps.map(m => original.copy(exps = m))
        case original@Expr.InvokeMethod(method, exp, exps, tpe, eff, loc) => original :: Nil
        case original@Expr.InvokeStaticMethod(method, exps, tpe, eff, loc) =>
            val mutateExps = exps.map(e => mutateExpr(e))
            mutateExps.map(m => original.copy(exps = m))
        case original@Expr.GetField(field, exp, tpe, eff, loc) =>
            mutateExpr(exp).map(m => original.copy(exp = m))
        case original@Expr.PutField(field, exp1, exp2, tpe, eff, loc) =>
            val mut1 = mutateExpr(exp1).map(m => original.copy(exp1 = m))
            val mut2 = mutateExpr(exp2).map(m => original.copy(exp2 = m))
            mut1 ::: mut2
        case original@Expr.GetStaticField(field, tpe, eff, loc) => original :: Nil
        case original@Expr.PutStaticField(field, exp, tpe, eff, loc) => mutateExpr(exp).map(m => original.copy(exp = m))
        case original@Expr.NewObject(name, clazz, tpe, eff, methods, loc) => original :: Nil
        case original@Expr.NewChannel(exp1, exp2, _, _, _) =>
            val mut1 = mutateExpr(exp1).map(m => original.copy(exp1 = m))
            val mut2 = mutateExpr(exp2).map(m => original.copy(exp2 = m))
            mut1 ::: mut2
        case original@Expr.GetChannel(exp, tpe, eff, loc) => mutateExpr(exp).map(m => original.copy(exp = m))
        case original@Expr.PutChannel(exp1, exp2, _, _, _) =>
            val mut1 = mutateExpr(exp1).map(m => original.copy(exp1 = m))
            val mut2 = mutateExpr(exp2).map(m => original.copy(exp2 = m))
            mut1 ::: mut2
        case original@Expr.SelectChannel(rules, default, tpe, eff, loc) => original :: Nil
        case original@Expr.Spawn(exp1, exp2, _, _, _) =>
            val mut1 = mutateExpr(exp1).map(m => original.copy(exp1 = m))
            val mut2 = mutateExpr(exp2).map(m => original.copy(exp2 = m))
            mut1 ::: mut2
        case original@Expr.ParYield(frags, exp, _, _, _) => mutateExpr(exp).map(m => original.copy(exp = m))
        case original@Expr.Lazy(exp, _, _) => mutateExpr(exp).map(m => original.copy(exp = m))
        case original@Expr.Force(exp, _, _, _) => mutateExpr(exp).map(m => original.copy(exp = m))
        case original@Expr.FixpointConstraintSet(cs, tpe, loc) => original :: Nil
        case original@Expr.FixpointLambda(pparams, exp, tpe, eff, loc) => mutateExpr(exp).map(m => original.copy(exp = m))
        case original@Expr.FixpointMerge(exp1, exp2, _, _, _) =>
            val mut1 = mutateExpr(exp1).map(m => original.copy(exp1 = m))
            val mut2 = mutateExpr(exp2).map(m => original.copy(exp2 = m))
            mut1 ::: mut2
        case original@Expr.FixpointSolve(exp, tpe, eff, loc) => mutateExpr(exp).map(m => original.copy(exp = m))
        case original@Expr.FixpointFilter(pred, exp, tpe, eff, loc) => mutateExpr(exp).map(m => original.copy(exp = m))
        case original@Expr.FixpointInject(exp, pred, tpe, eff, loc) => mutateExpr(exp).map(m => original.copy(exp = m))
        case original@Expr.FixpointProject(pred, exp, tpe, eff, loc) => mutateExpr(exp).map(m => original.copy(exp = m))
        case original@Expr.Error(m, tpe, eff) => original :: Nil
    }

//    private def mutateAndPrepend[A](original: A, mutatee: A, func: (A => List[A]), copyFunc: (A => A), list: List[A]): List[A] = {
//        func(mutatee).foldLeft(list)((acc, m) => copyFunc(m)::acc)
 //   }
    private def mutateMatchrule(mr: TypedAst.MatchRule): List[TypedAst.MatchRule] = {
        mutateExpr(mr.exp).map(m => mr.copy(exp = m))

    }
    private def mutatePattern(pattern: TypedAst.Pattern): List[TypedAst.Pattern] = {
        pattern match {
            case original@TypedAst.Pattern.Cst(cst, _, _) => mutateCst(cst).map(m => original.copy(m))
            case e => Nil
        }
    }
    private def mutateSop(sop: SemanticOp): List[SemanticOp] = {
        def helper(sop: SemanticOp): List[SemanticOp] = sop match {
            case op: SemanticOp.BoolOp => op match {
                case BoolOp.Not => BoolOp.Not :: Nil
                case _ => BoolOp.Or :: BoolOp.And :: BoolOp.Neq :: BoolOp.Eq :: Nil
            }
            case op: SemanticOp.CharOp => op match {
                case _ => CharOp.Neq :: CharOp.Eq :: CharOp.Neq :: CharOp.Ge :: CharOp.Le :: CharOp.Lt :: CharOp.Gt :: Nil
            }
            case op: SemanticOp.Float32Op => op match{
                case Float32Op.Neg => Float32Op.Neg :: Nil
                case Float32Op.Add | Float32Op.Sub | Float32Op.Mul |
                  Float32Op.Sub | Float32Op.Div | Float32Op.Exp =>
                    Float32Op.Add :: Float32Op.Sub :: Float32Op.Mul :: Float32Op.Sub :: Float32Op.Div :: Float32Op.Exp :: Nil
                case _ => Float32Op.Eq :: Float32Op.Neq :: Float32Op.Lt :: Float32Op.Le :: Float32Op.Gt :: Float32Op.Ge :: Nil
            }
            case op: SemanticOp.Float64Op => op match {
                case Float64Op.Neg | Float64Op.Add | Float64Op.Sub | Float64Op.Mul | Float64Op.Div | Float64Op.Exp =>
                    Float64Op.Neq :: Float64Op.Add :: Float64Op.Sub :: Float64Op.Mul :: Float64Op.Div :: Float64Op.Exp :: Nil
                case _ => Float64Op.Eq :: Float64Op.Neq :: Float64Op.Lt :: Float64Op.Le :: Float64Op.Gt :: Float64Op.Ge :: Nil
            }
            case op: SemanticOp.Int8Op => op match {
                case Int8Op.Neg => Int8Op.Neg :: Nil
                case Int8Op.Not => Int8Op.Not :: Nil
                case Int8Op.Add | Int8Op.Div | Int8Op.Sub | Int8Op.Mul | Int8Op.Rem | Int8Op.Exp | Int8Op.Shl | Int8Op.Shr =>
                    Int8Op.Add :: Int8Op.Div :: Int8Op.Sub :: Int8Op.Mul :: Int8Op.Rem :: Int8Op.Exp :: Int8Op.Shl :: Int8Op.Shr ::Nil
                case Int8Op.And | Int8Op.Or | Int8Op.Xor => Int8Op.And :: Int8Op.Or :: Int8Op.Xor :: Nil
                case _ => Int8Op.Eq :: Int8Op.Neq :: Int8Op.Lt :: Int8Op.Le :: Int8Op.Gt :: Int8Op.Ge :: Nil
            }
            case op: SemanticOp.Int16Op => op match {
                case Int16Op.Neg => Int16Op.Neg :: Nil
                case Int16Op.Not => Int16Op.Not :: Nil
                case Int16Op.Add | Int16Op.Sub | Int16Op.Mul | Int16Op.Div | Int16Op.Rem | Int16Op.Exp | Int16Op.Shr | Int16Op.Shl =>
                    Int16Op.Sub :: Int16Op.Sub :: Int16Op.Mul :: Int16Op.Div :: Int16Op.Rem :: Int16Op.Exp :: Int16Op.Shr :: Int16Op.Shl :: Nil
                case Int16Op.And | Int16Op.Or | Int16Op.Xor => Int16Op.And :: Int16Op.Or :: Int16Op.Xor :: Nil
                case _ => Int16Op.Eq :: Int16Op.Neq :: Int16Op.Lt :: Int16Op.Le :: Int16Op.Gt :: Int16Op.Ge :: Nil
            }
            case op: SemanticOp.Int32Op => op match {
                case Int32Op.Neg => Int32Op.Neg :: Nil
                case Int32Op.Not => Int32Op.Not :: Nil
                case Int32Op.Add | Int32Op.Sub | Int32Op.Mul | Int32Op.Div | Int32Op.Rem | Int32Op.Exp | Int32Op.Shr | Int32Op.Shl =>
                    Int32Op.Add :: Int32Op.Sub :: Int32Op.Mul :: Int32Op.Div :: Int32Op.Rem :: Int32Op.Exp :: Int32Op.Shr :: Int32Op.Shl :: Nil
                case Int32Op.And | Int32Op.Or | Int32Op.Xor => Int32Op.And :: Int32Op.Or :: Int32Op.Xor :: Nil
                case _ => Int32Op.Eq :: Int32Op.Neq :: Int32Op.Lt :: Int32Op.Le :: Int32Op.Gt :: Int32Op.Ge :: Nil
            }
            case op: SemanticOp.Int64Op => op match {
                case Int64Op.Neg => Int64Op.Neg :: Nil
                case Int64Op.Not => Int64Op.Not :: Nil
                case Int64Op.Add | Int64Op.Sub | Int64Op.Shl | Int64Op.Shr | Int64Op.Mul | Int64Op.Div | Int64Op.Rem | Int64Op.Exp  =>
                    Int64Op.Add :: Int64Op.Sub :: Int64Op.Mul :: Int64Op.Shl :: Int64Op.Shr :: Int64Op.Div :: Int64Op.Rem :: Int64Op.Exp :: Nil
                case Int64Op.And | Int64Op.Or | Int64Op.Xor => Int64Op.And :: Int64Op.Or :: Int64Op.Xor :: Nil
                case _ => Int64Op.Eq :: Int64Op.Neq :: Int64Op.Lt :: Int64Op.Le :: Int64Op.Gt :: Int64Op.Ge :: Nil
            }
            case op: SemanticOp.StringOp => op match {
                case StringOp.Concat => StringOp.Concat :: Nil
            }
        }
        helper(sop).filter(e => e != sop)
    }
    private def mutateCst(cst: Ast.Constant): List[Ast.Constant] = {
        cst match {
            case Constant.Unit => Constant.Unit :: Nil
            case Constant.Null => Constant.Null :: Nil
            case Constant.Bool(lit) => Constant.Bool(!lit) :: Nil
            case Constant.Char(lit) => Constant.Char((lit^Char.MaxValue).toChar) :: Nil
            case Constant.Float32(lit) => Constant.Float32(lit + 1) :: Nil
            case Constant.Float64(lit) => Constant.Float64(lit + 1) :: Nil
            case Constant.BigDecimal(lit) => Constant.BigDecimal(lit.add(java.math.BigDecimal.ONE)) :: Nil
            case Constant.Int8(lit) => Constant.Int8(lit.+(1).toByte) :: Nil
            case Constant.Int16(lit) => Constant.Int16(lit.+(1).toShort) :: Nil
            case Constant.Int32(lit) => Constant.Int32(lit + 1) :: Nil
            case Constant.Int64(lit) => Constant.Int64(lit + 1) :: Nil
            case Constant.BigInt(lit) => Constant.BigInt(lit.add(lit)) :: Nil
            case Constant.Str(lit) => Constant.Str(lit + "\b") :: Nil
            case Constant.Regex(_) => Constant.Regex(java.util.regex.Pattern.compile("a")) :: Nil
        }
    }

}

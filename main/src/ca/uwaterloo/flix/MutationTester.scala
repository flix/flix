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
    def run(files: Seq[File], options: Options, tester: String, testee: String): Unit = {
        val flix = new Flix
        flix.setOptions(options)
        files.map(file => flix.addFlix(file.toPath))
        flix.addSourceCode("testee",
            s"""
            |def main(): Unit \\ IO =
            |    println(Test.one())
            |def two(): Int32 = 2
            |mod Fake {
            |    pub def fake(): Int32 = 99
            |}
            |mod Test {
            |    pub def one(): Int32 = 0 + 1
            |    pub def ite(b: Bool): Int32 = if(b) 1 else 2
            |    pub def m(i: Int32): Int32 = match i {
            |       case 1 => 100
            |       case _ => 0
            |    }
            |}""".stripMargin)

        flix.addSourceCode("tester",
            s"""
            |mod MainTest {
            |    @test
            |    def testConstant(): Bool =
            |        Assert.eq(1, Test.one())
            |    @test
            |    def testIte(): Bool =
            |       Assert.eq(1, Test.ite(true))
            |    @test
            |    def testMatch(): Bool =
            |       Assert.eq(100, Test.m(1))
            |}""".stripMargin)
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
                timeTemp += (System.nanoTime() - start).toFloat / 1000000000
                if (testResults) {
                    survivorCount += 1
                    println("mutation in " + mDef.sym.toString + " survived")
                }
            })
        })
        val totalEndTime = System.nanoTime() - totalStartTime
        println("there where " + survivorCount.toString + " surviving mutations, out of " + mutantCounter.toString + " mutations")
        println("average time to test a mutant: " + timeTemp/mutantCounter)
        println("total time to test all mutants: " + totalEndTime.toFloat / 1000000000)
    }
    private def mutateRoot(root: TypedAst.Root, testee: String): List[(Symbol.DefnSym, List[TypedAst.Def])] = {
        val defs = root.defs
        val defSyms = root.modules.filter(pair => (pair._1.toString.equals(testee))).values.toList.flatten
        println(defSyms)
        mutateDefs(defs, defSyms).flatten
    }


    private def mutateDefs(defs: Map[Symbol.DefnSym, TypedAst.Def], defSyms: List[Symbol]) = {
        defs.toList.map(d => (d._1, d._2) match {
            case (s, fun) =>
                if (defSyms.contains(s)) {
                    val mutExps = mutateExpr(fun.exp)
                    println(fun.exp)
                    println(mutExps)
                    val mutDefs = mutExps.map(mexp => fun.copy(exp = mexp))
                    Some(d._1 -> mutDefs)
                } else None
            case _ => None
        })
    }

    /**
        // var doesn't contain a subtree, and we don't mutate them so we don't need to return them
        case Expr.Var(_, _, _) => Nil
      */
    private def mutateExpr(e: TypedAst.Expr): List[TypedAst.Expr] = e match {
        case Expr.Cst(cst, tpe, loc) =>
            mutateCst(cst).map(m => Expr.Cst(m, tpe, loc))
        case Expr.Var(_, _, _) => Nil
        case Expr.Def(sym, tpe, loc) => Nil
        case Expr.Sig(sym, tpe, loc) => Nil
        case Expr.Hole(sym, tpe, loc) => Nil
        case Expr.HoleWithExp(exp, tpe, eff, loc) => Nil
        case Expr.OpenAs(symUse, exp, tpe, loc) => Nil
        case Expr.Use(sym, alias, exp, loc) => Nil
        case original@Expr.Lambda(fparam, exp, tpe, loc) =>
            mutateExpr(exp).map(m => original.copy(exp = m))

        case original@Expr.Apply(exp, exps, tpe, eff, loc) =>
            def helper(expList: List[Expr], index: Int) : List[Expr]= (expList, index) match {
                case (x::Nil, 0) => mutateExpr(x) ::: Nil
                case (x::xs, 0) => mutateExpr(x) ::: xs
                case (x::xs, n) =>  x::helper(xs, n-1)
                case (_ ,_) => Nil
            }
            var list: List[List[Expr]] = Nil
            for (i <- 0 until exps.length)
                list = helper(exps, i) :: list
            list.map(m => original.copy(exps = m))

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
        case Expr.LetRec(sym, ann, mod, exp1, exp2, tpe, eff, loc) => Nil
        case Expr.Region(tpe, loc) => Nil
        case Expr.Scope(sym, regionVar, exp, tpe, eff, loc) => Nil
        case original@Expr.IfThenElse(exp1, exp2, exp3, _, _, _) =>
            val mut1 = mutateExpr(exp1).map(m => original.copy(exp1 = m))
            val mut2 = mutateExpr(exp2).map(m => original.copy(exp2 = m))
            val mut3 = mutateExpr(exp3).map(m => original.copy(exp3 = m))
            mut1:::mut2:::mut3
        case Expr.Stm(exp1, exp2, tpe, eff, loc) => Nil
        case Expr.Discard(exp, eff, loc) => Nil
        case original@Expr.Match(_, rules, _, _, _) =>
            // refactor to permutation
            val permutations = rules.permutations
            val shuffle = permutations.map(p => original.copy(rules = p)).toList
            val ruleMute = rules.map(r => mutateMatchrule(r)).map(m => original.copy(rules = m))
            shuffle:::ruleMute
        case Expr.TypeMatch(exp, rules, tpe, eff, loc) => Nil
        case Expr.RestrictableChoose(star, exp, rules, tpe, eff, loc) => Nil
        case Expr.Tag(sym, exp, tpe, eff, loc) => Nil
        case Expr.RestrictableTag(sym, exp, tpe, eff, loc) => Nil
        case original@Expr.Tuple(elms, _, _, _) =>
            val mutateElms = elms.map(e => mutateExpr(e))
            mutateElms.map(m => original.copy(elms = m))
        case Expr.RecordEmpty(tpe, loc) => Nil
        case Expr.RecordSelect(exp, label, tpe, eff, loc) => Nil
        case Expr.RecordExtend(label, exp1, exp2, tpe, eff, loc) => Nil
        case Expr.RecordRestrict(label, exp, tpe, eff, loc) => Nil
        case Expr.ArrayLit(exps, exp, tpe, eff, loc) => Nil
        case Expr.ArrayNew(exp1, exp2, exp3, tpe, eff, loc) => Nil
        case Expr.ArrayLoad(exp1, exp2, _, _, _) => Nil

        case Expr.ArrayLength(exp, eff, loc) => Nil
        case Expr.ArrayStore(exp1, exp2, exp3, eff, loc) => Nil
        case Expr.VectorLit(exps, tpe, eff, loc) => Nil
        case Expr.VectorLoad(exp1, exp2, tpe, eff, loc) => Nil
        case Expr.VectorLength(exp, loc) => Nil
        case Expr.Ref(exp1, exp2, tpe, eff, loc) => Nil
        case Expr.Deref(exp, tpe, eff, loc) => Nil
        case Expr.Assign(exp1, exp2, tpe, eff, loc) => Nil
        case Expr.Ascribe(exp, tpe, eff, loc) => Nil
        case Expr.InstanceOf(exp, clazz, loc) => Nil
        case Expr.CheckedCast(cast, exp, tpe, eff, loc) => Nil
        case Expr.UncheckedCast(exp, declaredType, declaredEff, tpe, eff, loc) => Nil
        case Expr.UncheckedMaskingCast(exp, tpe, eff, loc) => Nil
        case Expr.Without(exp, effUse, tpe, eff, loc) => Nil
        case Expr.TryCatch(exp, rules, tpe, eff, loc) => Nil
        case Expr.TryWith(exp, effUse, rules, tpe, eff, loc) => Nil
        case Expr.Do(op, exps, tpe, eff, loc) => Nil
        case Expr.InvokeConstructor(constructor, exps, tpe, eff, loc) => Nil
        case Expr.InvokeMethod(method, exp, exps, tpe, eff, loc) => Nil
        case Expr.InvokeStaticMethod(method, exps, tpe, eff, loc) => Nil
        case Expr.GetField(field, exp, tpe, eff, loc) => Nil
        case Expr.PutField(field, exp1, exp2, tpe, eff, loc) => Nil
        case Expr.GetStaticField(field, tpe, eff, loc) => Nil
        case Expr.PutStaticField(field, exp, tpe, eff, loc) => Nil
        case Expr.NewObject(name, clazz, tpe, eff, methods, loc) => Nil
        case Expr.NewChannel(exp1, exp2, tpe, eff, loc) => Nil
        case Expr.GetChannel(exp, tpe, eff, loc) => Nil
        case Expr.PutChannel(exp1, exp2, tpe, eff, loc) => Nil
        case Expr.SelectChannel(rules, default, tpe, eff, loc) => Nil
        case Expr.Spawn(exp1, exp2, tpe, eff, loc) => Nil
        case Expr.ParYield(frags, exp, tpe, eff, loc) => Nil
        case Expr.Lazy(exp, tpe, loc) => Nil
        case Expr.Force(exp, tpe, eff, loc) => Nil
        case Expr.FixpointConstraintSet(cs, tpe, loc) => Nil
        case Expr.FixpointLambda(pparams, exp, tpe, eff, loc) => Nil
        case Expr.FixpointMerge(exp1, exp2, tpe, eff, loc) => Nil
        case Expr.FixpointSolve(exp, tpe, eff, loc) => Nil
        case Expr.FixpointFilter(pred, exp, tpe, eff, loc) => Nil
        case Expr.FixpointInject(exp, pred, tpe, eff, loc) => Nil
        case Expr.FixpointProject(pred, exp, tpe, eff, loc) => Nil
        case Expr.Error(m, tpe, eff) => Nil
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
                case Int8Op.Neg =>Int8Op.Neg
                case Int8Op.Not =>Int8Op.Not
                case Int8Op.Add =>Int8Op.Sub
                case Int8Op.Sub =>Int8Op.Add
                case Int8Op.Mul =>Int8Op.Div
                case Int8Op.Div =>Int8Op.Mul
                case Int8Op.Rem =>Int8Op.Div
                case Int8Op.Exp =>Int8Op.Mul
                case Int8Op.And =>Int8Op.Or
                case Int8Op.Or => Int8Op.And
                case Int8Op.Xor =>Int8Op.Or
                case Int8Op.Shl =>Int8Op.Shr
                case Int8Op.Shr =>Int8Op.Shl
                case Int8Op.Eq => Int8Op.Neq
                case Int8Op.Neq =>Int8Op.Eq
                case Int8Op.Lt => Int8Op.Le
                case Int8Op.Le => Int8Op.Lt
                case Int8Op.Gt => Int8Op.Ge
                case Int8Op.Ge => Int8Op.Gt
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
                case Int64Op.Neg =>Int64Op.Neg
                case Int64Op.Not =>Int64Op.Not
                case Int64Op.Add =>Int64Op.Sub
                case Int64Op.Sub =>Int64Op.Add
                case Int64Op.Mul =>Int64Op.Div
                case Int64Op.Div =>Int64Op.Mul
                case Int64Op.Rem =>Int64Op.Div
                case Int64Op.Exp =>Int64Op.Mul
                case Int64Op.And =>Int64Op.Or
                case Int64Op.Or => Int64Op.And
                case Int64Op.Xor =>Int64Op.Or
                case Int64Op.Shl =>Int64Op.Shr
                case Int64Op.Shr =>Int64Op.Shl
                case Int64Op.Eq => Int64Op.Neq
                case Int64Op.Neq =>Int64Op.Eq
                case Int64Op.Lt => Int64Op.Le
                case Int64Op.Le => Int64Op.Lt
                case Int64Op.Gt => Int64Op.Ge
                case Int64Op.Ge => Int64Op.Gt
            }
            case op: SemanticOp.StringOp => op match {
                case StringOp.Concat => StringOp.Concat
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
            case Constant.Regex(lit) => Constant.Regex(java.util.regex.Pattern.compile("a")) :: Nil
        }
    }

}

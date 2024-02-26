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
            |    pub def one(): Int32 = 1
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
        println("time to generate mutations: " + end.toString)
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
            val defs = root.defs
            mutantCounter += mut._2.length
            mut._2.foreach(mDef => {
                val start = System.nanoTime()
                val n = defs + (mut._1 -> mDef)
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
                    val mutDefs = mutExps.map(mexp => fun.copy(exp = mexp))
                    Some(d._1 -> mutDefs)
                } else None
            case _ => None
        })
    }

    private def mutateExpr(e: TypedAst.Expr): List[TypedAst.Expr] =
        e match {
            case Expr.Cst(cst, tpe, loc) =>
                mutateCst(cst).map(m => Expr.Cst(m, tpe, loc))
            case Expr.Binary(sop, exp1, exp2, typ, eff, loc) =>
                val mut1 = Expr.Binary(mutateSop(sop), exp1, exp2, typ, eff, loc)
                val mut2 = mutateExpr(exp1).map(m => Expr.Binary(sop, m, exp2, typ, eff, loc))
                val mut3 = mutateExpr(exp2).map(m => Expr.Binary(sop, exp1, m, typ, eff, loc))
                mut1::mut2:::mut3
            case original@Expr.IfThenElse(exp1, exp2, exp3,_, _, _) =>
                val mut1 = mutateExpr(exp1).map(m => original.copy(exp1 = m))
                val mut2 = mutateExpr(exp2).map(m => original.copy(exp2 = m))
                val mut3 = mutateExpr(exp3).map(m => original.copy(exp3 = m))
                mut1:::mut2:::mut3
            // var doesn't contain a subtree, and we don't mutate them so we don't need to return them
            case Expr.Var(_, _, _) => Nil
            case original@Expr.Match(_, rules, _, _, _) =>
                rules.map(r => mutateMatchrule(r)).map(m => original.copy(rules = m))

            case e => List(e)
        }


    private def mutateMatchrule(mr: TypedAst.MatchRule): List[TypedAst.MatchRule] = {
        val mutExps = mutateExpr(mr.exp).map(m => mr.copy(exp = m))
        mutatePattern(mr.pat).foldLeft(mutExps)((acc, m) => mr.copy(pat = m)::acc)

    }
    private def mutatePattern(pattern: TypedAst.Pattern): List[TypedAst.Pattern] = {
        pattern match {
            case original@TypedAst.Pattern.Cst(cst, _, _) => mutateCst(cst).map(m => original.copy(m))
            case e => List(e)
        }
    }
    private def mutateSop(sop: SemanticOp): SemanticOp = {
        sop match {
            case op: SemanticOp.BoolOp => op match {
                case BoolOp.Not => BoolOp.Not
                case BoolOp.And => BoolOp.Or
                case BoolOp.Or => BoolOp.And
                case BoolOp.Eq => BoolOp.Neq
                case BoolOp.Neq => BoolOp.Eq
            }
            case op: SemanticOp.CharOp => op match {
                case CharOp.Eq => CharOp.Neq
                case CharOp.Neq => CharOp.Eq
                case CharOp.Lt => CharOp.Le
                case CharOp.Le => CharOp.Lt
                case CharOp.Gt => CharOp.Ge
                case CharOp.Ge => CharOp.Gt
            }
            case op: SemanticOp.Float32Op => op match {
                case Float32Op.Neg => Float32Op.Neg
                case Float32Op.Add => Float32Op.Sub
                case Float32Op.Sub => Float32Op.Add
                case Float32Op.Mul => Float32Op.Div
                case Float32Op.Div => Float32Op.Mul
                case Float32Op.Exp => Float32Op.Mul
                case Float32Op.Eq => Float32Op.Neq
                case Float32Op.Neq => Float32Op.Eq
                case Float32Op.Lt => Float32Op.Le
                case Float32Op.Le => Float32Op.Lt
                case Float32Op.Gt => Float32Op.Ge
                case Float32Op.Ge => Float32Op.Gt
            }
            case op: SemanticOp.Float64Op => op match {
                case Float64Op.Neg => Float64Op.Neg
                case Float64Op.Add => Float64Op.Sub
                case Float64Op.Sub => Float64Op.Add
                case Float64Op.Mul => Float64Op.Div
                case Float64Op.Div => Float64Op.Mul
                case Float64Op.Exp =>Float64Op.Mul
                case Float64Op.Eq => Float64Op.Neq
                case Float64Op.Neq =>Float64Op.Eq
                case Float64Op.Lt => Float64Op.Le
                case Float64Op.Le => Float64Op.Lt
                case Float64Op.Gt => Float64Op.Ge
                case Float64Op.Ge => Float64Op.Gt
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
                case Int16Op.Neg =>Int16Op.Neg
                case Int16Op.Not =>Int16Op.Not
                case Int16Op.Add =>Int16Op.Sub
                case Int16Op.Sub =>Int16Op.Add
                case Int16Op.Mul =>Int16Op.Div
                case Int16Op.Div =>Int16Op.Mul
                case Int16Op.Rem =>Int16Op.Div
                case Int16Op.Exp =>Int16Op.Mul
                case Int16Op.And =>Int16Op.Or
                case Int16Op.Or => Int16Op.And
                case Int16Op.Xor =>Int16Op.Or
                case Int16Op.Shl =>Int16Op.Shr
                case Int16Op.Shr =>Int16Op.Shl
                case Int16Op.Eq => Int16Op.Neq
                case Int16Op.Neq =>Int16Op.Eq
                case Int16Op.Lt => Int16Op.Le
                case Int16Op.Le => Int16Op.Lt
                case Int16Op.Gt => Int16Op.Ge
                case Int16Op.Ge => Int16Op.Gt
            }
            case op: SemanticOp.Int32Op => op match {
                case Int32Op.Neg =>Int32Op.Neg
                case Int32Op.Not =>Int32Op.Not
                case Int32Op.Add =>Int32Op.Sub
                case Int32Op.Sub =>Int32Op.Add
                case Int32Op.Mul =>Int32Op.Div
                case Int32Op.Div =>Int32Op.Mul
                case Int32Op.Rem =>Int32Op.Div
                case Int32Op.Exp =>Int32Op.Mul
                case Int32Op.And =>Int32Op.Or
                case Int32Op.Or => Int32Op.And
                case Int32Op.Xor =>Int32Op.Or
                case Int32Op.Shl =>Int32Op.Shr
                case Int32Op.Shr =>Int32Op.Shl
                case Int32Op.Eq => Int32Op.Neq
                case Int32Op.Neq =>Int32Op.Eq
                case Int32Op.Lt => Int32Op.Le
                case Int32Op.Le => Int32Op.Lt
                case Int32Op.Gt => Int32Op.Ge
                case Int32Op.Ge => Int32Op.Gt
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

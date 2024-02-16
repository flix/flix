package ca.uwaterloo.flix

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.Ast.Constant
import ca.uwaterloo.flix.language.ast.{Ast, TypedAst}
import ca.uwaterloo.flix.language.ast.TypedAst.Expr
import ca.uwaterloo.flix.util._

import java.io.File

object MutationTester {
    def run(files: Seq[File], options: Options, tester: String, testee: String): Unit = {
        val flix = new Flix
        flix.setOptions(options)
        flix.addSourceCode("testee",
            s"""
            |def main(): Unit \\ IO =
            |    println(Test.one())
            |
            |mod Test {
            |    pub def one(): Int32 = 1
            |}""".stripMargin)

        flix.addSourceCode("tester",
            s"""
            |mod MainTest {
            |    @test
            |    def testConstant(): Bool =
            |        Assert.eq(Test.one(), 1)
            |}""".stripMargin)
        val root = flix.check().unsafeGet
        val root1 = mutate(root, testee)
        val res = flix.codeGen(root1).unsafeGet.getTests
        println(res)
        /**
        val result = root1.map(r => flix.codeGen(r).unsafeGet)
        val tests = result.map(res => res.getTests)
        */
    }

    def mutate(root: TypedAst.Root, testee: String): TypedAst.Root = {
        val defs = root.defs
        //defs.toList.map(t => mutateExpr(t._2.exp))
        val defSyms = root.modules.filter(pair => (pair._1.toString.equals(testee))).values.toList.flatten
        println(defSyms)
        val newDefs = defs.map(d => (d._1,d._2) match {
            case (s, fun) =>
                if (defSyms.contains(s)) {
                    println(d._2)
                    val mut = TypedAst.Def(fun.sym, fun.spec, mutateExpr(fun.exp))
                    println(mut)
                    d._1 -> mut

                } else d
            case _ => d
        })
        root.copy(defs = newDefs)
    }

    def mutateExpr(e: TypedAst.Expr): TypedAst.Expr = {
        e match {
            case Expr.Cst(cst, tpe, loc) => Expr.Cst(mutateCst(cst), tpe, loc)
            case _ => null// do nothing
        }
    }

    def mutateCst(cst: Ast.Constant): Ast.Constant = {
        cst match {
            case Constant.Unit => Constant.Unit
            case Constant.Null => Constant.Null
            case Constant.Bool(lit) => Constant.Bool(!lit)
            case Constant.Char(lit) => Constant.Char((lit^Char.MaxValue).toChar)
            case Constant.Float32(lit) => Constant.Float32(lit + 1)
            case Constant.Float64(lit) => Constant.Float64(lit + 1)
            case Constant.BigDecimal(lit) => Constant.BigDecimal(lit.add(java.math.BigDecimal.ONE))
            case Constant.Int8(lit) => Constant.Int8(lit.+(1).toByte)
            case Constant.Int16(lit) => Constant.Int16(lit.+(1).toShort)
            case Constant.Int32(lit) => Constant.Int32(lit + 1)
            case Constant.Int64(lit) => Constant.Int64(lit + 1)
            case Constant.BigInt(lit) => Constant.BigInt(lit.add(lit))
            case Constant.Str(lit) => Constant.Str("")
            case Constant.Regex(lit) => Constant.Regex(java.util.regex.Pattern.compile("a"))
        }
    }

}

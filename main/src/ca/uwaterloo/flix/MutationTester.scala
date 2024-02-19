package ca.uwaterloo.flix

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.Ast.Constant
import ca.uwaterloo.flix.language.ast.{Ast, Symbol, TypedAst}
import ca.uwaterloo.flix.language.ast.TypedAst.Expr
import ca.uwaterloo.flix.tools.Tester
import ca.uwaterloo.flix.util._
import org.json4s.reflect.fail

import java.io.File


object MutationTester {
    def run(files: Seq[File], options: Options, tester: String, testee: String): Unit = {
        val flix = new Flix
        flix.setOptions(options)
        flix.addSourceCode("testee",
            s"""
            |def main(): Unit \\ IO =
            |    println(Test.one())
            |def two(): Int32 = 2
            |mod Test {
            |    pub def one(): Int32 = 1
            |    pub def ite(b: Bool): Int32 = if(b) 1 else 2
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
            |}""".stripMargin)
        val root = flix.check().unsafeGet
        val root1 = mutate(root, testee)
        runMutations(flix, root, root1)
        /**
        val result = root1.map(r => flix.codeGen(r).unsafeGet)
        val tests = result.map(res => res.getTests)
        */
    }

    def runMutations(flix: Flix, root: TypedAst.Root, mutatedDefs: List[(Symbol.DefnSym, List[TypedAst.Def])]): Unit = {
        mutatedDefs.foreach(mut => {
            val defs = root.defs
            mut._2.foreach(mDef => {
                val n = defs + (mut._1 -> mDef)
                val newRoot = root.copy(defs = n)
                val cRes = flix.codeGen(newRoot).unsafeGet
                Tester.run(Nil, cRes)(flix)
            })
        })
    }
    def mutate(root: TypedAst.Root, testee: String): List[(Symbol.DefnSym, List[TypedAst.Def])] = {
        val defs = root.defs
        //defs.toList.map(t => mutateExpr(t._2.exp))
        val defSyms = root.modules.filter(pair => (pair._1.toString.equals(testee))).values.toList.flatten
        println(defSyms)
        val mutatedDefs: List[Option[(Symbol.DefnSym, List[TypedAst.Def])]] = defs.toList.map(d => (d._1,d._2) match {
            case (s, fun) =>
                if (defSyms.contains(s)) {
                    println("before mutatiom", d._2)
                    val mutExps = mutateExpr(fun.exp)
                    val mutDefs = mutExps.map(mexp => TypedAst.Def(fun.sym, fun.spec, mexp))
                    println("after mutation", mutExps)
                    Some(d._1 -> mutDefs)
                } else None
            case _ => None
        })
        mutatedDefs.flatten
        /**
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
        */
    }


    def mutateExpr(e: TypedAst.Expr): List[TypedAst.Expr] = {
        e match {
            case Expr.Cst(cst, tpe, loc) => Expr.Cst(mutateCst(cst), tpe, loc) :: Nil
            case Expr.IfThenElse(exp1, exp2, exp3,typ, eff, loc) =>
                val mut1 = mutateExpr(exp1).map(m => Expr.IfThenElse(m, exp2, exp3, typ, eff, loc))
                val mut2 = mutateExpr(exp2).map(m => Expr.IfThenElse(exp1, m, exp3, typ, eff, loc))
                val mut3 = mutateExpr(exp3).map(m => Expr.IfThenElse(exp1, exp2, m, typ, eff, loc))
                mut1:::mut2:::mut3
            case Expr.Var(_, _, _) => Nil
            case e => fail(e.toString + " not implemented")// do nothing
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

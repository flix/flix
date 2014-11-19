import impl.ast2.{Ast, Parsing}
import org.scalatest.FunSuite
import scala.collection.immutable.Seq

class TestParser extends FunSuite {

  test("Parser.Namespace01") {
    val s = """namespace a {};"""
    //val a = Ast.NameSpace(Ast.SimpleName("a"), Seq.empty[Ast.Declaration])

    //assertResult(a)(Parsing.parse(s))
  }

  test("Parser.Namespace02") {
    val s = """namespace a.b {};"""
    //val a = Ast.NameSpace(Ast.SimpleName("a"), Seq.empty[Ast.Declaration])

    //assertResult(a)(Parsing.parse(s))
  }


  test("Parser.Type01") {
    val s = """type t = Bool;"""
    val a = Ast.Declaration.TypeDecl("t", Ast.Type.Bool)

    //assertResult(a)(Parsing.parse(s))
  }


}

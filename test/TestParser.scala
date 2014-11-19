import impl.ast2.{Ast, Parsing}
import org.scalatest.FunSuite
import scala.collection.immutable.Seq

class TestParser extends FunSuite {

  test("Parser.Type01") {
    val s = """type t = Unit;"""
    val a = Ast.Root(Seq(Ast.Declaration.TypeDecl("t", Ast.Type.Unit)))

    assertResult(a)(Parsing.parse(s))
  }

  test("Parser.Type02") {
    val s = """type t = Bool;"""
    val a = Ast.Root(Seq(Ast.Declaration.TypeDecl("t", Ast.Type.Bool)))

    assertResult(a)(Parsing.parse(s))
  }

  test("Parser.Type03") {
    val s = """type t = Int;"""
    val a = Ast.Root(Seq(Ast.Declaration.TypeDecl("t", Ast.Type.Int)))

    assertResult(a)(Parsing.parse(s))
  }

  test("Parser.Type04") {
    val s = """type t = Str;"""
    val a = Ast.Root(Seq(Ast.Declaration.TypeDecl("t", Ast.Type.Str)))

    assertResult(a)(Parsing.parse(s))
  }

}

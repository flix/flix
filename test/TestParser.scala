import impl.ast2.{Ast, Parsing}
import org.scalatest.FunSuite
import scala.collection.immutable.Seq

class TestParser extends FunSuite {

  test("Parser.Type01") {
    val s = """type t = Bool;"""
    val a = Ast.Root(Seq(Ast.Declaration.TypeDecl("t", Ast.Type.Bool)))

    assertResult(a)(Parsing.parse(s))
  }
}

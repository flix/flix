import impl.ast2.{Ast, Parsing}
import org.scalatest.FunSuite

class TestParser extends FunSuite {

  test("Parser.Type01") {
    val s = """type t = Bool;"""
    val t = Ast.TypeDeclaration("t", Ast.Type.Bool)

    assertResult(t)(Parsing.parse(s))
  }


}

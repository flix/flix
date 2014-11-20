import impl.ast2.{Ast, Parser}
import org.scalatest.FunSuite
import scala.collection.immutable.Seq

class TestParser extends FunSuite {

  test("Parser.Type.Unit") {
    val s = """type t = Unit;"""
    val a = Ast.Type.Unit

    assertResult(a)(getType(Parser.parse(s)))
  }

  test("Parser.Type.Bool") {
    val s = """type t = Bool;"""
    val t = Ast.Type.Bool

    assertResult(t)(getType(Parser.parse(s)))
  }

  test("Parser.Type.Int") {
    val s = """type t = Int;"""
    val t = Ast.Type.Int

    assertResult(t)(getType(Parser.parse(s)))
  }

  test("Parser.Type.Str") {
    val s = """type t = Str;"""
    val t = Ast.Type.Str

    assertResult(t)(getType(Parser.parse(s)))
  }

  test("Parser.Type.Tuple01") {
    val s = """type t = (Bool, Bool);"""
    val t = Ast.Type.Tuple(Seq(Ast.Type.Bool, Ast.Type.Bool))

    assertResult(t)(getType(Parser.parse(s)))
  }

  test("Parser.Type.Tuple02") {
    val s = """type t = (Bool, Bool, Bool);"""
    val t = Ast.Type.Tuple(Seq(Ast.Type.Bool, Ast.Type.Bool, Ast.Type.Bool))

    assertResult(t)(getType(Parser.parse(s)))
  }

  test("Parser.Type.Tuple03") {
    val s = """type t = (Bool, Int, Str);"""
    val t = Ast.Type.Tuple(Seq(Ast.Type.Bool, Ast.Type.Int, Ast.Type.Str))

    assertResult(t)(getType(Parser.parse(s)))
  }

  test("Parser.Type.Tuple04") {
    val s = """type t = (Unit, Bool, Int, Str, Unit, Bool, Int, Str);"""
    val t = Ast.Type.Tuple(Seq(Ast.Type.Unit, Ast.Type.Bool, Ast.Type.Int, Ast.Type.Str, Ast.Type.Unit, Ast.Type.Bool, Ast.Type.Int, Ast.Type.Str))

    assertResult(t)(getType(Parser.parse(s)))
  }

  test("Parser.Type.Tuple05") {
    val s = """type t = (Unit);"""
    val t = Ast.Type.Tuple(Seq(Ast.Type.Unit))

    assertResult(t)(getType(Parser.parse(s)))
  }

  test("Parser.Type.Set01") {
    val s = """type t = Set[Unit];"""
    val t = Ast.Type.Set(Ast.Type.Unit)

    assertResult(t)(getType(Parser.parse(s)))
  }

  test("Parser.Type.Set02") {
    val s = """type t = Set[Bool];"""
    val t = Ast.Type.Set(Ast.Type.Bool)

    assertResult(t)(getType(Parser.parse(s)))
  }

  test("Parser.Type.Set03") {
    val s = """type t = Set[Int];"""
    val t = Ast.Type.Set(Ast.Type.Int)

    assertResult(t)(getType(Parser.parse(s)))
  }

  test("Parser.Type.Set04") {
    val s = """type t = Set[Set[Int]];"""
    val t = Ast.Type.Set(Ast.Type.Set(Ast.Type.Int))

    assertResult(t)(getType(Parser.parse(s)))
  }


  test("Parser.Type.Map01") {
    val s = """type t = Map[Bool, Bool];"""
    val t = Ast.Type.Map(Ast.Type.Bool, Ast.Type.Bool)

    assertResult(t)(getType(Parser.parse(s)))
  }

  test("Parser.Type.Map02") {
    val s = """type t = Map[Int, Int];"""
    val t = Ast.Type.Map(Ast.Type.Int, Ast.Type.Int)

    assertResult(t)(getType(Parser.parse(s)))
  }

  test("Parser.Type.Map03") {
    val s = """type t = Map[Int, Map[Int, Int]];"""
    val t = Ast.Type.Map(Ast.Type.Int, Ast.Type.Map(Ast.Type.Int, Ast.Type.Int))

    assertResult(t)(getType(Parser.parse(s)))
  }

  test("Parser.Type.Map04") {
    val s = """type t = Map[Bool, Map[Int, Str]];"""
    val t = Ast.Type.Map(Ast.Type.Bool, Ast.Type.Map(Ast.Type.Int, Ast.Type.Str))

    assertResult(t)(getType(Parser.parse(s)))
  }

  test("Parser.Type.Map05") {
    val s = """type t = Map[Map[Bool, Int], Map[Int, Bool]];"""
    val t = Ast.Type.Map(Ast.Type.Bool, Ast.Type.Map(Ast.Type.Int, Ast.Type.Str))

    assertResult(t)(getType(Parser.parse(s)))
  }

  private def getType(root: Ast.Root): Ast.Type = root match {
    case Ast.Root(Seq(Ast.Declaration.TypeDecl(_, typ))) => typ
    case _ => throw new RuntimeException()
  }

}

package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.TestUtils
import ca.uwaterloo.flix.language.CompilationMessage
import ca.uwaterloo.flix.language.ast.TypedAst
import ca.uwaterloo.flix.language.errors.ParseError
import ca.uwaterloo.flix.util.Options
import org.scalatest.funsuite.AnyFunSuite

/**
  * Recover tests are cases where we would like to check that the parser recovers from a syntax error.
  * Each test contains one or more syntax error.
  * The parser should understand all the surrounding code but still produce a parse error for the syntax mistake.
  * That is asserted by checking that main is defined in the compilation result.
  * There is an theoretically infinite amount of possible syntax errors (we could just start fuzzing random strings),
  * so these test cover some "sensible amount" of broad errors.
  * Some areas that could use more test are:
  *   - Declarations other than definitions (module, enum, trait, signature, effect).
  *   - Patterns
  *   - Map, Set, List, Vector and Array literals.
  *   - "Niche" expressions (OpenAs, JVMops, Fixpoint expressions).
  */
class TestParserRecovery extends AnyFunSuite with TestUtils {

  test("UnterminatedInfixFunction.01") {
    val input = "1 `add 2"
    val result = check(input, Options.TestWithLibNix)
    expectError[ParseError](result)
  }

  test("UnterminatedInfixFunction.02") {
    val input = "1 `add/*this is a block comment*/` 2"
    val result = check(input, Options.TestWithLibNix)
    expectError[ParseError](result)
  }

  test("UnterminatedInfixFunction.03") {
    val input = "1 `add 2"
    val result = check(input, Options.TestWithLibNix)
    expectError[ParseError](result)
  }

  test("TrailingComma.01") {
    val input =
      """
        |def b(): List[Int32] = List#{2, /**/}
        |def main(): Unit = ()
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[ParseError](result)
    expectMain(result)
  }

  test("IllegalDefName.01") {
    val input =
      """
        |def A(): Unit = ()
        |def main(): Unit = ()
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[ParseError](result)
    expectMain(result)
  }

  test("IllegalDefName.02") {
    val input =
      """
        |pub trait A[a] {
        |    pub def A(): Unit = ()
        |}
        |def main(): Unit = ()
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[ParseError](result)
    expectMain(result)
  }

  test("IllegalDefName.03") {
    val input =
      """
        |trait A[x] {}
        |instance A[Int32] {
        |    pub def A(): Unit = ()
        |}
        |def main(): Unit = ()
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[ParseError](result)
    expectMain(result)
  }

  test("IllegalDefName.04") {
    val input =
      """
        |trait A[x] {}
        |instance A[Int32] {
        |    pub redef A(): Unit = ()
        |}
        |def main(): Unit = ()
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[ParseError](result)
    expectMain(result)
  }

  test("IllegalDefName.05") {
    val input =
      """
        |trait B[a] {
        |    law A:forall() false
        |}
        |def main(): Unit = ()
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[ParseError](result)
    expectMain(result)
  }

  test("IllegalDefName.06") {
    val input =
      """
        |pub eff C {
        |    def A(): Unit
        |}
        |def main(): Unit = ()
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[ParseError](result)
    expectMain(result)
  }

  test("IllegalDefName.07") {
    val input =
      """
        |def a(): Unit = {
        |    def A() = ();
        |    A()
        |}
        |def main(): Unit = ()
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[ParseError](result)
    expectMain(result)
  }

  test("Use.01") {
    val input =
      """
        |use Color.{Red;
        |enum Color { case Red }
        |def main(): Unit = ()
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[ParseError](result)
    expectMain(result)
  }

  test("Use.02") {
    val input =
      """
        |use Color.{Red =>
        |enum Color { case Red }
        |def main(): Unit = ()
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[ParseError](result)
    expectMain(result)
  }

  test("Use.03") {
    val input =
      """
        |use Color.{Red => ScarletRed;
        |enum Color { case Red }
        |def main(): Unit = ()
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[ParseError](result)
    expectMain(result)
  }

  test("Import.01") {
    val input =
      """
        |import java.lang.{StringBuffer,
        |def main(): Unit = ()
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[ParseError](result)
    expectMain(result)
  }

  test("Import.02") {
    val input =
      """
        |import java.lang.{StringBuffer => StrBuf
        |def main(): Unit = ()
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[ParseError](result)
    expectMain(result)
  }

  test("Import.03") {
    val input =
      """
        |import java.lang.{StringBuffer, , CharSequence};
        |def main(): Unit = ()
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[ParseError](result)
    expectMain(result)
  }

  test("Parameters.01") {
    val input =
      """
        |def foo(x: Int32, , z: Int32): Int32 = ???
        |def main(): Unit = ()
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[ParseError](result)
    expectMain(result)
  }

  test("Parameters.02") {
    val input =
      """
        |def foo(x: Int32,
        |def main(): Unit = ()
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[ParseError](result)
    expectMain(result)
  }

  test("StructNoTParams.01") {
    val input =
      """
        |struct S { }
        |def main(): Unit = ()
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[ParseError](result)
    expectMain(result)
  }

  test("DefNoParams.01") {
    val input =
      """
        |def main: Unit = ()
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[ParseError](result)
    expectMain(result)
  }

  test("DefNoParams.02") {
    val input =
      """
        |trait A[a] {
        |def someSig: Unit
        |}
        |def main(): Unit = ()
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[ParseError](result)
    expectMain(result)
  }

  test("DefNoParams.03") {
    val input =
      """
        |def main(): Unit = {
        |    def localFunc = ();
        |    localFunc()
        |}
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[ParseError](result)
    expectMain(result)
  }

  test("NoLiteralBody.01") {
    val input =
      """
        |def main(): Unit = {
        |    let _ = List#;
        |    ()
        |}
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[ParseError](result)
    expectMain(result)
  }


  test("NoDefBody.01") {
    val input =
      """
        |def foo(x: Int32): Int32
        |def main(): Unit = ()
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[ParseError](result)
    expectMain(result)
  }

  test("NoDefType.01") {
    val input =
      """
        |def foo(x: Int32) = ???
        |def main(): Unit = ()
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[ParseError](result)
    expectMain(result)
  }

  test("LeadOnCurlyR.01") {
    val input =
      """
        |} def main(): Unit = ()
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[ParseError](result)
    expectMain(result)
  }

  test("DanglingAnnotation.01") {
    val input =
      """
        |def main(): Unit = ()
        |@Internal
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[ParseError](result)
    expectMain(result)
  }

  test("DanglingModifier.01") {
    val input =
      """
        |def main(): Unit = ()
        |pub
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[ParseError](result)
    expectMain(result)
  }

  test("DanglingDocComment.01") {
    val input =
      """
        |def main(): Unit = ()
        |/// This documents nothing
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[ParseError](result)
    expectMain(result)
  }

  test("DanglingDocComment.02") {
    val input =
      """
        |mod Foo {
        |  /// This documents nothing
        |}
        |def main(): Unit = ()
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[ParseError](result)
    expectMain(result)
  }

  test("DanglingDocComment.03") {
    val input =
      """
        |trait Foo[t] {
        |  /// This documents nothing
        |}
        |def main(): Unit = ()
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[ParseError](result)
    expectMain(result)
  }

  test("DanglingDocComment.04") {
    val input =
      """
        |trait Test[t] {}
        |instance Test[Int32] {
        |  /// This documents nothing
        |}
        |def main(): Unit = ()
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[ParseError](result)
    expectMain(result)
  }

  test("DanglingDocComment.05") {
    val input =
      """
        |eff MyEff {
        |  /// This documents nothing
        |}
        |def main(): Unit = ()
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[ParseError](result)
    expectMain(result)
  }

  test("MangledModule.02") {
    val input =
      """
        |mod Bar {
        |    legumes provide healthy access to proteins
        |    pub def foo(): Int32 = 123
        |    /// This is not quite finished
        |    pub def
        |}
        |def main(): Int32 = Bar.foo()
        |
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[ParseError](result)
    expectMain(result)
  }

  test("BadTrait.01") {
    val input =
      """
        |trait DoesNothing[a
        |  pub def noop(x: a): Unit = ???
        |}
        |def main(): Unit = ()
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[ParseError](result)
    expectMain(result)
  }

  test("BadEnum.01") {
    val input =
      """
        |enum Legumes { Chickpea, Beans }
        |def main(): Unit = ()
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[ParseError](result)
    expectMain(result)
  }

  test("BadEnum.02") {
    val input =
      """
        |enum Legumes[a { case Chickpea(a), case Beans }
        |def main(): Unit = ()
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[ParseError](result)
    expectMain(result)
  }

  test("BadEnum.03") {
    val input =
      """
        |enum USD[a](a
        |def main(): Unit = ()
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[ParseError](result)
    expectMain(result)
  }

  test("BadEnum.04") {
    val input =
      """
        |// A Suit type deriving an Eq and ToString instance
        |enum Suit with Eq, ToString {
        |    case
        |    //case Clubs
        |    //case Hearts
        |    //case Spades
        |    //case Diamonds
        |}
        |def main(): Unit = ()
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[ParseError](result)
    expectMain(result)
  }

  test("TypeAlias.01") {
    val input =
      """
        |type alias M[k, = Map[k, Result[String, k]]
        |def main(): Unit = ()
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[ParseError](result)
    expectMain(result)
  }

  test("TypeAlias.02") {
    val input =
      """
        |type alias M[k] = Map[k, Result[String, ]
        |def main(): Unit = ()
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[ParseError](result)
    expectMain(result)
  }

  test("TypeAlias.03") {
    val input =
      """
        |type alias Magic
        |def main(): Unit = ()
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[ParseError](result)
    expectMain(result)
  }

  test("TypeAlias.04") {
    val input =
      """
        |type alias Magic =
        |def main(): Unit = ()
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[ParseError](result)
    expectMain(result)
  }

  test("BadCallExpr.01") {
    val input =
      """
        |def foo(x: Int32, y: Int32): Int32 = bar(1,
        |// Note: We need an enum here. If we just had main, it would get consumed as a LetRecDef.
        |enum Legumes { case ChickPea, Beans }
        |def main(): Unit = ()
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[ParseError](result)
    expectMain(result)
  }

  test("BadCall.02") {
    val input =
      """
        |def foo(x: Int32, y: Int32): Int32 = {
        |  bar( 1 + ;
        |  let x =
        |}
        |def main(): Unit = ()
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[ParseError](result)
    expectMain(result)
  }

  test("BadQualifiedPath.01") {
    val input =
      """
        |def foo(): In32 = Bar.
        |def main(): Unit = ()
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[ParseError](result)
    expectMain(result)
  }

  test("BadRecordSelect.01") {
    val input =
      """
        |def foo(): In32 = bar().
        |def main(): Unit = ()
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[ParseError](result)
    expectMain(result)
  }

  test("BadBinaryOperation.01") {
    val input =
      """
        |def foo(): Int32 = { 1 + }
        |def main(): Unit = ()
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[ParseError](result)
    expectMain(result)
  }

  test("BadBinaryOperation.02") {
    val input =
      """
        |def foo(): Int32 = # 2
        |def main(): Unit = ()
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[ParseError](result)
    expectMain(result)
  }


  test("BadLambda.01") {
    val input =
      """
        |def foo(): Int32 = { () -> ; 1 }
        |def main(): Unit = ()
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[ParseError](result)
    expectMain(result)
  }

  test("BadLambda.02") {
    val input =
      """
        |def foo(): Int32 = { (a, ) -> ; 1 }
        |def main(): Unit = ()
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[ParseError](result)
    expectMain(result)
  }

  test("BadTuple.01") {
    val input =
      """
        |def foo(): (Int32, Int32) = (1, )
        |def main(): Unit = ()
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[ParseError](result)
  }

  test("BadUnary.01") {
    val input =
      """
        |def foo(): Int32 = { lazy; 1 }
        |def main(): Unit = ()
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[ParseError](result)
    expectMain(result)
  }

  test("BadForFragments.01") {
    val input =
      """
        |def foo(): Int32 =
        |    forA ( x <- bar(); y <- baz() yield ???
        |def main(): Unit = ()
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[ParseError](result)
    expectMain(result)
  }

  test("BadForFragments.02") {
    val input =
      """
        |def foo(): Int32 =
        |    forA ( x <- bar(); y <- baz(); yield ???
        |def main(): Unit = ()
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[ParseError](result)
    expectMain(result)
  }

  test("BadForFragments.03") {
    val input =
      """
        |def foo(): Int32 =
        |    forA ( x <- bar(), y <- baz() yield ???
        |def main(): Unit = ()
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[ParseError](result)
    expectMain(result)
  }

  test("BadIfThenElse.01") {
    val input =
      """
        |def foo(): Int32 = if 123 else 321
        |def main(): Unit = ()
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[ParseError](result)
    expectMain(result)
  }

  test("BadIfThenElse.02") {
    val input =
      """
        |def foo(): Int32 = if () 123 else 321
        |def main(): Unit = ()
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[ParseError](result)
    expectMain(result)
  }

  test("BadIfThenElse.03") {
    val input =
      """
        |def foo(): Int32 = if (false) 123
        |def main(): Unit = ()
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[ParseError](result)
    expectMain(result)
  }

  test("BadIfThenElse.04") {
    val input =
      """
        |def foo(): Unit \ IO = if (false) if (true) println(1) else println(1)
        |def main(): Unit = ()
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[ParseError](result)
    expectMain(result)
  }

  test("BadLetMatch.01") {
    val input =
      """
        |def foo(): Int32 = {
        | let (_x, ) = (1, 2);
        | 123
        |}
        |def main(): Unit = ()
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[ParseError](result)
    expectMain(result)
  }

  test("BadLetMatch.02") {
    val input =
      """
        |def foo(): Int32 = {
        | let (, x  = (1, 2);
        | x
        |}
        |def main(): Unit = ()
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[ParseError](result)
    expectMain(result)
  }

  test("BadLetMatch.03") {
    val input =
      """
        |def foo(): Int32 = {
        | let x:  = enum;
        | x
        |}
        |def main(): Unit = ()
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[ParseError](result)
    expectMain(result)
  }

  test("BadMatch.01") {
    val input =
      """
        |def foo(): Int32 = match () { case }
        |def main(): Unit = ()
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[ParseError](result)
    expectMain(result)
  }

  test("BadMatch.02") {
    val input =
      """
        |def map(t: Int32): Int32 = match t
        |def main(): Int32 = 123
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[ParseError](result)
    expectMain(result)
  }

  test("BadFixpointConstraint.01") {
    val input =
      """
        |def getFacts(): #{ ParentOf(String, String), AncestorOf(String, String) } = #{
        |    ParentOf("Pompey", "Strabo").,
        |    ParentOf("Sextus", "Pompey").
        |}
        |def main(): Int32 = 123
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[ParseError](result)
    expectMain(result)
  }

  test("BadType.01") {
    val input =
      """
        |def foo(): List[Int32 = ???
        |def main(): Unit = ()
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[ParseError](result)
    expectMain(result)
  }

  test("BadType.02") {
    val input =
      """
        |def foo(): Int32 -> = ???
        |def main(): Unit = ()
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[ParseError](result)
    expectMain(result)
  }

  test("BadType.03") {
    val input =
      """
        |def foo(): Int32 -> Int32 \ { = ???
        |def main(): Unit = ()
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[ParseError](result)
    expectMain(result)
  }

  test("BadType.04") {
    val input =
      """
        |def foo(): #{ Node() | = ???
        |def main(): Unit = ()
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[ParseError](result)
    expectMain(result)
  }

  test("BadInstance.01") {
    val input =
      """
        |instance Order { }
        |def main(): Unit = ()
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[ParseError](result)
    expectMain(result)
  }

  test("BadEff.01") {
    val input =
      """
        |pub eff Print {
        |    /
        |    pub def printIt(): Unit
        |}
        |def main(): Unit = ()
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[ParseError](result)
    expectMain(result)
  }

  test("BadConstraintSet.01") {
    val input =
      """
        |def baz(): #{ Path(Int32, Int32) } = #{
        |    Edge(1, 2).
        |    Path(x, y) :-
        |}
        |def main(): Unit = ()
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[ParseError](result)
    expectMain(result)
  }

  test("ChainedApplyRecordSelect.01") {
    val input =
      """
        |def main(): Unit = ()
        |
        |def foo(): Int32 =
        |    let f = () -> { g = () -> { h = () -> 12 } };
        |    f()#
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[ParseError](result)
    expectMain(result)
  }

  test("ChainedApplyRecordSelect.02") {
    val input =
      """
        |def main(): Unit = ()
        |
        |def foo(): Int32 =
        |    let f = () -> { g = () -> { h = () -> 12 } };
        |    f()#g()#
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[ParseError](result)
    expectMain(result)
  }

  test("LetMatchNoStatement.01") {
    val input =
      """
        |def foo(): Unit \ IO  =
        |    let x =
        |    println("Hello World!")
        |
        |def main(): Unit = ()
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[ParseError](result)
    expectMain(result)
  }

  test("LetRecDefNoStatement.01") {
    val input =
      """
        |def foo(): Unit \ IO =
        |    def bar(): Int32 = 123
        |def main(): Int32 = 456
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[ParseError](result)
    expectMain(result)
  }

  test("MissingWithBody.01") {
    val input =
      """
        |def foo(): Bool =
        |    let result = run {
        |        mutual1(10)
        |    } with handler AskTell ;
        |    true
        |
        |def main(): Int32 = 123
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[ParseError](result)
    expectMain(result)
  }

  test("MissingCatchBody.01") {
    val input =
      """
        |def foo(): Bool =
        |    try { true } catch
        |def main(): Int32 = 123
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[ParseError](result)
    expectMain(result)
  }

  test("MissingRecordOperation.01") {
    val input =
      """
        |def main(): Int32 =
        |    let _ = { | {} };
        |    2
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[ParseError](result)
    expectMain(result)
  }

  test("MissingRunBody.01") {
    val input =
      """
        |def foo(): Bool =
        |    run { true }
        |def main(): Int32 = 123
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[ParseError](result)
    expectMain(result)
  }

  test("MissingTryBody.01") {
    val input =
      """
        |def foo(): Bool =
        |    try { true }
        |def main(): Int32 = 123
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[ParseError](result)
    expectMain(result)
  }

  test("MissingDotInDatalogConstraint.01") {
    val input =
      """def main(): Unit =
        |    let _ = #{
        |        Edge(1, 2)
        |    };
        |    ()
        |"""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[ParseError](result)
    expectMain(result)
  }

  test("MissingDotInDatalogConstraint.02") {
    val input =
      """def main(): Unit =
        |    let _ = #{
        |        Edge(1, 2).
        |        Path(x, y) :- Edge(x, y)
        |    };
        |    ()
        |"""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[ParseError](result)
    expectMain(result)
  }

  test("MissingWithInPQuery") {
    val input =
      """def main(): Unit =
        |    let p = #{
        |        Edge(1, 2).
        |        Edge(y, x) :- Edge(x, y).
        |    };
        |    let _ = pquery p select A() with ;
        |    ()
        |"""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[ParseError](result)
    expectMain(result)
  }

  test("Regression.#7646") {
    val input =
      """
        |instance ToString[Card] {
        |    pub def toString(x: Card): String = match x {
        |        case Card.Card(r, s) _ => "${r} of ${s}"
        |    }
        |}
        |def main(): Unit = ()
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[ParseError](result)
    expectMain(result)
  }

  test("BadArrowEffectApplication.01") {
    val input =
      """
        |type alias T[_a] = Unit
        |pub def seqCheck(f: a -> a \ l: T[a]): a = ???
        |""".stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[ParseError](result)
  }

  /**
    * Asserts that validation contains a defined entry point.
    */
  def expectMain(result: (Option[TypedAst.Root], List[CompilationMessage])): Unit = result match {
    case (Some(root), _) =>
      if (root.mainEntryPoint.isEmpty) {
        fail("Expected 'main' to be defined.")
      }
    case _ => fail("Expected 'main' to be defined.")
  }

}

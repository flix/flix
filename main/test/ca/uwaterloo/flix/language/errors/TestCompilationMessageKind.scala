package ca.uwaterloo.flix.language.errors

import ca.uwaterloo.flix.TestUtils
import ca.uwaterloo.flix.language.CompilationMessageKind
import ca.uwaterloo.flix.language.CompilationMessageKind.*
import ca.uwaterloo.flix.language.ast.Ast.SyntacticContext
import org.scalatest.funsuite.AnyFunSuite

class TestCompilationMessageKind extends AnyFunSuite with TestUtils {

  private val All: List[CompilationMessageKind] = List(
    DerivationError,
    EntryPointError,
    InstanceError,
    KindError,
    LexerError,
    NameError,
    ParseError(SyntacticContext.Unknown),
    PatternMatchError,
    RedundancyError,
    ResolutionError,
    SafetyError,
    StratificationError,
    TestError,
    TypeError,
    WeederError,
  )

  test("TestParserErrorDoesNotDependOnSctx.01") {
    val e1 = ParseError(SyntacticContext.Unknown)
    val e2 = ParseError(SyntacticContext.Use)
    assert(e1.compare(e2) == 0)
  }

  test("TestParserErrorDoesNotDependOnSctx.02") {
    val e1 = ParseError(SyntacticContext.Unknown)
    val e2 = ParseError(SyntacticContext.Use)
    assert(e2.compare(e1) == 0)
  }

}

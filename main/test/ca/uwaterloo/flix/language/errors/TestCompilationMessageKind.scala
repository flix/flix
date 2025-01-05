package ca.uwaterloo.flix.language.errors

import ca.uwaterloo.flix.TestUtils
import ca.uwaterloo.flix.language.CompilationMessageKind
import ca.uwaterloo.flix.language.CompilationMessageKind.*
import ca.uwaterloo.flix.language.ast.shared.SyntacticContext
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

  private val Cartesian: List[(CompilationMessageKind, CompilationMessageKind)] = All.flatMap(e1 => All.map(e2 => (e1, e2)))

  test("TestParserErrorOrderDoesNotDependOnSctx.01") {
    val e1 = ParseError(SyntacticContext.Unknown)
    val e2 = ParseError(SyntacticContext.Use)
    assert(e1.compare(e2) == 0)
  }

  test("TestParserErrorOrderDoesNotDependOnSctx.02") {
    val e1 = ParseError(SyntacticContext.Unknown)
    val e2 = ParseError(SyntacticContext.Use)
    assert(e2.compare(e1) == 0)
  }

  test("TestOrderIsSameAsIntegers") {
    val numbers = 1 to All.length
    val cartesianNumbers = numbers.flatMap(i => numbers.map(j => (i, j))).toList
    assert(Cartesian.zip(cartesianNumbers).forall { case ((e1, e2), (i, j)) => e1.compare(e2) == i.compare(j) })
  }

}

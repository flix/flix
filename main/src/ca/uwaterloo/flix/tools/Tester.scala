package ca.uwaterloo.flix.tools

import ca.uwaterloo.flix.language.ast.Symbol
import ca.uwaterloo.flix.runtime.CompilationResult
import ca.uwaterloo.flix.util.vt.VirtualTerminal
import ca.uwaterloo.flix.util.vt.VirtualString._
import flix.runtime.{FlixError, ProxyObject}

/**
  * Evaluates all tests in a model.
  */
object Tester {

  /**
    * Represents the outcome of a single test.
    */
  sealed trait TestResult {
    /**
      * The symbol associated with the test.
      */
    def sym: Symbol.DefnSym
  }

  object TestResult {

    /**
      * Represents a successful test case.
      */
    case class Success(sym: Symbol.DefnSym, msg: String) extends TestResult

    /**
      * Represents a failed test case.
      */
    case class Failure(sym: Symbol.DefnSym, msg: String) extends TestResult

  }

  /**
    * Represents the outcome of a run of a suite of tests.
    */
  sealed trait OverallTestResult

  object OverallTestResult {

    /**
      * Represents the outcome where all tests succeeded.
      */
    case object Success extends OverallTestResult

    /**
      * Represents the outcome where at least one test failed.
      */
    case object Failure extends OverallTestResult

    /**
      * Represents the outcome where no tests were run.
      */
    case object NoTests extends OverallTestResult
  }

  /**
    * Represents the results of running all the tests in a given model.
    */
  case class TestResults(results: List[TestResult]) {
    def output: VirtualTerminal = {
      var success = 0
      var failure = 0
      val vt = new VirtualTerminal()
      for ((ns, tests) <- results.groupBy(_.sym.namespace)) {
        val namespace = if (ns.isEmpty) "root" else ns.mkString("/")
        vt << Line("Tests", namespace)
        vt << Indent << NewLine
        for (test <- tests.sortBy(_.sym.loc)) {
          test match {
            case TestResult.Success(sym, msg) =>
              vt << Green("✓") << " " << sym.name << NewLine
              success = success + 1
            case TestResult.Failure(sym, msg) =>
              vt << Red("✗") << " " << sym.name << ": " << msg << " (" << Blue(sym.loc.format) << ")" << NewLine
              failure = failure + 1
          }
        }
        vt << Dedent << NewLine
      }
      // Summary
      if (failure == 0) {
        vt << Green("  Tests Passed!") << s" ($success / $success)" << NewLine
      } else {
        vt << Red(s"  Tests Failed!") << s" ($success / ${success + failure})"
      }
      vt
    }

    def overallResult: OverallTestResult = {
      if (results.isEmpty) {
        OverallTestResult.NoTests
      } else if (results.forall(_.isInstanceOf[TestResult.Success])) {
        OverallTestResult.Success
      } else {
        OverallTestResult.Failure
      }
    }
  }

  /**
    * Evaluates all tests.
    *
    * Returns a pair of (successful, failed)-tests.
    */
  def test(compilationResult: CompilationResult): TestResults = {
    val results = compilationResult.getTests.toList.map {
      case (sym, defn) =>
        try {
          val result = defn().asInstanceOf[ProxyObject]
          result.getValue match {
            case java.lang.Boolean.TRUE => TestResult.Success(sym, "Returned true.")
            case java.lang.Boolean.FALSE => TestResult.Failure(sym, "Returned false.")
            case _ => TestResult.Success(sym, "Returned non-boolean value.")
          }
        } catch {
          case ex: FlixError =>
            TestResult.Failure(sym, ex.getMessage)
        }
    }
    TestResults(results)
  }

}

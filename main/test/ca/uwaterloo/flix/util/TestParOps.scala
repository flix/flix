package ca.uwaterloo.flix.util

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.SourceLocation
import org.scalatest.funsuite.AnyFunSuite

import java.util.concurrent.ForkJoinPool

class TestParOps extends AnyFunSuite {

  /**
    * Returns a multi-threaded Flix instance with an initialized thread pool.
    */
  private def mkFlix(): Flix = {
    val flix = new Flix().setOptions(Options.Default.copy(threads = 4))
    flix.threadPool = new ForkJoinPool(4)
    flix
  }

  test("parMap.propagates.InternalCompilerException") {
    implicit val flix: Flix = mkFlix()
    val ex = intercept[InternalCompilerException] {
      ParOps.parMap(1 to 100)(i => if (i == 42) throw InternalCompilerException("boom", SourceLocation.Unknown) else i)
    }
    assert(ex.message == "boom")
  }

  test("parMap.records.all.but.one.exception.as.suppressed") {
    implicit val flix: Flix = mkFlix()
    // Every element throws, so exactly one exception is rethrown and the other 99 are suppressed.
    val ex = intercept[InternalCompilerException] {
      ParOps.parMap(1 to 100)(_ => throw InternalCompilerException("boom", SourceLocation.Unknown))
    }
    assert(ex.message == "boom")
    assert(ex.getSuppressed.length == 99)
  }

  test("parReach.propagates.InternalCompilerException.unwrapped") {
    implicit val flix: Flix = mkFlix()
    // The exception thrown inside `next` is wrapped in an ExecutionException by `Future.get`.
    // ParOps must unwrap it so the original InternalCompilerException (and its source location)
    // reaches the caller rather than a java.util.concurrent.ExecutionException.
    val ex = intercept[InternalCompilerException] {
      ParOps.parReach(Set(0), (t: Int) =>
        if (t == 5) throw InternalCompilerException("reach-boom", SourceLocation.Unknown)
        else if (t < 10) Set(t + 1)
        else Set.empty[Int]
      )
    }
    assert(ex.message == "reach-boom")
  }
}

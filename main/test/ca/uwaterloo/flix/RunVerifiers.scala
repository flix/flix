package ca.uwaterloo.flix

import ca.uwaterloo.flix.api.{Flix, FlixEvent, FlixListener}
import ca.uwaterloo.flix.language.ast.shared.SecurityContext
import ca.uwaterloo.flix.util.Options
import ca.uwaterloo.flix.verifier.{EffectVerifier, TokenVerifier, TypeVerifier}
import org.scalatest.funsuite.AnyFunSuite

import java.nio.file.Paths

/** Runs all verifiers on a select few programs. */
class RunVerifiers extends AnyFunSuite with TestUtils {

  test("RunAllVerifiers.01") {
    compileWithVerifiers("examples/datalog/railroad-network.flix")
  }

  test("RunAllVerifiers.02") {
    compileWithVerifiers("examples/interoperability/swing/swing-dialog.flix")
  }

  test("RunAllVerifiers.03") {
    compileWithVerifiers("examples/imperative-style/internal-mutability-with-regions.flix")
  }

  test("RunAllVerifiers.04") {
    compileWithVerifiers("examples/effects-and-handlers/advanced/backtracking.flix")
  }

  test("RunAllVerifiers.05") {
    compileWithVerifiers("examples/concurrency-and-parallelism/using-par-yield-recursively.flix")
  }

  /** Compiles the flix program at `path` with all verifiers enabled. */
  private def compileWithVerifiers(path: String): Unit = {
    implicit val flix: Flix = new Flix()

    flix.setOptions(Options.TestWithLibAll)
    flix.addListener(new FlixListener {
      override def notify(e: FlixEvent): Unit = e match {
        case FlixEvent.AfterLexer(sources) =>
          TokenVerifier.verify(sources)
        case FlixEvent.AfterTyper(root) =>
          EffectVerifier.verify(root)
        case FlixEvent.AfterTailPos(root) =>
          TypeVerifier.verify(root)
        case _ => ()
      }
    })
    flix.addFile(Paths.get(path))(SecurityContext.Unrestricted)

    val res = flix.compile()
    expectSuccess(res)
  }

}

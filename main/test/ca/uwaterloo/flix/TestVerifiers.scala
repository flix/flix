package ca.uwaterloo.flix

import ca.uwaterloo.flix.api.{Flix, FlixEvent, FlixListener}
import ca.uwaterloo.flix.language.ast.shared.SecurityContext
import ca.uwaterloo.flix.util.Options
import ca.uwaterloo.flix.verifier.{EffectVerifier, TypeVerifier}
import org.scalatest.funsuite.AnyFunSuite

import java.nio.file.Paths

/**
  * All verifiers should be added here.
  * This ensures that they are run even if disabled everywhere else.
  */
class TestVerifiers extends AnyFunSuite with TestUtils {

  test("RunAllVerifiers.01") {
    compileWithVerifiers("examples/fixpoints/railroad-network.flix")
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

  /** Compiles `program` with all verifiers enabled. */
  private def compileWithVerifiers(path: String): Unit = {
    implicit val flix: Flix = new Flix()

    flix.setOptions(Options.TestWithLibAll)
    flix.addListener(new FlixListener {
      override def notify(e: FlixEvent): Unit = e match {
        case FlixEvent.AfterTyper(root) =>
          EffectVerifier.verify(root)
        case FlixEvent.AfterTailPos(root) =>
          TypeVerifier.verify(root)
        case _ => ()
      }
    })
    flix.addFlix(Paths.get(path))(SecurityContext.AllPermissions)

    val res = flix.compile()
    expectSuccess(res)
  }

}

package ca.uwaterloo.flix

import ca.uwaterloo.flix.api.{Flix, FlixEvent, FlixListener}
import ca.uwaterloo.flix.util.Options
import ca.uwaterloo.flix.verifier.{TokenVerifier, TypeVerifier}
import org.scalatest.funsuite.AnyFunSuite

class TestVerifiers extends AnyFunSuite with TestUtils {

  test("RunAllVerifiers") {
    implicit val flix: Flix = new Flix()

    flix.setOptions(Options.TestWithLibAll)
    flix.addListener(new FlixListener {
      override def notify(e: FlixEvent): Unit = e match {
        case FlixEvent.AfterLexer(sources) =>
          TokenVerifier.verify(sources)
        case FlixEvent.AfterTailPos(root) =>
          TypeVerifier.verify(root)
        case _ => ()
      }
    })

    val res = flix.compile()
    expectSuccess(res)
  }

}

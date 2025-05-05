package ca.uwaterloo.flix

import ca.uwaterloo.flix.api.{Flix, FlixEvent, FlixListener}
import ca.uwaterloo.flix.language.ast.shared.SecurityContext
import ca.uwaterloo.flix.util.Options
import ca.uwaterloo.flix.verifier.{EffectVerifier, TypeVerifier}
import org.scalatest.funsuite.AnyFunSuite

class TestVerifiers extends AnyFunSuite with TestUtils {

  test("RunAllVerifiers") {
    implicit val flix: Flix = new Flix()

    flix.setOptions(Options.TestWithLibAll.copy())
    flix.addListener(new FlixListener {
      override def notify(e: FlixEvent): Unit = e match {
        case FlixEvent.AfterTyper(root) =>
          EffectVerifier.verify(root)
        case FlixEvent.AfterTailPos(root) =>
          TypeVerifier.verify(root)
        case _ => ()
      }
    })
    flix.addSourceCode("<test>", exampleProgram)(SecurityContext.AllPermissions)

    val res = flix.compile()
    expectSuccess(res)

  }

  /** "Complicated" Program to avoid tree shaking the whole AST away. */
  private def exampleProgram: String =
    """
      |def main(): Bool = {
      |  let edges = Map#{"a" => "b", "b" => "c", "b" => "d", "c" => "e"} |> Map.toList;
      |  let facts = inject edges into Edge;
      |  let start = "a";
      |  let p = #{
      |    Reachable(start).
      |    Reachable(x) :- Reachable(y), Edge(y, x).
      |  };
      |  let reachable = Foldable.toSet(query facts, p select x from Reachable(x));
      |  reachable == Set#{"a", "b", "c", "d", "e"}
      |}
      |""".stripMargin

}

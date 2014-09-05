import impl.logic.{Symbol, Term, Value}
import impl.runtime.Unification
import org.scalatest.FunSuite

class TestUnification extends FunSuite {

  test("Unification01") {
    val t = Term.Unit
    val v = Value.Unit

    val r = Unification.unify(t, v)

    assertResult(List(Map.empty))(r)
  }

  test("Unification05") {
    val t1 = Term.Var(Symbol.VariableSymbol("x"))
    val t2 = Term.Int(42)

  }

}

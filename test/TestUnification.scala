import impl.logic.{Value, Symbol, Term}
import impl.runtime.Unification
import org.scalatest.FunSuite

class TestUnification extends FunSuite {

  test("Unification01") {
    val t1 = Term.Var(Symbol.VariableSymbol("x"))
    val t2 = Term.Int(42)

    assertResult(List(Map(Symbol.VariableSymbol("x") -> Term.Int(42))))(Unification.unify(t1, t2, Map.empty[Symbol.VariableSymbol, Term]))
  }

}

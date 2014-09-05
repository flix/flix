import impl.logic.Symbol.VariableSymbol
import impl.logic.{Value, Symbol, Term}
import impl.runtime.Unification
import org.scalatest.FunSuite

class TestUnification extends FunSuite {

  test("Unification01") {
    val t1 = Term.Unit
    val t2 = Term.Unit

    val result = Unification.unify(t1, t2, Map.empty[VariableSymbol, Term])

    assertResult(List(Map()))(result)
  }



  test("Unification05") {
    val t1 = Term.Var(Symbol.VariableSymbol("x"))
    val t2 = Term.Int(42)

    val result = Unification.unify(t1, t2, Map.empty[VariableSymbol, Term])

    assertResult(List(Map(Symbol.VariableSymbol("x") -> Term.Int(42))))(result)
  }

}

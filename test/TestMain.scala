import impl.logic.{Value, Symbol, Term}
import impl.runtime.Unification

object TestMain {

  def main(args: Array[String]): Unit = {
    val t1 = Term.Set(Set(Term.Var(Symbol.VariableSymbol("x"))))
    val v1 = Value.Set(Set(Value.Int(1), Value.Int(2)))

    println(Unification.unify(t1, v1, Map.empty[Symbol.VariableSymbol, Value]))
  }

}

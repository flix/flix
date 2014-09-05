import impl.logic.{Symbol, Term, Type, Value}
import impl.runtime.Unification
import org.scalatest.FunSuite

class TestUnification extends FunSuite {

  test("Unification.Success.01") {
    val t = Term.Unit
    val v = Value.Unit
    val r = Unification.unify(t, v)
    assertResult(List(Map.empty))(r)
  }

  test("Unification.Success.02") {
    val t = Term.Bool(true)
    val v = Value.Bool(true)
    val r = Unification.unify(t, v)
    assertResult(List(Map.empty))(r)
  }

  test("Unification.Success.03") {
    val t = Term.Bool(false)
    val v = Value.Bool(false)
    val r = Unification.unify(t, v)
    assertResult(List(Map.empty))(r)
  }

  test("Unification.Success.04") {
    val t = Term.Int(42)
    val v = Value.Int(42)
    val r = Unification.unify(t, v)
    assertResult(List(Map.empty))(r)
  }

  test("Unification.Success.05") {
    val t = Term.Str("foo")
    val v = Value.Str("foo")
    val r = Unification.unify(t, v)
    assertResult(List(Map.empty))(r)
  }

  test("Unification.Success.06") {
    val t = Term.Tuple2(Term.Int(1), Term.Str("one"))
    val v = Value.Tuple2(Value.Int(1), Value.Str("one"))
    val r = Unification.unify(t, v)
    assertResult(List(Map.empty))(r)
  }

  test("Unification.Success.07") {
    val typ = Type.Sum(List(Type.Tagged(Symbol.NamedSymbol("Foo"), Type.Int)))
    val t = Term.Tagged(Symbol.NamedSymbol("Foo"), Term.Int(42), typ)
    val v = Value.Tagged(Symbol.NamedSymbol("Foo"), Value.Int(42), typ)
    val r = Unification.unify(t, v)
    assertResult(List(Map.empty))(r)
  }

  test("Unification.Failure.01") {
    val t = Term.Unit
    val v = Value.Bool(true)
    val r = Unification.unify(t, v)
    assertResult(List.empty)(r)
  }

  test("Unification.Failure.02") {
    val t = Term.Bool(true)
    val v = Value.Bool(false)
    val r = Unification.unify(t, v)
    assertResult(List.empty)(r)
  }

  test("Unification.Failure.03") {
    val t = Term.Bool(true)
    val v = Value.Int(42)
    val r = Unification.unify(t, v)
    assertResult(List.empty)(r)
  }

  test("Unification.Failure.04") {
    val t = Term.Int(1)
    val v = Value.Int(2)
    val r = Unification.unify(t, v)
    assertResult(List.empty)(r)
  }

  test("Unification.Failure.05") {
    val t = Term.Str("foo")
    val v = Value.Str("bar")
    val r = Unification.unify(t, v)
    assertResult(List.empty)(r)
  }
}

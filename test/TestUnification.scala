import impl.logic.{Symbol, Term, Type, Value}
import impl.runtime.Unification

import org.scalatest.FunSuite

class TestUnification extends FunSuite {

  val x = Symbol.VariableSymbol("x")
  val y = Symbol.VariableSymbol("y")
  val z = Symbol.VariableSymbol("z")

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

  test("Unification.Success.08") {
    val t = Term.Set(Set.empty)
    val v = Value.Set(Set.empty)
    val r = Unification.unify(t, v)
    assertResult(List(Map.empty))(r)
  }

  test("Unification.Success.09") {
    val t = Term.Set(Set(Term.Unit))
    val v = Value.Set(Set(Value.Unit))
    val r = Unification.unify(t, v)
    assertResult(List(Map.empty))(r)
  }

  test("Unification.Success.10") {
    val t = Term.Set(Set(Term.Bool(true), Term.Int(42)))
    val v = Value.Set(Set(Value.Bool(true), Value.Int(42)))
    val r = Unification.unify(t, v)
    assertResult(List(Map.empty))(r)
  }

  test("Unification.Success.11") {
    val t = Term.Set(Set(Term.Set(Set(Term.Unit))))
    val v = Value.Set(Set(Value.Set(Set(Value.Unit))))
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

  test("Unification.Failure.06") {
    val t = Term.Tuple2(Term.Int(1), Term.Str("one"))
    val v = Value.Tuple2(Value.Str("one"), Value.Int(1))
    val r = Unification.unify(t, v)
    assertResult(List.empty)(r)
  }

  test("Unification.Failure.07") {
    val typ = Type.Sum(List(Type.Tagged(Symbol.NamedSymbol("Foo"), Type.Int)))
    val t = Term.Tagged(Symbol.NamedSymbol("Foo"), Term.Int(42), typ)
    val v = Value.Tagged(Symbol.NamedSymbol("Bar"), Value.Int(42), typ)
    val r = Unification.unify(t, v)
    assertResult(List.empty)(r)
  }

  test("Unification.Failure.09") {
    val t = Term.Set(Set.empty)
    val v = Value.Set(Set(Value.Int(42)))
    val r = Unification.unify(t, v)
    assertResult(List.empty)(r)
  }

  test("Unification.Failure.10") {
    val t = Term.Set(Set(Term.Int(42)))
    val v = Value.Set(Set.empty)
    val r = Unification.unify(t, v)
    assertResult(List.empty)(r)
  }

  test("Unification.Failure.11") {
    val t = Term.Set(Set(Term.Unit))
    val v = Value.Set(Set(Value.Int(42)))
    val r = Unification.unify(t, v)
    assertResult(List.empty)(r)
  }

//  test("Unification.Success.10") {
//    val t = Term.Set(Set(Term.Bool(true), Term.Int(42)))
//    val v = Value.Set(Set(Value.Bool(true), Value.Int(42)))
//    val r = Unification.unify(t, v)
//    assertResult(List(Map.empty))(r)
//  }
//
//  test("Unification.Success.11") {
//    val t = Term.Set(Set(Term.Set(Set(Term.Unit))))
//    val v = Value.Set(Set(Value.Set(Set(Value.Unit))))
//    val r = Unification.unify(t, v)
//    assertResult(List(Map.empty))(r)
//  }

  test("Unification.Match.01") {
    val t = Term.Var(x)
    val v = Value.Unit
    val r = Unification.unify(t, v)
    assertResult(List(Map(x -> Value.Unit)))(r)
  }
}

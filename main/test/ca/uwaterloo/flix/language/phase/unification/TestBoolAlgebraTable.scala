package ca.uwaterloo.flix.language.phase.unification

import ca.uwaterloo.flix.language.phase.unification.BoolAlgebra._
import ca.uwaterloo.flix.language.phase.unification.BoolAlgebraTable._
import org.scalatest.FunSuite

class TestBoolAlgebraTable extends FunSuite {

  test("Minimize.True.01") {
    assertResult(expected = Top)(actual = minimizeFormula(Top))
  }

  test("Minimize.True.02") {
    assertResult(expected = Top)(actual = minimizeFormula(Neg(Bot)))
  }

  test("Minimize.True.03") {
    assertResult(expected = Top)(actual = minimizeFormula(Join(Top, Top)))
  }

  test("Minimize.True.04") {
    assertResult(expected = Top)(actual = minimizeFormula(Meet(Top, Top)))
  }

  test("Minimize.True.05") {
    assertResult(expected = Top)(actual = minimizeFormula(Meet(Top, Bot)))
  }

  test("Minimize.True.06") {
    assertResult(expected = Top)(actual = minimizeFormula(Meet(Bot, Top)))
  }

  test("Minimize.False.01") {
    assertResult(expected = Bot)(actual = minimizeFormula(Bot))
  }

  test("Minimize.False.02") {
    assertResult(expected = Top)(actual = minimizeFormula(Neg(Bot)))
  }

  test("Minimize.False.03") {
    assertResult(expected = Bot)(actual = minimizeFormula(Join(Bot, Bot)))
  }

  test("Minimize.False.04") {
    assertResult(expected = Bot)(actual = minimizeFormula(Join(Neg(Bot), Bot)))
  }

  test("Minimize.False.05") {
    assertResult(expected = Bot)(actual = minimizeFormula(Join(Bot, Neg(Bot))))
  }

  test("Minimize.False.06") {
    assertResult(expected = Top)(actual = minimizeFormula(Join(Neg(Bot), Neg(Bot))))
  }

  test("Minimize.False.07") {
    assertResult(expected = Bot)(actual = minimizeFormula(Meet(Bot, Bot)))
  }

  test("Minimize.False.08") {
    assertResult(expected = Top)(actual = minimizeFormula(Meet(Neg(Bot), Bot)))
  }

  test("Minimize.False.09") {
    assertResult(expected = Top)(actual = minimizeFormula(Meet(Bot, Neg(Bot))))
  }

  test("Minimize.False.10") {
    assertResult(expected = Top)(actual = minimizeFormula(Meet(Neg(Bot), Neg(Bot))))
  }

  test("Minimize.False.11") {
    assertResult(expected = Bot)(actual = minimizeFormula(Neg(Neg(Bot))))
  }

  test("Minimize.Var1.01") {
    assertResult(expected = Var(0))(actual = minimizeFormula(Var(0)))
  }

  test("Minimize.Var1.02") {
    assertResult(expected = Var(0))(actual = minimizeFormula(Neg(Neg(Var(0)))))
  }

  test("Minimize.Var1.03") {
    assertResult(expected = Var(0))(actual = minimizeFormula(Join(Var(0), Var(0))))
  }

  test("Minimize.Var1.04") {
    assertResult(expected = Var(0))(actual = minimizeFormula(Join(Var(0), Top)))
  }

  test("Minimize.Var1.05") {
    assertResult(expected = Var(0))(actual = minimizeFormula(Join(Top, Var(0))))
  }

  test("Minimize.Var1.06") {
    assertResult(expected = Var(0))(actual = minimizeFormula(Meet(Var(0), Var(0))))
  }

  test("Minimize.Var1.07") {
    assertResult(expected = Top)(actual = minimizeFormula(Meet(Var(0), Top)))
  }

  test("Minimize.Var1.08") {
    assertResult(expected = Top)(actual = minimizeFormula(Meet(Top, Var(0))))
  }

  test("Minimize.Var1.09") {
    assertResult(expected = Var(0))(actual = minimizeFormula(Meet(Var(0), Neg(Top))))
  }

  test("Minimize.Var1.10") {
    assertResult(expected = Var(0))(actual = minimizeFormula(Meet(Neg(Top), Var(0))))
  }

  test("Minimize.Var1.11") {
    assertResult(expected = Neg(Var(0)))(actual = minimizeFormula(Join(Top, Neg(Var(0)))))
  }

  test("Minimize.Var1.12") {
    assertResult(expected = Bot)(actual = minimizeFormula(Join(Bot, Neg(Var(0)))))
  }

  test("Minimize.Var1.13") {
    assertResult(expected = Neg(Var(0)))(actual = minimizeFormula(Meet(Bot, Neg(Var(0)))))
  }

  test("Minimize.Var1.14") {
    assertResult(expected = Top)(actual = minimizeFormula(Meet(Top, Neg(Var(0)))))
  }

  test("Minimize.Var1AndVar2.01") {
    assertResult(expected = Join(Var(0), Var(1)))(actual = minimizeFormula(Join(Var(0), Var(1))))
  }

  test("Minimize.Var1AndVar2.02") {
    assertResult(expected = Join(Var(0), Var(1)))(actual = minimizeFormula(Join(Var(1), Var(0))))
  }

  test("Minimize.Var1AndVar2.03") {
    assertResult(expected = Join(Var(0), Var(1)))(actual = minimizeFormula(Join(Var(0), Join(Var(1), Var(1)))))
  }

  test("Minimize.Var1AndVar2.04") {
    assertResult(expected = Join(Var(0), Var(1)))(actual = minimizeFormula(Join(Join(Var(1), Var(1)), Var(0))))
  }

  test("Minimize.Var1AndVar2.05") {
    assertResult(expected = Bot)(actual = minimizeFormula(Join(Join(Var(1), Neg(Var(1))), Var(0))))
  }

  test("Minimize.Var1AndVar2.06") {
    assertResult(expected = Var(0))(actual = minimizeFormula(Meet(Var(0), Var(0))))
  }

  test("Minimize.Var1AndVar2.07") {
    assertResult(expected = Top)(actual = minimizeFormula(Meet(Var(0), Neg(Var(0)))))
  }

  test("Minimize.Var1AndVar2.08") {
    assertResult(expected = Meet(Var(0), Var(1)))(actual = minimizeFormula(Meet(Var(0), Var(1))))
  }

  test("Minimize.Var1AndVar2.09") {
    assertResult(expected = Meet(Var(0), Var(1)))(actual = minimizeFormula(Meet(Var(1), Var(0))))
  }

  test("Minimize.Var1AndVar2.10") {
    assertResult(expected = Var(0))(actual = minimizeFormula(Join(Meet(Var(1), Var(0)), Var(0))))
  }

  test("Minimize.Var1AndVar2.11") {
    assertResult(expected = Meet(Var(0), Var(1)))(actual = minimizeFormula(Meet(Meet(Var(1), Var(0)), Var(0))))
  }

  test("Minimize.Var1AndVar2.12") {
    assertResult(expected = Var(0))(actual = minimizeFormula(Meet(Join(Var(1), Var(0)), Var(0))))
  }

  test("Minimize.Var1AndVar2.13") {
    assertResult(expected = Var(0))(actual = minimizeFormula(Join(Var(0), Meet(Var(1), Var(0)))))
  }

  test("Minimize.Var1AndVar2.14") {
    assertResult(expected = Meet(Var(0), Var(1)))(actual = minimizeFormula(Meet(Var(0), Meet(Var(1), Var(0)))))
  }

  test("Minimize.Var1AndVar2.15") {
    assertResult(expected = Var(0))(actual = minimizeFormula(Meet(Var(0), Join(Var(1), Var(0)))))
  }

  test("Minimize.Var1AndVar2.16") {
    assertResult(expected = Neg(Var(0)))(actual = minimizeFormula(Neg(Meet(Var(0), Join(Var(1), Var(0))))))
  }

  test("Minimize.Var1AndVar2AndVar3.01") {
    assertResult(expected = Join(Var(0), Join(Var(1), Var(2))))(actual = minimizeFormula(Join(Join(Var(0), Var(1)), Var(2))))
  }

  test("Minimize.Var1AndVar2AndVar3.02") {
    assertResult(expected = Join(Var(0), Join(Var(1), Var(2))))(actual = minimizeFormula(Join(Join(Var(2), Var(1)), Var(0))))
  }

  test("Minimize.Var1AndVar2AndVar3.03") {
    assertResult(expected = Meet(Var(0), Meet(Var(1), Var(2))))(actual = minimizeFormula(Meet(Meet(Var(0), Var(1)), Var(2))))
  }

  test("Minimize.Var1AndVar2AndVar3.04") {
    assertResult(expected = Meet(Var(0), Meet(Var(1), Var(2))))(actual = minimizeFormula(Meet(Meet(Var(2), Var(1)), Var(0))))
  }

  test("Minimize.Var1AndVar2AndVar3.05") {
    assertResult(expected = Join(Var(0), Join(Var(1), Var(2))))(actual = minimizeFormula(Join(Join(Join(Var(0), Var(1)), Var(2)), Join(Join(Var(0), Var(1)), Var(2)))))
  }

  test("Minimize.Var1AndVar2AndVar3.06") {
    assertResult(expected = Join(Var(0), Join(Var(1), Var(2))))(actual = minimizeFormula(Join(Join(Join(Var(2), Var(1)), Var(0)), Join(Join(Var(0), Var(1)), Var(2)))))
  }

  test("Minimize.Var1AndVar2AndVar3.07") {
    assertResult(expected = Bot)(actual = minimizeFormula(Join(Join(Join(Var(2), Var(1)), Var(0)), Neg(Join(Join(Var(0), Var(1)), Var(2))))))
  }

  test("Minimize.Var1AndVar2AndVar3.08") {
    assertResult(expected = Top)(actual = minimizeFormula(Meet(Join(Join(Var(2), Var(1)), Var(0)), Neg(Join(Join(Var(0), Var(1)), Var(2))))))
  }

  test("Minimize.Var1AndVar2AndVar3.09") {
    assertResult(expected = Join(Var(0), Join(Var(1), Var(2))))(actual = minimizeFormula(Join(Join(Join(Var(2), Var(1)), Var(0)), Meet(Var(2), Join(Var(2), Var(2))))))
  }

  test("Minimize.Var1AndVar2AndVar3.10") {
    assertResult(expected = Meet(Var(0), Meet(Var(1), Var(2))))(actual = minimizeFormula(Meet(Var(0), Meet(Var(1), Meet(Var(2), Meet(Var(0), Meet(Var(1), Var(2))))))))
  }

  test("Minimize.Var1AndVar2AndVar3.11") {
    assertResult(expected = Top)(actual = minimizeFormula(Meet(Neg(Var(0)), Meet(Var(1), Meet(Var(2), Meet(Var(0), Meet(Var(1), Var(2))))))))
  }

  test("Minimize.Var1AndVar2AndVar3.12") {
    assertResult(expected = Meet(Var(0), Meet(Var(1), Var(2))))(actual = minimizeFormula(Meet(Meet(Var(2), Meet(Var(1), Var(0))), Join(Var(0), Join(Var(0), Join(Var(2), Meet(Var(0), Meet(Var(1), Var(2)))))))))
  }

  test("Minimize.Var1AndVar2AndVar3.13") {
    assertResult(expected = Join(Var(0), Var(2)))(actual = minimizeFormula(Join(Meet(Var(1), Neg(Var(1))), Join(Var(2), Var(0)))))
  }

  test("Minimize.Var1AndVar2AndVar3.14") {
    assertResult(expected = Var(0))(actual = minimizeFormula(Meet(Var(0), Join(Join(Var(1), Neg(Var(1))), Meet(Var(2), Join(Var(2), Var(1)))))))
  }

  test("Minimize.Var1AndVar2AndVar3AndVar4.01") {
    assertResult(expected = Join(Var(0), Join(Var(1), Join(Var(2), Var(3)))))(actual = minimizeFormula(Join(Join(Var(0), Var(1)), Join(Var(2), Var(3)))))
  }

  test("Minimize.Var1AndVar2AndVar3AndVar4.02") {
    assertResult(expected = Join(Var(0), Join(Var(1), Join(Var(2), Var(3)))))(actual = minimizeFormula(Join(Join(Var(3), Var(2)), Join(Var(0), Var(1)))))
  }

  test("Minimize.Var1AndVar2AndVar3AndVar4.03") {
    assertResult(expected = Meet(Var(0), Meet(Var(1), Meet(Var(2), Var(3)))))(actual = minimizeFormula(Meet(Meet(Var(0), Var(1)), Meet(Var(2), Var(3)))))
  }

  test("Minimize.Var1AndVar2AndVar3AndVar4.04") {
    assertResult(expected = Join(Meet(Neg(Var(0)), Neg(Var(1))), Meet(Neg(Var(2)), Neg(Var(3)))))(actual = minimizeFormula(Neg(Meet(Join(Neg(Neg(Var(0))), Neg(Neg(Var(1)))), Join(Neg(Neg(Var(2))), Neg(Neg(Var(3))))))))
  }

}

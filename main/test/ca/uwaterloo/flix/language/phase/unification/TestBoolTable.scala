package ca.uwaterloo.flix.language.phase.unification

import ca.uwaterloo.flix.language.phase.unification.BoolFormula._
import ca.uwaterloo.flix.language.phase.unification.BoolTable._
import org.scalatest.FunSuite

class TestBoolTable extends FunSuite {

  test("Minimize.True.01") {
    assertResult(expected = True)(actual = minimizeFormula(True))
  }

  test("Minimize.True.02") {
    assertResult(expected = True)(actual = minimizeFormula(Neg(False)))
  }

  test("Minimize.True.03") {
    assertResult(expected = True)(actual = minimizeFormula(Conj(True, True)))
  }

  test("Minimize.True.04") {
    assertResult(expected = True)(actual = minimizeFormula(Disj(True, True)))
  }

  test("Minimize.True.05") {
    assertResult(expected = True)(actual = minimizeFormula(Disj(True, False)))
  }

  test("Minimize.True.06") {
    assertResult(expected = True)(actual = minimizeFormula(Disj(False, True)))
  }

  test("Minimize.False.01") {
    assertResult(expected = False)(actual = minimizeFormula(False))
  }

  test("Minimize.False.02") {
    assertResult(expected = True)(actual = minimizeFormula(Neg(False)))
  }

  test("Minimize.False.03") {
    assertResult(expected = False)(actual = minimizeFormula(Conj(False, False)))
  }

  test("Minimize.False.04") {
    assertResult(expected = False)(actual = minimizeFormula(Conj(Neg(False), False)))
  }

  test("Minimize.False.05") {
    assertResult(expected = False)(actual = minimizeFormula(Conj(False, Neg(False))))
  }

  test("Minimize.False.06") {
    assertResult(expected = True)(actual = minimizeFormula(Conj(Neg(False), Neg(False))))
  }

  test("Minimize.False.07") {
    assertResult(expected = False)(actual = minimizeFormula(Disj(False, False)))
  }

  test("Minimize.False.08") {
    assertResult(expected = True)(actual = minimizeFormula(Disj(Neg(False), False)))
  }

  test("Minimize.False.09") {
    assertResult(expected = True)(actual = minimizeFormula(Disj(False, Neg(False))))
  }

  test("Minimize.False.10") {
    assertResult(expected = True)(actual = minimizeFormula(Disj(Neg(False), Neg(False))))
  }

  test("Minimize.False.11") {
    assertResult(expected = False)(actual = minimizeFormula(Neg(Neg(False))))
  }

  test("Minimize.Var1.01") {
    assertResult(expected = Var(0))(actual = minimizeFormula(Var(0)))
  }

  test("Minimize.Var1.02") {
    assertResult(expected = Var(0))(actual = minimizeFormula(Neg(Neg(Var(0)))))
  }

  test("Minimize.Var1.03") {
    assertResult(expected = Var(0))(actual = minimizeFormula(Conj(Var(0), Var(0))))
  }

  test("Minimize.Var1.04") {
    assertResult(expected = Var(0))(actual = minimizeFormula(Conj(Var(0), True)))
  }

  test("Minimize.Var1.05") {
    assertResult(expected = Var(0))(actual = minimizeFormula(Conj(True, Var(0))))
  }

  test("Minimize.Var1.06") {
    assertResult(expected = Var(0))(actual = minimizeFormula(Disj(Var(0), Var(0))))
  }

  test("Minimize.Var1.07") {
    assertResult(expected = True)(actual = minimizeFormula(Disj(Var(0), True)))
  }

  test("Minimize.Var1.08") {
    assertResult(expected = True)(actual = minimizeFormula(Disj(True, Var(0))))
  }

  test("Minimize.Var1.09") {
    assertResult(expected = Var(0))(actual = minimizeFormula(Disj(Var(0), Neg(True))))
  }

  test("Minimize.Var1.10") {
    assertResult(expected = Var(0))(actual = minimizeFormula(Disj(Neg(True), Var(0))))
  }

  test("Minimize.Var1.11") {
    assertResult(expected = Neg(Var(0)))(actual = minimizeFormula(Conj(True, Neg(Var(0)))))
  }

  test("Minimize.Var1.12") {
    assertResult(expected = False)(actual = minimizeFormula(Conj(False, Neg(Var(0)))))
  }

  test("Minimize.Var1.13") {
    assertResult(expected = Neg(Var(0)))(actual = minimizeFormula(Disj(False, Neg(Var(0)))))
  }

  test("Minimize.Var1.14") {
    assertResult(expected = True)(actual = minimizeFormula(Disj(True, Neg(Var(0)))))
  }

  test("Minimize.Var1AndVar2.01") {
    assertResult(expected = Conj(Var(0), Var(1)))(actual = minimizeFormula(Conj(Var(0), Var(1))))
  }

  test("Minimize.Var1AndVar2.02") {
    assertResult(expected = Conj(Var(0), Var(1)))(actual = minimizeFormula(Conj(Var(1), Var(0))))
  }

  test("Minimize.Var1AndVar2.03") {
    assertResult(expected = Conj(Var(0), Var(1)))(actual = minimizeFormula(Conj(Var(0), Conj(Var(1), Var(1)))))
  }

  test("Minimize.Var1AndVar2.04") {
    assertResult(expected = Conj(Var(0), Var(1)))(actual = minimizeFormula(Conj(Conj(Var(1), Var(1)), Var(0))))
  }

  test("Minimize.Var1AndVar2.05") {
    assertResult(expected = False)(actual = minimizeFormula(Conj(Conj(Var(1), Neg(Var(1))), Var(0))))
  }

  test("Minimize.Var1AndVar2.06") {
    assertResult(expected = Var(0))(actual = minimizeFormula(Disj(Var(0), Var(0))))
  }

  test("Minimize.Var1AndVar2.07") {
    assertResult(expected = True)(actual = minimizeFormula(Disj(Var(0), Neg(Var(0)))))
  }

  test("Minimize.Var1AndVar2.08") {
    assertResult(expected = Disj(Var(0), Var(1)))(actual = minimizeFormula(Disj(Var(0), Var(1))))
  }

  test("Minimize.Var1AndVar2.09") {
    assertResult(expected = Disj(Var(0), Var(1)))(actual = minimizeFormula(Disj(Var(1), Var(0))))
  }

  test("Minimize.Var1AndVar2.10") {
    assertResult(expected = Var(0))(actual = minimizeFormula(Conj(Disj(Var(1), Var(0)), Var(0))))
  }

  test("Minimize.Var1AndVar2.11") {
    assertResult(expected = Disj(Var(0), Var(1)))(actual = minimizeFormula(Disj(Disj(Var(1), Var(0)), Var(0))))
  }

  test("Minimize.Var1AndVar2.12") {
    assertResult(expected = Var(0))(actual = minimizeFormula(Disj(Conj(Var(1), Var(0)), Var(0))))
  }

  test("Minimize.Var1AndVar2.13") {
    assertResult(expected = Var(0))(actual = minimizeFormula(Conj(Var(0), Disj(Var(1), Var(0)))))
  }

  test("Minimize.Var1AndVar2.14") {
    assertResult(expected = Disj(Var(0), Var(1)))(actual = minimizeFormula(Disj(Var(0), Disj(Var(1), Var(0)))))
  }

  test("Minimize.Var1AndVar2.15") {
    assertResult(expected = Var(0))(actual = minimizeFormula(Disj(Var(0), Conj(Var(1), Var(0)))))
  }

  test("Minimize.Var1AndVar2.16") {
    assertResult(expected = Neg(Var(0)))(actual = minimizeFormula(Neg(Disj(Var(0), Conj(Var(1), Var(0))))))
  }

  test("Minimize.Var1AndVar2AndVar3.01") {
    assertResult(expected = Conj(Var(0), Conj(Var(1), Var(2))))(actual = minimizeFormula(Conj(Conj(Var(0), Var(1)), Var(2))))
  }

  test("Minimize.Var1AndVar2AndVar3.02") {
    assertResult(expected = Conj(Var(0), Conj(Var(1), Var(2))))(actual = minimizeFormula(Conj(Conj(Var(2), Var(1)), Var(0))))
  }

  test("Minimize.Var1AndVar2AndVar3.03") {
    assertResult(expected = Disj(Var(0), Disj(Var(1), Var(2))))(actual = minimizeFormula(Disj(Disj(Var(0), Var(1)), Var(2))))
  }

  test("Minimize.Var1AndVar2AndVar3.04") {
    assertResult(expected = Disj(Var(0), Disj(Var(1), Var(2))))(actual = minimizeFormula(Disj(Disj(Var(2), Var(1)), Var(0))))
  }

  test("Minimize.Var1AndVar2AndVar3.05") {
    assertResult(expected = Conj(Var(0), Conj(Var(1), Var(2))))(actual = minimizeFormula(Conj(Conj(Conj(Var(0), Var(1)), Var(2)), Conj(Conj(Var(0), Var(1)), Var(2)))))
  }

  test("Minimize.Var1AndVar2AndVar3.06") {
    assertResult(expected = Conj(Var(0), Conj(Var(1), Var(2))))(actual = minimizeFormula(Conj(Conj(Conj(Var(2), Var(1)), Var(0)), Conj(Conj(Var(0), Var(1)), Var(2)))))
  }

  test("Minimize.Var1AndVar2AndVar3.07") {
    assertResult(expected = False)(actual = minimizeFormula(Conj(Conj(Conj(Var(2), Var(1)), Var(0)), Neg(Conj(Conj(Var(0), Var(1)), Var(2))))))
  }

  test("Minimize.Var1AndVar2AndVar3.08") {
    assertResult(expected = True)(actual = minimizeFormula(Disj(Conj(Conj(Var(2), Var(1)), Var(0)), Neg(Conj(Conj(Var(0), Var(1)), Var(2))))))
  }

  test("Minimize.Var1AndVar2AndVar3.09") {
    assertResult(expected = Conj(Var(0), Conj(Var(1), Var(2))))(actual = minimizeFormula(Conj(Conj(Conj(Var(2), Var(1)), Var(0)), Disj(Var(2), Conj(Var(2), Var(2))))))
  }

  test("Minimize.Var1AndVar2AndVar3.10") {
    assertResult(expected = Disj(Var(0), Disj(Var(1), Var(2))))(actual = minimizeFormula(Disj(Var(0), Disj(Var(1), Disj(Var(2), Disj(Var(0), Disj(Var(1), Var(2))))))))
  }

  test("Minimize.Var1AndVar2AndVar3.11") {
    assertResult(expected = True)(actual = minimizeFormula(Disj(Neg(Var(0)), Disj(Var(1), Disj(Var(2), Disj(Var(0), Disj(Var(1), Var(2))))))))
  }

  test("Minimize.Var1AndVar2AndVar3.12") {
    assertResult(expected = Disj(Var(0), Disj(Var(1), Var(2))))(actual = minimizeFormula(Disj(Disj(Var(2), Disj(Var(1), Var(0))), Conj(Var(0), Conj(Var(0), Conj(Var(2), Disj(Var(0), Disj(Var(1), Var(2)))))))))
  }

  test("Minimize.Var1AndVar2AndVar3.13") {
    assertResult(expected = Conj(Var(0), Var(2)))(actual = minimizeFormula(Conj(Disj(Var(1), Neg(Var(1))), Conj(Var(2), Var(0)))))
  }

  test("Minimize.Var1AndVar2AndVar3.14") {
    assertResult(expected = Var(0))(actual = minimizeFormula(Disj(Var(0), Conj(Conj(Var(1), Neg(Var(1))), Disj(Var(2), Conj(Var(2), Var(1)))))))
  }

  test("Minimize.Var1AndVar2AndVar3AndVar4.01") {
    assertResult(expected = Conj(Conj(Var(0), Var(1)), Conj(Var(2), Var(3))))(actual = minimizeFormula(Conj(Conj(Var(0), Var(1)), Conj(Var(2), Var(3)))))
  }

  test("Minimize.Var1AndVar2AndVar3AndVar4.02") {
    assertResult(expected = Conj(Conj(Var(2), Var(3)), Conj(Var(0), Var(1))))(actual = minimizeFormula(Conj(Conj(Var(3), Var(2)), Conj(Var(0), Var(1)))))
  }

  test("Minimize.Var1AndVar2AndVar3AndVar4.03") {
    assertResult(expected = Disj(Disj(Var(0), Var(1)), Disj(Var(2), Var(3))))(actual = minimizeFormula(Disj(Disj(Var(0), Var(1)), Disj(Var(2), Var(3)))))
  }

  test("Minimize.Var1AndVar2AndVar3AndVar4.04") {
    assertResult(expected = Neg(Disj(Conj(Var(0), Var(1)), Conj(Var(2), Var(3)))))(actual = minimizeFormula(Neg(Disj(Conj(Neg(Neg(Var(0))), Neg(Neg(Var(1)))), Conj(Neg(Neg(Var(2))), Neg(Neg(Var(3))))))))
  }

}

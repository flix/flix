package ca.uwaterloo.flix.language.phase.unification

import ca.uwaterloo.flix.language.phase.unification.BoolFormula._
import ca.uwaterloo.flix.language.phase.unification.BoolFormulaTable._
import org.scalatest.funsuite.AnyFunSuite

class TestBoolFormulaTable extends AnyFunSuite {

  test("Minimize.True.01") {
    assertResult(expected = True)(actual = minimizeFormula(True))
  }

  test("Minimize.True.02") {
    assertResult(expected = True)(actual = minimizeFormula(Not(False)))
  }

  test("Minimize.True.03") {
    assertResult(expected = True)(actual = minimizeFormula(And(True, True)))
  }

  test("Minimize.True.04") {
    assertResult(expected = True)(actual = minimizeFormula(Or(True, True)))
  }

  test("Minimize.True.05") {
    assertResult(expected = True)(actual = minimizeFormula(Or(True, False)))
  }

  test("Minimize.True.06") {
    assertResult(expected = True)(actual = minimizeFormula(Or(False, True)))
  }

  test("Minimize.False.01") {
    assertResult(expected = False)(actual = minimizeFormula(False))
  }

  test("Minimize.False.02") {
    assertResult(expected = True)(actual = minimizeFormula(Not(False)))
  }

  test("Minimize.False.03") {
    assertResult(expected = False)(actual = minimizeFormula(And(False, False)))
  }

  test("Minimize.False.04") {
    assertResult(expected = False)(actual = minimizeFormula(And(Not(False), False)))
  }

  test("Minimize.False.05") {
    assertResult(expected = False)(actual = minimizeFormula(And(False, Not(False))))
  }

  test("Minimize.False.06") {
    assertResult(expected = True)(actual = minimizeFormula(And(Not(False), Not(False))))
  }

  test("Minimize.False.07") {
    assertResult(expected = False)(actual = minimizeFormula(Or(False, False)))
  }

  test("Minimize.False.08") {
    assertResult(expected = True)(actual = minimizeFormula(Or(Not(False), False)))
  }

  test("Minimize.False.09") {
    assertResult(expected = True)(actual = minimizeFormula(Or(False, Not(False))))
  }

  test("Minimize.False.10") {
    assertResult(expected = True)(actual = minimizeFormula(Or(Not(False), Not(False))))
  }

  test("Minimize.False.11") {
    assertResult(expected = False)(actual = minimizeFormula(Not(Not(False))))
  }

  test("Minimize.Var1.01") {
    assertResult(expected = Var(0))(actual = minimizeFormula(Var(0)))
  }

  test("Minimize.Var1.02") {
    assertResult(expected = Var(0))(actual = minimizeFormula(Not(Not(Var(0)))))
  }

  test("Minimize.Var1.03") {
    assertResult(expected = Var(0))(actual = minimizeFormula(And(Var(0), Var(0))))
  }

  test("Minimize.Var1.04") {
    assertResult(expected = Var(0))(actual = minimizeFormula(And(Var(0), True)))
  }

  test("Minimize.Var1.05") {
    assertResult(expected = Var(0))(actual = minimizeFormula(And(True, Var(0))))
  }

  test("Minimize.Var1.06") {
    assertResult(expected = Var(0))(actual = minimizeFormula(Or(Var(0), Var(0))))
  }

  test("Minimize.Var1.07") {
    assertResult(expected = True)(actual = minimizeFormula(Or(Var(0), True)))
  }

  test("Minimize.Var1.08") {
    assertResult(expected = True)(actual = minimizeFormula(Or(True, Var(0))))
  }

  test("Minimize.Var1.09") {
    assertResult(expected = Var(0))(actual = minimizeFormula(Or(Var(0), Not(True))))
  }

  test("Minimize.Var1.10") {
    assertResult(expected = Var(0))(actual = minimizeFormula(Or(Not(True), Var(0))))
  }

  test("Minimize.Var1.11") {
    assertResult(expected = Not(Var(0)))(actual = minimizeFormula(And(True, Not(Var(0)))))
  }

  test("Minimize.Var1.12") {
    assertResult(expected = False)(actual = minimizeFormula(And(False, Not(Var(0)))))
  }

  test("Minimize.Var1.13") {
    assertResult(expected = Not(Var(0)))(actual = minimizeFormula(Or(False, Not(Var(0)))))
  }

  test("Minimize.Var1.14") {
    assertResult(expected = True)(actual = minimizeFormula(Or(True, Not(Var(0)))))
  }

  test("Minimize.Var1AndVar2.01") {
    assertResult(expected = And(Var(0), Var(1)))(actual = minimizeFormula(And(Var(0), Var(1))))
  }

  test("Minimize.Var1AndVar2.02") {
    assertResult(expected = And(Var(0), Var(1)))(actual = minimizeFormula(And(Var(1), Var(0))))
  }

  test("Minimize.Var1AndVar2.03") {
    assertResult(expected = And(Var(0), Var(1)))(actual = minimizeFormula(And(Var(0), And(Var(1), Var(1)))))
  }

  test("Minimize.Var1AndVar2.04") {
    assertResult(expected = And(Var(0), Var(1)))(actual = minimizeFormula(And(And(Var(1), Var(1)), Var(0))))
  }

  test("Minimize.Var1AndVar2.05") {
    assertResult(expected = False)(actual = minimizeFormula(And(And(Var(1), Not(Var(1))), Var(0))))
  }

  test("Minimize.Var1AndVar2.06") {
    assertResult(expected = Var(0))(actual = minimizeFormula(Or(Var(0), Var(0))))
  }

  test("Minimize.Var1AndVar2.07") {
    assertResult(expected = True)(actual = minimizeFormula(Or(Var(0), Not(Var(0)))))
  }

  test("Minimize.Var1AndVar2.08") {
    assertResult(expected = Or(Var(0), Var(1)))(actual = minimizeFormula(Or(Var(0), Var(1))))
  }

  test("Minimize.Var1AndVar2.09") {
    assertResult(expected = Or(Var(0), Var(1)))(actual = minimizeFormula(Or(Var(1), Var(0))))
  }

  test("Minimize.Var1AndVar2.10") {
    assertResult(expected = Var(0))(actual = minimizeFormula(And(Or(Var(1), Var(0)), Var(0))))
  }

  test("Minimize.Var1AndVar2.11") {
    assertResult(expected = Or(Var(0), Var(1)))(actual = minimizeFormula(Or(Or(Var(1), Var(0)), Var(0))))
  }

  test("Minimize.Var1AndVar2.12") {
    assertResult(expected = Var(0))(actual = minimizeFormula(Or(And(Var(1), Var(0)), Var(0))))
  }

  test("Minimize.Var1AndVar2.13") {
    assertResult(expected = Var(0))(actual = minimizeFormula(And(Var(0), Or(Var(1), Var(0)))))
  }

  test("Minimize.Var1AndVar2.14") {
    assertResult(expected = Or(Var(0), Var(1)))(actual = minimizeFormula(Or(Var(0), Or(Var(1), Var(0)))))
  }

  test("Minimize.Var1AndVar2.15") {
    assertResult(expected = Var(0))(actual = minimizeFormula(Or(Var(0), And(Var(1), Var(0)))))
  }

  test("Minimize.Var1AndVar2.16") {
    assertResult(expected = Not(Var(0)))(actual = minimizeFormula(Not(Or(Var(0), And(Var(1), Var(0))))))
  }

  test("Minimize.Var1AndVar2AndVar3.01") {
    assertResult(expected = And(Var(0), And(Var(1), Var(2))))(actual = minimizeFormula(And(And(Var(0), Var(1)), Var(2))))
  }

  test("Minimize.Var1AndVar2AndVar3.02") {
    assertResult(expected = And(Var(0), And(Var(1), Var(2))))(actual = minimizeFormula(And(And(Var(2), Var(1)), Var(0))))
  }

  test("Minimize.Var1AndVar2AndVar3.03") {
    assertResult(expected = Or(Var(0), Or(Var(1), Var(2))))(actual = minimizeFormula(Or(Or(Var(0), Var(1)), Var(2))))
  }

  test("Minimize.Var1AndVar2AndVar3.04") {
    assertResult(expected = Or(Var(0), Or(Var(1), Var(2))))(actual = minimizeFormula(Or(Or(Var(2), Var(1)), Var(0))))
  }

  test("Minimize.Var1AndVar2AndVar3.05") {
    assertResult(expected = And(Var(0), And(Var(1), Var(2))))(actual = minimizeFormula(And(And(And(Var(0), Var(1)), Var(2)), And(And(Var(0), Var(1)), Var(2)))))
  }

  test("Minimize.Var1AndVar2AndVar3.06") {
    assertResult(expected = And(Var(0), And(Var(1), Var(2))))(actual = minimizeFormula(And(And(And(Var(2), Var(1)), Var(0)), And(And(Var(0), Var(1)), Var(2)))))
  }

  test("Minimize.Var1AndVar2AndVar3.07") {
    assertResult(expected = False)(actual = minimizeFormula(And(And(And(Var(2), Var(1)), Var(0)), Not(And(And(Var(0), Var(1)), Var(2))))))
  }

  test("Minimize.Var1AndVar2AndVar3.08") {
    assertResult(expected = True)(actual = minimizeFormula(Or(And(And(Var(2), Var(1)), Var(0)), Not(And(And(Var(0), Var(1)), Var(2))))))
  }

  test("Minimize.Var1AndVar2AndVar3.09") {
    assertResult(expected = And(Var(0), And(Var(1), Var(2))))(actual = minimizeFormula(And(And(And(Var(2), Var(1)), Var(0)), Or(Var(2), And(Var(2), Var(2))))))
  }

  test("Minimize.Var1AndVar2AndVar3.10") {
    assertResult(expected = Or(Var(0), Or(Var(1), Var(2))))(actual = minimizeFormula(Or(Var(0), Or(Var(1), Or(Var(2), Or(Var(0), Or(Var(1), Var(2))))))))
  }

  test("Minimize.Var1AndVar2AndVar3.11") {
    assertResult(expected = True)(actual = minimizeFormula(Or(Not(Var(0)), Or(Var(1), Or(Var(2), Or(Var(0), Or(Var(1), Var(2))))))))
  }

  test("Minimize.Var1AndVar2AndVar3.12") {
    assertResult(expected = Or(Var(0), Or(Var(1), Var(2))))(actual = minimizeFormula(Or(Or(Var(2), Or(Var(1), Var(0))), And(Var(0), And(Var(0), And(Var(2), Or(Var(0), Or(Var(1), Var(2)))))))))
  }

  test("Minimize.Var1AndVar2AndVar3.13") {
    assertResult(expected = And(Var(0), Var(2)))(actual = minimizeFormula(And(Or(Var(1), Not(Var(1))), And(Var(2), Var(0)))))
  }

  test("Minimize.Var1AndVar2AndVar3.14") {
    assertResult(expected = Var(0))(actual = minimizeFormula(Or(Var(0), And(And(Var(1), Not(Var(1))), Or(Var(2), And(Var(2), Var(1)))))))
  }

  test("Minimize.Var1AndVar2AndVar3AndVar4.01") {
    assertResult(expected = And(Var(0), And(Var(1), And(Var(2), Var(3)))))(actual = minimizeFormula(And(And(Var(0), Var(1)), And(Var(2), Var(3)))))
  }

  test("Minimize.Var1AndVar2AndVar3AndVar4.02") {
    assertResult(expected = And(Var(0), And(Var(1), And(Var(2), Var(3)))))(actual = minimizeFormula(And(And(Var(3), Var(2)), And(Var(0), Var(1)))))
  }

  test("Minimize.Var1AndVar2AndVar3AndVar4.03") {
    assertResult(expected = Or(Var(0), Or(Var(1), Or(Var(2), Var(3)))))(actual = minimizeFormula(Or(Or(Var(0), Var(1)), Or(Var(2), Var(3)))))
  }

  test("Minimize.Var1AndVar2AndVar3AndVar4.04") {
    assertResult(expected = And(Or(Not(Var(0)), Not(Var(1))), Or(Not(Var(2)), Not(Var(3)))))(actual = minimizeFormula(Not(Or(And(Not(Not(Var(0))), Not(Not(Var(1)))), And(Not(Not(Var(2))), Not(Not(Var(3))))))))
  }

}

/*
 *  Copyright 2023 Anna Blume Jakobsen
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

package ca.uwaterloo.flix.language.phase.unification

import ca.uwaterloo.flix.language.phase.unification.BoolFormula._
import org.scalatest.funsuite.AnyFunSuite

class TestQMCtoBoolFormula extends AnyFunSuite {

  val alg: BoolFormulaAlg = new BoolFormulaAlg()

  test("Minimize.True.01") {
    assertResult(expected = True)(actual = alg.minimizeQMC(True))
  }

  test("Minimize.True.02") {
    assertResult(expected = True)(actual = alg.minimizeQMC(Not(False)))
  }

  test("Minimize.True.03") {
    assertResult(expected = True)(actual = alg.minimizeQMC(And(True, True)))
  }

  test("Minimize.True.04") {
    assertResult(expected = True)(actual = alg.minimizeQMC(Or(True, True)))
  }

  test("Minimize.True.05") {
    assertResult(expected = True)(actual = alg.minimizeQMC(Or(True, False)))
  }

  test("Minimize.True.06") {
    assertResult(expected = True)(actual = alg.minimizeQMC(Or(False, True)))
  }

  test("Minimize.False.01") {
    assertResult(expected = False)(actual = alg.minimizeQMC(False))
  }

  test("Minimize.False.02") {
    assertResult(expected = True)(actual = alg.minimizeQMC(Not(False)))
  }

  test("Minimize.False.03") {
    assertResult(expected = False)(actual = alg.minimizeQMC(And(False, False)))
  }

  test("Minimize.False.04") {
    assertResult(expected = False)(actual = alg.minimizeQMC(And(Not(False), False)))
  }

  test("Minimize.False.05") {
    assertResult(expected = False)(actual = alg.minimizeQMC(And(False, Not(False))))
  }

  test("Minimize.False.06") {
    assertResult(expected = True)(actual = alg.minimizeQMC(And(Not(False), Not(False))))
  }

  test("Minimize.False.07") {
    assertResult(expected = False)(actual = alg.minimizeQMC(Or(False, False)))
  }

  test("Minimize.False.08") {
    assertResult(expected = True)(actual = alg.minimizeQMC(Or(Not(False), False)))
  }

  test("Minimize.False.09") {
    assertResult(expected = True)(actual = alg.minimizeQMC(Or(False, Not(False))))
  }

  test("Minimize.False.10") {
    assertResult(expected = True)(actual = alg.minimizeQMC(Or(Not(False), Not(False))))
  }

  test("Minimize.False.11") {
    assertResult(expected = False)(actual = alg.minimizeQMC(Not(Not(False))))
  }

  test("Minimize.Var1.01") {
    assertResult(expected = Var(0))(actual = alg.minimizeQMC(Var(0)))
  }

  test("Minimize.Var1.02") {
    assertResult(expected = Var(0))(actual = alg.minimizeQMC(Not(Not(Var(0)))))
  }

  test("Minimize.Var1.03") {
    assertResult(expected = Var(0))(actual = alg.minimizeQMC(And(Var(0), Var(0))))
  }

  test("Minimize.Var1.04") {
    assertResult(expected = Var(0))(actual = alg.minimizeQMC(And(Var(0), True)))
  }

  test("Minimize.Var1.05") {
    assertResult(expected = Var(0))(actual = alg.minimizeQMC(And(True, Var(0))))
  }

  test("Minimize.Var1.06") {
    assertResult(expected = Var(0))(actual = alg.minimizeQMC(Or(Var(0), Var(0))))
  }

  test("Minimize.Var1.07") {
    assertResult(expected = True)(actual = alg.minimizeQMC(Or(Var(0), True)))
  }

  test("Minimize.Var1.08") {
    assertResult(expected = True)(actual = alg.minimizeQMC(Or(True, Var(0))))
  }

  test("Minimize.Var1.09") {
    assertResult(expected = Var(0))(actual = alg.minimizeQMC(Or(Var(0), Not(True))))
  }

  test("Minimize.Var1.10") {
    assertResult(expected = Var(0))(actual = alg.minimizeQMC(Or(Not(True), Var(0))))
  }

  test("Minimize.Var1.11") {
    assertResult(expected = Not(Var(0)))(actual = alg.minimizeQMC(And(True, Not(Var(0)))))
  }

  test("Minimize.Var1.12") {
    assertResult(expected = False)(actual = alg.minimizeQMC(And(False, Not(Var(0)))))
  }

  test("Minimize.Var1.13") {
    assertResult(expected = Not(Var(0)))(actual = alg.minimizeQMC(Or(False, Not(Var(0)))))
  }

  test("Minimize.Var1.14") {
    assertResult(expected = True)(actual = alg.minimizeQMC(Or(True, Not(Var(0)))))
  }

  test("Minimize.Var1AndVar2.01") {
    assertResult(expected = And(Var(0), Var(1)))(actual = alg.minimizeQMC(And(Var(0), Var(1))))
  }

  test("Minimize.Var1AndVar2.02") {
    assertResult(expected = And(Var(0), Var(1)))(actual = alg.minimizeQMC(And(Var(1), Var(0))))
  }

  test("Minimize.Var1AndVar2.03") {
    assertResult(expected = And(Var(0), Var(1)))(actual = alg.minimizeQMC(And(Var(0), And(Var(1), Var(1)))))
  }

  test("Minimize.Var1AndVar2.04") {
    assertResult(expected = And(Var(0), Var(1)))(actual = alg.minimizeQMC(And(And(Var(1), Var(1)), Var(0))))
  }

  test("Minimize.Var1AndVar2.05") {
    assertResult(expected = False)(actual = alg.minimizeQMC(And(And(Var(1), Not(Var(1))), Var(0))))
  }

  test("Minimize.Var1AndVar2.06") {
    assertResult(expected = Var(0))(actual = alg.minimizeQMC(Or(Var(0), Var(0))))
  }

  test("Minimize.Var1AndVar2.07") {
    assertResult(expected = True)(actual = alg.minimizeQMC(Or(Var(0), Not(Var(0)))))
  }

  test("Minimize.Var1AndVar2.08") {
    assertResult(expected = Or(Var(0), Var(1)))(actual = alg.minimizeQMC(Or(Var(0), Var(1))))
  }

  test("Minimize.Var1AndVar2.09") {
    assertResult(expected = Or(Var(0), Var(1)))(actual = alg.minimizeQMC(Or(Var(1), Var(0))))
  }

  test("Minimize.Var1AndVar2.10") {
    assertResult(expected = Var(0))(actual = alg.minimizeQMC(And(Or(Var(1), Var(0)), Var(0))))
  }

  test("Minimize.Var1AndVar2.11") {
    assertResult(expected = Or(Var(0), Var(1)))(actual = alg.minimizeQMC(Or(Or(Var(1), Var(0)), Var(0))))
  }

  test("Minimize.Var1AndVar2.12") {
    assertResult(expected = Var(0))(actual = alg.minimizeQMC(Or(And(Var(1), Var(0)), Var(0))))
  }

  test("Minimize.Var1AndVar2.13") {
    assertResult(expected = Var(0))(actual = alg.minimizeQMC(And(Var(0), Or(Var(1), Var(0)))))
  }

  test("Minimize.Var1AndVar2.14") {
    assertResult(expected = Or(Var(0), Var(1)))(actual = alg.minimizeQMC(Or(Var(0), Or(Var(1), Var(0)))))
  }

  test("Minimize.Var1AndVar2.15") {
    assertResult(expected = Var(0))(actual = alg.minimizeQMC(Or(Var(0), And(Var(1), Var(0)))))
  }

  test("Minimize.Var1AndVar2.16") {
    assertResult(expected = Not(Var(0)))(actual = alg.minimizeQMC(Not(Or(Var(0), And(Var(1), Var(0))))))
  }

  test("Minimize.Var1AndVar2AndVar3.01") {
    assertResult(expected = And(And(Var(0), Var(1)), Var(2)))(actual = alg.minimizeQMC(And(And(Var(0), Var(1)), Var(2))))
  }

  test("Minimize.Var1AndVar2AndVar3.02") {
    assertResult(expected = And(And(Var(0), Var(1)), Var(2)))(actual = alg.minimizeQMC(And(And(Var(2), Var(1)), Var(0))))
  }

  test("Minimize.Var1AndVar2AndVar3.03") {
    assertResult(expected = Or(Or(Var(2), Var(0)), Var(1)))(actual = alg.minimizeQMC(Or(Or(Var(0), Var(1)), Var(2))))
  }

  test("Minimize.Var1AndVar2AndVar3.04") {
    assertResult(expected = Or(Or(Var(2), Var(0)), Var(1)))(actual = alg.minimizeQMC(Or(Or(Var(2), Var(1)), Var(0))))
  }

  test("Minimize.Var1AndVar2AndVar3.05") {
    assertResult(expected = And(And(Var(0), Var(1)), Var(2)))(actual = alg.minimizeQMC(And(And(And(Var(0), Var(1)), Var(2)), And(And(Var(0), Var(1)), Var(2)))))
  }

  test("Minimize.Var1AndVar2AndVar3.06") {
    assertResult(expected = And(And(Var(0), Var(1)), Var(2)))(actual = alg.minimizeQMC(And(And(And(Var(2), Var(1)), Var(0)), And(And(Var(0), Var(1)), Var(2)))))
  }

  test("Minimize.Var1AndVar2AndVar3.07") {
    assertResult(expected = False)(actual = alg.minimizeQMC(And(And(And(Var(2), Var(1)), Var(0)), Not(And(And(Var(0), Var(1)), Var(2))))))
  }

  test("Minimize.Var1AndVar2AndVar3.08") {
    assertResult(expected = True)(actual = alg.minimizeQMC(Or(And(And(Var(2), Var(1)), Var(0)), Not(And(And(Var(0), Var(1)), Var(2))))))
  }

  test("Minimize.Var1AndVar2AndVar3.09") {
    assertResult(expected = And(And(Var(0), Var(1)), Var(2)))(actual = alg.minimizeQMC(And(And(And(Var(2), Var(1)), Var(0)), Or(Var(2), And(Var(2), Var(2))))))
  }

  test("Minimize.Var1AndVar2AndVar3.10") {
    assertResult(expected = Or(Or(Var(2), Var(0)), Var(1)))(actual = alg.minimizeQMC(Or(Var(0), Or(Var(1), Or(Var(2), Or(Var(0), Or(Var(1), Var(2))))))))
  }

  test("Minimize.Var1AndVar2AndVar3.11") {
    assertResult(expected = True)(actual = alg.minimizeQMC(Or(Not(Var(0)), Or(Var(1), Or(Var(2), Or(Var(0), Or(Var(1), Var(2))))))))
  }

  test("Minimize.Var1AndVar2AndVar3.12") {
    assertResult(expected = Or(Or(Var(2), Var(0)), Var(1)))(actual = alg.minimizeQMC(Or(Or(Var(2), Or(Var(1), Var(0))), And(Var(0), And(Var(0), And(Var(2), Or(Var(0), Or(Var(1), Var(2)))))))))
  }

  test("Minimize.Var1AndVar2AndVar3.13") {
    assertResult(expected = And(Var(0), Var(2)))(actual = alg.minimizeQMC(And(Or(Var(1), Not(Var(1))), And(Var(2), Var(0)))))
  }

  test("Minimize.Var1AndVar2AndVar3.14") {
    assertResult(expected = Var(0))(actual = alg.minimizeQMC(Or(Var(0), And(And(Var(1), Not(Var(1))), Or(Var(2), And(Var(2), Var(1)))))))
  }

  test("Minimize.Var1AndVar2AndVar3AndVar4.01") {
    assertResult(expected = And(And(And(Var(0), Var(1)), Var(2)), Var(3)))(actual = alg.minimizeQMC(And(And(Var(0), Var(1)), And(Var(2), Var(3)))))
  }

  test("Minimize.Var1AndVar2AndVar3AndVar4.02") {
    assertResult(expected = And(And(And(Var(0), Var(1)), Var(2)), Var(3)))(actual = alg.minimizeQMC(And(And(Var(3), Var(2)), And(Var(0), Var(1)))))
  }

  test("Minimize.Var1AndVar2AndVar3AndVar4.03") {
    assertResult(expected = Or(Or(Or(Var(1), Var(3)), Var(0)), Var(2)))(actual = alg.minimizeQMC(Or(Or(Var(0), Var(1)), Or(Var(2), Var(3)))))
  }

  test("Minimize.Var1AndVar2AndVar3AndVar4.04") {
    assertResult(expected = Or(Or(Or(And(Not(Var(0)), Not(Var(2))), And(Not(Var(1)), Not(Var(2)))), And(Not(Var(0)), Not(Var(3)))), And(Not(Var(1)), Not(Var(3)))))(actual = alg.minimizeQMC(Not(Or(And(Not(Not(Var(0))), Not(Not(Var(1)))), And(Not(Not(Var(2))), Not(Not(Var(3))))))))
  }

  test("Minimize.Var1AndVar2AndVar3AndVar4AndVar5AndVar6.01") {
    assertResult(expected = Or(Or(Or(Not(Var(1)), And(Not(Var(0)), Var(5))), And(Var(4), Var(5))), Not(Var(2))))(actual = alg.minimizeQMC(Or(And(Var(5), Not(And(Var(1), Var(0)))), Or(Not(And(Or(And(Not(And(Var(3), Var(5))), And(Var(4), Var(2))), And(Var(2), Var(2))), Var(1))), And(Var(4), Var(5))))))
  }

  test("Minimize.Var1AndVar2AndVar3AndVar4AndVar5AndVar6.02") {
    assertResult(expected = Or(Or(Not(Var(0)), Var(3)), Not(Var(2))))(actual = alg.minimizeQMC(Or(Not(And(Not(And(Var(3), Var(3))), Var(2))), Not(And(Var(0), Or(And(Var(3), Var(2)), Not(And(And(Var(1), Or(Var(4), Not(And(And(Var(5), Var(0)), Var(1))))), Var(3)))))))))
  }

  test("Minimize.Var1AndVar2AndVar3AndVar4AndVar5AndVar6.03") {
    assertResult(expected = True)(actual = alg.minimizeQMC(Or(Or(Or(Not(And(Or(Not(And(Var(2), Var(1))), And(Var(1), Var(1))), Var(3))), And(Var(3), True)), Not(And(Or(Not(Var(4)), And(Var(5), Var(0))), Var(4)))), And(Var(0), Var(4)))))
  }

  test("Minimize.Var1AndVar2AndVar3AndVar4AndVar5AndVar6.04") {
    assertResult(expected = False)(actual = alg.minimizeQMC(And(And(And(And(And(And(And(And(And(And(And(Var(0), Var(5)), Not(Var(1))), Not(Var(4))), Var(1)), Not(Var(3))), Not(Var(0))), Var(3)), Var(4)), Not(Var(2))), Var(2)), Not(Var(5)))))
  }

  test("Minimize.Var1AndVar2AndVar3AndVar4AndVar5AndVar6.05") {
    assertResult(expected = Or(Or(Or(Or(And(Var(2), Var(3)), And(And(Not(Var(0)), Not(Var(1))), Var(3))), And(Not(Var(1)), Var(2))), And(Var(2), Not(Var(4)))), And(Not(Var(0)), Var(5))))(actual = alg.minimizeQMC(Or(And(Or(Not(And(And(Var(2), Var(4)), Var(1))), And(And(True, Var(4)), Var(3))), Var(2)), And(And(Not(Var(0)), True), Or(Var(5), And(Var(3), Not(Var(1))))))))
  }

}

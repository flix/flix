package ca.uwaterloo.flix.language.phase.unification

import ca.uwaterloo.flix.TestUtils
import ca.uwaterloo.flix.language.ast.SourceLocation
import org.scalatest.funsuite.AnyFunSuite
import org.sosy_lab.pjbdd.api.{Creator, DD}

class TestBdd extends AnyFunSuite with TestUtils {

  val loc: SourceLocation = SourceLocation.Unknown
  val builder: Creator = BddFormulaAlg.GlobalBddBuilder
  val F: DD = builder.makeFalse()
  val T: DD = builder.makeTrue()
  val x0: DD = builder.makeIthVar(0)
  val x1: DD = builder.makeIthVar(1)
  val x2: DD = builder.makeIthVar(2)

  test("Bdd.True") {
    /*
    *       T
    */
    val dd = T

    assertResult(dd.isLeaf)(true)
    assertResult(dd.isFalse)(false)
    assertResult(dd.isTrue)(true)
  }

  test("Bdd.False") {
    /*
    *       F
    */
    val dd = F

    assertResult(dd.isLeaf)(true)
    assertResult(dd.isFalse)(true)
    assertResult(dd.isTrue)(false)
  }

  test("Bdd.Var.01") {
    /*
    *       4
    *      / \
    *     F  T
    */
    val dd = builder.makeIthVar(4)

    assertResult(dd.isLeaf)(false)
    assertResult(dd.getVariable)(4)
    assertResult(dd.getLow)(F)
    assertResult(dd.getHigh)(T)
  }

  test("Bdd.Var.02") {
    /*
    *       15
    *      / \
    *     F  T
    */
    val dd = builder.makeIthVar(15)

    assertResult(dd.isLeaf)(false)
    assertResult(dd.getVariable)(15)
    assertResult(dd.getLow)(F)
    assertResult(dd.getHigh)(T)
  }

  test("Bdd.Not.01") {
    /*
    *       0          0
    * not  / \   =>   / \
    *     F  T       T  F
    */
    val dd = builder.makeNot(x0)

    assertResult(dd.getVariable)(0)
    assertResult(dd.getLow)(T)
    assertResult(dd.getHigh)(F)
  }

  test("Bdd.Not.02") {
    /*
    *       1          1
    *      / \        / \
    * not |  2   =>  |  2
    *     |/ \       |/ \
    *     F  T       T  F
    */
    val dd = builder.makeNot(builder.makeNode(F, x2, 1))

    assertResult(dd.getVariable)(1)
    assertResult(dd.getLow)(T)

    val high = dd.getHigh
    assertResult(high.getVariable)(2)
    assertResult(high.getLow)(T)
    assertResult(high.getHigh)(F)
  }

  test("Bdd.And.01") {
    /*
    *       0          1          0
    *      / \   /\   / \   =>   / \
    *     F  T       F  T       F  1
    *                             / \
    *                            F  T
    */
    val dd = builder.makeAnd(x0, x1)

    assertResult(dd.getVariable)(0)
    assertResult(dd.getLow)(F)

    val high = dd.getHigh
    assertResult(high.getVariable)(1)
    assertResult(high.getLow)(F)
    assertResult(high.getHigh)(T)
  }

  test("Bdd.And.02") {
    assertResult(builder.makeAnd(x0, x1))(builder.makeAnd(x1, x0))
  }

  test("Bdd.And.03") {
    /*
    *       0          1          0
    *      / \   /\   / \   =>   / \
    *     F  2       3  T       F  1
    *       / \     / \           / \
    *      T  F    F  T         2    2
    *                          / \  / \
    *                         3  F T  F
    *                        / \
    *                       F  T
    */
    val dd1 = builder.makeNode(F, builder.makeNode(T, F, 2), 0)
    val dd2 = builder.makeNode(builder.makeNode(F, T, 3), T, 1)
    val dd = builder.makeAnd(dd1, dd2)

    assertResult(dd.getVariable)(0)
    assertResult(dd.getLow)(F)

    val high0 = dd.getHigh
    assertResult(high0.getVariable)(1)

    val low1 = high0.getLow
    assertResult(low1.getVariable)(2)
    assertResult(low1.getHigh)(F)

    val low2 = low1.getLow
    assertResult(low2.getVariable)(3)
    assertResult(low2.getLow)(F)
    assertResult(low2.getHigh)(T)

    val high1 = high0.getHigh
    assertResult(high1.getVariable)(2)
    assertResult(high1.getLow)(T)
    assertResult(high1.getHigh)(F)
  }

  test("Bdd.And.04") {
    /*
    *       0          1          0
    *      / \   /\   / \   =>   / \
    *     2  T       F  2       F   1
    *    / \           / \         / \
    *   F  T          T  F        F  2
    *                               / \
    *                              T  F
    */
    val dd1 = builder.makeNode(builder.makeNode(F, T, 2), T, 0)
    val dd2 = builder.makeNode(F, builder.makeNode(T, F, 2), 1)
    val dd = builder.makeAnd(dd1, dd2)

    assertResult(dd.getVariable)(0)
    assertResult(dd.getLow)(F)

    val high0 = dd.getHigh
    assertResult(high0.getVariable)(1)
    assertResult(high0.getLow)(F)

    val high1 = high0.getHigh
    assertResult(high1.getVariable)(2)
    assertResult(high1.getLow)(T)
    assertResult(high1.getHigh)(F)
  }

  test("Bdd.Or.01") {
    /*
    *       0          1           0
    *      / \   \/   / \   =>    / \
    *     F  T       F  T        1  T
    *                           / \
    *                          F  T
    */
    val dd = builder.makeOr(x0, x1)

    assertResult(dd.getVariable)(0)
    assertResult(dd.getHigh)(T)

    val low = dd.getLow
    assertResult(low.getVariable)(1)
    assertResult(low.getLow)(F)
    assertResult(low.getHigh)(T)
  }

  test("Bdd.Or.02") {
    assertResult(builder.makeOr(x0, x1))(builder.makeOr(x1, x0))
  }

  test("Bdd.Or.03") {
    /*
    *       0          1           0
    *      / \   \/   / \   =>   /   \
    *     F  2       3  T      1      1
    *       / \     / \       / \    / \
    *      T  F    F  T      |   T  2  T
    *                        |     / \
    *                        3    T  3
    *                       / \     / \
    *                      F  T    F  T
    */
    val dd1 = builder.makeNode(F, builder.makeNode(T, F, 2), 0)
    val dd2 = builder.makeNode(builder.makeNode(F, T, 3), T, 1)
    val dd = builder.makeOr(dd1, dd2)

    assertResult(dd.getVariable)(0)

    val low0 = dd.getLow
    assertResult(low0.getVariable)(1)
    assertResult(low0.getHigh)(T)

    val low1_1 = low0.getLow
    assertResult(low1_1.getVariable)(3)
    assertResult(low1_1.getLow)(F)
    assertResult(low1_1.getHigh)(T)

    val high0 = dd.getHigh
    assertResult(high0.getVariable)(1)
    assertResult(high0.getHigh)(T)

    val low1_2 = high0.getLow
    assertResult(low1_2.getVariable)(2)
    assertResult(low1_2.getLow)(T)

    val high2 = low1_2.getHigh
    assertResult(high2.getVariable)(3)
    assertResult(high2.getLow)(F)
    assertResult(high2.getHigh)(T)
  }

  test("Bdd.Or.04") {
    /*
    *       0          1          0
    *      / \   \/   / \   =>   / \
    *     2  T       F  2       1   T
    *    / \           / \     / \
    *   F  T          T  F    2  T
    *                        / \
    *                       F  T
    */
    val dd1 = builder.makeNode(builder.makeNode(F, T, 2), T, 0)
    val dd2 = builder.makeNode(F, builder.makeNode(T, F, 2), 1)
    val dd = builder.makeOr(dd1, dd2)

    assertResult(dd.getVariable)(0)
    assertResult(dd.getHigh)(T)

    val low0 = dd.getLow
    assertResult(low0.getVariable)(1)
    assertResult(low0.getHigh)(T)

    val low1 = low0.getLow
    assertResult(low1.getVariable)(2)
    assertResult(low1.getLow)(F)
    assertResult(low1.getHigh)(T)
  }

  test("Bdd.Xor.01") {
    /*
    *       0           1           0
    *      / \   XOR   / \   =>    / \
    *     F  T        F  T        1  1
    *                            / \/ \
    *                           F  T  F
    */
    val dd = builder.makeXor(x0, x1)
    assertResult(dd.getVariable)(0)
    val low = dd.getLow
    assertResult(low.getVariable)(1)
    assertResult(low.getLow)(F)
    assertResult(low.getHigh)(T)
    val high = dd.getHigh
    assertResult(high.getVariable)(1)
    assertResult(high.getLow)(T)
    assertResult(high.getHigh)(F)
  }

  test("Bdd.Xor.02") {
    assertResult(builder.makeXor(x0, x1))(builder.makeXor(x1, x0))
  }

  test("Bdd.Xor.03") {
    /*
    *       0           1           0
    *      / \   XOR   / \   =>   /   \
    *     F  2        3  T       1      1
    *       / \      / \        / \   /   \
    *      T  F     F  T       |  T  2     2
    *                          |    / \   / \
    *                          3   3   3  F T
    *                         / \ / \ / \
    *                         F  T   F  T
    */
    val dd1 = builder.makeNode(F, builder.makeNode(T, F, 2), 0)
    val dd2 = builder.makeNode(builder.makeNode(F, T, 3), T, 1)
    val dd = builder.makeXor(dd1, dd2)

    assertResult(dd.getVariable)(0)

    val low0 = dd.getLow
    assertResult(low0.getVariable)(1)
    assertResult(low0.getHigh)(T)

    val low1_1 = low0.getLow
    assertResult(low1_1.getVariable)(3)
    assertResult(low1_1.getLow)(F)
    assertResult(low1_1.getHigh)(T)

    val high0 = dd.getHigh
    assertResult(high0.getVariable)(1)

    val low1_2 = high0.getLow
    assertResult(low1_2.getVariable)(2)

    val low2 = low1_2.getLow
    assertResult(low2.getVariable)(3)
    assertResult(low2.getLow)(T)
    assertResult(low2.getHigh)(F)

    val high2 = low1_2.getHigh
    assertResult(high2.getVariable)(3)
    assertResult(high2.getLow)(F)
    assertResult(high2.getHigh)(T)

    val high1 = high0.getHigh
    assertResult(high1.getVariable)(2)
    assertResult(high1.getLow)(F)
    assertResult(high1.getHigh)(T)
  }

  test("Bdd.Xor.04") {
    /*
    *       0           1          0
    *      / \   XOR   / \   =>   / \
    *     2  T        F  2       1  1
    *    / \            / \     / \/ \
    *   F  T           T  F    2  T  2
    *                         / \   / \
    *                        F  T  F  T
    */
    val dd1 = builder.makeNode(builder.makeNode(F, T, 2), T, 0)
    val dd2 = builder.makeNode(F, builder.makeNode(T, F, 2), 1)
    val dd = builder.makeXor(dd1, dd2)

    assertResult(dd.getVariable)(0)

    val low0 = dd.getLow
    assertResult(low0.getVariable)(1)
    assertResult(low0.getHigh)(T)

    val low1 = low0.getLow
    assertResult(low1.getVariable)(2)
    assertResult(low1.getLow)(F)
    assertResult(low1.getHigh)(T)

    val high0 = dd.getHigh
    assertResult(high0.getVariable)(1)
    assertResult(high0.getLow)(T)

    val high1 = high0.getHigh
    assertResult(high1.getVariable)(2)
    assertResult(high1.getLow)(F)
    assertResult(high1.getHigh)(T)
  }

  test("Bdd.ITE.01") {
    assertResult(builder.makeIte(x0, T, F))(x0)
  }

  test("Bdd.ITE.02") {
    assertResult(builder.makeIte(x0, F, T))(builder.makeNot(x0))
  }

  test("Bdd.ITE.03") {
    assertResult(builder.makeIte(F, x1, x2))(x2)
  }

  test("Bdd.ITE.04") {
    assertResult(builder.makeIte(T, x1, x2))(x1)
  }

  test("Bdd.ITE.05") {
    assertResult(builder.makeIte(x0, x1, x1))(x1)
  }

  test("Bdd.ITE.06") {
    /*
    *       0          1          2          0
    *  if  / \  then  / \  else  / \  =>    / \
    *     F  T       F  T       F  T       /   1
    *                                     /   / \
    *                                    2   F  T
    *                                   / \
    *                                  F  T
    */
    val dd = builder.makeIte(x0, x1, x2)

    assertResult(dd.getVariable)(0)

    val low = dd.getLow
    assertResult(low.getVariable)(2)
    assertResult(low.getLow)(F)
    assertResult(low.getHigh)(T)

    val high = dd.getHigh
    assertResult(high.getVariable)(1)
    assertResult(high.getLow)(F)
    assertResult(high.getHigh)(T)
  }

  test("Bdd.ITE.07") {
    /*
    *       2          1          0           0
    *  if  / \  then  / \  else  / \  =>    /   \
    *     F  T       F  T       F  T       1     1
    *                                     / \   / \
    *                                    F  2  2  T
    *                                      / \/ \
    *                                     F  T  F
    */
    val dd = builder.makeIte(x2, x1, x0)

    assertResult(dd.getVariable)(0)

    val low0 = dd.getLow
    assertResult(low0.getVariable)(1)
    assertResult(low0.getLow)(F)

    val high1 = low0.getHigh
    assertResult(high1.getVariable)(2)
    assertResult(high1.getLow)(F)
    assertResult(high1.getHigh)(T)

    val high0 = dd.getHigh
    assertResult(high0.getVariable)(1)
    assertResult(high0.getHigh)(T)

    val low1 = high0.getLow
    assertResult(low1.getVariable)(2)
    assertResult(low1.getLow)(T)
    assertResult(low1.getHigh)(F)
  }

  test("Bdd.ITE.08") {
    /*
    *       0           1          4             0
    *  if  / \  then   / \  else  / \  =>     /    \
    *     F  2        3  T       T  F        |       1
    *       / \      / \                    |      /  \
    *      T  F     F  T                   |      2    2
    *                                     |      / \  | \
    *                                    |      3   \ T  \
    *                                   |      / \  |    |
    *                                   4     F  T  4    4
    *                                  / \         / \  / \
    *                                 T  F        T  F T  F
    */
    val dd1 = builder.makeNode(F, builder.makeNode(T, F, 2), 0)
    val dd2 = builder.makeNode(builder.makeNode(F, T, 3), T, 1)
    val dd3 = builder.makeNode(T, F, 4)
    val dd = builder.makeIte(dd1, dd2, dd3)

    assertResult(dd.getVariable)(0)

    val low0 = dd.getLow
    assertResult(low0.getVariable)(4)
    assertResult(low0.getLow)(T)
    assertResult(low0.getHigh)(F)

    val high0 = dd.getHigh
    assertResult(high0.getVariable)(1)

    val low1 = high0.getLow
    assertResult(low1.getVariable)(2)

    val low2 = low1.getLow
    assertResult(low2.getVariable)(3)
    assertResult(low2.getLow)(F)
    assertResult(low2.getHigh)(T)

    val high2_1 = low1.getHigh
    assertResult(high2_1.getVariable)(4)
    assertResult(high2_1.getLow)(T)
    assertResult(high2_1.getHigh)(F)

    val high1 = high0.getHigh
    assertResult(high1.getVariable)(2)
    assertResult(high1.getLow)(T)

    val high2_2 = high1.getHigh
    assertResult(high2_2.getVariable)(4)
    assertResult(high2_2.getLow)(T)
    assertResult(high2_2.getHigh)(F)
  }

  test("Bdd.ITE.09") {
    /*
    *       0           1           0           0
    *  if  / \  then   / \  else   / \   =>   /   \
    *     2  T        F  2        1  1       1     1
    *    / \            / \      / \/ \     / \   / \
    *   F  T           T  F     T  F  T    2  F  F  2
    *                                     / \      / \
    *                                    T  F     T  F
    */
    val dd1 = builder.makeNode(builder.makeNode(F, T, 2), T, 0)
    val dd2 = builder.makeNode(F, builder.makeNode(T, F, 2), 1)
    val dd3 = builder.makeNode(builder.makeNode(T, F, 1), builder.makeNode(F, T, 1), 0)
    val dd = builder.makeIte(dd1, dd2, dd3)

    assertResult(dd.getVariable)(0)

    val low0 = dd.getLow
    assertResult(low0.getVariable)(1)
    assertResult(low0.getHigh)(F)

    val low1 = low0.getLow
    assertResult(low1.getVariable)(2)
    assertResult(low1.getLow)(T)
    assertResult(low1.getHigh)(F)

    val high0 = dd.getHigh
    assertResult(high0.getVariable)(1)
    assertResult(high0.getLow)(F)

    val high1 = high0.getHigh
    assertResult(high1.getVariable)(2)
    assertResult(high1.getLow)(T)
    assertResult(high1.getHigh)(F)
  }
}

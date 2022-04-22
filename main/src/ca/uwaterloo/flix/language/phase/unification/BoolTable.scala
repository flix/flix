/*
 * Copyright 2022 Magnus Madsen
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package ca.uwaterloo.flix.language.phase.unification

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.{Kind, Symbol, Type}
import ca.uwaterloo.flix.language.phase.unification.BoolFormula._
import ca.uwaterloo.flix.util.InternalCompilerException
import ca.uwaterloo.flix.util.collection.Bimap

import scala.collection.immutable.SortedSet
import scala.collection.mutable.ListBuffer

/**
  * A Boolean minimization technique that uses on pre-computed tables of minimal formulas.
  *
  * We pre-compute (offline) a table of all formulas of up to `MaxVars` variables and
  * enumerate their minimal formula.
  *
  * We use this table to lookup the minimal formula of a given formula.
  */
object BoolTable {

  /**
    * A flag used to control whether to print debug information.
    */
  private val Debug: Boolean = false

  /**
    * The number of variables that the minimization table uses.
    *
    * Warning: If the number is set incorrectly minimization will be wrong!
    */
  private val MaxVars: Int = 3

  /**
    * The size a formula (but represented as a type) must have before we try to minimize it.
    */
  private val Threshold: Int = 10

  /**
    * A Boolean variable is represented by a unique number.
    */
  private type Variable = Int

  /**
    * A table that maps Boolean semantic functions to their minimal formulas.
    *
    * The table is pre-computed and initialized when this class is loaded.
    */
  private lazy val Table: Array[BoolFormula] = initTable()

  /**
    * Attempts to minimize the given Boolean formula `tpe`.
    *
    * Returns the same formula or a smaller formula that is equivalent.
    */
  def minimizeType(tpe: Type)(implicit flix: Flix): Type = {
    // Check whether minimization via tabling is disabled.
    if (flix.options.xnobooltable) {
      return tpe
    }

    // Check that the `tpe` argument is a Boolean formula.
    if (tpe.kind != Kind.Bool) {
      throw InternalCompilerException(s"Unexpected non-Bool kind: '${tpe.kind}'.")
    }

    // Compute the size of  `tpe`.
    val currentSize = tpe.size

    // Return `tpe` immediately if it is "small".
    if (currentSize < Threshold) {
      return tpe
    }

    // Compute the variables in `tpe`.
    val tvars = tpe.typeVars.map(_.sym).toList

    // Construct a bi-directional map from type variables to indices.
    // The idea is that the first variable becomes x0, the next x1, and so forth.
    val m = tvars.zipWithIndex.foldLeft(Bimap.empty[Symbol.KindedTypeVarSym, Variable]) {
      case (macc, (sym, x)) => macc + (sym -> x)
    }

    // Convert the type `tpe` to a Boolean formula.
    val input = fromType(tpe, m)

    // Minimize the Boolean formula.
    val minimized = minimizeFormula(input)

    // Convert the formula back to a type.
    toType(minimized, m, tpe.loc)
  }

  /**
    * Attempts to minimize the given formula `f`.
    *
    * Returns the same formula or a smaller formula.
    */
  def minimizeFormula(f: BoolFormula): BoolFormula = {
    // Compute the number of free variables.
    val numVars = f.freeVars.size

    // Determine whether to minimize once or recursively.
    val result = if (numVars <= MaxVars) {
      // Case 1: The number of variables in the formula is less than those of the table.
      // We can immediately lookup the minimal formula in the table.
      if (Debug) println(s"Minimize by lookup ($numVars variables)")
      lookup(f)
    } else {
      // Case 2: The formula has more variables than the table.
      // We try to recursively minimize each sub-formula.
      // This does not guarantee that we arrive at a minimal formula, but it is better than nothing.
      if (Debug) println(s"Minimize by recursion ($numVars variables)")
      minimizeFormulaRecursively(f)._1
    }

    // Debugging.
    if (Debug) {
      println(s"  Replace: $f")
      println(s"       By: $result")
      println(s"   Change: ${f.size} -> ${result.size}")
      println()
    }

    // Return the result.
    result
  }

  /**
    * Tries to recursively minimize the formula `f`.
    *
    * Each call returns a pair of a formula and its free variables.
    *
    * Bottom-up reconstructs a formula. Whenever we construct a conjunction or disjunction we
    * check whether we the number of free variables exceed those of the table. If so, we
    * minimize the two sub-formulas before constructing the conjunction / disjunction.
    *
    * This is not guaranteed to give a minimal representation. However, it does allow us to
    * use the tabling approach even when a formula overall has more variables than the table.
    */
  private def minimizeFormulaRecursively(f: BoolFormula): (BoolFormula, SortedSet[Variable]) = f match {
    case True => (True, SortedSet.empty)

    case False => (False, SortedSet.empty)

    case Var(x) => (Var(x), SortedSet(x))

    case Neg(formula) =>
      val (f, fvs) = minimizeFormulaRecursively(formula)
      (Neg(f), fvs)

    case Conj(formula1, formula2) =>
      // Recursive minimize each sub-formula.
      val (f1, fvs1) = minimizeFormulaRecursively(formula1)
      val (f2, fvs2) = minimizeFormulaRecursively(formula2)

      // Compute all the variables that occur in the left and right sub-formulas.
      val fvs = fvs1 ++ fvs2

      // Determine if we must minimize.
      if (fvs.size <= MaxVars) {
        // The number of free variables does not (yet) exceed the number of variables in the table.
        // Consequence we do not yet minimize. We do not yet minimize in attempt to avoid a
        // potential quadratic blow-up where we would minimize at every level.
        (Conj(f1, f2), fvs)
      } else {
        // The number of variables exceeds the number of variables in the table.
        // We minimize both the left and right sub-formulas and then construct the conjunction.
        // Note: We have to recompute the variables (since a variable could get eliminated).
        val minf1 = alphaRenameAndLookup(f1)
        val minf2 = alphaRenameAndLookup(f2)
        (Conj(minf1, minf2), minf1.freeVars ++ minf2.freeVars)
      }

    case Disj(formula1, formula2) =>
      // This case is similar to the above case.
      val (f1, fvs1) = minimizeFormulaRecursively(formula1)
      val (f2, fvs2) = minimizeFormulaRecursively(formula2)
      val fvs = fvs1 ++ fvs2

      if (fvs.size <= MaxVars) {
        (Disj(f1, f2), fvs)
      } else {
        val minf1 = alphaRenameAndLookup(f1)
        val minf2 = alphaRenameAndLookup(f2)
        (Disj(minf1, minf2), minf1.freeVars ++ minf2.freeVars)
      }
  }

  /**
    * Renames every variable in the given formula `f` and looks it up in the minimal table.
    */
  private def alphaRenameAndLookup(f: BoolFormula): BoolFormula = {
    // Compute a renaming. The first variable is x0, the next is x1, and so forth.
    val m = f.freeVars.toList.zipWithIndex.foldLeft(Bimap.empty[Variable, Variable]) {
      case (macc, (k, v)) => macc + (k -> v)
    }
    // Rename all variables, lookup the minimal formula, and then rename everything back.
    substitute(lookup(substitute(f, m)), m.swap)
  }

  /**
    * Attempts to minimize the given Boolean formula `f` using the table.
    */
  private def lookup(f: BoolFormula): BoolFormula = {
    // If the formula `f` has more variables than `f` then we cannot use the table.
    if (f.freeVars.size > MaxVars) {
      // Return the same formula.
      return f
    }

    // Computes the semantic function of `f`.
    val semantic = computeSemanticFunction(f, (0 until MaxVars).toList, 0, new Array[Boolean](MaxVars))

    // Lookup the semantic function in the table.
    Table(semantic)
  }

  /**
    * Computes the semantic function of the given formula `f` under the given environment `m`.
    *
    * @param f        the Boolean formula.
    * @param fvs      the list of free variables.
    * @param position the position in the bitvector where to store the result (true/false).
    * @param env      the environment which binds each variable to true or false.
    */
  private def computeSemanticFunction(f: BoolFormula, fvs: List[Variable], position: Int, env: Array[Boolean]): Int = fvs match {
    case Nil =>
      if (eval(f, env)) 1 << position else 0

    case x :: xs =>
      // The environment is modified in both recursive calls, hence we copy it.
      // We could probably safely re-use the original array, but we choose not to.
      val ml = env.clone()
      val mr = env.clone()
      ml(x) = true // Bind x to true.
      mr(x) = false // Bind x to false.

      // Recurse on both environments.
      val l = computeSemanticFunction(f, xs, position, ml)
      val r = computeSemanticFunction(f, xs, position + (1 << (fvs.length - 1)), mr)

      // The result is the bitwise union.
      l | r
  }

  /**
    * Evaluates the given formula `f` to a Boolean value under the given environment `env`.
    *
    * The environment must bind *all* variables in `f`.
    *
    * The environment maps the variable with index i to true or false.
    */
  private def eval(f: BoolFormula, env: Array[Boolean]): Boolean = f match {
    case True => true
    case False => false
    case Var(x) => env(x)
    case Neg(f) => !eval(f, env)
    case Conj(f1, f2) => eval(f1, env) && eval(f2, env)
    case Disj(f1, f2) => eval(f1, env) || eval(f2, env)
  }

  /**
    * Parses the built-in table into an S-expression and then into an in-memory table.
    */
  private def initTable(): Array[BoolFormula] = {
    val parsedTable = ExpressionParser.parse(Table3Vars)
    val table = parseTable(parsedTable)

    if (Debug) {
      println("== Minimization Table ==")
      println()
      for ((key, f) <- table) {
        println(s"  ${toBinaryString(key, 1 << MaxVars)}: $f")
      }
      println(s"Total Table Size = ${table.size}")
      println()
    }

    // If there are n variables, the table has size 2^(2^3).
    val array = new Array[BoolFormula](1 << (1 << MaxVars))
    for ((key, f) <- table) {
      array(key) = f
    }

    if (Debug) {
      println("== Minimization Array ==")
      println()
      for (i <- array.indices) {
        println(s"  ${toBinaryString(i, 1 << MaxVars)}: ${array(i)}")
      }
      println(s"Total Array Size = ${array.length}")
      println()
    }

    array
  }

  /**
    * Formats the given int `i` as a bit string with `n` bits.
    */
  private def toBinaryString(i: Int, n: Int): String =
    leftPad(i.toBinaryString, n)

  /**
    * Left pads `s` with `c` to reach length `len`.
    */
  private def leftPad(s: String, len: Int): String =
    ' '.toString * (len - s.length()) + s

  /**
    * Parses the given S-expression `sexp` into a map from semantic functions to their minimal formulas.
    */
  private def parseTable(sexp: SList): Map[Int, BoolFormula] = sexp match {
    case SList(elms) => elms.tail.map(parseKeyValue).toMap
  }

  /**
    * Parses the given S-expression `sexp` into a pair of a semantic function and a formula.
    */
  private def parseKeyValue(sexp: Element): (Int, BoolFormula) = sexp match {
    case SList(List(Atom(key), formula)) => parseKey(key) -> parseFormula(formula)
    case _ => throw InternalCompilerException(s"Unexpected S-expression: '$sexp'.")
  }

  /**
    * Parses the given `key` into a semantic function.
    */
  private def parseKey(key: String): Int = {
    var result = 0
    for ((c, position) <- key.zipWithIndex) {
      if (c == 'T') {
        result = result | (1 << (position - 1))
      }
    }
    result
  }

  /**
    * Parses the given S-expression `sexp` into a formula.
    */
  private def parseFormula(sexp: Element): BoolFormula = sexp match {
    case Atom("T") => True
    case Atom("F") => False
    case Atom("x0") => Var(0)
    case Atom("x1") => Var(1)
    case Atom("x2") => Var(2)
    case SList(List(Atom("not"), x)) => Neg(parseFormula(x))
    case SList(List(Atom("and"), x, y)) => Conj(parseFormula(x), parseFormula(y))
    case SList(List(Atom("or"), x, y)) => Disj(parseFormula(x), parseFormula(y))
    case _ => throw InternalCompilerException(s"Unexpected S-expression: '$sexp'.")
  }

  /**
    * The table of minimal Boolean formulas of three variables.
    */
  private val Table3Vars: String =
    """(table
      |("FFFFFFFF" F)
      |("TFFFFFFF" (and x0 (and x1 x2)))
      |("FTFFFFFF" (and x0 (and x1 (not x2))))
      |("TTFFFFFF" (and x0 x1))
      |("FFTFFFFF" (and x0 (and (not x1) x2)))
      |("TFTFFFFF" (and x0 x2))
      |("FTTFFFFF" (and x0 (and (or (not x1) (not x2)) (or x1 x2))))
      |("TTTFFFFF" (and x0 (or x1 x2)))
      |("FFFTFFFF" (and x0 (and (not x1) (not x2))))
      |("TFFTFFFF" (and x0 (or (and (not x1) (not x2)) (and x1 x2))))
      |("FTFTFFFF" (and x0 (not x2)))
      |("TTFTFFFF" (and x0 (or x1 (not x2))))
      |("FFTTFFFF" (and x0 (not x1)))
      |("TFTTFFFF" (and x0 (or (not x1) x2)))
      |("FTTTFFFF" (and x0 (or (not x1) (not x2))))
      |("TTTTFFFF" x0)
      |("FFFFTFFF" (and (not x0) (and x1 x2)))
      |("TFFFTFFF" (and x1 x2))
      |("FTFFTFFF" (and x1 (and (or (not x0) (not x2)) (or x0 x2))))
      |("TTFFTFFF" (and x1 (or x0 x2)))
      |("FFTFTFFF" (and x2 (and (or (not x0) (not x1)) (or x0 x1))))
      |("TFTFTFFF" (and x2 (or x0 x1)))
      |("FTTFTFFF" (and (or x0 x1) (and (or (not x0) (or (not x1) (not x2))) (or x2 (and x0 x1)))))
      |("TTTFTFFF" (or (and x0 x1) (and x2 (or x0 x1))))
      |("FFFTTFFF" (and (or (not x0) (not x1)) (or (and x0 (not x2)) (and x1 x2))))
      |("TFFTTFFF" (and (or (not x1) x2) (or x1 (and x0 (not x2)))))
      |("FTFTTFFF" (and (or (not x0) (not x2)) (or x0 (and x1 x2))))
      |("TTFTTFFF" (or (and x0 (not x2)) (and x1 x2)))
      |("FFTTTFFF" (and (or (not x0) (not x1)) (or x0 (and x1 x2))))
      |("TFTTTFFF" (or (and x0 (not x1)) (and x1 x2)))
      |("FTTTTFFF" (and (or (not x0) (or (not x1) (not x2))) (or x0 (and x1 x2))))
      |("TTTTTFFF" (or x0 (and x1 x2)))
      |("FFFFFTFF" (and (not x0) (and x1 (not x2))))
      |("TFFFFTFF" (and x1 (or (and (not x0) (not x2)) (and x0 x2))))
      |("FTFFFTFF" (and x1 (not x2)))
      |("TTFFFTFF" (and x1 (or x0 (not x2))))
      |("FFTFFTFF" (and (or (not x0) (not x1)) (and (or x0 (not x2)) (or x1 x2))))
      |("TFTFFTFF" (and (or (not x0) x2) (or x0 (and x1 (not x2)))))
      |("FTTFFTFF" (and (or (not x1) (not x2)) (or x1 (and x0 x2))))
      |("TTTFFTFF" (and (or x0 (not x2)) (or x1 x2)))
      |("FFFTFTFF" (and (not x2) (and (or (not x0) (not x1)) (or x0 x1))))
      |("TFFTFTFF" (and (or x0 x1) (and (or (not x0) (or (not x1) x2)) (or (not x2) (and x0 x1)))))
      |("FTFTFTFF" (and (not x2) (or x0 x1)))
      |("TTFTFTFF" (or (and x0 x1) (and (not x2) (or x0 x1))))
      |("FFTTFTFF" (and (or (not x0) (not x1)) (or x0 (and x1 (not x2)))))
      |("TFTTFTFF" (and (or (not x0) (or (not x1) x2)) (or x0 (and x1 (not x2)))))
      |("FTTTFTFF" (or (and x0 (not x1)) (and x1 (not x2))))
      |("TTTTFTFF" (or x0 (and x1 (not x2))))
      |("FFFFTTFF" (and (not x0) x1))
      |("TFFFTTFF" (and x1 (or (not x0) x2)))
      |("FTFFTTFF" (and x1 (or (not x0) (not x2))))
      |("TTFFTTFF" x1)
      |("FFTFTTFF" (and (or (not x0) (not x1)) (or x1 (and x0 x2))))
      |("TFTFTTFF" (or (and (not x0) x1) (and x0 x2)))
      |("FTTFTTFF" (and (or (not x0) (or (not x1) (not x2))) (or x1 (and x0 x2))))
      |("TTTFTTFF" (or x1 (and x0 x2)))
      |("FFFTTTFF" (and (or (not x0) (not x1)) (or x1 (and x0 (not x2)))))
      |("TFFTTTFF" (and (or (not x0) (or (not x1) x2)) (or x1 (and x0 (not x2)))))
      |("FTFTTTFF" (or (and (not x0) x1) (and x0 (not x2))))
      |("TTFTTTFF" (or x1 (and x0 (not x2))))
      |("FFTTTTFF" (and (or (not x0) (not x1)) (or x0 x1)))
      |("TFTTTTFF" (or (and (not x0) x1) (and x0 (or (not x1) x2))))
      |("FTTTTTFF" (or (and (not x0) x1) (and x0 (or (not x1) (not x2)))))
      |("TTTTTTFF" (or x0 x1))
      |("FFFFFFTF" (and (not x0) (and (not x1) x2)))
      |("TFFFFFTF" (and x2 (or (and (not x0) (not x1)) (and x0 x1))))
      |("FTFFFFTF" (and (or (not x0) x1) (or (and x0 (not x2)) (and (not x1) x2))))
      |("TTFFFFTF" (and (or (not x0) x1) (or x0 (and (not x1) x2))))
      |("FFTFFFTF" (and (not x1) x2))
      |("TFTFFFTF" (and x2 (or x0 (not x1))))
      |("FTTFFFTF" (and (or (not x1) (not x2)) (or x2 (and x0 x1))))
      |("TTTFFFTF" (and (or x0 (not x1)) (or x1 x2)))
      |("FFFTFFTF" (and (not x1) (and (or (not x0) (not x2)) (or x0 x2))))
      |("TFFTFFTF" (and (or x0 (not x1)) (and (or (not x0) (or x1 (not x2))) (or x2 (and x0 (not x1))))))
      |("FTFTFFTF" (and (or (not x0) (not x2)) (or x0 (and (not x1) x2))))
      |("TTFTFFTF" (or (and (not x0) (and (not x1) x2)) (and x0 (or x1 (not x2)))))
      |("FFTTFFTF" (and (not x1) (or x0 x2)))
      |("TFTTFFTF" (or (and x0 (not x1)) (and x2 (or x0 (not x1)))))
      |("FTTTFFTF" (or (and x0 (not x2)) (and (not x1) x2)))
      |("TTTTFFTF" (or x0 (and (not x1) x2)))
      |("FFFFTFTF" (and (not x0) x2))
      |("TFFFTFTF" (and x2 (or (not x0) x1)))
      |("FTFFTFTF" (and (or (not x0) (not x2)) (or x2 (and x0 x1))))
      |("TTFFTFTF" (and (or (not x0) x1) (or x0 x2)))
      |("FFTFTFTF" (and x2 (or (not x0) (not x1))))
      |("TFTFTFTF" x2)
      |("FTTFTFTF" (and (or (not x0) (or (not x1) (not x2))) (or x2 (and x0 x1))))
      |("TTTFTFTF" (or x2 (and x0 x1)))
      |("FFFTTFTF" (and (or (not x0) (not x2)) (or x2 (and x0 (not x1)))))
      |("TFFTTFTF" (and (or (not x0) (or x1 (not x2))) (or x2 (and x0 (not x1)))))
      |("FTFTTFTF" (and (or (not x0) (not x2)) (or x0 x2)))
      |("TTFTTFTF" (or (and (not x0) x2) (and x0 (or x1 (not x2)))))
      |("FFTTTFTF" (and (or (not x0) (not x1)) (or x0 x2)))
      |("TFTTTFTF" (or x2 (and x0 (not x1))))
      |("FTTTTFTF" (or (and (not x0) x2) (and x0 (or (not x1) (not x2)))))
      |("TTTTTFTF" (or x0 x2))
      |("FFFFFTTF" (and (not x0) (and (or (not x1) (not x2)) (or x1 x2))))
      |("TFFFFTTF" (and (or (not x0) x1) (or (and (not x0) (and x1 (not x2))) (and x2 (or x0 (not x1))))))
      |("FTFFFTTF" (and (or (not x1) (not x2)) (or x1 (and (not x0) x2))))
      |("TTFFFTTF" (or (and (not x0) (and (not x1) x2)) (and x1 (or x0 (not x2)))))
      |("FFTFFTTF" (and (or (not x1) (not x2)) (or x2 (and (not x0) x1))))
      |("TFTFFTTF" (or (and (not x0) (and x1 (not x2))) (and x2 (or x0 (not x1)))))
      |("FTTFFTTF" (and (or (not x1) (not x2)) (or x1 x2)))
      |("TTTFFTTF" (or (and (not x1) x2) (and x1 (or x0 (not x2)))))
      |("FFFTFTTF" (and (or (not x0) (not x1)) (or (and (not x0) (and (not x1) x2)) (and (not x2) (or x0 x1)))))
      |("TFFTFTTF" (and (or (not x0) (or (and (not x1) (not x2)) (and x1 x2))) (or x0 (and (or (not x1) (not x2)) (or x1 x2)))))
      |("FTFTFTTF" (or (and (not x0) (and (not x1) x2)) (and (not x2) (or x0 x1))))
      |("TTFTFTTF" (or (and x0 x1) (or (and (not x0) (and (not x1) x2)) (and (not x2) (or x0 x1)))))
      |("FFTTFTTF" (or (and (not x0) (and x1 (not x2))) (and (not x1) (or x0 x2))))
      |("TFTTFTTF" (or (and x0 (not x1)) (or (and (not x0) (and x1 (not x2))) (and x2 (or x0 (not x1))))))
      |("FTTTFTTF" (and (or (not x1) (not x2)) (or x0 (or x1 x2))))
      |("TTTTFTTF" (or x0 (and (or (not x1) (not x2)) (or x1 x2))))
      |("FFFFTTTF" (and (not x0) (or x1 x2)))
      |("TFFFTTTF" (or (and (not x0) x1) (and x2 (or (not x0) x1))))
      |("FTFFTTTF" (and (or (not x0) (not x2)) (or x1 x2)))
      |("TTFFTTTF" (or x1 (and (not x0) x2)))
      |("FFTFTTTF" (and (or (not x0) (not x1)) (or x1 x2)))
      |("TFTFTTTF" (or x2 (and (not x0) x1)))
      |("FTTFTTTF" (or (and (not x1) x2) (and x1 (or (not x0) (not x2)))))
      |("TTTFTTTF" (or x1 x2))
      |("FFFTTTTF" (and (or (not x0) (and (not x1) (not x2))) (or x0 (or x1 x2))))
      |("TFFTTTTF" (or (and (not x0) x1) (and (or (not x0) (or x1 (not x2))) (or x2 (and x0 (not x1))))))
      |("FTFTTTTF" (and (or (not x0) (not x2)) (or x0 (or x1 x2))))
      |("TTFTTTTF" (or x1 (and (or (not x0) (not x2)) (or x0 x2))))
      |("FFTTTTTF" (and (or (not x0) (not x1)) (or x0 (or x1 x2))))
      |("TFTTTTTF" (or x2 (and (or (not x0) (not x1)) (or x0 x1))))
      |("FTTTTTTF" (or (and (not x0) x1) (or (and x0 (not x2)) (and (not x1) x2))))
      |("TTTTTTTF" (or x0 (or x1 x2)))
      |("FFFFFFFT" (and (not x0) (and (not x1) (not x2))))
      |("TFFFFFFT" (and (or (not x0) x1) (and (or x0 (not x2)) (or (not x1) x2))))
      |("FTFFFFFT" (and (not x2) (or (and (not x0) (not x1)) (and x0 x1))))
      |("TTFFFFFT" (and (or (not x0) x1) (or x0 (and (not x1) (not x2)))))
      |("FFTFFFFT" (and (not x1) (or (and (not x0) (not x2)) (and x0 x2))))
      |("TFTFFFFT" (and (or (not x0) x2) (or x0 (and (not x1) (not x2)))))
      |("FTTFFFFT" (and (or x0 (not x1)) (and (or (not x0) (or x1 x2)) (or (not x2) (and x0 (not x1))))))
      |("TTTFFFFT" (or (and (not x0) (and (not x1) (not x2))) (and x0 (or x1 x2))))
      |("FFFTFFFT" (and (not x1) (not x2)))
      |("TFFTFFFT" (or (and (not x1) (not x2)) (and x0 (and x1 x2))))
      |("FTFTFFFT" (and (not x2) (or x0 (not x1))))
      |("TTFTFFFT" (and (or x0 (not x1)) (or x1 (not x2))))
      |("FFTTFFFT" (and (not x1) (or x0 (not x2))))
      |("TFTTFFFT" (and (or x0 (not x2)) (or (not x1) x2)))
      |("FTTTFFFT" (or (and x0 (not x1)) (and (not x2) (or x0 (not x1)))))
      |("TTTTFFFT" (or x0 (and (not x1) (not x2))))
      |("FFFFTFFT" (and (not x0) (or (and (not x1) (not x2)) (and x1 x2))))
      |("TFFFTFFT" (and (or (not x1) x2) (or x1 (and (not x0) (not x2)))))
      |("FTFFTFFT" (and (or (not x0) x1) (or (and (not x0) (and x1 x2)) (and (not x2) (or x0 (not x1))))))
      |("TTFFTFFT" (or (and (not x0) (and (not x1) (not x2))) (and x1 (or x0 x2))))
      |("FFTFTFFT" (and (or (not x0) (not x1)) (or (and (not x0) (and (not x1) (not x2))) (and x2 (or x0 x1)))))
      |("TFTFTFFT" (or (and (not x0) (and (not x1) (not x2))) (and x2 (or x0 x1))))
      |("FTTFTFFT" (or (and (not x0) (or (and (not x1) (not x2)) (and x1 x2))) (and x0 (and (or (not x1) (not x2)) (or x1 x2)))))
      |("TTTFTFFT" (or (and x0 x1) (or (and (not x0) (and (not x1) (not x2))) (and x2 (or x0 x1)))))
      |("FFFTTFFT" (or (and (not x1) (not x2)) (and (not x0) (and x1 x2))))
      |("TFFTTFFT" (or (and (not x1) (not x2)) (and x1 x2)))
      |("FTFTTFFT" (or (and (not x0) (and x1 x2)) (and (not x2) (or x0 (not x1)))))
      |("TTFTTFFT" (or (and (not x1) (not x2)) (and x1 (or x0 x2))))
      |("FFTTTFFT" (or (and (not x0) (and x1 x2)) (and (not x1) (or x0 (not x2)))))
      |("TFTTTFFT" (or (and (not x1) (not x2)) (and x2 (or x0 x1))))
      |("FTTTTFFT" (or (and x0 (not x1)) (or (and (not x0) (and x1 x2)) (and (not x2) (or x0 (not x1))))))
      |("TTTTTFFT" (or x0 (or (and (not x1) (not x2)) (and x1 x2))))
      |("FFFFFTFT" (and (not x0) (not x2)))
      |("TFFFFTFT" (or (and (not x0) (not x2)) (and x0 (and x1 x2))))
      |("FTFFFTFT" (and (not x2) (or (not x0) x1)))
      |("TTFFFTFT" (and (or (not x0) x1) (or x0 (not x2))))
      |("FFTFFTFT" (or (and (not x0) (not x2)) (and x0 (and (not x1) x2))))
      |("TFTFFTFT" (or (and (not x0) (not x2)) (and x0 x2)))
      |("FTTFFTFT" (and (or (not x0) (or x1 x2)) (or (not x2) (and x0 (not x1)))))
      |("TTTFFTFT" (or (and (not x0) (not x2)) (and x0 (or x1 x2))))
      |("FFFTFTFT" (and (not x2) (or (not x0) (not x1))))
      |("TFFTFTFT" (and (or (not x0) (or (not x1) x2)) (or (not x2) (and x0 x1))))
      |("FTFTFTFT" (not x2))
      |("TTFTFTFT" (or (not x2) (and x0 x1)))
      |("FFTTFTFT" (and (or (not x0) (not x1)) (or x0 (not x2))))
      |("TFTTFTFT" (or (and (not x0) (not x2)) (and x0 (or (not x1) x2))))
      |("FTTTFTFT" (or (not x2) (and x0 (not x1))))
      |("TTTTFTFT" (or x0 (not x2)))
      |("FFFFTTFT" (and (not x0) (or x1 (not x2))))
      |("TFFFTTFT" (or (and (not x0) (not x2)) (and x1 x2)))
      |("FTFFTTFT" (or (and (not x0) x1) (and (not x2) (or (not x0) x1))))
      |("TTFFTTFT" (or x1 (and (not x0) (not x2))))
      |("FFTFTTFT" (and (or (not x0) (and (not x1) x2)) (or x0 (or x1 (not x2)))))
      |("TFTFTTFT" (or (and (not x0) (not x2)) (and x2 (or x0 x1))))
      |("FTTFTTFT" (or (and (not x0) x1) (and (or (not x0) (or x1 x2)) (or (not x2) (and x0 (not x1))))))
      |("TTTFTTFT" (or x1 (or (and (not x0) (not x2)) (and x0 x2))))
      |("FFFTTTFT" (and (or (not x0) (not x1)) (or x1 (not x2))))
      |("TFFTTTFT" (or (and (not x1) (not x2)) (and x1 (or (not x0) x2))))
      |("FTFTTTFT" (or (not x2) (and (not x0) x1)))
      |("TTFTTTFT" (or x1 (not x2)))
      |("FFTTTTFT" (and (or (not x0) (not x1)) (or x0 (or x1 (not x2)))))
      |("TFTTTTFT" (or (and (not x0) x1) (and (or x0 (not x2)) (or (not x1) x2))))
      |("FTTTTTFT" (or (not x2) (and (or (not x0) (not x1)) (or x0 x1))))
      |("TTTTTTFT" (or x0 (or x1 (not x2))))
      |("FFFFFFTT" (and (not x0) (not x1)))
      |("TFFFFFTT" (or (and (not x0) (not x1)) (and x0 (and x1 x2))))
      |("FTFFFFTT" (or (and (not x0) (not x1)) (and x0 (and x1 (not x2)))))
      |("TTFFFFTT" (or (and (not x0) (not x1)) (and x0 x1)))
      |("FFTFFFTT" (and (not x1) (or (not x0) x2)))
      |("TFTFFFTT" (or (and (not x0) (not x1)) (and x0 x2)))
      |("FTTFFFTT" (and (or (not x0) (or x1 x2)) (or (not x1) (and x0 (not x2)))))
      |("TTTFFFTT" (or (and (not x0) (not x1)) (and x0 (or x1 x2))))
      |("FFFTFFTT" (and (not x1) (or (not x0) (not x2))))
      |("TFFTFFTT" (and (or (not x0) (or x1 (not x2))) (or (not x1) (and x0 x2))))
      |("FTFTFFTT" (or (and (not x0) (not x1)) (and x0 (not x2))))
      |("TTFTFFTT" (or (and (not x0) (not x1)) (and x0 (or x1 (not x2)))))
      |("FFTTFFTT" (not x1))
      |("TFTTFFTT" (or (not x1) (and x0 x2)))
      |("FTTTFFTT" (or (not x1) (and x0 (not x2))))
      |("TTTTFFTT" (or x0 (not x1)))
      |("FFFFTFTT" (and (not x0) (or (not x1) x2)))
      |("TFFFTFTT" (or (and (not x0) (not x1)) (and x1 x2)))
      |("FTFFTFTT" (or (and (not x0) (or (not x1) x2)) (and x0 (and x1 (not x2)))))
      |("TTFFTFTT" (or (and (not x0) (not x1)) (and x1 (or x0 x2))))
      |("FFTFTFTT" (or (and (not x0) (not x1)) (and x2 (or (not x0) (not x1)))))
      |("TFTFTFTT" (or x2 (and (not x0) (not x1))))
      |("FTTFTFTT" (or (and (not x0) (not x1)) (and (or (not x0) (or (not x1) (not x2))) (or x2 (and x0 x1)))))
      |("TTTFTFTT" (or x2 (or (and (not x0) (not x1)) (and x0 x1))))
      |("FFFTTFTT" (and (or (not x0) (not x2)) (or (not x1) x2)))
      |("TFFTTFTT" (or (and (not x1) (not x2)) (and x2 (or (not x0) x1))))
      |("FTFTTFTT" (and (or (not x0) (not x2)) (or x0 (or (not x1) x2))))
      |("TTFTTFTT" (or (and (not x0) (not x1)) (or (and x0 (not x2)) (and x1 x2))))
      |("FFTTTFTT" (or (not x1) (and (not x0) x2)))
      |("TFTTTFTT" (or (not x1) x2))
      |("FTTTTFTT" (or (not x1) (and (or (not x0) (not x2)) (or x0 x2))))
      |("TTTTTFTT" (or x0 (or (not x1) x2)))
      |("FFFFFTTT" (and (not x0) (or (not x1) (not x2))))
      |("TFFFFTTT" (or (and (not x0) (or (not x1) (not x2))) (and x0 (and x1 x2))))
      |("FTFFFTTT" (or (and (not x0) (not x1)) (and x1 (not x2))))
      |("TTFFFTTT" (or (and (not x0) (not x1)) (and x1 (or x0 (not x2)))))
      |("FFTFFTTT" (or (and (not x0) (not x2)) (and (not x1) x2)))
      |("TFTFFTTT" (or (and (not x0) (not x2)) (and x2 (or x0 (not x1)))))
      |("FTTFFTTT" (and (or (not x1) (not x2)) (or (not x0) (or x1 x2))))
      |("TTTFFTTT" (or (and (not x0) (not x1)) (and (or x0 (not x2)) (or x1 x2))))
      |("FFFTFTTT" (or (and (not x0) (not x1)) (and (not x2) (or (not x0) (not x1)))))
      |("TFFTFTTT" (or (and (not x0) (not x1)) (and (or (not x0) (or (not x1) x2)) (or (not x2) (and x0 x1)))))
      |("FTFTFTTT" (or (not x2) (and (not x0) (not x1))))
      |("TTFTFTTT" (or (not x2) (or (and (not x0) (not x1)) (and x0 x1))))
      |("FFTTFTTT" (or (not x1) (and (not x0) (not x2))))
      |("TFTTFTTT" (or (not x1) (or (and (not x0) (not x2)) (and x0 x2))))
      |("FTTTFTTT" (or (not x1) (not x2)))
      |("TTTTFTTT" (or x0 (or (not x1) (not x2))))
      |("FFFFTTTT" (not x0))
      |("TFFFTTTT" (or (not x0) (and x1 x2)))
      |("FTFFTTTT" (or (not x0) (and x1 (not x2))))
      |("TTFFTTTT" (or (not x0) x1))
      |("FFTFTTTT" (or (not x0) (and (not x1) x2)))
      |("TFTFTTTT" (or (not x0) x2))
      |("FTTFTTTT" (or (not x0) (and (or (not x1) (not x2)) (or x1 x2))))
      |("TTTFTTTT" (or (not x0) (or x1 x2)))
      |("FFFTTTTT" (or (not x0) (and (not x1) (not x2))))
      |("TFFTTTTT" (or (not x0) (or (and (not x1) (not x2)) (and x1 x2))))
      |("FTFTTTTT" (or (not x0) (not x2)))
      |("TTFTTTTT" (or (not x0) (or x1 (not x2))))
      |("FFTTTTTT" (or (not x0) (not x1)))
      |("TFTTTTTT" (or (not x0) (or (not x1) x2)))
      |("FTTTTTTT" (or (not x0) (or (not x1) (not x2))))
      |("TTTTTTTT" T)
      |)
      |""".stripMargin


  //
  // S-Expression Parser by Zen Bowman.
  //
  // https://github.com/ZenBowman/sexpr/
  //

  //
  //  The MIT License (MIT)
  //
  //  Copyright (c) 2013 ZenBowman
  //
  //  Permission is hereby granted, free of charge, to any person obtaining a copy of
  //  this software and associated documentation files (the "Software"), to deal in
  //  the Software without restriction, including without limitation the rights to
  //  use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
  //  the Software, and to permit persons to whom the Software is furnished to do so,
  //  subject to the following conditions:
  //
  //    The above copyright notice and this permission notice shall be included in all
  //    copies or substantial portions of the Software.
  //
  //  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  //  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
  //  FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
  //  COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
  //  IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
  //  CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

  private class InvalidSExpressionException extends Exception

  private sealed trait Element

  private case class Atom(symbol: String) extends Element

  private case class SList(values: List[Element]) extends Element

  private object ExpressionParser {
    private var remainingTokens: List[String] = List()

    def tokenize(expression: String): List[String] = {
      expression.replace("(", " ( ").replace(")", " ) ").trim().split("\\s+").toList
    }

    def parse(expression: String): SList = {
      remainingTokens = tokenize(expression)
      parseTokens()
    }

    def parseTokens(): SList = {
      val elements = new ListBuffer[Element]

      while (remainingTokens.nonEmpty) {
        val first = remainingTokens.head
        remainingTokens = remainingTokens.tail
        if (first == "(") {
          val element = parseTokens()
          elements.append(element)
        }
        else if (first == ")") {
          return SList(elements.toList)
        } else {
          elements.append(Atom(first))
        }
      }

      try {
        elements.head.asInstanceOf[SList]
      } catch {
        case _: Exception => throw new InvalidSExpressionException
      }
    }
  }

}

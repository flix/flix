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
import ca.uwaterloo.flix.language.ast.{Kind, Symbol, Type, TypeConstructor}
import ca.uwaterloo.flix.language.phase.unification.BoolFormula._
import ca.uwaterloo.flix.util.{InternalCompilerException, LocalResource, StreamOps}
import ca.uwaterloo.flix.util.collection.Bimap

import java.io.IOException
import java.util.zip.ZipInputStream
import scala.annotation.tailrec
import scala.collection.immutable.SortedSet

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
    * The path to the table.
    */
  private val Path: String = "/src/ca/uwaterloo/flix/language/phase/unification/Table4.pn.zip"

  /**
    * The number of variables that the minimization table uses.
    *
    * Warning: If the number is set incorrectly minimization will be wrong!
    */
  private val MaxVars: Int = 4

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
  def minimizeType(tpe0: Type)(implicit flix: Flix): Type = {
    // Check whether minimization via tabling is disabled.
    if (flix.options.xnobooltable) {
      return tpe0
    }

    // Check that the `tpe` argument is a Boolean formula.
    if (tpe0.kind != Kind.Bool && tpe0.kind != Kind.Effect) {
      throw InternalCompilerException(s"Unexpected non-Bool/non-Effect kind: '${tpe0.kind}'.")
    }

    // Erase aliases to get a processable type
    val tpe = Type.eraseAliases(tpe0)

    // Compute the size of  `tpe`.
    val currentSize = tpe.size

    // Return `tpe` immediately if it is "small".
    if (currentSize < Threshold) {
      return tpe
    }

    // Compute the variables in `tpe`.
    val tvars = tpe.typeVars.toList.map(tvar => BoolFormula.VarOrEff.Var(tvar.sym))
    val effs = getEffects(tpe).toList.map(BoolFormula.VarOrEff.Eff)

    // Construct a bi-directional map from type variables to indices.
    // The idea is that the first variable becomes x0, the next x1, and so forth.
    val m = (tvars ++ effs).zipWithIndex.foldLeft(Bimap.empty[BoolFormula.VarOrEff, Variable]) {
      case (macc, (sym, x)) => macc + (sym -> x)
    }

    // Convert the type `tpe` to a Boolean formula.
    val input = tpe.kind match {
      case Kind.Bool => fromBoolType(tpe, m)
      case Kind.Effect => fromEffType(tpe, m)
      case _ => throw InternalCompilerException(s"Unexpected non-Bool/non-Effect kind: '${tpe.kind}'.")
    }

    // Minimize the Boolean formula.
    val minimized = minimizeFormula(input)

    // Convert the formula back to a type.
    toType(minimized, m, tpe.kind, tpe.loc)
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
    val table = loadTable()

    if (Debug) {
      println("== Minimization Array ==")
      println()
      for (i <- table.indices) {
        println(s"  ${toBinaryString(i, 1 << MaxVars)}: ${table(i)}")
      }
      println(s"Total Array Size = ${table.length}")
      println()
    }

    table
  }

  /**
    * Loads the table of minimal Boolean formulas from the disk.
    */
  private def loadTable(): Array[BoolFormula] = try {
    val allLines = readTableFromZip(Path)

    // Split the string into lines.
    val lines = allLines.split("\n")

    // Parse each line into a formula.
    val formulas = lines.map(parseLine)

    // Allocate the result table. The table has size 2^(2^MaxVars).
    val table = new Array[BoolFormula](1 << (1 << MaxVars))

    // Fill the table.
    for ((f, i) <- formulas.zipWithIndex) {
      table(i) = f
    }

    // Return the table.
    table
  } catch {
    case ex: IOException => throw InternalCompilerException(s"Unable to load Boolean minimization table: '$Path'.")
  }

  /**
    * Attempts to read the given `path` as a local resource which is a zip-file.
    *
    * Returns the first zip-entry as a string.
    */
  private def readTableFromZip(path: String): String = {
    val inputStream = LocalResource.getInputStream(path)
    val zipIn = new ZipInputStream(inputStream)
    val entry = zipIn.getNextEntry
    StreamOps.readAll(zipIn)
  }

  /**
    * Parses the given line `l` into a Boolean formula.
    *
    * The format is in reverse polish notation:
    *
    * 301a2oa should be interpreted as:
    *
    * x3 x0 x1 and x2 or and, that is:
    * and(x3,or(and(x0,x1),x2))
    *
    */
  private def parseLine(l: String): BoolFormula = {
    @tailrec
    def parse(input: List[Char], stack: List[BoolFormula]): BoolFormula = (input, stack) match {
      case (Nil, formula :: Nil) => formula
      case ('T' :: rest, stack) => parse(rest, True :: stack)
      case ('F' :: rest, stack) => parse(rest, False :: stack)
      case ('0' :: rest, stack) => parse(rest, Var(0) :: stack)
      case ('1' :: rest, stack) => parse(rest, Var(1) :: stack)
      case ('2' :: rest, stack) => parse(rest, Var(2) :: stack)
      case ('3' :: rest, stack) => parse(rest, Var(3) :: stack)
      case ('n' :: rest, f :: stack) => parse(rest, Neg(f) :: stack)
      case ('a' :: rest, f2 :: f1 :: stack) => parse(rest, Conj(f1, f2) :: stack)
      case ('o' :: rest, f2 :: f1 :: stack) => parse(rest, Disj(f1, f2) :: stack)
      case _ => throw InternalCompilerException(s"Parse Error. input = ${input.mkString(" :: ")}, stack = $stack.")
    }

    parse(l.trim().toList, Nil)
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
    * Gets all the effects in the given type.
    */
  private def getEffects(t: Type): SortedSet[Symbol.EffectSym] = t match {
    case Type.Cst(TypeConstructor.Effect(sym), _) => SortedSet(sym)

    case _: Type.Cst => SortedSet.empty
    case _: Type.KindedVar => SortedSet.empty

    case Type.Apply(tpe1, tpe2, loc) => getEffects(tpe1) ++ getEffects(tpe2)
    case Type.Alias(cst, args, tpe, loc) => getEffects(tpe)

    case _: Type.Ascribe => throw InternalCompilerException("Unexpected unkinded type.")
    case _: Type.UnkindedVar => throw InternalCompilerException("Unexpected unkinded type.")
    case _: Type.UnkindedArrow => throw InternalCompilerException("Unexpected unkinded type.")
    case _: Type.ReadWrite => throw InternalCompilerException("Unexpected unkinded type.")
  }
}

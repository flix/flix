/*
 * Copyright 2024 Jonathan Lindegaard Starup
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

package ca.uwaterloo.flix.language.phase.unification.set

import ca.uwaterloo.flix.language.ast.SourceLocation
import ca.uwaterloo.flix.language.phase.unification.set.SetFormula.*
import ca.uwaterloo.flix.util.InternalCompilerException

import scala.util.Random

/** Generates randomized [[SetFormula]]. */
object SetFormulaGenerator {

  /**
    * Holds static parameters for [[generate]].
    *
    * @param maxConnectiveWidth the maximum number of subformula in [[Union]] and [[Inter]]
    * @param varDomSize the maximum number of different [[Var]] generated (variable domain size)
    * @param cstDomSize the maximum number of different [[Cst]] generated (constant domain size)
    * @param elemDomSize the maximum number of different elements in [[ElemSet]] generated
    *                    (element domain size)
    */
  final case class Options(maxConnectiveWidth: Int, varDomSize: Int, cstDomSize: Int, elemDomSize: Int) {

    // variables, constants, and elements must use disjoint numbers, so we make three disjoint
    // ranges.

    /**
      * Generated [[Var]] should use integers i where
      * `varOffset <= i < varOffset + varDomSize`.
      */
    val varOffset: Int = 0

    /**
      * Generated [[Cst]] should use integers i where
      * `cstOffset <= i < cstOffset + cstDomSize`.
      */
    val cstOffset: Int = 0 + (varDomSize max 0)

    /**
      * Generated [[ElemSet]] should use integers i where
      * `elemOffset <= i < elemOffset + elemDomSize`
      */
    val elemOffset: Int = cstOffset + (cstDomSize max 0)

  }

  /**
    * Generate a random [[SetFormula]] of a given `size` and a `maxDepth`.
    *
    * While this function faithfully wants the generate a formula of exactly the given `size`, the
    * constructors of [[SetFormula]] will collapse and simplify formulas, making the output smaller
    * in most cases.
    *
    * @param size the wanted size (counted in binary connectives like [[SetFormula.size]])
    * @param maxDepth the maximum nesting levels of [[Union]], [[Inter]], and [[Compl]]. `0` is a
    *                 flat formula and `d<0` is unlimited depth.
    */
  def generate(size: Int, maxDepth: Int)(implicit r: Random, opts: Options): SetFormula = {
    if (size <= 0) chooseAtom() match {
      case Atom.Var => generateVar()
      case Atom.Cst => generateCst()
      case Atom.ElemSet => generateElem()
      case Atom.Univ => Univ
      case Atom.Empty => Empty
    } else if (maxDepth == 0) {
      // We know that size >= 1
      // We must fulfil our size goal without nesting, potentially ignoring the width limit.
      chooseNAryOp() match {
        case Op.Inter => mkInterAll(List.fill(size)(generate(0, 0)))
        case Op.Union => mkUnionAll(List.fill(size)(generate(0, 0)))
      }
    } else {
      // We know that size >= 1 and maxDepth != 0
      chooseOp() match {
        case Op.Inter => mkInterAll(chooseSubSizes(size).map(generate(_, maxDepth - 1)))
        case Op.Union => mkUnionAll(chooseSubSizes(size).map(generate(_, maxDepth - 1)))
        case Op.Compl => mkCompl(generate(size - 1, maxDepth - 1))
      }
    }
  }

  /**
    * Returns a random singleton [[Var]] from the domain.
    *
    * @throws InternalCompilerException if [[Options.varDomSize]] is non-positive.
    */
  def generateVar()(implicit r: Random, opts: Options): Var = {
    if (opts.varDomSize <= 0) throw InternalCompilerException(
      s"Tried to generate a variable with domain size ${opts.varDomSize}", SourceLocation.Unknown
    ) else Var(opts.varOffset + r.nextInt(opts.varDomSize))
  }

  /**
    * Returns a random singleton [[Cst]] from the domain.
    *
    * @throws InternalCompilerException if [[Options.cstDomSize]] is non-positive.
    */
  def generateCst()(implicit r: Random, opts: Options): Cst = {
    if (opts.cstDomSize <= 0) throw InternalCompilerException(
      s"Tried to generate a constant with domain size ${opts.cstDomSize}", SourceLocation.Unknown
    ) else Cst(opts.cstOffset + r.nextInt(opts.cstDomSize))
  }

  /**
    * Returns a random singleton [[ElemSet]] from the domain.
    *
    * @throws InternalCompilerException if [[Options.elemDomSize]] is non-positive.
    */
  def generateElem()(implicit r: Random, opts: Options): ElemSet = {
    if (opts.elemDomSize <= 0) throw InternalCompilerException(
      s"Tried to generate an element with domain size ${opts.elemDomSize}", SourceLocation.Unknown
    ) else mkElemSet(opts.elemOffset + r.nextInt(opts.elemDomSize))
  }

  /** Represents [[SetFormula]] without [[SetFormula]] nested inside. */
  private sealed trait Atom

  private object Atom {
    /** Represents [[SetFormula.Var]]. */
    case object Var extends Atom

    /** Represents [[SetFormula.Cst]]. */
    case object Cst extends Atom

    /** Represents [[SetFormula.ElemSet]]. */
    case object ElemSet extends Atom

    /** Represents [[SetFormula.Univ]]. */
    case object Univ extends Atom

    /** Represents [[SetFormula.Empty]]. */
    case object Empty extends Atom
  }

  /**
    * Returns a random [[Atom]], valid to generate by `opts`.
    *
    * [[Atom.Var]], [[Atom.Cst]], and [[Atom.ElemSet]] are only returned if their respective domain
    * is non-empty (see [[Option]]).
    *
    * [[Atom.Univ]] and [[Atom.Empty]] will only be chosen if the domains (see [[Option]]) of
    * constants, variables, and elements are empty.
    */
  private def chooseAtom()(implicit r: Random, opts: Options): Atom = {
    var options: List[Atom] = Nil
    if (opts.varDomSize > 0) options = Atom.Var :: options
    if (opts.cstDomSize > 0) options = Atom.Cst :: options
    if (opts.elemDomSize > 0) options = Atom.ElemSet :: options
    if (options.isEmpty) chooseFrom(Atom.Univ, Atom.Empty)
    else chooseFrom(options)
  }

  /** Represents [[SetFormula]] with [[SetFormula]] inside. */
  private sealed trait Op

  /** Represents [[SetFormula]] with more than one [[SetFormula]] inside. */
  private sealed trait NAryOp extends Op

  private object Op {
    /** Represents [[SetFormula.Inter]]. */
    case object Inter extends Op with NAryOp

    /** Represents [[SetFormula.Union]]. */
    case object Union extends Op with NAryOp

    /** Represents [[SetFormula.Compl]]. */
    case object Compl extends Op
  }

  /** Returns a random [[Op]]. */
  private def chooseOp()(implicit r: Random): Op = {
    chooseFrom(Op.Inter, Op.Union, Op.Compl)
  }

  /** Returns a random [[NAryOp]]. */
  private def chooseNAryOp()(implicit r: Random): NAryOp = {
    chooseFrom(Op.Inter, Op.Union)
  }

  /**
    * Returns a list of subformula sizes of length at least two while respecting
    * [[Options.maxConnectiveWidth]] and `size`.
    *
    * The total sum of the returned list `l` is `size - (l.length - 1)`, for a total size of `size`.
    */
  private def chooseSubSizes(size: Int)(implicit r: Random, opts: Options): List[Int] = {
    // We need a connectiveCount between 1 and max, but size might constrain the choice further.
    val maxSubformulas = opts.maxConnectiveWidth min (size + 1)
    val subformulaCount = r.between(minInclusive = 2, maxExclusive = (maxSubformulas max 3))
    val connectiveCount = subformulaCount - 1
    // We have used connectiveCount connectives, the remaining needs to be distributed into random
    // subformula.
    val remainingConnectives = size - connectiveCount
    // Iterate through each leftover connective, and distribute it randomly.
    // This is slow but assures uniform distribution.
    val buckets = Array.fill(subformulaCount)(0)
    for (_ <- Range(0, remainingConnectives)) {
      buckets(r.nextInt(subformulaCount)) += 1
    }
    buckets.toList
  }

  /** Returns some given value, randomly chosen with uniform distribution. */
  private def chooseFrom[T](x: T, xs: T*)(implicit r: Random): T = {
    val i = r.nextInt(xs.length + 1)
    if (i == 0) x else xs(i - 1)
  }

  /**
    * Returns some given value, randomly chosen with uniform distribution.
    *
    * Throws [[InternalCompilerException]] if `xs` is empty.
    */
  private def chooseFrom[T](xs: List[T])(implicit r: Random): T = {
    if (xs.isEmpty) throw InternalCompilerException("Cannot choose one of zero elements", SourceLocation.Unknown)
    else xs(r.nextInt(xs.length))
  }

  def main(args: Array[String]): Unit = {
    val r: Random = new Random(-62150363)
    for (i <- 0 until 1000) {
      val f = generate(i, -1)(r, Options(maxConnectiveWidth = 3, varDomSize = 3, cstDomSize = 3, elemDomSize = 3))
      println()
      println(f)
      val fProp = SetFormula.propagation(f)
      println(fProp)
      assert(SetFormula.isEquivalent(f, fProp))
    }
  }

}

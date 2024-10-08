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

/**
  * Generates randomized [[SetFormula]].
  */
object SetFormulaGenerator {

  final case class Options(maxWidth: Int, varDomSize: Int, cstDomSize: Int, elemDomSize: Int) {
    // variables, constants, and elements must use disjoint numbers, so these are the starting indices
    val varOffset: Int = 0
    val cstOffset: Int = 0 + varDomSize
    val elemOffset: Int = cstOffset + cstDomSize
  }

  /**
    * @param size the wanted size (counted in binary connectives)
    * @param depth the maximum depth
    */
  def generate(size: Int, depth: Int)(implicit r: Random, opts: Options): SetFormula = {
    if (size <= 0) chooseAtom() match {
      case Atom.Var => generateVar()
      case Atom.Cst => generateCst()
      case Atom.ElemSet => generateElem()
      case Atom.Univ => Univ
      case Atom.Empty => Empty
    } else if (depth == 0) {
      // We know that size >= 1
      // We must fulfil our size goal without nesting, potentially ignoring the width limit.
      chooseNAryOp() match {
        case Op.Inter => mkInterAll(List.fill(size)(generate(0, 0)))
        case Op.Union => mkUnionAll(List.fill(size)(generate(0, 0)))
      }
    } else {
      // We know that size >= 1 and depth != 0
      chooseOp() match {
        case Op.Inter => mkInterAll(chooseSubSizes(size).map(generate(_, depth - 1)))
        case Op.Union => mkUnionAll(chooseSubSizes(size).map(generate(_, depth - 1)))
        case Op.Compl => mkCompl(generate(size - 1, depth - 1))
      }
    }
  }

  /** Returns a random singleton [[Var]] from the domain, if it it non-empty. */
  def generateVar()(implicit r: Random, opts: Options): Var = {
    if (opts.varDomSize <= 0) throw InternalCompilerException(
      s"Tried to generate a variable with domain size ${opts.varDomSize}", SourceLocation.Unknown
    ) else Var(opts.varOffset + r.nextInt(opts.varDomSize))
  }

  /** Returns a random singleton [[Cst]] from the domain, if it it non-empty. */
  def generateCst()(implicit r: Random, opts: Options): Cst = {
    if (opts.cstDomSize <= 0) throw InternalCompilerException(
      s"Tried to generate a constant with domain size ${opts.cstDomSize}", SourceLocation.Unknown
    ) else Cst(opts.cstOffset + r.nextInt(opts.cstDomSize))
  }

  /** Returns a random singleton [[ElemSet]] from the domain, if it it non-empty. */
  def generateElem()(implicit r: Random, opts: Options): ElemSet = {
    if (opts.elemDomSize <= 0) throw InternalCompilerException(
      s"Tried to generate an element with domain size ${opts.elemDomSize}", SourceLocation.Unknown
    ) else mkElemSet(opts.elemOffset + r.nextInt(opts.elemDomSize))
  }

  private sealed trait Atom

  private object Atom {
    case object Var extends Atom

    case object Cst extends Atom

    case object ElemSet extends Atom

    case object Univ extends Atom

    case object Empty extends Atom
  }

  /** Returns a random [[Atom]] that is not [[Atom.Univ]] or [[Atom.Empty]] if possible. */
  private def chooseAtom()(implicit r: Random, opts: Options): Atom = {
    var options: List[Atom] = Nil
    if (opts.varDomSize > 0) options = Atom.Var :: options
    if (opts.cstDomSize > 0) options = Atom.Cst :: options
    if (opts.elemDomSize > 0) options = Atom.ElemSet :: options
    if (options.isEmpty) chooseFrom(Atom.Univ, Atom.Empty)
    else chooseFrom(options)
  }

  private sealed trait Op

  private sealed trait NAryOp extends Op

  private object Op {
    case object Inter extends Op with NAryOp

    case object Union extends Op with NAryOp

    case object Compl extends Op
  }

  private def chooseOp()(implicit r: Random): Op = {
    chooseFrom(Op.Inter, Op.Union, Op.Compl)
  }

  private def chooseNAryOp()(implicit r: Random): NAryOp = {
    chooseFrom(Op.Inter, Op.Union)
  }

  /**
    * Returns a number in the range 2 <= x <= [[Options.maxWidth]], or always 2 if the option is 2
    * or less.
    */
  private def chooseSubSizes(size: Int)(implicit r: Random, opts: Options): List[Int] = {
    // We need at least two sub sizes, but are perhaps limited by the total size, since they need
    // one each.
    val connectives = r.nextInt(((opts.maxWidth max 2) - 1) min size) + 1
    assert(connectives <= (opts.maxWidth max 2) - 1)
    assert(connectives <= size)
    assert(connectives >= 1)
    // w are subtracted for the overarching connective, and w are additionally subtracted since
    // every subsize must be at least one.
    val leftoverSize = size - connectives
    assert(leftoverSize >= 0)
    val bucketCount = connectives + 1
    assert(bucketCount >= 2)
    // we need to distribute leftoverSize in connectives + 1 buckets
    // we do so in O(lefoverSize) time so have uniform distribution
    val buckets = Array.fill(bucketCount)(0)
    for (_ <- Range(0, leftoverSize)) {
      buckets(r.nextInt(bucketCount)) += 1
    }
    buckets.toList
  }

  /** Returns some given element, randomly chosen with uniform distribution. */
  private def chooseFrom[T](x: T, xs: T*)(implicit r: Random): T = {
    val i = r.nextInt(xs.length + 1)
    if (i == 0) x
    else xs(i - 1)
  }

  /**
    * Returns some given element, randomly chosen with uniform distribution.
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
      val f = generate(i, -1)(r, Options(maxWidth = 3, varDomSize = 3, cstDomSize = 3, elemDomSize = 3))
      println()
      println(f)
      val fProp = SetFormula.propagation(f)
      println(fProp)
      assert(SetFormula.isEquivalent(f, fProp))
    }
  }

}

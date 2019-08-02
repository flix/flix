/*
 * Copyright 2017 Magnus Madsen
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

package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.CompilationError
import ca.uwaterloo.flix.language.ast.SimplifiedAst._
import ca.uwaterloo.flix.language.ast.{SourceLocation, SpecialOperator, Symbol, Type}
import ca.uwaterloo.flix.util.Validation._
import ca.uwaterloo.flix.util.{InternalCompilerException, Validation}

import scala.collection.mutable

/**
  * The Uncurrier introduces uncurried versions of a subset of definitions needed for interop.
  *
  * Specifically, certain definitions are called by the Flix runtime environment and solver.
  *
  * The runtime solver cannot call curried functions, so we introduce uncurried versions.
  */
object Uncurrier extends Phase[Root, Root] {

  /**
    * Mutable map of top level definitions.
    */
  private type TopLevel = mutable.Map[Symbol.DefnSym, Def]

  /**
    * Introduces uncurried definitions where needed.
    */
  def run(root: Root)(implicit flix: Flix): Validation[Root, CompilationError] = flix.phase("Uncurrier") {
    // A mutable map to hold new top-level definitions.
    val newDefs: TopLevel = mutable.Map.empty

    // Uncurry lattice operations.
    val newLatticeOps = visitLatticeOps(root.latticeComponents, newDefs, root)

    // Uncurry special operations.
    val newSpecialOps = visitSpecialOps(root.specialOps, newDefs, root)

    // Reassemble the ast.
    root.copy(defs = root.defs ++ newDefs, latticeComponents = newLatticeOps, specialOps = newSpecialOps).toSuccess
  }

  /**
    * Uncurries lattice operations.
    */
  def visitLatticeOps(lattices: Map[Type, LatticeComponents], newDefs: TopLevel, root: Root)(implicit flix: Flix): Map[Type, LatticeComponents] = {
    lattices.foldLeft(Map.empty[Type, LatticeComponents]) {
      case (macc, (_, LatticeComponents(tpe, bot, top, equ, leq, lub, glb, loc))) =>
        // Uncurry the four lattice operations.
        val uncurriedEqu = mkUncurried2(equ, newDefs, root)
        val uncurriedLeq = mkUncurried2(leq, newDefs, root)
        val uncurriedLub = mkUncurried2(lub, newDefs, root)
        val uncurriedGlb = mkUncurried2(glb, newDefs, root)
        macc + (tpe -> LatticeComponents(tpe, bot, top, uncurriedEqu, uncurriedLeq, uncurriedLub, uncurriedGlb, loc))
    }
  }

  /**
    * Uncurries the equality operation.
    */
  def visitSpecialOps(specialOps: Map[SpecialOperator, Map[Type, Symbol.DefnSym]], newDefs: TopLevel, root: Root)(implicit flix: Flix): Map[SpecialOperator, Map[Type, Symbol.DefnSym]] = {
    val newEqOps = specialOps(SpecialOperator.Equality).foldLeft(Map.empty[Type, Symbol.DefnSym]) {
      case (macc, (tpe, sym)) =>
        val newSym = mkUncurried2(sym, newDefs, root)
        macc + (tpe -> newSym)
    }

    specialOps + (SpecialOperator.Equality -> newEqOps)
  }

  /**
    * Uncurries all definitions inside the given constraint `c`.
    */
  def visitConstraint(c: Constraint, newDefs: TopLevel, root: Root)(implicit flix: Flix): Constraint = c match {
    case Constraint(cparams, head0, body0) =>
      // TODO: Currently we do not need to uncurry in the head because of an earlier transformation.
      val head = head0
      val body = body0.map(visitBodyPredicate(_, newDefs, root))
      Constraint(cparams, head, body)
  }

  /**
    * Uncurries all definitions inside the given head predicate `h`.
    */
  def visitHeadPredicate(h: Predicate.Head, newDefs: TopLevel, root: Root)(implicit flix: Flix): Predicate.Head = h match {
    case Predicate.Head.Atom(pred, terms, tpe, loc) =>
      val ts = terms.map(visitHeadTerm(_, newDefs, root))
      Predicate.Head.Atom(pred, ts, tpe, loc)
  }

  /**
    * Uncurries all definitions inside the given body predicate `h`.
    */
  def visitBodyPredicate(b: Predicate.Body, newDefs: TopLevel, root: Root)(implicit flix: Flix): Predicate.Body = b match {
    case Predicate.Body.Atom(pred, polarity, terms, tpe, loc) => b

    case Predicate.Body.Functional(sym, term, loc) =>
      val t = visitHeadTerm(term, newDefs, root)
      Predicate.Body.Functional(sym, t, loc)

    case Predicate.Body.Guard(exp, loc) => b

    case Predicate.Body.Filter(sym, terms, loc) => terms match {
      case Nil => b
      case x :: Nil => b
      case x :: y :: Nil =>
        val freshSym = mkUncurried2(sym, newDefs, root)
        Predicate.Body.Filter(freshSym, x :: y :: Nil, loc)

      case _ =>
        throw InternalCompilerException(s"Unable to uncurry definition of arity n > 2.")
    }
  }

  /**
    * Uncurries all definitions inside the given head term `t`.
    */
  def visitHeadTerm(t: Term.Head, newDefs: TopLevel, root: Root)(implicit flix: Flix): Term.Head = t match {
    case Term.Head.QuantVar(sym, tpe, loc) => t
    case Term.Head.CapturedVar(sym, tpe, loc) => t
    case Term.Head.Lit(lit, tpe, loc) => t
    case Term.Head.App(sym, terms, tpe, loc) => terms match {
      case Nil => t
      case x :: Nil => t
      case x :: y :: Nil =>
        val freshSym = mkUncurried2(sym, newDefs, root)
        Term.Head.App(freshSym, x :: y :: Nil, tpe, loc)
      case _ =>
        throw InternalCompilerException(s"Unable to uncurry definition of arity n > 2.")
    }
  }

  /**
    * Introduces an uncurried version of the given binary function definition.
    */
  def mkUncurried2(sym: Symbol.DefnSym, newDefs: TopLevel, root: Root)(implicit flix: Flix): Symbol.DefnSym = {
    // Lookup the original definition.
    val defn = root.defs(sym)

    // The type of the 1st argument.
    val typeX = getFstArg(defn.tpe)

    // The type of the 2nd argument.
    val typeY = getSndArg(defn.tpe)

    // The return type.
    val returnType = getReturnType(defn.tpe)

    // Construct a fresh definition that takes two arguments.
    val loc = SourceLocation.Unknown
    val ann = defn.ann
    val mod = defn.mod

    // Construct a fresh symbol for the new definition.
    val freshSym = Symbol.freshDefnSym(sym)

    // Construct fresh symbols for the two formal parameters.
    val varX = Symbol.freshVarSym("x")
    val varY = Symbol.freshVarSym("y")
    val paramX = FormalParam(varX, mod, typeX, loc)
    val paramY = FormalParam(varY, mod, typeY, loc)
    val fs = List(paramX, paramY)

    // Construct an expression that calls the original symbol passing one argument at a time.
    val innerExp = Expression.Def(sym, Type.mkArrow(List(typeX, typeY), returnType), loc)
    val innerApply = Expression.Apply(innerExp, List(Expression.Var(varX, typeX, loc)), Type.mkArrow(typeY, returnType), loc)
    val outerApply = Expression.Apply(innerApply, List(Expression.Var(varY, typeY, loc)), returnType, loc)

    // Construct the uncurried definition.
    val uncurriedType = Type.mkUncurriedArrow(List(typeX, typeY), returnType)
    val uncurriedDefn = Def(ann, mod, freshSym, fs, outerApply, uncurriedType, loc)

    // Add it to the global map of new definitions.
    newDefs += (freshSym -> uncurriedDefn)

    // Return the fresh symbol.
    freshSym
  }

  /**
    * Returns the 1st argument of the given binary arrow type `tpe`.
    */
  def getFstArg(tpe: Type): Type = tpe.typeArguments.head

  /**
    * Returns the 2nd argument of the given binary arrow type `tpe`.
    */
  def getSndArg(tpe: Type): Type = tpe.typeArguments.tail.head.typeArguments.head

  /**
    * Returns the return type of the given binary arrow type `tpe`.
    */
  def getReturnType(tpe: Type): Type = tpe.typeArguments.tail.head.typeArguments.tail.head

}

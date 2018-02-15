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
import ca.uwaterloo.flix.language.ast.Ast.{Annotations, Modifiers}
import ca.uwaterloo.flix.language.ast.SimplifiedAst._
import ca.uwaterloo.flix.language.ast.{SourceLocation, SpecialOperator, Symbol, Type}
import ca.uwaterloo.flix.util.Validation
import ca.uwaterloo.flix.util.Validation._

import scala.collection.mutable

/**
  * Introduces uncurried versions of certain definitions where needed for interop.
  */
object Uncurrier extends Phase[Root, Root] {

  /**
    * Mutable map of top level definitions.
    */
  private type TopLevel = mutable.Map[Symbol.DefnSym, Def]

  /**
    * Introduces uncurried definitions where needed.
    */
  def run(root: Root)(implicit flix: Flix): Validation[Root, CompilationError] = {
    // A mutable map to hold new top-level definitions.
    val newDefs: TopLevel = mutable.Map.empty

    // Uncurry symbols in constraints.
    val newStrata = root.strata.map(visitStratum(_, newDefs, root))

    // Uncurry special operations.
    val newSpecialOps = visitSpecialOps(root.specialOps, newDefs, root)

    // Reassemble the ast.
    root.copy(defs = root.defs ++ newDefs, strata = newStrata, specialOps = newSpecialOps).toSuccess
  }

  /**
    * Uncurries special operators where needed.
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
    * Uncurries symbols in the given stratum.
    */
  def visitStratum(s: Stratum, newDefs: TopLevel, root: Root)(implicit flix: Flix): Stratum = s match {
    case Stratum(constraints) => Stratum(constraints.map(visitConstraint(_, newDefs, root)))
  }

  // TODO
  def visitConstraint(c: Constraint, newDefs: TopLevel, root: Root)(implicit flix: Flix): Constraint = c match {
    case Constraint(cparams, head, body) =>
      Constraint(cparams, visitHeadPredicate(head, newDefs, root), body.map(visitBodyPredicate(_, newDefs, root)))
  }

  // TODO
  def visitHeadPredicate(h: Predicate.Head, newDefs: TopLevel, root: Root)(implicit flix: Flix): Predicate.Head = h

  // TODO
  def visitBodyPredicate(b: Predicate.Body, newDefs: TopLevel, root: Root)(implicit flix: Flix): Predicate.Body = b match {
    case Predicate.Body.Atom(_, _, _, _) => b
    case Predicate.Body.Filter(sym, terms, loc) =>
      // TODO: Refactor and document.
      if (terms.length == 2) {
        val freshSym = mkUncurried2(sym, newDefs, root)
        Predicate.Body.Filter(freshSym, terms, loc)
      }
      else
        b
    case Predicate.Body.Loop(_, _, _) => b
  }

  /**
    * Constructs an uncurried version of the given binary definition.
    */
  def mkUncurried2(sym: Symbol.DefnSym, newDefs: TopLevel, root: Root)(implicit flix: Flix): Symbol.DefnSym = {
    implicit val _ = flix.genSym

    // Lookup the original definition.
    val defn = root.defs(sym)

    // The type of the 1st argument.
    val typeX = defn.tpe.typeArguments.head

    // The type of the 2nd argument.
    val typeY = defn.tpe.typeArguments.tail.head.typeArguments.head

    // The return type.
    val returnType = defn.tpe.typeArguments.tail.head.typeArguments.tail.head

    // Construct a fresh definition that takes two arguments.
    val loc = SourceLocation.Unknown
    val ann = Annotations.Empty
    val mod = Modifiers.Empty

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
    val uncurriedType = Type.mkArrowNoCurry(List(typeX, typeY), returnType)
    val uncurriedDefn = Def(ann, mod, freshSym, fs, outerApply, uncurriedType, loc)

    // Add it to the global map of new definitions.
    newDefs += (freshSym -> uncurriedDefn)

    // Return the fresh symbol.
    freshSym
  }

}

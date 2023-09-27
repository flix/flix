/*
 * Copyright 2023 Matthew Lutze
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
package ca.uwaterloo.flix.language.phase.constraintgeneration

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.{Ast, Kind, KindedAst, LevelEnv, RigidityEnv, SourceLocation, Symbol, Type}

import scala.collection.mutable.ListBuffer

sealed trait Constraint {
  override def toString: String = this match {
    case Constraint.Equality(tpe1, tpe2, lenv, prov, loc) => s"$tpe1 ~ $tpe2" // TODO ASSOC-TYPES ignoring lenv
    case Constraint.Class(sym, tpe, lenv, loc) => s"$sym[$tpe]"
  }
}

object Constraint {
  case class Equality(tpe1: Type, tpe2: Type, lenv: LevelEnv, prov: Provenance, loc: SourceLocation) extends Constraint

  case class Class(sym: Symbol.ClassSym, tpe: Type, lenv: LevelEnv, loc: SourceLocation) extends Constraint


  /**
    * Contains information to perform type unification in an expression.
    */
  case class Context(constrs: ListBuffer[Constraint], var renv: RigidityEnv, var lenv: LevelEnv)

  sealed trait Provenance

  object Provenance {

    /**
      * The constraint indicates that the left type is the expected type, while the right type is the actual type.
      */
    object ExpectLeft extends Provenance

    /**
      * The constraint indicates that the left type is the expected type of the `n`th argument to a function.
      */
    case class ExpectLeftArgument(sym: Symbol, num: Int) extends Provenance

    /**
      * The constraint indicates that the types must match.
      */
    object Match extends Provenance
  }

  object Context {
    def empty(): Context = Context(ListBuffer.empty, RigidityEnv.empty, LevelEnv.Top)
  }

  /**
    * Generates constraints unifying the given types.
    */
  def unifyTypeM(tpe1: Type, tpe2: Type, loc: SourceLocation)(implicit c: Context): Unit = {
    c.constrs.append(Constraint.Equality(tpe1, tpe2, c.lenv, Provenance.Match, loc))
  }

  /**
    * Generates constraints unifying the given types.
    */
  def unifyAllTypesM(tpes: List[Type], kind: Kind, loc: SourceLocation)(implicit c: Context, flix: Flix): Type = {
    tpes match {
      case tpe1 :: rest =>
        rest.foreach(unifyTypeM(tpe1, _, loc))
        tpe1
      case Nil => Type.freshVar(kind, loc.asSynthetic)
    }
  }

  /**
    * Generates constraints expecting the given type arguments to unify.
    */
  def expectTypeArguments(sym: Symbol, expectedTypes: List[Type], actualTypes: List[Type], actualLocs: List[SourceLocation], loc: SourceLocation)(implicit c: Context, root: KindedAst.Root, flix: Flix): Unit = {
    expectedTypes.zip(actualTypes).zip(actualLocs).zipWithIndex.foreach {
      case (((expectedType, actualType), loc), index) =>
        val oneBasedIndex = index + 1
        val constr = Constraint.Equality(expectedType, actualType, c.lenv, Provenance.ExpectLeftArgument(sym, oneBasedIndex), loc)
        c.constrs.append(constr)
    }
  }

  /**
    * Generates constraints unifying the given types.
    */
  def unifyType3M(tpe1: Type, tpe2: Type, tpe3: Type, loc: SourceLocation)(implicit c: Context): Unit = {
    unifyTypeM(tpe1, tpe2, loc)
    unifyTypeM(tpe1, tpe3, loc)
  }

  /**
    * Generates constraints unifying the given effects.
    */
  // TODO ASSOC-TYPES this should actually do something
  def unifyEffM(tpe1: Type, tpe2: Type, loc: SourceLocation)(implicit c: Context, flix: Flix): Unit = {
    unifyTypeM(tpe1, tpe2, loc)
  }

  /**
    * Generates constraints expecting the given types to unify.
    */
  def expectTypeM(expected: Type, actual: Type, loc: SourceLocation)(implicit c: Context): Unit = {
    c.constrs.append(Constraint.Equality(expected, actual, c.lenv, Provenance.ExpectLeft, loc))
  }

  /**
    * Generates constraints expecting the given types to unify, binding them to the bound type.
    */
  // TODO ASSOC-TYPES what does this do?
  def expectTypeBindM(expected: Type, actual: Type, bind: Type, loc: SourceLocation)(implicit c: Context): Unit = {
    expectTypeM(expected, actual, loc)
    unifyTypeM(expected, bind, loc)
  }

  /**
    * Generates constraints unifying the given Booleans.
    */
  // TODO ASSOC-TYPES this should actually do something
  def unifyBoolM(tpe1: Type, tpe2: Type, loc: SourceLocation)(implicit c: Context): Unit = {
    unifyTypeM(tpe1, tpe2, loc)
  }

  /**
    * Adds the given class constraints to the context.
    */
  def addTypeConstraintsM(tconstrs0: List[Ast.TypeConstraint], loc: SourceLocation)(implicit c: Context): Unit = {
    val tconstrs = tconstrs0.map {
      case Ast.TypeConstraint(head, arg, _) => Constraint.Class(head.sym, arg, c.lenv, loc)
    }
    c.constrs.addAll(tconstrs)
  }

  /**
    * Marks the given type variable as rigid in the context.
    */
  def rigidifyM(sym: Symbol.KindedTypeVarSym)(implicit c: Context): Unit = {
    c.renv = c.renv.markRigid(sym)
  }

  /**
    * Enters the type variable's scope in the context.
    */
  def enterScopeM(sym: Symbol.KindedTypeVarSym)(implicit c: Context): Unit = {
    c.lenv = c.lenv.enterScope(sym)
  }

  /**
    * Exits the type variable's scope in the context.
    */
  def exitScopeM(sym: Symbol.KindedTypeVarSym)(implicit c: Context): Unit = {
    c.lenv = c.lenv.exitScope(sym)
  }
}

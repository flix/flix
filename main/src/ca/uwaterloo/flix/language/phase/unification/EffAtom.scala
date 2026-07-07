/*
 *  Copyright 2024 Jonathan Lindegaard Starup
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

import ca.uwaterloo.flix.language.ast.shared.RegionScope
import ca.uwaterloo.flix.language.ast.shared.SymUse.AssocTypeSymUse
import ca.uwaterloo.flix.language.ast.{Kind, RigidityEnv, SourceLocation, Symbol, Type, TypeConstructor}

import scala.annotation.tailrec
import scala.collection.mutable

/**
  * Atomic effects that can be represented as atoms in
  * [[ca.uwaterloo.flix.language.phase.unification.set.SetFormula]]. Atomic effects are
  * variables, effects, errors, or simple associated effects, described in the grammar below.
  *
  * atom ::= VarFlex | VarRigid | Eff | Error | atomAssoc
  * atomAssoc ::= Assoc atomAssoc | VarRigid
  */
private sealed trait EffAtom extends Ordered[EffAtom] {
  override def compare(that: EffAtom): Int = (this, that) match {
    case (EffAtom.VarFlex(sym1), EffAtom.VarFlex(sym2)) => sym1.id - sym2.id
    case (EffAtom.VarRigid(sym1), EffAtom.VarRigid(sym2)) => sym1.id - sym2.id
    case (EffAtom.Eff(sym1), EffAtom.Eff(sym2)) => sym1.compare(sym2)
    case (EffAtom.Region(sym1), EffAtom.Region(sym2)) => sym1.compare(sym2)
    case (EffAtom.Assoc(sym1, arg1), EffAtom.Assoc(sym2, arg2)) =>
      val symCmp = sym1.compare(sym2)
      if (symCmp != 0) symCmp else arg1.compare(arg2)
    case (EffAtom.Error(id1), EffAtom.Error(id2)) => id1 - id2
    case _ =>
      def ordinal(a: EffAtom): Int = a match {
        case EffAtom.VarFlex(_) => 0
        case EffAtom.VarRigid(_) => 1
        case EffAtom.Region(_) => 2
        case EffAtom.Eff(_) => 3
        case EffAtom.Assoc(_, _) => 4
        case EffAtom.Error(_) => 5
      }

      ordinal(this) - ordinal(that)
  }
}

private object EffAtom {
  /** Representing a flexible variable. */
  case class VarFlex(sym: Symbol.KindedTypeVarSym) extends EffAtom

  /** Representing a rigid variable. */
  case class VarRigid(sym: Symbol.KindedTypeVarSym) extends EffAtom

  /** Representing an effect constant. */
  case class Eff(sym: Symbol.EffSym) extends EffAtom

  /** Represents an associated effect. */
  case class Assoc(sym: Symbol.AssocTypeSym, arg: EffAtom) extends EffAtom

  /** Represents a region. */
  case class Region(sym: Symbol.RegionSym) extends EffAtom

  /** Represents an error type. */
  case class Error(id: Int) extends EffAtom

  /** Returns the [[EffAtom]] representation of `t` or throws [[InvalidType]]. */
  @tailrec
  def fromType(t: Type)(implicit scope: RegionScope, renv: RigidityEnv): EffAtom = t match {
    case Type.Var(sym, _) if renv.isRigid(sym) => EffAtom.VarRigid(sym)
    case Type.Var(sym, _) => EffAtom.VarFlex(sym)
    case Type.Cst(TypeConstructor.Effect(sym, _), _) => EffAtom.Eff(sym)
    case Type.Cst(TypeConstructor.Region(sym), _) => EffAtom.Region(sym)
    case assoc@Type.AssocType(_, _, _, _) => assocFromType(assoc)
    case Type.Cst(TypeConstructor.Error(id, _), _) => EffAtom.Error(id)
    case Type.Alias(_, _, tpe, _) => fromType(tpe)
    case _ => throw InvalidType(t)
  }

  /** Returns the [[EffAtom]] representation of `t` or throws [[InvalidType]]. */
  private def assocFromType(t: Type)(implicit scope: RegionScope, renv: RigidityEnv): EffAtom = t match {
    case Type.Var(sym, _) if renv.isRigid(sym) => EffAtom.VarRigid(sym)
    case Type.AssocType(AssocTypeSymUse(sym, _), arg, _, _) => EffAtom.Assoc(sym, assocFromType(arg))
    case Type.Alias(_, _, tpe, _) => assocFromType(tpe)
    case _ => throw InvalidType(t)
  }

  /**
    * Adds the valid [[EffAtom]]s that occur in `t` (according to [[EffAtom.fromType]]) to `acc`.
    * Invalid or unrelated types are ignored.
    *
    * The validity of atoms are checked top-down, so even though `MyTrait.MyType[x]` is a valid
    * atom where `x` is rigid, `collectAtoms(MyTrait.MyType[MyTrait.MyType[x] ∪ IO], acc)` will
    * add nothing since the outermost associated type is not valid. This behaviour aligns with
    * the needs of [[EffUnification3.toSetFormula]].
    *
    * Examples:
    *   - `collectAtoms(Crash ∪ ef, acc)` adds `Eff(Crash)` and `VarFlex(ef)` (if
    *     [[RigidityEnv.isRigid]] is false for `ef`)
    *   - `collectAtoms(Indexable.Aef[Error], acc)` adds nothing
    */
  def collectAtoms(t: Type, acc: mutable.HashSet[EffAtom])(implicit scope: RegionScope, renv: RigidityEnv): Unit = t match {
    case Type.Var(sym, _) if renv.isRigid(sym) => acc += EffAtom.VarRigid(sym)
    case Type.Var(sym, _) => acc += EffAtom.VarFlex(sym)
    case Type.Cst(TypeConstructor.Effect(sym, _), _) => acc += EffAtom.Eff(sym)
    case Type.Cst(TypeConstructor.Region(sym), _) => acc += EffAtom.Region(sym)
    case Type.Cst(TypeConstructor.Error(id, _), _) => acc += EffAtom.Error(id)
    case Type.Apply(tpe1, tpe2, _) =>
      collectAtoms(tpe1, acc)
      collectAtoms(tpe2, acc)
    case Type.Alias(_, _, tpe, _) => collectAtoms(tpe, acc)
    case assoc@Type.AssocType(_, _, _, _) => getAssocAtoms(assoc).foreach(acc += _)
    case _ => ()
  }

  /**
    * Returns the [[EffAtom]] of `t` if it is a valid associated [[EffAtom]] (according to
    * [[EffAtom.assocFromType]]). Invalid or unrelated types return [[None]].
    */
  private def getAssocAtoms(t: Type)(implicit scope: RegionScope, renv: RigidityEnv): Option[EffAtom] = t match {
    case Type.Var(sym, _) if renv.isRigid(sym) => Some(EffAtom.VarRigid(sym))
    case Type.AssocType(AssocTypeSymUse(sym, _), arg, _, _) =>
      getAssocAtoms(arg).map(EffAtom.Assoc(sym, _))
    case Type.Alias(_, _, tpe, _) => getAssocAtoms(tpe)
    case _ => None
  }

  /**
    * Returns the [[Type]] represented by `atom` with location `loc`. The kind of errors and
    * associated types are set to be [[Kind.Eff]].
    */
  def toType(atom: EffAtom, loc: SourceLocation): Type = atom match {
    case EffAtom.Eff(sym) => Type.Cst(TypeConstructor.Effect(sym, Kind.Eff), loc)
    case EffAtom.Region(sym) => Type.Cst(TypeConstructor.Region(sym), loc)
    case EffAtom.VarRigid(sym) => Type.Var(sym, loc)
    case EffAtom.VarFlex(sym) => Type.Var(sym, loc)
    case EffAtom.Assoc(sym, arg0) =>
      Type.AssocType(AssocTypeSymUse(sym, loc), toType(arg0, loc), Kind.Eff, loc)
    case EffAtom.Error(id) => Type.Cst(TypeConstructor.Error(id, Kind.Eff), loc)
  }
}

/**
  * An exception used for partial functions that convert [[Type]] into [[EffAtom]].
  *
  * This exception should not leak outside this phase - it should always be caught. It is used to
  * avoid having [[Option]] types on recursive functions.
  */
private case class InvalidType(tpe: Type) extends RuntimeException

/*
 * Copyright 2015-2016 Magnus Madsen
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

package ca.uwaterloo.flix.language.ast

import ca.uwaterloo.flix.language.phase.GenSym
import ca.uwaterloo.flix.util.InternalCompilerException

import scala.collection.immutable

/**
  * Representation of monotypes.
  */
sealed trait Type {
  /**
    * The kind of `this` type.
    */
  def kind: Kind

  /**
    * Returns the type variables in `this` type.
    */
  def typeVars: Set[Type.Var] = this match {
    case x: Type.Var => Set(x)
    case Type.Unit => Set.empty
    case Type.Bool => Set.empty
    case Type.Char => Set.empty
    case Type.Float32 => Set.empty
    case Type.Float64 => Set.empty
    case Type.Int8 => Set.empty
    case Type.Int16 => Set.empty
    case Type.Int32 => Set.empty
    case Type.Int64 => Set.empty
    case Type.BigInt => Set.empty
    case Type.Str => Set.empty
    case Type.Native => Set.empty
    case Type.Arrow(l) => Set.empty
    case Type.FTuple(l) => Set.empty
    case Type.FOpt => Set.empty
    case Type.FList => Set.empty
    case Type.FVec => Set.empty
    case Type.FSet => Set.empty
    case Type.FMap => Set.empty
    case Type.Enum(enumName, cases, kind) => cases.flatMap {
      case (tagName, tpe) => tpe.typeVars
    }.toSet
    case Type.Apply(t, ts) => t.typeVars ++ ts.flatMap(_.typeVars)
    case Type.Forall(quantifiers, base) => base.typeVars -- quantifiers
  }

  /**
    * Returns `true` if `this` type is a tuple type.
    */
  def isTuple: Boolean = this match {
    case Type.FTuple(_) => true
    case Type.Apply(t1, t2) => t1.isTuple
    case _ => false
  }

  /**
    * Returns a human readable string representation of `this` type.
    */
  override def toString: String = this match {
    case Type.Var(x, k) => "'" + x
    case Type.Unit => "Unit"
    case Type.Bool => "Bool"
    case Type.Char => "Char"
    case Type.Float32 => "Float32"
    case Type.Float64 => "Float64"
    case Type.Int8 => "Int8"
    case Type.Int16 => "Int16"
    case Type.Int32 => "Int32"
    case Type.Int64 => "Int64"
    case Type.BigInt => "BigInt"
    case Type.Str => "Str"
    case Type.Native => "Native"
    case Type.Arrow(l) => "Arrow"
    case Type.FTuple(l) => "Tuple"
    case Type.FOpt => "Opt"
    case Type.FList => "List"
    case Type.FVec => "Vec"
    case Type.FSet => "Set"
    case Type.FMap => "Map"
    case Type.Apply(Type.Arrow(l), ts) => ts.mkString(" -> ")
    case Type.Apply(t, ts) => s"$t[${ts.mkString(", ")}]"
    case Type.Enum(enum, cases, kind) => enum.toString
    case Type.Forall(quantifiers, base) => s"∀(${quantifiers.mkString(", ")}). $base"
  }
}

object Type {

  /////////////////////////////////////////////////////////////////////////////
  // Types                                                                   //
  /////////////////////////////////////////////////////////////////////////////

  /**
    * A type variable expression.
    */
  case class Var(id: Int, kind: Kind) extends Type

  /**
    * A type constructor that represents the unit value.
    */
  case object Unit extends Type {
    def kind: Kind = Kind.Star
  }

  /**
    * A type constructor that represent boolean values.
    */
  case object Bool extends Type {
    def kind: Kind = Kind.Star
  }

  /**
    * A type constructor that represent character values.
    */
  case object Char extends Type {
    def kind: Kind = Kind.Star
  }

  /**
    * A type constructor that represent 32-bit floating point numbers.
    */
  case object Float32 extends Type {
    def kind: Kind = Kind.Star
  }

  /**
    * A type constructor that represent 64-bit floating point numbers.
    */
  case object Float64 extends Type {
    def kind: Kind = Kind.Star
  }

  /**
    * A type constructor that represent 8-bit signed integers.
    */
  case object Int8 extends Type {
    def kind: Kind = Kind.Star
  }

  /**
    * A type constructor that represent 16-bit signed integers.
    */
  case object Int16 extends Type {
    def kind: Kind = Kind.Star
  }

  /**
    * A type constructor that represent 32-bit signed integers.
    */
  case object Int32 extends Type {
    def kind: Kind = Kind.Star
  }

  /**
    * A type constructor that represent 64-bit signed integers.
    */
  case object Int64 extends Type {
    def kind: Kind = Kind.Star
  }

  /**
    * A type constructor that represent arbitrary-precision integers.
    */
  case object BigInt extends Type {
    def kind: Kind = Kind.Star
  }

  /**
    * A type constructor that represent strings.
    */
  case object Str extends Type {
    def kind: Kind = Kind.Star
  }

  /**
    * A type constructor that represent native objects.
    */
  case object Native extends Type {
    def kind: Kind = Kind.Star
  }

  /**
    * A type expression that represents functions.
    */
  case class Arrow(length: Int) extends Type {
    def kind: Kind = Kind.Arrow((0 until length).map(_ => Kind.Star).toList, Kind.Star)
  }

  /**
    * A type constructor that represents tuples of the given `length`.
    */
  case class FTuple(length: Int) extends Type {
    def kind: Kind = Kind.Arrow((0 until length).map(_ => Kind.Star).toList, Kind.Star)
  }

  /**
    * A type constructor that represents options.
    */
  case object FOpt extends Type {
    def kind: Kind = Kind.Arrow(List(Kind.Star), Kind.Star)
  }

  /**
    * A type constructor that represents list values.
    */
  case object FList extends Type {
    def kind: Kind = Kind.Arrow(List(Kind.Star), Kind.Star)
  }

  /**
    * A type constructor that represents vector values
    */
  case object FVec extends Type {
    def kind: Kind = Kind.Arrow(List(Kind.Star), Kind.Star)
  }

  /**
    * A type constructor that represents set values.
    */
  case object FSet extends Type {
    def kind: Kind = Kind.Arrow(List(Kind.Star), Kind.Star)
  }

  /**
    * A type constructor that represents map values.
    */
  case object FMap extends Type {
    def kind: Kind = Kind.Arrow(List(Kind.Star, Kind.Star), Kind.Star)
  }

  /**
    * A type constructor that represents enums.
    *
    * @param sym   the symbol of the enum.
    * @param cases a map from tag names to tag types.
    * @param kind  the kind of the enum.
    */
  case class Enum(sym: Symbol.EnumSym, cases: immutable.Map[String, Type], kind: Kind) extends Type

  /**
    * A type expression that represents the application of `ts` to `t`.
    */
  case class Apply(t: Type, ts: List[Type]) extends Type {
    /**
      * Returns the kind of `this` type.
      *
      * The kind of a type application can unique be determined
      * from the kind of the first type argument `t`.
      */
    def kind: Kind = t.kind match {
      case Kind.Star => throw InternalCompilerException("Illegal kind.")
      case Kind.Arrow(_, k) => k
    }
  }

  /**
    * A universally quantified type expression.
    */
  // TODO: Move into Scheme.
  //TODO: Deprecated
  case class Forall(quantifiers: List[Type.Var], base: Type) extends Type {
    def kind: Kind = base.kind
  }

  /////////////////////////////////////////////////////////////////////////////
  // Helper Functions                                                        //
  /////////////////////////////////////////////////////////////////////////////
  /**
    * Returns a fresh type variable.
    */
  def freshTypeVar(k: Kind = Kind.Star)(implicit genSym: GenSym): Type.Var = Type.Var(genSym.freshId(), k)

  /**
    * Constructs the function type A -> B where `A` is the given type `a` and `B` is the given type `b`.
    */
  def mkArrow(a: Type, b: Type): Type = Apply(Arrow(2), List(a, b))

  /**
    * Constructs the function type [A] -> B where `A` is the given sequence of types `as` and `B` is the given type `b`.
    */
  def mkArrow(as: List[Type], b: Type): Type = Apply(Arrow(as.length + 1), as ::: b :: Nil)

  /**
    * Constructs the type Opt[A] where `A` is the given type `tpe`.
    */
  def mkFOpt(a: Type): Type = Apply(FOpt, List(a))

  /**
    * Constructs the tuple type (A, B, ...) where the types are drawn from the list `ts`.
    */
  def mkFTuple(ts: List[Type]): Type = Apply(FTuple(ts.length), ts)

  /**
    * Constructs the type List[A] where `A` is the given type `tpe`.
    */
  def mkFList(a: Type): Type = Apply(FList, List(a))

  /**
    * Constructs the type Vec[A] where `A` is the given type `tpe`.
    */
  def mkFVec(a: Type): Type = Apply(FVec, List(a))

  /**
    * Constructs the type Set[A] where `A` is the given type `tpe`.
    */
  def mkFSet(a: Type): Type = Apply(FSet, List(a))

  /**
    * Constructs the type Map[K, V] where `K` is the given type `k` and `V` is the given type `v`.
    */
  def mkFMap(k: Type, v: Type): Type = Apply(FMap, List(k, v))

  /**
    * Constructors the universally quantified type ∀(xs...) base.
    *
    * Returns the base type if the given list of quantifiers is empty.
    */
  //TODO: Deprecated
  def mkForall(quantifiers: List[Type.Var], base: Type): Type = if (quantifiers.isEmpty) base else Type.Forall(quantifiers, base)

  /**
    * Instantiates the given type `tpe` by replacing all quantified type variables with fresh type variables.
    */
  //TODO: Deprecated
  def instantiate(tpe: Type)(implicit genSym: GenSym): Type = tpe match {
    case Type.Forall(quantifiers, base) => refreshTypeVars(quantifiers, base)
    case _ => tpe
  }

  /**
    * Replaces every free occurrence of a type variable in `typeVars`
    * with a fresh type variable in the given type `tpe`.
    */
  def refreshTypeVars(typeVars: List[Type.Var], tpe: Type)(implicit genSym: GenSym): Type = {
    val freshVars = typeVars.foldLeft(Map.empty[Int, Type.Var]) {
      case (macc, tvar) => macc + (tvar.id -> freshTypeVar(tvar.kind))
    }

    /**
      * Replaces every variable occurrence in the given type using the map `freeVars`.
      */
    def visit(t0: Type): Type = t0 match {
      case Type.Var(x, k) => freshVars.getOrElse(x, t0)
      case Type.Unit => Type.Unit
      case Type.Bool => Type.Bool
      case Type.Char => Type.Char
      case Type.Float32 => Type.Float32
      case Type.Float64 => Type.Float64
      case Type.Int8 => Type.Int8
      case Type.Int16 => Type.Int16
      case Type.Int32 => Type.Int32
      case Type.Int64 => Type.Int64
      case Type.BigInt => Type.BigInt
      case Type.Str => Type.Str
      case Type.Native => Type.Native
      case Type.Arrow(l) => Type.Arrow(l)
      case Type.FTuple(l) => Type.FTuple(l)
      case Type.FOpt => Type.FOpt
      case Type.FList => Type.FList
      case Type.FVec => Type.FVec
      case Type.FSet => Type.FSet
      case Type.FMap => Type.FMap
      case Type.Apply(t, ts) => Type.Apply(visit(t), ts map visit)
      case Type.Enum(enum, cases, kind) => Type.Enum(enum, cases map {
        case (k, v) => k -> visit(v)
      }, kind)
      case Type.Forall(quantifiers, base) => Type.Forall(quantifiers, visit(base))
    }

    visit(tpe)
  }

}
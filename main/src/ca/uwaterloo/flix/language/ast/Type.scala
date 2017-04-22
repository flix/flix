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

import ca.uwaterloo.flix.language.GenSym
import ca.uwaterloo.flix.util.InternalCompilerException

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
    case Type.Enum(enumName, kind) => Set.empty
    case Type.Apply(t, ts) => t.typeVars ++ ts.flatMap(_.typeVars)
  }

  /**
    * Returns `true` if `this` type is an enum type.
    */
  def isEnum: Boolean = this match {
    case Type.Enum(sym, kind) => true
    case Type.Apply(t, ts) => t.isEnum
    case _ => false
  }

  /**
    * Returns `true` if `this` type is a tuple type.
    */
  def isTuple: Boolean = this match {
    case Type.FTuple(l) => true
    case Type.Apply(t, ts) => t.isTuple
    case _ => false
  }

  /**
    * Returns a human readable string representation of `this` type.
    */
  override def toString: String = this match {
    case tvar@Type.Var(x, k) => tvar.getText.getOrElse("'" + x)
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
    case Type.Arrow(l) => s"Arrow($l)"
    case Type.FTuple(l) => s"Tuple($l)"
    case Type.Apply(Type.FTuple(l), ts) => "(" + ts.mkString(", ") + ")"
    case Type.Apply(Type.Arrow(l), ts) => ts.mkString(" -> ")
    case Type.Apply(t, ts) => s"$t[${ts.mkString(", ")}]"
    case Type.Enum(enum, kind) => enum.toString
  }
}

object Type {

  /////////////////////////////////////////////////////////////////////////////
  // Types                                                                   //
  /////////////////////////////////////////////////////////////////////////////

  /**
    * A type variable expression.
    */
  case class Var(id: Int, kind: Kind) extends Type {
    /**
      * The optional textual name of `this` type variable.
      */
    private var text: Option[String] = None

    /**
      * Optionally returns the textual name of `this` type variable.
      */
    def getText: Option[String] = text

    /**
      * Sets the textual name of `this` type variable.
      */
    def setText(s: String): Unit = {
      text = Some(s)
    }

    /**
      * Returns `true` if `this` type variable is equal to `o`.
      */
    override def equals(o: scala.Any): Boolean = o match {
      case that: Var => this.id == that.id
      case _ => false
    }

    /**
      * Returns the hash code of `this` type variable.
      */
    override def hashCode(): Int = id
  }

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
    * A type constructor that represents enums.
    *
    * @param sym  the symbol of the enum.
    * @param kind the kind of the enum.
    */
  case class Enum(sym: Symbol.EnumSym, kind: Kind) extends Type

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
    * Constructs the tuple type (A, B, ...) where the types are drawn from the list `ts`.
    */
  def mkFTuple(ts: List[Type]): Type = Apply(FTuple(ts.length), ts)

  /**
    * Constructs the set type of A.
    */
  def mkFSet(a: Type): Type = Type.Apply(Type.Enum(Symbol.mkEnumSym("Set"), Kind.Arrow(List(Kind.Star), Kind.Star)), List(a))

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
      case Type.Apply(t, ts) => Type.Apply(visit(t), ts map visit)
      case Type.Enum(enum, kind) => Type.Enum(enum, kind)
    }

    visit(tpe)
  }

}
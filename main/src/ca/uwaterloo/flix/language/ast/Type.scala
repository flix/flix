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

import ca.uwaterloo.flix.util.InternalCompilerException

import scala.collection.immutable

/**
  * A common super-type for types.
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
    case Type.Arrow => Set.empty
    case Type.FTuple(l) => Set.empty
    case Type.FOpt => Set.empty
    case Type.FList => Set.empty
    case Type.FVec => Set.empty
    case Type.FMap => Set.empty
    case Type.Enum(_, cases) => (cases flatMap {
      case (_, Type.Tag(_, _, tpe)) => tpe.typeVars
    }).toSet
    case Type.Apply(t1, t2) => t1.typeVars ++ t2.typeVars

    case _ => throw InternalCompilerException(s"Unexpected type: `${this}'.")
  }

  /**
    * Returns a human readable string representation of `this` type.
    */
  override def toString: String = this match {
    case Type.Var(x, k) => s"Var($x)"
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

    case Type.Apply(Type.Apply(Type.Arrow, t2), t1) => t1.toString + " -> " + t2.toString
    case Type.Apply(Type.FOpt, t) => "Opt[" + t.toString + "]"
    case Type.Apply(Type.FList, t) => "List[" + t.toString + "]"
    case Type.Apply(Type.FVec, t) => "Vec[" + t.toString + "]"
    case Type.Apply(Type.FSet, t) => "Set[" + t.toString + "]"
    case Type.Apply(Type.Apply(Type.FMap, k), v) => "Map[" + k.toString + ", " + v.toString + "]"

    case Type.Tag(enum, tag, tpe) => tag.name + "(" + tpe + ")"
    case Type.Enum(enum, cases) => enum.fqn
    case Type.Tuple(elms) => "(" + elms.mkString(". ") + ")"

    case _ => super.toString
  }
}

object Type {

  /////////////////////////////////////////////////////////////////////////////
  // Types                                                                   //
  /////////////////////////////////////////////////////////////////////////////

  /**
    * A type variable expression.
    */
  case class Var(name: String, kind: Kind) extends Type

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
  object Arrow extends Type {
    def kind: Kind = Kind.Arrow(Kind.Star, Kind.Arrow(Kind.Star, Kind.Star))
  }

  /**
    * A type constructor that represents tuples of the given `length`.
    */
  case class FTuple(length: Int) extends Type {
    def kind: Kind = (0 until length).foldLeft(Kind.Star: Kind) {
      case (kacc, _) => Kind.Arrow(Kind.Star, kacc)
    }
  }

  /**
    * A type constructor that represents options.
    */
  case object FOpt extends Type {
    def kind: Kind = Kind.Arrow(Kind.Star, Kind.Star)
  }

  /**
    * A type constructor that represents list values.
    */
  case object FList extends Type {
    def kind: Kind = Kind.Arrow(Kind.Star, Kind.Star)
  }

  /**
    * A type constructor that represents vector values
    */
  case object FVec extends Type {
    def kind: Kind = Kind.Arrow(Kind.Star, Kind.Star)
  }

  /**
    * A type constructor that represents set values.
    */
  case object FSet extends Type {
    def kind: Kind = Kind.Arrow(Kind.Star, Kind.Star)
  }

  /**
    * A type constructor that represents map values.
    */
  case object FMap extends Type {
    def kind: Kind = Kind.Arrow(Kind.Star, Kind.Arrow(Kind.Star, Kind.Star))
  }

  /**
    * A type constructor that represents enums.
    *
    * @param name  the fully qualified name of the enum.
    * @param cases a map from tag names to tag types.
    */
  case class Enum(name: Symbol.Resolved, cases: immutable.Map[String, Type.Tag]) extends Type {
    def kind: Kind = Kind.Star
  }

  /**
    * A type expression that represents the application of `t2` to `t1`.
    */
  case class Apply(t1: Type, t2: Type) extends Type {
    /**
      * Returns the kind of `this` type.
      *
      * The kind of a type application can unique be determined
      * from the kind of the first type argument `t1`.
      */
    def kind: Kind = t1.kind match {
      case Kind.Star => throw InternalCompilerException("Illegal kind.")
      case Kind.Arrow(_, k) => k
    }
  }

  //
  // TODO: --- Everything below here may be removed ---
  //
  /**
    * An AST node that represents the proposition type.
    */
  // TODO: Remove?
  case object Prop extends Type {
    def kind: Kind = Kind.Star
  }

  /**
    * An AST node that represent a parametric type.
    *
    * @param name the ambiguous name.
    * @param elms the type of the type parameters.
    */
  // TODO: remove?
  case class Parametric(name: Name.QName, elms: Seq[Type]) extends Type {
    def kind: Kind = ???
  }


  //
  // TODO: --- Everything below here is considered deprecated  ---
  //


  /**
    * An AST node that represents a predicate type.
    *
    * @param terms the type of predicate terms.
    */
  // TODO: To be removed
  case class Predicate(terms: List[Type]) extends Type {
    def kind: Kind = Kind.Star
  }

  /**
    * An AST node that represent a reference to an unresolved type.
    *
    * @param name the name of the unresolved type.
    */
  // TODO: To be removed
  case class Unresolved(name: Name.QName) extends Type {
    def kind: Kind = Kind.Star
  }

  // TODO: To be removed
  case class Abs(name: Var, tpe: Type) extends Type {
    def kind: Kind = Kind.Star
  }

  // TODO: To be removed
  case object Any extends Type {
    def kind: Kind = Kind.Star
  }

  /**
    * An AST node that represents the type of a tag.
    *
    * @param enum the fully qualified name of the enum.
    * @param tag  the name of the tag.
    * @param tpe  the type of the nested value.
    */
  // TODO: To be removed
  case class Tag(enum: Symbol.Resolved, tag: Name.Ident, tpe: Type) extends Type {
    def kind: Kind = Kind.Star
  }

  /**
    * An AST node that represents a tuple type.
    *
    * @param elms the types of the elements.
    */
  case class Tuple(elms: List[Type]) extends Type {
    def kind: Kind = Kind.Star
  }

  /**
    * TODO:
    * The type of functions.
    *
    * @param args   the type of the arguments.
    * @param retTpe the type of the return type.
    */
  // TODO: Rename to arrow?
  case class Lambda(args: List[Type], retTpe: Type) extends Type {
    def kind: Kind = Kind.Star
  }

  /////////////////////////////////////////////////////////////////////////////
  // Helper Functions                                                        //
  /////////////////////////////////////////////////////////////////////////////
  /**
    * Constructs the function type A -> B where `A` is the given type `a` and `B` is the given type `b`.
    */
  def mkArrow(a: Type, b: Type): Type = Apply(Apply(Arrow, a), b)

  /**
    * Constructs the type Opt[A] where `A` is the given type `tpe`.
    */
  def mkFOpt(tpe: Type): Type = Apply(FOpt, tpe)

  /**
    * Constructs the tuple type (A, B, ...) where the types are drawn from the list `ts`.
    */
  def mkFTuple(ts: List[Type]): Type = ts.foldLeft(Type.FTuple(ts.length): Type) {
    case (tacc, tpe) => Apply(tacc, tpe)
  }

  /**
    * Constructs the type List[A] where `A` is the given type `tpe`.
    */
  def mkFList(tpe: Type): Type = Apply(FList, tpe)

  /**
    * Constructs the type Vec[A] where `A` is the given type `tpe`.
    */
  def mkFVec(tpe: Type): Type = Apply(FVec, tpe)

  /**
    * Constructs the type Set[A] where `A` is the given type `tpe`.
    */
  def mkFSet(tpe: Type): Type = Apply(FSet, tpe)

  /**
    * Constructs the type Map[K, V] where `K` is the given type `k` and `V` is the given type `v`.
    */
  def mkFMap(k: Type, v: Type): Type = Apply(Apply(FMap, k), v)

}
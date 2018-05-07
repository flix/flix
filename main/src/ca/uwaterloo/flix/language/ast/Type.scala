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
import ca.uwaterloo.flix.util.tc.Show

/**
  * Representation of types.
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
    case Type.Array => Set.empty
    case Type.Vector => Set.empty
    case Type.Native => Set.empty
    case Type.Ref => Set.empty
    case Type.Zero => Set.empty
    case Type.Succ(n, t) => Set.empty
    case Type.Arrow(l) => Set.empty
    case Type.Tuple(l) => Set.empty
    case Type.Enum(enumName, kind) => Set.empty
    case Type.Apply(tpe1, tpe2) => tpe1.typeVars ++ tpe2.typeVars
  }

  /**
    * Returns the type constructor of `this` type.
    *
    * For example,
    *
    * Celsius                       =>      Celsius
    * Option[Int]                   =>      Option
    * Arrow[Bool, Char]             =>      Arrow
    * Tuple[Bool, Int]              =>      Tuple
    * Result[Bool, Int]             =>      Result
    * Result[Bool][Int]             =>      Result
    * Option[Result[Bool, Int]]     =>      Option
    */
  def typeConstructor: Type = this match {
    case Type.Apply(t1, _) => t1.typeConstructor
    case _ => this
  }

  /**
    * Returns the type arguments of `this` type.
    *
    * For example,
    *
    * Celsius                       =>      Nil
    * Option[Int]                   =>      Int :: Nil
    * Arrow[Bool, Char]             =>      Bool :: Char :: Nil
    * Tuple[Bool, Int]              =>      Bool :: Int :: Nil
    * Result[Bool, Int]             =>      Bool :: Int :: Nil
    * Result[Bool][Int]             =>      Bool :: Int :: Nil
    * Option[Result[Bool, Int]]     =>      Result[Bool, Int] :: Nil
    */
  def typeArguments: List[Type] = this match {
    case Type.Apply(tpe1, tpe2) => tpe1.typeArguments ::: tpe2 :: Nil
    case _ => Nil
  }

  /**
    * Returns `true` if `this` type is an arrow type.
    */
  def isArrow: Boolean = typeConstructor match {
    case Type.Arrow(l) => true
    case _ => false
  }

  /**
    * Returns `true` if `this` type is an enum type.
    */
  def isEnum: Boolean = typeConstructor match {
    case Type.Enum(sym, kind) => true
    case _ => false
  }

  /**
    * Returns `true` if `this` type is a tuple type.
    */
  def isTuple: Boolean = typeConstructor match {
    case Type.Tuple(l) => true
    case _ => false
  }

  /**
    * Returns `true` if `this` type is a reference type.
    */
  def isRef: Boolean = typeConstructor match {
    case Type.Ref => true
    case _ => false
  }

  /**
    * Returns `true` if `this` type does not contain type variables.
    */
  def isDeterminate: Boolean = this match {
    case Type.Var(id, kind) => false
    case Type.Apply(tpe1, tpe2) => tpe1.isDeterminate && tpe2.isDeterminate
    case _ => true
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
    case Type.Array => "Array"
    case Type.Vector => "Vector"
    case Type.Zero => "Zero"
    case Type.Succ(n, t) => s"Successor($n, $t)"
    case Type.Native => "Native"
    case Type.Ref => "Ref"
    case Type.Arrow(l) => s"Arrow($l)"
    case Type.Enum(enum, kind) => enum.toString
    case Type.Tuple(l) => s"Tuple($l)"
    case Type.Apply(tpe1, tpe2) => s"$tpe1[$tpe2]"
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
    * A type constructor that represent arrays.
    */
  case object Array extends Type {
    def kind: Kind = Kind.Star
  }

  /**
    * A type constructor that represent vectors.
    */
  case object Vector extends Type {
    def kind: Kind = Kind.Star
  }

  /**
    * A type constructor that represent native objects.
    */
  case object Native extends Type {
    def kind: Kind = Kind.Star
  }

  /**
    * A type constructor that represents references.
    */
  case object Ref extends Type {
    def kind: Kind = Kind.Star
  }

  /**
    * A type expression that represents functions.
    */
  case class Arrow(length: Int) extends Type {
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
    * A type constructor that represents tuples of the given `length`.
    */
  case class Tuple(length: Int) extends Type {
    def kind: Kind = Kind.Arrow((0 until length).map(_ => Kind.Star).toList, Kind.Star)
  }

  case object Zero extends Type {
    def kind: Kind = Kind.Star
  }

  case class Succ(len: Int, t: Type) extends Type {
    def kind: Kind = Kind.Star
  }

  /**
    * A type expression that a type application tpe1[tpe2].
    */
  case class Apply(tpe1: Type, tpe2: Type) extends Type {
    /**
      * Returns the kind of `this` type.
      *
      * The kind of a type application can unique be determined
      * from the kind of the first type argument `t1`.
      */
    def kind: Kind = tpe1.kind match {
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
  def mkArrow(a: Type, b: Type): Type = Apply(Apply(Arrow(2), a), b)

  /**
    * Constructs the function type [A] -> B where `A` is the given sequence of types `as` and `B` is the given type `b`.
    */
  def mkArrow(as: List[Type], b: Type): Type = {
    val arrow = Arrow(as.length + 1)
    val inner = as.foldLeft(arrow: Type) {
      case (acc, x) => Apply(acc, x)
    }
    Apply(inner, b)
  }

  /**
    * Constructs the tuple type (A, B, ...) where the types are drawn from the list `ts`.
    */
  def mkTuple(ts: Type*): Type = mkTuple(ts.toList)

  /**
    * Constructs the tuple type (A, B, ...) where the types are drawn from the list `ts`.
    */
  def mkTuple(ts: List[Type]): Type = {
    val tuple = Tuple(ts.length)
    ts.foldLeft(tuple: Type) {
      case (acc, x) => Apply(acc, x)
    }
  }

  /**
    * Constructs the array type [elmType] where 'elmType' is the given type.
    */
  def mkArray(elmType: Type): Type = Apply(Array, elmType)

  /**
    * Constructs the vector type [|elmType, Len|] where
    * 'elmType' is the given element type
    * 'len' is the given length of the vector.
    * len expected input is an instance of Succ(Int, Type), where Int is the length, and Type is either Type.Zero or a fresh variable.
    */
  def mkVector(elmType: Type, len: Type) : Type = Apply(Apply(Vector, elmType), len)


  /**
    * Constructs the set type of A.
    */
  def mkFSet(a: Type): Type = {
    Type.Apply(Type.Enum(Symbol.mkEnumSym("Set"), Kind.Arrow(List(Kind.Star), Kind.Star)), a)
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
      case Type.Array => Type.Array
      case Type.Vector => Type.Vector
      case Type.Native => Type.Native
      case Type.Ref => Type.Ref
      case Type.Arrow(l) => Type.Arrow(l)
      case Type.Tuple(l) => Type.Tuple(l)
      case Type.Zero => Type.Zero
      case Type.Succ(n, t) => Type.Succ(n, t)
      case Type.Apply(tpe1, tpe2) => Type.Apply(visit(tpe1), visit(tpe2))
      case Type.Enum(enum, kind) => Type.Enum(enum, kind)
    }

    visit(tpe)
  }

  /////////////////////////////////////////////////////////////////////////////
  // Type Class Instances                                                    //
  /////////////////////////////////////////////////////////////////////////////
  /**
    * Show instance for Type.
    */
  implicit object ShowInstance extends Show[Type] {
    def show(a: Type): String = {
      /**
        * Local visitor.
        */
      def visit(tpe: Type, m: Map[Int, String]): String = {
        // Retrieve the type constructor and type arguments.
        val base = tpe.typeConstructor
        val args = tpe.typeArguments

        base match {
          //
          // Type Variable.
          //
          case Type.Var(id, kind) => m(id)

          //
          // Primitive Types.
          //
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
          case Type.Str => "String"
          case Type.Array => "Array"
          case Type.Vector => "Vector"
          case Type.Zero => "Zero"
          case Type.Succ(n, t) => n.toString + " " + t.toString
          case Type.Native => "Native"
          case Type.Ref => "Ref"

          //
          // Arrow.
          //
          case Type.Arrow(l) =>
            val argumentTypes = args.init
            val resultType = args.last
            if (argumentTypes.length == 1) {
              visit(argumentTypes.head, m) + " -> " + visit(resultType, m)
            } else {
              "(" + argumentTypes.map(visit(_, m)).mkString(", ") + ") -> " + visit(resultType, m)
            }

          //
          // Tuple.
          //
          case Type.Tuple(l) =>
            "(" + args.map(visit(_, m)).mkString(", ") + ")"

          //
          // Enum.
          //
          case Type.Enum(sym, kind) =>
            if (args.isEmpty) {
              sym.toString
            } else {
              sym.toString + "[" + args.map(visit(_, m)).mkString(", ") + "]"
            }

          //
          // Type Application.
          //
          case Type.Apply(tpe1, tpe2) => visit(tpe1, m) + "[" + visit(tpe2, m) + "]"
        }
      }

      //
      // Compute a mapping from type variables to human readable variable names.
      //
      // E.g. the type variable 8192 might be mapped to 'a'.
      //  and the type variable 8193 might be mapped to 'b'.
      //
      val var2str = a.typeVars.toList.sortBy(_.id).zipWithIndex.map {
        case (tvar, index) => tvar.id -> (index + 'a').toChar.toString
      }.toMap

      visit(a, var2str)
    }
  }

}
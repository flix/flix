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

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.util.InternalCompilerException
import ca.uwaterloo.flix.util.tc.Show

import scala.collection.immutable.SortedSet

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
  // NB: This must be a sorted set to ensure that the compiler is deterministic.
  def typeVars: SortedSet[Type.Var] = this match {
    case x: Type.Var => SortedSet(x)
    case Type.Cst(tc) => SortedSet.empty
    case Type.Zero => SortedSet.empty
    case Type.Succ(n, t) => t.typeVars
    case Type.Arrow(_, eff) => eff.typeVars
    case Type.RecordEmpty => SortedSet.empty
    case Type.RecordExtend(label, value, rest) => value.typeVars ++ rest.typeVars
    case Type.SchemaEmpty => SortedSet.empty
    case Type.SchemaExtend(sym, tpe, rest) => tpe.typeVars ++ rest.typeVars
    case Type.Lambda(tvar, tpe) => tpe.typeVars - tvar
    case Type.Apply(tpe1, tpe2) => tpe1.typeVars ++ tpe2.typeVars
  }

  /**
    * Returns the type constructor of `this` type.
    *
    * For example,
    *
    * {{{
    * Celsius                       =>      Celsius
    * Option[Int]                   =>      Option
    * Arrow[Bool, Char]             =>      Arrow
    * Tuple[Bool, Int]              =>      Tuple
    * Result[Bool, Int]             =>      Result
    * Result[Bool][Int]             =>      Result
    * Option[Result[Bool, Int]]     =>      Option
    * }}}
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
    * {{{
    * Celsius                       =>      Nil
    * Option[Int]                   =>      Int :: Nil
    * Arrow[Bool, Char]             =>      Bool :: Char :: Nil
    * Tuple[Bool, Int]              =>      Bool :: Int :: Nil
    * Result[Bool, Int]             =>      Bool :: Int :: Nil
    * Result[Bool][Int]             =>      Bool :: Int :: Nil
    * Option[Result[Bool, Int]]     =>      Result[Bool, Int] :: Nil
    * }}}
    */
  def typeArguments: List[Type] = this match {
    case Type.Apply(tpe1, tpe2) => tpe1.typeArguments ::: tpe2 :: Nil
    case _ => Nil
  }

  /**
    * Returns the size of `this` type.
    */
  def size: Int = this match {
    case Type.Var(_, _) => 1
    case Type.Cst(tc) => 1
    case Type.Arrow(_, eff) => eff.size + 1
    case Type.RecordEmpty => 1
    case Type.RecordExtend(_, value, rest) => value.size + rest.size
    case Type.SchemaEmpty => 1
    case Type.SchemaExtend(_, tpe, rest) => tpe.size + rest.size
    case Type.Zero => 1
    case Type.Succ(_, t) => t.size + 1
    case Type.Lambda(_, tpe) => tpe.size + 1
    case Type.Apply(tpe1, tpe2) => tpe1.size + tpe2.size + 1
  }

  /**
    * Returns a human readable string representation of `this` type.
    */
  override def toString: String = Type.fmtType(this, renameVars = false)

}

object Type {

  /////////////////////////////////////////////////////////////////////////////
  // Type Constants                                                          //
  /////////////////////////////////////////////////////////////////////////////

  /**
    * Represents the Unit type.
    */
  val Unit: Type = Type.Cst(TypeConstructor.Unit)

  /**
    * Represents the Bool type.
    */
  val Bool: Type = Type.Cst(TypeConstructor.Bool)

  /**
    * Represents the Char type.
    */
  val Char: Type = Type.Cst(TypeConstructor.Char)

  /**
    * Represents the Float32 type.
    */
  val Float32: Type = Type.Cst(TypeConstructor.Float32)

  /**
    * Represents the Float64 type.
    */
  val Float64: Type = Type.Cst(TypeConstructor.Float64)

  /**
    * Represents the Int8 type.
    */
  val Int8: Type = Type.Cst(TypeConstructor.Int8)

  /**
    * Represents the Int16 type.
    */
  val Int16: Type = Type.Cst(TypeConstructor.Int16)

  /**
    * Represents the Int32 type.
    */
  val Int32: Type = Type.Cst(TypeConstructor.Int32)

  /**
    * Represents the Int64 type.
    */
  val Int64: Type = Type.Cst(TypeConstructor.Int64)

  /**
    * Represents the BigInt type.
    */
  val BigInt: Type = Type.Cst(TypeConstructor.BigInt)

  /**
    * Represents the String type.
    */
  val Str: Type = Type.Cst(TypeConstructor.Str)

  /**
    * Represents the Pure effect. (TRUE in the Boolean algebra.)
    */
  val Pure: Type = Type.Cst(TypeConstructor.Pure)

  /**
    * Represents the Impure effect. (FALSE in the Boolean algebra.)
    */
  val Impure: Type = Type.Cst(TypeConstructor.Impure)

  /////////////////////////////////////////////////////////////////////////////
  // Types                                                                   //
  /////////////////////////////////////////////////////////////////////////////

  /**
    * A type variable expression.
    */
  case class Var(id: Int, kind: Kind) extends Type with Ordered[Type.Var] {
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

    /**
      * Compares `this` type variable to `that` type variable.
      */
    override def compare(that: Type.Var): Int = this.id - that.id
  }

  /**
    * A type represented by the type constructor `tc`.
    */
  case class Cst(tc: TypeConstructor) extends Type {
    def kind: Kind = tc.kind
  }

  /**
    * A type expression that represents functions.
    */
  case class Arrow(arity: Int, eff: Type) extends Type {
    def kind: Kind = Kind.Arrow((0 until arity).map(_ => Kind.Star).toList, Kind.Star)
  }

  /**
    * A type constructor that represents the empty record type.
    */
  case object RecordEmpty extends Type {
    def kind: Kind = Kind.Record
  }

  /**
    * A type constructor that represents a record extension type.
    */
  case class RecordExtend(label: String, value: Type, rest: Type) extends Type {
    def kind: Kind = Kind.Star -> Kind.Record
  }

  /**
    * A type constructor that represents the empty schema type.
    */
  case object SchemaEmpty extends Type {
    def kind: Kind = Kind.Schema
  }

  /**
    * A type constructor that represents a schema extension type.
    */
  case class SchemaExtend(sym: Symbol.PredSym, tpe: Type, rest: Type) extends Type {
    def kind: Kind = Kind.Star -> Kind.Schema
  }

  /**
    * A type constructor that represents zero.
    */
  case object Zero extends Type {
    def kind: Kind = Kind.Star
  }

  /**
    * A type constructor that represents the successor of a type.
    */
  case class Succ(len: Int, t: Type) extends Type {
    def kind: Kind = Kind.Star
  }

  /**
    * A type expression that represents a type abstraction [x] => tpe.
    */
  case class Lambda(tvar: Type.Var, tpe: Type) extends Type {
    def kind: Kind = Kind.Star -> Kind.Star
  }

  /**
    * A type expression that a represents a type application tpe1[tpe2].
    */
  case class Apply(tpe1: Type, tpe2: Type) extends Type {
    /**
      * Returns the kind of `this` type.
      *
      * The kind of a type application can unique be determined
      * from the kind of the first type argument `t1`.
      */
    def kind: Kind = tpe1.kind match {
      case Kind.Arrow(_, k) => k
      case _ => throw InternalCompilerException("Illegal kind.")
    }
  }

  /////////////////////////////////////////////////////////////////////////////
  // Helper Functions                                                        //
  /////////////////////////////////////////////////////////////////////////////
  /**
    * Returns a fresh type variable.
    */
  def freshTypeVar(k: Kind = Kind.Star)(implicit flix: Flix): Type.Var = Type.Var(flix.genSym.freshId(), k)

  /**
    * Returns a fresh type variable of effect kind.
    */
  def freshEffectVar()(implicit flix: Flix): Type.Var = Type.Var(flix.genSym.freshId(), Kind.Effect)

  /**
    * Constructs an arrow with the given effect type A ->eff B.
    */
  def mkArrow(a: Type, f: Type, b: Type): Type = Apply(Apply(Arrow(2, f), a), b)

  /**
    * Constructs the arrow type A ->> B.
    */
  def mkPureArrow(a: Type, b: Type): Type = Apply(Apply(Arrow(2, Pure), a), b)

  /**
    * Constructs the arrow type A ~>> B.
    */
  def mkImpureArrow(a: Type, b: Type): Type = Apply(Apply(Arrow(2, Impure), a), b)

  /**
    * Constructs the arrow type A_1 ->> ... ->> A_n ->{eff} B.
    */
  def mkArrow(as: List[Type], eff: Type, b: Type): Type = {
    val a = as.last
    val base = mkArrow(a, eff, b)
    as.init.foldRight(base)(mkPureArrow)
  }

  /**
    * Constructs the arrow type [A] -> B.
    */
  // TODO: Split into two: one for pure and one for impure.
  def mkUncurriedArrow(as: List[Type], b: Type): Type = {
    // TODO: Folding in wrong order?
    val arrow = Arrow(as.length + 1, Pure)
    val inner = as.foldLeft(arrow: Type) {
      case (acc, x) => Apply(acc, x)
    }
    Apply(inner, b)
  }

  /**
    * Constructs the apply type base[t_1, ,..., t_n].
    */
  def mkApply(base: Type, ts: List[Type]): Type = ts.foldLeft(base) {
    case (acc, t) => Apply(acc, t)
  }

  /**
    * Constructs the tuple type (A, B, ...) where the types are drawn from the list `ts`.
    */
  def mkTuple(ts: List[Type]): Type = {
    val tuple = Type.Cst(TypeConstructor.Tuple(ts.length))
    ts.foldLeft(tuple: Type) {
      case (acc, x) => Apply(acc, x)
    }
  }

  /**
    * Returns a human readable representation of the given type `tpe0`.
    *
    * @param renameVars whether to use human readable variable names.
    */
  def fmtType(tpe0: Type, renameVars: Boolean): String = {
    def visit(tpe: Type, m: Map[Int, String]): String = {
      // Retrieve the type constructor and type arguments.
      val base = tpe.typeConstructor
      val args = tpe.typeArguments

      base match {
        case Type.Var(id, kind) =>
          // Lookup the human-friendly name in `m`.
          m.get(id) match {
            case None =>
              // No human-friendly name. Return the id. Use ' for types and '' for effects.
              if (kind != Kind.Effect) "'" + id.toString else "''" + id.toString
            case Some(s) => s
          }

        case Type.Cst(TypeConstructor.Array) =>
          s"Array[${args.map(visit(_, m)).mkString(", ")}]"

        case Type.Cst(TypeConstructor.Channel) =>
          s"Channel[${args.map(visit(_, m)).mkString(", ")}]"

        case Type.Cst(TypeConstructor.Enum(sym, _)) =>
          if (args.isEmpty)
            sym.toString
          else
            sym.toString + "[" + args.map(visit(_, m)).mkString(", ") + "]"

        case Type.Cst(TypeConstructor.Tuple(l)) =>
          "(" + args.map(visit(_, m)).mkString(", ") + ")"

        case Type.Cst(TypeConstructor.Pure) => "Pure"

        case Type.Cst(TypeConstructor.Impure) => "Impure"

        case Type.Cst(TypeConstructor.Not) => args match {
          case (t1: Type.Var) :: Nil => s"¬${visit(t1, m)}"
          case t1 :: Nil => s"¬(${visit(t1, m)})"
          case _ => throw InternalCompilerException(s"Unexpected type: '$tpe'.")
        }

        case Type.Cst(TypeConstructor.And) => args match {
          case (t1: Type.Var) :: (t2: Type.Var) :: Nil => s"${visit(t1, m)} ∧ ${visit(t2, m)}"
          case (t1: Type.Var) :: t2 :: Nil => s"${visit(t1, m)} ∧ (${visit(t2, m)})"
          case t1 :: (t2: Type.Var) :: Nil => s"(${visit(t1, m)}) ∧ ${visit(t2, m)}"
          case t1 :: t2 :: Nil => s"(${visit(t1, m)}) ∧ (${visit(t2, m)})"
          case _ => throw InternalCompilerException(s"Unexpected type: '$tpe'.")
        }

        case Type.Cst(TypeConstructor.Or) => args match {
          case (t1: Type.Var) :: (t2: Type.Var) :: Nil => s"${visit(t1, m)} ∨ ${visit(t2, m)}"
          case (t1: Type.Var) :: t2 :: Nil => s"${visit(t1, m)} ∨ (${visit(t2, m)})"
          case t1 :: (t2: Type.Var) :: Nil => s"(${visit(t1, m)}) ∨ ${visit(t2, m)}"
          case t1 :: t2 :: Nil => s"(${visit(t1, m)}) ∨ (${visit(t2, m)})"
          case _ => throw InternalCompilerException(s"Unexpected type: '$tpe'.")
        }

        case Type.Cst(tc) => tc.toString + (if (args.isEmpty) "" else "[" + args.map(visit(_, m)).mkString(", ") + "]")

        case Type.Zero => "Zero"

        case Type.Succ(n, t) => n.toString + " " + t.toString

        case Type.Arrow(l, eff) =>
          // Retrieve the arguments and result types.
          val argumentTypes = args.init
          val resultType = args.last

          // Format the arguments.
          val argPart = if (argumentTypes.length == 1) {
            visit(argumentTypes.head, m)
          } else {
            "(" + argumentTypes.map(visit(_, m)).mkString(", ") + ")"
          }
          // Format the arrow.
          val arrowPart = eff match {
            case Type.Cst(TypeConstructor.Impure) => " ~> "
            case _ => " -> "
          }
          // Format the effect.
          val effPart = eff match {
            case Type.Cst(TypeConstructor.Pure) => ""
            case Type.Cst(TypeConstructor.Impure) => " & Impure"
            case _ => " & (" + visit(eff, m) + ")"
          }
          // Format the result type.
          val resultPart = visit(resultType, m)

          // Put everything together.
          argPart + arrowPart + resultPart + effPart

        case Type.RecordEmpty => "{ }"

        case Type.RecordExtend(label, value, rest) => "{" + label + " = " + visit(value, m) + " | " + visit(rest, m) + "}"

        case Type.SchemaEmpty => "#{ }"

        case Type.SchemaExtend(sym, t, rest) => "#{" + sym + " = " + visit(t, m) + " | " + visit(rest, m) + "}"

        case Type.Lambda(tvar, tpe) => m.getOrElse(tvar.id, tvar.id.toString) + " => " + visit(tpe, m)

        case Type.Apply(tpe1, tpe2) => visit(tpe1, m) + "[" + visit(tpe2, m) + "]"
      }
    }

    /**
      * Computes a mapping from type variables to human readable variable names.
      *
      * E.g. the type variable 8192 might be mapped to 'a'and the type variable 8193 might be mapped to 'b'.
      */
    def alphaRenameVars(tpe0: Type): Map[Int, String] = {
      tpe0.typeVars.toList.sortBy(_.id).zipWithIndex.map {
        case (tvar, index) => tvar.id -> (index + 'a').toChar.toString
      }.toMap
    }

    // Determine whether to use human readable variables.
    if (renameVars)
      visit(tpe0, alphaRenameVars(tpe0))
    else
      visit(tpe0, Map.empty)
  }

  implicit object ShowInstance extends Show[Type] {
    def show(tpe: Type): String = fmtType(tpe, renameVars = true)
  }

}

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
    case Type.Cst(tc) => Set.empty
    case Type.Zero => Set.empty
    case Type.Succ(n, t) => Set.empty
    case Type.Arrow(_, _) => Set.empty
    case Type.RecordEmpty => Set.empty
    case Type.RecordExtend(label, value, rest) => value.typeVars ++ rest.typeVars
    case Type.SchemaEmpty => Set.empty
    case Type.SchemaExtend(sym, tpe, rest) => tpe.typeVars ++ rest.typeVars
    case Type.Lambda(tvar, tpe) => tpe.typeVars - tvar
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
    * Returns a human readable string representation of `this` type.
    */
  override def toString: String = this match {
    case tvar@Type.Var(x, k) => tvar.getText.getOrElse("'" + x)
    case Type.Cst(tc) => tc.toString
    case Type.Zero => "Zero"
    case Type.Succ(n, t) => s"Successor($n, $t)"
    case Type.Arrow(eff, l) => s"Arrow($eff, $l)"
    case Type.RecordEmpty => "{ }"
    case Type.RecordExtend(label, value, rest) => "{ " + label + " : " + value + " | " + rest + " }"
    case Type.SchemaEmpty => "Schema { }"
    case Type.SchemaExtend(sym, tpe, rest) => "Schema { " + sym + " : " + tpe + " | " + rest + " }"
    case Type.Lambda(tvar, tpe) => s"$tvar => $tpe"
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
    * A type represented by the type constructor `tc`.
    */
  case class Cst(tc: TypeConstructor) extends Type {
    def kind: Kind = tc.kind
  }


  /**
    * A type expression that represents functions.
    */
  case class Arrow(eff: Eff, length: Int) extends Type {
    def kind: Kind = Kind.Arrow((0 until length).map(_ => Kind.Star).toList, Kind.Star)
  }

  /**
    * A type constructor that represents the empty record type.
    */
  case object RecordEmpty extends Type {
    def kind: Kind = ??? // TODO
  }

  /**
    * A type constructor that represents a record extension type.
    */
  case class RecordExtend(label: String, value: Type, rest: Type) extends Type {
    def kind: Kind = ??? // TODO
  }

  /**
    * A type constructor that represents the empty schema type.
    */
  case object SchemaEmpty extends Type {
    def kind: Kind = ??? // TODO
  }

  /**
    * A type constructor that represents a schema extension type.
    */
  case class SchemaExtend(sym: Symbol.PredSym, tpe: Type, rest: Type) extends Type {
    def kind: Kind = ??? // TODO
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
    def kind: Kind = ??? // TODO
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
  def freshTypeVar(k: Kind = Kind.Star)(implicit flix: Flix): Type.Var = Type.Var(flix.genSym.freshId(), k)

  /**
    * Constructs the arrow type A -> B.
    */
  // TODO: Deprecated
  def mkArrow(a: Type, b: Type): Type = Apply(Apply(Arrow(Eff.Pure, 2), a), b) // TODO: Pure?

  /**
    * Constructs the arrow type A -> B with the effect `eff`.
    */
  def mkArrow(a: Type, eff: Eff, b: Type): Type = Apply(Apply(Arrow(eff, 2), a), b)

  /**
    * Constructs the arrow type A_1 -> .. -> A_n -> B.
    */
  // TODO: Deprecated
  def mkArrow(as: List[Type], b: Type): Type = { // TODO: Pure?
    as.foldRight(b)(mkArrow)
  }

  /**
    * Constructs the arrow type A_1 -> .. -> A_n -> B with the effect `eff` on each arrow.
    */
  def mkArrow(as: List[Type], eff: Eff, b: Type): Type = {
    as.foldRight(b)(mkArrow(_, eff, _))
  }

  /**
    * Constructs the arrow type [A] -> B.
    */
  def mkUncurriedArrow(as: List[Type], b: Type): Type = {
    val arrow = Arrow(Eff.Pure, as.length + 1) // TODO: Pure?
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

  // TODO: Move these helpers into the Typer.
  /**
    * Constructs the tuple type (A, B, ...) where the types are drawn from the list `ts`.
    */
  def mkTuple(ts: List[Type]): Type = {
    val tuple = Type.Cst(TypeConstructor.Tuple(ts.length))
    ts.foldLeft(tuple: Type) {
      case (acc, x) => Apply(acc, x)
    }
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
          case Type.Var(id, kind) => m.getOrElse(id, id.toString)

          //
          // Array
          //
          case Type.Cst(TypeConstructor.Array) =>
            "Array" + "[" + args.map(visit(_, m)).mkString(", ") + "]"

          //
          // Channel
          //
          case Type.Cst(TypeConstructor.Channel) =>
            "Channel" + "[" + args.map(visit(_, m)).mkString(", ") + "]"

          //
          // Enum.
          //
          case Type.Cst(TypeConstructor.Enum(sym, _)) =>
            if (args.isEmpty) sym.toString else sym.toString + "[" + args.map(visit(_, m)).mkString(", ") + "]"

          //
          // Tuple.
          //
          case Type.Cst(TypeConstructor.Tuple(l)) =>
            "(" + args.map(visit(_, m)).mkString(", ") + ")"

          //
          // Type Constructors.
          //
          case Type.Cst(tc) => tc.toString + (if (args.isEmpty) "" else "[" + args.map(visit(_, m)).mkString(", ") + "]")

          //
          // Primitive Types.
          //
          case Type.Zero => "Zero"
          case Type.Succ(n, t) => n.toString + " " + t.toString

          //
          // Arrow.
          //
          case Type.Arrow(_, l) =>
            val argumentTypes = args.init
            val resultType = args.last
            if (argumentTypes.length == 1) {
              visit(argumentTypes.head, m) + " -> " + visit(resultType, m)
            } else {
              "(" + argumentTypes.map(visit(_, m)).mkString(", ") + ") -> " + visit(resultType, m)
            }

          //
          // RecordEmpty.
          //
          case Type.RecordEmpty => "{ }"

          //
          // RecordExtension.
          //
          case Type.RecordExtend(label, value, rest) =>
            "{" + label + " = " + visit(value, m) + " | " + visit(rest, m) + "}"

          //
          // SchemaEmpty.
          //
          case Type.SchemaEmpty => "Schema { }"

          //
          // SchemaExtend.
          //
          case Type.SchemaExtend(sym, t, rest) =>
            "{" + sym + " = " + visit(t, m) + " | " + visit(rest, m) + "}"

          //
          // Abstraction.
          //
          case Type.Lambda(tvar, tpe) => m.getOrElse(tvar.id, tvar.id.toString) + " => " + visit(tpe, m)

          //
          // Application.
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

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
    case Type.Cst(tc) => Set.empty
    case Type.Native(clazz) => Set.empty
    case Type.Zero => Set.empty
    case Type.Succ(n, t) => Set.empty
    case Type.Arrow(l) => Set.empty
    case Type.RecordEmpty => Set.empty
    case Type.RecordExtend(label, value, rest) => value.typeVars ++ rest.typeVars
    case Type.SchemaEmpty => Set.empty
    case Type.SchemaExtend(sym, tpe, rest) => tpe.typeVars ++ rest.typeVars
    case Type.Relation(_, ts, _) => ts.flatMap(_.typeVars).toSet
    case Type.Lattice(_, ts, _) => ts.flatMap(_.typeVars).toSet
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
    * Returns `true` if `this` type is a type variable.
    */
  def isVar: Boolean = typeConstructor match {
    case Type.Var(_, _) => true
    case _ => false
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
  @deprecated("removed", "0.5")
  def isEnum: Boolean = typeConstructor match {
    case Type.Cst(TypeConstructor.Enum(sym, kind)) => true
    case _ => false
  }

  /**
    * Returns `true` if `this` type is a relation type.
    */
  def isRelation: Boolean = typeConstructor match {
    case Type.Relation(sym, _, _) => true
    case _ => false
  }

  /**
    * Returns `true` if `this` type is a lattice type.
    */
  def isLattice: Boolean = typeConstructor match {
    case Type.Lattice(sym, _, _) => true
    case _ => false
  }

  /**
    * Returns `true` if `this` type is a tuple type.
    */
  @deprecated("removed", "0.5")
  def isTuple: Boolean = typeConstructor match {
    case Type.Cst(TypeConstructor.Tuple(l)) => true
    case _ => false
  }

  /**
    * Returns `true` if `this` type is a record type.
    */
  def isRecord: Boolean = typeConstructor match {
    case Type.RecordEmpty => true
    case Type.RecordExtend(base, label, value) => true
    case _ => false
  }

  /**
    * Returns `true` if `this` type is a schema type.
    */
  def isSchema: Boolean = typeConstructor match {
    case Type.SchemaEmpty => true
    case Type.SchemaExtend(_, _, _) => true
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
    case Type.Cst(tc) => tc.toString
    case Type.Zero => "Zero"
    case Type.Succ(n, t) => s"Successor($n, $t)"
    case Type.Native(clazz) => "Native"
    case Type.Arrow(l) => s"Arrow($l)"
    case Type.Relation(sym, attr, _) => sym.toString + "(" + attr.mkString(", ") + ")"
    case Type.Lattice(sym, attr, _) => sym.toString + "(" + attr.mkString(", ") + ")"
    case Type.RecordEmpty => "{ }"
    case Type.RecordExtend(label, value, rest) => "{ " + label + " : " + value + " | " + rest + " }"
    case Type.SchemaEmpty => "Schema { }"
    case Type.SchemaExtend(sym, tpe, rest) => "Schema { " + sym + " : " + tpe + " | " + rest + " }"
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
    * A type constructor that represent native objects.
    */
  case class Native(clazz: Class[_]) extends Type {
    def kind: Kind = Kind.Star
  }

  /**
    * A type expression that represents functions.
    */
  case class Arrow(length: Int) extends Type {
    def kind: Kind = Kind.Arrow((0 until length).map(_ => Kind.Star).toList, Kind.Star)
  }

  /**
    * A type constructor that represents a relation with attributes of the given types.
    *
    * @param sym  the symbol of the relation.
    * @param attr the attribute types of the relation.
    * @param kind the kind of the relation.
    */
  case class Relation(sym: Symbol.RelSym, attr: List[Type], kind: Kind) extends Type

  /**
    * A type constructor that represents a lattice with attributes of the given types.
    *
    * @param sym  the symbol of the lattice.
    * @param attr the attribute types of the relation.
    * @param kind the kind of the lattice.
    */
  case class Lattice(sym: Symbol.LatSym, attr: List[Type], kind: Kind) extends Type

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
    * Constructs the arrow type A -> B.
    */
  def mkArrow(a: Type, b: Type): Type = Apply(Apply(Arrow(2), a), b)

  /**
    * Constructs the arrow type A_1 -> .. -> A_n -> B.
    */
  def mkArrow(as: List[Type], b: Type): Type = {
    as.foldRight(b)(mkArrow)
  }

  /**
    * Constructs the arrow type [A] -> B.
    */
  def mkUncurriedArrow(as: List[Type], b: Type): Type = {
    val arrow = Arrow(as.length + 1)
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
    * Returns the type corresponding to the given predicate symbol `sym` with the given attribute types `ts`.
    */
  def mkRelationOrLattice(predSym: Symbol.PredSym, ts: List[Type]): Type = predSym match {
    case sym: Symbol.RelSym => Type.Relation(sym, ts, Kind.Star)
    case sym: Symbol.LatSym => Type.Lattice(sym, ts, Kind.Star)
  }

  /**
    * Constructs the tuple type (A, B, ...) where the types are drawn from the list `ts`.
    */
  def mkTuple(ts: Type*): Type = mkTuple(ts.toList)

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
    * Constructs the array type [elmType] where 'elmType' is the given type.
    */
  def mkArray(elmType: Type): Type = Apply(Type.Cst(TypeConstructor.Array), elmType)

  /**
    * Constructs the channel type [elmType] where 'elmType' is the given type.
    */
  def mkChannel(elmType: Type): Type = Apply(Type.Cst(TypeConstructor.Channel), elmType)

  /**
    * Constructs the vector type [|elmType, Len|] where
    *
    * @param elmType is the given element type
    * @param len     is the given length of the vector.
    *
    *                len expected input is an instance of Succ(Int, Type), where Int is the length, and Type is either Type.Zero or a fresh variable.
    */
  def mkVector(elmType: Type, len: Type): Type = Apply(Apply(Cst(TypeConstructor.Vector), elmType), len)

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
      case Type.Cst(tc) => Type.Cst(tc)
      case Type.Native(clazz) => Type.Native(clazz)
      case Type.Arrow(l) => Type.Arrow(l)
      case Type.RecordEmpty => Type.RecordEmpty
      case Type.RecordExtend(label, value, rest) => Type.RecordExtend(label, visit(value), visit(rest))
      case Type.SchemaEmpty => Type.SchemaEmpty
      case Type.SchemaExtend(sym, t, rest) => Type.SchemaExtend(sym, visit(t), visit(rest))
      case Type.Zero => Type.Zero
      case Type.Succ(n, t) => Type.Succ(n, t)
      case Type.Apply(tpe1, tpe2) => Type.Apply(visit(tpe1), visit(tpe2))
      case Type.Relation(sym, attr, kind) => Type.Relation(sym, attr map visit, kind)
      case Type.Lattice(sym, attr, kind) => Type.Lattice(sym, attr map visit, kind)
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
          case Type.Cst(tc) => tc.toString

          //
          // Primitive Types.
          //
          case Type.Zero => "Zero"
          case Type.Succ(n, t) => n.toString + " " + t.toString
          case Type.Native(clazz) => "#" + clazz.getName

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
          // Relation.
          //
          case Type.Relation(sym, attr, _) =>
            sym.toString + "(" + attr.map(visit(_, m)).mkString(", ") + ")"

          //
          // Lattice.
          //
          case Type.Lattice(sym, attr, _) =>
            sym.toString + "(" + attr.map(visit(_, m)).mkString(", ") + ")"

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
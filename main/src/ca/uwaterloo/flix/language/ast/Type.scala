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
import ca.uwaterloo.flix.language.debug.{Audience, FormatType}
import ca.uwaterloo.flix.util.InternalCompilerException

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
    *
    * Returns a sorted set to ensure that the compiler is deterministic.
    */
  def typeVars: SortedSet[Type.Var] = this match {
    case x: Type.Var => SortedSet(x)
    case Type.Cst(tc) => SortedSet.empty
    case Type.Lambda(tvar, tpe) => tpe.typeVars - tvar
    case Type.Apply(tpe1, tpe2) => tpe1.typeVars ++ tpe2.typeVars
  }

  /**
    * Optionally returns the type constructor of `this` type.
    *
    * Return `None` if the type constructor is a variable.
    *
    * Otherwise returns `Some(tc)` where `tc` is the left-most type constructor.
    *
    * For example,
    *
    * {{{
    * x                             =>      None
    * Celsius                       =>      Some(Celsius)
    * Option[Int]                   =>      Some(Option)
    * Arrow[Bool, Char]             =>      Some(Arrow)
    * Tuple[Bool, Int]              =>      Some(Tuple)
    * Result[Bool, Int]             =>      Some(Result)
    * Result[Bool][Int]             =>      Some(Result)
    * Option[Result[Bool, Int]]     =>      Some(Option)
    * }}}
    */
  def typeConstructor: Option[TypeConstructor] = this match {
    case Type.Var(_, _, _) => None
    case Type.Cst(tc) => Some(tc)
    case Type.Apply(t1, _) => t1.typeConstructor
    case Type.Lambda(_, _) => throw InternalCompilerException(s"Unexpected type constructor: Lambda.")
  }

  /**
    * Returns a list of all type constructors in `this` type.
    */
  def typeConstructors: List[TypeConstructor] = this match {
    case Type.Var(_, _, _) => Nil
    case Type.Cst(tc) => tc :: Nil
    case Type.Apply(t1, t2) => t1.typeConstructors ::: t2.typeConstructors
    case Type.Lambda(_, _) => throw InternalCompilerException(s"Unexpected type constructor: Lambda.")
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
    * Applies `f` to every type variable in `this` type.
    */
  def map(f: Type.Var => Type): Type = this match {
    case tvar: Type.Var => f(tvar)
    case Type.Cst(_) => this
    case Type.Lambda(tvar, tpe) => Type.Lambda(tvar, tpe.map(f))
    case Type.Apply(tpe1, tpe2) => Type.Apply(tpe1.map(f), tpe2.map(f))
  }

  /**
    * Returns the argument types of `this` arrow type.
    *
    * NB: Assumes that `this` type is an arrow.
    */
  def arrowArgTypes: List[Type] = typeConstructor match {
    case Some(TypeConstructor.Arrow(n)) => typeArguments.drop(1).dropRight(1)
    case _ => throw InternalCompilerException(s"Unexpected non-arrow type: '$this'.")
  }

  /**
    * Returns the result type of `this` arrow type.
    *
    * NB: Assumes that `this` type is an arrow.
    */
  def arrowResultType: Type = typeConstructor match {
    case Some(TypeConstructor.Arrow(n)) => typeArguments.last
    case _ => throw InternalCompilerException(s"Unexpected non-arrow type: '$this'.")
  }

  /**
    * Returns the effect type of `this` arrow type.
    *
    * NB: Assumes that `this` type is an arrow.
    */
  def arrowEffectType: Type = typeConstructor match {
    case Some(TypeConstructor.Arrow(n)) => typeArguments.head
    case _ => throw InternalCompilerException(s"Unexpected non-arrow type: '$this'.")
  }

  /**
    * Returns the size of `this` type.
    */
  def size: Int = this match {
    case Type.Var(_, _, _) => 1
    case Type.Cst(tc) => 1
    case Type.Lambda(_, tpe) => tpe.size + 1
    case Type.Apply(tpe1, tpe2) => tpe1.size + tpe2.size + 1
  }

  /**
    * Returns a human readable string representation of `this` type.
    */
  override def toString: String = FormatType.formatType(this)(Audience.Internal)

}

object Type {

  /////////////////////////////////////////////////////////////////////////////
  // Constants                                                               //
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
    * Represents the Array type constructor.
    *
    * NB: This type has kind: * -> *.
    */
  val Array: Type = Type.Cst(TypeConstructor.Array)

  /**
    * Represents the Channel type constructor.
    *
    * NB: This type has kind: * -> *.
    */
  val Channel: Type = Type.Cst(TypeConstructor.Channel)

  /**
    * Represents the Reference type constructor.
    *
    * NB: This type has kind: * -> *.
    */
  val Ref: Type = Type.Cst(TypeConstructor.Ref)

  /**
    * Represents the Relation type constructor.
    */
  val Relation: Type = Type.Cst(TypeConstructor.Relation)

  /**
    * Represents the Lattice type constructor.
    */
  val Lattice: Type = Type.Cst(TypeConstructor.Lattice)

  /**
    * Represents the type of an empty record.
    */
  val RecordEmpty: Type = Type.Cst(TypeConstructor.RecordEmpty)

  /**
    * Represents the type of an empty schema.
    */
  val SchemaEmpty: Type = Type.Cst(TypeConstructor.SchemaEmpty)

  /**
    * Represents the Boolean True.
    */
  val True: Type = Type.Cst(TypeConstructor.True)

  /**
    * Represents the Boolean False.
    */
  val False: Type = Type.Cst(TypeConstructor.False)

  /**
    * Represents the Pure effect. (TRUE in the Boolean algebra.)
    */
  val Pure: Type = True

  /**
    * Represents the Impure effect. (FALSE in the Boolean algebra.)
    */
  val Impure: Type = False

  /**
    * Represents the Not type constructor.
    *
    * NB: This type has kind: * -> *.
    */
  val Not: Type = Type.Cst(TypeConstructor.Not)

  /**
    * Represents the And type constructor.
    *
    * NB: This type has kind: * -> (* -> *).
    */
  val And: Type = Type.Cst(TypeConstructor.And)

  /**
    * Represents the Or type constructor.
    *
    * NB: This type has kind: * -> (* -> *).
    */
  val Or: Type = Type.Cst(TypeConstructor.Or)

  /////////////////////////////////////////////////////////////////////////////
  // Constructors                                                            //
  /////////////////////////////////////////////////////////////////////////////

  /**
    * A type variable expression.
    */
  case class Var(id: Int, kind: Kind, rigidity: Rigidity = Rigidity.Flexible) extends Type with Ordered[Type.Var] {
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
    * A type expression that represents a type abstraction [x] => tpe.
    */
  case class Lambda(tvar: Type.Var, tpe: Type) extends Type {
    def kind: Kind = Kind.Star ->: tpe.kind
  }

  /**
    * A type expression that a represents a type application tpe1[tpe2].
    */
  case class Apply(tpe1: Type, tpe2: Type) extends Type {
    /**
      * Returns the kind of `this` type.
      *
      * The kind of a type application can unique be determined from the kind of the first type argument `t1`.
      */
    val kind: Kind = {
      tpe1.kind match {
        case Kind.Arrow(k1, k2) =>
          // TODO: Kind check (but only for boolean formulas for now).
          //          if (k1 == Kind.Bool) {
          //            val k3 = tpe2.kind
          //            if (k3 != Kind.Bool && !k3.isInstanceOf[Kind.Var]) {
          //               throw InternalCompilerException(s"Unexpected non-bool kind: '$k3'.")
          //            }
          //          }
          k2
        case _ => throw InternalCompilerException(s"Illegal kind: '${tpe1.kind}' of type '$tpe1'.")
      }
    }
  }

  /////////////////////////////////////////////////////////////////////////////
  // Utility Functions                                                       //
  /////////////////////////////////////////////////////////////////////////////

  /**
    * Returns a fresh type variable of the given kind `k` and rigidity `r`.
    */
  def freshVar(k: Kind, r: Rigidity = Rigidity.Flexible)(implicit flix: Flix): Type.Var = {
    val id = flix.genSym.freshId()
    Type.Var(id, k, r)
  }

  /**
    * Constructs the pure arrow type A -> B.
    */
  def mkPureArrow(a: Type, b: Type): Type = mkArrowWithEffect(a, Pure, b)

  /**
    * Constructs the impure arrow type A ~> B.
    */
  def mkImpureArrow(a: Type, b: Type): Type = mkArrowWithEffect(a, Impure, b)

  /**
    * Constructs the arrow type A -> B & e.
    */
  def mkArrowWithEffect(a: Type, e: Type, b: Type): Type = mkApply(Type.Cst(TypeConstructor.Arrow(2)), List(e, a, b))

  /**
    * Constructs the pure curried arrow type A_1 -> (A_2  -> ... -> A_n) -> B.
    */
  def mkPureCurriedArrow(as: List[Type], b: Type): Type = mkCurriedArrowWithEffect(as, Pure, b)

  /**
    * Constructs the impure curried arrow type A_1 -> (A_2  -> ... -> A_n) ~> B.
    */
  def mkImpureCurriedArrow(as: List[Type], b: Type): Type = mkCurriedArrowWithEffect(as, Impure, b)

  /**
    * Constructs the curried arrow type A_1 -> (A_2  -> ... -> A_n) -> B & e.
    */
  def mkCurriedArrowWithEffect(as: List[Type], e: Type, b: Type): Type = {
    val a = as.last
    val base = mkArrowWithEffect(a, e, b)
    as.init.foldRight(base)(mkPureArrow)
  }

  /**
    * Constructs the pure uncurried arrow type (A_1, ..., A_n) -> B.
    */
  def mkPureUncurriedArrow(as: List[Type], b: Type): Type = mkUncurriedArrowWithEffect(as, Pure, b)

  /**
    * Constructs the impure uncurried arrow type (A_1, ..., A_n) ~> B.
    */
  def mkImpureUncurriedArrow(as: List[Type], b: Type): Type = mkUncurriedArrowWithEffect(as, Impure, b)

  /**
    * Constructs the uncurried arrow type (A_1, ..., A_n) -> B & e.
    */
  def mkUncurriedArrowWithEffect(as: List[Type], e: Type, b: Type): Type = {
    val arrow = Type.Apply(Type.Cst(TypeConstructor.Arrow(as.length + 1)),  e)
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
    * Returns the type `Array[tpe]`.
    */
  def mkArray(elmType: Type): Type = Apply(Array, elmType)

  /**
    * Returns the type `Channel[tpe]`.
    */
  def mkChannel(tpe: Type): Type = Type.Apply(Channel, tpe)

  /**
    * Returns the type `Ref[tpe]`.
    */
  def mkRef(tpe: Type): Type = Type.Apply(Ref, tpe)

  /**
    * Construct the enum type constructor for the given symbol `sym` with the given kind `k`.
    */
  def mkEnum(sym: Symbol.EnumSym, k: Kind): Type = Type.Cst(TypeConstructor.Enum(sym, k))

  /**
    * Construct the enum type `Sym[ts]`.
    */
  def mkEnum(sym: Symbol.EnumSym, ts: List[Type]): Type = mkApply(Type.Cst(TypeConstructor.Enum(sym, Kind.mkArrow(ts.length))), ts)

  /**
    * Constructs a tag type for the given `sym`, `tag`, `caseType` and `resultType`.
    *
    * A tag type can be understood as a "function type" from the `caseType` to the `resultType`.
    *
    * For example, for:
    *
    * {{{
    * enum List[a] {
    *   case Nil,
    *   case Cons(a, List[a])
    * }
    *
    * We have:
    *
    *   Nil:  Unit -> List[a]           (caseType = Unit, resultType = List[a])
    *   Cons: (a, List[a]) -> List[a]   (caseType = (a, List[a]), resultType = List[a])
    * }}}
    */
  def mkTag(sym: Symbol.EnumSym, tag: String, caseType: Type, resultType: Type): Type = {
    Type.Apply(Type.Apply(Type.Cst(TypeConstructor.Tag(sym, tag)), caseType), resultType)
  }

  /**
    * Constructs the tuple type (A, B, ...) where the types are drawn from the list `ts`.
    */
  def mkTuple(ts: List[Type]): Type = {
    val init = Type.Cst(TypeConstructor.Tuple(ts.length))
    ts.foldLeft(init: Type) {
      case (acc, x) => Apply(acc, x)
    }
  }

  /**
    * Constructs the a native type.
    */
  def mkNative(clazz: Class[_]): Type = Type.Cst(TypeConstructor.Native(clazz))

  /**
    * Constructs a RecordExtend type.
    */
  def mkRecordExtend(label: String, tpe: Type, rest: Type): Type = {
    mkApply(Type.Cst(TypeConstructor.RecordExtend(label)), List(tpe, rest))
  }

  /**
    * Constructs a SchemaExtend type.
    */
  def mkSchemaExtend(name: String, tpe: Type, rest: Type): Type = {
    mkApply(Type.Cst(TypeConstructor.SchemaExtend(name)), List(tpe, rest))
  }

  /**
    * Returns the type `tpe0 ? nullity`.
    */
  def mkNullable(tpe0: Type, nullity: Type): Type = Apply(Apply(Cst(TypeConstructor.Nullable), tpe0), nullity)

  /**
    * Construct a relation type with the given list of type arguments `ts0`.
    */
  def mkRelation(ts0: List[Type]): Type = {
    val ts = ts0 match {
      case Nil => Type.Unit
      case x :: Nil => x
      case xs => mkTuple(xs)
    }

    Apply(Relation, ts)
  }

  /**
    * Construct a lattice type with the given list of type arguments `ts0`.
    */
  def mkLattice(ts0: List[Type]): Type = {
    val ts = ts0 match {
      case Nil => Type.Unit
      case x :: Nil => x
      case xs => mkTuple(xs)
    }

    Apply(Lattice, ts)
  }

  /**
    * Returns the type `Not(tpe0)`.
    */
  def mkNot(tpe0: Type): Type = tpe0 match {
    case Type.True => Type.False
    case Type.False => Type.True
    case _ => Type.Apply(Type.Not, tpe0)
  }

  /**
    * Returns the type `And(tpe1, tpe2)`.
    */
  def mkAnd(tpe1: Type, tpe2: Type): Type = (tpe1, tpe2) match {
    case (Type.Cst(TypeConstructor.True), _) => tpe2
    case (_, Type.Cst(TypeConstructor.True)) => tpe1
    case (Type.Cst(TypeConstructor.False), _) => Type.False
    case (_, Type.Cst(TypeConstructor.False)) => Type.False
    case _ => Type.Apply(Type.Apply(Type.And, tpe1), tpe2)
  }

  /**
    * Returns the type `And(tpe1, And(tpe2, tpe3))`.
    */
  def mkAnd(tpe1: Type, tpe2: Type, tpe3: Type): Type = mkAnd(tpe1, mkAnd(tpe2, tpe3))

  /**
    * Returns the type `And(tpe1, And(tpe2, ...))`.
    */
  def mkAnd(tpes: List[Type]): Type = tpes match {
    case Nil => Type.True
    case x :: xs => mkAnd(x, mkAnd(xs))
  }

  /**
    * Returns the type `Or(tpe1, tpe2)`.
    */
  def mkOr(tpe1: Type, tpe2: Type): Type = (tpe1, tpe2) match {
    case (Type.Cst(TypeConstructor.True), _) => Type.True
    case (_, Type.Cst(TypeConstructor.True)) => Type.True
    case (Type.Cst(TypeConstructor.False), _) => tpe2
    case (_, Type.Cst(TypeConstructor.False)) => tpe1
    case _ => Type.Apply(Type.Apply(Type.Or, tpe1), tpe2)
  }

  /**
    * Returns a Boolean type that represents the equivalence of `x` and `y`.
    *
    * That is, `x == y` iff `(x /\ y) \/ (not x /\ not y)`
    */
  def mkEquiv(x: Type, y: Type): Type = Type.mkOr(Type.mkAnd(x, y), Type.mkAnd(Type.mkNot(x), Type.mkNot(y)))

  /**
    * Returns a simplified (evaluated) form of the given type `tpe0`.
    *
    * Performs beta-reduction of type abstractions and applications.
    */
  def simplify(tpe0: Type): Type = {
    def eval(t: Type, subst: Map[Type.Var, Type]): Type = t match {
      case tvar: Type.Var => subst.getOrElse(tvar, tvar)

      case Type.Cst(_) => t

      case Type.Apply(Type.Apply(Type.Cst(TypeConstructor.RecordExtend(label)), tpe), rest) =>
        val t1 = eval(tpe, subst)
        val t2 = eval(rest, subst)
        Type.mkRecordExtend(label, t1, t2)

      case Type.Apply(Type.Apply(Type.Cst(TypeConstructor.SchemaExtend(sym)), tpe), rest) =>
        val t1 = eval(tpe, subst)
        val t2 = eval(rest, subst)
        Type.mkSchemaExtend(sym, t1, t2)

      case Type.Lambda(tvar, tpe) => Type.Lambda(tvar, eval(tpe, subst))

      // TODO: Does not take variable capture into account.
      case Type.Apply(tpe1, tpe2) => (eval(tpe1, subst), eval(tpe2, subst)) match {
        case (Type.Lambda(tvar, tpe3), t2) => eval(tpe3, subst + (tvar -> t2))
        case (t1, t2) => Type.Apply(t1, t2)
      }
    }

    eval(tpe0, Map.empty)
  }

}

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
import ca.uwaterloo.flix.language.ast.Ast.{EliminatedBy, IntroducedBy}
import ca.uwaterloo.flix.language.debug.{Audience, FormatType}
import ca.uwaterloo.flix.language.phase.Kinder
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
    case Type.Var(_, _, _, _) => None
    case Type.UnkindedVar(_, _, _) => None
    case Type.Cst(tc, _) => Some(tc)
    case Type.Apply(t1, _) => t1.typeConstructor
    case Type.Lambda(_, _) => throw InternalCompilerException(s"Unexpected type constructor: Lambda.")
  }

  /**
    * Returns a list of all type constructors in `this` type.
    */
  def typeConstructors: List[TypeConstructor] = this match {
    case Type.Var(_, _, _, _) => Nil
    case Type.UnkindedVar(_, _, _) => Nil
    case Type.Cst(tc, _) => tc :: Nil
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
    case Type.Var(_, _, _, _) => 1
    case Type.UnkindedVar(_, _, _) => 1
    case Type.Cst(tc, _) => 1
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

  // TODO: Reduce usage of these in favor of the ones with source locations.

  /**
    * Represents the Unit type.
    */
  val Unit: Type = Type.Cst(TypeConstructor.Unit, SourceLocation.Unknown)

  /**
    * Represents the Null type.
    */
  val Null: Type = Type.Cst(TypeConstructor.Null, SourceLocation.Unknown)

  /**
    * Represents the Bool type.
    */
  val Bool: Type = Type.Cst(TypeConstructor.Bool, SourceLocation.Unknown)

  /**
    * Represents the Char type.
    */
  val Char: Type = Type.Cst(TypeConstructor.Char, SourceLocation.Unknown)

  /**
    * Represents the Float32 type.
    */
  val Float32: Type = Type.Cst(TypeConstructor.Float32, SourceLocation.Unknown)

  /**
    * Represents the Float64 type.
    */
  val Float64: Type = Type.Cst(TypeConstructor.Float64, SourceLocation.Unknown)

  /**
    * Represents the Int8 type.
    */
  val Int8: Type = Type.Cst(TypeConstructor.Int8, SourceLocation.Unknown)

  /**
    * Represents the Int16 type.
    */
  val Int16: Type = Type.Cst(TypeConstructor.Int16, SourceLocation.Unknown)

  /**
    * Represents the Int32 type.
    */
  val Int32: Type = Type.Cst(TypeConstructor.Int32, SourceLocation.Unknown)

  /**
    * Represents the Int64 type.
    */
  val Int64: Type = Type.Cst(TypeConstructor.Int64, SourceLocation.Unknown)

  /**
    * Represents the BigInt type.
    */
  val BigInt: Type = Type.Cst(TypeConstructor.BigInt, SourceLocation.Unknown)

  /**
    * Represents the String type.
    */
  val Str: Type = Type.Cst(TypeConstructor.Str, SourceLocation.Unknown)

  /**
    * Represents the Array type constructor.
    *
    * NB: This type has kind: * -> *.
    */
  val Array: Type = Type.Cst(TypeConstructor.Array, SourceLocation.Unknown)

  /**
    * Represents the Channel type constructor.
    *
    * NB: This type has kind: * -> *.
    */
  val Channel: Type = Type.Cst(TypeConstructor.Channel, SourceLocation.Unknown)

  /**
    * Represents the Lazy type constructor.
    *
    * NB: This type has kind: * -> *.
    */
  val Lazy: Type = Type.Cst(TypeConstructor.Lazy, SourceLocation.Unknown)

  /**
    * Represents the Relation type constructor.
    */
  val Relation: Type = Type.Cst(TypeConstructor.Relation, SourceLocation.Unknown)

  /**
    * Represents the Lattice type constructor.
    */
  val Lattice: Type = Type.Cst(TypeConstructor.Lattice, SourceLocation.Unknown)

  /**
    * Represents the type of an empty record.
    */
  val RecordEmpty: Type = Type.Cst(TypeConstructor.RecordEmpty, SourceLocation.Unknown)

  /**
    * Represents the type of an empty schema.
    */
  val SchemaEmpty: Type = Type.Cst(TypeConstructor.SchemaEmpty, SourceLocation.Unknown)

  /**
    * Represents the Boolean True.
    */
  val True: Type = Type.Cst(TypeConstructor.True, SourceLocation.Unknown)

  /**
    * Represents the Boolean False.
    */
  val False: Type = Type.Cst(TypeConstructor.False, SourceLocation.Unknown)

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
  val Not: Type = Type.Cst(TypeConstructor.Not, SourceLocation.Unknown)

  /**
    * Represents the And type constructor.
    *
    * NB: This type has kind: * -> (* -> *).
    */
  val And: Type = Type.Cst(TypeConstructor.And, SourceLocation.Unknown)

  /**
    * Represents the Or type constructor.
    *
    * NB: This type has kind: * -> (* -> *).
    */
  val Or: Type = Type.Cst(TypeConstructor.Or, SourceLocation.Unknown)

  /////////////////////////////////////////////////////////////////////////////
  // Constructors                                                            //
  /////////////////////////////////////////////////////////////////////////////

  sealed trait MaybeKindedVar extends Type {
    def id: Int
    def text: Option[String]
  }

  /**
    * A type variable.
    */
  @IntroducedBy(Kinder.getClass)
  case class Var(id: Int, kind: Kind, rigidity: Rigidity = Rigidity.Flexible, text: Option[String] = None) extends Type with MaybeKindedVar with Ordered[Type.Var] {
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
    * A type variable without a kind.
    */
  @EliminatedBy(Kinder.getClass)
  case class UnkindedVar(id: Int, rigidity: Rigidity = Rigidity.Flexible, text: Option[String] = None) extends Type with MaybeKindedVar with Ordered[Type.UnkindedVar] {

    @deprecated("An UnkindedVar has no associated kind.")
    override def kind: Kind = throw InternalCompilerException("Attempt to access kind of unkinded type variable")

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
    override def compare(that: Type.UnkindedVar): Int = this.id - that.id
  }

  /**
    * Represents a type ascribed with a kind.
    */
  @EliminatedBy(Kinder.getClass)
  case class Ascribe(tpe: Type, kind: Kind) extends Type

  /**
    * A type represented by the type constructor `tc`.
    */
  case class Cst(tc: TypeConstructor, loc: SourceLocation) extends Type {
    def kind: Kind = tc.kind

    override def hashCode(): Int = tc.hashCode()

    override def equals(o: Any): Boolean = o match {
      case that: Cst => this.tc == that.tc
      case _ => false
    }
  }

  /**
    * A type expression that represents a type abstraction [x] => tpe.
    */
  case class Lambda(tvar: Type.MaybeKindedVar, tpe: Type) extends Type {
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
  def freshVar(k: Kind, r: Rigidity = Rigidity.Flexible, text: Option[String] = None)(implicit flix: Flix): Type.Var = {
    val id = flix.genSym.freshId()
    Type.Var(id, k, r, text)
  }

  /**
    * Returns a fresh unkinded type variable of the given kind `k` and rigidity `r`.
    */
  def freshUnkindedVar(r: Rigidity = Rigidity.Flexible, text: Option[String] = None)(implicit flix: Flix): Type.UnkindedVar = {
    val id = flix.genSym.freshId()
    Type.UnkindedVar(id, r, text)
  }

  /**
    * Returns the Unit type with given source location `loc`.
    */
  def mkUnit(loc: SourceLocation): Type = Type.Cst(TypeConstructor.Unit, loc)

  /**
    * Returns the Null type with the given source location `loc`.
    */
  def mkNull(loc: SourceLocation): Type = Type.Cst(TypeConstructor.Null, loc)

  /**
    * Returns the Bool type with the given source location `loc`.
    */
  def mkBool(loc: SourceLocation): Type = Type.Cst(TypeConstructor.Bool, loc)

  /**
    * Returns the Char type with the given source location `loc`.
    */
  def mkChar(loc: SourceLocation): Type = Type.Cst(TypeConstructor.Char, loc)

  /**
    * Returns the Float32 type with the given source location `loc`.
    */
  def mkFloat32(loc: SourceLocation): Type = Type.Cst(TypeConstructor.Float32, loc)

  /**
    * Returns the Float64 type with the given source location `loc`.
    */
  def mkFloat64(loc: SourceLocation): Type = Type.Cst(TypeConstructor.Float64, loc)

  /**
    * Returns the Int8 type with the given source location `loc`.
    */
  def mkInt8(loc: SourceLocation): Type = Type.Cst(TypeConstructor.Int8, loc)

  /**
    * Returns the Int16 type with the given source location `loc`.
    */
  def mkInt16(loc: SourceLocation): Type = Type.Cst(TypeConstructor.Int16, loc)

  /**
    * Returns the Int32 type with the given source location `loc`.
    */
  def mkInt32(loc: SourceLocation): Type = Type.Cst(TypeConstructor.Int32, loc)

  /**
    * Returns the Int64 type with the given source location `loc`.
    */
  def mkInt64(loc: SourceLocation): Type = Type.Cst(TypeConstructor.Int64, loc)

  /**
    * Returns the BigInt type with the given source location `loc`.
    */
  def mkBigInt(loc: SourceLocation): Type = Type.Cst(TypeConstructor.BigInt, loc)

  /**
    * Returns the String type with the given source location `loc`.
    */
  def mkString(loc: SourceLocation): Type = Type.Cst(TypeConstructor.Str, loc)

  /**
    * Returns the Array type with the given source location `loc`.
    */
  def mkArray(loc: SourceLocation): Type = Type.Cst(TypeConstructor.Array, loc)

  /**
    * Returns the type `Array[tpe]` with the given optional source location `loc`.
    */
  def mkArray(elmType: Type, loc: SourceLocation = SourceLocation.Unknown): Type = Apply(Type.Cst(TypeConstructor.Array, loc), elmType)

  /**
    * Returns the Channel type with the given source location `loc`.
    */
  def mkChannel(loc: SourceLocation): Type = Type.Cst(TypeConstructor.Channel, loc)

  /**
    * Returns the type `Channel[tpe]` with the given optional source location `loc`.
    */
  def mkChannel(tpe: Type, loc: SourceLocation = SourceLocation.Unknown): Type = Type.Apply(Type.Cst(TypeConstructor.Channel, loc), tpe)

  /**
    * Returns the Lazy type with the given source location `loc`.
    */
  def mkLazy(loc: SourceLocation): Type = Type.Cst(TypeConstructor.Lazy, loc)

  /**
    * Returns the type `Lazy[tpe]` with the given optional source location `loc`.
    */
  def mkLazy(tpe: Type, loc: SourceLocation = SourceLocation.Unknown): Type = Type.Apply(Type.Cst(TypeConstructor.Lazy, loc), tpe)

  /**
    * Returns the type `ScopedRef[tpe, lifetime]` with the given optional source location `loc`.
    */
  def mkScopedRef(tpe1: Type, tpe2: Type, loc: SourceLocation = SourceLocation.Unknown): Type =
    Type.Apply(Type.Apply(Type.Cst(TypeConstructor.ScopedRef, loc), tpe1), tpe2)

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
  def mkArrowWithEffect(a: Type, e: Type, b: Type): Type = mkApply(Type.Cst(TypeConstructor.Arrow(2), SourceLocation.Unknown), List(e, a, b))

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
    val arrow = Type.Apply(Type.Cst(TypeConstructor.Arrow(as.length + 1), SourceLocation.Unknown), e)
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
    * Returns the type `Choice[tpe, isAbsent, isPresent]`.
    */
  def mkChoice(tpe0: Type, isAbsent: Type, isPresent: Type): Type = {
    val sym = Symbol.mkEnumSym("Choice")
    val kind = Kind.Star ->: Kind.Bool ->: Kind.Bool ->: Kind.Star
    val tc = TypeConstructor.Enum(sym, kind)
    Apply(Apply(Apply(Cst(tc, SourceLocation.Unknown), tpe0), isAbsent), isPresent)
  }

  /**
    * Construct the enum type constructor for the given symbol `sym` with the given kind `k`.
    */
  def mkEnum(sym: Symbol.EnumSym, k: Kind, loc: SourceLocation): Type = Type.Cst(TypeConstructor.Enum(sym, k), loc)

  /**
    * Construct the enum type constructor for the given symbol `sym` with the given kind `k`.
    */
  def mkUnkindedEnum(sym: Symbol.EnumSym, loc: SourceLocation): Type = Type.Cst(TypeConstructor.UnkindedEnum(sym), loc)

  /**
    * Construct the enum type `Sym[ts]`.
    */
  def mkEnum(sym: Symbol.EnumSym, ts: List[Type]): Type = mkApply(Type.Cst(TypeConstructor.Enum(sym, Kind.mkArrow(ts.length)), SourceLocation.Unknown), ts)

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
  def mkTag(sym: Symbol.EnumSym, tag: Name.Tag, caseType: Type, resultType: Type): Type = {
    Type.Apply(Type.Apply(Type.Cst(TypeConstructor.Tag(sym, tag), SourceLocation.Unknown), caseType), resultType)
  }

  /**
    * Constructs the tuple type (A, B, ...) where the types are drawn from the list `ts`.
    */
  def mkTuple(ts: List[Type]): Type = {
    val init = Type.Cst(TypeConstructor.Tuple(ts.length), SourceLocation.Unknown)
    ts.foldLeft(init: Type) {
      case (acc, x) => Apply(acc, x)
    }
  }

  /**
    * Constructs the a native type.
    */
  def mkNative(clazz: Class[_]): Type = Type.Cst(TypeConstructor.Native(clazz), SourceLocation.Unknown)

  /**
    * Constructs a RecordExtend type.
    */
  def mkRecordExtend(field: Name.Field, tpe: Type, rest: Type): Type = {
    mkApply(Type.Cst(TypeConstructor.RecordExtend(field), SourceLocation.Unknown), List(tpe, rest))
  }

  /**
    * Constructs a SchemaExtend type.
    */
  def mkSchemaExtend(pred: Name.Pred, tpe: Type, rest: Type): Type = {
    mkApply(Type.Cst(TypeConstructor.SchemaExtend(pred), SourceLocation.Unknown), List(tpe, rest))
  }

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
    case (Type.Cst(TypeConstructor.True, _), _) => tpe2
    case (_, Type.Cst(TypeConstructor.True, _)) => tpe1
    case (Type.Cst(TypeConstructor.False, _), _) => Type.False
    case (_, Type.Cst(TypeConstructor.False, _)) => Type.False
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
    case (Type.Cst(TypeConstructor.True, _), _) => Type.True
    case (_, Type.Cst(TypeConstructor.True, _)) => Type.True
    case (Type.Cst(TypeConstructor.False, _), _) => tpe2
    case (_, Type.Cst(TypeConstructor.False, _)) => tpe1
    case _ => Type.Apply(Type.Apply(Type.Or, tpe1), tpe2)
  }

  /**
    * Returns the type `Or(tpe1, Or(tpe2, ...))`.
    */
  def mkOr(tpes: List[Type]): Type = tpes match {
    case Nil => Type.False
    case x :: xs => mkOr(x, mkOr(xs))
  }

  /**
    * Returns the type `tpe1 => tpe2`.
    */
  def mkImplies(tpe1: Type, tpe2: Type): Type = Type.mkOr(Type.mkNot(tpe1), tpe2)

  /**
    * Returns a Boolean type that represents the equivalence of `x` and `y`.
    *
    * That is, `x == y` iff `(x /\ y) \/ (not x /\ not y)`
    */
  def mkEquiv(x: Type, y: Type): Type = Type.mkOr(Type.mkAnd(x, y), Type.mkAnd(Type.mkNot(x), Type.mkNot(y)))

  /**
    * Returns a Region type for the given rigid variable `l` with the given source location `loc`.
    */
  def mkRegion(l: Type.Var, loc: SourceLocation): Type =
    Type.Apply(Type.Cst(TypeConstructor.Region, loc), l)

  // MATT docs
  object Kinded {

    /**
      * Returns the type variables in `this` type.
      *
      * Returns a sorted set to ensure that the compiler is deterministic.
      */
    def typeVars(tpe: Type): SortedSet[Type.Var] = tpe match {
      case x: Type.Var => SortedSet(x)
      case Type.Cst(tc, _) => SortedSet.empty
      case Type.Lambda(tvar, tpe) => tvar match {
        case kindedTvar: Var => typeVars(tpe) - kindedTvar
        case _: UnkindedVar => throw InternalCompilerException("Unexpected unkinded type variable")
      }
      case Type.Apply(tpe1, tpe2) => typeVars(tpe1) ++ typeVars(tpe2)
      case _: Type.UnkindedVar => throw InternalCompilerException("Unexpected unkinded type variable")
    }

    /**
      * Applies `f` to every type variable in `this` type.
      */
    def map(tpe: Type)(f: Type.Var => Type): Type = tpe match {
      case tvar: Type.Var => f(tvar)
      case Type.Cst(_, _) => tpe
      case Type.Lambda(tvar, tpe) => Type.Lambda(tvar, map(tpe)(f))
      case Type.Apply(tpe1, tpe2) => Type.Apply(map(tpe1)(f), map(tpe2)(f))
      case _: Type.UnkindedVar => throw InternalCompilerException("Unexpected unkinded type variable")
    }
  }

  // MATT docs
  object Unkinded {

    /**
      * Returns a simplified (evaluated) form of the given type `tpe0`.
      *
      * Performs beta-reduction of type abstractions and applications.
      */
    def simplify(tpe0: Type): Type = {
      def eval(t: Type, subst: Map[Type.UnkindedVar, Type]): Type = t match {
        case tvar: Type.UnkindedVar => subst.getOrElse(tvar, tvar)

        case Type.Cst(_, _) => t

        case Type.Apply(Type.Apply(Type.Cst(TypeConstructor.RecordExtend(field), _), tpe), rest) =>
          val t1 = eval(tpe, subst)
          val t2 = eval(rest, subst)
          Type.mkRecordExtend(field, t1, t2)

        case Type.Apply(Type.Apply(Type.Cst(TypeConstructor.SchemaExtend(pred), _), tpe), rest) =>
          val t1 = eval(tpe, subst)
          val t2 = eval(rest, subst)
          Type.mkSchemaExtend(pred, t1, t2)

        case Type.Lambda(tvar, tpe) => Type.Lambda(tvar, eval(tpe, subst))

        // TODO: Does not take variable capture into account.
        case Type.Apply(tpe1, tpe2) => (eval(tpe1, subst), eval(tpe2, subst)) match {
          case (Type.Lambda(tvar, tpe3), t2) => tvar match {
            case _: Type.Var => throw InternalCompilerException("Unexpected kinded type variable")
            case unkindedTvar: Type.UnkindedVar => eval(tpe3, subst + (unkindedTvar -> t2))
          }

          case (t1, t2) => Type.Apply(t1, t2)
        }

        case _: Type.Var => throw InternalCompilerException("Unexpected kinded type variable")
      }

      eval(tpe0, Map.empty)
    }
  }

}

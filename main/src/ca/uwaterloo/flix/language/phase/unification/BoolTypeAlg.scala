package ca.uwaterloo.flix.language.phase.unification

import ca.uwaterloo.flix.language.ast.{SourceLocation, Symbol, Type}
import ca.uwaterloo.flix.util.InternalCompilerException
import ca.uwaterloo.flix.util.collection.Bimap

import scala.collection.immutable.SortedSet

class BoolTypeAlg extends BoolAlg2[Type, Symbol.KindedTypeVarSym] {
  /**
    * Returns `true` if `f` represents TRUE.
    */
  override def isTrue(f: Type): Boolean = f == Type.True

  /**
    * Returns `true` if `f` represents FALSE.
    */
  override def isFalse(f: Type): Boolean = f == Type.False

  /**
    * Returns `true` if `f` represents a variable.
    */
  override def isVar(f: Type): Boolean = f match {
    case Type.Var(_, _) => true
    case _ => false
  }

  /**
    * Returns a representation of TRUE.
    */
  override def mkTrue: Type = Type.True

  /**
    * Returns a representation of FALSE.
    */
  override def mkFalse: Type = Type.False

  /**
    * Returns a representation of the variable with the given `id`.
    */
  override def mkVar(id: Symbol.KindedTypeVarSym): Type = Type.Var(id, SourceLocation.Unknown)

  /**
    * Returns a representation of the complement of `f`.
    */
  override def mkNot(f: Type): Type = Type.mkNot(f, SourceLocation.Unknown)

  /**
    * Returns a representation of the disjunction of `f1` and `f2`.
    */
  override def mkOr(f1: Type, f2: Type): Type = Type.mkOr(f1, f2, SourceLocation.Unknown)

  /**
    * Returns a representation of the conjunction of `f1` and `f2`.
    */
  override def mkAnd(f1: Type, f2: Type): Type = Type.mkAnd(f1, f2, SourceLocation.Unknown)

  /**
    * Returns the set of free variables in `f`.
    */
  override def freeVars(f: Type): SortedSet[Symbol.KindedTypeVarSym] = f.typeVars.map(_.sym)

  /**
    * Applies the function `fn` to every variable in `f`.
    */
  override def map(f: Type)(fn: Symbol.KindedTypeVarSym => Type): Type = f.map {
    case Type.Var(sym, _) => fn(sym)
  }

  /**
    * Converts the given formula f into another Boolean formula.
    */
  override def convert[G, W](f: Type, env: Bimap[Symbol.KindedTypeVarSym, W])(implicit otherAlg: BoolAlg2[G, W]): G = f match {
    case Type.True => otherAlg.mkTrue
    case Type.False => otherAlg.mkFalse
    case Type.Apply(Type.Not, tpe1, _) => otherAlg.mkNot(convert(tpe1, env))
    case Type.Apply(Type.Apply(Type.And, tpe1, _), tpe2, _) => otherAlg.mkAnd(convert(tpe1, env), convert(tpe2, env))
    case Type.Apply(Type.Apply(Type.Or, tpe1, _), tpe2, _) => otherAlg.mkOr(convert(tpe1, env), convert(tpe2, env))
    case _ => throw InternalCompilerException(s"Unexpected type: ${f}", SourceLocation.Unknown)
  }
}

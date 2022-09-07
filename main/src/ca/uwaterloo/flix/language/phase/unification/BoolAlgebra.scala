/*
 * Copyright 2022 Magnus Madsen
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
package ca.uwaterloo.flix.language.phase.unification

import ca.uwaterloo.flix.language.ast.{Kind, SourceLocation, Symbol, Type, TypeConstructor}
import ca.uwaterloo.flix.util.InternalCompilerException
import ca.uwaterloo.flix.util.collection.Bimap

import scala.collection.immutable.SortedSet

/**
  * A common super-type for Boolean algebras.
  */
sealed trait BoolAlgebra {

  /**
    * Returns the free variables in `this` expression.
    */
  final def freeVars: SortedSet[Int] = this match {
    case BoolAlgebra.Top => SortedSet.empty
    case BoolAlgebra.Bot => SortedSet.empty
    case BoolAlgebra.Var(x) => SortedSet(x)
    case BoolAlgebra.Neg(f) => f.freeVars
    case BoolAlgebra.Join(f1, f2) => f1.freeVars ++ f2.freeVars
    case BoolAlgebra.Meet(f1, f2) => f1.freeVars ++ f2.freeVars
  }

  /**
    * Returns the size of `this` expression.
    *
    * The size is the number of joins and meets
    */
  final def size: Int = this match {
    case BoolAlgebra.Top => 0
    case BoolAlgebra.Bot => 0
    case BoolAlgebra.Var(_) => 0
    case BoolAlgebra.Neg(t) => t.size
    case BoolAlgebra.Join(t1, t2) => t1.size + t2.size + 1
    case BoolAlgebra.Meet(t1, t2) => t1.size + t2.size + 1
  }

  /**
    * Returns a human-readable string representation of `this` expression.
    */
  override def toString: String = this match {
    case BoolAlgebra.Top => "true"
    case BoolAlgebra.Bot => "false"
    case BoolAlgebra.Var(x) => s"x$x"
    case BoolAlgebra.Neg(f) => f match {
      case BoolAlgebra.Var(x) => s"!x$x"
      case _ => s"!($f)"
    }
    case BoolAlgebra.Join(f1, f2) => s"(and $f1 $f2)"
    case BoolAlgebra.Meet(f1, f2) => s"(or $f1 $f2)"
  }

}

object BoolAlgebra {

  /**
    * Represents the constant ⊤.
    */
  case object Top extends BoolAlgebra

  /**
    * Represents the constant ⊥.
    */
  case object Bot extends BoolAlgebra

  /**
    * Represents a variable. Variables are numbered by integers.
    */
  case class Var(x: Int) extends BoolAlgebra

  /**
    * Represents ¬f
    */
  case class Neg(f: BoolAlgebra) extends BoolAlgebra

  /**
    * Represents f1 ⊓ f2
    */
  case class Join(f1: BoolAlgebra, f2: BoolAlgebra) extends BoolAlgebra

  /**
    * Represents f1 ⊔ f2
    */
  case class Meet(f1: BoolAlgebra, f2: BoolAlgebra) extends BoolAlgebra

  /**
    * Substitutes all variables in `f` using the substitution map `m`.
    *
    * The map `m` must bind each free variable in `f` to a (new) variable.
    */
  def substitute(f: BoolAlgebra, m: Bimap[Int, Int]): BoolAlgebra = f match {
    case Top => Top
    case Bot => Bot
    case Var(x) => m.getForward(x) match {
      case None => throw InternalCompilerException(s"Unexpected unbound variable: 'x$x'.")
      case Some(y) => Var(y)
    }
    case Neg(f1) => Neg(substitute(f1, m))
    case Join(f1, f2) => Join(substitute(f1, m), substitute(f2, m))
    case Meet(f1, f2) => Meet(substitute(f1, m), substitute(f2, m))
  }

  /**
    * Converts the given type `tpe` to a Boolean algebra expression under the given variable substitution map `m`.
    *
    * The map `m` must bind each free type variable in `tpe` to a Boolean variable.
    */
  def fromBoolType(tpe: Type, m: Bimap[VarOrEff, Int]): BoolAlgebra = tpe match {
    case Type.Var(sym, _) => m.getForward(VarOrEff.Var(sym)) match {
      case None => throw InternalCompilerException(s"Unexpected unbound variable: '$sym'.")
      case Some(x) => Var(x)
    }
    case Type.True => Top
    case Type.False => Bot
    case Type.Apply(Type.Cst(TypeConstructor.Not, _), tpe1, _) => Neg(fromBoolType(tpe1, m))
    case Type.Apply(Type.Apply(Type.Cst(TypeConstructor.And, _), tpe1, _), tpe2, _) => Join(fromBoolType(tpe1, m), fromBoolType(tpe2, m))
    case Type.Apply(Type.Apply(Type.Cst(TypeConstructor.Or, _), tpe1, _), tpe2, _) => Meet(fromBoolType(tpe1, m), fromBoolType(tpe2, m))
    case _ => throw InternalCompilerException(s"Unexpected type: '$tpe'.")
  }

  /**
    * Converts the given type `tpe` to a Boolean formula under the given variable substitution map `m`.
    *
    * The map `m` must bind each free type variable in `tpe` to a Boolean variable.
    */
  def fromEffType(tpe: Type, m: Bimap[VarOrEff, Int]): BoolAlgebra = tpe match {
    case Type.Var(sym, _) => m.getForward(VarOrEff.Var(sym)) match {
      case None => throw InternalCompilerException(s"Unexpected unbound variable: '$sym'.")
      case Some(x) => Var(x)
    }
    case Type.Cst(TypeConstructor.Effect(sym), _) => m.getForward(VarOrEff.Eff(sym)) match {
      case None => throw InternalCompilerException(s"Unexpected unbound effect: '$sym'.")
      case Some(x) => Var(x)
    }
    case Type.All => Top
    case Type.Empty => Bot
    case Type.Apply(Type.Cst(TypeConstructor.Complement, _), tpe1, _) => Neg(fromEffType(tpe1, m))
    case Type.Apply(Type.Apply(Type.Cst(TypeConstructor.Intersection, _), tpe1, _), tpe2, _) => Join(fromEffType(tpe1, m), fromEffType(tpe2, m))
    case Type.Apply(Type.Apply(Type.Cst(TypeConstructor.Union, _), tpe1, _), tpe2, _) => Meet(fromEffType(tpe1, m), fromEffType(tpe2, m))
    case _ => throw InternalCompilerException(s"Unexpected type: '$tpe'.")
  }

  /**
    * Converts the given algebraic expression `f` back to a type under the given variable substitution map `m`.
    *
    * The map `m` must bind each free variable in `f` to a type variable.
    */
  def toType(f: BoolAlgebra, m: Bimap[VarOrEff, Int], kind: Kind, loc: SourceLocation): Type = kind match {
    case Kind.Bool => toBoolType(f, m, loc)
    case Kind.Effect => toEffType(f, m, loc)
    case _ => throw InternalCompilerException(s"Unexpected kind: '$kind'.")
  }

  /**
    * Converts the given algebraic expression `f` back to a type under the given variable substitution map `m`.
    *
    * The map `m` must bind each free variable in `f` to a type variable.
    */
  private def toBoolType(f: BoolAlgebra, m: Bimap[VarOrEff, Int], loc: SourceLocation): Type = f match {
    case Top => Type.True
    case Bot => Type.False
    case Var(x) => m.getBackward(x) match {
      case None => throw InternalCompilerException(s"Unexpected unbound variable: '$x'.")
      case Some(VarOrEff.Var(sym)) => Type.Var(sym, loc)
      case Some(VarOrEff.Eff(sym)) => throw InternalCompilerException(s"Unexpected effect: '$sym'.")
    }
    case Neg(f1) => Type.mkNot(toBoolType(f1, m, loc), loc)
    case Join(t1, t2) => Type.mkAnd(toBoolType(t1, m, loc), toBoolType(t2, m, loc), loc)
    case Meet(t1, t2) => Type.mkOr(toBoolType(t1, m, loc), toBoolType(t2, m, loc), loc)
  }

  /**
    * Converts the given algebraic expression `f` back to a type under the given variable substitution map `m`.
    *
    * The map `m` must bind each free variable in `f` to a type variable.
    */
  private def toEffType(f: BoolAlgebra, m: Bimap[VarOrEff, Int], loc: SourceLocation): Type = f match {
    case Top => Type.All
    case Bot => Type.Empty
    case Var(x) => m.getBackward(x) match {
      case None => throw InternalCompilerException(s"Unexpected unbound variable: '$x'.")
      case Some(VarOrEff.Var(sym)) => Type.Var(sym, loc)
      case Some(VarOrEff.Eff(sym)) => Type.Cst(TypeConstructor.Effect(sym), loc)
    }
    case Neg(f1) => Type.mkComplement(toEffType(f1, m, loc), loc)
    case Join(t1, t2) => Type.mkIntersection(toEffType(t1, m, loc), toEffType(t2, m, loc), loc)
    case Meet(t1, t2) => Type.mkUnion(toEffType(t1, m, loc), toEffType(t2, m, loc), loc)
  }


  /**
    * Union of variable and effect types.
    */
  sealed trait VarOrEff

  object VarOrEff {
    /**
      * A type variable.
      */
    case class Var(sym: Symbol.KindedTypeVarSym) extends VarOrEff

    /**
      * An effect constant.
      */
    case class Eff(sym: Symbol.EffectSym) extends VarOrEff
  }
}

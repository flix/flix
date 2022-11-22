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

import scala.annotation.tailrec
import scala.collection.immutable.SortedSet

/**
  * A common super-type for Boolean algebras.
  */
sealed trait BoolFormula {

  /**
    * Returns the free variables in `this` expression.
    */
  final def freeVars: SortedSet[Int] = this match {
    case BoolFormula.True => SortedSet.empty
    case BoolFormula.False => SortedSet.empty
    case BoolFormula.Var(x) => SortedSet(x)
    case BoolFormula.Not(f) => f.freeVars
    case BoolFormula.And(f1, f2) => f1.freeVars ++ f2.freeVars
    case BoolFormula.Or(f1, f2) => f1.freeVars ++ f2.freeVars
  }

  /**
    * Returns the size of `this` expression.
    *
    * The size is the number of joins and meets
    */
  final def size: Int = this match {
    case BoolFormula.True => 0
    case BoolFormula.False => 0
    case BoolFormula.Var(_) => 0
    case BoolFormula.Not(t) => t.size
    case BoolFormula.And(t1, t2) => t1.size + t2.size + 1
    case BoolFormula.Or(t1, t2) => t1.size + t2.size + 1
  }

  /**
    * Returns a human-readable string representation of `this` expression.
    */
  override def toString: String = this match {
    case BoolFormula.True => "true"
    case BoolFormula.False => "false"
    case BoolFormula.Var(x) => s"x$x"
    case BoolFormula.Not(f) => f match {
      case BoolFormula.Var(x) => s"!x$x"
      case _ => s"!($f)"
    }
    case BoolFormula.And(f1, f2) => s"(and $f1 $f2)"
    case BoolFormula.Or(f1, f2) => s"(or $f1 $f2)"
  }

}

object BoolFormula {

  /**
    * Represents the constant ⊤.
    */
  case object True extends BoolFormula

  /**
    * Represents the constant ⊥.
    */
  case object False extends BoolFormula

  /**
    * Represents a variable. Variables are numbered by integers.
    */
  case class Var(x: Int) extends BoolFormula

  /**
    * Represents ¬f
    */
  case class Not(f: BoolFormula) extends BoolFormula

  /**
    * Represents f1 ⊓ f2
    */
  case class And(f1: BoolFormula, f2: BoolFormula) extends BoolFormula

  /**
    * Represents f1 ⊔ f2
    */
  case class Or(f1: BoolFormula, f2: BoolFormula) extends BoolFormula

  /**
    * Substitutes all variables in `f` using the substitution map `m`.
    *
    * The map `m` must bind each free variable in `f` to a (new) variable.
    */
  def substitute(f: BoolFormula, m: Bimap[Int, Int]): BoolFormula = f match {
    case True => True
    case False => False
    case Var(x) => m.getForward(x) match {
      case None => throw InternalCompilerException(s"Unexpected unbound variable: 'x$x'.")
      case Some(y) => Var(y)
    }
    case Not(f1) => Not(substitute(f1, m))
    case And(f1, f2) => And(substitute(f1, m), substitute(f2, m))
    case Or(f1, f2) => Or(substitute(f1, m), substitute(f2, m))
  }

  /**
    * Converts the given type `tpe` to a Boolean algebra expression under the given variable substitution map `m`.
    *
    * The map `m` must bind each free type variable in `tpe` to a Boolean variable.
    */
  def fromBoolType(tpe: Type, m: Bimap[VarOrEff, Int]): BoolFormula = tpe match {
    case Type.Var(sym, _) => m.getForward(VarOrEff.Var(sym)) match {
      case None => throw InternalCompilerException(s"Unexpected unbound variable: '$sym'.")
      case Some(x) => Var(x)
    }
    case Type.True => True
    case Type.False => False
    case Type.Apply(Type.Cst(TypeConstructor.Not, _), tpe1, _) => Not(fromBoolType(tpe1, m))
    case Type.Apply(Type.Apply(Type.Cst(TypeConstructor.And, _), tpe1, _), tpe2, _) => And(fromBoolType(tpe1, m), fromBoolType(tpe2, m))
    case Type.Apply(Type.Apply(Type.Cst(TypeConstructor.Or, _), tpe1, _), tpe2, _) => Or(fromBoolType(tpe1, m), fromBoolType(tpe2, m))
    case _ => throw InternalCompilerException(s"Unexpected type: '$tpe'.")
  }

  /**
    * Converts the given type `tpe` to a Boolean formula under the given variable substitution map `m`.
    *
    * The map `m` must bind each free type variable in `tpe` to a Boolean variable.
    */
  def fromEffType(tpe: Type, m: Bimap[VarOrEff, Int]): BoolFormula = tpe match {
    case Type.Var(sym, _) => m.getForward(VarOrEff.Var(sym)) match {
      case None => throw InternalCompilerException(s"Unexpected unbound variable: '$sym'.")
      case Some(x) => Var(x)
    }
    case Type.Cst(TypeConstructor.Effect(sym), _) => m.getForward(VarOrEff.Eff(sym)) match {
      case None => throw InternalCompilerException(s"Unexpected unbound effect: '$sym'.")
      case Some(x) => Var(x)
    }
    case Type.All => True
    case Type.Empty => False
    case Type.Apply(Type.Cst(TypeConstructor.Complement, _), tpe1, _) => Not(fromEffType(tpe1, m))
    case Type.Apply(Type.Apply(Type.Cst(TypeConstructor.Intersection, _), tpe1, _), tpe2, _) => And(fromEffType(tpe1, m), fromEffType(tpe2, m))
    case Type.Apply(Type.Apply(Type.Cst(TypeConstructor.Union, _), tpe1, _), tpe2, _) => Or(fromEffType(tpe1, m), fromEffType(tpe2, m))
    case _ => throw InternalCompilerException(s"Unexpected type: '$tpe'.")
  }

  /**
    * Converts the given algebraic expression `f` back to a type under the given variable substitution map `m`.
    *
    * The map `m` must bind each free variable in `f` to a type variable.
    */
  def toType(f: BoolFormula, m: Bimap[VarOrEff, Int], kind: Kind, loc: SourceLocation): Type = kind match {
    case Kind.Bool => toBoolType(f, m, loc)
    case Kind.Effect => toEffType(f, m, loc)
    case _ => throw InternalCompilerException(s"Unexpected kind: '$kind'.")
  }

  /**
    * Converts the given algebraic expression `f` back to a type under the given variable substitution map `m`.
    *
    * The map `m` must bind each free variable in `f` to a type variable.
    */
  private def toBoolType(f: BoolFormula, m: Bimap[VarOrEff, Int], loc: SourceLocation): Type = f match {
    case True => Type.True
    case False => Type.False
    case Var(x) => m.getBackward(x) match {
      case None => throw InternalCompilerException(s"Unexpected unbound variable: '$x'.")
      case Some(VarOrEff.Var(sym)) => Type.Var(sym, loc)
      case Some(VarOrEff.Eff(sym)) => throw InternalCompilerException(s"Unexpected effect: '$sym'.")
    }
    case Not(f1) => Type.mkNot(toBoolType(f1, m, loc), loc)
    case And(t1, t2) => Type.mkAnd(toBoolType(t1, m, loc), toBoolType(t2, m, loc), loc)
    case Or(t1, t2) => Type.mkOr(toBoolType(t1, m, loc), toBoolType(t2, m, loc), loc)
  }

  /**
    * Converts the given algebraic expression `f` back to a type under the given variable substitution map `m`.
    *
    * The map `m` must bind each free variable in `f` to a type variable.
    */
  private def toEffType(f: BoolFormula, m: Bimap[VarOrEff, Int], loc: SourceLocation): Type = f match {
    case True => Type.All
    case False => Type.Empty
    case Var(x) => m.getBackward(x) match {
      case None => throw InternalCompilerException(s"Unexpected unbound variable: '$x'.")
      case Some(VarOrEff.Var(sym)) => Type.Var(sym, loc)
      case Some(VarOrEff.Eff(sym)) => Type.Cst(TypeConstructor.Effect(sym), loc)
    }
    case Not(f1) => Type.mkComplement(toEffType(f1, m, loc), loc)
    case And(t1, t2) => Type.mkIntersection(toEffType(t1, m, loc), toEffType(t2, m, loc), loc)
    case Or(t1, t2) => Type.mkUnion(toEffType(t1, m, loc), toEffType(t2, m, loc), loc)
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

  implicit val AsBoolAlgTrait: BoolAlg[BoolFormula] = new BoolAlg[BoolFormula] {

    override def isTrue(f: BoolFormula): Boolean = f == BoolFormula.True

    override def isFalse(f: BoolFormula): Boolean = f == BoolFormula.False

    override def mkTrue: BoolFormula = True

    override def mkFalse: BoolFormula = False

    override def mkVar(id: Int): BoolFormula = Var(id)

    override def mkNot(f: BoolFormula): BoolFormula = f match {
      case BoolFormula.True =>
        BoolFormula.False

      case BoolFormula.False =>
        BoolFormula.True

      case BoolFormula.Not(x) =>
        x

//      // ¬(¬x ∨ y) => x ∧ ¬y
//      case BoolFormula.Or(BoolFormula.Not(x), y) =>
//        mkAnd(x, mkNot(y))
//
//      // ¬(x ∨ ¬y) => ¬x ∧ y
//      case BoolFormula.Or(x, BoolFormula.Not(y)) =>
//        mkAnd(mkNot(x), y)

      case _ => BoolFormula.Not(f)
    }

    override def mkAnd(f1: BoolFormula, f2: BoolFormula): BoolFormula = (f1, f2) match {
      // T ∧ x => x
      case (BoolFormula.True, _) =>
        f2

      // x ∧ T => x
      case (_, BoolFormula.True) =>
        f1

      // F ∧ x => F
      case (BoolFormula.False, _) =>
        BoolFormula.False

      // x ∧ F => F
      case (_, BoolFormula.False) =>
        BoolFormula.False

//      // ¬x ∧ (x ∨ y) => ¬x ∧ y
//      case (BoolFormula.Not(x1), BoolFormula.Or(x2, y)) if x1 == x2 =>
//        mkAnd(mkNot(x1), y)

      // x ∧ ¬x => F
      case (x1, BoolFormula.Not(x2)) if x1 == x2 =>
        BoolFormula.False

      // ¬x ∧ x => F
      case (BoolFormula.Not(x1), x2) if x1 == x2 =>
        BoolFormula.False

//      // x ∧ (x ∧ y) => (x ∧ y)
//      case (x1, BoolFormula.And(x2, y)) if x1 == x2 =>
//        mkAnd(x1, y)
//
//      // x ∧ (y ∧ x) => (x ∧ y)
//      case (x1, BoolFormula.And(y, x2)) if x1 == x2 =>
//        mkAnd(x1, y)

      // (x ∧ y) ∧ x) => (x ∧ y)
      case (BoolFormula.And(x1, y), x2) if x1 == x2 =>
        mkAnd(x1, y)

      // (x ∧ y) ∧ y) => (x ∧ y)
      case (BoolFormula.And(x, y1), y2) if y1 == y2 =>
        mkAnd(x, y1)

//      // x ∧ (x ∨ y) => x
//      case (x1, BoolFormula.Or(x2, _)) if x1 == x2 =>
//        x1
//
//      // (x ∨ y) ∧ x => x
//      case (BoolFormula.Or(x1, _), x2) if x1 == x2 =>
//        x1
//
//      // x ∧ (y ∧ ¬x) => F
//      case (x1, BoolFormula.And(_, BoolFormula.Not(x2))) if x1 == x2 =>
//        BoolFormula.False
//
//      // (¬x ∧ y) ∧ x => F
//      case (BoolFormula.And(BoolFormula.Not(x1), _), x2) if x1 == x2 =>
//        BoolFormula.False
//
//      // x ∧ ¬(x ∨ y) => F
//      case (x1, BoolFormula.Not(BoolFormula.Or(x2, _))) if x1 == x2 =>
//        BoolFormula.False
//
//      // ¬(x ∨ y) ∧ x => F
//      case (BoolFormula.Not(BoolFormula.Or(x1, _)), x2) if x1 == x2 =>
//        BoolFormula.False
//
//      // x ∧ (¬x ∧ y) => F
//      case (x1, BoolFormula.And(BoolFormula.Not(x2), _)) if x1 == x2 =>
//        BoolFormula.False
//
//      // (¬x ∧ y) ∧ x => F
//      case (BoolFormula.And(BoolFormula.Not(x1), _), x2) if x1 == x2 =>
//        BoolFormula.False

      // x ∧ x => x
      case _ if f1 == f2 => f1

      case _ =>
        //      val s = s"And($eff1, $eff2)"
        //      val len = s.length
        //      if (true) {
        //        println(s.substring(0, Math.min(len, 300)))
        //      }

        BoolFormula.And(f1, f2)
    }

    override def mkOr(f1: BoolFormula, f2: BoolFormula): BoolFormula = (f1, f2) match {
      // T ∨ x => T
      case (BoolFormula.True, _) =>
        BoolFormula.True

      // F ∨ y => y
      case (BoolFormula.False, _) =>
        f2

      // x ∨ T => T
      case (_, BoolFormula.True) =>
        BoolFormula.True

      // x ∨ F => x
      case (_, BoolFormula.False) =>
        f1

//      // x ∨ (y ∨ x) => x ∨ y
//      case (x1, BoolFormula.Or(y, x2)) if x1 == x2 =>
//        mkOr(x1, y)
//
//      // (x ∨ y) ∨ x => x ∨ y
//      case (BoolFormula.Or(x1, y), x2) if x1 == x2 =>
//        mkOr(x1, y)

      // ¬x ∨ x => T
      case (BoolFormula.Not(x), y) if x == y =>
        BoolFormula.True

      // x ∨ ¬x => T
      case (x, BoolFormula.Not(y)) if x == y =>
        BoolFormula.True

//      // (¬x ∨ y) ∨ x) => T
//      case (BoolFormula.Or(BoolFormula.Not(x), _), y) if x == y =>
//        BoolFormula.True
//
//      // x ∨ (¬x ∨ y) => T
//      case (x, BoolFormula.Or(BoolFormula.Not(y), _)) if x == y =>
//        BoolFormula.True
//
//      // x ∨ (y ∧ x) => x
//      case (x1, BoolFormula.And(_, x2)) if x1 == x2 => x1
//
//      // (y ∧ x) ∨ x => x
//      case (BoolFormula.And(_, x1), x2) if x1 == x2 => x1
//
//      // x ∨ x => x
//      case _ if f1 == f2 =>
//        f1

      case _ =>

        //              val s = s"Or($eff1, $eff2)"
        //              val len = s.length
        //              if (len > 30) {
        //                println(s.substring(0, Math.min(len, 300)))
        //              }

        BoolFormula.Or(f1, f2)
    }

    override def map(f: BoolFormula)(fn: Int => BoolFormula): BoolFormula = f match {
      case True => True
      case False => False
      case And(f1, f2) => mkAnd(map(f1)(fn), map(f2)(fn))
      case Or(f1, f2) => mkOr(map(f1)(fn), map(f2)(fn))
      case Not(f1) => mkNot(map(f1)(fn))
      case Var(sym) => fn(sym)
    }

    override def getEnv(fs: List[Type]): Bimap[Symbol.KindedTypeVarSym, Int] = {
      // Compute the variables in `tpe`.
      val tvars = fs.flatMap(_.typeVars).map(_.sym).to(SortedSet)

      // Construct a bi-directional map from type variables to indices.
      // The idea is that the first variable becomes x0, the next x1, and so forth.
      tvars.zipWithIndex.foldLeft(Bimap.empty[Symbol.KindedTypeVarSym, Int]) {
        case (macc, (sym, x)) => macc + (sym -> x)
      }
    }

    override def toType(f: BoolFormula, env: Bimap[Symbol.KindedTypeVarSym, Int]): Type = f match {
      case True => Type.True
      case False => Type.False
      case And(f1, f2) => Type.mkApply(Type.And, List(toType(f1, env), toType(f2, env)), SourceLocation.Unknown)
      case Or(f1, f2) => Type.mkApply(Type.Or, List(toType(f1, env), toType(f2, env)), SourceLocation.Unknown)
      case Not(f1) => Type.Apply(Type.Not, toType(f1, env), SourceLocation.Unknown)
      case Var(id) => env.getBackward(id) match {
        case Some(sym) => Type.Var(sym, SourceLocation.Unknown)
        case None => throw InternalCompilerException(s"unexpected unknown ID: $id")
      }
    }

    override def freeVars(f: BoolFormula): SortedSet[Int] = f match {
      case True => SortedSet.empty
      case False => SortedSet.empty
      case And(f1, f2) => freeVars(f1) ++ freeVars(f2)
      case Or(f1, f2) => freeVars(f1) ++ freeVars(f2)
      case Not(f1) => freeVars(f1)
      case Var(id) => SortedSet(id)
    }

    override def minimize(f: BoolFormula): BoolFormula = BoolFormulaTable.minimizeFormula(f)
  }
}

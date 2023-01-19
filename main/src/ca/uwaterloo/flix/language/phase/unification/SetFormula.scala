package ca.uwaterloo.flix.language.phase.unification

import ca.uwaterloo.flix.language.ast.{SourceLocation, Symbol, Type, TypeConstructor}
import ca.uwaterloo.flix.util.InternalCompilerException
import ca.uwaterloo.flix.util.collection.Bimap

import scala.collection.immutable.SortedSet

sealed trait SetFormula {

  /**
    * Returns the free variables in `this` expression.
    */
  final def freeVars: SortedSet[Int] = this match {
    case SetFormula.All => SortedSet.empty
    case SetFormula.Cst(_) => SortedSet.empty
    case SetFormula.Var(x) => SortedSet(x)
    case SetFormula.Not(f) => f.freeVars
    case SetFormula.And(f1, f2) => f1.freeVars ++ f2.freeVars
    case SetFormula.Or(f1, f2) => f1.freeVars ++ f2.freeVars
  }

  /**
    * Returns the size of `this` expression.
    *
    * The size is the number of joins and meets
    */
  final def size: Int = this match {
    case SetFormula.All => 0
    case SetFormula.Cst(_) => 0
    case SetFormula.Var(_) => 0
    case SetFormula.Not(t) => t.size
    case SetFormula.And(t1, t2) => t1.size + t2.size + 1
    case SetFormula.Or(t1, t2) => t1.size + t2.size + 1
  }

  /**
    * Returns a human-readable string representation of `this` expression.
    */
  override def toString: String = this match {
    case SetFormula.All => "T"
    case SetFormula.Cst(s) => s.map(_.toString).mkString("{", ", ", "}")
    case SetFormula.Var(x) => s"x$x"
    case SetFormula.Not(f) => f match {
      case SetFormula.Var(x) => s"~x$x"
      case _ => s"~($f)"
    }
    case SetFormula.And(f1, f2) => s"($f1 ∪ $f2)"
    case SetFormula.Or(f1, f2) => s"($f1 ∩ $f2)"
  }

}

object SetFormula {

  case object All extends SetFormula

  val Empty: SetFormula = Cst(Set.empty)

  case class Cst(s: Set[Int]) extends SetFormula

  case class Var(x: Int) extends SetFormula

  case class Not(f: SetFormula) extends SetFormula

  case class And(f1: SetFormula, f2: SetFormula) extends SetFormula

  case class Or(f1: SetFormula, f2: SetFormula) extends SetFormula

  /**
    * Substitutes all variables in `f` using the substitution map `m`.
    *
    * The map `m` must bind each free variable in `f` to a (new) variable.
    */
  def substitute(f: SetFormula, m: Bimap[Int, Int]): SetFormula = f match {
    case All => All
    case Cst(s) => Cst(s)
    case Var(x) => m.getForward(x) match {
      case None => throw InternalCompilerException(s"Unexpected unbound variable: 'x$x'.", SourceLocation.Unknown)
      case Some(y) => Var(y)
    }
    case Not(f1) => Not(substitute(f1, m))
    case And(f1, f2) => And(substitute(f1, m), substitute(f2, m))
    case Or(f1, f2) => Or(substitute(f1, m), substitute(f2, m))
  }

  /**
    * Converts the given algebraic expression `f` back to a type under the given variable substitution map `m`.
    *
    * The map `m` must bind each free variable in `f` to a type variable.
    */
  def fromCaseType(tpe: Type, m: Bimap[VarOrCase, Int]): SetFormula = tpe match {
    case Type.Var(sym, _) => m.getForward(VarOrCase.Var(sym)) match {
      case None => throw InternalCompilerException(s"Unexpected unbound variable: '$sym'.", sym.loc)
      case Some(x) => Var(x)
    }
    case Type.Cst(TypeConstructor.CaseConstant(sym), _) => m.getForward(VarOrCase.Case(sym)) match {
      case None => throw InternalCompilerException(s"Unexpected unbound case: '$sym'.", sym.loc)
      case Some(x) => Cst(Set(x))
    }
    case Type.Cst(TypeConstructor.CaseAll(_), _) => All
    case Type.Cst(TypeConstructor.CaseEmpty(_), _) => Empty
    case Type.Apply(Type.Cst(TypeConstructor.CaseComplement(_), _), tpe1, _) =>
      Not(fromCaseType(tpe1, m))
    case Type.Apply(Type.Apply(Type.Cst(TypeConstructor.CaseIntersection(_), _), tpe1, _), tpe2, _) =>
      And(fromCaseType(tpe1, m), fromCaseType(tpe2, m))
    case Type.Apply(Type.Apply(Type.Cst(TypeConstructor.CaseUnion(_), _), tpe1, _), tpe2, _) =>
      Or(fromCaseType(tpe1, m), fromCaseType(tpe2, m))
    case _ => throw InternalCompilerException(s"Unexpected type: '$tpe'.", tpe.loc)
  }

  /**
    * Converts the given algebraic expression `f` back to a type under the given variable substitution map `m`.
    *
    * The map `m` must bind each free variable in `f` to a type variable.
    */
  private def toCaseType(f: SetFormula, sym: Symbol.RestrictableEnumSym, m: Bimap[VarOrCase, Int], loc: SourceLocation): Type = f match {
    case All => Type.Cst(TypeConstructor.CaseAll(sym), loc)
    case Cst(s) => s.
      map(i => m.getBackward(i) match {
        case Some(VarOrCase.Case(caseSym)) => caseSym
        case Some(VarOrCase.Var(_)) => throw InternalCompilerException(s"Unexpected type var in case set: '$i'.", loc)
        case None => throw InternalCompilerException(s"Unexpected unbound case set constant: '$i'.", loc)
      }).
      map(caseSym => Type.Cst(TypeConstructor.CaseConstant(caseSym), loc): Type).
      reduceOption(Type.mkCaseUnion(_, _, sym, loc)).getOrElse(Type.Cst(TypeConstructor.CaseEmpty(sym), loc))
    case Var(x) => m.getBackward(x) match {
      case None => throw InternalCompilerException(s"Unexpected unbound variable: '$x'.", loc)
      case Some(VarOrCase.Var(sym)) => Type.Var(sym, loc)
      case Some(VarOrCase.Case(sym)) => Type.Cst(TypeConstructor.CaseConstant(sym), loc)
    }
    case Not(f1) => Type.mkCaseComplement(toCaseType(f1, sym, m, loc), sym, loc)
    case And(t1, t2) => Type.mkCaseIntersection(toCaseType(t1, sym, m, loc), toCaseType(t2, sym, m, loc), sym, loc)
    case Or(t1, t2) => Type.mkCaseUnion(toCaseType(t1, sym, m, loc), toCaseType(t2, sym, m, loc), sym, loc)
  }

  /**
    * Union of variable and case types.
    */
  sealed trait VarOrCase

  object VarOrCase {
    /**
      * A type variable.
      */
    case class Var(sym: Symbol.KindedTypeVarSym) extends VarOrCase

    /**
      * A Case constant.
      */
    case class Case(sym: Symbol.RestrictableCaseSym) extends VarOrCase
  }

}

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
    case SetFormula.Cst(s) => s.map(_.toString).mkString("{", ", ", "}")
    case SetFormula.Var(x) => s"x$x"
    case SetFormula.Not(f) => f match {
      case SetFormula.Var(x) => s"~x$x"
      case _ => s"~($f)"
    }
    case SetFormula.And(f1, f2) => s"($f1 ∩ $f2)"
    case SetFormula.Or(f1, f2) => s"($f1 ∪ $f2)"
  }

}

object SetFormula {

  case class Cst(s: Set[Int]) extends SetFormula

  case class Var(x: Int) extends SetFormula

  case class Not(f: SetFormula) extends SetFormula

  case class And(f1: SetFormula, f2: SetFormula) extends SetFormula

  case class Or(f1: SetFormula, f2: SetFormula) extends SetFormula

  /**
    * Represents the empty set.
    */
  val Empty: SetFormula = Cst(Set.empty)

  /**
    * Constructs the universe set.
    */
  def mkUni()(implicit univ: Set[Int]): SetFormula = Cst(univ)

  /**
    * Returns the constant set for the given `s`.
    */
  def mkCst(s: Set[Int]): SetFormula = Cst(s)

  /**
    * Returns the negation of the set formula `tpe0`.
    */
  def mkNot(tpe0: SetFormula)(implicit univ: Set[Int]): SetFormula = tpe0 match {
    case SetFormula.Cst(s) =>
      SetFormula.Cst(univ -- s)

    case SetFormula.Not(x) =>
      x

    //    // ¬(¬x ∨ y) => x ∧ ¬y
    //    case SetFormula.Or(SetFormula.Not(x), y) =>
    //      mkAnd(x, mkNot(y))
    //
    //    // ¬(x ∨ ¬y) => ¬x ∧ y
    //    case SetFormula.Or(x, SetFormula.Not(y)) =>
    //      mkAnd(mkNot(x), y)

    case _ => SetFormula.Not(tpe0)
  }

  /**
    * Returns the conjunction of the two set formulas `tpe1` and `tpe2`.
    */
  // NB: The order of cases has been determined by code coverage analysis.
  def mkAnd(tpe1: SetFormula, tpe2: SetFormula)(implicit univ: Set[Int]): SetFormula = (tpe1, tpe2) match {
    case (SetFormula.Cst(x1), x2) if x1 == univ =>
      x2

    case (x1, SetFormula.Cst(x2)) if x2 == univ =>
      x1

    case (SetFormula.Cst(x1), SetFormula.Cst(x2)) =>
      SetFormula.Cst(x1 & x2)

    //    // ¬x ∧ (x ∨ y) => ¬x ∧ y
    //    case (SetFormula.Not(x1), SetFormula.Or(x2, y)) if x1 == x2 =>
    //      mkAnd(mkNot(x1), y)
    //
    //    // x ∧ ¬x => F
    //    case (x1, SetFormula.Not(x2)) if x1 == x2 =>
    //      SetFormula.Empty
    //
    //    // ¬x ∧ x => F
    //    case (SetFormula.Not(x1), x2) if x1 == x2 =>
    //      SetFormula.Empty
    //
    //    // x ∧ (x ∧ y) => (x ∧ y)
    //    case (x1, SetFormula.And(x2, y)) if x1 == x2 =>
    //      mkAnd(x1, y)
    //
    //    // x ∧ (y ∧ x) => (x ∧ y)
    //    case (x1, SetFormula.And(y, x2)) if x1 == x2 =>
    //      mkAnd(x1, y)
    //
    //    // (x ∧ y) ∧ x) => (x ∧ y)
    //    case (SetFormula.And(x1, y), x2) if x1 == x2 =>
    //      mkAnd(x1, y)
    //
    //    // (x ∧ y) ∧ y) => (x ∧ y)
    //    case (SetFormula.And(x, y1), y2) if y1 == y2 =>
    //      mkAnd(x, y1)
    //
    //    // x ∧ (x ∨ y) => x
    //    case (x1, SetFormula.Or(x2, _)) if x1 == x2 =>
    //      x1
    //
    //    // (x ∨ y) ∧ x => x
    //    case (SetFormula.Or(x1, _), x2) if x1 == x2 =>
    //      x1
    //
    //    // x ∧ (y ∧ ¬x) => F
    //    case (x1, SetFormula.And(_, SetFormula.Not(x2))) if x1 == x2 =>
    //      SetFormula.Empty
    //
    //    // (¬x ∧ y) ∧ x => F
    //    case (SetFormula.And(SetFormula.Not(x1), _), x2) if x1 == x2 =>
    //      SetFormula.Empty
    //
    //    // x ∧ ¬(x ∨ y) => F
    //    case (x1, SetFormula.Not(SetFormula.Or(x2, _))) if x1 == x2 =>
    //      SetFormula.Empty
    //
    //    // ¬(x ∨ y) ∧ x => F
    //    case (SetFormula.Not(SetFormula.Or(x1, _)), x2) if x1 == x2 =>
    //      SetFormula.Empty
    //
    //    // x ∧ (¬x ∧ y) => F
    //    case (x1, SetFormula.And(SetFormula.Not(x2), _)) if x1 == x2 =>
    //      SetFormula.Empty
    //
    //    // (¬x ∧ y) ∧ x => F
    //    case (SetFormula.And(SetFormula.Not(x1), _), x2) if x1 == x2 =>
    //      SetFormula.Empty

    // x ∧ x => x
    case _ if tpe1 == tpe2 => tpe1

    case _ =>
      //      val s = s"And($eff1, $eff2)"
      //      val len = s.length
      //      if (true) {
      //        println(s.substring(0, Math.min(len, 300)))
      //      }

      SetFormula.And(tpe1, tpe2)
  }

  /**
    * Returns the disjunction of the two set formulas `tpe1` and `tpe2`.
    */
  // NB: The order of cases has been determined by code coverage analysis.
  def mkOr(tpe1: SetFormula, tpe2: SetFormula)(implicit univ: Set[Int]): SetFormula = (tpe1, tpe2) match {
    case (SetFormula.Cst(x1), x2) if x1 == univ =>
      mkUni()

    case (x1, SetFormula.Cst(x2)) if x2 == univ =>
      mkUni()

    case (SetFormula.Cst(s1), SetFormula.Cst(s2)) =>
      SetFormula.Cst(s1 ++ s2)
    //
    //    // x ∨ (y ∨ x) => x ∨ y
    //    case (x1, SetFormula.Or(y, x2)) if x1 == x2 =>
    //      mkOr(x1, y)
    //
    //    // (x ∨ y) ∨ x => x ∨ y
    //    case (SetFormula.Or(x1, y), x2) if x1 == x2 =>
    //      mkOr(x1, y)
    //
    //    // ¬x ∨ x => T
    //    case (SetFormula.Not(x), y) if x == y =>
    //      SetFormula.Cst(univ)
    //
    //    // x ∨ ¬x => T
    //    case (x, SetFormula.Not(y)) if x == y =>
    //      SetFormula.Cst(univ)
    //
    //    // (¬x ∨ y) ∨ x) => T
    //    case (SetFormula.Or(SetFormula.Not(x), _), y) if x == y =>
    //      SetFormula.Cst(univ)
    //
    //    // x ∨ (¬x ∨ y) => T
    //    case (x, SetFormula.Or(SetFormula.Not(y), _)) if x == y =>
    //      SetFormula.Cst(univ)
    //
    //    // x ∨ (y ∧ x) => x
    //    case (x1, SetFormula.And(_, x2)) if x1 == x2 => x1
    //
    //    // (y ∧ x) ∨ x => x
    //    case (SetFormula.And(_, x1), x2) if x1 == x2 => x1
    //
    //    // x ∨ x => x
    //    case _ if tpe1 == tpe2 =>
    //      tpe1

    case _ =>
      SetFormula.Or(tpe1, tpe2)
  }


  /**
    * Evaluates the set formula. Assumes there are no variables in the formula.
    */
  def eval(f: SetFormula)(implicit univ: Set[Int]): Set[Int] = f match {
    case SetFormula.Cst(s) => s
    case SetFormula.Not(f) => univ -- eval(f)
    case SetFormula.And(f1, f2) => eval(f1) & eval(f2)
    case SetFormula.Or(f1, f2) => eval(f1) ++ eval(f2)
    case SetFormula.Var(x) => throw InternalCompilerException("unexpected var", SourceLocation.Unknown)
  }

  // TODO: DOC
  def minimize(f: SetFormula)(implicit univ: Set[Int]): SetFormula = {
    val bot = SetFormula.Cst(Set.empty): SetFormula
    val top = SetFormula.Cst(univ): SetFormula

    def visit(fvs: List[Int]): List[(SetFormula, Map[Int, SetFormula])] = fvs match {
      case Nil => List((top, Map.empty))
      case v :: vs =>
        visit(vs) flatMap {
          case (p, partialSubst) =>
            val p1 = mkAnd(SetFormula.Not(SetFormula.Var(v)), p)
            val p2 = mkAnd(SetFormula.Var(v), p)
            List((p1, partialSubst + (v -> bot)), (p2, partialSubst + (v -> top)))
        }
    }

    visit(f.freeVars.toList).foldLeft(bot) {
      case (acc, (p, subst)) => mkOr(acc, mkAnd(p, applySubst(f, subst)))
    }
  }

  private def applySubst(f: SetFormula, m: Map[Int, SetFormula])(implicit univ: Set[Int]): SetFormula = SetFormula.map(f)(m)(univ)

  /**
    * Substitutes all variables in `f` using the substitution map `m`.
    *
    * The map `m` must bind each free variable in `f` to a (new) variable.
    */
  def substitute(f: SetFormula, m: Map[Int, Int]): SetFormula = f match {
    case Cst(s) => Cst(s)
    case Var(x) => m.get(x) match {
      case None => throw InternalCompilerException(s"Unexpected unbound variable: 'x$x'.", SourceLocation.Unknown)
      case Some(y) => Var(y)
    }
    case Not(f1) => Not(substitute(f1, m))
    case And(f1, f2) => And(substitute(f1, m), substitute(f2, m))
    case Or(f1, f2) => Or(substitute(f1, m), substitute(f2, m))
  }

  /**
    * Runs the function `fn` on all the variables in the formula.
    */
  def map(f: SetFormula)(fn: Int => SetFormula)(implicit univ: Set[Int]): SetFormula = f match {
    case Cst(s) => Cst(s)
    case Var(x) => fn(x)
    case Not(f1) => mkNot(map(f1)(fn))
    case And(f1, f2) => mkAnd(map(f1)(fn), map(f2)(fn))
    case Or(f1, f2) => mkOr(map(f1)(fn), map(f2)(fn))
  }

  /**
    * Creates an environment for mapping between proper types and formulas.
    */
  def mkEnv(ts: List[Type], univ: List[Symbol.RestrictableCaseSym]): (Bimap[VarOrCase, Int], Set[Int]) = {
    val vars = ts.flatMap(_.typeVars).map(_.sym).distinct.map(VarOrCase.Var)
    val cases = (univ ++ ts.flatMap(_.cases)).distinct.map(VarOrCase.Case)

    val forward = (vars ++ cases).zipWithIndex.toMap[VarOrCase, Int]
    val backward = forward.map { case (a, b) => (b, a) }

    val newUniv = univ.map {
      case sym => forward(VarOrCase.Case(sym))
    }

    (Bimap(forward, backward), newUniv.toSet)
  }

  /**
    * Converts the given algebraic expression `f` back to a type under the given variable substitution map `m`.
    *
    * The map `m` must bind each free variable in `f` to a type variable.
    */
  def fromCaseType(tpe: Type, m: Bimap[VarOrCase, Int], univ: Set[Int]): SetFormula = tpe match {
    case Type.Var(sym, _) => m.getForward(VarOrCase.Var(sym)) match {
      case None => throw InternalCompilerException(s"Unexpected unbound variable: '$sym'.", sym.loc)
      case Some(x) => Var(x)
    }
    case Type.Cst(TypeConstructor.CaseConstant(sym), _) => m.getForward(VarOrCase.Case(sym)) match {
      case None => throw InternalCompilerException(s"Unexpected unbound case: '$sym'.", sym.loc)
      case Some(x) => Cst(Set(x))
    }
    case Type.Cst(TypeConstructor.CaseAll(_), _) => SetFormula.Cst(univ)
    case Type.Cst(TypeConstructor.CaseEmpty(_), _) => Empty
    case Type.Apply(Type.Cst(TypeConstructor.CaseComplement(_), _), tpe1, _) =>
      Not(fromCaseType(tpe1, m, univ))
    case Type.Apply(Type.Apply(Type.Cst(TypeConstructor.CaseIntersection(_), _), tpe1, _), tpe2, _) =>
      And(fromCaseType(tpe1, m, univ), fromCaseType(tpe2, m, univ))
    case Type.Apply(Type.Apply(Type.Cst(TypeConstructor.CaseUnion(_), _), tpe1, _), tpe2, _) =>
      Or(fromCaseType(tpe1, m, univ), fromCaseType(tpe2, m, univ))
    case _ => throw InternalCompilerException(s"Unexpected type: '$tpe'.", tpe.loc)
  }

  /**
    * Converts the given algebraic expression `f` back to a type under the given variable substitution map `m`.
    *
    * The map `m` must bind each free variable in `f` to a type variable.
    */
  def toCaseType(f: SetFormula, sym: Symbol.RestrictableEnumSym, m: Bimap[VarOrCase, Int], loc: SourceLocation): Type = f match {
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

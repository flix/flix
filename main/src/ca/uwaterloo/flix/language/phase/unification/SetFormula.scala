package ca.uwaterloo.flix.language.phase.unification

import ca.uwaterloo.flix.language.ast.{RigidityEnv, SourceLocation, Symbol, Type, TypeConstructor}
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
  def mkNot(f0: SetFormula)(implicit univ: Set[Int]): SetFormula = f0 match {
    case SetFormula.Cst(s) =>
      SetFormula.Cst(univ -- s)

    case SetFormula.Not(x) =>
      x

    case _ => SetFormula.Not(f0)
  }

  /**
    * Returns the conjunction of the two set formulas `tpe1` and `tpe2`.
    */
  def mkAnd(f1: SetFormula, f2: SetFormula)(implicit univ: Set[Int]): SetFormula = (f1, f2) match {
    case (SetFormula.Cst(x1), x2) if x1 == univ =>
      x2

    case (x1, SetFormula.Cst(x2)) if x2 == univ =>
      x1

    case (SetFormula.Cst(s1), _) if s1.isEmpty =>
      Empty

    case (_, SetFormula.Cst(s2)) if s2.isEmpty =>
      Empty

    case (SetFormula.Cst(x1), SetFormula.Cst(x2)) =>
      SetFormula.Cst(x1 & x2)

    case _ => SetFormula.And(f1, f2)
  }

  /**
    * Returns the disjunction of the two set formulas `tpe1` and `tpe2`.
    */
  def mkOr(f1: SetFormula, f2: SetFormula)(implicit univ: Set[Int]): SetFormula = (f1, f2) match {
    case (SetFormula.Cst(x1), _) if x1 == univ =>
      mkUni()

    case (_, SetFormula.Cst(x2)) if x2 == univ =>
      mkUni()

    case (SetFormula.Cst(s1), x2) if s1.isEmpty =>
      x2

    case (x1, SetFormula.Cst(s2)) if s2.isEmpty =>
      x1

    case (SetFormula.Cst(s1), SetFormula.Cst(s2)) =>
      SetFormula.Cst(s1 ++ s2)

    case _ => SetFormula.Or(f1, f2)
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

  /**
    * Returns a minimized type based on SetFormula minimization.
    */
  def minimizeType(tpe: Type, sym: Symbol.RestrictableEnumSym, univ: SortedSet[Symbol.RestrictableCaseSym], loc: SourceLocation): Type = {
    val (m, setFormulaUniv) = mkEnv(List(tpe), univ)
    val setFormula = fromCaseType(tpe, m, setFormulaUniv)
    val minimizedSetFormula = minimize(setFormula)(setFormulaUniv)
    toCaseType(minimizedSetFormula, sym, m, loc)
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
  def mkEnv(ts: List[Type], univ: SortedSet[Symbol.RestrictableCaseSym]): (Bimap[VarOrCase, Int], Set[Int]) = {
    val vars = ts.flatMap(_.typeVars).map(_.sym).distinct.map(VarOrCase.Var)
    val cases = (univ.toSet ++ ts.flatMap(_.cases)).map(VarOrCase.Case)
    // TODO RESTR-VARS do I even need the ts part here?

    val forward = (vars ++ cases).zipWithIndex.toMap[VarOrCase, Int]
    val backward = forward.map { case (a, b) => (b, a) }

    val newUniv = univ.map {
      case sym => forward(VarOrCase.Case(sym))
    }

    (Bimap(forward, backward), newUniv.toSet)
  }

  /**
    * Converts a rigidity environment to an equivalent environment for use with set formulas.
    */
  def liftRigidityEnv(renv: RigidityEnv, env: Bimap[VarOrCase, Int]): Set[Int] = {
    // We use flatmap because if a var is not in the env,
    // then it is not relevant to the types.
    renv.s.flatMap {
      case sym => env.getForward(VarOrCase.Var(sym))
    }
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
    case Type.Cst(TypeConstructor.CaseSet(syms, _), _) =>
      val cases = syms.toSet
        .map(VarOrCase.Case) // convert to case
        .map(m.getForward) // lookup in the map
        .map(_.getOrElse(throw InternalCompilerException(s"Unexpected unbound case", SourceLocation.Unknown)))

      Cst(cases)
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
  def toCaseType(f: SetFormula, enumSym: Symbol.RestrictableEnumSym, m: Bimap[VarOrCase, Int], loc: SourceLocation): Type = f match {
    case Cst(s) =>
      val syms = s
        .map(m.getBackward) // lookup in the map
        .map(_.getOrElse(throw InternalCompilerException("Unexpected unbound case set constant", loc)))
        .map(VarOrCase.getCase)
      Type.Cst(TypeConstructor.CaseSet(syms.to(SortedSet), enumSym), loc)
    case Var(x) => m.getBackward(x) match {
      case None => throw InternalCompilerException(s"Unexpected unbound variable: '$x'.", loc)
      case Some(VarOrCase.Var(sym)) => Type.Var(sym, loc)
      case Some(VarOrCase.Case(sym)) => Type.Cst(TypeConstructor.CaseSet(SortedSet(sym), enumSym), loc)
    }
    case Not(f1) => Type.mkCaseComplement(toCaseType(f1, enumSym, m, loc), enumSym, loc)
    case And(t1, t2) => Type.mkCaseIntersection(toCaseType(t1, enumSym, m, loc), toCaseType(t2, enumSym, m, loc), enumSym, loc)
    case Or(t1, t2) => Type.mkCaseUnion(toCaseType(t1, enumSym, m, loc), toCaseType(t2, enumSym, m, loc), enumSym, loc)
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

    /**
      * Extracts the sym from the case.
      */
    def getCase(x: VarOrCase): Symbol.RestrictableCaseSym = x match {
      case Var(_) => throw InternalCompilerException("unexpected var", SourceLocation.Unknown)
      case Case(sym) => sym
    }
  }

}

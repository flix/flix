package ca.uwaterloo.flix.language.phase.unification

import ca.uwaterloo.flix.language.ast.SourceLocation
import ca.uwaterloo.flix.util.InternalCompilerException

import scala.collection.immutable.SortedSet

/** assumes T has infinite values, i.e. Cst(...) != Top */
sealed trait InfSetFormula {

  final def freeVars: SortedSet[Int] = this match {
    case InfSetFormula.Top => SortedSet.empty
    case InfSetFormula.Cst(_) => SortedSet.empty
    case InfSetFormula.Var(x) => SortedSet(x)
    case InfSetFormula.Compl(f) => f.freeVars
    case InfSetFormula.Inter(f1, f2) => f1.freeVars ++ f2.freeVars
    case InfSetFormula.Union(f1, f2) => f1.freeVars ++ f2.freeVars
  }

  /**
    * The size is the number of unions and intersections
    */
  final def size: Int = this match {
    case InfSetFormula.Top => 0
    case InfSetFormula.Cst(_) => 0
    case InfSetFormula.Var(_) => 0
    case InfSetFormula.Compl(t) => t.size
    case InfSetFormula.Inter(t1, t2) => t1.size + t2.size + 1
    case InfSetFormula.Union(t1, t2) => t1.size + t2.size + 1
  }

  override def toString: String = this match {
    case InfSetFormula.Top => "T"
    case InfSetFormula.Cst(s) => s.map(_.toString).mkString("{", ", ", "}")
    case InfSetFormula.Var(x) => s"x$x"
    case InfSetFormula.Compl(f) => f match {
      case InfSetFormula.Var(x) => s"~x$x"
      case _ => s"~($f)"
    }
    case InfSetFormula.Inter(f1, f2) => s"($f1 ∩ $f2)"
    case InfSetFormula.Union(f1, f2) => s"($f1 ∪ $f2)"
  }

}

/** assumes T has infinite values, i.e. Cst(...) != Top */
object InfSetFormula {

  type T = Int

  case object Top extends InfSetFormula

  case class Cst(s: Set[T]) extends InfSetFormula

  case class Var(x: Int) extends InfSetFormula

  case class Compl(f: InfSetFormula) extends InfSetFormula

  case class Inter(f1: InfSetFormula, f2: InfSetFormula) extends InfSetFormula

  case class Union(f1: InfSetFormula, f2: InfSetFormula) extends InfSetFormula

  val Bot: InfSetFormula = Cst(Set.empty)

  def mkCompl(f0: InfSetFormula): InfSetFormula = f0 match {
    case Top => Bot
    case Bot => Top
    case Compl(x) => x
    case _ => Compl(f0)
  }

  def mkInter(f1: InfSetFormula, f2: InfSetFormula): InfSetFormula = (f1, f2) match {
    case (Top, _) => f2
    case (_, Top) => f1
    case (Bot, _) => Bot
    case (_, Bot) => Bot
    case (Cst(x1), Cst(x2)) => Cst(x1 intersect x2)
    case (x1, Compl(x2)) if x1 == x2 => Bot
    case (Compl(x1), x2) if x1 == x2 => Bot
    case (Inter(x1, y), x2) if x1 == x2 => mkInter(x1, y)
    case (Inter(y, x1), x2) if x1 == x2 => mkInter(y, x1)
    case _ if f1 == f2 => f1
    case _ => Inter(f1, f2)
  }

  def mkUnion(f1: InfSetFormula, f2: InfSetFormula): InfSetFormula = (f1, f2) match {
    case (Top, _) => Top
    case (_, Top) => Top
    case (Bot, _) => f2
    case (_, Bot) => f1
    case (Cst(s1), Cst(s2)) => Cst(s1 union s2)
    case (Compl(x1), x2) if x1 == x2 => Top
    case (x1, Compl(x2)) if x1 == x2 => Top
    case _ => Union(f1, f2)
  }

  def minimize(f: InfSetFormula): InfSetFormula = {
    if (f.freeVars.isEmpty) evaluateGround(f) else f
  }

  /**
    * The result is either `Top`, `Cst(_)`, `Compl(Cst(_))`
    */
  def evaluateGround(f: InfSetFormula, topVars: List[Int], botVars: List[Int]): InfSetFormula = f match {
    case Top => Top
    case Cst(s) => Cst(s)
    case Compl(ff) => ff match {
      case Top => Bot
      case Cst(s) => Compl(Cst(s))
      case Compl(Cst(s)) => Cst(s)
      case Compl(_) => ??? // impossible
      case Inter(_, _) => ??? // impossible
      case Union(_, _) => ??? // impossible
      case Var(_) => ??? // impossible
    }
    case Inter(f1, f2) => (evaluateGround(f1, topVars, botVars), evaluateGround(f2, topVars, botVars)) match {
      case (Top, Top) => Top
      case (Top, Cst(_)) => f2
      case (Top, Compl(Cst(_))) => f2
      case (Cst(_), Top) => f1
      case (Compl(Cst(_)), Top) => f1
      case (Cst(s1), Cst(s2)) => Cst(s1 intersect s2)
      case (Cst(s1), Compl(Cst(s2))) => Cst(s1 removedAll s2)
      case (Compl(Cst(s1)), Cst(s2)) => Cst(s2 removedAll s1)
      case (Compl(Cst(s1)), Compl(Cst(s2))) =>
        // !s1 ∩ !s1 =
        // !!(!s1 ∩ !s1) =
        // !(s1 ∪ s2)
        Compl(Cst(s1 union s2))
      case (_: Compl, _) => ??? // impossible
      case (_, _: Compl) => ??? // impossible
      case (_: Inter, _) => ??? // impossible
      case (_, _: Inter) => ??? // impossible
      case (_: Union, _) => ??? // impossible
      case (_, _: Union) => ??? // impossible
      case (_: Var, _) => ??? // impossible
      case (_, _: Var) => ??? // impossible
    }
    case Union(f1, f2) => (evaluateGround(f1, topVars, botVars), evaluateGround(f2, topVars, botVars)) match {
      case (Top, Top) => Top
      case (Top, Cst(_)) => Top
      case (Top, Compl(Cst(_))) => Top
      case (Cst(_), Top) => Top
      case (Compl(Cst(_)), Top) => Top
      case (Cst(s1), Cst(s2)) => Cst(s1 union s2)
      case (Cst(s1), Compl(Cst(s2))) =>
        // s1 ∪ !s2 =
        // !!(s1 ∪ !s2) =
        // !(!s1 ∩ s2) =
        // !(s2 - s1)
        Compl(Cst(s2 removedAll s1))
      case (Compl(Cst(s1)), Cst(s2)) => Compl(Cst(s1 removedAll s2))
      case (Compl(Cst(s1)), Compl(Cst(s2))) =>
        // !s1 ∪ !s2 =
        // !!(!s1 ∪ !s2) =
        // !(s1 ∩ s2)
        Compl(Cst(s1 intersect s2))
      case (_: Compl, _) => ??? // impossible
      case (_, _: Compl) => ??? // impossible
      case (_: Inter, _) => ??? // impossible
      case (_, _: Inter) => ??? // impossible
      case (_: Union, _) => ??? // impossible
      case (_, _: Union) => ??? // impossible
      case (_: Var, _) => ??? // impossible
      case (_, _: Var) => ??? // impossible
    }
    case Var(x) if topVars.contains(x) => Top
    case Var(x) if botVars.contains(x) => Bot
    case Var(_) => throw InternalCompilerException("Ungroup and Unassigned", SourceLocation.Unknown)
  }
  //  {
  //    // The minimized formula is constructed by adding each table row result to
  //    // its formula (`x ∧ y ∧ {Cst}`) and then creating a union of all the row
  //    // formulas. The table row result (always a constant set) is the original
  //    // formula where the row assignment is substituted in.
  //    table(f.freeVars.toList, bot, top).foldLeft(bot) {
  //      case (acc, (p, subst)) => mkUnion(acc, mkInter(p, applySubst(f, subst)))
  //    }
  //  }
  //
  //  /**
  //    * Returns all exponentially many assignments of `fvs` to top or bot,
  //    * together with their respective formula (see example). `bot` and `top`
  //    * should always be `Cst(Set.empty)` and `Cst(univ)` but are reused from
  //    * arguments for efficiency.
  //    *
  //    * Examples (the list order is not accurate):
  //    * {{{
  //    * table(x :: y :: Nil, bot, top) =
  //    *     ( x ∧  y, Map(x -> top, y -> top)) ::
  //    *     ( x ∧ ¬y, Map(x -> top, y -> bot)) ::
  //    *     (¬x ∧  y, Map(x -> bot, y -> top)) ::
  //    *     (¬x ∧ ¬y, Map(x -> bot, y -> bot)) ::
  //    *     Nil
  //    * }}}
  //    */
  //  private def table(fvs: List[Int], bot: InfSetFormula, top: InfSetFormula)(implicit univ: Set[Int]): List[(InfSetFormula, Map[Int, InfSetFormula])] = fvs match {
  //    case Nil => List((top, Map.empty))
  //    case v :: vs =>
  //      table(vs, bot, top) flatMap {
  //        case (p, partialSubst) =>
  //          val p1 = mkInter(InfSetFormula.Compl(InfSetFormula.Var(v)), p)
  //          val p2 = mkInter(InfSetFormula.Var(v), p)
  //          List((p1, partialSubst + (v -> bot)), (p2, partialSubst + (v -> top)))
  //      }
  //  }
  //
  //  /**
  //    * Returns a minimized type based on SetFormula2 minimization.
  //    */
  //  def minimizeType(tpe: Type, sym: Symbol.RestrictableEnumSym, univ: SortedSet[Symbol.RestrictableCaseSym], loc: SourceLocation): Type = {
  //    val (m, setFormulaUniv) = mkEnv(List(tpe), univ)
  //    val setFormula = fromCaseType(tpe, m, setFormulaUniv)
  //    val minimizedSetFormula2 = minimize(setFormula)(setFormulaUniv)
  //    toCaseType(minimizedSetFormula2, sym, m, loc)
  //  }
  //
  //  private def applySubst(f: InfSetFormula, m: Map[Int, InfSetFormula])(implicit univ: Set[Int]): InfSetFormula = InfSetFormula.map(f)(m)(univ)
  //
  //  /**
  //    * Substitutes all variables in `f` using the substitution map `m`.
  //    *
  //    * The map `m` must bind each free variable in `f` to a (new) variable.
  //    */
  //  def substitute(f: InfSetFormula, m: Map[Int, Int]): InfSetFormula = f match {
  //    case Cst(s) => Cst(s)
  //    case Var(x) => m.get(x) match {
  //      case None => throw InternalCompilerException(s"Unexpected unbound variable: 'x$x'.", SourceLocation.Unknown)
  //      case Some(y) => Var(y)
  //    }
  //    case Compl(f1) => Not(substitute(f1, m))
  //    case Inter(f1, f2) => And(substitute(f1, m), substitute(f2, m))
  //    case Union(f1, f2) => Or(substitute(f1, m), substitute(f2, m))
  //  }
  //
  //  /**
  //    * Runs the function `fn` on all the variables in the formula.
  //    */
  //  def map(f: InfSetFormula)(fn: Int => InfSetFormula)(implicit univ: Set[Int]): InfSetFormula = f match {
  //    case Cst(s) => Cst(s)
  //    case Var(x) => fn(x)
  //    case Compl(f1) => mkComplement(map(f1)(fn))
  //    case Inter(f1, f2) => mkInter(map(f1)(fn), map(f2)(fn))
  //    case Union(f1, f2) => mkUnion(map(f1)(fn), map(f2)(fn))
  //  }
  //
  //  /**
  //    * Creates an environment for mapping between proper types and formulas.
  //    */
  //  def mkEnv(ts: List[Type], univ: SortedSet[Symbol.RestrictableCaseSym]): (Bimap[VarOrCase, Int], Set[Int]) = {
  //    val vars = ts.flatMap(_.typeVars).map(_.sym).distinct.map(VarOrCase.Var)
  //    val cases = (univ.toSet ++ ts.flatMap(_.cases)).map(VarOrCase.Case)
  //    // TODO RESTR-VARS do I even need the ts part here?
  //
  //    val forward = (vars ++ cases).zipWithIndex.toMap[VarOrCase, Int]
  //    val backward = forward.map { case (a, b) => (b, a) }
  //
  //    val newUniv = univ.map {
  //      case sym => forward(VarOrCase.Case(sym))
  //    }
  //
  //    (Bimap(forward, backward), newUniv.toSet)
  //  }
  //
  //  /**
  //    * Converts a rigidity environment to an equivalent environment for use with set formulas.
  //    */
  //  def liftRigidityEnv(renv: RigidityEnv, env: Bimap[VarOrCase, Int]): Set[Int] = {
  //    // We use flatmap because if a var is not in the env,
  //    // then it is not relevant to the types.
  //    renv.s.flatMap {
  //      case sym => env.getForward(VarOrCase.Var(sym))
  //    }
  //  }
  //
  //  /**
  //    * Converts the given algebraic expression `f` back to a type under the given variable substitution map `m`.
  //    *
  //    * The map `m` must bind each free variable in `f` to a type variable.
  //    */
  //  def fromCaseType(tpe: Type, m: Bimap[VarOrCase, Int], univ: Set[Int]): InfSetFormula = tpe match {
  //    case Type.Var(sym, _) => m.getForward(VarOrCase.Var(sym)) match {
  //      case None => throw InternalCompilerException(s"Unexpected unbound variable: '$sym'.", sym.loc)
  //      case Some(x) => Var(x)
  //    }
  //    case Type.Cst(TypeConstructor.CaseSet(syms, _), _) =>
  //      val cases = syms.toSet
  //        .map(VarOrCase.Case) // convert to case
  //        .map(m.getForward) // lookup in the map
  //        .map(_.getOrElse(throw InternalCompilerException(s"Unexpected unbound case", SourceLocation.Unknown)))
  //
  //      Cst(cases)
  //    case Type.Apply(Type.Cst(TypeConstructor.CaseComplement(_), _), tpe1, _) =>
  //      Not(fromCaseType(tpe1, m, univ))
  //    case Type.Apply(Type.Apply(Type.Cst(TypeConstructor.CaseIntersection(_), _), tpe1, _), tpe2, _) =>
  //      And(fromCaseType(tpe1, m, univ), fromCaseType(tpe2, m, univ))
  //    case Type.Apply(Type.Apply(Type.Cst(TypeConstructor.CaseUnion(_), _), tpe1, _), tpe2, _) =>
  //      Or(fromCaseType(tpe1, m, univ), fromCaseType(tpe2, m, univ))
  //    case _ => throw InternalCompilerException(s"Unexpected type: '$tpe'.", tpe.loc)
  //  }
  //
  //  /**
  //    * Converts the given algebraic expression `f` back to a type under the given variable substitution map `m`.
  //    *
  //    * The map `m` must bind each free variable in `f` to a type variable.
  //    */
  //  def toCaseType(f: InfSetFormula, enumSym: Symbol.RestrictableEnumSym, m: Bimap[VarOrCase, Int], loc: SourceLocation): Type = f match {
  //    case Cst(s) =>
  //      val syms = s
  //        .map(m.getBackward) // lookup in the map
  //        .map(_.getOrElse(throw InternalCompilerException("Unexpected unbound case set constant", loc)))
  //        .map(VarOrCase.getCase)
  //      Type.Cst(TypeConstructor.CaseSet(syms.to(SortedSet), enumSym), loc)
  //    case Var(x) => m.getBackward(x) match {
  //      case None => throw InternalCompilerException(s"Unexpected unbound variable: '$x'.", loc)
  //      case Some(VarOrCase.Var(sym)) => Type.Var(sym, loc)
  //      case Some(VarOrCase.Case(sym)) => Type.Cst(TypeConstructor.CaseSet(SortedSet(sym), enumSym), loc)
  //    }
  //    case Compl(f1) => Type.mkCaseComplement(toCaseType(f1, enumSym, m, loc), enumSym, loc)
  //    case Inter(t1, t2) => Type.mkCaseIntersection(toCaseType(t1, enumSym, m, loc), toCaseType(t2, enumSym, m, loc), enumSym, loc)
  //    case Union(t1, t2) => Type.mkCaseUnion(toCaseType(t1, enumSym, m, loc), toCaseType(t2, enumSym, m, loc), enumSym, loc)
  //  }

}

package ca.uwaterloo.flix.language.phase.unification

import ca.uwaterloo.flix.language.ast.{Kind, RigidityEnv, SourceLocation, Symbol, Type, TypeConstructor, TypedAst}
import ca.uwaterloo.flix.util.InternalCompilerException
import ca.uwaterloo.flix.util.collection.Bimap

import scala.annotation.tailrec
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

  /**
    * Returns a minimized, equivalent formula of `f`.
    *
    * Currently works by building the exponential form based on [[table]]
    */
  def minimize(f: SetFormula)(implicit univ: Set[Int]): SetFormula = {
    val bot = SetFormula.Cst(Set.empty): SetFormula
    val top = SetFormula.Cst(univ): SetFormula

    // The minimized formula is constructed by adding each table row result to
    // its formula (`x ∧ y ∧ {Cst}`) and then creating a union of all the row
    // formulas. The table row result (always a constant set) is the original
    // formula where the row assignment is substituted in.
    table(f.freeVars.toList, bot, top).foldLeft(bot) {
      case (acc, (p, subst)) => mkOr(acc, mkAnd(p, applySubst(f, subst)))
    }
  }

  /**
    * Returns all exponentially many assignments of `fvs` to top or bot,
    * together with their respective formula (see example). `bot` and `top`
    * should always be `Cst(Set.empty)` and `Cst(univ)` but are reused from
    * arguments for efficiency.
    *
    * Examples (the list order is not accurate):
    * {{{
    * table(x :: y :: Nil, bot, top) =
    *     ( x ∧  y, Map(x -> top, y -> top)) ::
    *     ( x ∧ ¬y, Map(x -> top, y -> bot)) ::
    *     (¬x ∧  y, Map(x -> bot, y -> top)) ::
    *     (¬x ∧ ¬y, Map(x -> bot, y -> bot)) ::
    *     Nil
    * }}}
    */
  private def table(fvs: List[Int], bot: SetFormula, top: SetFormula)(implicit univ: Set[Int]): List[(SetFormula, Map[Int, SetFormula])] = fvs match {
    case Nil => List((top, Map.empty))
    case v :: vs =>
      table(vs, bot, top) flatMap {
        case (p, partialSubst) =>
          val p1 = mkAnd(SetFormula.Not(SetFormula.Var(v)), p)
          val p2 = mkAnd(SetFormula.Var(v), p)
          List((p1, partialSubst + (v -> bot)), (p2, partialSubst + (v -> top)))
      }
  }

  private def applySubst(f: SetFormula, m: Map[Int, SetFormula])(implicit univ: Set[Int]): SetFormula = SetFormula.map(f)(m)(univ)

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
    val vars = ts.flatMap(_.typeVars).map(_.sym).distinct.map(VarOrCase.Var.apply)
    val cases = (univ.toSet ++ ts.flatMap(_.cases)).map(VarOrCase.Case.apply)
    // TODO RESTR-VARS do I even need the ts part here?

    val forward = (vars ++ cases).zipWithIndex.toMap[VarOrCase, Int]
    val backward = forward.map { case (a, b) => (b, a) }

    val newUniv = univ.map {
      case sym => forward(VarOrCase.Case(sym))
    }

    (Bimap(forward, backward), newUniv)
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
        .map(VarOrCase.Case.apply) // convert to case
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

  /**
    * Returns the case set bounds of a restrictable enum or an empty string
    * otherwise.
    *
    * The case set bounds include what cases the set will always have and
    * cases the set can maximally have.
    * {{{
    * formatLowerAndUpperBounds(Expr[s ++ <Expr.Cst>][Int32]) =
    *     Lowerbound: {Cst}
    *     Upperbound: {Cst, Var, Not, And, Or, Xor}
    *
    * formatLowerAndUpperBounds(Expr[s1 -- s2 -- <Expr.Xor, Expr.And> ++ <Expr.Cst, Expr.Not>]) =
    *     Lowerbound: {Cst, Not}
    *     Upperbound: {Cst, Var, Not, Or}
    * }}}
    * The string is prefixed with a newline (if not empty).
    */
  def formatLowerAndUpperBounds(tpe: Type)(implicit root: TypedAst.Root): String = {
    "\n" + restrictableEnumBounds(tpe).getOrElse("")
  }

  /**
    * Returns the case set bounds of a restrictable enum. The case set bounds
    * include what cases the set will always have and what cases the set can
    * maximally have.
    * {{{
    * restrictableEnumBounds(Expr[s ++ <Expr.Cst>][Int32]) =
    *     Lowerbound: {Cst}
    *     Upperbound: {Cst, Var, Not, And, Or, Xor}
    *
    * restrictableEnumBounds(Expr[s1 -- s2 -- <Expr.Xor, Expr.And> ++ <Expr.Cst, Expr.Not>]) =
    *     Lowerbound: {Cst, Not}
    *     Upperbound: {Cst, Var, Not, Or}
    * }}}
    */
  private def restrictableEnumBounds(tpe: Type)(implicit root: TypedAst.Root): Option[String] = {
    for {
      (index, sym) <- findIndexOfEnum(tpe)
      emumDecl = root.restrictableEnums(sym)
      (lower, upper) <- boundsAnalysisType(index, emumDecl)
    } yield formatBounds(lower, upper)
  }

  /**
    * Extracts the index type (kind CaseSet) of a restrictable enum along
    * with its enumSym.
    * {{{
    * findIndexOfEnum(E[index: CaseSet(Color), Int32, String]) = Some((index, Color))
    * findIndexOfEnum(Int32) = None
    * findIndexOfEnum(Int32 -> E[index]) = None
    * }}}
    */
  @tailrec
  private def findIndexOfEnum(tpe: Type): Option[(Type, Symbol.RestrictableEnumSym)] = tpe match {
    case Type.Apply(Type.Cst(TypeConstructor.RestrictableEnum(_, _), _), index, _) =>
      index.kind match {
        case Kind.CaseSet(sym) => Some((index, sym))
        case _ => None
      }
    case Type.Apply(base, _, _) => findIndexOfEnum(base)
    case _ => None
  }

  /**
    * Returns `(lowerBound,upperBound)` if `tpe` is a case set, otherwise
    * returns `None`. The bounds are only in terms of constants, cases that must
    * always be present (lowerBound) and which cases that can maximally be
    * present (upperBound).
    * {{{
    * boundsAnalysisType(s ++ <Expr.Cst>) =
    *     Some(({Cst}, {Cst, Var, Not, And, Or, Xor}))
    *
    * boundsAnalysisType(s1 -- s2 -- <Expr.Xor, Expr.And> ++ <Expr.Cst, Expr.Not>) =
    *     Some(({Cst, Not}, {Cst, Var, Not, Or}))
    *
    * boundsAnalysisType(Int32) = None
    * }}}
    */
  private def boundsAnalysisType(tpe: Type, enm: TypedAst.RestrictableEnum): Option[(Set[Symbol.RestrictableCaseSym], Set[Symbol.RestrictableCaseSym])] = {
    tpe.kind match {
      case Kind.CaseSet(_) =>
        val (m, setFormulaUniv) = mkEnv(List(tpe), SortedSet.from(enm.cases.keys))
        val setFormula = fromCaseType(tpe, m, setFormulaUniv)
        val (lower, upper) = boundsAnalysis(setFormula)(setFormulaUniv)
        Some(cstToType(lower, m), cstToType(upper, m))
      case _ => None
    }
  }

  /**
    * Converts a [[SetFormula]] set into a [[Symbol.RestrictableCaseSym]] set by
    * the mappings in `m`.
    */
  private def cstToType(cst: Set[Int], m: Bimap[VarOrCase, Int]): Set[Symbol.RestrictableCaseSym] = {
    cst.map(c => m.getBackward(c) match {
      case Some(VarOrCase.Var(_)) => throw InternalCompilerException("Unexpected case set var in constant", SourceLocation.Unknown)
      case Some(VarOrCase.Case(sym)) => sym
      case None => throw InternalCompilerException("Unexpected empty case set constant mapping", SourceLocation.Unknown)
    })
  }

  /**
    * Returns `(lowerBound,upperBound)`. The bounds are only in terms of
    * constants, cases that must always be present (lowerBound) and which cases
    * that can maximally be present (upperBound).
    * {{{
    * boundsAnalysisType(s or Cst{1}) =
    *     ({1}, {1, 2, ..., n})
    *
    * boundsAnalysisType((s1 and  s2 and Cst{4, 6}) or Cst{1, 3}) =
    *     ({1, 3}, {1, 2, 3, 5, 7, 8, ..., n})
    * }}}
    */
  private def boundsAnalysis(f: SetFormula)(implicit univ: Set[Int]): (Set[Int], Set[Int]) = {
    val bot = SetFormula.Cst(Set.empty): SetFormula
    val top = SetFormula.Cst(univ): SetFormula

    // forcefully extracts a set formula of constant form
    def extractCst(form: SetFormula): Set[Int] = form match {
      case Cst(s) => s
      case _ => throw InternalCompilerException("Unexpected non-evalued formula", SourceLocation.Unknown)
    }

    val assignmentResults = table(f.freeVars.toList, bot, top).map{case (_, assignment) => extractCst(applySubst(f, assignment))}
    // intersect all possible results to get a lower bound
    val minimum = assignmentResults.reduceOption(_.intersect(_)).getOrElse(extractCst(applySubst(f, Map.empty)))
    // union all possible results to get an upper bound
    val maximum = assignmentResults.reduceOption(_.union(_)).getOrElse(extractCst(applySubst(f, Map.empty)))
    (minimum, maximum)
  }

  /**
    * Returns a string like:
    * {{{
    * Lowerbound: 'nice print of lowerBounds'
    * Upperbound: 'nice print of upperBound'
    * }}}
    * Only the simple names of symbols are used, no namespace info.
    */
  private def formatBounds(lowerBound: Set[Symbol.RestrictableCaseSym], upperBound: Set[Symbol.RestrictableCaseSym]): String = {
    val minStr = lowerBound.map(_.name).mkString("{", ", ", "}")
    val maxStr = upperBound.map(_.name).mkString("{", ", ", "}")
    s"Lowerbound: $minStr\nUpperbound: $maxStr\n"
  }

}

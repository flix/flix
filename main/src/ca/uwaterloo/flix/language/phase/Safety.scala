package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.CompilationError
import ca.uwaterloo.flix.language.ast.Ast.Polarity
import ca.uwaterloo.flix.language.ast.TypedAst._
import ca.uwaterloo.flix.language.ast.ops.TypedAstOps._
import ca.uwaterloo.flix.language.ast.{SourceLocation, Symbol, TypedAst}
import ca.uwaterloo.flix.language.errors.SafetyError
import ca.uwaterloo.flix.util.Validation
import ca.uwaterloo.flix.util.Validation._

/**
  * Performs safety and well-formedness.
  */
object Safety extends Phase[Root, Root] {

  /**
    * Performs safety and well-formedness checks on the given AST `root`.
    */
  def run(root: Root)(implicit flix: Flix): Validation[Root, CompilationError] = flix.phase("Safety") {
    //
    // Collect all errors.
    //
    val errors = root.defs.flatMap {
      case (sym, defn) => visitDef(defn)
    }

    //
    // Check if any errors were detected.
    //
    if (errors.isEmpty)
      root.toSuccess
    else
      Validation.Failure(errors.to(LazyList))
  }

  /**
    * Performs safety and well-formedness checks on the given definition `def0`.
    */
  private def visitDef(def0: TypedAst.Def): List[CompilationError] = visitExp(def0.impl.exp)

  /**
    * Performs safety and well-formedness checks on the given expression `exp0`.
    */
  private def visitExp(exp0: Expression): List[CompilationError] = exp0 match {
    case Expression.Unit(loc) => Nil

    case Expression.Null(tpe, loc) => Nil

    case Expression.True(loc) => Nil

    case Expression.False(loc) => Nil

    case Expression.Char(lit, loc) => Nil

    case Expression.Float32(lit, loc) => Nil

    case Expression.Float64(lit, loc) => Nil

    case Expression.Int8(lit, loc) => Nil

    case Expression.Int16(lit, loc) => Nil

    case Expression.Int32(lit, loc) => Nil

    case Expression.Int64(lit, loc) => Nil

    case Expression.BigInt(lit, loc) => Nil

    case Expression.Str(lit, loc) => Nil

    case Expression.Default(tpe, loc) => Nil

    case Expression.Wild(tpe, loc) => Nil

    case Expression.Var(sym, tpe, loc) => Nil

    case Expression.Def(sym, tpe, loc) => Nil

    case Expression.Sig(sym, tpe, loc) => Nil

    case Expression.Hole(sym, tpe, eff, loc) => Nil

    case Expression.Lambda(fparam, exp, tpe, loc) => visitExp(exp)

    case Expression.Apply(exp, exps, tpe, eff, loc) =>
      val init = visitExp(exp)
      exps.foldLeft(init) {
        case (acc, exp) => acc ::: visitExp(exp)
      }

    case Expression.Unary(sop, exp, tpe, eff, loc) => visitExp(exp)

    case Expression.Binary(sop, exp1, exp2, tpe, eff, loc) => visitExp(exp1) ::: visitExp(exp2)

    case Expression.Let(sym, exp1, exp2, tpe, eff, loc) => visitExp(exp1) ::: visitExp(exp2)

    case Expression.IfThenElse(exp1, exp2, exp3, tpe, eff, loc) => visitExp(exp1) ::: visitExp(exp2) ::: visitExp(exp3)

    case Expression.Stm(exp1, exp2, tpe, eff, loc) => visitExp(exp1) ::: visitExp(exp2)

    case Expression.Match(exp, rules, tpe, eff, loc) =>
      rules.foldLeft(visitExp(exp)) {
        case (acc, MatchRule(p, g, e)) => acc ::: visitExp(g) ::: visitExp(e)
      }

    case Expression.Choose(exps, rules, tpe, eff, loc) =>
      exps.flatMap(visitExp) ++ rules.flatMap {
        case ChoiceRule(_, exp) => visitExp(exp)
      }

    case Expression.Tag(sym, tag, exp, tpe, eff, loc) => visitExp(exp)

    case Expression.Tuple(elms, tpe, eff, loc) =>
      elms.foldLeft(Nil: List[CompilationError]) {
        case (acc, e) => acc ::: visitExp(e)
      }

    case Expression.RecordEmpty(tpe, loc) => Nil

    case Expression.RecordSelect(exp, _, tpe, eff, loc) =>
      visitExp(exp)

    case Expression.RecordExtend(_, value, rest, tpe, eff, loc) =>
      visitExp(value) ::: visitExp(rest)

    case Expression.RecordRestrict(_, rest, tpe, eff, loc) =>
      visitExp(rest)

    case Expression.ArrayLit(elms, tpe, eff, loc) =>
      elms.foldLeft(Nil: List[CompilationError]) {
        case (acc, e) => acc ::: visitExp(e)
      }

    case Expression.ArrayNew(elm, len, tpe, eff, loc) => visitExp(elm)

    case Expression.ArrayLoad(base, index, tpe, eff, loc) => visitExp(base) ::: visitExp(index)

    case Expression.ArrayLength(base, eff, loc) => visitExp(base)

    case Expression.ArrayStore(base, index, elm, loc) => visitExp(base) ::: visitExp(index) ::: visitExp(elm)

    case Expression.ArraySlice(base, beginIndex, endIndex, tpe, loc) => visitExp(base) ::: visitExp(beginIndex) ::: visitExp(endIndex)

    case Expression.Ref(exp, tpe, eff, loc) => visitExp(exp)

    case Expression.Deref(exp, tpe, eff, loc) => visitExp(exp)

    case Expression.Assign(exp1, exp2, tpe, eff, loc) => visitExp(exp1) ::: visitExp(exp2)

    case Expression.Existential(fparam, exp, loc) => visitExp(exp)

    case Expression.Universal(fparam, exp, loc) => visitExp(exp)

    case Expression.Ascribe(exp, tpe, eff, loc) => visitExp(exp)

    case Expression.Cast(exp, tpe, eff, loc) => visitExp(exp)

    case Expression.TryCatch(exp, rules, tpe, eff, loc) =>
      rules.foldLeft(visitExp(exp)) {
        case (acc, CatchRule(_, _, e)) => acc ::: visitExp(e)
      }

    case Expression.InvokeConstructor(constructor, args, tpe, eff, loc) =>
      args.foldLeft(Nil: List[CompilationError]) {
        case (acc, e) => acc ::: visitExp(e)
      }

    case Expression.InvokeMethod(method, exp, args, tpe, eff, loc) =>
      args.foldLeft(visitExp(exp)) {
        case (acc, e) => acc ::: visitExp(e)
      }

    case Expression.InvokeStaticMethod(method, args, tpe, eff, loc) =>
      args.foldLeft(Nil: List[CompilationError]) {
        case (acc, e) => acc ::: visitExp(e)
      }

    case Expression.GetField(field, exp, tpe, eff, loc) =>
      visitExp(exp)

    case Expression.PutField(field, exp1, exp2, tpe, eff, loc) =>
      visitExp(exp1) ::: visitExp(exp2)

    case Expression.GetStaticField(field, tpe, eff, loc) =>
      Nil

    case Expression.PutStaticField(field, exp, tpe, eff, loc) =>
      visitExp(exp)

    case Expression.NewChannel(exp, tpe, eff, loc) => visitExp(exp)

    case Expression.GetChannel(exp, tpe, eff, loc) => visitExp(exp)

    case Expression.PutChannel(exp1, exp2, tpe, eff, loc) => visitExp(exp1) ::: visitExp(exp2)

    case Expression.SelectChannel(rules, default, tpe, eff, loc) =>
      val rs = rules.foldLeft(Nil: List[CompilationError]) {
        case (acc, SelectChannelRule(sym, chan, body)) => acc ::: visitExp(chan) ::: visitExp(body)
      }

      val d = default.map(visitExp).getOrElse(Nil)

      rs ++ d

    case Expression.Spawn(exp, tpe, eff, loc) => visitExp(exp)

    case Expression.Lazy(exp, tpe, loc) => visitExp(exp)

    case Expression.Force(exp, tpe, eff, loc) => visitExp(exp)

    case Expression.FixpointConstraintSet(cs, stf, tpe, loc) => cs.flatMap(checkConstraint)

    case Expression.FixpointCompose(exp1, exp2, stf, tpe, eff, loc) => visitExp(exp1) ::: visitExp(exp2)

    case Expression.FixpointSolve(exp, stf, tpe, eff, loc) => visitExp(exp)

    case Expression.FixpointProject(pred, exp, tpe, eff, loc) => visitExp(exp)

    case Expression.FixpointEntails(exp1, exp2, tpe, eff, loc) => visitExp(exp1) ::: visitExp(exp2)

  }

  /**
    * Performs safety and well-formedness checks on the given constraint `c0`.
    */
  private def checkConstraint(c0: Constraint): List[CompilationError] = {
    //
    // Compute the set of positively defined variable symbols in the constraint.
    //
    val posVars = positivelyDefinedVariables(c0)

    //
    // Compute the quantified variables in the constraint.
    //
    // A lexically bound variable does not appear in this set and is never free.
    //
    val quantVars = c0.cparams.map(_.sym).toSet

    //
    // Check that all negative atoms only use positively defined variable symbols.
    //
    c0.body.flatMap(checkBodyPredicate(_, posVars, quantVars))
  }

  /**
    * Performs safety and well-formedness checks on the given body predicate `p0`
    * with the given positively defined variable symbols `posVars`.
    */
  private def checkBodyPredicate(p0: Predicate.Body, posVars: Set[Symbol.VarSym], quantVars: Set[Symbol.VarSym]): List[CompilationError] = p0 match {
    case Predicate.Body.Atom(pred, den, polarity, terms, tpe, loc) =>
      checkBodyAtomPredicate(polarity, terms, posVars, quantVars, loc)

    case Predicate.Body.Guard(exp, loc) => visitExp(exp)
  }

  /**
    * Performs safety and well-formedness checks on an atom with the given polarity, terms, and positive variables.
    */
  private def checkBodyAtomPredicate(polarity: Polarity, terms: List[TypedAst.Pattern], posVars: Set[Symbol.VarSym], quantVars: Set[Symbol.VarSym], loc: SourceLocation): List[CompilationError] = {
    polarity match {
      case Polarity.Positive => Nil
      case Polarity.Negative =>
        // Compute the free variables in the terms which are *not* bound by the lexical scope.
        val freeVars = terms.flatMap(freeVarsOf).toSet intersect quantVars

        // Check if any free variables are not positively bound.
        ((freeVars -- posVars) map {
          case unboundVar => SafetyError.IllegalNonPositivelyBoundVariable(unboundVar, loc)
        }).toList
    }
  }

  /**
    * Returns all the positively defined variable symbols in the given constraint `c0`.
    */
  private def positivelyDefinedVariables(c0: Constraint): Set[Symbol.VarSym] = c0.body.flatMap(positivelyDefinedVariables).toSet

  /**
    * Returns all positively defined variable symbols in the given body predicate `p0`.
    */
  private def positivelyDefinedVariables(p0: Predicate.Body): Set[Symbol.VarSym] = p0 match {
    case Predicate.Body.Atom(pred, den, polarity, terms, tpe, loc) => polarity match {
      case Polarity.Positive =>
        // Case 1: A positive atom positively defines all its free variables.
        terms.flatMap(freeVarsOf).toSet
      case Polarity.Negative =>
        // Case 2: A negative atom does not positively define any variables.
        Set.empty
    }

    case Predicate.Body.Guard(exp, loc) => Set.empty
  }

}

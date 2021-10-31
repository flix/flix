package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.CompilationMessage
import ca.uwaterloo.flix.language.ast.Ast.Polarity
import ca.uwaterloo.flix.language.ast.TypedAst._
import ca.uwaterloo.flix.language.ast.ops.TypedAstOps._
import ca.uwaterloo.flix.language.ast.{SourceLocation, Symbol, TypedAst}
import ca.uwaterloo.flix.language.errors.SafetyError
import ca.uwaterloo.flix.language.errors.SafetyError._
import ca.uwaterloo.flix.util.Validation
import ca.uwaterloo.flix.util.Validation._

import scala.annotation.tailrec

/**
  * Performs safety and well-formedness.
  */
object Safety extends Phase[Root, Root] {

  /**
    * Performs safety and well-formedness checks on the given AST `root`.
    */
  def run(root: Root)(implicit flix: Flix): Validation[Root, CompilationMessage] = flix.phase("Safety") {
    //
    // Collect all errors.
    //
    val errors = root.defs.flatMap {
      case (_, defn) => visitDef(defn)
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
  private def visitDef(def0: TypedAst.Def): List[CompilationMessage] = visitExp(def0.impl.exp)

  /**
    * Performs safety and well-formedness checks on the given expression `exp0`.
    */
  private def visitExp(exp0: Expression): List[CompilationMessage] = exp0 match {
    case Expression.Unit(_) => Nil

    case Expression.Null(_, _) => Nil

    case Expression.True(_) => Nil

    case Expression.False(_) => Nil

    case Expression.Char(_, _) => Nil

    case Expression.Float32(_, _) => Nil

    case Expression.Float64(_, _) => Nil

    case Expression.Int8(_, _) => Nil

    case Expression.Int16(_, _) => Nil

    case Expression.Int32(_, _) => Nil

    case Expression.Int64(_, _) => Nil

    case Expression.BigInt(_, _) => Nil

    case Expression.Str(_, _) => Nil

    case Expression.Default(_, _) => Nil

    case Expression.Wild(_, _) => Nil

    case Expression.Var(_, _, _) => Nil

    case Expression.Def(_, _, _) => Nil

    case Expression.Sig(_, _, _) => Nil

    case Expression.Hole(_, _, _, _) => Nil

    case Expression.Lambda(_, exp, _, _) =>
      visitExp(exp)

    case Expression.Apply(exp, exps, _, _, _) =>
      visitExp(exp) ::: exps.flatMap(visitExp)

    case Expression.Unary(_, exp, _, _, _) =>
      visitExp(exp)

    case Expression.Binary(_, exp1, exp2, _, _, _) =>
      visitExp(exp1) ::: visitExp(exp2)

    case Expression.Let(_, _, exp1, exp2, _, _, _) =>
      visitExp(exp1) ::: visitExp(exp2)

    case Expression.LetRegion(_, exp, _, _, _) =>
      visitExp(exp)

    case Expression.IfThenElse(exp1, exp2, exp3, _, _, _) =>
      visitExp(exp1) ::: visitExp(exp2) ::: visitExp(exp3)

    case Expression.Stm(exp1, exp2, _, _, _) =>
      visitExp(exp1) ::: visitExp(exp2)

    case Expression.Match(exp, rules, _, _, _) =>
      visitExp(exp) :::
        rules.flatMap { case MatchRule(_, g, e) => visitExp(g) ::: visitExp(e) }

    case Expression.Choose(exps, rules, _, _, _) =>
      exps.flatMap(visitExp) :::
        rules.flatMap { case ChoiceRule(_, exp) => visitExp(exp) }

    case Expression.Tag(_, _, exp, _, _, _) =>
      visitExp(exp)

    case Expression.Tuple(elms, _, _, _) =>
      elms.flatMap(visitExp)

    case Expression.RecordEmpty(_, _) => Nil

    case Expression.RecordSelect(exp, _, _, _, _) =>
      visitExp(exp)

    case Expression.RecordExtend(_, value, rest, _, _, _) =>
      visitExp(value) ::: visitExp(rest)

    case Expression.RecordRestrict(_, rest, _, _, _) =>
      visitExp(rest)

    case Expression.ArrayLit(elms, _, _, _) =>
      elms.flatMap(visitExp)

    case Expression.ArrayNew(elm, _, _, _, _) =>
      visitExp(elm)

    case Expression.ArrayLoad(base, index, _, _, _) =>
      visitExp(base) ::: visitExp(index)

    case Expression.ArrayLength(base, _, _) =>
      visitExp(base)

    case Expression.ArrayStore(base, index, elm, _) =>
      visitExp(base) ::: visitExp(index) ::: visitExp(elm)

    case Expression.ArraySlice(base, beginIndex, endIndex, _, _) =>
      visitExp(base) ::: visitExp(beginIndex) ::: visitExp(endIndex)

    case Expression.Ref(exp, _, _, _) =>
      visitExp(exp)

    case Expression.Deref(exp, _, _, _) =>
      visitExp(exp)

    case Expression.Assign(exp1, exp2, _, _, _) =>
      visitExp(exp1) ::: visitExp(exp2)

    case Expression.Existential(_, exp, _) =>
      visitExp(exp)

    case Expression.Universal(_, exp, _) =>
      visitExp(exp)

    case Expression.Ascribe(exp, _, _, _) =>
      visitExp(exp)

    case Expression.Cast(exp, _, _, _) =>
      visitExp(exp)

    case Expression.TryCatch(exp, rules, _, _, _) =>
      visitExp(exp) :::
        rules.flatMap { case CatchRule(_, _, e) => visitExp(e) }

    case Expression.InvokeConstructor(_, args, _, _, _) =>
      args.flatMap(visitExp)

    case Expression.InvokeMethod(_, exp, args, _, _, _) =>
      visitExp(exp) ::: args.flatMap(visitExp)

    case Expression.InvokeStaticMethod(_, args, _, _, _) =>
      args.flatMap(visitExp)

    case Expression.GetField(_, exp, _, _, _) =>
      visitExp(exp)

    case Expression.PutField(_, exp1, exp2, _, _, _) =>
      visitExp(exp1) ::: visitExp(exp2)

    case Expression.GetStaticField(_, _, _, _) =>
      Nil

    case Expression.PutStaticField(_, exp, _, _, _) =>
      visitExp(exp)

    case Expression.NewChannel(exp, _, _, _) =>
      visitExp(exp)

    case Expression.GetChannel(exp, _, _, _) =>
      visitExp(exp)

    case Expression.PutChannel(exp1, exp2, _, _, _) =>
      visitExp(exp1) ::: visitExp(exp2)

    case Expression.SelectChannel(rules, default, _, _, _) =>
      rules.flatMap { case SelectChannelRule(_, chan, body) => visitExp(chan) :::
        visitExp(body)
      } :::
        default.map(visitExp).getOrElse(Nil)

    case Expression.Spawn(exp, _, _, _) =>
      visitExp(exp)

    case Expression.Lazy(exp, _, _) =>
      visitExp(exp)

    case Expression.Force(exp, _, _, _) =>
      visitExp(exp)

    case Expression.FixpointConstraintSet(cs, _, _, _) =>
      cs.flatMap(checkConstraint)

    case Expression.FixpointMerge(exp1, exp2, _, _, _, _) =>
      visitExp(exp1) ::: visitExp(exp2)

    case Expression.FixpointSolve(exp, _, _, _, _) =>
      visitExp(exp)

    case Expression.FixpointFilter(_, exp, _, _, _) =>
      visitExp(exp)

    case Expression.FixpointProjectIn(exp, _, _, _, _) =>
      visitExp(exp)

    case Expression.FixpointProjectOut(_, exp, _, _, _) =>
      visitExp(exp)

    case Expression.Reify(_, _, _, _) => Nil

    case Expression.ReifyType(_, _, _, _, _) => Nil

  }

  /**
    * Performs safety and well-formedness checks on the given constraint `c0`.
    */
  private def checkConstraint(c0: Constraint): List[CompilationMessage] = {
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
  private def checkBodyPredicate(p0: Predicate.Body, posVars: Set[Symbol.VarSym], quantVars: Set[Symbol.VarSym]): List[CompilationMessage] = p0 match {
    case Predicate.Body.Atom(_, _, polarity, terms, _, loc) =>
      checkBodyAtomPredicate(polarity, terms, posVars, quantVars, loc)

    case Predicate.Body.Guard(exp, _) => visitExp(exp)
  }

  /**
    * Creates an error for a non-positively bound variable, dependent on `sym.isWild`.
    *
    * @param loc the location of the atom containing the terms.
    */
  private def makeIllegalNonPositivelyBoundVariableError(sym: Symbol.VarSym, loc: SourceLocation): SafetyError =
    if (sym.isWild) IllegalNegativelyBoundWildVariable(sym, loc) else IllegalNonPositivelyBoundVariable(sym, loc)

  /**
    * Performs safety and well-formedness checks on an atom with the given polarity, terms, and positive variables.
    */
  private def checkBodyAtomPredicate(polarity: Polarity, terms: List[TypedAst.Pattern], posVars: Set[Symbol.VarSym], quantVars: Set[Symbol.VarSym], loc: SourceLocation): List[CompilationMessage] = {
    polarity match {
      case Polarity.Positive => Nil
      case Polarity.Negative =>
        // Compute the free variables in the terms which are *not* bound by the lexical scope.
        val freeVars = terms.flatMap(freeVarsOf).toSet intersect quantVars
        val wildcardNegErrors = visitPats(terms, loc)

        // Check if any free variables are not positively bound.
        val variableNegErrors = ((freeVars -- posVars) map (makeIllegalNonPositivelyBoundVariableError(_, loc))).toList
        wildcardNegErrors ++ variableNegErrors
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
    case Predicate.Body.Atom(_, _, polarity, terms, _, _) => polarity match {
      case Polarity.Positive =>
        // Case 1: A positive atom positively defines all its free variables.
        terms.flatMap(freeVarsOf).toSet
      case Polarity.Negative =>
        // Case 2: A negative atom does not positively define any variables.
        Set.empty
    }

    case Predicate.Body.Guard(_, _) => Set.empty
  }

  /**
    * Returns an error for each occurrence of wildcards in each term.
    *
    * @param loc the location of the atom containing the terms.
    */
  private def visitPats(terms: List[TypedAst.Pattern], loc: SourceLocation): List[CompilationMessage] = {
    terms.flatMap(visitPat(_, loc))
  }

  /**
    * Returns an error for each occurrence of wildcards.
    *
    * @param loc the location of the atom containing the term.
    */
  @tailrec
  private def visitPat(term: TypedAst.Pattern, loc: SourceLocation): List[CompilationMessage] = term match {
    case Pattern.Wild(_, _) => List(IllegalNegativelyBoundWildcard(loc))
    case Pattern.Var(_, _, _) => Nil
    case Pattern.Unit(_) => Nil
    case Pattern.True(_) => Nil
    case Pattern.False(_) => Nil
    case Pattern.Char(_, _) => Nil
    case Pattern.Float32(_, _) => Nil
    case Pattern.Float64(_, _) => Nil
    case Pattern.Int8(_, _) => Nil
    case Pattern.Int16(_, _) => Nil
    case Pattern.Int32(_, _) => Nil
    case Pattern.Int64(_, _) => Nil
    case Pattern.BigInt(_, _) => Nil
    case Pattern.Str(_, _) => Nil
    case Pattern.Tag(_, _, pat, _, _) => visitPat(pat, loc)
    case Pattern.Tuple(elms, _, _) => visitPats(elms, loc)
    case Pattern.Array(elms, _, _) => visitPats(elms, loc)
    case Pattern.ArrayTailSpread(elms, _, _, _) => visitPats(elms, loc)
    case Pattern.ArrayHeadSpread(_, elms, _, _) => visitPats(elms, loc)
  }

}

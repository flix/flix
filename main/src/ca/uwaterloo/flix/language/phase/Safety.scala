package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.CompilationError
import ca.uwaterloo.flix.language.ast.Ast.Polarity
import ca.uwaterloo.flix.language.ast.{SourceLocation, Symbol, TypedAst}
import ca.uwaterloo.flix.language.ast.TypedAst._
import ca.uwaterloo.flix.language.ast.ops.TypedAstOps._
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
      Validation.Failure(errors.toStream)
  }

  /**
    * Performs safety and well-formedness checks on the given definition `def0`.
    */
  private def visitDef(def0: TypedAst.Def): List[CompilationError] = visitExp(def0.exp)

  /**
    * Performs safety and well-formedness checks on the given expression `exp0`.
    */
  private def visitExp(exp0: Expression): List[CompilationError] = exp0 match {
    case Expression.Unit(loc) => Nil
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

    case Expression.Wild(tpe, eff, loc) => Nil

    case Expression.Var(sym, tpe, eff, loc) => Nil

    case Expression.Def(sym, tpe, eff, loc) => Nil

    case Expression.Eff(sym, tpe, eff, loc) => Nil

    case Expression.Hole(sym, tpe, eff, loc) => Nil

    case Expression.Lambda(fparam, exp, tpe, eff, loc) => visitExp(exp)

    case Expression.Apply(exp1, exp2, tpe, eff, loc) => visitExp(exp1) ::: visitExp(exp2)

    case Expression.Unary(op, exp, tpe, eff, loc) => visitExp(exp)

    case Expression.Binary(op, exp1, exp2, tpe, eff, loc) => visitExp(exp1) ::: visitExp(exp2)

    case Expression.Let(sym, exp1, exp2, tpe, eff, loc) => visitExp(exp1) ::: visitExp(exp2)

    case Expression.LetRec(sym, exp1, exp2, tpe, eff, loc) => visitExp(exp1) ::: visitExp(exp2)

    case Expression.IfThenElse(exp1, exp2, exp3, tpe, eff, loc) => visitExp(exp1) ::: visitExp(exp2) ::: visitExp(exp3)

    case Expression.Stm(exp1, exp2, tpe, eff, loc) => visitExp(exp1) ::: visitExp(exp2)

    case Expression.Match(exp, rules, tpe, eff, loc) =>
      rules.foldLeft(visitExp(exp)) {
        case (acc, MatchRule(p, g, e)) => acc ::: visitExp(g) ::: visitExp(e)
      }

    case Expression.Switch(rules, tpe, eff, loc) =>
      rules.foldLeft(Nil: List[CompilationError]) {
        case (acc, (e1, e2)) => acc ::: visitExp(e1) ::: visitExp(e2)
      }

    case Expression.Tag(sym, tag, exp, tpe, eff, loc) => visitExp(exp)

    case Expression.Tuple(elms, tpe, eff, loc) =>
      elms.foldLeft(Nil: List[CompilationError]) {
        case (acc, e) => acc ::: visitExp(e)
      }

    case Expression.RecordEmpty(tpe, eff, loc) => Nil

    case Expression.RecordSelect(exp, label, tpe, eff, loc) =>
      visitExp(exp)

    case Expression.RecordExtend(label, value, rest, tpe, eff, loc) =>
      visitExp(value) ::: visitExp(rest)

    case Expression.RecordRestrict(label, rest, tpe, eff, loc) =>
      visitExp(rest)

    case Expression.ArrayLit(elms, tpe, eff, loc) =>
      elms.foldLeft(Nil: List[CompilationError]) {
        case (acc, e) => acc ::: visitExp(e)
      }

    case Expression.ArrayNew(elm, len, tpe, eff, loc) => visitExp(elm)

    case Expression.ArrayLoad(base, index, tpe, eff, loc) => visitExp(base) ::: visitExp(index)

    case Expression.ArrayLength(base, tpe, eff, loc) => visitExp(base)

    case Expression.ArrayStore(base, index, elm, tpe, eff, loc) => visitExp(base) ::: visitExp(index) ::: visitExp(elm)

    case Expression.ArraySlice(base, beginIndex, endIndex, tpe, eff, loc) => visitExp(base) ::: visitExp(beginIndex) ::: visitExp(endIndex)

    case Expression.VectorLit(elms, tpe, eff, loc) =>
      elms.foldLeft(Nil: List[CompilationError]) {
        case (acc, e) => acc ::: visitExp(e)
      }

    case Expression.VectorNew(elm, len, tpe, eff, loc) => visitExp(elm)

    case Expression.VectorLoad(base, index, tpe, eff, loc) => visitExp(base)

    case Expression.VectorStore(base, index, elm, tpe, eff, loc) => visitExp(base) ::: visitExp(elm)

    case Expression.VectorLength(base, tpe, eff, loc) => visitExp(base)

    case Expression.VectorSlice(base, startIndex, endIndex, tpe, eff, loc) => visitExp(base)

    case Expression.Ref(exp, tpe, eff, loc) => visitExp(exp)

    case Expression.Deref(exp, tpe, eff, loc) => visitExp(exp)

    case Expression.Assign(exp1, exp2, tpe, eff, loc) => visitExp(exp1) ::: visitExp(exp2)

    case Expression.HandleWith(exp, bindings, tpe, eff, loc) =>
      bindings.foldLeft(visitExp(exp)) {
        case (acc, HandlerBinding(_, e)) => acc ::: visitExp(e)
      }

    case Expression.Existential(fparam, exp, eff, loc) => visitExp(exp)

    case Expression.Universal(fparam, exp, eff, loc) => visitExp(exp)

    case Expression.Ascribe(exp, tpe, eff, loc) => visitExp(exp)

    case Expression.Cast(exp, tpe, eff, loc) => visitExp(exp)

    case Expression.NativeConstructor(constructor, args, tpe, eff, loc) =>
      args.foldLeft(Nil: List[CompilationError]) {
        case (acc, e) => acc ::: visitExp(e)
      }

    case Expression.TryCatch(exp, rules, tpe, eff, loc) =>
      rules.foldLeft(visitExp(exp)) {
        case (acc, CatchRule(_, _, e)) => acc ::: visitExp(e)
      }

    case Expression.NativeField(field, tpe, eff, loc) => Nil

    case Expression.NativeMethod(method, args, tpe, eff, loc) =>
      args.foldLeft(Nil: List[CompilationError]) {
        case (acc, e) => acc ::: visitExp(e)
      }

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

    case Expression.Sleep(exp, tpe, eff, loc) => visitExp(exp)

    case Expression.FixpointConstraint(con, tpe, eff, loc) => checkConstraint(con)

    case Expression.FixpointCompose(exp1, exp2, tpe, eff, loc) => visitExp(exp1) ::: visitExp(exp2)

    case Expression.FixpointSolve(exp, tpe, eff, loc) => visitExp(exp)

    case Expression.FixpointProject(pred, exp, tpe, eff, loc) => visitPredicateWithParam(pred) ::: visitExp(exp)

    case Expression.FixpointEntails(exp1, exp2, tpe, eff, loc) => visitExp(exp1) ::: visitExp(exp2)

    case Expression.UserError(tpe, eff, loc) => Nil

  }

  /**
    * Performs safety and well-formedness checks on the given predicate with parameter `p0`.
    */
  private def visitPredicateWithParam(p0: PredicateWithParam): List[CompilationError] = p0 match {
    case PredicateWithParam(sym, exp) => visitExp(exp)
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
    case Predicate.Body.Atom(pred, polarity, terms, tpe, loc) =>
      visitPredicateWithParam(pred) ::: checkBodyAtomPredicate(polarity, terms, posVars, quantVars, loc)
    case Predicate.Body.Filter(sym, terms, loc) => Nil
    case Predicate.Body.Functional(sym, term, loc) => Nil
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
    case Predicate.Body.Atom(pred, polarity, terms, tpe, loc) => polarity match {
      case Polarity.Positive =>
        // Case 1: A positive atom positively defines all its free variables.
        terms.flatMap(freeVarsOf).toSet
      case Polarity.Negative =>
        // Case 2: A negative atom does not positively define any variables.
        Set.empty
    }
    case Predicate.Body.Filter(sym, terms, loc) => Set.empty
    case Predicate.Body.Functional(sym, term, loc) => Set.empty
  }

}

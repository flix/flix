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
  * Performs safety and well-formedness checks on a typed ast.
  */
object Safety extends Phase[Root, Root] {

  // TODO: Carefully vet this class.

  /**
    * Performs safety and well-formedness checks on a typed ast.
    */
  def run(root: Root)(implicit flix: Flix): Validation[Root, CompilationError] = flix.phase("Safety") {
    //
    // Check each stratum for safety errors.
    //
    val strataErrors = root.defs.flatMap {
      case (sym, defn) => visitDef(defn)
    }

    //
    // Combine all errors.
    //
    val allErrors = strataErrors

    //
    // Check if any errors were detected.
    //
    if (allErrors.isEmpty)
      root.toSuccess
    else
      Validation.Failure(allErrors.toStream)
  }

  // TODO: DOC
  private def visitDef(d0: TypedAst.Def): List[CompilationError] = visitExp(d0.exp)

  // TODO: DOC
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

    case Expression.Match(exp, rules, tpe, eff, loc) =>
      rules.foldLeft(visitExp(exp)) {
        case (acc, MatchRule(p, g, e)) => acc ::: visitExp(e)
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

    case Expression.ArrayLit(elms, tpe, eff, loc) =>
      elms.foldLeft(Nil: List[CompilationError]) {
        case (acc, e) => acc ::: visitExp(e)
      }

    case Expression.ArrayNew(elm, len, tpe, eff, loc) => visitExp(elm)

    case Expression.ArrayLoad(base, index, tpe, eff, loc) => visitExp(base) ::: visitExp(index)

    //    case class ArrayLoad(base: TypedAst.Expression, index: TypedAst.Expression, tpe: Type, eff: ast.Eff, loc: SourceLocation) extends TypedAst.Expression
    //
    //    case class ArrayLength(base: TypedAst.Expression, tpe: Type, eff: ast.Eff, loc: SourceLocation) extends TypedAst.Expression
    //
    //    case class ArrayStore(base: TypedAst.Expression, index: TypedAst.Expression, elm: TypedAst.Expression, tpe: Type, eff: ast.Eff, loc: SourceLocation) extends TypedAst.Expression
    //
    //    case class ArraySlice(base: TypedAst.Expression, beginIndex: TypedAst.Expression, endIndex: TypedAst.Expression, tpe: Type, eff: ast.Eff, loc: SourceLocation) extends TypedAst.Expression
    //
    //    case class VectorLit(elms: List[TypedAst.Expression], tpe: Type, eff: ast.Eff, loc: SourceLocation) extends TypedAst.Expression
    //
    //    case class VectorNew(elm: TypedAst.Expression, len: Int, tpe: Type, eff: ast.Eff, loc: SourceLocation) extends TypedAst.Expression
    //
    //    case class VectorLoad(base: TypedAst.Expression, index: Int, tpe: Type, eff: ast.Eff, loc: SourceLocation) extends TypedAst.Expression
    //
    //    case class VectorStore(base: TypedAst.Expression, index: Int, elm: TypedAst.Expression, tpe: Type, eff: ast.Eff, loc: SourceLocation) extends TypedAst.Expression
    //
    //    case class VectorLength(base: TypedAst.Expression, tpe: Type, eff: ast.Eff, loc: SourceLocation) extends TypedAst.Expression
    //
    //    case class VectorSlice(base: TypedAst.Expression, startIndex: Int, endIndex: TypedAst.Expression, tpe: Type, eff: ast.Eff, loc: SourceLocation) extends TypedAst.Expression
    //
    //    case class Ref(exp: TypedAst.Expression, tpe: Type, eff: ast.Eff, loc: SourceLocation) extends TypedAst.Expression
    //
    //    case class Deref(exp: TypedAst.Expression, tpe: Type, eff: ast.Eff, loc: SourceLocation) extends TypedAst.Expression
    //
    //    case class Assign(exp1: TypedAst.Expression, exp2: TypedAst.Expression, tpe: Type, eff: ast.Eff, loc: SourceLocation) extends TypedAst.Expression
    //
    //    case class HandleWith(exp: TypedAst.Expression, bindings: List[TypedAst.HandlerBinding], tpe: Type, eff: ast.Eff, loc: SourceLocation) extends TypedAst.Expression
    //

    case Expression.Existential(fparam, exp, eff, loc) => visitExp(exp)

    case Expression.Universal(fparam, exp, eff, loc) => visitExp(exp)

    case Expression.Ascribe(exp, tpe, eff, loc) => visitExp(exp)

    case Expression.Cast(exp, tpe, eff, loc) => visitExp(exp)

    case Expression.NativeConstructor(constructor, args, tpe, eff, loc) =>
      args.foldLeft(Nil: List[CompilationError]) {
        case (acc, e) => acc ::: visitExp(e)
      }

    //
    //    case class TryCatch(exp: TypedAst.Expression, rules: List[TypedAst.CatchRule], tpe: Type, eff: ast.Eff, loc: SourceLocation) extends TypedAst.Expression
    //
    //    case class NativeField(field: Field, tpe: Type, eff: ast.Eff, loc: SourceLocation) extends TypedAst.Expression
    //

    case Expression.NativeMethod(method, args, tpe, eff, loc) =>
      args.foldLeft(Nil: List[CompilationError]) {
        case (acc, e) => acc ::: visitExp(e)
      }
    //
    //    case class NewRelation(sym: Symbol.RelSym, tpe: Type, eff: ast.Eff, loc: SourceLocation) extends TypedAst.Expression
    //
    //    case class NewLattice(sym: Symbol.LatSym, tpe: Type, eff: ast.Eff, loc: SourceLocation) extends TypedAst.Expression
    //
    //    case class Constraint(con: TypedAst.Constraint, tpe: Type, eff: ast.Eff, loc: SourceLocation) extends TypedAst.Expression
    //

    case Expression.Constraint(con, tpe, eff, loc) => checkConstraint(con)

    case Expression.ConstraintUnion(exp1, exp2, tpe, eff, loc) => visitExp(exp1) ::: visitExp(exp2)

    case Expression.FixpointSolve(exp, tpe, eff, loc) => visitExp(exp)

    case Expression.FixpointCheck(exp, tpe, eff, loc) => visitExp(exp)

    case Expression.UserError(tpe, eff, loc) => Nil

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
    // Check that all negative atoms only use positively defined variable symbols.
    //
    c0.body.flatMap(checkBodyPredicate(_, posVars))
  }

  /**
    * Performs safety and well-formedness checks on the given body predicate `p0`
    * with the given positively defined variable symbols `posVars`.
    */
  private def checkBodyPredicate(p0: Predicate.Body, posVars: Set[Symbol.VarSym]): List[CompilationError] = p0 match {
    case Predicate.Body.RelAtom(base, sym, polarity, terms, loc) => checkBodyAtomPredicate(polarity, terms, posVars, loc)
    case Predicate.Body.LatAtom(base, sym, polarity, terms, loc) => checkBodyAtomPredicate(polarity, terms, posVars, loc)
    case Predicate.Body.Filter(sym, terms, loc) => Nil
    case Predicate.Body.Functional(sym, term, loc) => Nil
  }

  /**
    * Performs safety and well-formedness checks on an atom with the given polarity, terms, and positive variables.
    */
  private def checkBodyAtomPredicate(polarity: Polarity, terms: List[TypedAst.Pattern], posVars: Set[Symbol.VarSym], loc: SourceLocation): List[CompilationError] = {
    polarity match {
      case Polarity.Positive => Nil
      case Polarity.Negative =>
        // Compute the free variables in the terms.
        val freeVars = terms.flatMap(freeVarsOf).toSet

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
    * Returns all positively defiend variable symbols in the given body predicate `p0`.
    */
  private def positivelyDefinedVariables(p0: Predicate.Body): Set[Symbol.VarSym] = p0 match {
    case Predicate.Body.RelAtom(base, sym, polarity, terms, loc) => polarity match {
      case Polarity.Positive =>
        // Case 1: A positive atom positively defines all its free variables.
        terms.flatMap(freeVarsOf).toSet
      case Polarity.Negative =>
        // Case 2: A negative atom does not positively define any variables.
        Set.empty
    }
    case Predicate.Body.LatAtom(base, sym, polarity, terms, loc) => polarity match {
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

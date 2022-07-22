package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.CompilationMessage
import ca.uwaterloo.flix.language.ast.Ast.{Denotation, Fixity, Polarity}
import ca.uwaterloo.flix.language.ast.TypedAst.Predicate.Body
import ca.uwaterloo.flix.language.ast.TypedAst._
import ca.uwaterloo.flix.language.ast.ops.TypedAstOps._
import ca.uwaterloo.flix.language.ast.{Kind, SourceLocation, Symbol, Type}
import ca.uwaterloo.flix.language.errors.SafetyError
import ca.uwaterloo.flix.language.errors.SafetyError._
import ca.uwaterloo.flix.util.Validation
import ca.uwaterloo.flix.util.Validation._

import scala.annotation.tailrec

/**
  * Performs safety and well-formedness of Datalog constraints.
  */
object Safety {

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
  private def visitDef(def0: Def): List[CompilationMessage] = visitExp(def0.impl.exp)

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

    case Expression.Hole(_, _, _) => Nil

    case Expression.Lambda(_, exp, _, _) =>
      visitExp(exp)

    case Expression.Apply(exp, exps, _, _, _, _) =>
      visitExp(exp) ::: exps.flatMap(visitExp)

    case Expression.Unary(_, exp, _, _, _, _) =>
      visitExp(exp)

    case Expression.Binary(_, exp1, exp2, _, _, _, _) =>
      visitExp(exp1) ::: visitExp(exp2)

    case Expression.Let(_, _, exp1, exp2, _, _, _, _) =>
      visitExp(exp1) ::: visitExp(exp2)

    case Expression.LetRec(_, _, exp1, exp2, _, _, _, _) =>
      visitExp(exp1) ::: visitExp(exp2)

    case Expression.Region(_, _) =>
      Nil

    case Expression.Scope(_, _, exp, _, _, _, _) =>
      visitExp(exp)

    case Expression.IfThenElse(exp1, exp2, exp3, _, _, _, _) =>
      visitExp(exp1) ::: visitExp(exp2) ::: visitExp(exp3)

    case Expression.Stm(exp1, exp2, _, _, _, _) =>
      visitExp(exp1) ::: visitExp(exp2)

    case Expression.Discard(exp, _, _, _) =>
      visitExp(exp)

    case Expression.Match(exp, rules, _, _, _, _) =>
      visitExp(exp) :::
        rules.flatMap { case MatchRule(_, g, e) => visitExp(g) ::: visitExp(e) }

    case Expression.Choose(exps, rules, _, _, _, _) =>
      exps.flatMap(visitExp) :::
        rules.flatMap { case ChoiceRule(_, exp) => visitExp(exp) }

    case Expression.Tag(_, _, exp, _, _, _, _) =>
      visitExp(exp)

    case Expression.Tuple(elms, _, _, _, _) =>
      elms.flatMap(visitExp)

    case Expression.RecordEmpty(_, _) => Nil

    case Expression.RecordSelect(exp, _, _, _, _, _) =>
      visitExp(exp)

    case Expression.RecordExtend(_, value, rest, _, _, _, _) =>
      visitExp(value) ::: visitExp(rest)

    case Expression.RecordRestrict(_, rest, _, _, _, _) =>
      visitExp(rest)

    case Expression.ArrayLit(elms, exp, _, _, _, _) =>
      elms.flatMap(visitExp) ::: visitExp(exp)

    case Expression.ArrayNew(exp1, exp2, exp3, _, _, _, _) =>
      visitExp(exp1) ::: visitExp(exp2) ::: visitExp(exp3)

    case Expression.ArrayLoad(base, index, _, _, _, _) =>
      visitExp(base) ::: visitExp(index)

    case Expression.ArrayLength(base, _, _, _) =>
      visitExp(base)

    case Expression.ArrayStore(base, index, elm, _, _) =>
      visitExp(base) ::: visitExp(index) ::: visitExp(elm)

    case Expression.ArraySlice(base, beginIndex, endIndex, _, _, _) =>
      visitExp(base) ::: visitExp(beginIndex) ::: visitExp(endIndex)

    case Expression.Ref(exp1, exp2, _, _, _, _) =>
      visitExp(exp1) ::: visitExp(exp2)

    case Expression.Deref(exp, _, _, _, _) =>
      visitExp(exp)

    case Expression.Assign(exp1, exp2, _, _, _, _) =>
      visitExp(exp1) ::: visitExp(exp2)

    case Expression.Ascribe(exp, _, _, _, _) =>
      visitExp(exp)

    case Expression.Cast(exp, _, _, _, _, _, _, _) =>
      visitExp(exp)

    case Expression.Upcast(exp, tpe, pur, eff, loc) =>
      println(s"tpe    : $tpe")
      println(s"pur    : $pur")
      println(s"eff    : $eff")
      println(s"exp.tpe: ${exp.tpe}")
      println(s"exp.pur: ${exp.pur}")
      println(s"exp.eff: ${exp.eff}")
      val x = tpe match {
        // case _: Type.Var => List(UnsafeUpcast(loc))
        case _ => Nil
      }
      val ps = (pur, exp.pur) match {
        case (Type.Pure, _) =>
          // Nothing can be cast to pure
          List(UnsafeUpcast(loc))

        case (_, Type.Impure) =>
          // An Impure expression cannot be cast to anything else
          List(UnsafeUpcast(loc))

        case _ => Nil
      }
      visitExp(exp) ::: ps ::: x

    case Expression.Without(exp, _, _, _, _, _) =>
      visitExp(exp)

    case Expression.TryCatch(exp, rules, _, _, _, _) =>
      visitExp(exp) :::
        rules.flatMap { case CatchRule(_, _, e) => visitExp(e) }

    case Expression.TryWith(exp, _, rules, _, _, _, _) =>
      visitExp(exp) :::
        rules.flatMap { case HandlerRule(_, _, e) => visitExp(e) }

    case Expression.Do(_, exps, _, _, _) =>
      exps.flatMap(visitExp)

    case Expression.Resume(exp, _, _) =>
      visitExp(exp)

    case Expression.InvokeConstructor(_, args, _, _, _, _) =>
      args.flatMap(visitExp)

    case Expression.InvokeMethod(_, exp, args, _, _, _, _) =>
      visitExp(exp) ::: args.flatMap(visitExp)

    case Expression.InvokeStaticMethod(_, args, _, _, _, _) =>
      args.flatMap(visitExp)

    case Expression.GetField(_, exp, _, _, _, _) =>
      visitExp(exp)

    case Expression.PutField(_, exp1, exp2, _, _, _, _) =>
      visitExp(exp1) ::: visitExp(exp2)

    case Expression.GetStaticField(_, _, _, _, _) =>
      Nil

    case Expression.PutStaticField(_, exp, _, _, _, _) =>
      visitExp(exp)

    case Expression.NewObject(_, _, _, _, methods, _) =>
      methods.flatMap { case JvmMethod(_, _, exp, _, _, _, _) => visitExp(exp) }

    case Expression.NewChannel(exp, _, _, _, _) =>
      visitExp(exp)

    case Expression.GetChannel(exp, _, _, _, _) =>
      visitExp(exp)

    case Expression.PutChannel(exp1, exp2, _, _, _, _) =>
      visitExp(exp1) ::: visitExp(exp2)

    case Expression.SelectChannel(rules, default, _, _, _, _) =>
      rules.flatMap { case SelectChannelRule(_, chan, body) => visitExp(chan) :::
        visitExp(body)
      } :::
        default.map(visitExp).getOrElse(Nil)

    case Expression.Spawn(exp, _, _, _, _) =>
      visitExp(exp)

    case Expression.Lazy(exp, _, _) =>
      visitExp(exp)

    case Expression.Force(exp, _, _, _, _) =>
      visitExp(exp)

    case Expression.FixpointConstraintSet(cs, _, _, _) =>
      cs.flatMap(checkConstraint)

    case Expression.FixpointLambda(_, exp, _, _, _, _, _) =>
      visitExp(exp)

    case Expression.FixpointMerge(exp1, exp2, _, _, _, _, _) =>
      visitExp(exp1) ::: visitExp(exp2)

    case Expression.FixpointSolve(exp, _, _, _, _, _) =>
      visitExp(exp)

    case Expression.FixpointFilter(_, exp, _, _, _, _) =>
      visitExp(exp)

    case Expression.FixpointInject(exp, _, _, _, _, _) =>
      visitExp(exp)

    case Expression.FixpointProject(_, exp, _, _, _, _) =>
      visitExp(exp)

    case Expression.Reify(_, _, _, _, _) => Nil

    case Expression.ReifyType(_, _, _, _, _, _) => Nil

    case Expression.ReifyEff(_, exp1, exp2, exp3, _, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2) ++ visitExp(exp3)

  }

  /**
    * Performs safety and well-formedness checks on the given constraint `c0`.
    */
  private def checkConstraint(c0: Constraint): List[CompilationMessage] = {
    //
    // Compute the set of positively defined variable symbols in the constraint.
    //
    val posVars = positivelyDefinedVariables(c0)

    // The variables that are used in a non-fixed lattice position
    val latVars0 = nonFixedLatticeVariablesOf(c0)
    // the variables that are used in a fixed position
    val fixedLatVars0 = fixedLatticeVariablesOf(c0)

    // The variables that are used in lattice position, either fixed or non-fixed.
    val latVars = latVars0 union fixedLatVars0
    // The lattice variables that are always fixed can be used in the head.
    val safeLatVars = fixedLatVars0 -- latVars0
    // The lattice variables that cannot be used relationally in the head.
    val unsafeLatVars = latVars -- safeLatVars

    //
    // Compute the quantified variables in the constraint.
    //
    // A lexically bound variable does not appear in this set and is never free.
    //
    val quantVars = c0.cparams.map(_.sym).toSet

    //
    // Check that all negative atoms only use positively defined variable symbols
    // and that lattice variables are not used in relational position.
    //
    val err1 = c0.body.flatMap(checkBodyPredicate(_, posVars, quantVars, latVars))

    //
    // Check that the free relational variables in the head atom are not lattice variables.
    //
    val err2 = checkHeadPredicate(c0.head, unsafeLatVars)

    err1 concat err2
  }

  /**
    * Performs safety and well-formedness checks on the given body predicate `p0`
    * with the given positively defined variable symbols `posVars`.
    */
  private def checkBodyPredicate(p0: Predicate.Body, posVars: Set[Symbol.VarSym], quantVars: Set[Symbol.VarSym], latVars: Set[Symbol.VarSym]): List[CompilationMessage] = p0 match {
    case Predicate.Body.Atom(_, den, polarity, _, terms, _, loc) =>
      // check for non-positively bound negative variables.
      val err1 = polarity match {
        case Polarity.Positive => Nil
        case Polarity.Negative =>
          // Compute the free variables in the terms which are *not* bound by the lexical scope.
          val freeVars = terms.flatMap(freeVarsOf).toSet intersect quantVars
          val wildcardNegErrors = visitPats(terms, loc)

          // Check if any free variables are not positively bound.
          val variableNegErrors = ((freeVars -- posVars) map (makeIllegalNonPositivelyBoundVariableError(_, loc))).toList
          wildcardNegErrors ++ variableNegErrors
      }
      // check for relational use of lattice variables. We still look at fixed
      // atoms since latVars (which means that they occur non-fixed) cannot be
      // in another fixed atom.
      val relTerms = den match {
        case Denotation.Relational => terms
        case Denotation.Latticenal => terms.dropRight(1)
      }
      val err2 = relTerms.flatMap(freeVarsOf).filter(latVars.contains).map(
        s => IllegalRelationalUseOfLatticeVariable(s, loc)
      )

      // Combine the messages
      err1 ++ err2

    case Predicate.Body.Guard(exp, _) => visitExp(exp)

    case Predicate.Body.Loop(_, exp, _) => visitExp(exp)
  }

  /**
    * Creates an error for a non-positively bound variable, dependent on `sym.isWild`.
    *
    * @param loc the location of the atom containing the terms.
    */
  private def makeIllegalNonPositivelyBoundVariableError(sym: Symbol.VarSym, loc: SourceLocation): SafetyError =
    if (sym.isWild) IllegalNegativelyBoundWildVariable(sym, loc) else IllegalNonPositivelyBoundVariable(sym, loc)

  /**
    * Returns all the positively defined variable symbols in the given constraint `c0`.
    */
  private def positivelyDefinedVariables(c0: Constraint): Set[Symbol.VarSym] =
    c0.body.flatMap(positivelyDefinedVariables).toSet

  /**
    * Returns all positively defined variable symbols in the given body predicate `p0`.
    */
  private def positivelyDefinedVariables(p0: Predicate.Body): Set[Symbol.VarSym] = p0 match {
    case Predicate.Body.Atom(_, _, polarity, _, terms, _, _) => polarity match {
      case Polarity.Positive =>
        // Case 1: A positive atom positively defines all its free variables.
        terms.flatMap(freeVarsOf).toSet
      case Polarity.Negative =>
        // Case 2: A negative atom does not positively define any variables.
        Set.empty
    }

    case Predicate.Body.Guard(_, _) => Set.empty

    case Predicate.Body.Loop(_, _, _) => Set.empty
  }

  /**
    * Computes the free variables that occur in lattice position in
    * atoms that are marked with fix.
    */
  private def fixedLatticeVariablesOf(c0: Constraint): Set[Symbol.VarSym] =
    c0.body.flatMap(fixedLatticenalVariablesOf).toSet

  /**
    * Computes the lattice variables of `p0` if it is a fixed atom.
    */
  private def fixedLatticenalVariablesOf(p0: Predicate.Body): Set[Symbol.VarSym] = p0 match {
    case Body.Atom(_, Denotation.Latticenal, _, Fixity.Fixed, terms, _, _) =>
      terms.lastOption.map(freeVarsOf).getOrElse(Set.empty)

    case _ => Set.empty
  }

  /**
    * Computes the free variables that occur in a lattice position in
    * atoms that are not marked with fix.
    */
  private def nonFixedLatticeVariablesOf(c0: Constraint): Set[Symbol.VarSym] =
    c0.body.flatMap(latticenalVariablesOf).toSet

  /**
    * Computes the lattice variables of `p0` if it is not a fixed atom.
    */
  private def latticenalVariablesOf(p0: Predicate.Body): Set[Symbol.VarSym] = p0 match {
    case Predicate.Body.Atom(_, Denotation.Latticenal, _, Fixity.Loose, terms, _, _) =>
      terms.lastOption.map(freeVarsOf).getOrElse(Set.empty)

    case _ => Set.empty
  }

  /**
    * Checks for `IllegalRelationalUseOfLatticeVariable` in the given `head` predicate.
    */
  private def checkHeadPredicate(head: Predicate.Head, latVars: Set[Symbol.VarSym]): List[CompilationMessage] = head match {
    case Predicate.Head.Atom(_, Denotation.Latticenal, terms, _, loc) =>
      // Check the relational terms ("the keys").
      checkTerms(terms.dropRight(1), latVars, loc)
    case Predicate.Head.Atom(_, Denotation.Relational, terms, _, loc) =>
      // Check every term.
      checkTerms(terms, latVars, loc)
  }

  /**
    * Checks that the free variables of the terms does not contain any of the variables in `latVars`.
    * If they do contain a lattice variable then a `IllegalRelationalUseOfLatticeVariable` is created.
    */
  private def checkTerms(terms: List[Expression], latVars: Set[Symbol.VarSym], loc: SourceLocation): List[CompilationMessage] = {
    // Compute the free variables in all terms.
    val allVars = terms.foldLeft(Set.empty[Symbol.VarSym])({
      case (acc, term) => acc ++ freeVars(term).keys
    })

    // Compute the lattice variables that are illegally used in the terms.
    allVars.intersect(latVars).toList.map(sym => IllegalRelationalUseOfLatticeVariable(sym, loc))
  }

  /**
    * Returns an error for each occurrence of wildcards in each term.
    *
    * @param loc the location of the atom containing the terms.
    */
  private def visitPats(terms: List[Pattern], loc: SourceLocation): List[CompilationMessage] = {
    terms.flatMap(visitPat(_, loc))
  }

  /**
    * Returns an error for each occurrence of wildcards.
    *
    * @param loc the location of the atom containing the term.
    */
  @tailrec
  private def visitPat(term: Pattern, loc: SourceLocation): List[CompilationMessage] = term match {
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

package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.TypedAst.*
import ca.uwaterloo.flix.language.ast.TypedAst.Predicate.Body
import ca.uwaterloo.flix.language.ast.ops.TypedAstOps
import ca.uwaterloo.flix.language.ast.ops.TypedAstOps.*
import ca.uwaterloo.flix.language.ast.shared.*
import ca.uwaterloo.flix.language.ast.{RigidityEnv, SourceLocation, Symbol, Type, TypeConstructor}
import ca.uwaterloo.flix.language.dbg.AstPrinter.*
import ca.uwaterloo.flix.language.errors.SafetyError
import ca.uwaterloo.flix.language.errors.SafetyError.*
import ca.uwaterloo.flix.util.{JvmUtils, ParOps}

import java.math.BigInteger
import java.util.concurrent.ConcurrentLinkedQueue
import scala.annotation.tailrec
import scala.jdk.CollectionConverters.CollectionHasAsScala

/**
  * Checks the safety and well-formedness of:
  *   - Datalog constraints.
  *   - New object expressions.
  *   - CheckedCast expressions.
  *   - UncheckedCast expressions.
  *   - TypeMatch expressions.
  *   - Throw expressions.
  */
object Safety {

  /** Checks the safety and well-formedness of `root`. */
  def run(root: Root)(implicit flix: Flix): (Root, List[SafetyError]) = flix.phaseNew("Safety") {
    implicit val sctx: SharedContext = SharedContext.mk()
    ParOps.parMap(root.traits.values.flatMap(_.sigs))(visitSig)
    ParOps.parMap(root.defs.values)(visitDef)
    ParOps.parMap(TypedAstOps.instanceDefsOf(root))(visitDef)
    ParOps.parMap(root.sigs.values)(visitSig)

    (root, sctx.errors.asScala.toList)
  }

  /** Checks the safety and well-formedness of `sig`. */
  private def visitSig(sig: Sig)(implicit flix: Flix, sctx: SharedContext): Sig = {
    val renv = RigidityEnv.ofRigidVars(sig.spec.tparams.map(_.sym))
    sig.exp.foreach(visitExp(_)(inTryCatch = false, renv, sctx, flix))
    sig
  }

  /** Checks the safety and well-formedness of `defn`. */
  private def visitDef(defn: Def)(implicit sctx: SharedContext, flix: Flix): Def = {
    val renv = RigidityEnv.ofRigidVars(defn.spec.tparams.map(_.sym))
    visitExp(defn.exp)(inTryCatch = false, renv, sctx, flix)
    defn
  }

  /**
    * Checks tje safety and well-formedness of `exp0`.
    *
    * The checks are:
    *   - Nested [[Expr.TryCatch]] expressions are disallowed unless the inner [[Expr.TryCatch]]
    *     will be extracted (e.g. Lambda).
    *   - [[Expr.TypeMatch]] must end with a default case.
    *
    *
    * @param inTryCatch indicates whether `exp` is enclosed in a try-catch. This can be reset if the
    *                   expression will later be extracted to its own function (e.g [[Expr.Lambda]]).
    */
  private def visitExp(exp0: Expr)(implicit inTryCatch: Boolean, renv: RigidityEnv, sctx: SharedContext, flix: Flix): Unit = exp0 match {
    case Expr.Cst(_, _, _) =>
      ()

    case Expr.Var(_, _, _) =>
      ()

    case Expr.Hole(_, _, _, _) =>
      ()

    case Expr.HoleWithExp(exp, _, _, _) =>
      visitExp(exp)

    case Expr.OpenAs(_, exp, _, _) =>
      visitExp(exp)

    case Expr.Use(_, _, exp, _) =>
      visitExp(exp)

    case Expr.Lambda(_, exp, _, _) =>
      // `exp` will be in its own function, so `inTryCatch` is reset.
      visitExp(exp)(inTryCatch = false, renv, sctx, flix)

    case Expr.ApplyClo(exp1, exp2, _, _, _) =>
      visitExp(exp1)
      visitExp(exp2)

    case Expr.ApplyDef(_, exps, _, _, _, _) =>
      exps.foreach(visitExp)

    case Expr.ApplyLocalDef(_, exps, _, _, _, _) =>
      exps.foreach(visitExp)

    case Expr.ApplySig(_, exps, _, _, _, _) =>
      exps.foreach(visitExp)

    case Expr.Unary(_, exp, _, _, _) =>
      visitExp(exp)

    case Expr.Binary(_, exp1, exp2, _, _, _) =>
      visitExp(exp1)
      visitExp(exp2)

    case Expr.Let(_, exp1, exp2, _, _, _) =>
      visitExp(exp1)
      visitExp(exp2)

    case Expr.LocalDef(_, _, exp1, exp2, _, _, _) =>
      // `exp1` will be its own function, so `inTryCatch` is reset.
      visitExp(exp1)(inTryCatch = false, renv, sctx, flix)
      visitExp(exp2)

    case Expr.Region(_, _) =>
      ()

    case Expr.Scope(_, _, exp, _, _, _) =>
      visitExp(exp)

    case Expr.IfThenElse(exp1, exp2, exp3, _, _, _) =>
      visitExp(exp1)
      visitExp(exp2)
      visitExp(exp3)

    case Expr.Stm(exp1, exp2, _, _, _) =>
      visitExp(exp1)
      visitExp(exp2)

    case Expr.Discard(exp, _, _) =>
      visitExp(exp)

    case Expr.Match(exp, rules, _, _, _) =>
      visitExp(exp)
      rules.foreach { rule =>
        rule.guard.foreach(visitExp)
        visitExp(rule.exp)
      }

    case Expr.TypeMatch(exp, rules, _, _, _) =>
      // check whether the last case in the type match looks like `..: _`
      rules.lastOption match {
        // Use top scope since the rigidity check only cares if it's a syntactically known variable
        case Some(TypeMatchRule(_, Type.Var(sym, _), _)) if renv.isFlexible(sym)(Scope.Top) =>
          ()
        case Some(_) | None =>
          sctx.errors.add(SafetyError.MissingDefaultTypeMatchCase(exp.loc))
      }
      visitExp(exp)
      rules.foreach(rule => visitExp(rule.exp))

    case Expr.RestrictableChoose(_, exp, rules, _, _, _) =>
      visitExp(exp)
      rules.foreach(rule => visitExp(rule.exp))

      case Expr.Tag(_, exps, _, _, _) =>
        exps.foreach(visitExp)

      case Expr.RestrictableTag(_, exps, _, _, _) =>
        exps.foreach(visitExp)

    case Expr.Tuple(elms, _, _, _) =>
      elms.foreach(visitExp)

    case Expr.RecordSelect(exp, _, _, _, _) =>
      visitExp(exp)

    case Expr.RecordExtend(_, value, rest, _, _, _) =>
      visitExp(value)
      visitExp(rest)

    case Expr.RecordRestrict(_, rest, _, _, _) =>
      visitExp(rest)

    case Expr.ArrayLit(elms, exp, _, _, _) =>
      elms.foreach(visitExp)
      visitExp(exp)

    case Expr.ArrayNew(exp1, exp2, exp3, _, _, _) =>
      visitExp(exp1)
      visitExp(exp2)
      visitExp(exp3)

    case Expr.ArrayLoad(exp1, exp2, _, _, _) =>
      visitExp(exp1)
      visitExp(exp2)

    case Expr.ArrayLength(base, _, _) =>
      visitExp(base)

    case Expr.ArrayStore(exp1, exp2, exp3, _, _) =>
      visitExp(exp1)
      visitExp(exp2)
      visitExp(exp3)

    case Expr.StructNew(_, fields, region, _, _, _) =>
      fields.foreach { case (_, exp) => visitExp(exp) }
      visitExp(region)

    case Expr.StructGet(e, _, _, _, _) =>
      visitExp(e)

    case Expr.StructPut(exp1, _, exp2, _, _, _) =>
      visitExp(exp1)
      visitExp(exp2)

    case Expr.VectorLit(elms, _, _, _) =>
      elms.foreach(visitExp)

    case Expr.VectorLoad(exp1, exp2, _, _, _) =>
      visitExp(exp1)
      visitExp(exp2)

    case Expr.VectorLength(exp, _) =>
      visitExp(exp)

    case Expr.Ascribe(exp, _, _, _) =>
      visitExp(exp)

    case Expr.InstanceOf(exp, _, _) =>
      visitExp(exp)

    case cast@Expr.CheckedCast(castType, exp, _, _, _) =>
      castType match {
        case CheckedCastType.TypeCast => checkCheckedTypeCast(cast)
        case CheckedCastType.EffectCast => ()
      }
      visitExp(exp)

    case cast@Expr.UncheckedCast(exp, _, _, _, _, loc) =>
      verifyUncheckedCast(cast)
      checkAllPermissions(loc.security, loc)
      visitExp(exp)

    case Expr.Unsafe(exp, _, _, _, loc) =>
      checkAllPermissions(loc.security, loc)
      visitExp(exp)

    case Expr.Without(exp, _, _, _, _) =>
      visitExp(exp)

    case Expr.TryCatch(exp, rules, _, _, loc) =>
      // Check for [[IllegalNestedTryCatch]]
      if (inTryCatch) {
        sctx.errors.add(IllegalNestedTryCatch(loc))
      }
      visitExp(exp)(inTryCatch = true, renv, sctx, flix)
      rules.foreach { case CatchRule(bnd, clazz, e) =>
        checkCatchClass(clazz, bnd.sym.loc)
        visitExp(e)
      }

    case Expr.Throw(exp, _, _, loc) =>
      checkAllPermissions(loc.security, loc)
      visitExp(exp)
      checkThrow(exp)

    case Expr.Handler(effUse, rules, _, _, _, _, _) =>
      // Check for [[PrimitiveEffectInTryWith]]
      if (Symbol.isPrimitiveEff(effUse.sym)) {
        sctx.errors.add(PrimitiveEffectInTryWith(effUse.sym, effUse.qname.loc))
      }
      rules.foreach(rule => visitExp(rule.exp))

    case Expr.RunWith(exp1, exp2, tpe, eff, loc) =>
      visitExp(exp1)
      visitExp(exp2)

    case Expr.Do(_, exps, _, _, _) =>
      exps.foreach(visitExp)

    case Expr.InvokeConstructor(_, args, _, _, loc) =>
      checkAllPermissions(loc.security, loc)
      args.foreach(visitExp)

    case Expr.InvokeMethod(_, exp, args, _, _, loc) =>
      checkAllPermissions(loc.security, loc)
      visitExp(exp)
      args.foreach(visitExp)

    case Expr.InvokeStaticMethod(_, args, _, _, loc) =>
      checkAllPermissions(loc.security, loc)
      args.foreach(visitExp)

    case Expr.GetField(_, exp, _, _, loc) =>
      checkAllPermissions(loc.security, loc)
      visitExp(exp)

    case Expr.PutField(_, exp1, exp2, _, _, loc) =>
      checkAllPermissions(loc.security, loc)
      visitExp(exp1)
      visitExp(exp2)

    case Expr.GetStaticField(_, _, _, loc) =>
      checkAllPermissions(loc.security, loc)

    case Expr.PutStaticField(_, exp, _, _, loc) =>
      checkAllPermissions(loc.security, loc)
      visitExp(exp)

    case newObject@Expr.NewObject(_, _, _, _, methods, loc) =>
      checkAllPermissions(loc.security, loc)
      checkObjectImplementation(newObject)
      methods.foreach(method => visitExp(method.exp))

    case Expr.NewChannel(exp, _, _, _) =>
      visitExp(exp)

    case Expr.GetChannel(exp, _, _, _) =>
      visitExp(exp)

    case Expr.PutChannel(exp1, exp2, _, _, _) =>
      visitExp(exp1)
      visitExp(exp2)

    case Expr.SelectChannel(rules, default, _, _, _) =>
      rules.foreach {
        case SelectChannelRule(_, chan, body) =>
          visitExp(chan)
          visitExp(body)
      }
      default.map(visitExp).getOrElse(Nil)

    case Expr.Spawn(exp1, exp2, _, _, _) =>
      if (hasControlEffects(exp1.eff)) sctx.errors.add(IllegalSpawnEffect(exp1.eff, exp1.loc))
      visitExp(exp1)
      visitExp(exp2)

    case Expr.ParYield(frags, exp, _, _, _) =>
      frags.foreach(fragment => visitExp(fragment.exp))
      visitExp(exp)

    case Expr.Lazy(exp, _, _) =>
      visitExp(exp)

    case Expr.Force(exp, _, _, _) =>
      visitExp(exp)

    case Expr.FixpointConstraintSet(cs, _, _) =>
      cs.foreach(checkConstraint)

    case Expr.FixpointLambda(_, exp, _, _, _) =>
      visitExp(exp)

    case Expr.FixpointMerge(exp1, exp2, _, _, _) =>
      visitExp(exp1)
      visitExp(exp2)

    case Expr.FixpointSolve(exp, _, _, _) =>
      visitExp(exp)

    case Expr.FixpointFilter(_, exp, _, _, _) =>
      visitExp(exp)

    case Expr.FixpointInject(exp, _, _, _, _) =>
      visitExp(exp)

    case Expr.FixpointProject(_, exp, _, _, _) =>
      visitExp(exp)

    case Expr.Error(_, _, _) =>
      ()

  }

  /** Checks that `ctx` is [[SecurityContext.AllPermissions]]. */
  private def checkAllPermissions(ctx: SecurityContext, loc: SourceLocation)(implicit sctx: SharedContext): Unit = {
    ctx match {
      case SecurityContext.AllPermissions => ()
      case SecurityContext.NoPermissions => sctx.errors.add(SafetyError.Forbidden(ctx, loc))
    }
  }

  /** Checks if `cast` is legal. */
  private def checkCheckedTypeCast(cast: Expr.CheckedCast)(implicit sctx: SharedContext, flix: Flix): Unit = cast match {
    case Expr.CheckedCast(_, exp, tpe, _, loc) =>
      val from = exp.tpe
      val to = tpe
      (Type.eraseAliases(from).baseType, Type.eraseAliases(to).baseType) match {

        // Allow casting Null to a Java type.
        case (Type.Cst(TypeConstructor.Null, _), Type.Cst(TypeConstructor.Native(_), _)) => ()
        case (Type.Cst(TypeConstructor.Null, _), Type.Cst(TypeConstructor.BigInt, _)) => ()
        case (Type.Cst(TypeConstructor.Null, _), Type.Cst(TypeConstructor.BigDecimal, _)) => ()
        case (Type.Cst(TypeConstructor.Null, _), Type.Cst(TypeConstructor.Str, _)) => ()
        case (Type.Cst(TypeConstructor.Null, _), Type.Cst(TypeConstructor.Regex, _)) => ()
        case (Type.Cst(TypeConstructor.Null, _), Type.Cst(TypeConstructor.Array, _)) => ()

        // Allow casting one Java type to another if there is a sub-type relationship.
        case (Type.Cst(TypeConstructor.Native(left), _), Type.Cst(TypeConstructor.Native(right), _)) =>
          if (right.isAssignableFrom(left)) () else sctx.errors.add(IllegalCheckedCast(from, to, loc))

        // Similar, but for String.
        case (Type.Cst(TypeConstructor.Str, _), Type.Cst(TypeConstructor.Native(right), _)) =>
          if (right.isAssignableFrom(classOf[String])) () else sctx.errors.add(IllegalCheckedCast(from, to, loc))

        // Similar, but for Regex.
        case (Type.Cst(TypeConstructor.Regex, _), Type.Cst(TypeConstructor.Native(right), _)) =>
          if (right.isAssignableFrom(classOf[java.util.regex.Pattern])) () else sctx.errors.add(IllegalCheckedCast(from, to, loc))

        // Similar, but for BigInt.
        case (Type.Cst(TypeConstructor.BigInt, _), Type.Cst(TypeConstructor.Native(right), _)) =>
          if (right.isAssignableFrom(classOf[BigInteger])) () else sctx.errors.add(IllegalCheckedCast(from, to, loc))

        // Similar, but for BigDecimal.
        case (Type.Cst(TypeConstructor.BigDecimal, _), Type.Cst(TypeConstructor.Native(right), _)) =>
          if (right.isAssignableFrom(classOf[java.math.BigDecimal])) () else sctx.errors.add(IllegalCheckedCast(from, to, loc))

        // Similar, but for Arrays.
        case (Type.Cst(TypeConstructor.Array, _), Type.Cst(TypeConstructor.Native(right), _)) =>
          if (right.isAssignableFrom(classOf[Array[Object]])) () else sctx.errors.add(IllegalCheckedCast(from, to, loc))

        // Disallow casting a type variable.
        case (src@Type.Var(_, _), _) =>
          sctx.errors.add(IllegalCheckedCastFromVar(src, to, loc))

        // Disallow casting a type variable (symmetric case)
        case (_, dst@Type.Var(_, _)) =>
          sctx.errors.add(IllegalCheckedCastToVar(from, dst, loc))

        // Disallow casting a Java type to any other type.
        case (Type.Cst(TypeConstructor.Native(clazz), _), _) =>
          sctx.errors.add(IllegalCheckedCastToNonJava(clazz, to, loc))

        // Disallow casting a Java type to any other type (symmetric case).
        case (_, Type.Cst(TypeConstructor.Native(clazz), _)) =>
          sctx.errors.add(IllegalCheckedCastFromNonJava(from, clazz, loc))

        // Disallow all other casts.
        case _ => sctx.errors.add(IllegalCheckedCast(from, to, loc))
      }
  }

  /**
    * Checks if there are any impossible casts, i.e. casts that always fail.
    *
    *   - No primitive type can be cast to a reference type and vice-versa.
    *   - No Bool type can be cast to a non-Bool type and vice-versa.
    */
  private def verifyUncheckedCast(cast: Expr.UncheckedCast)(implicit sctx: SharedContext, flix: Flix): Unit = cast match {
    case Expr.UncheckedCast(exp, declaredType, _, _, _, loc) =>
      val from = exp.tpe
      val to = declaredType
      val primitives = List(
        Type.Unit, Type.Bool, Type.Char,
        Type.Float32, Type.Float64, Type.Int8,
        Type.Int16, Type.Int32, Type.Int64,
        Type.Str, Type.Regex, Type.BigInt, Type.BigDecimal
      )

      (Type.eraseAliases(from).baseType, to.map(Type.eraseAliases).map(_.baseType)) match {
        // Allow casts where one side is a type variable.
        case (Type.Var(_, _), _) => ()
        case (_, Some(Type.Var(_, _))) => ()

        // Allow casts between Java types.
        case (Type.Cst(TypeConstructor.Native(_), _), _) => ()
        case (_, Some(Type.Cst(TypeConstructor.Native(_), _))) => ()

        // Disallow casting a Boolean to another primitive type.
        case (Type.Bool, Some(t2)) if primitives.filter(_ != Type.Bool).contains(t2) =>
          sctx.errors.add(ImpossibleUncheckedCast(from, to.get, loc))

        // Disallow casting a Boolean to another primitive type (symmetric case).
        case (t1, Some(Type.Bool)) if primitives.filter(_ != Type.Bool).contains(t1) =>
          sctx.errors.add(ImpossibleUncheckedCast(from, to.get, loc))

        // Disallowing casting a non-primitive type to a primitive type.
        case (t1, Some(t2)) if primitives.contains(t1) && !primitives.contains(t2) =>
          sctx.errors.add(ImpossibleUncheckedCast(from, to.get, loc))

        // Disallowing casting a non-primitive type to a primitive type (symmetric case).
        case (t1, Some(t2)) if primitives.contains(t2) && !primitives.contains(t1) =>
          sctx.errors.add(ImpossibleUncheckedCast(from, to.get, loc))

        case _  => ()
      }
  }

  /** Checks the safety and well-formedness of `c`. */
  private def checkConstraint(c: Constraint)(implicit inTryCatch: Boolean, renv: RigidityEnv, sctx: SharedContext, flix: Flix): Unit = {
    // Compute the set of positively defined variable symbols in the constraint.
    val posVars = positivelyDefinedVariables(c)

    // The variables that are used in a non-fixed lattice position.
    val latVars0 = nonFixedLatticeVariablesOf(c)

    // The variables that are used in a fixed position.
    val fixedLatVars0 = fixedLatticeVariablesOf(c)

    // The variables that are used in lattice position, either fixed or non-fixed.
    val latVars = latVars0 union fixedLatVars0

    // The lattice variables that are always fixed can be used in the head.
    val safeLatVars = fixedLatVars0 -- latVars0

    // The lattice variables that cannot be used relationally in the head.
    val unsafeLatVars = latVars -- safeLatVars

    // Compute the quantified variables in the constraint.
    // A lexically bound variable does not appear in this set and is never free.
    val quantVars = c.cparams.map(_.bnd.sym).toSet

    // Check that all negative atoms only use positively defined variable symbols
    // and that lattice variables are not used in relational position.
    c.body.foreach(checkBodyPredicate(_, posVars, quantVars, latVars))

    // Check that the free relational variables in the head atom are not lattice variables.
   checkHeadPredicate(c.head, unsafeLatVars)

    // Check that patterns in atom body are legal.
   c.body.foreach(s => checkBodyPattern(s))
  }

  /** Checks that `p` only contains [[Pattern.Var]], [[Pattern.Wild]], and [[Pattern.Cst]]. */
  private def checkBodyPattern(p: Predicate.Body)(implicit sctx: SharedContext): Unit = p match {
    case Predicate.Body.Atom(_, _, _, _, terms, _, loc) =>
      terms.foreach {
        case Pattern.Var(_, _, _) => ()
        case Pattern.Wild(_, _) => ()
        case Pattern.Cst(_, _, _) => ()
        case _ => sctx.errors.add(IllegalPatternInBodyAtom(loc))
      }
    case _ => ()
  }

  /**
    * Checks that `p` is well-formed.
    *
    * @param posVars the positively bound variables.
    * @param quantVars the quantified variables, not bound by lexical scope.
    * @param latVars the variables in lattice position.
    */
  private def checkBodyPredicate(p: Predicate.Body, posVars: Set[Symbol.VarSym], quantVars: Set[Symbol.VarSym], latVars: Set[Symbol.VarSym])(implicit inTryCatch: Boolean, renv: RigidityEnv, flix: Flix, sctx: SharedContext): Unit = p match {
    case Predicate.Body.Atom(_, den, polarity, _, terms, _, loc) =>
      // Check for non-positively bound negative variables.
      polarity match {
        case Polarity.Positive => ()
        case Polarity.Negative =>
          // Compute the free variables in the terms which are *not* bound by the lexical scope.
          val freeVars = terms.flatMap(freeVarsOf).toSet.intersect(quantVars)
          terms.foreach(visitPat(_, loc))

          // Check if any free variables are not positively bound.
          (freeVars -- posVars).foreach(mkIllegalNonPositivelyBoundVariableError(_, loc))
      }
      // Check for relational use of lattice variables. We still look at fixed atoms since latVars
      // (which means that they occur non-fixed) cannot be in another fixed atom.
      val relTerms = den match {
        case Denotation.Relational => terms
        case Denotation.Latticenal => terms.dropRight(1)
      }
      val relVars = relTerms.flatMap(freeVarsOf).toSet
      relVars.intersect(latVars).map(IllegalRelationalUseOfLatticeVar(_, loc)).foreach(sctx.errors.add)

    case Predicate.Body.Functional(_, exp, loc) =>
      // Check for non-positively in variables (free variables in exp).
      val inVars = freeVars(exp).keySet.intersect(quantVars)
      (inVars -- posVars).toList.foreach(mkIllegalNonPositivelyBoundVariableError(_, loc))

      visitExp(exp)

    case Predicate.Body.Guard(exp, _) =>
      visitExp(exp)

  }

  /** Returns a non-positively bound variable error, depending on `sym.isWild`. */
  private def mkIllegalNonPositivelyBoundVariableError(sym: Symbol.VarSym, loc: SourceLocation)(implicit sctx: SharedContext): Unit = {
    if (sym.isWild)
      sctx.errors.add(IllegalNegativelyBoundWildVar(sym, loc))
    else
      sctx.errors.add(IllegalNonPositivelyBoundVar(sym, loc))
  }

  /** Returns all the free and positively defined variable symbols in the given constraint `c`. */
  private def positivelyDefinedVariables(c: Constraint): Set[Symbol.VarSym] =
    c.body.flatMap(positivelyDefinedVariables).toSet

  /** Returns all free and positively defined variable symbols in the given body predicate `p`. */
  private def positivelyDefinedVariables(p: Predicate.Body): Set[Symbol.VarSym] = p match {
    case Predicate.Body.Atom(_, _, Polarity.Positive, _, terms, _, _) =>
      terms.flatMap(freeVarsOf).toSet
    case Predicate.Body.Atom(_, _, Polarity.Negative, _, _, _, _) =>
      Set.empty
    case Predicate.Body.Functional(_, _, _) =>
      // Functional does not positively bind any variables. Not even its outVars.
      Set.empty
    case Predicate.Body.Guard(_, _) =>
      Set.empty
  }

  /** Returns the free lattice variables of `c` that are marked with fix. */
  private def fixedLatticeVariablesOf(c: Constraint): Set[Symbol.VarSym] =
    c.body.flatMap(fixedLatticenalVariablesOf).toSet

  /** Returns the free lattice variables of `p` that are marked with fix. */
  private def fixedLatticenalVariablesOf(p: Predicate.Body): Set[Symbol.VarSym] = p match {
    case Body.Atom(_, Denotation.Latticenal, _, Fixity.Fixed, terms, _, _) =>
      terms.lastOption.map(freeVarsOf).getOrElse(Set.empty)
    case _ => Set.empty
  }

  /** Returns the free lattice variables of `c` that are not marked with fix. */
  private def nonFixedLatticeVariablesOf(c: Constraint): Set[Symbol.VarSym] =
    c.body.flatMap(latticeVariablesOf).toSet

  /** Returns the free lattice variables of `p` that are not marked with fix. */
  private def latticeVariablesOf(p: Predicate.Body): Set[Symbol.VarSym] = p match {
    case Predicate.Body.Atom(_, Denotation.Latticenal, _, Fixity.Loose, terms, _, _) =>
      terms.lastOption.map(freeVarsOf).getOrElse(Set.empty)
    case _ => Set.empty
  }

  /** Checks that the free relational variables in `head` does not include `latVars`. */
  private def checkHeadPredicate(head: Predicate.Head, latVars: Set[Symbol.VarSym])(implicit sctx: SharedContext): Unit = head match {
    case Predicate.Head.Atom(_, Denotation.Latticenal, terms, _, loc) =>
      val relationalTerms = terms.dropRight(1)
      checkTerms(relationalTerms, latVars, loc)
    case Predicate.Head.Atom(_, Denotation.Relational, terms, _, loc) =>
      checkTerms(terms, latVars, loc)
  }

  /** Checks that the free variables in `exps` does not include `latVars`. */
  private def checkTerms(exps: List[Expr], latVars: Set[Symbol.VarSym], loc: SourceLocation)(implicit sctx: SharedContext): Unit = {
    val allFreeVars = exps.flatMap(t => freeVars(t).keys).toSet

    // Compute the lattice variables that are illegally used in the exps.
    allFreeVars.intersect(latVars).toList.map(IllegalRelationalUseOfLatticeVar(_, loc)).foreach(sctx.errors.add)
  }

  /** Checks that `pat` contains no wildcards. */
  private def visitPat(pat: Pattern, loc: SourceLocation)(implicit sctx: SharedContext): Unit = pat match {
    case Pattern.Wild(_, _) => sctx.errors.add(IllegalNegativelyBoundWildCard(loc))
    case Pattern.Var(_, _, _) => ()
    case Pattern.Cst(_, _, _) => ()
    case Pattern.Tag(_, pats, _, _) => pats.foreach(visitPat(_, loc))
    case Pattern.Tuple(elms, _, _) => elms.foreach(visitPat(_, loc))
    case Pattern.Record(pats, pat, _, _) =>
      pats.map(_.pat).foreach(visitPat(_, loc))
      visitPat(pat, loc)
    case Pattern.Error(_, _) => ()
  }

  /**
    * Checks that `clazz` is [[java.lang.Throwable]] or a subclass.
    *
    * @param clazz the Java class specified in the catch clause
    * @param loc   the location of the catch parameter.
    */
  private def checkCatchClass(clazz: Class[?], loc: SourceLocation )(implicit sctx: SharedContext): Unit =
    if (!isThrowable(clazz)) {
      sctx.errors.add(IllegalCatchType(loc))
    }

  /** Returns `true` if `clazz` is [[java.lang.Throwable]] or a subclass of it. */
  private def isThrowable(clazz: Class[?]): Boolean =
    classOf[Throwable].isAssignableFrom(clazz)

  /** Checks that the type of the argument to `throw` is [[java.lang.Throwable]] or a subclass. */
  private def checkThrow(exp: Expr )(implicit sctx: SharedContext): Unit =
    if (!isThrowableType(exp.tpe)) sctx.errors.add(IllegalThrowType(exp.loc))

  /** Returns `true` if `tpe` is [[java.lang.Throwable]] or a subclass of it. */
  @tailrec
  private def isThrowableType(tpe: Type): Boolean = tpe match {
    case Type.Cst(TypeConstructor.Native(clazz), _) => isThrowable(clazz)
    case Type.Alias(_, _, tpe, _) => isThrowableType(tpe)
    case _ => false
  }

  /**
    * Checks that `newObject` correctly implements its class. `newObject` contains `methods` that
    * are supposed to implement `clazz`.
    *
    * The conditions are that:
    *   - `clazz` must be an interface or have a non-private constructor without arguments.
    *   - `clazz` must be public.
    *   - `methods` must take the object itself (`this`) as the first argument.
    *   - `methods` must include all required signatures (e.g. abstract methods).
    *   - `methods` must not include non-existing methods.
    *   - `methods` must not let control effects escape.
    */
  private def checkObjectImplementation(newObject: Expr.NewObject)(implicit flix: Flix , sctx: SharedContext): Unit = newObject match {
    case Expr.NewObject(_, clazz, tpe0, _, methods, loc) =>
      val tpe = Type.eraseAliases(tpe0)
      // `clazz` must be an interface or have a non-private constructor without arguments.
      if (!clazz.isInterface && !hasNonPrivateZeroArgConstructor(clazz)) {
        sctx.errors.add(NewObjectMissingPublicZeroArgConstructor(clazz, loc))
      }

      // `clazz` must be public.
      if (!isPublicClass(clazz)) {
        sctx.errors.add(NewObjectNonPublicClass(clazz, loc))
      }

      // `methods` must take the object itself (`this`) as the first argument.
      methods.foreach {
        case JvmMethod(ident, fparams, _, _, _, methodLoc) =>
          val firstParam = fparams.head
          firstParam.tpe match {
            case t if Type.eraseAliases(t) == tpe =>
              ()
            case Type.Unit =>
              // Unit arguments are likely inserted by the compiler.
              sctx.errors.add(NewObjectMissingThisArg(clazz, ident.name, methodLoc))
            case _ =>
              sctx.errors.add(NewObjectIllegalThisType(clazz, firstParam.tpe, ident.name, methodLoc))
          }
      }

      val flixMethods = getFlixMethodSignatures(methods)
      val implemented = flixMethods.keySet

      val classMethods = getInstanceMethods(clazz)
      val objectClassMethods = getInstanceMethods(classOf[Object]).keySet
      val canImplement = classMethods.keySet
      val mustImplement = canImplement.filter(m =>
        isAbstractMethod(classMethods(m)) && !objectClassMethods.contains(m)
      )

      // `methods` must include all required signatures (e.g. abstract methods).
      val unimplemented = mustImplement.diff(implemented)
      unimplemented.map(m => NewObjectMissingMethod(clazz, classMethods(m), loc)).foreach(sctx.errors.add)

      // `methods` must not include non-existing methods.
      val undefined = implemented.diff(canImplement)
      undefined.map(m => NewObjectUndefinedMethod(clazz, m.name, flixMethods(m).loc)).foreach(sctx.errors.add)

      // `methods` must not let control effects escape.
      val controlEffecting = methods.filter(m => hasControlEffects(m.eff))
      controlEffecting.map(m => SafetyError.IllegalMethodEffect(m.eff, m.loc)).foreach(sctx.errors.add)
  }

  /**
    * Represents the Flix signature of a Java method.
    *
    * The signature contains no [[Type.Alias]].
    */
  private case class MethodSignature(name: String, paramTypes: List[Type], retTpe: Type)

  /** Returns a map of `methods` based on their [[MethodSignature]]. */
  private def getFlixMethodSignatures(methods: List[JvmMethod]): Map[MethodSignature, JvmMethod] = {
    methods.map {
      case m@JvmMethod(ident, fparams, _, retTpe, _, _) =>
        // Drop the first formal parameter (which always represents `this`)
        val paramTypes = fparams.tail.map(_.tpe)
        val signature = MethodSignature(ident.name, paramTypes.map(Type.eraseAliases), Type.eraseAliases(retTpe))
        signature -> m
    }.toMap
  }

  /** Returns the instance methods of `clazz` with their [[MethodSignature]]. */
  private def getInstanceMethods(clazz: Class[?]): Map[MethodSignature, java.lang.reflect.Method] = {
    val methods = JvmUtils.getInstanceMethods(clazz)
    methods.map(m => {
      val signature = MethodSignature(m.getName, m.getParameterTypes.toList.map(Type.getFlixType), Type.getFlixType(m.getReturnType))
      signature -> m
    }).toMap
  }

  /** Return `true` if `clazz` has a non-private constructor with zero arguments. */
  private def hasNonPrivateZeroArgConstructor(clazz: Class[?]): Boolean = {
    try {
      val constructor = clazz.getDeclaredConstructor()
      !java.lang.reflect.Modifier.isPrivate(constructor.getModifiers)
    } catch {
      case _: NoSuchMethodException => false
    }
  }

  /** Returns `true` if `c` is public. */
  private def isPublicClass(c: Class[?]): Boolean =
    java.lang.reflect.Modifier.isPublic(c.getModifiers)

  /** Return `true` if `m` is abstract. */
  private def isAbstractMethod(m: java.lang.reflect.Method): Boolean =
    java.lang.reflect.Modifier.isAbstract(m.getModifiers)

  /** Returns `true` if `eff` includes control effects (e.g. Console). */
  private def hasControlEffects(eff: Type): Boolean = {
    // TODO: This is unsound and incomplete because it ignores type variables, associated types, etc.
    !eff.effects.forall(Symbol.isPrimitiveEff)
  }

  /**
   * Companion object for [[SharedContext]]
   */
  private object SharedContext {

    /**
     * Returns a fresh shared context.
     */
    def mk(): SharedContext = new SharedContext(new ConcurrentLinkedQueue())
  }

  /**
   * A global shared context. Must be thread-safe.
   *
   * @param errors the [[SafetyError]]s in the AST, if any.
   */
  private case class SharedContext(errors: ConcurrentLinkedQueue[SafetyError])

}

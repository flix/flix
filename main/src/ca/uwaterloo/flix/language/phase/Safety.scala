package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.TypedAst.*
import ca.uwaterloo.flix.language.ast.TypedAst.Predicate.Body
import ca.uwaterloo.flix.language.ast.ops.TypedAstOps
import ca.uwaterloo.flix.language.ast.ops.TypedAstOps.*
import ca.uwaterloo.flix.language.ast.shared.*
import ca.uwaterloo.flix.language.ast.{Kind, RigidityEnv, SourceLocation, Symbol, Type, TypeConstructor, TypedAst}
import ca.uwaterloo.flix.language.dbg.AstPrinter.*
import ca.uwaterloo.flix.language.errors.SafetyError
import ca.uwaterloo.flix.language.errors.SafetyError.*
import ca.uwaterloo.flix.util.{JvmUtils, ParOps}

import java.math.BigInteger
import scala.annotation.tailrec

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
    val classSigErrs = ParOps.parMap(root.traits.values.flatMap(_.sigs))(visitSig).flatten
    val defErrs = ParOps.parMap(root.defs.values)(visitDef).flatten
    val instanceDefErrs = ParOps.parMap(TypedAstOps.instanceDefsOf(root))(visitDef).flatten
    val sigErrs = ParOps.parMap(root.sigs.values)(visitSig).flatten
    val errors = classSigErrs ++ defErrs ++ instanceDefErrs ++ sigErrs ++ checkSendableInstances(root)

    (root, errors.toList)
  }

  /** Checks that no type parameters for types that implement `Sendable` are of kind [[Kind.Eff]]. */
  private def checkSendableInstances(root: Root): List[SafetyError] = {
    val sendableClass = new Symbol.TraitSym(Nil, "Sendable", SourceLocation.Unknown)

    root.instances.getOrElse(sendableClass, Nil).flatMap {
      case TypedAst.Instance(_, _, _, _, tpe, _, _, _, _, loc) =>
        if (tpe.typeArguments.exists(_.kind == Kind.Eff))
          List(SafetyError.IllegalSendableInstance(tpe, loc))
        else
          Nil
    }
  }

  /** Checks the safety and well-formedness of `sig`. */
  private def visitSig(sig: Sig)(implicit flix: Flix): List[SafetyError] = {
    val renv = RigidityEnv.ofRigidVars(sig.spec.tparams.map(_.sym))
    sig.exp.map(visitExp(_)(inTryCatch = false, renv, flix)).getOrElse(Nil)
  }

  /** Checks the safety and well-formedness of `defn`. */
  private def visitDef(defn: Def)(implicit flix: Flix): List[SafetyError] = {
    val renv = RigidityEnv.ofRigidVars(defn.spec.tparams.map(_.sym))
    visitExp(defn.exp)(inTryCatch = false, renv, flix)
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
  private def visitExp(exp0: Expr)(implicit inTryCatch: Boolean, renv: RigidityEnv, flix: Flix): List[SafetyError] = exp0 match {
    case Expr.Cst(_, _, _) =>
      Nil

    case Expr.Var(_, _, _) =>
      Nil

    case Expr.Hole(_, _, _, _) =>
      Nil

    case Expr.HoleWithExp(exp, _, _, _) =>
      visitExp(exp)

    case Expr.OpenAs(_, exp, _, _) =>
      visitExp(exp)

    case Expr.Use(_, _, exp, _) =>
      visitExp(exp)

    case Expr.Lambda(_, exp, _, _) =>
      // `exp` will be in its own function, so `inTryCatch` is reset.
      visitExp(exp)(inTryCatch = false, renv, flix)

    case Expr.ApplyClo(exp, exps, _, _, _) =>
      visitExp(exp) ++ exps.flatMap(visitExp)

    case Expr.ApplyDef(_, exps, _, _, _, _) =>
      exps.flatMap(visitExp)

    case Expr.ApplyLocalDef(_, exps, _, _, _, _) =>
      exps.flatMap(visitExp)

    case Expr.ApplySig(_, exps, _, _, _, _) =>
      exps.flatMap(visitExp)

    case Expr.Unary(_, exp, _, _, _) =>
      visitExp(exp)

    case Expr.Binary(_, exp1, exp2, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2)

    case Expr.Let(_, exp1, exp2, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2)

    case Expr.LocalDef(_, _, exp1, exp2, _, _, _) =>
      // `exp1` will be its own function, so `inTryCatch` is reset.
      visitExp(exp1)(inTryCatch = false, renv, flix) ++ visitExp(exp2)

    case Expr.Region(_, _) =>
      Nil

    case Expr.Scope(_, _, exp, _, _, _) =>
      visitExp(exp)

    case Expr.IfThenElse(exp1, exp2, exp3, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2) ++ visitExp(exp3)

    case Expr.Stm(exp1, exp2, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2)

    case Expr.Discard(exp, _, _) =>
      visitExp(exp)

    case Expr.Match(exp, rules, _, _, _) =>
      visitExp(exp) ++ rules.flatMap(rule => rule.guard.map(visitExp).getOrElse(Nil) ++ visitExp(rule.exp))

    case Expr.TypeMatch(exp, rules, _, _, _) =>
      // check whether the last case in the type match looks like `..: _`
      val missingDefault = rules.lastOption match {
        // Use top scope since the rigidity check only cares if it's a syntactically known variable
        case Some(TypeMatchRule(_, Type.Var(sym, _), _)) if renv.isFlexible(sym)(Scope.Top) =>
          Nil
        case Some(_) | None =>
          List(SafetyError.MissingDefaultTypeMatchCase(exp.loc))
      }
      visitExp(exp) ++ missingDefault ++ rules.flatMap(rule => visitExp(rule.exp))

    case Expr.RestrictableChoose(_, exp, rules, _, _, _) =>
      visitExp(exp) ++ rules.flatMap(rule => visitExp(rule.exp))

    case Expr.Tag(_, exp, _, _, _) =>
      visitExp(exp)

    case Expr.RestrictableTag(_, exp, _, _, _) =>
      visitExp(exp)

    case Expr.Tuple(elms, _, _, _) =>
      elms.flatMap(visitExp)

    case Expr.RecordEmpty(_, _) =>
      Nil

    case Expr.RecordSelect(exp, _, _, _, _) =>
      visitExp(exp)

    case Expr.RecordExtend(_, value, rest, _, _, _) =>
      visitExp(value) ++ visitExp(rest)

    case Expr.RecordRestrict(_, rest, _, _, _) =>
      visitExp(rest)

    case Expr.ArrayLit(elms, exp, _, _, _) =>
      elms.flatMap(visitExp) ++ visitExp(exp)

    case Expr.ArrayNew(exp1, exp2, exp3, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2) ++ visitExp(exp3)

    case Expr.ArrayLoad(base, index, _, _, _) =>
      visitExp(base) ++ visitExp(index)

    case Expr.ArrayLength(base, _, _) =>
      visitExp(base)

    case Expr.ArrayStore(base, index, elm, _, _) =>
      visitExp(base) ++ visitExp(index) ++ visitExp(elm)

    case Expr.StructNew(_, fields, region, _, _, _) =>
      fields.flatMap { case (_, exp) => visitExp(exp) } ++ visitExp(region)

    case Expr.StructGet(e, _, _, _, _) =>
      visitExp(e)

    case Expr.StructPut(e1, _, e2, _, _, _) =>
      visitExp(e1) ++ visitExp(e2)

    case Expr.VectorLit(elms, _, _, _) =>
      elms.flatMap(visitExp)

    case Expr.VectorLoad(exp1, exp2, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2)

    case Expr.VectorLength(exp, _) =>
      visitExp(exp)

    case Expr.Ascribe(exp, _, _, _) =>
      visitExp(exp)

    case Expr.InstanceOf(exp, _, _) =>
      visitExp(exp)

    case cast@Expr.CheckedCast(castType, exp, _, _, _) =>
      val castErrors = castType match {
        case CheckedCastType.TypeCast => checkCheckedTypeCast(cast)
        case CheckedCastType.EffectCast => Nil
      }
      visitExp(exp) ++ castErrors

    case cast@Expr.UncheckedCast(exp, _, _, _, _, loc) =>
      val castErrors = verifyUncheckedCast(cast)
      val permissionErrors = checkAllPermissions(loc.security, loc)
      permissionErrors ++ visitExp(exp) ++ castErrors

    case Expr.UncheckedMaskingCast(exp, _, _, loc) =>
      val permissionErrors = checkAllPermissions(loc.security, loc)
      permissionErrors ++ visitExp(exp)

    case Expr.Without(exp, _, _, _, _) =>
      visitExp(exp)

    case Expr.TryCatch(exp, rules, _, _, loc) =>
      val nestedTryCatchError = if (inTryCatch) List(IllegalNestedTryCatch(loc)) else Nil
      nestedTryCatchError ++ visitExp(exp)(inTryCatch = true, renv, flix) ++
        rules.flatMap { case CatchRule(bnd, clazz, e) => checkCatchClass(clazz, bnd.sym.loc) ++ visitExp(e) }

    case Expr.Throw(exp, _, _, loc) =>
      val permissionErrors = checkAllPermissions(loc.security, loc)
      permissionErrors ++ visitExp(exp) ++ checkThrow(exp)

    case Expr.TryWith(exp, effUse, rules, _, _, _) =>
      val effectErrors = {
        if (Symbol.isPrimitiveEff(effUse.sym)) List(PrimitiveEffectInTryWith(effUse.sym, effUse.loc))
        else Nil
      }
      effectErrors ++ visitExp(exp) ++ rules.flatMap(rule => visitExp(rule.exp))


    case Expr.Do(_, exps, _, _, _) =>
      exps.flatMap(visitExp)

    case Expr.InvokeConstructor(_, args, _, _, loc) =>
      val permissionErrors = checkAllPermissions(loc.security, loc)
      permissionErrors ++ args.flatMap(visitExp)

    case Expr.InvokeMethod(_, exp, args, _, _, loc) =>
      val permissionErrors = checkAllPermissions(loc.security, loc)
      permissionErrors ++ visitExp(exp) ++ args.flatMap(visitExp)

    case Expr.InvokeStaticMethod(_, args, _, _, loc) =>
      val permissionErrors = checkAllPermissions(loc.security, loc)
      permissionErrors ++ args.flatMap(visitExp)

    case Expr.GetField(_, exp, _, _, loc) =>
      val permissionErrors = checkAllPermissions(loc.security, loc)
      permissionErrors ++ visitExp(exp)

    case Expr.PutField(_, exp1, exp2, _, _, loc) =>
      val permissionErrors = checkAllPermissions(loc.security, loc)
      permissionErrors ++ visitExp(exp1) ++ visitExp(exp2)

    case Expr.GetStaticField(_, _, _, loc) =>
      checkAllPermissions(loc.security, loc)

    case Expr.PutStaticField(_, exp, _, _, loc) =>
      val permissionErrors = checkAllPermissions(loc.security, loc)
      permissionErrors ++ visitExp(exp)

    case newObject@Expr.NewObject(_, _, _, _, methods, loc) =>
      val permissionErrors = checkAllPermissions(loc.security, loc)
      val objectErrors = checkObjectImplementation(newObject)
      permissionErrors ++ objectErrors ++ methods.flatMap(method => visitExp(method.exp))

    case Expr.NewChannel(exp1, exp2, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2)

    case Expr.GetChannel(exp, _, _, _) =>
      visitExp(exp)

    case Expr.PutChannel(exp1, exp2, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2)

    case Expr.SelectChannel(rules, default, _, _, _) =>
      rules.flatMap {
        case SelectChannelRule(_, chan, body) => visitExp(chan) ++ visitExp(body)
      } ++ default.map(visitExp).getOrElse(Nil)

    case Expr.Spawn(exp1, exp2, _, _, _) =>
      val illegalSpawnEffect = {
        if (hasControlEffects(exp1.eff)) List(IllegalSpawnEffect(exp1.eff, exp1.loc))
        else Nil
      }

      illegalSpawnEffect ++ visitExp(exp1) ++ visitExp(exp2)

    case Expr.ParYield(frags, exp, _, _, _) =>
      frags.flatMap(fragment => visitExp(fragment.exp)) ++ visitExp(exp)

    case Expr.Lazy(exp, _, _) =>
      visitExp(exp)

    case Expr.Force(exp, _, _, _) =>
      visitExp(exp)

    case Expr.FixpointConstraintSet(cs, _, _) =>
      cs.flatMap(checkConstraint)

    case Expr.FixpointLambda(_, exp, _, _, _) =>
      visitExp(exp)

    case Expr.FixpointMerge(exp1, exp2, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2)

    case Expr.FixpointSolve(exp, _, _, _) =>
      visitExp(exp)

    case Expr.FixpointFilter(_, exp, _, _, _) =>
      visitExp(exp)

    case Expr.FixpointInject(exp, _, _, _, _) =>
      visitExp(exp)

    case Expr.FixpointProject(_, exp, _, _, _) =>
      visitExp(exp)

    case Expr.Error(_, _, _) =>
      Nil

  }

  /** Checks that `ctx` is [[SecurityContext.AllPermissions]]. */
  private def checkAllPermissions(ctx: SecurityContext, loc: SourceLocation): List[SafetyError] = {
    ctx match {
      case SecurityContext.AllPermissions => Nil
      case SecurityContext.NoPermissions => List(SafetyError.Forbidden(ctx, loc))
    }
  }

  /** Checks if `cast` is legal. */
  private def checkCheckedTypeCast(cast: Expr.CheckedCast)(implicit flix: Flix): List[SafetyError] = cast match {
    case Expr.CheckedCast(_, exp, tpe, _, loc) =>
      val from = exp.tpe
      val to = tpe
      (Type.eraseAliases(from).baseType, Type.eraseAliases(to).baseType) match {

        // Allow casting Null to a Java type.
        case (Type.Cst(TypeConstructor.Null, _), Type.Cst(TypeConstructor.Native(_), _)) => Nil
        case (Type.Cst(TypeConstructor.Null, _), Type.Cst(TypeConstructor.BigInt, _)) => Nil
        case (Type.Cst(TypeConstructor.Null, _), Type.Cst(TypeConstructor.BigDecimal, _)) => Nil
        case (Type.Cst(TypeConstructor.Null, _), Type.Cst(TypeConstructor.Str, _)) => Nil
        case (Type.Cst(TypeConstructor.Null, _), Type.Cst(TypeConstructor.Regex, _)) => Nil
        case (Type.Cst(TypeConstructor.Null, _), Type.Cst(TypeConstructor.Array, _)) => Nil

        // Allow casting one Java type to another if there is a sub-type relationship.
        case (Type.Cst(TypeConstructor.Native(left), _), Type.Cst(TypeConstructor.Native(right), _)) =>
          if (right.isAssignableFrom(left)) Nil else IllegalCheckedCast(from, to, loc) :: Nil

        // Similar, but for String.
        case (Type.Cst(TypeConstructor.Str, _), Type.Cst(TypeConstructor.Native(right), _)) =>
          if (right.isAssignableFrom(classOf[String])) Nil else IllegalCheckedCast(from, to, loc) :: Nil

        // Similar, but for Regex.
        case (Type.Cst(TypeConstructor.Regex, _), Type.Cst(TypeConstructor.Native(right), _)) =>
          if (right.isAssignableFrom(classOf[java.util.regex.Pattern])) Nil else IllegalCheckedCast(from, to, loc) :: Nil

        // Similar, but for BigInt.
        case (Type.Cst(TypeConstructor.BigInt, _), Type.Cst(TypeConstructor.Native(right), _)) =>
          if (right.isAssignableFrom(classOf[BigInteger])) Nil else IllegalCheckedCast(from, to, loc) :: Nil

        // Similar, but for BigDecimal.
        case (Type.Cst(TypeConstructor.BigDecimal, _), Type.Cst(TypeConstructor.Native(right), _)) =>
          if (right.isAssignableFrom(classOf[java.math.BigDecimal])) Nil else IllegalCheckedCast(from, to, loc) :: Nil

        // Similar, but for Arrays.
        case (Type.Cst(TypeConstructor.Array, _), Type.Cst(TypeConstructor.Native(right), _)) =>
          if (right.isAssignableFrom(classOf[Array[Object]])) Nil else IllegalCheckedCast(from, to, loc) :: Nil

        // Disallow casting a type variable.
        case (src@Type.Var(_, _), _) =>
          IllegalCheckedCastFromVar(src, to, loc) :: Nil

        // Disallow casting a type variable (symmetric case)
        case (_, dst@Type.Var(_, _)) =>
          IllegalCheckedCastToVar(from, dst, loc) :: Nil

        // Disallow casting a Java type to any other type.
        case (Type.Cst(TypeConstructor.Native(clazz), _), _) =>
          IllegalCheckedCastToNonJava(clazz, to, loc) :: Nil

        // Disallow casting a Java type to any other type (symmetric case).
        case (_, Type.Cst(TypeConstructor.Native(clazz), _)) =>
          IllegalCheckedCastFromNonJava(from, clazz, loc) :: Nil

        // Disallow all other casts.
        case _ => IllegalCheckedCast(from, to, loc) :: Nil
      }
  }

  /**
    * Checks if there are any impossible casts, i.e. casts that always fail.
    *
    *   - No primitive type can be cast to a reference type and vice-versa.
    *   - No Bool type can be cast to a non-Bool type and vice-versa.
    */
  private def verifyUncheckedCast(cast: Expr.UncheckedCast)(implicit flix: Flix): List[SafetyError.ImpossibleUncheckedCast] = cast match {
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
        case (Type.Var(_, _), _) => Nil
        case (_, Some(Type.Var(_, _))) => Nil

        // Allow casts between Java types.
        case (Type.Cst(TypeConstructor.Native(_), _), _) => Nil
        case (_, Some(Type.Cst(TypeConstructor.Native(_), _))) => Nil

        // Disallow casting a Boolean to another primitive type.
        case (Type.Bool, Some(t2)) if primitives.filter(_ != Type.Bool).contains(t2) =>
          ImpossibleUncheckedCast(from, to.get, loc) :: Nil

        // Disallow casting a Boolean to another primitive type (symmetric case).
        case (t1, Some(Type.Bool)) if primitives.filter(_ != Type.Bool).contains(t1) =>
          ImpossibleUncheckedCast(from, to.get, loc) :: Nil

        // Disallowing casting a non-primitive type to a primitive type.
        case (t1, Some(t2)) if primitives.contains(t1) && !primitives.contains(t2) =>
          ImpossibleUncheckedCast(from, to.get, loc) :: Nil

        // Disallowing casting a non-primitive type to a primitive type (symmetric case).
        case (t1, Some(t2)) if primitives.contains(t2) && !primitives.contains(t1) =>
          ImpossibleUncheckedCast(from, to.get, loc) :: Nil

        case _ => Nil
      }
  }

  /** Checks the safety and well-formedness of `c`. */
  private def checkConstraint(c: Constraint)(implicit inTryCatch: Boolean, renv: RigidityEnv, flix: Flix): List[SafetyError] = {
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
    val err1 = c.body.flatMap(checkBodyPredicate(_, posVars, quantVars, latVars))

    // Check that the free relational variables in the head atom are not lattice variables.
    val err2 = checkHeadPredicate(c.head, unsafeLatVars)

    // Check that patterns in atom body are legal.
    val err3 = c.body.flatMap(s => checkBodyPattern(s))

    err1 ++ err2 ++ err3
  }

  /** Checks that `p` only contains [[Pattern.Var]], [[Pattern.Wild]], and [[Pattern.Cst]]. */
  private def checkBodyPattern(p: Predicate.Body): List[SafetyError] = p match {
    case Predicate.Body.Atom(_, _, _, _, terms, _, loc) =>
      terms.flatMap {
        case Pattern.Var(_, _, _) => None
        case Pattern.Wild(_, _) => None
        case Pattern.Cst(_, _, _) => None
        case _ => Some(IllegalPatternInBodyAtom(loc))
      }
    case _ => Nil
  }

  /**
    * Checks that `p` is well-formed.
    *
    * @param posVars the positively bound variables.
    * @param quantVars the quantified variables, not bound by lexical scope.
    * @param latVars the variables in lattice position.
    */
  private def checkBodyPredicate(p: Predicate.Body, posVars: Set[Symbol.VarSym], quantVars: Set[Symbol.VarSym], latVars: Set[Symbol.VarSym])(implicit inTryCatch: Boolean, renv: RigidityEnv, flix: Flix): List[SafetyError] = p match {
    case Predicate.Body.Atom(_, den, polarity, _, terms, _, loc) =>
      // Check for non-positively bound negative variables.
      val err1 = polarity match {
        case Polarity.Positive => Nil
        case Polarity.Negative =>
          // Compute the free variables in the terms which are *not* bound by the lexical scope.
          val freeVars = terms.flatMap(freeVarsOf).toSet.intersect(quantVars)
          val wildcardNegErrors = terms.flatMap(visitPat(_, loc))

          // Check if any free variables are not positively bound.
          val variableNegErrors = (freeVars -- posVars).map(mkIllegalNonPositivelyBoundVariableError(_, loc)).toList
          wildcardNegErrors ++ variableNegErrors
      }
      // Check for relational use of lattice variables. We still look at fixed atoms since latVars
      // (which means that they occur non-fixed) cannot be in another fixed atom.
      val relTerms = den match {
        case Denotation.Relational => terms
        case Denotation.Latticenal => terms.dropRight(1)
      }
      val relVars = relTerms.flatMap(freeVarsOf).toSet
      val err2 = relVars.intersect(latVars).map(IllegalRelationalUseOfLatticeVar(_, loc))

      // Combine the messages
      err1 ++ err2

    case Predicate.Body.Functional(_, exp, loc) =>
      // Check for non-positively in variables (free variables in exp).
      val inVars = freeVars(exp).keySet.intersect(quantVars)
      val err1 = (inVars -- posVars).toList.map(mkIllegalNonPositivelyBoundVariableError(_, loc))

      err1 ::: visitExp(exp)

    case Predicate.Body.Guard(exp, _) =>
      visitExp(exp)

  }

  /** Returns a non-positively bound variable error, depending on `sym.isWild`. */
  private def mkIllegalNonPositivelyBoundVariableError(sym: Symbol.VarSym, loc: SourceLocation): SafetyError = {
    if (sym.isWild) IllegalNegativelyBoundWildVar(sym, loc)
    else IllegalNonPositivelyBoundVar(sym, loc)
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
  private def checkHeadPredicate(head: Predicate.Head, latVars: Set[Symbol.VarSym]): List[SafetyError] = head match {
    case Predicate.Head.Atom(_, Denotation.Latticenal, terms, _, loc) =>
      val relationalTerms = terms.dropRight(1)
      checkTerms(relationalTerms, latVars, loc)
    case Predicate.Head.Atom(_, Denotation.Relational, terms, _, loc) =>
      checkTerms(terms, latVars, loc)
  }

  /** Checks that the free variables in `exps` does not include `latVars`. */
  private def checkTerms(exps: List[Expr], latVars: Set[Symbol.VarSym], loc: SourceLocation): List[SafetyError] = {
    val allFreeVars = exps.flatMap(t => freeVars(t).keys).toSet

    // Compute the lattice variables that are illegally used in the exps.
    allFreeVars.intersect(latVars).toList.map(IllegalRelationalUseOfLatticeVar(_, loc))
  }

  /** Checks that `pat` contains no wildcards. */
  private def visitPat(pat: Pattern, loc: SourceLocation): List[SafetyError] = pat match {
    case Pattern.Wild(_, _) => List(IllegalNegativelyBoundWildCard(loc))
    case Pattern.Var(_, _, _) => Nil
    case Pattern.Cst(_, _, _) => Nil
    case Pattern.Tag(_, pat, _, _) => visitPat(pat, loc)
    case Pattern.Tuple(elms, _, _) => elms.flatMap(visitPat(_, loc))
    case Pattern.Record(pats, pat, _, _) => pats.map(_.pat).flatMap(visitPat(_, loc)) ++ visitPat(pat, loc)
    case Pattern.RecordEmpty(_, _) => Nil
    case Pattern.Error(_, _) => Nil
  }

  /**
    * Checks that `clazz` is [[java.lang.Throwable]] or a subclass.
    *
    * @param clazz the Java class specified in the catch clause
    * @param loc   the location of the catch parameter.
    */
  private def checkCatchClass(clazz: Class[?], loc: SourceLocation): List[SafetyError] = {
    if (isThrowable(clazz)) List.empty
    else List(IllegalCatchType(loc))
  }

  /** Returns `true` if `clazz` is [[java.lang.Throwable]] or a subclass of it. */
  private def isThrowable(clazz: Class[?]): Boolean =
    classOf[Throwable].isAssignableFrom(clazz)

  /** Checks that the type of the argument to `throw` is [[java.lang.Throwable]] or a subclass. */
  private def checkThrow(exp: Expr): List[SafetyError] = {
    if (isThrowableType(exp.tpe)) List()
    else List(IllegalThrowType(exp.loc))
  }

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
  private def checkObjectImplementation(newObject: Expr.NewObject)(implicit flix: Flix): List[SafetyError] = newObject match {
    case Expr.NewObject(_, clazz, tpe0, _, methods, loc) =>
      val tpe = Type.eraseAliases(tpe0)
      // `clazz` must be an interface or have a non-private constructor without arguments.
      val constructorErrors = {
        if (clazz.isInterface) {
          List.empty
        } else {
          if (hasNonPrivateZeroArgConstructor(clazz)) List.empty
          else List(NewObjectMissingPublicZeroArgConstructor(clazz, loc))
        }
      }

      // `clazz` must be public.
      val visibilityErrors = {
        if (!isPublicClass(clazz)) List(NewObjectNonPublicClass(clazz, loc))
        else List.empty
      }

      // `methods` must take the object itself (`this`) as the first argument.
      val thisErrors = methods.flatMap {
        case JvmMethod(ident, fparams, _, _, _, methodLoc) =>
          val firstParam = fparams.head
          firstParam.tpe match {
            case t if Type.eraseAliases(t) == tpe =>
              None
            case Type.Unit =>
              // Unit arguments are likely inserted by the compiler.
              Some(NewObjectMissingThisArg(clazz, ident.name, methodLoc))
            case _ =>
              Some(NewObjectIllegalThisType(clazz, firstParam.tpe, ident.name, methodLoc))
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
      val unimplementedErrors = unimplemented.map(m => NewObjectMissingMethod(clazz, classMethods(m), loc))

      // `methods` must not include non-existing methods.
      val undefined = implemented.diff(canImplement)
      val undefinedErrors = undefined.map(m => NewObjectUndefinedMethod(clazz, m.name, flixMethods(m).loc))

      // `methods` must not let control effects escape.
      val controlEffecting = methods.filter(m => hasControlEffects(m.eff))
      val controlEffectingErrors = controlEffecting.map(m => SafetyError.IllegalMethodEffect(m.eff, m.loc))

      constructorErrors ++ visibilityErrors ++ thisErrors ++ unimplementedErrors ++ undefinedErrors ++ controlEffectingErrors
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

}

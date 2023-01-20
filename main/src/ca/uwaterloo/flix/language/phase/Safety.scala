package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.CompilationMessage
import ca.uwaterloo.flix.language.ast.Ast.{Denotation, Fixity, Polarity}
import ca.uwaterloo.flix.language.ast.TypedAst.Predicate.Body
import ca.uwaterloo.flix.language.ast.TypedAst._
import ca.uwaterloo.flix.language.ast.ops.TypedAstOps._
import ca.uwaterloo.flix.language.ast.{Kind, RigidityEnv, SourceLocation, Symbol, Type, TypeConstructor}
import ca.uwaterloo.flix.language.errors.SafetyError
import ca.uwaterloo.flix.language.errors.SafetyError._
import ca.uwaterloo.flix.language.phase.unification.Unification
import ca.uwaterloo.flix.util.Validation
import ca.uwaterloo.flix.util.Validation._

import java.math.BigInteger
import scala.annotation.tailrec

/**
  * Performs safety and well-formedness checks on:
  *  - Datalog constraints
  *  - Anonymous objects
  *  - Upcast expressions
  *  - Supercast expressions
  *  - TypeMatch expressions
  */
object Safety {

  /**
    * Performs safety and well-formedness checks on the given AST `root`.
    */
  def run(root: Root)(implicit flix: Flix): Validation[Root, CompilationMessage] = flix.phase("Safety") {
    //
    // Collect all errors.
    //
    val defErrors = root.defs.flatMap {
      case (_, defn) => visitDef(defn, root)
    }

    val instanceErrors = visitSendable(root)

    val errors = defErrors ++ instanceErrors

    //
    // Check if any errors were detected.
    //
    if (errors.isEmpty)
      root.toSuccess
    else
      Validation.Failure(errors.to(LazyList))
  }

  /**
    * Checks that no type parameters for types that implement `Sendable` of kind `Region`
    */
  private def visitSendable(root: Root)(implicit flix: Flix): List[CompilationMessage] = {

    val sendableClass = new Symbol.ClassSym(Nil, "Sendable", SourceLocation.Unknown)

    root.instances.getOrElse(sendableClass, Nil) flatMap {
      case Instance(_, _, _, _, tpe, _, _, _, loc) =>
        if (tpe.typeArguments.exists(_.kind == Kind.Bool))
          List(SafetyError.SendableError(tpe, loc))
        else
          Nil
    }
  }

  /**
    * Performs safety and well-formedness checks on the given definition `def0`.
    */
  private def visitDef(def0: Def, root: Root)(implicit flix: Flix): List[CompilationMessage] = {
    val renv = def0.spec.tparams.map(_.sym).foldLeft(RigidityEnv.empty) {
      case (acc, e) => acc.markRigid(e)
    }
    visitExp(def0.impl.exp, renv, root)
  }


  /**
    * Performs safety and well-formedness checks on the given expression `exp0`.
    */
  private def visitExp(e0: Expression, renv: RigidityEnv, root: Root)(implicit flix: Flix): List[CompilationMessage] = {

    def visit(exp0: Expression): List[CompilationMessage] = exp0 match {
      case Expression.Cst(_, _, _) => Nil

      case Expression.Wild(_, _) => Nil

      case Expression.Var(_, _, _) => Nil

      case Expression.Def(_, _, _) => Nil

      case Expression.Sig(_, _, _) => Nil

      case Expression.Hole(_, _, _) => Nil

      case Expression.HoleWithExp(exp, _, _, _, _) =>
        visit(exp)

      case Expression.Use(_, exp, _) =>
        visit(exp)

      case Expression.Lambda(_, exp, _, _) =>
        visit(exp)

      case Expression.Apply(exp, exps, _, _, _, _) =>
        visit(exp) ::: exps.flatMap(visit)

      case Expression.Unary(_, exp, _, _, _, _) =>
        visit(exp)

      case Expression.Binary(_, exp1, exp2, _, _, _, _) =>
        visit(exp1) ::: visit(exp2)

      case Expression.Let(_, _, exp1, exp2, _, _, _, _) =>
        visit(exp1) ::: visit(exp2)

      case Expression.LetRec(_, _, exp1, exp2, _, _, _, _) =>
        visit(exp1) ::: visit(exp2)

      case Expression.Region(_, _) =>
        Nil

      case Expression.Scope(_, _, exp, _, _, _, _) =>
        visit(exp)

      case Expression.IfThenElse(exp1, exp2, exp3, _, _, _, _) =>
        visit(exp1) ::: visit(exp2) ::: visit(exp3)

      case Expression.Stm(exp1, exp2, _, _, _, _) =>
        visit(exp1) ::: visit(exp2)

      case Expression.Discard(exp, _, _, _) =>
        visit(exp)

      case Expression.Match(exp, rules, _, _, _, _) =>
        visit(exp) :::
          rules.flatMap { case MatchRule(_, g, e) => g.toList.flatMap(visit) ::: visit(e) }

      case Expression.TypeMatch(exp, rules, _, _, _, _) =>
        // check whether the last case in the type match looks like `...: _`
        val missingDefault = rules.last match {
          case MatchTypeRule(_, tpe, _) => tpe match {
            case Type.Var(sym, _) if renv.isFlexible(sym) => Nil
            case _ => List(SafetyError.MissingDefaultMatchTypeCase(exp.loc))
          }
        }
        visit(exp) ::: missingDefault :::
          rules.flatMap { case MatchTypeRule(_, _, e) => visit(e) }

      case Expression.RelationalChoose(exps, rules, _, _, _, _) =>
        exps.flatMap(visit) :::
          rules.flatMap { case RelationalChoiceRule(_, exp) => visit(exp) }

      case Expression.RestrictableChoose(_, exp, rules, _, _, _, _) =>
        visit(exp) :::
          rules.flatMap { case RestrictableChoiceRule(pat, exp) => visit(exp) }

      case Expression.Tag(_, exp, _, _, _, _) =>
        visit(exp)

      case Expression.RestrictableTag(_, exp, _, _, _, _) =>
        visit(exp)

      case Expression.Tuple(elms, _, _, _, _) =>
        elms.flatMap(visit)

      case Expression.RecordEmpty(_, _) => Nil

      case Expression.RecordSelect(exp, _, _, _, _, _) =>
        visit(exp)

      case Expression.RecordExtend(_, value, rest, _, _, _, _) =>
        visit(value) ::: visit(rest)

      case Expression.RecordRestrict(_, rest, _, _, _, _) =>
        visit(rest)

      case Expression.ArrayLit(elms, exp, _, _, _, _) =>
        elms.flatMap(visit) ::: visit(exp)

      case Expression.ArrayNew(exp1, exp2, exp3, _, _, _, _) =>
        visit(exp1) ::: visit(exp2) ::: visit(exp3)

      case Expression.ArrayLoad(base, index, _, _, _, _) =>
        visit(base) ::: visit(index)

      case Expression.ArrayLength(base, _, _, _) =>
        visit(base)

      case Expression.ArrayStore(base, index, elm, _, _, _) =>
        visit(base) ::: visit(index) ::: visit(elm)

      case Expression.ArraySlice(reg, base, beginIndex, endIndex, _, _, _, _) =>
        visit(reg) ::: visit(base) ::: visit(beginIndex) ::: visit(endIndex)

      case Expression.Ref(exp1, exp2, _, _, _, _) =>
        visit(exp1) ::: visit(exp2)

      case Expression.Deref(exp, _, _, _, _) =>
        visit(exp)

      case Expression.Assign(exp1, exp2, _, _, _, _) =>
        visit(exp1) ::: visit(exp2)

      case Expression.Ascribe(exp, _, _, _, _) =>
        visit(exp)

      case Expression.Of(_, exp, _, _, _, _) =>
        visit(exp)

      case e@Expression.Cast(exp, _, _, _, _, _, _, _) =>
        val errors = checkCastSafety(e)
        visit(exp) ::: errors

      case Expression.Mask(exp, _, _, _, _) =>
        visit(exp)

      case Expression.Upcast(exp, tpe, loc) =>
        val errors = checkUpcastSafety(exp, tpe, renv, root, loc)
        visit(exp) ::: errors

      case Expression.Supercast(exp, tpe, loc) =>
        val errors = checkSupercastSafety(exp, tpe, loc)
        visit(exp) ::: errors

      case Expression.Without(exp, _, _, _, _, _) =>
        visit(exp)

      case Expression.TryCatch(exp, rules, _, _, _, _) =>
        visit(exp) :::
          rules.flatMap { case CatchRule(_, _, e) => visit(e) }

      case Expression.TryWith(exp, _, rules, _, _, _, _) =>
        visit(exp) :::
          rules.flatMap { case HandlerRule(_, _, e) => visit(e) }

      case Expression.Do(_, exps, _, _, _) =>
        exps.flatMap(visit)

      case Expression.Resume(exp, _, _) =>
        visit(exp)

      case Expression.InvokeConstructor(_, args, _, _, _, _) =>
        args.flatMap(visit)

      case Expression.InvokeMethod(_, exp, args, _, _, _, _) =>
        visit(exp) ::: args.flatMap(visit)

      case Expression.InvokeStaticMethod(_, args, _, _, _, _) =>
        args.flatMap(visit)

      case Expression.GetField(_, exp, _, _, _, _) =>
        visit(exp)

      case Expression.PutField(_, exp1, exp2, _, _, _, _) =>
        visit(exp1) ::: visit(exp2)

      case Expression.GetStaticField(_, _, _, _, _) =>
        Nil

      case Expression.PutStaticField(_, exp, _, _, _, _) =>
        visit(exp)

      case Expression.NewObject(_, clazz, tpe, _, _, methods, loc) =>
        val erasedType = Type.eraseAliases(tpe)
        checkObjectImplementation(clazz, erasedType, methods, loc) ++
          methods.flatMap {
            case JvmMethod(_, _, exp, _, _, _, _) => visit(exp)
          }

      case Expression.NewChannel(exp1, exp2, _, _, _, _) =>
        visit(exp1) ::: visit(exp2)

      case Expression.GetChannel(exp, _, _, _, _) =>
        visit(exp)

      case Expression.PutChannel(exp1, exp2, _, _, _, _) =>
        visit(exp1) ::: visit(exp2)

      case Expression.SelectChannel(rules, default, _, _, _, _) =>
        rules.flatMap { case SelectChannelRule(_, chan, body) => visit(chan) :::
          visit(body)
        } :::
          default.map(visit).getOrElse(Nil)

      case Expression.Spawn(exp1, exp2, _, _, _, _) =>
        visit(exp1) ::: visit(exp2)

      case Expression.Par(exp, _) =>
        // Only tuple expressions are allowed to be parallelized with `par`.
        exp match {
          case e: Expression.Tuple => visit(e)
          case _ => IllegalParExpression(exp, exp.loc) :: Nil
        }

      case Expression.ParYield(frags, exp, _, _, _, _) =>
        frags.flatMap { case ParYieldFragment(_, e, _) => visit(e) } ::: visit(exp)

      case Expression.Lazy(exp, _, _) =>
        visit(exp)

      case Expression.Force(exp, _, _, _, _) =>
        visit(exp)

      case Expression.FixpointConstraintSet(cs, _, _, _) =>
        cs.flatMap(checkConstraint(_, renv, root))

      case Expression.FixpointLambda(_, exp, _, _, _, _, _) =>
        visit(exp)

      case Expression.FixpointMerge(exp1, exp2, _, _, _, _, _) =>
        visit(exp1) ::: visit(exp2)

      case Expression.FixpointSolve(exp, _, _, _, _, _) =>
        visit(exp)

      case Expression.FixpointFilter(_, exp, _, _, _, _) =>
        visit(exp)

      case Expression.FixpointInject(exp, _, _, _, _, _) =>
        visit(exp)

      case Expression.FixpointProject(_, exp, _, _, _, _) =>
        visit(exp)

      case Expression.Error(_, _, _, _) =>
        Nil

    }

    visit(e0)

  }

  /**
    * Performs basic checks on the type cast `cast`. Returns a list of safety errors if there are
    * any impossible casts.
    *
    * No primitive type can be cast to a reference type and vice-versa.
    *
    * No Bool type can be cast to a non-Bool type  and vice-versa.
    */
  private def checkCastSafety(cast: Expression.Cast)(implicit flix: Flix): List[SafetyError] = {
    val tpe1 = Type.eraseAliases(cast.exp.tpe).baseType
    val tpe2 = cast.declaredType.map(Type.eraseAliases).map(_.baseType)

    val primitives = {
      Type.Unit :: Type.Bool :: Type.Char ::
        Type.Float32 :: Type.Float64 :: Type.Int8 ::
        Type.Int16 :: Type.Int32 :: Type.Int64 ::
        Type.Str :: Type.BigInt :: Type.BigDecimal :: Nil
    }

    (tpe1, tpe2) match {

      // Allow anything with type variables
      case (Type.Var(_, _), _) => Nil
      case (_, Some(Type.Var(_, _))) => Nil

      // Allow anything with Java interop
      case (Type.Cst(TypeConstructor.Native(_), _), _) => Nil
      case (_, Some(Type.Cst(TypeConstructor.Native(_), _))) => Nil

      // Boolean primitive to other primitives
      case (Type.Bool, Some(t2)) if primitives.filter(_ != Type.Bool).contains(t2) =>
        ImpossibleCast(cast.exp.tpe, cast.declaredType.get, cast.loc) :: Nil

      // Symmetric case
      case (t1, Some(Type.Bool)) if primitives.filter(_ != Type.Bool).contains(t1) =>
        ImpossibleCast(cast.exp.tpe, cast.declaredType.get, cast.loc) :: Nil

      // JVM Reference types and primitives
      case (t1, Some(t2)) if primitives.contains(t1) && !primitives.contains(t2) =>
        ImpossibleCast(cast.exp.tpe, cast.declaredType.get, cast.loc) :: Nil

      // Symmetric case
      case (t1, Some(t2)) if primitives.contains(t2) && !primitives.contains(t1) =>
        ImpossibleCast(cast.exp.tpe, cast.declaredType.get, cast.loc) :: Nil

      case _ => Nil
    }
  }

  /**
    * Checks that `tpe1` is a subtype of `tpe2`.
    *
    * `tpe1` is a subtype of `tpe2` if:
    *
    * (a) `tpe1` has the exact same flix type as `tpe2`
    *
    * (b) both types are java types and `tpe1` is a subtype of `tpe2`
    *
    * (c) both types are functions and `tpe1` is a subtype of `tpe2`
    *
    * AND
    *
    * the purity of the expression is being cast from `pure` -> `ef` -> `impure`.
    *
    * OR
    *
    * the effect set of the expression is a subset of the effect set being cast to.
    *
    */
  private def isSubtypeOf(tpe1: Type, tpe2: Type, renv: RigidityEnv, root: Root)(implicit flix: Flix): Boolean = (tpe1.baseType, tpe2.baseType) match {
    case (Type.Empty, _) => true
    case (Type.True, _) => true
    case (Type.Var(_, _), Type.False) => true

    case (Type.Cst(TypeConstructor.Native(left), _), Type.Cst(TypeConstructor.Native(right), _)) =>
      right.isAssignableFrom(left)

    case (Type.Cst(TypeConstructor.Str, _), Type.Cst(TypeConstructor.Native(right), _)) =>
      right.isAssignableFrom(classOf[java.lang.String])

    case (Type.Cst(TypeConstructor.BigInt, _), Type.Cst(TypeConstructor.Native(right), _)) =>
      right.isAssignableFrom(classOf[java.math.BigInteger])

    case (Type.Cst(TypeConstructor.BigDecimal, _), Type.Cst(TypeConstructor.Native(right), _)) =>
      right.isAssignableFrom(classOf[java.math.BigDecimal])

    case (Type.Cst(TypeConstructor.Arrow(n1), _), Type.Cst(TypeConstructor.Arrow(n2), _)) if n1 == n2 =>
      // purities
      val pur1 = tpe1.arrowPurityType
      val pur2 = tpe2.arrowPurityType
      val subTypePurity = isSubtypeOf(pur1, pur2, renv, root)

      // set effects
      // The rule for effect sets is:
      // S1 < S2 <==> exists S3 . S1 U S3 == S2
      val loc = tpe1.loc.asSynthetic
      val s1 = tpe1.arrowEffectType
      val s2 = tpe2.arrowEffectType
      val s3 = Type.freshVar(Kind.Effect, loc)
      val s1s3 = Type.mkUnion(s1, s3, loc)
      val isEffSubset = Unification.unifiesWith(s1s3, s2, renv)(root.univ, flix)

      // check that parameters are supertypes
      val args1 = tpe1.arrowArgTypes
      val args2 = tpe2.arrowArgTypes
      val superTypeArgs = args1.zip(args2).forall {
        case (t1, t2) =>
          isSubtypeOf(t2, t1, renv, root)
      }

      // check that result is a subtype
      val expectedResTpe = tpe1.arrowResultType
      val actualResTpe = tpe2.arrowResultType
      val subTypeResult = isSubtypeOf(expectedResTpe, actualResTpe, renv, root)

      subTypePurity && isEffSubset && superTypeArgs && subTypeResult

    case _ => tpe1 == tpe2

  }

  /**
    * Returns true if `tpe1` and `tpe2` are both Java types
    * and `tpe1` is a subtype of `tpe2`.
    * Note that `tpe1` is also allowed to be a Flix string
    * or BigInt/BigDecimal while `tpe2` is a supertype of this.
    */
  private def isJavaSubtypeOf(tpe1: Type, tpe2: Type)(implicit flix: Flix): Boolean = (tpe1.baseType, tpe2.baseType) match {

    case (Type.Cst(TypeConstructor.Native(left), _), Type.Cst(TypeConstructor.Native(right), _)) =>
      if (right.isAssignableFrom(left)) true else false

    case (Type.Cst(TypeConstructor.Str, _), Type.Cst(TypeConstructor.Native(right), _)) =>
      if (right.isAssignableFrom(classOf[java.lang.String])) true else false

    case (Type.Cst(TypeConstructor.BigInt, _), Type.Cst(TypeConstructor.Native(right), _)) =>
      if (right.isAssignableFrom(classOf[java.math.BigInteger])) true else false

    case (Type.Cst(TypeConstructor.BigDecimal, _), Type.Cst(TypeConstructor.Native(right), _)) =>
      if (right.isAssignableFrom(classOf[java.math.BigDecimal])) true else false

    case (Type.Var(_, _), _) | (_, Type.Var(_, _)) =>
      false

    case (Type.Cst(TypeConstructor.Native(_), _), _) | (_, Type.Cst(TypeConstructor.Native(_), _)) =>
      false

    case _ => false

  }

  /**
    * Returns a list of errors if the the upcast is invalid.
    */
  private def checkUpcastSafety(exp: Expression, tpe: Type, renv: RigidityEnv, root: Root, loc: SourceLocation)(implicit flix: Flix): List[SafetyError] = {
    val tpe1 = Type.eraseAliases(exp.tpe)
    val tpe2 = Type.eraseAliases(tpe)
    if (isSubtypeOf(tpe1, tpe2, renv, root))
      Nil
    else
      UnsafeUpcast(exp.tpe, tpe, loc) :: Nil
  }

  /**
    * Returns a list of errors if the the supercast is invalid.
    */
  private def checkSupercastSafety(exp: Expression, tpe: Type, loc: SourceLocation)(implicit flix: Flix): List[SafetyError] = {
    val tpe1 = Type.eraseAliases(exp.tpe)
    val tpe2 = Type.eraseAliases(tpe)
    if (isJavaSubtypeOf(tpe1, tpe2))
      Nil
    else
      collectSupercastErrors(exp, tpe, loc)
  }

  /**
    * Returns a list of supercast errors.
    */
  private def collectSupercastErrors(exp: Expression, tpe: Type, loc: SourceLocation)(implicit flix: Flix): List[SafetyError] = {
    val tpe1 = Type.eraseAliases(exp.tpe)
    val tpe2 = Type.eraseAliases(tpe)

    (tpe1.baseType, tpe2.baseType) match {

      case (Type.Cst(TypeConstructor.Native(left), _), Type.Cst(TypeConstructor.Native(right), _)) =>
        if (right.isAssignableFrom(left)) Nil else UnsafeSupercast(exp.tpe, tpe, loc) :: Nil

      case (Type.Cst(TypeConstructor.Str, _), Type.Cst(TypeConstructor.Native(right), _)) =>
        if (right.isAssignableFrom(classOf[String])) Nil else UnsafeSupercast(exp.tpe, tpe, loc) :: Nil

      case (Type.Cst(TypeConstructor.BigInt, _), Type.Cst(TypeConstructor.Native(right), _)) =>
        if (right.isAssignableFrom(classOf[BigInteger])) Nil else UnsafeSupercast(exp.tpe, tpe, loc) :: Nil

      case (Type.Cst(TypeConstructor.BigDecimal, _), Type.Cst(TypeConstructor.Native(right), _)) =>
        if (right.isAssignableFrom(classOf[java.math.BigDecimal])) Nil else UnsafeSupercast(exp.tpe, tpe, loc) :: Nil

      case (Type.Var(_, _), _) =>
        FromTypeVariableSupercast(exp.tpe, tpe, loc) :: Nil

      case (_, Type.Var(_, _)) =>
        ToTypeVariableSupercast(exp.tpe, tpe, loc) :: Nil

      case (Type.Cst(TypeConstructor.Native(clazz), _), _) =>
        ToNonJavaTypeSupercast(clazz, tpe, loc) :: Nil

      case (_, Type.Cst(TypeConstructor.Native(clazz), _)) =>
        FromNonJavaTypeSupercast(exp.tpe, clazz, loc) :: Nil

      case _ => UnsafeSupercast(exp.tpe, tpe, loc) :: Nil

    }
  }

  /**
    * Performs safety and well-formedness checks on the given constraint `c0`.
    */
  private def checkConstraint(c0: Constraint, renv: RigidityEnv, root: Root)(implicit flix: Flix): List[CompilationMessage] = {
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
    val err1 = c0.body.flatMap(checkBodyPredicate(_, posVars, quantVars, latVars, renv, root))

    //
    // Check that the free relational variables in the head atom are not lattice variables.
    //
    val err2 = checkHeadPredicate(c0.head, unsafeLatVars)

    //
    // Check that patterns in atom body are legal
    //
    val err3 = c0.body.flatMap(s => checkBodyPattern(s))

    err1 ++ err2 ++ err3
  }

  /**
    * Performs safety check on the pattern of an atom body.
    */
  private def checkBodyPattern(p0: Predicate.Body): List[CompilationMessage] = p0 match {
    case Predicate.Body.Atom(_, _, _, _, terms, _, loc) =>
      terms.foldLeft[List[SafetyError]](Nil)((acc, term) => term match {
        case Pattern.Var(_, _, _) => acc
        case Pattern.Wild(_, _) => acc
        case Pattern.Cst(_, _, _) => acc
        case _ => UnexpectedPatternInBodyAtom(loc) :: acc
      })
    case _ => Nil
  }

  /**
    * Performs safety and well-formedness checks on the given body predicate `p0`
    * with the given positively defined variable symbols `posVars`.
    */
  private def checkBodyPredicate(p0: Predicate.Body, posVars: Set[Symbol.VarSym], quantVars: Set[Symbol.VarSym], latVars: Set[Symbol.VarSym], renv: RigidityEnv, root: Root)(implicit flix: Flix): List[CompilationMessage] = p0 match {
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

    case Predicate.Body.Guard(exp, _) => visitExp(exp, renv, root)

    case Predicate.Body.Loop(_, exp, _) => visitExp(exp, renv, root)
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
    case Pattern.Cst(_, _, _) => Nil
    case Pattern.Tag(_, pat, _, _) => visitPat(pat, loc)
    case Pattern.Tuple(elms, _, _) => visitPats(elms, loc)
    case Pattern.Array(elms, _, _) => visitPats(elms, loc)
    case Pattern.ArrayTailSpread(elms, _, _, _) => visitPats(elms, loc)
    case Pattern.ArrayHeadSpread(_, elms, _, _) => visitPats(elms, loc)
  }

  /**
    * Ensures that `methods` fully implement `clazz`
    */
  private def checkObjectImplementation(clazz: java.lang.Class[_], tpe: Type, methods: List[JvmMethod], loc: SourceLocation): List[CompilationMessage] = {
    //
    // Check that `clazz` doesn't have a non-default constructor
    //
    val constructorErrors = if (clazz.isInterface) {
      // Case 1: Interface. No need for a constructor.
      List.empty
    } else {
      // Case 2: Class. Must have a non-private zero argument constructor.
      if (hasNonPrivateZeroArgConstructor(clazz))
        List.empty
      else
        List(MissingPublicZeroArgConstructor(clazz, loc))
    }

    //
    // Check that `clazz` is public
    //
    val visibilityErrors = if (!isPublicClass(clazz))
      List(NonPublicClass(clazz, loc))
    else
      List.empty

    //
    // Check that the first argument looks like "this"
    //
    val thisErrors = methods.flatMap {
      case JvmMethod(ident, fparams, _, _, _, _, methodLoc) =>
        // Check that the declared type of `this` matches the type of the class or interface.
        val fparam = fparams.head
        val thisType = Type.eraseAliases(fparam.tpe)
        thisType match {
          case `tpe` => None
          case Type.Unit => Some(MissingThis(clazz, ident.name, methodLoc))
          case _ => Some(IllegalThisType(clazz, fparam.tpe, ident.name, methodLoc))
        }
    }

    val flixMethods = getFlixMethodSignatures(methods)
    val implemented = flixMethods.keySet

    val javaMethods = getJavaMethodSignatures(clazz)
    val objectMethods = getJavaMethodSignatures(classOf[Object]).keySet
    val canImplement = javaMethods.keySet
    val mustImplement = canImplement.filter(m => isAbstractMethod(javaMethods(m)) && !objectMethods.contains(m))

    //
    // Check that there are no unimplemented methods.
    //
    val unimplemented = mustImplement diff implemented
    val unimplementedErrors = unimplemented.map(m => UnimplementedMethod(clazz, javaMethods(m), loc))

    //
    // Check that there are no methods that aren't in the interface
    //
    val extra = implemented diff canImplement
    val extraErrors = extra.map(m => ExtraMethod(clazz, m.name, flixMethods(m).loc))

    constructorErrors ++ visibilityErrors ++ thisErrors ++ unimplementedErrors ++ extraErrors
  }

  /**
    * Represents the signature of a method, used to compare Java signatures against Flix signatures.
    */
  private case class MethodSignature(name: String, paramTypes: List[Type], retTpe: Type)

  /**
    * Convert a list of Flix methods to a set of MethodSignatures. Returns a map to allow subsequent reverse lookup.
    */
  private def getFlixMethodSignatures(methods: List[JvmMethod]): Map[MethodSignature, JvmMethod] = {
    methods.foldLeft(Map.empty[MethodSignature, JvmMethod]) {
      case (acc, m@JvmMethod(ident, fparams, _, retTpe, _, _, _)) =>
        // Drop the first formal parameter (which always represents `this`)
        val paramTypes = fparams.tail.map(_.tpe)
        val signature = MethodSignature(ident.name, paramTypes.map(t => Type.eraseAliases(t)), Type.eraseAliases(retTpe))
        acc + (signature -> m)
    }
  }

  /**
    * Get a Set of MethodSignatures representing the methods of `clazz`. Returns a map to allow subsequent reverse lookup.
    */
  private def getJavaMethodSignatures(clazz: java.lang.Class[_]): Map[MethodSignature, java.lang.reflect.Method] = {
    val methods = clazz.getMethods.toList.filterNot(isStaticMethod)
    methods.foldLeft(Map.empty[MethodSignature, java.lang.reflect.Method]) {
      case (acc, m) =>
        val signature = MethodSignature(m.getName, m.getParameterTypes.toList.map(Type.getFlixType), Type.getFlixType(m.getReturnType))
        acc + (signature -> m)
    }
  }

  /**
    * Return true if the given `clazz` has a non-private zero argument constructor.
    */
  private def hasNonPrivateZeroArgConstructor(clazz: java.lang.Class[_]): Boolean = {
    try {
      val constructor = clazz.getDeclaredConstructor()
      !java.lang.reflect.Modifier.isPrivate(constructor.getModifiers())
    } catch {
      case _: NoSuchMethodException => false
    }
  }

  /**
    * Returns `true` if the given class `c` is public.
    */
  private def isPublicClass(c: java.lang.Class[_]): Boolean =
    java.lang.reflect.Modifier.isPublic(c.getModifiers)

  /**
    * Return `true` if the given method `m` is abstract.
    */
  private def isAbstractMethod(m: java.lang.reflect.Method): Boolean =
    java.lang.reflect.Modifier.isAbstract(m.getModifiers)

  /**
    * Returns `true` if the given method `m` is static.
    */
  private def isStaticMethod(m: java.lang.reflect.Method): Boolean =
    java.lang.reflect.Modifier.isStatic(m.getModifiers)

}

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
    val defsVal = traverse(root.defs) {
      case (_, defn) => visitDef(defn, root)
    }

    //
    // Check if any errors were detected.
    //
    val sendableVal = visitSendable(root)

    mapN(defsVal, sendableVal)((_, _) => root)
  }

  /**
    * Checks that no type parameters for types that implement `Sendable` of kind `Region`
    */
  private def visitSendable(root: Root)(implicit flix: Flix): Validation[List[Instance], CompilationMessage] = {

    val sendableClass = new Symbol.ClassSym(Nil, "Sendable", SourceLocation.Unknown)
    traverse(root.instances.getOrElse(sendableClass, Nil)) {
      case Instance(doc, ann, mod, clazz, tpe, tconstrs, defs, ns, loc) =>
        if (tpe.typeArguments.exists(_.kind == Kind.Bool)) {
          SafetyError.SendableError(tpe, loc).toFailure
        } else {
          Instance(doc, ann, mod, clazz, tpe, tconstrs, defs, ns, loc).toSuccess
        }
    }
  }

  /**
    * Performs safety and well-formedness checks on the given definition `def0`.
    */
  private def visitDef(def0: Def, root: Root)(implicit flix: Flix): Validation[Def, CompilationMessage] = {
    val renv = def0.spec.tparams.map(_.sym).foldLeft(RigidityEnv.empty) {
      case (acc, e) => acc.markRigid(e)
    }
    mapN(visitExp(def0.impl.exp, renv, root))(_ => def0)
  }


  /**
    * Performs safety and well-formedness checks on the given expression `exp0`.
    */
  private def visitExp(e0: Expression, renv: RigidityEnv, root: Root)(implicit flix: Flix): Validation[Expression, CompilationMessage] = {

    def visit(exp0: Expression): Validation[Expression, CompilationMessage] = exp0 match {
      case Expression.Cst(cst, tpe, loc) => Expression.Cst(cst, tpe, loc).toSuccess

      case Expression.Wild(tpe, loc) => Expression.Wild(tpe, loc).toSuccess

      case Expression.Var(sym, tpe, loc) => Expression.Var(sym, tpe, loc).toSuccess

      case Expression.Def(sym, tpe, loc) => Expression.Def(sym, tpe, loc).toSuccess

      case Expression.Sig(sym, tpe, loc) => Expression.Sig(sym, tpe, loc).toSuccess

      case Expression.Hole(sym, tpe, loc) => Expression.Hole(sym, tpe, loc).toSuccess

      case Expression.HoleWithExp(exp, tpe, pur, eff, loc) =>
        val expVal = visit(exp)
        mapN(expVal)(Expression.HoleWithExp(_, tpe, pur, eff, loc))

      case Expression.Use(sym, exp, loc) =>
        val expVal = visit(exp)
        mapN(expVal)(Expression.Use(sym, _, loc))

      case Expression.OpenAs(sym, exp, tpe, loc) =>
        val expVal = visit(exp)
        mapN(expVal)(Expression.OpenAs(sym, _, tpe, loc))

      case Expression.Lambda(fparam, exp, tpe, loc) =>
        val expVal = visit(exp)
        mapN(expVal)(Expression.Lambda(fparam, _, tpe, loc))

      case Expression.Apply(exp, exps, tpe, pur, eff, loc) =>
        val expVal = visit(exp)
        val expsVal = traverse(exps)(visit)
        mapN(expVal, expsVal)(Expression.Apply(_, _, tpe, pur, eff, loc))

      case Expression.Unary(sop, exp, tpe, pur, eff, loc) =>
        val expVal = visit(exp)
        mapN(expVal)(Expression.Unary(sop, _, tpe, pur, eff, loc))

      case Expression.Binary(sop, exp1, exp2, tpe, pur, eff, loc) =>
        val expVal1 = visit(exp1)
        val expVal2 = visit(exp2)
        mapN(expVal1, expVal2)(Expression.Binary(sop, _, _, tpe, pur, eff, loc))

      case Expression.Let(sym, mod, exp1, exp2, tpe, pur, eff, loc) =>
        val expVal1 = visit(exp1)
        val expVal2 = visit(exp2)
        mapN(expVal1, expVal2)(Expression.Let(sym, mod, _, _, tpe, pur, eff, loc))

      case Expression.LetRec(sym, mod, exp1, exp2, tpe, pur, eff, loc) =>
        val expVal1 = visit(exp1)
        val expVal2 = visit(exp2)
        mapN(expVal1, expVal2)(Expression.LetRec(sym, mod, _, _, tpe, pur, eff, loc))

      case Expression.Region(tpe, loc) => Expression.Region(tpe, loc).toSuccess

      case Expression.Scope(sym, regionVar, exp, tpe, pur, eff, loc) =>
        val expVal = visit(exp)
        mapN(expVal)(Expression.Scope(sym, regionVar, _, tpe, pur, eff, loc))

      case Expression.ScopeExit(exp1, exp2, tpe, pur, eff, loc) =>
        val expVal1 = visit(exp1)
        val expVal2 = visit(exp2)
        mapN(expVal1, expVal2)(Expression.ScopeExit(_, _, tpe, pur, eff, loc))

      case Expression.IfThenElse(exp1, exp2, exp3, tpe, pur, eff, loc) =>
        val expVal1 = visit(exp1)
        val expVal2 = visit(exp2)
        val expVal3 = visit(exp3)
        mapN(expVal1, expVal2, expVal3)(Expression.IfThenElse(_, _, _, tpe, pur, eff, loc))

      case Expression.Stm(exp1, exp2, tpe, pur, eff, loc) =>
        val expVal1 = visit(exp1)
        val expVal2 = visit(exp2)
        mapN(expVal1, expVal2)(Expression.Stm(_, _, tpe, pur, eff, loc))

      case Expression.Discard(exp, pur, eff, loc) =>
        val expVal = visit(exp)
        mapN(expVal)(Expression.Discard(_, pur, eff, loc))

      case Expression.Match(exp, rules, tpe, pur, eff, loc) =>
        val expVal = visit(exp)
        val rulesVal = traverse(rules) {
          case MatchRule(p, g, e) =>
            val eVal = visit(e)
            val gexpVal = traverseOpt(g)(visit)
            mapN(gexpVal, eVal)(MatchRule(p, _, _))
        }
        mapN(expVal, rulesVal)(Expression.Match(_, _, tpe, pur, eff, loc))

      case Expression.TypeMatch(exp, rules, tpe, pur, eff, loc) =>
        // check whether the last case in the type match looks like `...: _`
        val missingDefault = rules.last match {
          case MatchTypeRule(sym, tpe, exp) => tpe match {
            case Type.Var(s, _) if renv.isFlexible(s) => MatchTypeRule(sym, tpe, exp).toSuccess
            case _ => SafetyError.MissingDefaultMatchTypeCase(exp.loc).toFailure
          }
        }
        val expVal = visit(exp)
        val rulesVal = traverse(rules) {
          case MatchTypeRule(sym, tpe, mexp) => mapN(visit(mexp))(MatchTypeRule(sym, tpe, _))
        }
        mapN(expVal, rulesVal, missingDefault) {
          (e, rs, _) => Expression.TypeMatch(e, rs, tpe, pur, eff, loc)
        }

      case Expression.RelationalChoose(exps, rules, tpe, pur, eff, loc) =>
        val expsVal = traverse(exps)(visit)
        val rulesVal = traverse(rules) {
          case RelationalChoiceRule(pat, exp) => mapN(visit(exp))(RelationalChoiceRule(pat, _))
        }
        mapN(expsVal, rulesVal)(Expression.RelationalChoose(_, _, tpe, pur, eff, loc))

      case Expression.RestrictableChoose(star, exp, rules, tpe, pur, eff, loc) =>
        val expVal = visit(exp)
        val rulesVal = traverse(rules) {
          case RestrictableChoiceRule(pat, exp1) => mapN(visit(exp1))(RestrictableChoiceRule(pat, _))
        }
        mapN(expVal, rulesVal)(Expression.RestrictableChoose(star, _, _, tpe, pur, eff, loc))

      case Expression.Tag(sym, exp, tpe, pur, eff, loc) =>
        val expVal = visit(exp)
        mapN(expVal)(Expression.Tag(sym, _, tpe, pur, eff, loc))

      case Expression.RestrictableTag(sym, exp, tpe, pur, eff, loc) =>
        val expVal = visit(exp)
        mapN(expVal)(Expression.RestrictableTag(sym, _, tpe, pur, eff, loc))

      case Expression.Tuple(elms, tpe, pur, eff, loc) =>
        val elmsVal = traverse(elms)(visit)
        mapN(elmsVal)(Expression.Tuple(_, tpe, pur, eff, loc))

      case Expression.RecordEmpty(tpe, loc) => Expression.RecordEmpty(tpe, loc).toSuccess

      case Expression.RecordSelect(exp, field, tpe, pur, eff, loc) =>
        val expVal = visit(exp)
        mapN(expVal)(Expression.RecordSelect(_, field, tpe, pur, eff, loc))

      case Expression.RecordExtend(field, value, rest, tpe, pur, eff, loc) =>
        val valueVal = visit(value)
        val restVal = visit(rest)
        mapN(valueVal, restVal)(Expression.RecordExtend(field, _, _, tpe, pur, eff, loc))

      case Expression.RecordRestrict(field, rest, tpe, pur, eff, loc) =>
        val restVal = visit(rest)
        mapN(restVal)(Expression.RecordRestrict(field, _, tpe, pur, eff, loc))

      case Expression.ArrayLit(elms, exp, tpe, pur, eff, loc) =>
        val elmsVal = traverse(elms)(visit)
        val expVal = visit(exp)
        mapN(elmsVal, expVal)(Expression.ArrayLit(_, _, tpe, pur, eff, loc))

      case Expression.ArrayNew(exp1, exp2, exp3, tpe, pur, eff, loc) =>
        val expVal1 = visit(exp1)
        val expVal2 = visit(exp2)
        val expVal3 = visit(exp3)
        mapN(expVal1, expVal2, expVal3)(Expression.ArrayNew(_, _, _, tpe, pur, eff, loc))

      case Expression.ArrayLoad(base, index, tpe, pur, eff, loc) =>
        val baseVal = visit(base)
        val indexVal = visit(index)
        mapN(baseVal, indexVal)(Expression.ArrayLoad(_, _, tpe, pur, eff, loc))

      case Expression.ArrayLength(base, pur, eff, loc) =>
        val baseVal = visit(base)
        mapN(baseVal)(Expression.ArrayLength(_, pur, eff, loc))

      case Expression.ArrayStore(base, index, elm, pur, eff, loc) =>
        val baseVal = visit(base)
        val indexVal = visit(index)
        val elmVal = visit(elm)
        mapN(baseVal, indexVal, elmVal)(Expression.ArrayStore(_, _, _, pur, eff, loc))

      case Expression.ArraySlice(reg, base, beginIndex, endIndex, tpe, pur, eff, loc) =>
        val regVal = visit(reg)
        val baseVal = visit(base)
        val beginIndexVal = visit(beginIndex)
        val endIndexVal = visit(endIndex)
        mapN(regVal, baseVal, beginIndexVal, endIndexVal)(Expression.ArraySlice(_, _, _, _, tpe, pur, eff, loc))

      case Expression.Ref(exp1, exp2, tpe, pur, eff, loc) =>
        val expVal1 = visit(exp1)
        val expVal2 = visit(exp2)
        mapN(expVal1, expVal2)(Expression.Ref(_, _, tpe, pur, eff, loc))

      case Expression.VectorLit(elms, tpe, pur, eff, loc) =>
        val elmsVal = traverse(elms)(visit)
        mapN(elmsVal)(Expression.VectorLit(_, tpe, pur, eff, loc))

      case Expression.VectorLoad(exp1, exp2, tpe, pur, eff, loc) =>
        val expVal1 = visit(exp1)
        val expVal2 = visit(exp2)
        mapN(expVal1, expVal2)(Expression.VectorLoad(_, _, tpe, pur, eff, loc))

      case Expression.VectorLength(exp, loc) =>
        val expVal = visit(exp)
        mapN(expVal)(Expression.VectorLength(_, loc))

      case Expression.Deref(exp, tpe, pur, eff, loc) =>
        val expVal = visit(exp)
        mapN(expVal)(Expression.Deref(_, tpe, pur, eff, loc))

      case Expression.Assign(exp1, exp2, tpe, pur, eff, loc) =>
        val expVal1 = visit(exp1)
        val expVal2 = visit(exp2)
        mapN(expVal1, expVal2)(Expression.Assign(_, _, tpe, pur, eff, loc))

      case Expression.Ascribe(exp, tpe, pur, eff, loc) =>
        val expVal = visit(exp)
        mapN(expVal)(Expression.Ascribe(_, tpe, pur, eff, loc))

      case Expression.Of(sym, exp, tpe, pur, eff, loc) =>
        val expVal = visit(exp)
        mapN(expVal)(Expression.Of(sym, _, tpe, pur, eff, loc))

      case e@Expression.Cast(exp, declaredType, declaredPur, declaredEff, tpe, pur, eff, loc) =>
        val check = checkCastSafety(e)
        val expVal = visit(exp)
        mapN(expVal, check) {
          (e1, _) => Expression.Cast(e1, declaredType, declaredPur, declaredEff, tpe, pur, eff, loc)
        } recoverOne {
          case err: ImpossibleCast => Expression.Error(err, tpe, pur, eff)
        }

      case Expression.Mask(exp, tpe, pur, eff, loc) =>
        val expVal = visit(exp)
        mapN(expVal)(Expression.Mask(_, tpe, pur, eff, loc))

      case Expression.Upcast(exp, tpe, loc) =>
        val check = checkUpcastSafety(exp, tpe, renv, root, loc)
        val expVal = visit(exp)
        mapN(expVal, check) {
          (e, _) => Expression.Upcast(e, tpe, loc)
        } recoverOne {
          case err: UnsafeUpcast => Expression.Error(err, tpe, exp.pur, exp.eff)
        }

      case Expression.Supercast(exp, tpe, loc) =>
        val check = checkSupercastSafety(exp, tpe, loc)
        val expVal = visit(exp)
        mapN(expVal, check) {
          (e, _) => Expression.Supercast(e, tpe, loc)
        }

      case Expression.Without(exp, effUse, tpe, pur, eff, loc) =>
        val expVal = visit(exp)
        mapN(expVal)(Expression.Without(_, effUse, tpe, pur, eff, loc))

      case Expression.TryCatch(exp, rules, tpe, pur, eff, loc) =>
        val expVal = visit(exp)
        val rulesVal = traverse(rules) {
          case CatchRule(sym, clazz, exp) => mapN(visit(exp))(CatchRule(sym, clazz, _))
        }
        mapN(expVal, rulesVal)(Expression.TryCatch(_, _, tpe, pur, eff, loc))

      case Expression.TryWith(exp, effUse, rules, tpe, pur, eff, loc) =>
        val expVal = visit(exp)
        val rulesVal = traverse(rules) {
          case HandlerRule(op, fparams, exp) => mapN(visit(exp))(HandlerRule(op, fparams, _))
        }
        mapN(expVal, rulesVal)(Expression.TryWith(_, effUse, _, tpe, pur, eff, loc))

      case Expression.Do(op, exps, pur, eff, loc) =>
        val expsVal = traverse(exps)(visit)
        mapN(expsVal)(Expression.Do(op, _, pur, eff, loc))

      case Expression.Resume(exp, tpe, loc) =>
        val expVal = visit(exp)
        mapN(expVal)(Expression.Resume(_, tpe, loc))

      case Expression.InvokeConstructor(constructor, args, tpe, pur, eff, loc) =>
        val argsVal = traverse(args)(visit)
        mapN(argsVal)(Expression.InvokeConstructor(constructor, _, tpe, pur, eff, loc))

      case Expression.InvokeMethod(method, exp, args, tpe, pur, eff, loc) =>
        val expVal = visit(exp)
        val argsVal = traverse(args)(visit)
        mapN(expVal, argsVal)(Expression.InvokeMethod(method, _, _, tpe, pur, eff, loc))

      case Expression.InvokeStaticMethod(method, args, tpe, pur, eff, loc) =>
        val argsVal = traverse(args)(visit)
        mapN(argsVal)(Expression.InvokeStaticMethod(method, _, tpe, pur, eff, loc))

      case Expression.GetField(field, exp, tpe, pur, eff, loc) =>
        val expVal = visit(exp)
        mapN(expVal)(Expression.GetField(field, _, tpe, pur, eff, loc))

      case Expression.PutField(field, exp1, exp2, tpe, pur, eff, loc) =>
        val expVal1 = visit(exp1)
        val expVal2 = visit(exp2)
        mapN(expVal1, expVal2)(Expression.PutField(field, _, _, tpe, pur, eff, loc))

      case Expression.GetStaticField(field, tpe, pur, eff, loc) =>
        Expression.GetStaticField(field, tpe, pur, eff, loc).toSuccess

      case Expression.PutStaticField(field, exp, tpe, pur, eff, loc) =>
        val expVal = visit(exp)
        mapN(expVal)(Expression.PutStaticField(field, _, tpe, pur, eff, loc))

      case Expression.NewObject(name, clazz, tpe, pur, eff, methods, loc) =>
        val erasedType = Type.eraseAliases(tpe)
        val objImpl = traverse(checkObjectImplementation(clazz, erasedType, methods, loc))(_.toFailure)
        val methodsVal = traverse(methods) {
          case JvmMethod(ident, fparams, exp, retTpe, pur, eff, loc) =>
            mapN(visit(exp))(JvmMethod(ident, fparams, _, retTpe, pur, eff, loc))
        }
        mapN(methodsVal, objImpl) {
          (ms, _) => Expression.NewObject(name, clazz, tpe, pur, eff, ms, loc)
        } recoverOne {
          case err: SafetyError => Expression.Error(err, tpe, pur, eff)
        }

      case Expression.NewChannel(exp1, exp2, tpe, pur, eff, loc) =>
        val expVal1 = visit(exp1)
        val expVal2 = visit(exp2)
        mapN(expVal1, expVal2)(Expression.NewChannel(_, _, tpe, pur, eff, loc))

      case Expression.GetChannel(exp, tpe, pur, eff, loc) =>
        val expVal = visit(exp)
        mapN(expVal)(Expression.GetChannel(_, tpe, pur, eff, loc))

      case Expression.PutChannel(exp1, exp2, tpe, pur, eff, loc) =>
        val expVal1 = visit(exp1)
        val expVal2 = visit(exp2)
        mapN(expVal1, expVal2)(Expression.PutChannel(_, _, tpe, pur, eff, loc))

      case Expression.SelectChannel(rules, default, tpe, pur, eff, loc) =>
        val rulesVal = traverse(rules) {
          case SelectChannelRule(sym, chan, body) =>
            val chanVal = visit(chan)
            val bodyVal = visit(body)
            mapN(chanVal, bodyVal)(SelectChannelRule(sym, _, _))
        }
        val defaultVal = traverseOpt(default)(visit)
        mapN(rulesVal, defaultVal)(Expression.SelectChannel(_, _, tpe, pur, eff, loc))

      case Expression.Spawn(exp1, exp2, tpe, pur, eff, loc) =>
        val expVal1 = visit(exp1)
        val expVal2 = visit(exp2)
        mapN(expVal1, expVal2)(Expression.Spawn(_, _, tpe, pur, eff, loc))

      case Expression.Par(exp, loc) =>
        // Only tuple expressions are allowed to be parallelized with `par`.
        flatMapN(visit(exp)) {
          case e: Expression.Tuple => Expression.Par(e, loc).toSuccess
          case e => e.toSoftFailure(IllegalParExpression(e, e.loc))
        }

      case Expression.ParYield(frags, exp, tpe, pur, eff, loc) =>
        val fragsVal = traverse(frags) {
          case ParYieldFragment(pat, exp, loc) => mapN(visit(exp))(ParYieldFragment(pat, _, loc))
        }
        val expVal = visit(exp)
        mapN(fragsVal, expVal)(Expression.ParYield(_, _, tpe, pur, eff, loc))

      case Expression.Lazy(exp, tpe, loc) =>
        val expVal = visit(exp)
        mapN(expVal)(Expression.Lazy(_, tpe, loc))

      case Expression.Force(exp, tpe, pur, eff, loc) =>
        val expVal = visit(exp)
        mapN(expVal)(Expression.Force(_, tpe, pur, eff, loc))

      case Expression.FixpointConstraintSet(cs, stf, tpe, loc) =>
        val csVal = traverse(cs)(checkConstraint(_, renv, root))
        mapN(csVal)(Expression.FixpointConstraintSet(_, stf, tpe, loc))

      case Expression.FixpointLambda(pparams, exp, stf, tpe, pur, eff, loc) =>
        val expVal = visit(exp)
        mapN(expVal)(Expression.FixpointLambda(pparams, _, stf, tpe, pur, eff, loc))

      case Expression.FixpointMerge(exp1, exp2, stf, tpe, pur, eff, loc) =>
        val expVal1 = visit(exp1)
        val expVal2 = visit(exp2)
        mapN(expVal1, expVal2)(Expression.FixpointMerge(_, _, stf, tpe, pur, eff, loc))

      case Expression.FixpointSolve(exp, stf, tpe, pur, eff, loc) =>
        val expVal = visit(exp)
        mapN(expVal)(Expression.FixpointSolve(_, stf, tpe, pur, eff, loc))

      case Expression.FixpointFilter(pred, exp, tpe, pur, eff, loc) =>
        val expVal = visit(exp)
        mapN(expVal)(Expression.FixpointFilter(pred, _, tpe, pur, eff, loc))

      case Expression.FixpointInject(exp, pred, tpe, pur, eff, loc) =>
        val expVal = visit(exp)
        mapN(expVal)(Expression.FixpointInject(_, pred, tpe, pur, eff, loc))

      case Expression.FixpointProject(pred, exp, tpe, pur, eff, loc) =>
        val expVal = visit(exp)
        mapN(expVal)(Expression.FixpointProject(pred, _, tpe, pur, eff, loc))

      case Expression.Error(m, tpe, pur, eff) => Expression.Error(m, tpe, pur, eff).toSuccess

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
  private def checkCastSafety(cast: Expression.Cast)(implicit flix: Flix): Validation[Expression, ImpossibleCast] = {
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
      case (Type.Var(_, _), _) => cast.toSuccess
      case (_, Some(Type.Var(_, _))) => cast.toSuccess

      // Allow anything with Java interop
      case (Type.Cst(TypeConstructor.Native(_), _), _) => cast.toSuccess
      case (_, Some(Type.Cst(TypeConstructor.Native(_), _))) => cast.toSuccess

      // Boolean primitive to other primitives
      case (Type.Bool, Some(t2)) if primitives.filter(_ != Type.Bool).contains(t2) =>
        ImpossibleCast(cast.exp.tpe, cast.declaredType.get, cast.loc).toFailure

      // Symmetric case
      case (t1, Some(Type.Bool)) if primitives.filter(_ != Type.Bool).contains(t1) =>
        ImpossibleCast(cast.exp.tpe, cast.declaredType.get, cast.loc).toFailure

      // JVM Reference types and primitives
      case (t1, Some(t2)) if primitives.contains(t1) && !primitives.contains(t2) =>
        ImpossibleCast(cast.exp.tpe, cast.declaredType.get, cast.loc).toFailure

      // Symmetric case
      case (t1, Some(t2)) if primitives.contains(t2) && !primitives.contains(t1) =>
        ImpossibleCast(cast.exp.tpe, cast.declaredType.get, cast.loc).toFailure

      case _ => cast.toSuccess
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
      val isEffSubset = Unification.unifiesWith(s1s3, s2, renv)

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
  private def checkUpcastSafety(exp: Expression, tpe: Type, renv: RigidityEnv, root: Root, loc: SourceLocation)(implicit flix: Flix): Validation[Expression, UnsafeUpcast] = {
    val tpe1 = Type.eraseAliases(exp.tpe)
    val tpe2 = Type.eraseAliases(tpe)
    if (isSubtypeOf(tpe1, tpe2, renv, root))
      exp.toSuccess
    else
      UnsafeUpcast(exp.tpe, tpe, loc).toFailure
  }

  /**
    * Returns a list of errors if the the supercast is invalid.
    */
  private def checkSupercastSafety(exp: Expression, tpe: Type, loc: SourceLocation)(implicit flix: Flix): Validation[Expression, CompilationMessage] = {
    val tpe1 = Type.eraseAliases(exp.tpe)
    val tpe2 = Type.eraseAliases(tpe)
    if (isJavaSubtypeOf(tpe1, tpe2))
      exp.toSuccess
    else
      collectSupercastErrors(exp, tpe, loc)
  }

  /**
    * Returns a list of supercast errors.
    */
  private def collectSupercastErrors(exp: Expression, tpe: Type, loc: SourceLocation)(implicit flix: Flix): Validation[Expression, CompilationMessage] = {
    val tpe1 = Type.eraseAliases(exp.tpe)
    val tpe2 = Type.eraseAliases(tpe)

    (tpe1.baseType, tpe2.baseType) match {

      case (Type.Cst(TypeConstructor.Native(left), _), Type.Cst(TypeConstructor.Native(right), _)) =>
        if (right.isAssignableFrom(left)) exp.toSuccess else UnsafeSupercast(exp.tpe, tpe, loc).toFailure

      case (Type.Cst(TypeConstructor.Str, _), Type.Cst(TypeConstructor.Native(right), _)) =>
        if (right.isAssignableFrom(classOf[String])) exp.toSuccess else UnsafeSupercast(exp.tpe, tpe, loc).toFailure

      case (Type.Cst(TypeConstructor.BigInt, _), Type.Cst(TypeConstructor.Native(right), _)) =>
        if (right.isAssignableFrom(classOf[BigInteger])) exp.toSuccess else UnsafeSupercast(exp.tpe, tpe, loc).toFailure

      case (Type.Cst(TypeConstructor.BigDecimal, _), Type.Cst(TypeConstructor.Native(right), _)) =>
        if (right.isAssignableFrom(classOf[java.math.BigDecimal])) exp.toSuccess else UnsafeSupercast(exp.tpe, tpe, loc).toFailure

      case (Type.Var(_, _), _) =>
        FromTypeVariableSupercast(exp.tpe, tpe, loc).toFailure

      case (_, Type.Var(_, _)) =>
        ToTypeVariableSupercast(exp.tpe, tpe, loc).toFailure

      case (Type.Cst(TypeConstructor.Native(clazz), _), _) =>
        ToNonJavaTypeSupercast(clazz, tpe, loc).toFailure

      case (_, Type.Cst(TypeConstructor.Native(clazz), _)) =>
        FromNonJavaTypeSupercast(exp.tpe, clazz, loc).toFailure

      case _ => UnsafeSupercast(exp.tpe, tpe, loc).toFailure

    }
  }

  /**
    * Performs safety and well-formedness checks on the given constraint `c0`.
    */
  private def checkConstraint(c0: Constraint, renv: RigidityEnv, root: Root)(implicit flix: Flix): Validation[Constraint, CompilationMessage] = {
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
    val bodyVal = traverse(c0.body)(checkBodyPredicate(_, posVars, quantVars, latVars, renv, root))

    //
    // Check that the free relational variables in the head atom are not lattice variables.
    //
    val headVal = checkHeadPredicate(c0.head, unsafeLatVars)

    //
    // Check that patterns in atom body are legal
    //
    val bodyPatternVal = traverse(c0.body)(checkBodyPattern)

    mapN(bodyVal, headVal, bodyPatternVal)((b, h, _) => Constraint(c0.cparams, h, b, c0.loc))
  }

  /**
    * Performs safety check on the pattern of an atom body.
    */
  private def checkBodyPattern(p0: Predicate.Body): Validation[List[Pattern], CompilationMessage] = p0 match {
    case Predicate.Body.Atom(_, _, _, _, terms, _, loc) =>
      traverse(terms) {
        case Pattern.Var(sym, tpe, loc) => Pattern.Var(sym, tpe, loc).toSuccess
        case Pattern.Wild(tpe, loc) => Pattern.Wild(tpe, loc).toSuccess
        case Pattern.Cst(cst, tpe, loc) => Pattern.Cst(cst, tpe, loc).toSuccess
        case _ => UnexpectedPatternInBodyAtom(loc).toFailure
      }
    case _ => SuccessNil
  }

  /**
    * Performs safety and well-formedness checks on the given body predicate `p0`
    * with the given positively defined variable symbols `posVars`.
    */
  private def checkBodyPredicate(p0: Predicate.Body, posVars: Set[Symbol.VarSym], quantVars: Set[Symbol.VarSym], latVars: Set[Symbol.VarSym], renv: RigidityEnv, root: Root)(implicit flix: Flix): Validation[Predicate.Body, CompilationMessage] = p0 match {
    case Predicate.Body.Atom(pred, den, polarity, fixity, terms, tpe, loc) =>
      // check for non-positively bound negative variables.
      val err1 = polarity match {
        case Polarity.Positive => Polarity.Positive.toSuccess
        case Polarity.Negative =>
          // Compute the free variables in the terms which are *not* bound by the lexical scope.
          val freeVars = terms.flatMap(freeVarsOf).toSet intersect quantVars
          val wildcardNegErrors = visitPats(terms, loc)

          // Check if any free variables are not positively bound.
          val variableNegErrors = traverse(freeVars -- posVars)(makeIllegalNonPositivelyBoundVariableError(_, loc).toFailure)

          mapN(wildcardNegErrors, variableNegErrors)((_, _) => Polarity.Negative)
      }
      // check for relational use of lattice variables. We still look at fixed
      // atoms since latVars (which means that they occur non-fixed) cannot be
      // in another fixed atom.
      val relTerms = den match {
        case Denotation.Relational => terms
        case Denotation.Latticenal => terms.dropRight(1)
      }
      val err2 = mapN(traverse(relTerms.flatMap(freeVarsOf).filter(latVars.contains)) {
        s => IllegalRelationalUseOfLatticeVariable(s, loc).toFailure
      })(_ => den)

      // Combine the messages
      mapN(err2, err1)(Predicate.Body.Atom(pred, _, _, fixity, terms, tpe, loc))

    case Predicate.Body.Guard(exp, loc) => mapN(visitExp(exp, renv, root))(Predicate.Body.Guard(_, loc))

    case Predicate.Body.Loop(varSyms, exp, loc) => mapN(visitExp(exp, renv, root))(Predicate.Body.Loop(varSyms, _, loc))
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
  private def checkHeadPredicate(head: Predicate.Head, latVars: Set[Symbol.VarSym]): Validation[Predicate.Head, CompilationMessage] = head match {
    case Predicate.Head.Atom(pred, Denotation.Latticenal, terms, tpe, loc) =>
      // Check the relational terms ("the keys").
      mapN(checkTerms(terms.dropRight(1), latVars, loc))(Predicate.Head.Atom(pred, Denotation.Latticenal, _, tpe, loc))
    case Predicate.Head.Atom(pred, Denotation.Relational, terms, tpe, loc) =>
      // Check every term.
      mapN(checkTerms(terms, latVars, loc))(Predicate.Head.Atom(pred, Denotation.Relational, _, tpe, loc))
  }

  /**
    * Checks that the free variables of the terms does not contain any of the variables in `latVars`.
    * If they do contain a lattice variable then a `IllegalRelationalUseOfLatticeVariable` is created.
    */
  private def checkTerms(terms: List[Expression], latVars: Set[Symbol.VarSym], loc: SourceLocation): Validation[List[Expression], CompilationMessage] = {
    // Compute the free variables in all terms.
    val allVars = terms.foldLeft(Set.empty[Symbol.VarSym])({
      case (acc, term) => acc ++ freeVars(term).keys
    })

    // TODO: Change structure to go over expressions individually
    // Compute the lattice variables that are illegally used in the terms.
    mapN(traverse(allVars.intersect(latVars).toList)(IllegalRelationalUseOfLatticeVariable(_, loc).toFailure))(_ => terms)
  }

  /**
    * Returns an error for each occurrence of wildcards in each term.
    *
    * @param loc the location of the atom containing the terms.
    */
  private def visitPats(terms: List[Pattern], loc: SourceLocation): Validation[List[Pattern], CompilationMessage] = {
    traverse(terms)(visitPat(_, loc))
  }

  /**
    * Returns an error for each occurrence of wildcards.
    *
    * @param l the location of the atom containing the term.
    */
  private def visitPat(term: Pattern, l: SourceLocation): Validation[Pattern, CompilationMessage] = term match {
    case Pattern.Wild(_, _) => IllegalNegativelyBoundWildcard(l).toFailure
    case Pattern.Var(sym, tpe, loc) => Pattern.Var(sym, tpe, loc).toSuccess
    case Pattern.Cst(cst, tpe, loc) => Pattern.Cst(cst, tpe, loc).toSuccess
    case Pattern.Tag(sym, pat, tpe, loc) => mapN(visitPat(pat, l))(Pattern.Tag(sym, _, tpe, loc))
    case Pattern.Tuple(elms, tpe, loc) => mapN(visitPats(elms, l))(Pattern.Tuple(_, tpe, loc))
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
      !java.lang.reflect.Modifier.isPrivate(constructor.getModifiers)
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

  /**
    * Represents the signature of a method, used to compare Java signatures against Flix signatures.
    */
  private case class MethodSignature(name: String, paramTypes: List[Type], retTpe: Type)

}

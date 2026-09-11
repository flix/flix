package ca.uwaterloo.flix.language.phase.typer

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.{Kind, KindedAst, SourceLocation, Symbol, Type, TypeConstructor, TypedAst}
import ca.uwaterloo.flix.language.errors.DefaultHandlerError
import ca.uwaterloo.flix.language.phase.unification.EqualityEnv
import ca.uwaterloo.flix.util.Result
import ca.uwaterloo.flix.util.collection.Nel

import scala.collection.mutable

object DefaultHandlers {

  /**
    * Returns the valid default handlers in `root` together with the errors of the invalid ones.
    */
  def visitDefaultHandlers(root: KindedAst.Root)(implicit eqEnv: EqualityEnv, flix: Flix): (List[TypedAst.DefaultHandler], List[DefaultHandlerError]) = {
    val handlerDefs = root.defs.toList.filter {
      case (_, defn) => defn.spec.ann.isDefaultHandler
    }

    // We check every handler and report the errors of all invalid ones.
    val results = handlerDefs.map {
      case (sym, defn) => checkHandler(sym, defn, root)
    }
    val validHandlers = results.collect { case Result.Ok(handler) => handler }
    val errors = results.collect { case Result.Err(errs) => errs }

    // Check for [[DefaultHandlerError.DuplicateHandler]].
    val duplicateErrors = mutable.ListBuffer.empty[DefaultHandlerError]
    val seen = mutable.Map.empty[Symbol.EffSym, SourceLocation]
    for (TypedAst.DefaultHandler(handlerSym, handledSym) <- validHandlers) {
      val loc1 = handlerSym.loc
      seen.get(handledSym) match {
        case None =>
          seen.put(handledSym, loc1)
        case Some(loc2) =>
          duplicateErrors += DefaultHandlerError.DuplicateHandler(handledSym, loc1, loc2)
          duplicateErrors += DefaultHandlerError.DuplicateHandler(handledSym, loc2, loc1)
      }
    }

    (validHandlers, errors.flatten ++ duplicateErrors.toList)
  }

  /**
    * Validates that the function `handlerDef` marked with `@DefaultHandler` is a valid default handler.
    *
    * A valid default handler must:
    *   - be public,
    *   - reside in the companion module of an existing effect `E`,
    *   - have no trait or equality constraints,
    *   - take exactly one argument of type `Unit -> a \ ef` where `a` and `ef` are type variables,
    *   - return `a`, and
    *   - have an effect equivalent to `(ef - E[t1, ..., tn]) + IO` where `t1, ..., tn` are distinct type variables.
    *
    * For example:
    * {{{
    *     eff E[t]
    *     mod E {
    *         @DefaultHandler
    *         pub def handle(f: Unit -> a \ ef): a \ (ef - E[t]) + IO = exp
    *     }
    * }}}
    *
    * Every violated rule is reported separately.
    *
    * @return [[Result.Ok]] of the [[TypedAst.DefaultHandler]] if the handler is valid, [[Result.Err]] of the errors otherwise.
    */
  private def checkHandler(handlerSym: Symbol.DefnSym, handlerDef: KindedAst.Def, root: KindedAst.Root)(implicit eqEnv: EqualityEnv, flix: Flix): Result[TypedAst.DefaultHandler, List[DefaultHandlerError]] = {
    // All default handlers must be public.
    val pubErrors = if (handlerDef.spec.mod.isPublic) Nil else {
      List(DefaultHandlerError.NonPublicHandler(handlerSym, handlerSym.loc))
    }

    // The default handler must reside in the companion module of the effect. Hence we use the
    // namespace of the handler to construct the expected effect symbol and look it up in the AST.
    // Companion effects have their symbol in the parent namespace (e.g. Fs.Glob), which matches
    // the handler's namespace (also Fs.Glob).
    val effSym = Symbol.mkEffSym(handlerSym.namespace.mkString("."))
    val signatureErrors = root.effects.get(effSym) match {
      case None =>
        // We cannot check the signature without knowing the effect.
        List(DefaultHandlerError.NotInCompanionModule(handlerSym, handlerSym.loc))
      case Some(effect) =>
        checkSignature(handlerSym, handlerDef.spec, effect)
    }

    pubErrors ++ signatureErrors match {
      case Nil => Result.Ok(TypedAst.DefaultHandler(handlerSym, effSym))
      case errors => Result.Err(errors)
    }
  }

  /**
    * Returns the errors in the signature `spec` of the default handler `handlerSym` for `effect`.
    *
    * The constraints and the arity are checked independently of the rest of the signature. The return
    * type and the effect are only checked when the parameter is well-formed, since they refer to the
    * type variables `a` and `ef` of the parameter type `Unit -> a \ ef`.
    */
  private def checkSignature(handlerSym: Symbol.DefnSym, spec: KindedAst.Spec, effect: KindedAst.Effect)(implicit eqEnv: EqualityEnv, flix: Flix): List[DefaultHandlerError] = {
    val loc = handlerSym.loc.asSynthetic

    // The handled effect applied to its own type parameters, e.g. `E[t]`. Only used in error messages.
    val handledEff = mkEffectType(effect, loc)

    // A default handler must not have trait or equality constraints. Otherwise it would not be applicable
    // to every entry point, since the constraints are not checked when the handler is applied.
    val constraintLocs = spec.tconstrs.map(_.loc) ++ spec.econstrs.map(_.loc)
    val constraintErrors = constraintLocs.map(DefaultHandlerError.IllegalConstraint(handlerSym, handledEff, _))

    // A default handler must take exactly one argument. We highlight the first extraneous argument.
    val Nel(fparam, extraParams) = spec.fparams
    val arityErrors = extraParams.headOption.toList.map {
      extra => DefaultHandlerError.IllegalArity(handlerSym, handledEff, spec.fparams.size, extra.loc)
    }

    // The argument must be a thunk `Unit -> a \ ef` where `a` and `ef` are type variables.
    val shapeErrors = returnTypeAndEff(fparam.tpe) match {
      case None =>
        List(DefaultHandlerError.IllegalParameterType(handlerSym, handledEff, fparam.tpe, fparam.tpe.loc))
      case Some((a, ef)) =>
        checkReturnType(handlerSym, spec, handledEff, a) ++ checkEffect(handlerSym, spec, effect, handledEff, a, ef, loc)
    }

    constraintErrors ++ arityErrors ++ shapeErrors
  }

  /**
    * Returns the return type `a` and the effect `ef` of the thunk type `tpe` if it is `Unit -> a \ ef` where `a` and
    * `ef` are type variables, and `None` otherwise.
    */
  private def returnTypeAndEff(tpe: Type): Option[(Type.Var, Type.Var)] = {
    val t = Type.eraseAliases(tpe)
    t.typeConstructor match {
      case Some(TypeConstructor.Arrow(2)) =>
        (t.arrowArgTypes, t.arrowEffectType, t.arrowResultType) match {
          case (List(Type.Cst(TypeConstructor.Unit, _)), ef: Type.Var, a: Type.Var) => Some((a, ef))
          case _ => None
        }
      case _ => None
    }
  }

  /**
    * Returns an error if the return type of `spec` is not the type variable `a` of the thunk `Unit -> a \ ef`.
    */
  private def checkReturnType(handlerSym: Symbol.DefnSym, spec: KindedAst.Spec, handledEff: Type, a: Type.Var)(implicit flix: Flix): List[DefaultHandlerError] = {
    if (Type.eraseAliases(spec.tpe) == a) {
      Nil
    } else {
      List(DefaultHandlerError.IllegalReturnType(handlerSym, handledEff, a, spec.tpe, spec.tpe.loc))
    }
  }

  /**
    * Returns the errors in the effect of `spec`, which must be equivalent to `(ef - E[t1, ..., tn]) + IO`
    * where `E` is `effect`, `ef` is the effect variable of the thunk, and `t1, ..., tn` are distinct type
    * variables that are different from `a` and `ef`.
    */
  private def checkEffect(handlerSym: Symbol.DefnSym, spec: KindedAst.Spec, effect: KindedAst.Effect, handledEff: Type, a: Type.Var, ef: Type.Var, loc: SourceLocation)(implicit eqEnv: EqualityEnv, flix: Flix): List[DefaultHandlerError] = {
    // A missing effect annotation means the handler is pure.
    val eff = spec.eff.getOrElse(Type.Pure)
    val effLoc = spec.eff.map(_.loc).getOrElse(spec.tpe.loc)

    Type.findEffect(effect.sym, eff) match {
      case None =>
        // The handled effect does not occur in the declared effect, so it cannot be removed from `ef`.
        List(DefaultHandlerError.MissingHandledEffect(handlerSym, handledEff, eff, effLoc))

      case Some(handled) =>
        // The handler must handle `E[t1, ..., tn]` for *all* type arguments. Hence the arguments must be
        // distinct type variables that do not occur elsewhere in the signature.
        val args = handled.typeArguments
        val argsAreDistinctVars = args.forall(_.isInstanceOf[Type.Var]) && (a :: ef :: args).distinct.size == args.size + 2
        val argErrors = if (argsAreDistinctVars) Nil else {
          List(DefaultHandlerError.IllegalEffectArguments(handlerSym, handledEff, handled, handled.loc))
        }

        // The declared effect must be *equivalent* to `(ef - E[t1, ..., tn]) + IO`, so e.g. `IO + (ef - E)` is fine.
        val expectedEff = Type.mkUnion(Type.mkDifference(ef, handled, loc), Type.IO, loc)
        val effErrors = if (ConstraintSolver2.isEquivalent(Type.eraseAliases(eff), expectedEff)) Nil else {
          List(DefaultHandlerError.IllegalEffect(handlerSym, handledEff, expectedEff, eff, effLoc))
        }

        argErrors ++ effErrors
    }
  }

  /**
    * Returns the type of `effect` applied to its own type parameters, e.g. `E[t1, ..., tn]`.
    */
  private def mkEffectType(effect: KindedAst.Effect, loc: SourceLocation): Type = {
    val tparams = effect.tparams.map(tparam => Type.Var(tparam.sym, loc))
    val kind = Kind.mkArrowTo(tparams.map(_.kind), Kind.Eff)
    Type.mkApply(Type.Cst(TypeConstructor.Effect(effect.sym, kind), loc), tparams, loc)
  }

}

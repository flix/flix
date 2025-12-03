/*
 * Copyright 2022 Matthew Lutze
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.shared.*
import ca.uwaterloo.flix.language.ast.shared.BoundBy.FormalParam
import ca.uwaterloo.flix.language.ast.shared.SymUse.DefSymUse
import ca.uwaterloo.flix.language.ast.{Kind, RigidityEnv, Scheme, SourceLocation, Symbol, Type, TypeConstructor, TypedAst}
import ca.uwaterloo.flix.language.dbg.AstPrinter.*
import ca.uwaterloo.flix.language.errors.EntryPointError
import ca.uwaterloo.flix.language.phase.typer.{ConstraintSolver2, SubstitutionTree, TypeConstraint}
import ca.uwaterloo.flix.runtime.shell.Shell
import ca.uwaterloo.flix.util.collection.{Chain, CofiniteSet}
import ca.uwaterloo.flix.util.{InternalCompilerException, ParOps, Result, Validation}

import java.util.concurrent.ConcurrentLinkedQueue
import java.util.concurrent.atomic.AtomicBoolean
import scala.annotation.tailrec
import scala.collection.immutable.SortedSet
import scala.collection.mutable
import scala.jdk.CollectionConverters.*

/**
  * Processes all entry points of the program.
  *
  * A function is an entry point if:
  *   - It is the main function (called `main` by default, but can configured to an arbitrary name).
  *   - It is a test (annotated with `@Test`).
  *   - It is an exported function (annotated with `@Export`).
  *
  * This phase has these sub-phases:
  *   - Resolve the entrypoint option so that there is no implicit default entry point.
  *   - Check that all entry points have valid signatures, where rules differ from main, tests, and
  *     exports. If an entrypoint does not have a valid signature, its related annotation is
  *     removed to allow further compilation to continue with valid assumptions.
  *   - Wrap entry points with default effect handlers
  *   - Replace the existing main function by a new main function that prints the returned value if
  *     its return type is not Unit.
  *   - Compute the set of all entry points and store it in Root.
  */
object EntryPoints {

  private case object ErrorOrMalformed

  // We don't use regions, so we are safe to use the global scope everywhere in this phase.
  private implicit val S: Scope = Scope.Top


  def run(root: TypedAst.Root)(implicit flix: Flix): (TypedAst.Root, List[EntryPointError]) = flix.phaseNew("EntryPoints") {
    val (root1, errs1) = resolveMain(root)
    // Wrap entry points to handle non-primitive effects with defaultHandlers
    val (rootWithWrappedEntryPoints, errsDefaultHandlers) = WrapInDefaultHandlers.run(root1)
    val (root2, errs2) = CheckEntryPoints.run(rootWithWrappedEntryPoints)
    // WrapTest and WrapMain assumes sensible tests, so CheckEntryPoints must run first.
    // Notice that a main function with test annotation will be wrapped twice.
    val root3 = WrapMain.run(root2)
    // WrapMain might change main, so findEntryPoints must be after.
    val root4 = findEntryPoints(root3)
    (root4, errs1 ++ errs2 ++ errsDefaultHandlers)
  }

  /**
    * Converts [[TypedAst.Root.mainEntryPoint]] to be explicit instead of implicit and checks that a
    * given entry point exists.
    *
    * In the input, a None entrypoint means to use `main` if it exists.
    * In the output, None means no entrypoint and Some is an entrypoint, guaranteed to be in defs.
    */
  private def resolveMain(root: TypedAst.Root): (TypedAst.Root, List[EntryPointError]) = {
    val defaultMainName = Symbol.mkDefnSym("main")

    root.mainEntryPoint match {
      case None =>
        root.defs.get(defaultMainName) match {
          case None =>
            // No main is given and default does not exist - no main.
            (root, Nil)
          case Some(entryPoint) =>
            // No main is given but default exists - use default.
            (root.copy(mainEntryPoint = Some(entryPoint.sym)), Nil)
        }
      case Some(sym) => root.defs.get(sym) match {
        case Some(shell) if shell.sym.name == Shell.ShellEntryPointName =>
          // A main is given and it is the shell's main - transform it.
          val newShell = rewriteShellEntryPoint(shell)
          (root.copy(defs = root.defs + (shell.sym -> newShell)), Nil)
        case Some(_) =>
          // A main is given and it exists - use it.
          (root, Nil)
        case None =>
          // A main is given and it does not exist - no main and give an error.
          (root.copy(mainEntryPoint = None), List(EntryPointError.MainEntryPointNotFound(sym)))
      }
    }
  }

  /**
    * Returns a new shell function that instead of returning unit, calls the local function _f.
    * This function _f simply prints the expression to be evaluated in the shell.
    *
    * Takes
    * {{{
    *   def main(): Unit \ IO + NonDet + Chan =
    *     def _f()={ println(exp) }
    *     checked_ecast(())
    * }}}
    *
    * Returns
    * {{{
    *   def main(): Unit \ expEffs + IO =
    *     def _f()={ println(exp) }
    *     _f()
    * }}}
    *
    * This is necessary because the expression being evaluated may have non primitive effects
    * that have default handlers. The set of effects with default handlers is not known from the start,
    * unlike the set of all primitive effects, so we have to add them after typing.
    *
    * In order to support the current way of creating a shell (create some simple wrapping code around exp and
    * execute it), we need to:
    *
    *   - Replace checked_ecast(()) by a call to _f.
    *   - Substitute (IO + NonDet + Chan) with the real set of effects generated by exp at EntryPoints plus an
    *   additional IO due to `println`.
    *
    */
  private def rewriteShellEntryPoint(oldShell: TypedAst.Def): TypedAst.Def = {
    val exp = oldShell.exp.asInstanceOf[TypedAst.Expr.LocalDef]
    val tpe = exp.bnd.tpe.asInstanceOf[Type.Apply]
    val spec = oldShell.spec.copy(
      // Replace main's effects with the effects of _f
      eff = tpe.tpe2,
    )
    // Substitute checked_ecast(()) for the contents of _f
    // Namely, println(exp)
    val newExp = exp.copy(exp2 = exp.exp1 )
    oldShell.copy(spec = spec, exp = newExp)
  }

  /**
    * A function is an entry point if:
    *   - It is the main function (called `main` by default, but can configured
    *     to an arbitrary name).
    *   - It is a test (annotated with `@Test`).
    *   - It is an exported function (annotated with `@Export`).
    */
  private def isEntryPoint(defn: TypedAst.Def)(implicit root: TypedAst.Root): Boolean =
    isMain(defn) || isTest(defn) || isExport(defn)

  /** Returns `true` if `defn` is a test. */
  private def isTest(defn: TypedAst.Def): Boolean =
    defn.spec.ann.isTest

  /** Returns `true` if `defn` is an exported function. */
  private def isExport(defn: TypedAst.Def): Boolean =
    defn.spec.ann.isExport

  /** Returns `true` if `defn` is the main function. */
  private def isMain(defn: TypedAst.Def)(implicit root: TypedAst.Root): Boolean =
    root.mainEntryPoint.contains(defn.sym)

  /** Returns `true` if `tpe` is equivalent to Unit (via type aliases). */
  @tailrec
  private def isUnitType(tpe: Type): Result[Boolean, ErrorOrMalformed.type] = tpe match {
    case Type.Cst(TypeConstructor.Unit, _) => Result.Ok(true)
    case Type.Cst(_, _) => Result.Ok(false)
    case Type.Apply(_, _, _) => Result.Ok(false)
    case Type.Alias(_, _, t, _) => isUnitType(t)
    case Type.Var(_, _) => Result.Err(ErrorOrMalformed)
    case Type.AssocType(_, _, _, _) => Result.Err(ErrorOrMalformed)
    case Type.JvmToType(_, _) => Result.Err(ErrorOrMalformed)
    case Type.JvmToEff(_, _) => Result.Err(ErrorOrMalformed)
    case Type.UnresolvedJvmType(_, _) => Result.Err(ErrorOrMalformed)
  }

  private object CheckEntryPoints {

    /**
      * CheckEntryPoints checks that all entry points (main/test/export) have valid signatures.
      *
      * Because of resilience, invalid entry points are not discarded. Its entry point marker is
      * removed (removed as the main function in root or have its annotation removed).
      */
    def run(root: TypedAst.Root)(implicit flix: Flix): (TypedAst.Root, List[EntryPointError]) = {
      implicit val sctx: SharedContext = SharedContext.mk()
      implicit val r: TypedAst.Root = root

      ParOps.parMapValues(root.defs)(visitDef(_))

      // Remove the entrypoint if it is not valid.
      val root1 = if (sctx.invalidMain.get()) root.copy(mainEntryPoint = None) else root
      val errs = sctx.errors.asScala.toList
      (root1, errs)
    }

    /**
      * Checks `defn` with relevant checks for its entry point kind (main/test/export).
      *
      * Because of resilience, invalid entry points are not discarded. Its entry point marker is
      * removed (removed as the main function in root or have its annotation removed).
      *
      * A function can be main, a test, and exported at the same time.
      */
    private def visitDef(defn: TypedAst.Def)(implicit sctx: SharedContext, root: TypedAst.Root, flix: Flix): TypedAst.Def = {
      // checkMain is different than the other two because the entry point designation exists on
      // root and invalid main functions are communicated via SharedContext.
      if (isMain(defn)) checkMain(defn)
      val defn1 = if (isTest(defn)) visitTest(defn) else defn
      val defn2 = if (isExport(defn)) visitExport(defn1) else defn1
      defn2
    }

    /**
      * Rules for main - it has:
      *   - No type variables.
      *   - One parameter of type Unit.
      *   - An effect that is a subset of the primitive effects.
      *   - Return type Unit or return type `t` where `ToString[t]` exists
      *     (This is split into two cases to allow compilation without the standard library).
      */
    private def checkMain(defn: TypedAst.Def)(implicit sctx: SharedContext, root: TypedAst.Root, flix: Flix): Unit = {
      val errs = checkNoTypeVariables(defn) match {
        case Some(err) => List(err)
        case None =>
          // Only run these on functions without type variables.
          checkUnitArg(defn) ++ checkToStringOrUnitResult(defn) ++ checkEffects(defn, Symbol.PrimitiveEffs)
      }
      if (errs.nonEmpty) {
        // Invalidate main and add errors.
        sctx.invalidMain.set(true)
        errs.foreach(sctx.errors.add)
      }
    }

    /**
      * Rules for tests - a test has:
      *   - No type variables.
      *   - One parameter of type Unit.
      *   - An effect that is a subset of the primitive effects.
      */
    private def visitTest(defn: TypedAst.Def)(implicit sctx: SharedContext, flix: Flix): TypedAst.Def = {
      val errs = checkNoTypeVariables(defn) match {
        case Some(err) => List(err)
        case None =>
          // Only run these on functions without type variables.
          checkUnitArg(defn) ++ checkEffects(defn, Symbol.TestEffs)
      }
      if (errs.isEmpty) {
        defn
      } else {
        errs.foreach(sctx.errors.add)
        removeTestAnnotation(defn)
      }
    }

    /** Returns `defn` without a test annotation. */
    private def removeTestAnnotation(defn: TypedAst.Def): TypedAst.Def =
      defn.copy(
        spec = defn.spec.copy(
          ann = defn.spec.ann.copy(
            annotations = defn.spec.ann.annotations.filterNot(_.isInstanceOf[Annotation.Test])
          )
        )
      )

    /**
      * Rules for exported functions - an exported function has:
      *   - No type variables.
      *   - An effect that is a subset of the primitive effects.
      *   - Is not in the root namespace.
      *   - Is `pub`.
      *   - Has a name that is valid in Java.
      *   - Has types that are valid in Java (not Flix types like `List[Int32]`).
      */
    private def visitExport(defn: TypedAst.Def)(implicit sctx: SharedContext, flix: Flix): TypedAst.Def = {
      val errs = (checkNoTypeVariables(defn) match {
        case Some(err) => List(err)
        case None =>
          // Only run these on functions without type variables.
          checkEffects(defn, Symbol.PrimitiveEffs).toList ++ checkJavaTypes(defn)
      }) ++
        checkNonRootNamespace(defn) ++
        checkPub(defn) ++
        checkValidJavaName(defn)
      if (errs.isEmpty) {
        defn
      } else {
        errs.foreach(sctx.errors.add)
        removeExportAnnotation(defn)
      }
    }

    /** Returns `defn` without a test annotation. */
    private def removeExportAnnotation(defn: TypedAst.Def): TypedAst.Def =
      defn.copy(
        spec = defn.spec.copy(
          ann = defn.spec.ann.copy(
            annotations = defn.spec.ann.annotations.filterNot(_.isInstanceOf[Annotation.Export])
          )
        )
      )

    /**
      * Returns an error if `defn` has type variables.
      *
      * If a function has no type variables in surface syntax we know that:
      *   - Traits and trait constraints do not occur since their syntax is limited to `Trait[var]`.
      *   - Associated types do not occur since their syntax is limited to `Trait.Assoc[var]`.
      *   - Equality constraints do not occur since their syntax is limited to
      *     `Trait.Assoc[var] ~ type`
      */
    private def checkNoTypeVariables(defn: TypedAst.Def): Option[EntryPointError] = {
      // `tparams` lies sometimes when explicit tparams are given and _ are used.
      val monomorphic = defn.spec.tparams.isEmpty && typesOf(defn).forall(_.typeVars.isEmpty)
      if (monomorphic) None
      else Some(EntryPointError.IllegalEntryPointTypeVariables(defn.sym.loc))
    }

    /** Returns all the types in the signature of `defn`. */
    private def typesOf(defn: TypedAst.Def): List[Type] = {
      defn.spec.fparams.map(_.tpe) ++
        List(defn.spec.retTpe) ++
        List(defn.spec.eff) ++
        defn.spec.tconstrs.map(_.arg) ++
        defn.spec.econstrs.flatMap(ec => List(ec.tpe1, ec.tpe2))
    }

    /** Returns `None` if `defn` has a single parameter of type Unit. Returns an error otherwise. */
    private def checkUnitArg(defn: TypedAst.Def): Option[EntryPointError] = {
      defn.spec.fparams match {
        // One parameter of type Unit - valid.
        case List(arg) =>
          isUnitType(arg.tpe) match {
            case Result.Ok(true) => None
            case Result.Ok(false) =>
              Some(EntryPointError.IllegalRunnableEntryPointArgs(defn.sym.loc))
            case Result.Err(ErrorOrMalformed) =>
              // Do not report an error, since previous phases should have done already.
              None
          }
        // One parameter of a non-Unit type or more than two parameters - invalid.
        case _ :: _ =>
          Some(EntryPointError.IllegalRunnableEntryPointArgs(defn.sym.loc))
        // Zero parameters.
        case Nil => throw InternalCompilerException(s"Unexpected main with zero parameters ('${defn.sym}'", defn.sym.loc)
      }
    }

    /**
      * Returns `None` if `defn` has return type Unit or has a return type with `ToString` defined.
      * Returns an error otherwise.
      *
      * In order to support compilation without the standard library, Unit is checked first even
      * though it has `ToString` defined.
      */
    private def checkToStringOrUnitResult(defn: TypedAst.Def)(implicit root: TypedAst.Root, flix: Flix): Option[EntryPointError] = {
      val resultType = defn.spec.retTpe
      isUnitType(resultType) match {
        case Result.Ok(true) =>
          None
        case Result.Ok(false) =>
          val unknownTraitSym = new Symbol.TraitSym(Nil, "ToString", SourceLocation.Unknown)
          val traitSym = root.traits.getOrElse(unknownTraitSym, throw InternalCompilerException(s"'$unknownTraitSym' trait not found", defn.sym.loc)).sym
          val constraint = TypeConstraint.Trait(traitSym, resultType, SourceLocation.Unknown)
          val hasToString = ConstraintSolver2.solveAll(List(constraint), SubstitutionTree.empty)(Scope.Top, RigidityEnv.empty, root.traitEnv, root.eqEnv, flix) match {
            // If we could reduce all the way, it has ToString.
            case (Nil, _) => true
            // If not, there is no ToString instance.
            case (_ :: _, _) => false
          }
          if (hasToString) None
          else Some(EntryPointError.IllegalMainEntryPointResult(resultType, resultType.loc))
        case Result.Err(ErrorOrMalformed) =>
          // Do not report an error, since previous phases should have done already.
          None
      }
    }

    /**
      * Returns `None` if `defn` has an effect that is a subset of `allowed`.
      *
      * Returns `None` if `defn` has a malformed effect.
      *
      * Return `Some(err)` otherwise.
      */
    private def checkEffects(defn: TypedAst.Def, allowed: SortedSet[Symbol.EffSym])(implicit flix: Flix): Option[EntryPointError] = {
      val eff = defn.spec.eff
      checkSubset(eff, allowed) match {
        case Result.Ok(true) =>
          None
        case Result.Ok(false) =>
          Some(EntryPointError.IllegalEntryPointEffect(eff, eff.loc))
        case Result.Err(ErrorOrMalformed) =>
          // Do not report an error, since previous phases should have done already.
          None
      }
    }

    /**
      * Returns `true` if `smaller` is a subset of `larger`.
      *
      * Returns `false` for all effects containing type variables or Error.
      */
    private def checkSubset(smaller: Type, larger: SortedSet[Symbol.EffSym]): Result[Boolean, ErrorOrMalformed.type] = {
      for (
        s <- eval(smaller)
      ) yield {
        // Check that s is a subset of larger.
        // s ⊆ larger <=> s - larger = {}
        CofiniteSet.difference(s, CofiniteSet.mkSet(larger)).isEmpty
      }
    }

    /** Returns an error if `defn` is in the root namespace. */
    private def checkNonRootNamespace(defn: TypedAst.Def): Option[EntryPointError] = {
      val inRoot = defn.sym.namespace.isEmpty
      if (inRoot) Some(EntryPointError.IllegalExportNamespace(defn.sym.loc))
      else None
    }

    /** Returns an error if `defn` is not a public function. */
    private def checkPub(defn: TypedAst.Def): Option[EntryPointError] = {
      val isPub = defn.spec.mod.isPublic
      if (isPub) None
      else Some(EntryPointError.NonPublicExport(defn.sym.loc))
    }

    /** Returns `None` if `defn` has a name that is valid in Java. Returns an error otherwise. */
    private def checkValidJavaName(defn: TypedAst.Def): Option[EntryPointError] = {
      val validName = defn.sym.name.matches("[a-z][a-zA-Z0-9]*")
      if (validName) None
      else Some(EntryPointError.IllegalExportName(defn.sym.loc))
    }

    /** Returns an error for each type in `defn` that is not valid in Java. */
    private def checkJavaTypes(defn: TypedAst.Def): List[EntryPointError] = {
      val types = defn.spec.retTpe :: defn.spec.fparams.map(_.tpe)
      types.flatMap(tpe => {
        isExportableType(tpe) match {
          case Result.Ok(true) =>
            None
          case Result.Ok(false) =>
            Some(EntryPointError.IllegalExportType(tpe, tpe.loc))
          case Result.Err(ErrorOrMalformed) =>
            // Do not report an error, since previous phases should have done already.
            None
        }
      })
    }

    /**
      * Returns `true` if `tpe` is a valid Java type that can be exported.
      *
      *   - `isExportableType(Int32) = true`
      *   - `isExportableType(Bool) = true`
      *   - `isExportableType(String) = true`
      *   - `isExportableType(List[String]) = false`
      *   - `isExportableType(java.lang.Object) = true`
      */
    @tailrec
    private def isExportableType(tpe: Type): Result[Boolean, ErrorOrMalformed.type] = {
      // TODO: Currently, because of eager erasure, we only allow primitive types and Object.
      tpe match {
        case Type.Cst(TypeConstructor.Bool, _) => Result.Ok(true)
        case Type.Cst(TypeConstructor.Char, _) => Result.Ok(true)
        case Type.Cst(TypeConstructor.Float32, _) => Result.Ok(true)
        case Type.Cst(TypeConstructor.Float64, _) => Result.Ok(true)
        case Type.Cst(TypeConstructor.Int8, _) => Result.Ok(true)
        case Type.Cst(TypeConstructor.Int16, _) => Result.Ok(true)
        case Type.Cst(TypeConstructor.Int32, _) => Result.Ok(true)
        case Type.Cst(TypeConstructor.Int64, _) => Result.Ok(true)
        case Type.Cst(TypeConstructor.Native(clazz), _) if clazz == classOf[java.lang.Object] => Result.Ok(true)
        case Type.Cst(_, _) => Result.Ok(false)
        case Type.Apply(_, _, _) => Result.Ok(false)
        case Type.Alias(_, _, t, _) => isExportableType(t)
        case Type.Var(_, _) => Result.Err(ErrorOrMalformed)
        case Type.AssocType(_, _, _, _) => Result.Err(ErrorOrMalformed)
        case Type.JvmToType(_, _) => Result.Err(ErrorOrMalformed)
        case Type.JvmToEff(_, _) => Result.Err(ErrorOrMalformed)
        case Type.UnresolvedJvmType(_, _) => Result.Err(ErrorOrMalformed)
      }
    }

    private object SharedContext {
      /** Returns a fresh shared context. */
      def mk(): SharedContext = new SharedContext(
        new ConcurrentLinkedQueue(),
        new AtomicBoolean(false)
      )
    }

    /**
      * A global shared context. Must be thread-safe.
      *
      * @param errors      the [[EntryPointError]]s in the AST, if any.
      * @param invalidMain marks the main entrypoint as invalid.
      */
    private case class SharedContext(errors: ConcurrentLinkedQueue[EntryPointError], invalidMain: AtomicBoolean)

  }

  private object WrapInDefaultHandlers {
    /**
      * Default handler components
      *
      * Given:
      * {{{
      *     eff E
      *     mod E {
      *         @DefaultHandler
      *         def handle(f: Unit -> a \ ef): a \ (ef - E) + IO = exp
      *     }
      * }}}
      *
      *   - handlerSym: handle's [[Symbol.DefnSym]].
      *   - handlerDef: handle's [[TypedAst.Def]].
      *   - handlerDef: E's [[Type]].
      *   - handlerDef: E's [[Symbol.EffSym]].
      * */
    private case class DefaultHandler(
                                       handlerSym: Symbol.DefnSym,
                                       handlerDef: TypedAst.Def,
                                       handledEff: Type,
                                       handledSym: Symbol.EffSym,
                                     )

    /**
      * WrapInDefaultHandlers takes a root and returns a new root where the entry points' definitions
      * have been wrapped in a default handler for any extra non-primitive effects.
      *
      * It handles multiple effects with default handlers by nesting these handlers.
      * The nesting order is unspecified and should not be relied upon.
      *
      *
      * Takes
      * {{{
      *     def f(...): tpe \ ef = exp
      * }}}
      * Returns
      * {{{
      *     def f(...): tpe \ efHandler + (ef - efDefault) = E.handle(_ ->  exp)
      * }}}
      *
      * The [[EntryPoints]] and [[Typer]] phases ensure that every entry point:
      *   - Takes Unit as an argument
      *   - Has a valid signature (without type variables, etc.)
      *
      */
    def run(root: TypedAst.Root)(implicit flix: Flix): (TypedAst.Root, List[EntryPointError]) = {
      val defaultHandlers = root.defs.filter {
        case (_, defn) => defn.spec.ann.isDefaultHandler
      }
      val (errors, validHandlers) = defaultHandlers.map {
        case (sym, defn) => checkHandler(sym, defn, root)
      }.partitionMap {
        case Validation.Failure(err) => Left(err)
        case Validation.Success(validHandler) => Right(validHandler)
      }

      // Check for [[EntryPointError.DuplicatedDefaultHandlers]].
      val seen = mutable.Map.empty[Symbol.EffSym, SourceLocation]
      val duplicatedHandlerErrors = mutable.ArrayBuffer.empty[EntryPointError]
      for (DefaultHandler(handlerSym, _, _, handledSym) <- validHandlers) {
        val loc1 = handlerSym.loc
        seen.get(handledSym) match {
          case None =>
            seen.put(handledSym, loc1)
          case Some(loc2) =>
            duplicatedHandlerErrors += EntryPointError.DuplicateDefaultHandler(handledSym, loc1, loc2)
            duplicatedHandlerErrors += EntryPointError.DuplicateDefaultHandler(handledSym, loc2, loc1)
        }
      }
      val defsWithDefaultHandlers = ParOps.parMapValues(root.defs)(
        defn =>
          if (isEntryPoint(defn)(root)) wrapDefWithDefaultHandlers(defn, validHandlers, root) else defn
      )
      (root.copy(defs = defsWithDefaultHandlers), errors.toList.flatMap(_.toList) ++ duplicatedHandlerErrors)
    }

    /**
      * Validates that a function marked with @DefaultHandler has the correct signature.
      *
      * A valid default handler must:
      *   - Be in the companion module of an existing effect
      *   - Take exactly one argument of function type: Unit -> a \ ef
      *   - Return the same type variable 'a' as the function argument
      *   - Have an effect signature of the form: (ef - HandledEffect) + IO
      *
      * Valid handler example
      * {{{
      *     eff E
      *     mod E {
      *         @DefaultHandler
      *         def handle(f: Unit -> a \ ef): a \ (ef - E) + IO = exp
      *     }
      * }}}
      *
      * Invalid handler example
      * {{{
      *     @DefaultHandler
      *     def handle(f: Unit -> a \ ef, arg: tpe): Bool \ (ef - ef2) + Environment = exp
      * }}}
      *
      * @param handlerSym The symbol of the handler function
      * @param handlerDef The definition of the handler function
      * @param root       The typed AST root
      * @return [[Validation]] of [[DefaultHandler]] or [[EntryPointError]]
      */
    private def checkHandler(handlerSym: Symbol.DefnSym, handlerDef: TypedAst.Def, root: TypedAst.Root)(implicit flix: Flix): Validation[DefaultHandler, EntryPointError] = {
      var errs: Chain[EntryPointError] = Chain.empty
      // All default handlers must be public
      if (!handlerDef.spec.mod.isPublic) {
        errs = errs ++ Chain(EntryPointError.NonPublicDefaultHandler(handlerSym, handlerSym.loc))
      }
      // The Default Handler must reside in the companion module of the effect.
      // Hence we use the namespace of the handler to construct the expected
      // effect symbol and look it up in the AST.
      val effFqn = handlerSym.namespace.mkString(".")
      val effSym = Symbol.mkEffSym(effFqn)
      val companionEffect = root.effects.get(effSym)
      companionEffect match {
        case None => Validation.Failure(errs ++ Chain(EntryPointError.DefaultHandlerNotInModule(handlerSym, handlerSym.loc)))
        // The default handler is NOT in the companion module of an effect
        case Some(_) =>
          // Synthetic location of our handler
          val loc = handlerSym.loc.asSynthetic
          // There is a valid effect to wrap
          val handledEff = Type.Cst(TypeConstructor.Effect(effSym, Kind.Eff), loc)
          val declaredScheme = handlerDef.spec.declaredScheme
          // Generate expected scheme for generating IO
          val expectedSchemeIO = getDefaultHandlerTypeScheme(handledEff, Type.IO, loc)
          // Check if handler's scheme fits any of the valid handler's schemes and if not generate an error
          if (!Scheme.equal(expectedSchemeIO, declaredScheme, root.traitEnv, root.eqEnv, Nil)) {
            errs = errs ++ Chain(EntryPointError.IllegalDefaultHandlerSignature(effSym, handlerSym, handlerSym.loc))
          }
          if (errs.isEmpty) {
            Validation.Success(DefaultHandler(handlerSym, handlerDef, handledEff, effSym))
          } else {
            Validation.Failure(errs)
          }
      }
    }

    /**
      * Returns the type scheme of
      * {{{
      *     def handle(f: Unit -> a \ ef): a \ (ef - handledEff) + IO
      * }}}
      *
      * @param handledEff                The type of the effect associated with the default handler
      * @param generatedPrimitiveEffects The type of the generated primitive effects by the default handler
      * @param loc                       The source location to be used for members of the scheme
      * @return The expected scheme of the default handler
      */
    private def getDefaultHandlerTypeScheme(handledEff: Type, generatedPrimitiveEffects: Type, loc: SourceLocation)(implicit flix: Flix): Scheme = {
      val a = Type.freshVar(Kind.Star, loc)
      val ef = Type.freshVar(Kind.Eff, loc)
      Scheme(
        quantifiers = List(a.sym, ef.sym),
        tconstrs = Nil,
        econstrs = Nil,
        base = Type.mkArrowWithEffect(
          a = Type.mkArrowWithEffect(Type.Unit, ef, a, loc),
          // (ef - HandledEff) + generatedPrimitiveEffects
          p = Type.mkUnion(Type.mkDifference(ef, handledEff, loc), generatedPrimitiveEffects, loc),
          b = a,
          loc = loc
        )
      )
    }

    /**
      * Wraps an entry point function with calls to the default handlers of each of the effects appearing in
      * its signature. The order in which the handlers are applied is not defined and should not be relied upon.
      *
      * If the signature is not well-formed or it has type variables, it adds calls to every default handler.
      *
      * For example, if we had default handlers for some effects A, B and C:
      *
      * Transforms a function:
      * {{{
      *     def f(arg1: tpe1, ...): tpe \ A + B = exp
      * }}}
      * Into:
      * {{{
      *     def f(arg1: tpe1, ...): tpe \ (((ef - A) + IO) - B) + IO =
      *         handlerB(_ -> handlerA(_ ->exp))
      * }}}
      *
      * Each of the wrappers:
      *   - Removes the handled effect from the function's effect set and adds IO
      *   - Creates a lambda (_ -> originalBody) and passes it to each handler
      *   - Updates the function's type signature accordingly
      *
      * @param currentDef      The entry point function definition to wrap
      * @param defaultHandlers Iterable of information about the default handler to apply
      * @param root            The typed AST root
      * @return The wrapped function definition with all necessary default effect handlers
      */
    private def wrapDefWithDefaultHandlers(currentDef: TypedAst.Def, defaultHandlers: Iterable[DefaultHandler], root: TypedAst.Root)(implicit flix: Flix): TypedAst.Def = {
      // Obtain the concrete effect set of the definition that is going to be wrapped.
      // We are expecting entry points, and all entry points should have a concrete effect set
      val defEffects: CofiniteSet[Symbol.EffSym] = eval(currentDef.spec.eff) match {
        case Result.Ok(s) => s
        // This means eff is either not well-formed or it has type variables.
        // Either way, in this case we will wrap with all default handlers
        // to make sure that the effects present in the signature that have default handlers are handled
        case Result.Err(_) => CofiniteSet.universe
      }
      val necessaryHandlers = defaultHandlers.filter(h => defEffects.contains(h.handledSym))
      // Wrap the expression in each of the default handlers for the effects appearing in the signature.
      // Right now, the order depends on the order of defaultHandlers.
      necessaryHandlers.foldLeft(currentDef)((defn, handler) => wrapInHandler(defn, handler, root))
    }

    /**
      * Wraps an entry point function with a default effect handler.
      *
      * Transforms a function:
      * {{{
      *     def f(arg1: tpe1, ...): tpe \ ef = exp
      * }}}
      *
      * Into:
      * {{{
      *     def f(arg1: tpe1, ...): tpe \ (ef - handledEffect) + IO =
      *         handler(_ -> exp)
      * }}}
      *
      * The wrapper:
      *   - Removes the handled effect from the function's effect set and adds IO
      *   - Creates a lambda (_ -> originalBody) and passes it to each handler
      *   - Updates the function's type signature accordingly
      *
      * @param defn        The entry point function definition to wrap
      * @param defaultHandler Information about the default handler to apply
      * @param root        The typed AST root
      * @return The wrapped function definition with updated effect signature
      */
    private def wrapInHandler(defn: TypedAst.Def, defaultHandler: DefaultHandler, root: TypedAst.Root)(implicit flix: Flix): TypedAst.Def = {
      // Create synthetic locations
      val effLoc = defn.spec.eff.loc.asSynthetic
      val baseTypeLoc = defn.spec.declaredScheme.base.loc.asSynthetic
      val expLoc = defn.exp.loc.asSynthetic
      // The new type is the same as the wrapped def with an effect set of
      // `(ef - handledEffect) + IO` where `ef` is the effect set of the previous definition.
      val effDif = Type.mkDifference(defn.spec.eff, defaultHandler.handledEff, effLoc)
      // Technically we could perform this outside at the wrapInHandlers level
      // by just checking the length of the handlers and if it is greater than 0
      // just adding IO. However, that would only work while default handlers can only generate IO.
      val eff = Type.mkUnion(effDif, Type.IO, effLoc)
      val tpe = Type.mkCurriedArrowWithEffect(defn.spec.fparams.map(_.tpe), eff, defn.spec.retTpe, baseTypeLoc)
      val spec = defn.spec.copy(
        declaredScheme = defn.spec.declaredScheme.copy(base = tpe),
        eff = eff,
      )
      // Create _ -> exp
      val innerLambda =
        TypedAst.Expr.Lambda(
          TypedAst.FormalParam(
            TypedAst.Binder(Symbol.freshVarSym("_", FormalParam, expLoc), Type.Unit),
            Type.Unit,
            TypeSource.Inferred,
            expLoc
          ),
          defn.exp,
          Type.mkArrowWithEffect(Type.Unit, defn.spec.eff, defn.spec.retTpe, expLoc),
          expLoc
      )
      // Create the instantiated type of the handler
      val handlerArrowType = Type.mkArrowWithEffect(innerLambda.tpe, eff, defn.spec.retTpe, expLoc)
      // Create HandledEff.handle(_ -> exp)
      val handlerDefSymUse = SymUse.DefSymUse(defaultHandler.handlerSym, expLoc)
      val handlerCall = TypedAst.Expr.ApplyDef(handlerDefSymUse, List(innerLambda), List(innerLambda.tpe), handlerArrowType, defn.spec.retTpe, eff, expLoc)
      defn.copy(spec = spec, exp = handlerCall)
    }
  }

  private object WrapMain {

    /**
      * WrapMain takes the main function (if it exists) and creates a new main function that prints
      * the value of the existing main function (unless it returns unit).
      *
      * From previous phases we know that the main function:
      *   - Exists if root.mainEntryPoint is Some.
      *   - Returns Unit or has a return type with ToString defined
      *   - Has a valid signature (without type variables, etc.)
      */
    def run(root: TypedAst.Root)(implicit flix: Flix): TypedAst.Root = {
      root.mainEntryPoint.map(root.defs(_)) match {
        case Some(main0: TypedAst.Def) =>
          isUnitType(main0.spec.retTpe) match {
            case Result.Ok(true) =>
              // main doesn't need a wrapper.
              root
            case Result.Ok(false) =>
              // main needs a wrapper.
              val main = mkEntryPoint(main0, root)
              val root1 = root.copy(
                defs = root.defs + (main.sym -> main),
                mainEntryPoint = Some(main.sym)
              )
              root1
            case Result.Err(ErrorOrMalformed) =>
              // Do not report an error, since previous phases should have done already.
              root
          }

        case Some(_) | None =>
          // Main doesn't need a wrapper or no main exists.
          root
      }
    }

    /**
      * Returns a new function that calls the main function and prints its value
      * (`mainFunc` in the example below).
      *
      * {{{
      *   def mainFunc(): tpe \ ef = exp
      *
      *   def main$(): Unit \ ef + IO = println(main())
      * }}}
      */
    private def mkEntryPoint(oldEntryPoint: TypedAst.Def, root: TypedAst.Root)(implicit flix: Flix): TypedAst.Def = {
      val argSym = Symbol.freshVarSym("_unit", BoundBy.FormalParam, SourceLocation.Unknown)

      // The new type is `Unit -> Unit \ IO + eff` where `eff` is the effect of the old main.
      val argType = Type.Unit
      val retType = Type.Unit
      val eff = Type.mkUnion(Type.IO, oldEntryPoint.spec.eff, SourceLocation.Unknown)

      val tpe = Type.mkArrowWithEffect(argType, eff, retType, SourceLocation.Unknown)
      val spec = TypedAst.Spec(
        doc = Doc(Nil, SourceLocation.Unknown),
        ann = Annotations.Empty,
        mod = Modifiers.Empty,
        tparams = Nil,
        fparams = List(TypedAst.FormalParam(
          TypedAst.Binder(argSym, argType),
          argType,
          TypeSource.Ascribed,
          SourceLocation.Unknown)
        ),
        declaredScheme = Scheme(Nil, Nil, Nil, tpe),
        retTpe = retType,
        eff = eff,
        tconstrs = Nil,
        econstrs = Nil,
      )

      val mainArrowType = oldEntryPoint.spec.declaredScheme.base
      val mainSym = DefSymUse(oldEntryPoint.sym, SourceLocation.Unknown)
      val mainArgType = Type.Unit
      val mainArg = TypedAst.Expr.Cst(Constant.Unit, mainArgType, SourceLocation.Unknown)
      val mainReturnType = mainArrowType.arrowResultType
      val mainEffect = mainArrowType.arrowEffectType
      // `mainFunc()`.
      val mainCall = TypedAst.Expr.ApplyDef(mainSym, List(mainArg), List.empty, mainArrowType, mainReturnType, mainEffect, SourceLocation.Unknown)

      // `println(mainFunc())`.
      val printSym = DefSymUse(root.defs(new Symbol.DefnSym(None, Nil, "println", SourceLocation.Unknown)).sym, SourceLocation.Unknown)
      val printArg = mainCall
      val printArgType = mainCall.tpe
      val printReturnType = Type.Unit
      val printEffect = Type.IO
      val printArrowType = Type.mkArrowWithEffect(printArgType, printEffect, printReturnType, SourceLocation.Unknown)
      val printCallEffect = Type.mkUnion(printEffect, printArg.eff, SourceLocation.Unknown)
      val printCall = TypedAst.Expr.ApplyDef(printSym, List(printArg), List(printArgType), printArrowType, printReturnType, printCallEffect, SourceLocation.Unknown)

      val sym = new Symbol.DefnSym(None, Nil, "main" + Flix.Delimiter, SourceLocation.Unknown)

      TypedAst.Def(sym, spec, printCall, SourceLocation.Unknown)
    }

  }

  /** Returns a new root where [[TypedAst.Root.entryPoints]] contains all entry points (main/test/export). */
  private def findEntryPoints(root: TypedAst.Root): TypedAst.Root = {
    val s = mutable.Set.empty[Symbol.DefnSym]
    for ((sym, defn) <- root.defs if isEntryPoint(defn)(root)) {
      s += sym
    }
    val entryPoints = s.toSet
    root.copy(entryPoints = entryPoints)
  }

  /**
    * Evaluates `eff` if it is well-formed and has no type variables,
    * associated types, or error types.
    */
  private def eval(eff: Type): Result[CofiniteSet[Symbol.EffSym], ErrorOrMalformed.type] = eff match {
    case Type.Cst(tc, _) => tc match {
      case TypeConstructor.Pure => Result.Ok(CofiniteSet.empty)
      case TypeConstructor.Univ => Result.Ok(CofiniteSet.universe)
      case TypeConstructor.Effect(sym, _) => Result.Ok(CofiniteSet.mkSet(sym))
      case _ => Result.Err(ErrorOrMalformed)
    }
    case Type.Apply(Type.Cst(TypeConstructor.Complement, _), x0, _) =>
      Result.mapN(eval(x0)) {
        case x => CofiniteSet.complement(x)
      }
    case Type.Apply(Type.Apply(Type.Cst(TypeConstructor.Union, _), x0, _), y0, _) =>
      Result.mapN(eval(x0), eval(y0)) {
        case (x, y) => CofiniteSet.union(x, y)
      }
    case Type.Apply(Type.Apply(Type.Cst(TypeConstructor.Intersection, _), x0, _), y0, _) =>
      Result.mapN(eval(x0), eval(y0)) {
        case (x, y) => CofiniteSet.intersection(x, y)
      }
    case Type.Apply(Type.Apply(Type.Cst(TypeConstructor.Difference, _), x0, _), y0, _) =>
      Result.mapN(eval(x0), eval(y0)) {
        case (x, y) => CofiniteSet.difference(x, y)
      }
    case Type.Apply(Type.Apply(Type.Cst(TypeConstructor.SymmetricDiff, _), x0, _), y0, _) =>
      Result.mapN(eval(x0), eval(y0)) {
        case (x, y) => CofiniteSet.xor(x, y)
      }
    case Type.Alias(_, _, tpe, _) => eval(tpe)
    case Type.Var(_, _) => Result.Err(ErrorOrMalformed)
    case Type.Apply(_, _, _) => Result.Err(ErrorOrMalformed)
    case Type.AssocType(_, _, _, _) => Result.Err(ErrorOrMalformed)
    case Type.JvmToType(_, _) => Result.Err(ErrorOrMalformed)
    case Type.JvmToEff(_, _) => Result.Err(ErrorOrMalformed)
    case Type.UnresolvedJvmType(_, _) => Result.Err(ErrorOrMalformed)
  }

}

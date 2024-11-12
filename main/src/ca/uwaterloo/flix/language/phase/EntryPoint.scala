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
import ca.uwaterloo.flix.language.ast.shared.SymUse.DefSymUse
import ca.uwaterloo.flix.language.ast.{Ast, Scheme, SourceLocation, Symbol, Type, TypeConstructor, TypedAst}
import ca.uwaterloo.flix.language.dbg.AstPrinter.*
import ca.uwaterloo.flix.language.errors.EntryPointError
import ca.uwaterloo.flix.language.phase.unification.{TraitEnv, TraitEnvironment}
import ca.uwaterloo.flix.util.collection.ListMap
import ca.uwaterloo.flix.util.{CofiniteEffSet, InternalCompilerException, ParOps, Result}

import java.util.concurrent.ConcurrentLinkedQueue
import java.util.concurrent.atomic.AtomicBoolean
import scala.annotation.tailrec
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
  * This phase has three sub-phases:
  *   1. Check that all entry points have valid signatures, where rules differ from main, tests, and
  *     exports.
  *   1. Replace the existing main function by a new main function that prints the returned value.
  *   1. Compute the set of all entry points and store it in Root.
  */
object EntryPoint {

  // We don't use regions, so we are safe to use the global scope everywhere in this phase.
  private implicit val S: Scope = Scope.Top

  /**
    * The default entry point in case none is specified. (`main`)
    */
  private val DefaultEntryPoint = Symbol.mkDefnSym("main")

  def run(root: TypedAst.Root)(implicit flix: Flix): (TypedAst.Root, List[EntryPointError]) = flix.phaseNew("EntryPoint") {
    val (root1, errs1) = lookupEntryPoint(root)
    val (root2, errs2) = CheckEntryPoints.run(root1)
    // WrapMain assumes a sensible main, so CheckEntryPoints must run first.
    val (root3, errs3) = WrapMain.run(root2)
    // WrapMain might change main, so FindEntryPoints must be after.
    val root4 = FindEntryPoints.run(root3)
    (root4, errs1 ++ errs2 ++ errs3)
  }

  /**
    * Converts [[TypedAst.Root.entryPoint]] to be explicit instead of implicit and checks that the
    * given entry point (if any) exists.
    *
    * In the input, a None entrypoint means to use `main` if it exists.
    * In the output, None means no entrypoint and Some is an entrypoint, guaranteed to be in defs.
    */
  private def lookupEntryPoint(root: TypedAst.Root): (TypedAst.Root, List[EntryPointError]) = {
    root.entryPoint match {
      // If no main is given, only use main if it is found
      case None => root.defs.get(DefaultEntryPoint) match {
        case None =>
          // Actually no main.
          (root, Nil)
        case Some(entryPoint) =>
          // Use the default main explicitly.
          (root.copy(entryPoint = Some(entryPoint.sym)), Nil)
      }
      case Some(sym) => root.defs.get(sym) match {
        case Some(_) =>
          // The given entry point exists.
          (root, Nil)
        case None =>
          // The given entry point does not exist.
          (root.copy(entryPoint = None), List(EntryPointError.MainEntryPointNotFound(sym)))
      }
    }
  }

  /**
    * A function is an entry point if:
    *   - It is the main entry point function (called `main` by default, but can configured to an
    *     arbitrary name).
    *   - It is a test (annotated with `@Test`).
    *   - It is an exported function (annotated with `@Export`).
    */
  private def isEntryPoint(defn: TypedAst.Def)(implicit root: TypedAst.Root): Boolean =
    isMain(defn) || isTest(defn) || isExport(defn)

  private def isTest(defn: TypedAst.Def): Boolean =
    defn.spec.ann.isTest

  private def isExport(defn: TypedAst.Def): Boolean =
    defn.spec.ann.isExport

  private def isMain(defn: TypedAst.Def)(implicit root: TypedAst.Root): Boolean =
    root.entryPoint.contains(defn.sym)

  /** Check if a non-polymorphic type is `Unit`. */
  @tailrec
  private def isUnitType(tpe: Type): Boolean = tpe match {
    case Type.Cst(TypeConstructor.Unit, _) => true
    case Type.Cst(_, _) => false
    case Type.Apply(_, _, _) => false
    case Type.Alias(_, _, tpe, _) => isUnitType(tpe)
    case Type.Var(_, _) => throw InternalCompilerException(s"Unexpected entry point parameter type '$tpe'", tpe.loc)
    case Type.AssocType(_, _, _, _) => throw InternalCompilerException(s"Unexpected entry point parameter type '$tpe'", tpe.loc)
    case Type.JvmToType(_, _) => throw InternalCompilerException(s"Unexpected entry point parameter type '$tpe'", tpe.loc)
    case Type.JvmToEff(_, _) => throw InternalCompilerException(s"Unexpected entry point parameter type '$tpe'", tpe.loc)
    case Type.UnresolvedJvmType(_, _) => throw InternalCompilerException(s"Unexpected entry point parameter type '$tpe'", tpe.loc)
  }

  private object CheckEntryPoints {

    def run(root: TypedAst.Root)(implicit flix: Flix): (TypedAst.Root, List[EntryPointError]) = {
      implicit val sctx: SharedContext = SharedContext.mk()
      implicit val r: TypedAst.Root = root

      ParOps.parMapValues(root.defs)(visitDef)

      // Remove the entrypoint if it is not valid.
      val root1 = if (sctx.invalidMain.get()) root.copy(entryPoint = None) else root
      val errs = sctx.errors.asScala.toList
      (root1, errs)
    }

    private def visitDef(defn: TypedAst.Def)(implicit sctx: SharedContext, root: TypedAst.Root, flix: Flix): TypedAst.Def = {
      // Note that these categories are not disjoint, so
      // multiple checks might be needed for each function.
      if (isMain(defn)(root)) checkMain(defn)
      val defn1 = if (isTest(defn)) visitTest(defn) else defn
      val defn2 = if (isExport(defn)) visitExport(defn1) else defn1
      defn2
    }

    /**
      * Rules for main:
      *   - Non-polymorphic - no type variables (OBS: check first!).
      *     - This means that traits and trait constraints do not occur since their syntax is
      *       limited to `Trait[var]`.
      *     - This means that associated types do not occur since their syntax is limited to
      *       `Trait.Assoc[var]`.
      *     - This means that equality constraints do not occur since their syntax is limited to
      *       `Trait.Assoc[var] ~ type`
      *   - One `Unit` type parameter.
      *   - Returns type Unit or type `t` where `ToString[t]` exists.
      *     - This is split into two cases to allow LibNix to run, where `ToString` is not defined.
      *   - Has effect `eff` where `eff ⊆ primitive effects`.
      *
      */
    private def checkMain(defn: TypedAst.Def)(implicit sctx: SharedContext, root: TypedAst.Root, flix: Flix): Unit = {
      // TODO what about being public?
      val errs = checkNonPolymorphic(defn) match {
        case Some(err) => List(err)
        case None =>
          // These checks assume non-polymorphic types.
          checkUnitArg(defn) ++ checkToStringOrUnitResult(defn) ++ checkPrimitiveEffect(defn)
      }
      if (errs.isEmpty) {
        ()
      } else {
        sctx.invalidMain.set(true)
        errs.foreach(sctx.errors.add)
      }
    }

    /**
      * Rules for tests:
      *   - Non-polymorphic - no type variables (OBS: check first!).
      *     - This means that traits and trait constraints do not occur since their syntax is
      *       limited to `Trait[var]`.
      *     - This means that associated types do not occur since their syntax is limited to
      *       `Trait.Assoc[var]`.
      *     - This means that equality constraints do not occur since their syntax is limited to
      *       `Trait.Assoc[var] ~ type`
      *   - One `Unit` type parameter.
      *   - Has effect `eff` where `eff ⊆ primitive effects`.
      */
    private def visitTest(defn: TypedAst.Def)(implicit sctx: SharedContext, flix: Flix): TypedAst.Def = {
      val errs = checkNonPolymorphic(defn) match {
        case Some(err) => List(err)
        case None =>
          // These checks assume non-polymorphic types.
          checkUnitArg(defn) ++ checkPrimitiveEffect(defn)
      }
      if (errs.isEmpty) {
        defn
      } else {
        errs.foreach(sctx.errors.add)
        removeTestAnnotation(defn)
      }
    }

    private def removeTestAnnotation(defn: TypedAst.Def): TypedAst.Def =
      defn.copy(
        spec = defn.spec.copy(
          ann = defn.spec.ann.copy(
            annotations = defn.spec.ann.annotations.filterNot(_.isInstanceOf[Annotation.Test])
          )
        )
      )

    /**
      * Rules for exports:
      *   - Non-polymorphic - no type variables (OBS: check first!).
      *     - This means that traits and trait constraints do not occur since their syntax is
      *       limited to `Trait[var]`.
      *     - This means that associated types do not occur since their syntax is limited to
      *       `Trait.Assoc[var]`.
      *     - This means that equality constraints do not occur since their syntax is limited to
      *       `Trait.Assoc[var] ~ type`
      *   - Has effect `eff` where `eff ⊆ primitive effects`.
      *   - Is not in the root namespace.
      *   - Is `pub`.
      *   - Has a name that is valid in Java.
      *   - Has types that are valid in Java (not Flix types like `List[Int32]`).
      */
    private def visitExport(defn: TypedAst.Def)(implicit sctx: SharedContext, flix: Flix): TypedAst.Def = {
      val errs = (checkNonPolymorphic(defn) match {
        case Some(err) => List(err)
        case None =>
          // These checks assume non-polymorphic types.
          checkPrimitiveEffect(defn).toList ++ checkJavaTypes(defn)
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

    private def removeExportAnnotation(defn: TypedAst.Def): TypedAst.Def =
      defn.copy(
        spec = defn.spec.copy(
          ann = defn.spec.ann.copy(
            annotations = defn.spec.ann.annotations.filterNot(_.isInstanceOf[Annotation.Export])
          )
        )
      )

    private def checkNonPolymorphic(defn: TypedAst.Def): Option[EntryPointError] = {
      val monomorphic = defn.spec.tparams.isEmpty
      if (monomorphic) None
      else Some(EntryPointError.IllegalEntryPointPolymorphism(defn.sym.loc))
    }

    private def checkUnitArg(defn: TypedAst.Def): Option[EntryPointError] = {
      defn.spec.fparams match {
        // One Unit type parameter - valid.
        case List(arg) if isUnitType(arg.tpe) => None
        // One non-unit parameter or two or more parameters.
        case _ :: _ =>
          Some(EntryPointError.IllegalRunnableEntryPointArgs(defn.sym.loc))
        // Zero parameters.
        case Nil => throw InternalCompilerException(s"Unexpected main with 0 parameters ('${defn.sym}'", defn.sym.loc)
      }
    }

    private def checkToStringOrUnitResult(defn: TypedAst.Def)(implicit root: TypedAst.Root, flix: Flix): Option[EntryPointError] = {
      val resultType = defn.spec.retTpe
      if (isUnitType(resultType)) None
      else {
        val unknownTraitSym = new Symbol.TraitSym(Nil, "ToString", SourceLocation.Unknown)
        val traitSym = root.traits.getOrElse(unknownTraitSym, throw InternalCompilerException(s"'$unknownTraitSym' trait not found", defn.sym.loc)).sym
        val constraint = TraitConstraint(TraitConstraint.Head(traitSym, SourceLocation.Unknown), resultType, SourceLocation.Unknown)
        val hasToString = TraitEnvironment.holds(constraint, TraitEnv(root.traitEnv), ListMap.empty)
        if (hasToString) None
        else Some(EntryPointError.IllegalMainEntryPointResult(resultType, resultType.loc))
      }
    }

    private def checkPrimitiveEffect(defn: TypedAst.Def)(implicit flix: Flix): Option[EntryPointError] = {
      val eff = defn.spec.eff
      if (isPrimitiveEffect(eff)) None
      else Some(EntryPointError.IllegalEntryPointEffect(eff, eff.loc))
    }

    private def isPrimitiveEffect(eff: Type): Boolean = {
      if (eff.typeVars.nonEmpty) return false
      // Now that we have the monomorphic effect, we can evaluate it.
      eval(eff) match {
        case Some(CofiniteEffSet.Set(s)) =>
          // Check that it is a set of only primitive effects.
          s.forall(Symbol.isPrimitiveEff)
        case Some(CofiniteEffSet.Compl(_)) =>
          // A set like `not IO` can never be allowed
          false
        case None =>
          // The effect has an Error, don't throw more errors
          true
      }
    }

    private def eval(eff: Type): Option[CofiniteEffSet] = eff match {
      case Type.Cst(tc, _) => tc match {
        case TypeConstructor.Pure => Some(CofiniteEffSet.empty)
        case TypeConstructor.Univ => Some(CofiniteEffSet.universe)
        case TypeConstructor.Effect(sym) => Some(CofiniteEffSet.mkSet(sym))
        case TypeConstructor.Error(_, _) => None
        case _ => throw InternalCompilerException(s"Unexpected effect '$eff'.", eff.loc)
      }
      case Type.Apply(Type.Cst(TypeConstructor.Complement, _), x, _) =>
        eval(x).map(CofiniteEffSet.complement)
      case Type.Apply(Type.Apply(Type.Cst(TypeConstructor.Union, _), x, _), y, _) =>
        eval(x).flatMap(a => eval(y).map(b => CofiniteEffSet.union(a, b)))
      case Type.Apply(Type.Apply(Type.Cst(TypeConstructor.Intersection, _), x, _), y, _) =>
        eval(x).flatMap(a => eval(y).map(b => CofiniteEffSet.intersection(a, b)))
      case Type.Apply(Type.Apply(Type.Cst(TypeConstructor.SymmetricDiff, _), x, _), y, _) =>
        eval(x).flatMap(a => eval(y).map(b => CofiniteEffSet.xor(a, b)))
      case Type.Alias(_, _, tpe, _) => eval(tpe)
      case Type.Var(_, _) => throw InternalCompilerException(s"Unexpected effect '$eff'.", eff.loc)
      case Type.Apply(_, _, _) => throw InternalCompilerException(s"Unexpected effect '$eff'.", eff.loc)
      case Type.AssocType(_, _, _, _) => throw InternalCompilerException(s"Unexpected effect '$eff'.", eff.loc)
      case Type.JvmToType(_, _) => throw InternalCompilerException(s"Unexpected effect '$eff'.", eff.loc)
      case Type.JvmToEff(_, _) => throw InternalCompilerException(s"Unexpected effect '$eff'.", eff.loc)
      case Type.UnresolvedJvmType(_, _) => throw InternalCompilerException(s"Unexpected effect '$eff'.", eff.loc)
    }

    private def checkNonRootNamespace(defn: TypedAst.Def): Option[EntryPointError] = {
      val inRoot = defn.sym.namespace.isEmpty
      if (inRoot) Some(EntryPointError.IllegalExportNamespace(defn.sym.loc))
      else None
    }

    private def checkPub(defn: TypedAst.Def): Option[EntryPointError] = {
      val isPub = defn.spec.mod.isPublic
      if (isPub) None
      else Some(EntryPointError.NonPublicExport(defn.sym.loc))
    }

    private def checkValidJavaName(defn: TypedAst.Def): Option[EntryPointError] = {
      val validName = defn.sym.name.matches("[a-z][a-zA-Z0-9]*")
      if (validName) None
      else Some(EntryPointError.IllegalExportName(defn.sym.loc))
    }

    private def checkJavaTypes(defn: TypedAst.Def): List[EntryPointError] = {
      // Currently, because of eager erasure, we only allow primitive/object types.
      val types = defn.spec.retTpe :: defn.spec.fparams.map(_.tpe)
      types.flatMap(tpe => {
        if (isJavaType(tpe)) None
        else Some(EntryPointError.IllegalExportType(tpe, tpe.loc))
      })
    }

    @tailrec
    private def isJavaType(tpe: Type): Boolean = tpe match {
      case Type.Cst(TypeConstructor.Bool, _) => true
      case Type.Cst(TypeConstructor.Char, _) => true
      case Type.Cst(TypeConstructor.Float32, _) => true
      case Type.Cst(TypeConstructor.Float64, _) => true
      case Type.Cst(TypeConstructor.Int8, _) => true
      case Type.Cst(TypeConstructor.Int16, _) => true
      case Type.Cst(TypeConstructor.Int32, _) => true
      case Type.Cst(TypeConstructor.Int64, _) => true
      case Type.Cst(TypeConstructor.Native(clazz), _) if clazz == classOf[java.lang.Object] => true
      case Type.Cst(_, _) => false
      case Type.Apply(_, _, _) => false
      case Type.Alias(_, _, tpe, _) => isJavaType(tpe)
      case Type.Var(_, _) => throw InternalCompilerException(s"Unexpected entry point parameter type '$tpe'", tpe.loc)
      case Type.AssocType(_, _, _, _) => throw InternalCompilerException(s"Unexpected entry point parameter type '$tpe'", tpe.loc)
      case Type.JvmToType(_, _) => throw InternalCompilerException(s"Unexpected entry point parameter type '$tpe'", tpe.loc)
      case Type.JvmToEff(_, _) => throw InternalCompilerException(s"Unexpected entry point parameter type '$tpe'", tpe.loc)
      case Type.UnresolvedJvmType(_, _) => throw InternalCompilerException(s"Unexpected entry point parameter type '$tpe'", tpe.loc)
    }

    /** Companion object for [[SharedContext]] */
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
      * @param errors the [[EntryPointError]]s in the AST, if any.
      * @param invalidMain marks the main entrypoint as invalid in some way.
      */
    private case class SharedContext(errors: ConcurrentLinkedQueue[EntryPointError], invalidMain: AtomicBoolean)

  }

  private object WrapMain {

    def run(root: TypedAst.Root)(implicit flix: Flix): (TypedAst.Root, List[EntryPointError]) = {
      root.entryPoint.map(root.defs(_)) match {
        case Some(main0: TypedAst.Def) if !isUnitType(main0.spec.retTpe) =>
          val main = mkEntryPoint(main0, root)
          val root1 = root.copy(
            defs = root.defs + (main.sym -> main),
            entryPoint = Some(main.sym)
          )
          (root1, Nil)
        case Some(_) | None =>
          // main doesn't need wrapper or no main exists.
          (root, Nil)
      }
    }

    /**
      * Builds the new main entry point function that calls the old entry point function.
      *
      * {{{
      *   // We know that `ToString[tpe]` exists.
      *   def main(): tpe \ ef = exp
      *
      *   def genMain(): Unit \ ef + IO = printUnlessUnit(main())
      * }}}
      */
    private def mkEntryPoint(oldEntryPoint: TypedAst.Def, root: TypedAst.Root)(implicit flix: Flix): TypedAst.Def = {
      val argSym = Symbol.freshVarSym("_unit", Ast.BoundBy.FormalParam, SourceLocation.Unknown)

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
          Modifiers.Empty,
          argType,
          Ast.TypeSource.Ascribed,
          SourceLocation.Unknown)
        ),
        declaredScheme = Scheme(Nil, Nil, Nil, tpe),
        retTpe = retType,
        eff = eff,
        tconstrs = Nil,
        econstrs = Nil,
      )

      // NB: Getting the type directly from the scheme assumes the function is not polymorphic.
      // We have checked this in CheckEntryPoints.
      val itpe = oldEntryPoint.spec.declaredScheme.base
      val symUse = DefSymUse(oldEntryPoint.sym, SourceLocation.Unknown)
      val args = List(TypedAst.Expr.Cst(Constant.Unit, Type.Unit, SourceLocation.Unknown))
      // func()
      val call = TypedAst.Expr.ApplyDef(symUse, args, itpe, itpe.arrowResultType, itpe.arrowEffectType, SourceLocation.Unknown)

      // one of:
      // printUnlessUnit(func(args))
      val printSym = root.defs(new Symbol.DefnSym(None, Nil, "printUnlessUnit", SourceLocation.Unknown)).sym
      val printTpe = Type.mkArrowWithEffect(itpe.arrowResultType, Type.IO, Type.Unit, SourceLocation.Unknown)
      val print = TypedAst.Expr.ApplyDef(DefSymUse(printSym, SourceLocation.Unknown), List(call), printTpe, Type.Unit, Type.IO, SourceLocation.Unknown)

      val sym = new Symbol.DefnSym(None, Nil, "main" + Flix.Delimiter, SourceLocation.Unknown)

      TypedAst.Def(sym, spec, print, SourceLocation.Unknown)
    }

  }

  private object FindEntryPoints {

    /** Returns a new root where [[TypedAst.Root.reachable]] is contains all entry points. */
    def run(root: TypedAst.Root): TypedAst.Root = {
      root.copy(reachable = getEntrypoints(root))
    }

    /** Returns all entrypoint functions. */
    private def getEntrypoints(root: TypedAst.Root): Set[Symbol.DefnSym] = {
      val s = mutable.Set.empty[Symbol.DefnSym]
      for ((sym, defn) <- root.defs if isEntryPoint(defn)(root)) {
        s += sym
      }
      s.toSet
    }

  }

}


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
import ca.uwaterloo.flix.language.ast.Symbol.isPrimitiveEff
import ca.uwaterloo.flix.language.ast.shared.*
import ca.uwaterloo.flix.language.ast.shared.SymUse.DefSymUse
import ca.uwaterloo.flix.language.ast.{Ast, RigidityEnv, Scheme, SourceLocation, Symbol, Type, TypeConstructor, TypedAst}
import ca.uwaterloo.flix.language.dbg.AstPrinter.*
import ca.uwaterloo.flix.language.errors.EntryPointError
import ca.uwaterloo.flix.language.phase.unification.{TraitEnv, TraitEnvironment}
import ca.uwaterloo.flix.util.{CofiniteEffSet, InternalCompilerException}
import ca.uwaterloo.flix.util.collection.ListMap

import java.util.concurrent.ConcurrentLinkedQueue
import scala.collection.mutable
import scala.jdk.CollectionConverters.*

/**
  * Processes the entry point of the program.
  *
  * The entry point is replaced by a new function (`main%`) that calls the old entry point (`func`).
  *
  * The argument to the `func` must have type `Unit`.
  *
  * If the result type of `func` has type `Unit`,
  * then the result is returned from `main%` as normal.
  * If the result type of `func` is some other type with a `ToString` instance,
  * then the result is printed in `main%`.
  * If the result type of `func` is some other type without a `ToString` instance,
  * then an error is raised.
  *
  * For example, given an entry point `func` with type `Unit -> Float64`,
  * we produce:
  * {{{
  *  pub def main%(): Unit \ IO = {
  *      println(func(args))
  *  }
  * }}}
  */
object EntryPoint {

  // We don't use regions, so we are safe to use the global scope everywhere in this phase.
  private implicit val S: Scope = Scope.Top

  /**
    * The scheme of the entry point function.
    * `Unit -> Unit`
    */
  private val EntryPointScheme = Scheme(Nil, Nil, Nil, Type.mkIoArrow(Type.Unit, Type.Unit, SourceLocation.Unknown))

  /**
    * The default entry point in case none is specified. (`main`)
    */
  private val DefaultEntryPoint = Symbol.mkDefnSym("main")

  /**
    * Introduces a new function `main%` which calls the entry point (if any).
    */
  def run(root: TypedAst.Root)(implicit flix: Flix): (TypedAst.Root, List[EntryPointError]) = flix.phaseNew("EntryPoint") {
    implicit val sctx: SharedContext = SharedContext.mk()
    val newRoot = findOriginalEntryPoint(root) match {
      // Case 1: We have an entry point. Wrap it.
      case Some(entryPoint0) =>
        val entryPoint = visitEntryPoint(entryPoint0, root, TraitEnv(root.traitEnv))
        root.copy(
          defs = root.defs + (entryPoint.sym -> entryPoint),
          entryPoint = Some(entryPoint.sym),
          reachable = getReachable(root) + entryPoint.sym
        )
      // Case 2: No entry point. Don't touch anything.
      case None => root.copy(reachable = getReachable(root))
    }
    (newRoot, sctx.errors.asScala.toList)
  }

  /**
    * Returns all reachable definitions.
    */
  private def getReachable(root: TypedAst.Root): Set[Symbol.DefnSym] = {
    val s = mutable.Set.empty[Symbol.DefnSym]
    for ((sym, defn) <- root.defs) {
      if (defn.spec.ann.isTest || defn.spec.ann.isExport) {
        s += sym
      }
    }
    s.toSet
  }

  /**
    * Finds the entry point in the given `root`.
    */
  private def findOriginalEntryPoint(root: TypedAst.Root)(implicit sctx: SharedContext): Option[TypedAst.Def] = {
    root.entryPoint match {
      case None => root.defs.get(DefaultEntryPoint) match {
        case None => None
        case Some(entryPoint) => Some(entryPoint)
      }
      case Some(sym) => root.defs.get(sym) match {
        case None =>
          val error = EntryPointError.EntryPointNotFound(sym, getArbitrarySourceLocation(root))
          sctx.errors.add(error)
          None
        case Some(entryPoint) => Some(entryPoint)
      }
    }
  }

  /**
    * Retrieves an arbitrary source location from the root.
    */
  private def getArbitrarySourceLocation(root: TypedAst.Root): SourceLocation = {
    root.sources.headOption match {
      // Case 1: Some arbitrary source. Use its location.
      case Some((_, loc)) => loc
      // Case 2: No inputs. Give up and use unknown.
      case None => SourceLocation.Unknown
    }
  }


  /**
    * Checks that the given def is a valid entry point,
    * and returns a new entry point that calls it.
    *
    * The new entry point should be added to the AST.
    */
  private def visitEntryPoint(defn: TypedAst.Def, root: TypedAst.Root, traitEnv: TraitEnv)(implicit sctx: SharedContext, flix: Flix): TypedAst.Def = {
    checkEntryPointArgs(defn, traitEnv)
    checkEntryPointResult(defn, root, traitEnv)
    mkEntryPoint(defn, root)
  }

  /**
    * Checks the entry point function arguments.
    * Returns a flag indicating whether the args should be passed to this function or ignored.
    */
  private def checkEntryPointArgs(defn: TypedAst.Def, traitEnv: TraitEnv)(implicit sctx: SharedContext, flix: Flix): Unit = defn match {
    case TypedAst.Def(sym, TypedAst.Spec(_, _, _, _, _, declaredScheme, _, _, _, _), _, loc) =>

      // First check that there's exactly one argument.
      declaredScheme.base.arrowArgTypes match {

        // Case 1: One arg. Ok :)
        case arg :: Nil =>
          // Case 1.1: Check that `arg` is the Unit parameter, i.e, `defn: Unit -> XYZ`
          if (!isUnitParameter(traitEnv, arg)) {
            val error = EntryPointError.IllegalEntryPointArgs(sym, sym.loc)
            sctx.errors.add(error)
          }

        // Case 2: Multiple args. Error.
        case _ :: _ :: _ =>
          val error = EntryPointError.IllegalEntryPointArgs(sym, sym.loc)
          sctx.errors.add(error)

        // Case 3: Empty arguments. Impossible since this is desugared to Unit.
        // Resilience: OK because this is a desugaring that is always performed by the Weeder.
        case Nil => throw InternalCompilerException("Unexpected empty argument list.", loc)
      }
  }

  /**
    * Returns `true` iff `arg` is the Unit type.
    */
  private def isUnitParameter(traitEnv: TraitEnv, arg: Type)(implicit flix: Flix) = {
    val unitScheme = Scheme.generalize(Nil, Nil, Type.Unit, RigidityEnv.empty)
    Scheme.equal(unitScheme, Scheme.generalize(Nil, Nil, arg, RigidityEnv.empty), traitEnv, ListMap.empty) // TODO ASSOC-TYPES better eqEnv
  }

  /**
    * Checks the entry point function result type.
    * Returns a flag indicating whether the result should be printed, cast, or unchanged.
    */
  private def checkEntryPointResult(defn: TypedAst.Def, root: TypedAst.Root, traitEnv: TraitEnv)(implicit sctx: SharedContext, flix: Flix): Unit = defn match {
    case TypedAst.Def(sym, TypedAst.Spec(_, _, _, _, _, declaredScheme, _, declaredEff, _, _), _, _) =>
      val resultTpe = declaredScheme.base.arrowResultType
      val unitSc = Scheme.generalize(Nil, Nil, Type.Unit, RigidityEnv.empty)
      val resultSc = Scheme.generalize(Nil, Nil, resultTpe, RigidityEnv.empty)

      // Check for IllegalEntryPointEffect
      if (isBadEntryPointEffect(declaredEff)) {
        val error = EntryPointError.IllegalEntryPointEff(sym, declaredEff, declaredEff.loc)
        sctx.errors.add(error)
      }

      // Case 1: XYZ -> Unit.
      val isUnitResult = Scheme.equal(unitSc, resultSc, traitEnv, ListMap.empty) // TODO ASSOC-TYPES better eqEnv

      // Case 2: XYZ -> a with ToString[a]
      val toStringTrait = root.traits(new Symbol.TraitSym(Nil, "ToString", SourceLocation.Unknown)).sym
      val hasToStringConstraint = TraitEnvironment.holds(TraitConstraint(TraitConstraint.Head(toStringTrait, SourceLocation.Unknown), resultTpe, SourceLocation.Unknown), traitEnv, root.eqEnv)

      // Case 3: Bad result type. Error.
      val isBadResultType = !isUnitResult && !hasToStringConstraint

      if (isBadResultType) {
        val error = EntryPointError.IllegalEntryPointResult(sym, resultTpe, sym.loc)
        sctx.errors.add(error)
      }
  }

  /**
    * Returns `true` iff `declaredEff` is not `Pure` and contains some non-primitive effect.
    */
  private def isBadEntryPointEffect(declaredEff: Type): Boolean = {
    if (declaredEff.typeVars.nonEmpty) return true
    // Now that we have the monomorphic effect, we can evaluate it.
    eval(declaredEff) match {
      case Some(CofiniteEffSet.Set(s)) =>
        // Check that it is a set of only primitive effects.
        s.exists(!isPrimitiveEff(_))
      case Some(CofiniteEffSet.Compl(_)) =>
        // A set like `not IO` can never be allowed
        true
      case None =>
        // The effect has an Error, don't throw more errors
        false
    }
  }

  /** Returns the evaluated effect, or `None` if the effect contains `Error`. */
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

  /**
    * Builds the new entry point function that calls the old entry point function.
    */
  private def mkEntryPoint(oldEntryPoint: TypedAst.Def, root: TypedAst.Root)(implicit flix: Flix): TypedAst.Def = {

    val argSym = Symbol.freshVarSym("_unit", Ast.BoundBy.FormalParam, SourceLocation.Unknown)

    val spec = TypedAst.Spec(
      doc = Doc(Nil, SourceLocation.Unknown),
      ann = Annotations.Empty,
      mod = Modifiers.Empty,
      tparams = Nil,
      fparams = List(TypedAst.FormalParam(TypedAst.Binder(argSym, Type.Unit), Modifiers.Empty, Type.Unit, Ast.TypeSource.Ascribed, SourceLocation.Unknown)),
      declaredScheme = EntryPointScheme,
      retTpe = Type.Unit,
      eff = Type.IO,
      tconstrs = Nil,
      econstrs = Nil,
    )

    // NB: Getting the type directly from the scheme assumes the function is not polymorphic.
    // This is a valid assumption with the limitations we set on the entry point.
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
    * @param errors the [[EntryPointError]]s in the AST, if any.
    */
  private case class SharedContext(errors: ConcurrentLinkedQueue[EntryPointError])

}


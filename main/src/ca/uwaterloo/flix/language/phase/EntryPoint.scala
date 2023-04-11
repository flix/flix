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
import ca.uwaterloo.flix.language.ast.{Ast, Scheme, SourceLocation, Symbol, Type, TypeConstructor, TypedAst}
import ca.uwaterloo.flix.language.errors.EntryPointError
import ca.uwaterloo.flix.language.phase.unification.ClassEnvironment
import ca.uwaterloo.flix.util.Validation.{ToSoftFailure, ToSuccess, flatMapN, mapN}
import ca.uwaterloo.flix.util.collection.ListMap
import ca.uwaterloo.flix.util.{InternalCompilerException, Validation}

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

  /**
    * The scheme of the entry point function.
    * `Unit -> Unit`
    */
  private val EntryPointScheme = Scheme(Nil, Nil, Nil, Type.mkImpureArrow(Type.Unit, Type.Unit, SourceLocation.Unknown))

  /**
    * The default entry point in case none is specified. (`main`)
    */
  private val DefaultEntryPoint = Symbol.mkDefnSym("main")


  /**
    * Introduces a new function `main%` which calls the entry point (if any).
    */
  def run(root: TypedAst.Root)(implicit flix: Flix): Validation[TypedAst.Root, EntryPointError] = flix.phase("EntryPoint") {
    flatMapN(findOriginalEntryPoint(root)) {
      // Case 1: We have an entry point. Wrap it.
      case Some(entryPoint0) =>
        mapN(visitEntryPoint(entryPoint0, root, root.classEnv)) {
          entryPoint =>
            root.copy(
              defs = root.defs + (entryPoint.sym -> entryPoint),
              entryPoint = Some(entryPoint.sym)
            )
        }
      // Case 2: No entry point. Don't touch anything.
      case None => root.toSuccess
    }
  }

  /**
    * Finds the entry point in the given `root`.
    */
  private def findOriginalEntryPoint(root: TypedAst.Root)(implicit flix: Flix): Validation[Option[TypedAst.Def], EntryPointError] = {
    root.entryPoint match {
      case None => root.defs.get(DefaultEntryPoint) match {
        case None => None.toSuccess
        case Some(entryPoint) => Some(entryPoint).toSuccess
      }
      case Some(sym) => root.defs.get(sym) match {
        case None => None.toSoftFailure(EntryPointError.EntryPointNotFound(sym, getArbitrarySourceLocation(root)))
        case Some(entryPoint) => Some(entryPoint).toSuccess
      }
    }
  }

  /**
    * Retrieves an arbitrary source location from the root.
    */
  private def getArbitrarySourceLocation(root: TypedAst.Root)(implicit flix: Flix): SourceLocation = {
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
  private def visitEntryPoint(defn: TypedAst.Def, root: TypedAst.Root, classEnv: Map[Symbol.ClassSym, Ast.ClassContext])(implicit flix: Flix): Validation[TypedAst.Def, EntryPointError] = {
    val argsVal = checkEntryPointArgs(defn, classEnv, root)
    val resultVal = checkEntryPointResult(defn, root, classEnv)

    mapN(argsVal, resultVal) {
      case (_, _) =>
        mkEntryPoint(defn, root)
    }
  }

  /**
    * Checks the entry point function arguments.
    * Returns a flag indicating whether the args should be passed to this function or ignored.
    */
  private def checkEntryPointArgs(defn: TypedAst.Def, classEnv: Map[Symbol.ClassSym, Ast.ClassContext], root: TypedAst.Root)(implicit flix: Flix): Validation[Unit, EntryPointError] = defn match {
    case TypedAst.Def(sym, TypedAst.Spec(_, _, _, _, _, declaredScheme, _, _, _, _, loc), _) =>
      val unitSc = Scheme.generalize(Nil, Nil, Type.Unit)

      // First check that there's exactly one argument.
      val argVal = declaredScheme.base.arrowArgTypes match {
        // Case 1: One arg. Ok :)
        case arg :: Nil => Some(arg).toSuccess
        // Case 2: Multiple args. Error.
        case _ :: _ :: _ => None.toSoftFailure(EntryPointError.IllegalEntryPointArgs(sym, sym.loc))
        // Case 3: Empty arguments. Impossible since this is desugared to Unit.
        case Nil => throw InternalCompilerException("Unexpected empty argument list.", loc)
      }

      flatMapN(argVal: Validation[Option[Type], EntryPointError]) {
        // Case 1: Unit -> XYZ. We can ignore the args.
        case Some(arg) if Scheme.equal(unitSc, Scheme.generalize(Nil, Nil, arg), classEnv, ListMap.empty) =>
          ().toSuccess

        // Case 2: Bad arguments. Error
        // Case 3: argVal was None / SoftError
        case Some(_) | None =>
          ().toSoftFailure(EntryPointError.IllegalEntryPointArgs(sym, sym.loc))
      }
  }

  /**
    * Checks the entry point function result type.
    * Returns a flag indicating whether the result should be printed, cast, or unchanged.
    */
  private def checkEntryPointResult(defn: TypedAst.Def, root: TypedAst.Root, classEnv: Map[Symbol.ClassSym, Ast.ClassContext])(implicit flix: Flix): Validation[Unit, EntryPointError] = defn match {
    case TypedAst.Def(sym, TypedAst.Spec(_, _, _, _, _, declaredScheme, _, _, _, _, _), _) =>
      val resultTpe = declaredScheme.base.arrowResultType
      val unitSc = Scheme.generalize(Nil, Nil, Type.Unit)
      val resultSc = Scheme.generalize(Nil, Nil, resultTpe)


      if (Scheme.equal(unitSc, resultSc, classEnv, ListMap.empty)) { // TODO ASSOC-TYPES better eqEnv
        // Case 1: XYZ -> Unit.
        ().toSuccess
      } else {
        // Delay ToString resolution if main has return type unit for testing with lib nix.
        val toString = root.classes(new Symbol.ClassSym(Nil, "ToString", SourceLocation.Unknown)).sym
        if (ClassEnvironment.holds(Ast.TypeConstraint(Ast.TypeConstraint.Head(toString, SourceLocation.Unknown), resultTpe, SourceLocation.Unknown), classEnv)) {
          // Case 2: XYZ -> a with ToString[a]
          ().toSuccess
        } else {
          // Case 3: Bad result type. Error.
          ().toSoftFailure(EntryPointError.IllegalEntryPointResult(sym, resultTpe, sym.loc))
        }
      }
  }

  /**
    * Builds the new entry point function that calls the old entry point function.
    */
  private def mkEntryPoint(oldEntryPoint: TypedAst.Def, root: TypedAst.Root)(implicit flix: Flix): TypedAst.Def = {

    val argSym = Symbol.freshVarSym("_unit", Ast.BoundBy.FormalParam, SourceLocation.Unknown)

    val spec = TypedAst.Spec(
      doc = Ast.Doc(Nil, SourceLocation.Unknown),
      ann = Ast.Annotations.Empty,
      mod = Ast.Modifiers.Empty,
      tparams = Nil,
      fparams = List(TypedAst.FormalParam(argSym, Ast.Modifiers.Empty, Type.Unit, Ast.TypeSource.Ascribed, SourceLocation.Unknown)),
      declaredScheme = EntryPointScheme,
      retTpe = Type.Unit,
      pur = Type.Impure,
      eff = Type.Empty,
      tconstrs = Nil,
      loc = SourceLocation.Unknown
    )

    // NB: Getting the type directly from the scheme assumes the function is not polymorphic.
    // This is a valid assumption with the limitations we set on the entry point.
    val func = TypedAst.Expression.Def(oldEntryPoint.sym, oldEntryPoint.spec.declaredScheme.base, SourceLocation.Unknown)

    // func()
    val call = TypedAst.Expression.Apply(func, List(TypedAst.Expression.Cst(Ast.Constant.Unit, Type.Unit, SourceLocation.Unknown)), oldEntryPoint.spec.declaredScheme.base.arrowResultType, oldEntryPoint.spec.declaredScheme.base.arrowPurityType, oldEntryPoint.spec.declaredScheme.base.arrowEffectType, SourceLocation.Unknown)

    // one of:
    // printUnlessUnit(func(args))
    val printSym = root.defs(new Symbol.DefnSym(None, Nil, "printUnlessUnit", SourceLocation.Unknown)).sym
    val ioSym = root.effects(new Symbol.EffectSym(Nil, "IO", SourceLocation.Unknown)).sym
    val ioTpe = Type.Cst(TypeConstructor.Effect(ioSym), SourceLocation.Unknown)
    val printTpe = Type.mkArrowWithEffect(oldEntryPoint.spec.declaredScheme.base.arrowResultType, Type.Impure, ioTpe, Type.Unit, SourceLocation.Unknown)
    val printFunc = TypedAst.Expression.Def(printSym, printTpe, SourceLocation.Unknown)
    val print = TypedAst.Expression.Apply(printFunc, List(call), Type.Unit, Type.Impure, ioTpe, SourceLocation.Unknown)

    val impl = TypedAst.Impl(
      exp = print,
      inferredScheme = EntryPointScheme
    )

    val sym = new Symbol.DefnSym(None, Nil, "main" + Flix.Delimiter, SourceLocation.Unknown)

    TypedAst.Def(sym, spec, impl)
  }
}


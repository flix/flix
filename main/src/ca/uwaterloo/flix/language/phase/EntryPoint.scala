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
import ca.uwaterloo.flix.language.ast.{Ast, Scheme, SourceLocation, Symbol, Type, TypedAst}
import ca.uwaterloo.flix.language.errors.EntryPointError
import ca.uwaterloo.flix.language.phase.unification.ClassEnvironment
import ca.uwaterloo.flix.util.{InternalCompilerException, Validation}
import ca.uwaterloo.flix.util.Validation.{ToFailure, ToSuccess, flatMapN, mapN}

/**
  * Processes the entry point of the program.
  *
  * The entry point is replaced by a new function (`main%`) that calls the old entry point (`func`).
  *
  * If the argument to `func` has type `Array[String]`,
  *   then the arguments are passed from `main%` to `func`.
  * If the argument to `func` has type `Unit`,
  *   then the arguments to `main%` are ignored.
  * If the argument to `func` has some other type,
  *   an error is raised.
  *
  * If the result type of `func` has type `Unit`,
  *   then the result is returned from `main%` as normal.
  * If the result type of `func` is some other type with a `ToString` instance,
  *   then the result is printed in `main%`.
  * If the result type of `func` is some other type without a `ToString` instance,
  *   then an error is raised.
  *
  *  For example, given an entry point `func` with type `Array[String] -> Float64`,
  *  we produce:
  *  {{{
  *  pub def main%(args: Array[String]): Unit = {
  *      println(func(args))
  *  }
  *  }}}
  */
object EntryPoint {

  /**
    * The resulting scheme of the entry point function.
    * `Array[String] -> Int32`
    */
  private val EntryPointScheme = Scheme(Nil, Nil, Type.mkImpureArrow(Type.mkArray(Type.Str, SourceLocation.Unknown), Type.Int32, SourceLocation.Unknown))


  def run(root: TypedAst.Root)(implicit flix: Flix): Validation[TypedAst.Root, EntryPointError] = flix.phase("Typer") {
    // MATT probably need to check that the entry point exists? But what location to give that?
    root.entryPoint match {
      // Case 1: There is an entry point. Wrap it.
      case Some(sym) =>
        val entrypoint0 = root.defs(sym)
        mapN(visitEntrypoint(entrypoint0, root, root.classEnv)) {
          entrypoint =>
            root.copy(
              defs = root.defs + (entrypoint.sym -> entrypoint),
              entryPoint = Some(entrypoint.sym)
            )
        }
      // Case 2: No entry point. Return the AST unchanged.
      case None => root.toSuccess
    }
  }


  /**
    * Checks that the given def is a valid entrypoint,
    * and returns a new entrypoint that calls it.
    *
    * The new entrypoint should be added to the AST.
    */
  private def visitEntrypoint(defn: TypedAst.Def, root: TypedAst.Root, classEnv: Map[Symbol.ClassSym, Ast.ClassContext])(implicit flix: Flix): Validation[TypedAst.Def, EntryPointError] = {
    val argsActionVal = checkEntryPointArgs(defn, root, classEnv)
    val resultActionVal = checkEntryPointResult(defn, root, classEnv)

    mapN(argsActionVal, resultActionVal) {
      case (argsAction, resultAction) =>
        mkEntryPoint(defn, argsAction, resultAction, root)
    }
  }

  /**
    * Checks the entry point function arguments.
    * Returns a flag indicating whether the args should be passed to this function or ignored.
    */
  private def checkEntryPointArgs(defn: TypedAst.Def, root: TypedAst.Root, classEnv: Map[Symbol.ClassSym, Ast.ClassContext])(implicit flix: Flix): Validation[EntryPointArgsAction, EntryPointError] = defn match {
    case TypedAst.Def(sym, TypedAst.Spec(_, _, _, _, _, declaredScheme, _, _, _), _) =>
      val unitSc = Scheme.generalize(Nil, Type.Unit)

      // First check that there's exactly one argument.
      val argVal = declaredScheme.base.arrowArgTypes match {
        // Case 1: One arg. Ok :)
        case arg :: Nil => arg.toSuccess
        // Case 2: Multiple args. Error.
        case _ :: _ :: _ => EntryPointError.TooManyEntryPointArgs(sym, sym.loc).toFailure
        // Case 3: Empty arguments. Impossible since this is desugared to Unit.
        case Nil => throw InternalCompilerException("Unexpected empty argument list.")
      }

      flatMapN(argVal: Validation[Type, EntryPointError]) {
        arg =>
          val argSc = Scheme.generalize(Nil, arg)
          val stringArraySc = Scheme.generalize(Nil, Type.mkArray(Type.Str, SourceLocation.Unknown))

          if (Scheme.equal(unitSc, argSc, classEnv)) {
            // Case 1: Unit -> XYZ. We can ignore the args.
            EntryPointArgsAction.Ignore.toSuccess
          } else if (Scheme.equal(argSc, stringArraySc, classEnv)) {
            // Case 2: Array[String] -> XYZ. We need to pass along the input args.
            EntryPointArgsAction.Use.toSuccess
          } else {
            // Case 3: Bad arguments. Error.
            EntryPointError.UnexpectedEntryPointArg(sym, arg, sym.loc).toFailure
          }
      }
  }

  /**
    * Checks the entry point function result type.
    * Returns a flag indicating whether the result should be printed, cast, or unchanged.
    */
  private def checkEntryPointResult(defn: TypedAst.Def, root: TypedAst.Root, classEnv: Map[Symbol.ClassSym, Ast.ClassContext])(implicit flix: Flix): Validation[EntryPointResultAction, EntryPointError] = defn match {
    case TypedAst.Def(sym, TypedAst.Spec(_, _, _, _, _, declaredScheme, _, _, _), _) =>
      val resultTpe = declaredScheme.base.arrowResultType
      val unitSc = Scheme.generalize(Nil, Type.Unit)
      val resultSc = Scheme.generalize(Nil, resultTpe)

      val toString = root.classes(new Symbol.ClassSym(Nil, "ToString", SourceLocation.Unknown)).sym

      if (Scheme.equal(unitSc, resultSc, classEnv)) {
        // Case 1: XYZ -> Unit.
        if (declaredScheme.base.arrowEffectType == Type.Pure) {
          // Case 1.1: XYZ ->{Pure} Unit. Need to cast.
          EntryPointResultAction.Cast.toSuccess
        } else {
          // Case 1.2: XYZ ->{Impure} Unit. No cast needed.
          EntryPointResultAction.Nothing.toSuccess
        }
      } else if (ClassEnvironment.holds(Ast.TypeConstraint(toString, resultTpe, SourceLocation.Unknown), classEnv)) {
        // Case 2: XYZ -> a with ToString[a]. Need to print.
        EntryPointResultAction.Print.toSuccess
      } else {
        // Case 3: Bad result type. Error.
        EntryPointError.UnexpectedEntryPointResult(sym, resultTpe, sym.loc).toFailure
      }
  }

  /**
    * Builds the new entry point function that calls the old entry point function.
    */
  private def mkEntryPoint(oldEntryPoint: TypedAst.Def, argsAction: EntryPointArgsAction, resultAction: EntryPointResultAction, root: TypedAst.Root)(implicit flix: Flix): TypedAst.Def = {

    // The formal parameter name must be marked as unused if we don't use it.
    val argsName = argsAction match {
      case EntryPointArgsAction.Use => "args" + Flix.Delimiter
      case EntryPointArgsAction.Ignore => "_args" + Flix.Delimiter
    }

    val argsSym = Symbol.freshVarSym(argsName, Ast.BoundBy.FormalParam, SourceLocation.Unknown)

    val stringArray = Type.mkArray(Type.Str, SourceLocation.Unknown)

    val spec = TypedAst.Spec(
      doc = Ast.Doc(Nil, SourceLocation.Unknown),
      ann = Nil,
      mod = Ast.Modifiers.Empty,
      tparams = Nil,
      fparams = List(TypedAst.FormalParam(argsSym, Ast.Modifiers.Empty, stringArray, SourceLocation.Unknown)),
      declaredScheme = EntryPointScheme,
      retTpe = Type.Unit,
      eff = Type.Impure,
      loc = SourceLocation.Unknown
    )

    // NB: Getting the type directly from the scheme assumes the function is not polymorphic.
    // This is a valid assumption with the limitations we set on the entry point.
    val func = TypedAst.Expression.Def(oldEntryPoint.sym, oldEntryPoint.spec.declaredScheme.base, SourceLocation.Unknown)

    // Apply to the arguments if we use them, Unit if not.
    val arg = argsAction match {
      case EntryPointArgsAction.Use => TypedAst.Expression.Var(argsSym, stringArray, SourceLocation.Unknown)
      case EntryPointArgsAction.Ignore => TypedAst.Expression.Unit(SourceLocation.Unknown)
    }

    // one of:
    // - func(args)
    // - func()
    val call = TypedAst.Expression.Apply(func, List(arg), oldEntryPoint.spec.declaredScheme.base.arrowResultType, oldEntryPoint.spec.declaredScheme.base.arrowEffectType, SourceLocation.Unknown)

    // one of:
    // - println(func(args))
    // - println(func())
    // - func(args)
    // - func()
    // - func(args) as Impure
    // - func() as Impure
    val print = resultAction match {
      // Case 1: We need to print the result
      case EntryPointResultAction.Print =>
        val printSym = root.defs(new Symbol.DefnSym(None, Nil, "println", SourceLocation.Unknown)).sym
        val printTpe = Type.mkImpureArrow(oldEntryPoint.spec.declaredScheme.base.arrowResultType, Type.Unit, SourceLocation.Unknown)
        val printFunc = TypedAst.Expression.Def(printSym, printTpe, SourceLocation.Unknown)
        TypedAst.Expression.Apply(printFunc, List(call), Type.Unit, Type.Impure, SourceLocation.Unknown)
      // Case 2: No printing, but the result is Pure. Cast it.
      case EntryPointResultAction.Cast =>
        TypedAst.Expression.Cast(call, None, Some(Type.Impure), call.tpe, Type.Impure, SourceLocation.Unknown)
      case EntryPointResultAction.Nothing => call
    }

    val impl = TypedAst.Impl(
      exp = print,
      inferredScheme = EntryPointScheme
    )

    val sym = new Symbol.DefnSym(None, Nil, "main" + Flix.Delimiter, SourceLocation.Unknown)

    TypedAst.Def(sym, spec, impl)
  }

  /**
    * Describes the action to take with the arguments to main.
    */
  private sealed trait EntryPointArgsAction

  private object EntryPointArgsAction {
    /**
      * Pass the arguments from main to the wrapped entry point.
      */
    case object Use extends EntryPointArgsAction

    /**
      * Ignore the arguments to main.
      */
    case object Ignore extends EntryPointArgsAction
  }

  /**
    * Describes the action to take with the result from the wrapped entry point.
    */
  private sealed trait EntryPointResultAction

  private object EntryPointResultAction {

    /**
      * Print the result to the terminal.
      */
    case object Print extends EntryPointResultAction

    /**
      * Cast the result to Impure.
      */
    case object Cast extends EntryPointResultAction

    /**
      * Take no action of the result.
      */
    case object Nothing extends EntryPointResultAction
  }

}


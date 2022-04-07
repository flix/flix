package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.{Ast, Scheme, SourceLocation, Symbol, Type, TypedAst}
import ca.uwaterloo.flix.language.errors.EntryPointError
import ca.uwaterloo.flix.language.phase.unification.ClassEnvironment
import ca.uwaterloo.flix.language.phase.util.PredefinedClasses
import ca.uwaterloo.flix.util.Validation
import ca.uwaterloo.flix.util.Validation.{ToSuccess, mapN}

object EntryPoint {

  /**
    * The resulting scheme of the entry point function.
    * `Array[String] -> Int32`
    */
  private val EntryPointScheme = Scheme(Nil, Nil, Type.mkImpureArrow(Type.mkArray(Type.Str, SourceLocation.Unknown), Type.Int32, SourceLocation.Unknown))


  def run(root: TypedAst.Root)(implicit flix: Flix): Validation[TypedAst.Root, EntryPointError] = flix.phase("Typer") {
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
  private def visitEntrypoint(defn: TypedAst.Def, root: TypedAst.Root, classEnv: Map[Symbol.ClassSym, Ast.ClassContext])(implicit flix: Flix): Validation[TypedAst.Def, EntryPointError] = defn match {
    case TypedAst.Def(sym, spec, impl) =>

      val argsActionVal = checkEntrypointArgs(defn, root, classEnv)
      val resultActionVal = checkEntrypointResult(defn, root, classEnv)

      mapN(argsActionVal, resultActionVal) {
        case (argsAction, resultAction) =>
          mkEntrypoint(defn, argsAction, resultAction, root)
      }
  }

  // MATT docs
  private def checkEntrypointArgs(defn: TypedAst.Def, root: TypedAst.Root, classEnv: Map[Symbol.ClassSym, Ast.ClassContext])(implicit flix: Flix): Validation[EntrypointArgsAction, EntryPointError] = defn match {
    case TypedAst.Def(sym, TypedAst.Spec(doc, ann, mod, tparams, fparams, declaredScheme, retTpe, eff, loc), impl) =>
      val unitSc = Scheme.generalize(Nil, Type.Unit)
      val argSc = Scheme.generalize(Nil, declaredScheme.base.arrowArgTypes.head) // MATT must check only one parameter
      val stringArraySc = Scheme.generalize(Nil, Type.mkArray(Type.Str, SourceLocation.Unknown))

      // MATT case docs
      if (Scheme.equal(unitSc, argSc, classEnv)) {
        EntrypointArgsAction.Ignore.toSuccess
      } else if (Scheme.equal(argSc, stringArraySc, classEnv)) {
        EntrypointArgsAction.Use.toSuccess
      } else {
        ??? // MATT throw error about bad args
      }
  }

  // MATT docs
  private def checkEntrypointResult(defn: TypedAst.Def, root: TypedAst.Root, classEnv: Map[Symbol.ClassSym, Ast.ClassContext])(implicit flix: Flix): Validation[EntrypointResultAction, EntryPointError] = defn match {
    case TypedAst.Def(sym, TypedAst.Spec(doc, ann, mod, tparams, fparams, declaredScheme, retTpe, eff, loc), impl) =>
      val resultTpe = declaredScheme.base.arrowResultType
      val unitSc = Scheme.generalize(Nil, Type.Unit)
      val resultSc = Scheme.generalize(Nil, resultTpe)

      val toString = root.classes(new Symbol.ClassSym(Nil, "ToString", SourceLocation.Unknown)).sym

      // MATT case docs
      if (Scheme.equal(unitSc, resultSc, classEnv)) {
        if (declaredScheme.base.arrowEffectType == Type.Pure) {
          EntrypointResultAction.Cast.toSuccess
        } else {
          EntrypointResultAction.Nothing.toSuccess
        }
      } else if (ClassEnvironment.holds(Ast.TypeConstraint(toString, resultTpe, SourceLocation.Unknown), classEnv)) {
        EntrypointResultAction.Print.toSuccess
      } else {
        ??? // MATT throw error about bad result
      }
  }

  // MATT docs
  private def mkEntrypoint(oldEntrypoint: TypedAst.Def, argsAction: EntrypointArgsAction, resultAction: EntrypointResultAction, root: TypedAst.Root)(implicit flix: Flix): TypedAst.Def = {

    // The formal parameter name must be marked as unused if we don't use it.
    val argsName = argsAction match {
      case EntrypointArgsAction.Use => "args" + Flix.Delimiter
      case EntrypointArgsAction.Ignore => "_args" + Flix.Delimiter
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
    // This is a valid assumption with the limitations we set on the entrypoint.
    val func = TypedAst.Expression.Def(oldEntrypoint.sym, oldEntrypoint.spec.declaredScheme.base, SourceLocation.Unknown)

    // Apply to the arguments if we use them, Unit if not.
    val arg = argsAction match {
      case EntrypointArgsAction.Use => TypedAst.Expression.Var(argsSym, stringArray, SourceLocation.Unknown)
      case EntrypointArgsAction.Ignore => TypedAst.Expression.Unit(SourceLocation.Unknown)
    }

    // one of:
    // - func(args)
    // - func()
    val call = TypedAst.Expression.Apply(func, List(arg), oldEntrypoint.spec.declaredScheme.base.arrowResultType, oldEntrypoint.spec.declaredScheme.base.arrowEffectType, SourceLocation.Unknown)

    // one of:
    // - println(func(args))
    // - println(func())
    // - func(args)
    // - func()
    // - func(args) as Impure
    // - func() as Impure
    val print = resultAction match {
      // Case 1: We need to print the result
      case EntrypointResultAction.Print =>
        val printSym = root.defs(new Symbol.DefnSym(None, Nil, "println", SourceLocation.Unknown)).sym
        val printTpe = Type.mkImpureArrow(oldEntrypoint.spec.declaredScheme.base.arrowResultType, Type.Unit, SourceLocation.Unknown)
        val printFunc = TypedAst.Expression.Def(printSym, printTpe, SourceLocation.Unknown)
        TypedAst.Expression.Apply(printFunc, List(call), Type.Unit, Type.Impure, SourceLocation.Unknown)
      // Case 2: No printing, but the result is Pure. Cast it.
      case EntrypointResultAction.Cast =>
        TypedAst.Expression.Cast(call, None, Some(Type.Impure), call.tpe, Type.Impure, SourceLocation.Unknown)
      case EntrypointResultAction.Nothing => call
    }

    val impl = TypedAst.Impl(
      exp = print,
      inferredScheme = EntryPointScheme
    )

    val sym = new Symbol.DefnSym(None, Nil, "main" + Flix.Delimiter, SourceLocation.Unknown)

    TypedAst.Def(sym, spec, impl)
  }

  // MATT docs for all of this
  private sealed trait EntrypointArgsAction

  private object EntrypointArgsAction {
    case object Use extends EntrypointArgsAction

    case object Ignore extends EntrypointArgsAction
  }

  private sealed trait EntrypointResultAction

  private object EntrypointResultAction {
    case object Print extends EntrypointResultAction

    case object Cast extends EntrypointResultAction

    case object Nothing extends EntrypointResultAction
  }

}


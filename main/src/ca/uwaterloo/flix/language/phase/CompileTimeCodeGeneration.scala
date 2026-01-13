package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.TypedAst.DefaultHandler
import ca.uwaterloo.flix.language.ast.shared.BoundBy.FormalParam
import ca.uwaterloo.flix.language.ast.shared.SymUse.DefSymUse
import ca.uwaterloo.flix.language.ast.shared.{Annotation, BoundBy, Constant, Scope, SymUse, TypeSource}
import ca.uwaterloo.flix.language.ast.{Kind, Name, SourceLocation, Symbol, Type, TypeConstructor, TypedAst}
import ca.uwaterloo.flix.language.dbg.AstPrinter.DebugTypedAst
import ca.uwaterloo.flix.util.collection.CofiniteSet
import ca.uwaterloo.flix.util.{InternalCompilerException, ParOps, Result}

object CompileTimeCodeGeneration {
  /** The name of the generated function that returns all tests in a module. */
  val getTestsFnName = "getTests"

  /** Suffix appended to reflected test function names to avoid conflicts. */
  val reflectedTestNameSuffix = "€reflected$"

  /** Fully qualified name for the UnitTest enum. */
  val unitTestEnum = "Reflect.UnitTest"

  /** Fully qualified name for the function to create IO UnitTests. */
  val mkIOUnitTest = "Reflect.mkIOUnitTest"

  /** Fully qualified name for the function to create Pure UnitTests. */
  val mkPureUnitTest = "Reflect.mkPureUnitTest"

  /**
    * Main entry point for compile-time code generation phase.
    *
    * This phase performs two key transformations:
    * 1. Introduces test reflection infrastructure by generating a getTests function for each module
    * 2. Wraps entry point functions with their required default effect handlers
    *
    * @param root The typed AST root to transform
    * @return The transformed AST root with generated code
    */
  def run(root: TypedAst.Root)(implicit flix: Flix): TypedAst.Root = flix.phase("Compile Time Code Generation") {
    val rootWithTests = introduceTests(root)
    val defsWithDefaultHandlers = ParOps.parMapValues(rootWithTests.defs) {
      defn0 =>
        // If the definition is an entry point it is wrapped with the required default handlers before the rest of lowering.
        // For example, tests with an Assert effect will be wrapped with a call to Assert.runWithIO
        if (EntryPoints.isEntryPoint(defn0)(rootWithTests)) {
          wrapDefWithDefaultHandlers(defn0, rootWithTests, excludeAssert = false)
        } else {
          defn0
        }
    }
    rootWithTests.copy(defs = defsWithDefaultHandlers)
  }
  def generateGetTestsDocString(ns: List[String]) : List[String] = {
      val firstLine = if(ns.isEmpty) {
        f"This function returns all of the program's tests as a `Vector[$unitTestEnum]`."
      } else {
        f"This function returns all of ${ns.mkString(".")}'s tests (including those in children modules) as a `Vector[$unitTestEnum]`."
      }
    firstLine::List(
      "This function is generated at compile time and thus has no source code backing it up",
    )
  }
  /**
    * Creates a lambda expression that takes a Unit argument and calls the given definition.
    *
    * Produces:
    * {{{
    *     arg0 -> defn(arg0)
    * }}}
    *
    * This is used to wrap test functions so they can be stored and invoked later
    * by the test framework.
    *
    * @param defnSym The symbol of the definition to call
    * @param fnEff   The effect type of the function
    * @return A lambda expression wrapping the function call
    */
  private def createFnArg(defnSym: Symbol.DefnSym, fnEff: Type)(implicit flix: Flix): TypedAst.Expr = {
    val fbind =
      Symbol.freshVarSym("arg0", BoundBy.FormalParam, SourceLocation.Unknown)(Scope.Top, flix)

    TypedAst.Expr.Lambda(
      TypedAst.FormalParam(
        TypedAst.Binder(fbind, Type.Unit),
        Type.Unit,
        TypeSource.Inferred,
        SourceLocation.Unknown
      ),
      TypedAst.Expr.ApplyDef(
        DefSymUse(defnSym, SourceLocation.Unknown),
        TypedAst.Expr.Var(
          fbind,
          Type.Unit,
          SourceLocation.Unknown
        ) :: Nil,
        Nil,
        Type.mkArrowWithEffect(Type.Unit, fnEff, Type.Unit, SourceLocation.Unknown),
        Type.Unit,
        fnEff,
        SourceLocation.Unknown
      ),
      Type.mkArrowWithEffect(Type.Unit, fnEff, Type.Unit, SourceLocation.Unknown),
      SourceLocation.Unknown
    )
  }

  /**
    * Creates a call to either `mkIOUnitTest` or `mkPureUnitTest` that constructs
    * a UnitTest value containing metadata about a test function.
    *
    * The generated call includes:
    * - Test name (defnSym name without the reflected suffix)
    * - Source file location
    * - Line and column positions (start and end)
    * - A lambda wrapping the test function
    *
    * @param defnSym The symbol of the test definition
    * @param defn    The test definition itself
    * @return An expression that creates a UnitTest value
    */
  private def createMkUnitTestCall(defnSym: Symbol.DefnSym, defn: TypedAst.Def)(implicit flix: Flix): TypedAst.Expr = {
    val mkStringArg =
      (str: String) => TypedAst.Expr.Cst(Constant.Str(str), Type.Str, SourceLocation.Unknown)

    val mkIntArg =
      (int: Int) => TypedAst.Expr.Cst(Constant.Int32(int), Type.Int32, SourceLocation.Unknown)

    val (eff, symUseName) = {
      // We can use unsafeGet as we now that the spec is eval-able from being in Lowering
      // If it weren't, this would have failed on the Entry points test checks
      if (Type.eval(defn.spec.eff).unsafeGet.contains(Symbol.IO)) {
        (Type.mkUnion(Type.IO, Type.Assert, SourceLocation.Unknown), mkIOUnitTest)
      } else {
        (Type.Assert, mkPureUnitTest)
      }
    }

    val symUse =
      SymUse.DefSymUse(Symbol.mkDefnSym(symUseName), SourceLocation.Unknown)

    TypedAst.Expr.ApplyDef(
      targs = List(
        Type.Cst(
          TypeConstructor.Enum(Symbol.mkEnumSym(unitTestEnum), Kind.Star),
          SourceLocation.Unknown
        )
      ),
      symUse = symUse,
      exps = List(
        mkStringArg(defnSym.toString.dropRight(reflectedTestNameSuffix.length)),
        mkStringArg(defnSym.loc.source.toString),
        mkIntArg(defnSym.loc.start.lineOneIndexed),
        mkIntArg(defnSym.loc.start.colOneIndexed),
        mkIntArg(defnSym.loc.end.lineOneIndexed),
        mkIntArg(defnSym.loc.end.colOneIndexed),
        createFnArg(defnSym, eff)
      ),
      itpe = Type.mkPureUncurriedArrow(
        List(
          Type.Str,
          Type.Str,
          Type.Int32,
          Type.Int32,
          Type.Int32,
          Type.Int32,
          Type.mkArrowWithEffect(Type.Unit, eff, Type.Unit, SourceLocation.Unknown)
        ),
        Type.Cst(
          TypeConstructor.Enum(Symbol.mkEnumSym(unitTestEnum), Kind.Star),
          SourceLocation.Unknown
        ),
        SourceLocation.Unknown
      ),
      tpe = Type.Cst(
        TypeConstructor.Enum(Symbol.mkEnumSym(unitTestEnum), Kind.Star),
        SourceLocation.Unknown
      ),
      eff = Type.Pure,
      loc = SourceLocation.Unknown
    )
  }

  /**
    * Creates a call to a module's `getTests` function.
    *
    * This is used to retrieve all tests from a module or submodule when building
    * the complete test vector.
    *
    * @param sym The symbol of the getTests function to call
    * @return A call to the specified getTests function
    */
  private def createCallToGetTests(sym: Symbol.DefnSym): TypedAst.Expr = {
    val symUse =
      SymUse.DefSymUse(sym, SourceLocation.Unknown)

    TypedAst.Expr.ApplyDef(
      targs = Nil,
      symUse = symUse,
      exps = List(TypedAst.Expr.Cst(Constant.Unit, Type.Unit, SourceLocation.Unknown)),
      itpe = Type.mkPureUncurriedArrow(
        List(Type.Unit),
        Type.mkVector(
          Type.Cst(
            TypeConstructor.Enum(Symbol.mkEnumSym(unitTestEnum), Kind.Star),
            SourceLocation.Unknown
          ),
          SourceLocation.Unknown
        ),
        SourceLocation.Unknown
      ),
      tpe = Type.mkVector(
        Type.Cst(
          TypeConstructor.Enum(Symbol.mkEnumSym(unitTestEnum), Kind.Star),
          SourceLocation.Unknown
        ),
        SourceLocation.Unknown
      ),
      eff = Type.Pure,
      loc = SourceLocation.Unknown
    )
  }

  /**
    * Creates a flattened vector combining tests from batch functions and child modules.
    *
    * Produces:
    * {{{
    *     Vector.flatten(Vector#[getTestsBatched1(), ..., children1.getTests(), ...])
    * }}}
    *
    * This aggregates all tests in a module by:
    * 1. Calling each batch getTests function (for tests defined in this module)
    * 2. Calling each child module's getTests function
    * 3. Flattening the resulting nested vector into a single vector of tests
    *
    * @param batchedTests     Symbols of the batch getTests functions in this module
    * @param childrenGetTests Symbols of the getTests functions in child modules
    * @return An expression that produces a flattened vector of all tests
    */
  private def createCombinedTestVector(
    batchedTests: List[Symbol.DefnSym],
    childrenGetTests: List[Symbol.DefnSym]
  ): TypedAst.Expr = {

    val symUseFlatten =
      SymUse.DefSymUse(Symbol.mkDefnSym("Vector.flatten"), SourceLocation.Unknown)
    val childrenTests =
      childrenGetTests.map(createCallToGetTests)
    val tests = batchedTests.map(createCallToGetTests)
    val allTests =
      tests ::: childrenTests

    val nestedVectorType =
      Type.mkVector(
        Type.mkVector(
          Type.Cst(
            TypeConstructor.Enum(Symbol.mkEnumSym(unitTestEnum), Kind.Star),
            SourceLocation.Unknown
          ),
          SourceLocation.Unknown
        ),
        SourceLocation.Unknown
      )

    val vectorToFlatten =
      TypedAst.Expr.VectorLit(
        exps = allTests,
        tpe = nestedVectorType,
        eff = Type.Pure,
        loc = SourceLocation.Unknown
      )

    TypedAst.Expr.ApplyDef(
      targs = List(
        Type.Cst(
          TypeConstructor.Enum(Symbol.mkEnumSym(unitTestEnum), Kind.Star),
          SourceLocation.Unknown
        )
      ),
      symUse = symUseFlatten,
      exps = List(vectorToFlatten),
      itpe = Type.mkPureUncurriedArrow(
        List(nestedVectorType),
        Type.mkVector(
          Type.Cst(
            TypeConstructor.Enum(Symbol.mkEnumSym(unitTestEnum), Kind.Star),
            SourceLocation.Unknown
          ),
          SourceLocation.Unknown
        ),
        SourceLocation.Unknown
      ),
      tpe = Type.mkVector(
        Type.Cst(
          TypeConstructor.Enum(Symbol.mkEnumSym(unitTestEnum), Kind.Star),
          SourceLocation.Unknown
        ),
        SourceLocation.Unknown
      ),
      eff = Type.Pure,
      loc = SourceLocation.Unknown
    )
  }

  /**
    * Creates a copy of a test definition for reflection purposes.
    *
    * The copied definition:
    * - Has a new symbol with the reflected suffix appended to its name
    * - Has the @Test annotation removed (it's no longer a direct test)
    * - Is wrapped with default handlers (excluding Assert handler to be able to handle it in a custom way by the user)
    *
    * This allows the test framework to invoke tests through reflection while ensuring
    * proper effect handling.
    *
    * @param defsym The original test definition symbol
    * @param defn   The original test definition
    * @param root   The typed AST root
    * @return A tuple of (new symbol, transformed definition)
    */
  private def copyDefnForReflectTesting(defsym: Symbol.DefnSym, defn: TypedAst.Def, root: TypedAst.Root)(implicit flix: Flix): (Symbol.DefnSym, TypedAst.Def) = {
    val defnSym = Symbol.mkDefnSym(Name.mkUnlocatedNName(defsym.namespace), Name.Ident(defsym.name ++ reflectedTestNameSuffix, defsym.loc))
    (defnSym, wrapDefWithDefaultHandlers(defn.copy(
      sym = defnSym,
      spec = defn.spec.copy(
        ann = defn.spec.ann.copy(
          annotations = defn.spec.ann.annotations.filterNot(_.isInstanceOf[Annotation.Test])
        )
      )
    ), root, excludeAssert = true))
  }

  /**
    * Creates a batch getTests function that returns a vector of test metadata.
    *
    * Batching is used to avoid creating functions with too much data directly
    * in the body, which would create expressions too large for the bytecode.
    * Instead, tests are grouped into batches of 128.
    *
    * The generated function has the name: `{originalName}_test_batch_{index}€reflected$`
    *
    * @param defsym The symbol of the original getTests function
    * @param defn   The original getTests definition
    * @param index  The batch index number
    * @param batch  The tests to include in this batch
    * @return A tuple of (batch function symbol, batch function definition)
    */
  private def copyDefnForReflectBatchingTests(defsym: Symbol.DefnSym, defn: TypedAst.Def, index: Int, batch: Iterable[(Symbol.DefnSym, TypedAst.Def)])(implicit flix: Flix): (Symbol.DefnSym, TypedAst.Def) = {
    val defnSym = Symbol.mkDefnSym(Name.mkUnlocatedNName(defsym.namespace), Name.Ident(defsym.name ++ f"_test_batch_$index" ++ reflectedTestNameSuffix, defsym.loc))
    val vector = defn.exp.asInstanceOf[TypedAst.Expr.VectorLit]
    val newVector = vector.copy(
      exps = batch.map(t => createMkUnitTestCall(t._1, t._2)).toList
    )
    (defnSym, defn.copy(
      sym = defnSym,
      exp = newVector
    ))
  }

  /**
    * Introduces test reflection infrastructure into the AST.
    *
    * This transformation:
    * 1. Creates reflected copies of all test functions (with €reflected$ suffix)
    * 2. Groups tests by module
    * 3. Creates batch getTests functions (128 tests per batch) for each module
    * 4. Updates each module's getTests function to combine:
    *    - All batch functions from this module
    *    - All child module getTests functions
    *
    * The result is a hierarchical test discovery system where calling any module's
    * getTests function returns all tests in that module and its submodules.
    *
    * @param root The typed AST root
    * @return The transformed AST root with test reflection infrastructure
    */
  private def introduceTests(root: TypedAst.Root)(implicit flix: Flix): TypedAst.Root = {
    // Create reflected copies of all test definitions that are marked with @Test
    // and not marked with @Skip. Each copy gets a new symbol with the €reflected$ suffix
    // and has the @Test annotation removed. The reflected version is wrapped with
    // default handlers (excluding Assert) to ensure proper effect handling.
    // This is needed in order to preserve a version of the test that is not wrapped with Assert's default handler
    val testCopies = root.defs
      .filter(d => d._2.spec.ann.isTest && !d._2.spec.ann.isSkip)
      .map {
        case (s, d) => copyDefnForReflectTesting(s, d, root)
      }

    // Group all reflected test definitions by their containing module.
    // This creates a map from module symbols to the tests defined in that module,
    // which will be used to generate batch getTests functions for each module.
    val testsByModule: Map[Symbol.ModuleSym, Iterable[(Symbol.DefnSym, TypedAst.Def)]] =
      testCopies
        .groupBy(_._1.namespace)
        .map {
          case (ns, defns) =>
            Symbol.mkModuleSym(ns) -> defns
        }

    // For each module in the AST, generate new getTests function definitions:
    // 1. Find the existing getTests function in the module
    // 2. Create batch functions (128 tests each) for all tests in this module
    // 3. Update the main getTests function to call all batch functions and
    //    recursively call getTests from all child modules
    val newGetTestsFns =
      root.modules.m.flatMap {
        case (msym, melements) =>
          // Get all tests that belong to this specific module
          val modTests =
            testsByModule.getOrElse(msym, List())

          // Find the existing getTests function symbol in this module
          val oldGetTests =
            melements.collectFirst {
              case x: Symbol.DefnSym if x.text == getTestsFnName => x
            }
          oldGetTests match {
            case Some(oldGetTestSym) =>
              // Get the definition of the existing getTests function
              val oldGetTestsDef =
                root.defs(oldGetTestSym)

              // Split tests into batches of 128 and create a batch function for each group.
              // Batching prevents creating functions with too much data in their body,
              // which would create expressions too large for the bytecode.
              val batchDefs = modTests.grouped(128).zipWithIndex.map {
                case (g, i) => copyDefnForReflectBatchingTests(oldGetTestSym, oldGetTestsDef, i, g)
              }.toList

              // Find all child modules' getTests functions so we can call them
              // to create a hierarchical test discovery system
              val getTestsChildrenCalls =
                melements.collect {
                  case x: Symbol.ModuleSym =>
                    root.modules
                      .get(x)
                      .collectFirst {
                        case y: Symbol.DefnSym if y.text == getTestsFnName => y
                      }
                }.flatten

              // Create the updated getTests function that:
              // - Calls all batch functions (for tests in this module)
              // - Calls all child module getTests functions (for tests in submodules)
              // - Flattens the results into a single vector
              Some(
                (oldGetTestSym -> oldGetTestsDef.copy(
                  exp = createCombinedTestVector(batchDefs.map(_._1), getTestsChildrenCalls)
                )
                  ) :: batchDefs
              )

            case None =>
              // This module has no getTests function, so nothing to update
              // However, this only happens with non-proper modules like traits which don't have tests
              None
          }
      }.flatten

    // Return the updated AST with:
    // - All original definitions
    // - All reflected test copies (with €reflected$ suffix)
    // - All new and updated getTests functions (both main and batch versions)
    root.copy(defs = root.defs ++ testCopies ++ newGetTestsFns)
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
    * @param currentDef The entry point function definition to wrap
    * @param root       The typed AST root
    * @return The wrapped function definition with all necessary default effect handlers
    */
  private def wrapDefWithDefaultHandlers(currentDef: TypedAst.Def, root: TypedAst.Root, excludeAssert: Boolean)(implicit flix: Flix): TypedAst.Def = {
    // Obtain the concrete effect set of the definition that is going to be wrapped.
    // We are expecting entry points, and all entry points should have a concrete effect set
    // Obtain the concrete effect set of the definition that is going to be wrapped.
    // We are expecting entry points, and all entry points should have a concrete effect set
    val defEffects: CofiniteSet[Symbol.EffSym] = Type.eval(currentDef.spec.eff) match {
      case Result.Ok(s) => s
      // This means eff is either not well-formed or it has type variables.
      // Either way, in this case we will wrap with all default handlers
      // to make sure that the effects present in the signature that have default handlers are handled
      case Result.Err(_) => throw InternalCompilerException("Unexpected illegal effect set on entry point", currentDef.spec.eff.loc)
    }
    // Gather only the default handlers for the effects appearing in the signature of the definition.
    val handlersForEffects =
      root.defaultHandlers.filter(h => defEffects.contains(h.handledSym))

    val requiredHandlers =
      if (excludeAssert)
        handlersForEffects.filterNot(_.handledSym == Symbol.Assert)
      else
        handlersForEffects

    // Wrap the expression in each of the required default handlers.
    // Right now, the order depends on the order of defaultHandlers.
    requiredHandlers.foldLeft(currentDef)((defn, handler) => wrapInHandler(defn, handler))
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
    * @param defn           The entry point function definition to wrap
    * @param defaultHandler Information about the default handler to apply
    * @return The wrapped function definition with updated effect signature
    */
  private def wrapInHandler(defn: TypedAst.Def, defaultHandler: DefaultHandler)(implicit flix: Flix): TypedAst.Def = {
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
          TypedAst.Binder(Symbol.freshVarSym("_", FormalParam, expLoc)(Scope.Top, flix), Type.Unit),
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

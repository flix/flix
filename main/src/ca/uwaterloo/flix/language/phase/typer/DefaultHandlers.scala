package ca.uwaterloo.flix.language.phase.typer

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.{Kind, KindedAst, Scheme, SourceLocation, Symbol, Type, TypeConstructor, TypedAst}
import ca.uwaterloo.flix.language.ast.shared.Scope
import ca.uwaterloo.flix.language.errors.TypeError
import ca.uwaterloo.flix.language.phase.Typer.SharedContext
import ca.uwaterloo.flix.language.phase.unification.{EqualityEnv, TraitEnv}
import ca.uwaterloo.flix.util.Validation
import scala.collection.mutable

object DefaultHandlers {
  /**
    * visitDefaultHandlers takes a root and returns a list of the correct/valid default handlers that exist
    * in the program
    */
  def visitDefaultHandlers(root: KindedAst.Root)(implicit flix: Flix, sctx: SharedContext, traitEnv: TraitEnv, eqEnv: EqualityEnv): List[TypedAst.DefaultHandler] = {
    val defaultHandlers = root.defs.filter {
      case (_, defn) => defn.spec.ann.isDefaultHandler
    }

    val validHandlers = defaultHandlers.flatMap {
      case (sym, defn) => checkHandler(sym, defn, root)
    }

    // Check for [[TypeError.DuplicatedDefaultHandlers]].
    val seen = mutable.Map.empty[Symbol.EffSym, SourceLocation]
    for (TypedAst.DefaultHandler(handlerSym, _, handledSym) <- validHandlers) {
      val loc1 = handlerSym.loc
      seen.get(handledSym) match {
        case None =>
          seen.put(handledSym, loc1)
        case Some(loc2) =>
          sctx.errors.add(TypeError.DuplicateDefaultHandler(handledSym, loc1, loc2))
          sctx.errors.add(TypeError.DuplicateDefaultHandler(handledSym, loc2, loc1))
      }
    }

    validHandlers.toList
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
    * @return [[Validation]] of [[TypedAst.DefaultHandler]] or [[TypeError]]
    */
  private def checkHandler(handlerSym: Symbol.DefnSym, handlerDef: KindedAst.Def, root: KindedAst.Root)(implicit flix: Flix, sctx: SharedContext, traitEnv: TraitEnv, eqEnv: EqualityEnv): Option[TypedAst.DefaultHandler] = {
    var errors = false
    // All default handlers must be public
    if (!handlerDef.spec.mod.isPublic) {
      sctx.errors.add(TypeError.NonPublicDefaultHandler(handlerSym, handlerSym.loc))
      errors = true
    }
    // The Default Handler must reside in the companion module of the effect.
    // Hence we use the namespace of the handler to construct the expected
    // effect symbol and look it up in the AST.
    val effFqn = handlerSym.namespace.mkString(".")
    val effSym = Symbol.mkEffSym(effFqn)
    val companionEffect = root.effects.get(effSym)
    companionEffect match {
      case None =>
        sctx.errors.add(TypeError.DefaultHandlerNotInModule(handlerSym, handlerSym.loc))
        None
      // The default handler is NOT in the companion module of an effect
      case Some(_) =>
        // Synthetic location of our handler
        val loc = handlerSym.loc.asSynthetic
        // There is a valid effect to wrap
        val handledEff = Type.Cst(TypeConstructor.Effect(effSym, Kind.Eff), loc)
        val declaredScheme = handlerDef.spec.sc
        // Generate expected scheme for generating IO
        val expectedSchemeIO = getDefaultHandlerTypeScheme(handledEff, Type.IO, loc)
        // Check if handler's scheme fits any of the valid handler's schemes and if not generate an error
        if (!Scheme.equal(expectedSchemeIO, declaredScheme, traitEnv, eqEnv, Nil)(Scope.Top, flix)) {
          sctx.errors.add(TypeError.IllegalDefaultHandlerSignature(effSym, handlerSym, handlerSym.loc))
          errors = true
        }
        if (!errors) {
          Some(TypedAst.DefaultHandler(handlerSym, handledEff, effSym))
        } else {
          None
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
    val a = Type.freshVar(Kind.Star, loc)(Scope.Top, flix)
    val ef = Type.freshVar(Kind.Eff, loc)(Scope.Top, flix)
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
}

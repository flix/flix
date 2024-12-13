/*
 * Copyright 2021 Magnus Madsen
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
package ca.uwaterloo.flix.language.phase.extra

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.api.lsp.acceptors.AllAcceptor
import ca.uwaterloo.flix.api.lsp.{Consumer, Index, Visitor}
import ca.uwaterloo.flix.language.ast.TypedAst.Predicate.{Body, Head}
import ca.uwaterloo.flix.language.ast.TypedAst.*
import ca.uwaterloo.flix.language.ast.shared.{Annotation, Annotations, SymUse, TraitConstraint}
import ca.uwaterloo.flix.language.ast.shared.SymUse.DefSymUse
import ca.uwaterloo.flix.language.ast.{Ast, SourceLocation, Symbol, Type, TypeConstructor, TypedAst}
import ca.uwaterloo.flix.language.errors.CodeHint
import ca.uwaterloo.flix.util.collection.MultiMap

object CodeHinter {

  /**
    * Returns a collection of code quality hints for the given AST `root`.
    */
  def run(root: TypedAst.Root, sources: Set[String]): List[CodeHint] = {
    var hints: List[CodeHint] = Nil

    object HintConsumer extends Consumer {
      override def consumeTraitSymUse(symUse: SymUse.TraitSymUse): Unit = {
        hints = hints ++ considerTrait(root, symUse.sym, symUse.loc)
      }
      override def consumeTraitConstraintHead(tcHead: TraitConstraint.Head): Unit = {
        hints = hints ++ considerTrait(root, tcHead.sym, tcHead.loc)
      }
      override def consumeDefSymUse(sym: DefSymUse): Unit = {
        hints = hints ++ considerDef(root, sym.sym, sym.loc)
      }
      override def consumeType(tpe: Type): Unit = tpe match {
        case Type.Cst(TypeConstructor.Enum(sym, _), loc) =>
          hints = hints ++ considerEnum(root, sym, loc)
        case _ => ()
      }
      override def consumeCaseSymUse(sym: SymUse.CaseSymUse): Unit = {
        hints = hints ++ considerEnum(root, sym.sym.enumSym, sym.loc)
      }
    }

    Visitor.visitRoot(root, HintConsumer, AllAcceptor)

    hints.filter(include(_, sources))
  }

  private def considerTrait(root: Root, sym: Symbol.TraitSym, loc: SourceLocation): List[CodeHint] = {
    val trt = root.traits(sym)
    val ann = trt.ann
    checkDeprecated(ann, loc) ++ checkExperimental(ann, loc)
  }

  private def considerDef(root: Root, sym: Symbol.DefnSym, loc: SourceLocation): List[CodeHint] = {
    val defn = root.defs(sym)
    val ann = defn.spec.ann
    checkDeprecated(ann, loc) ++
      checkExperimental(ann, loc) ++
      checkLazy(ann, loc) ++
      checkLazyWhenPure(sym, loc) ++
      checkParallel(ann, loc)
  }

  private def considerEnum(root: Root, sym: Symbol.EnumSym, loc: SourceLocation): List[CodeHint] = {
    val enm = root.enums(sym)
    val ann = enm.ann
    checkDeprecated(ann, loc) ++ checkExperimental(ann, loc)
  }

  private def checkDeprecated(ann: Annotations, loc: SourceLocation): List[CodeHint] = {
    if (ann.isDeprecated) { CodeHint.Deprecated(loc) :: Nil } else { Nil }
  }

  private def checkExperimental(ann: Annotations, loc: SourceLocation): List[CodeHint] = {
    if (ann.isExperimental) { CodeHint.Experimental(loc) :: Nil } else { Nil }
  }

  private def checkLazy(ann: Annotations, loc: SourceLocation): List[CodeHint] = {
    if (ann.isLazy) { CodeHint.Lazy(loc) :: Nil } else { Nil }
  }

  private def checkParallel(ann: Annotations, loc: SourceLocation): List[CodeHint] = {
    if (ann.isParallel) { CodeHint.Parallel(loc) :: Nil } else { Nil }
  }

  /**
    * Checks whether `sym` would benefit from `tpe` being pure.
    */
  private def checkEffect(sym: Symbol.DefnSym, tpe: Type, loc: SourceLocation)(implicit root: Root): List[CodeHint] = {
    if (lazyWhenPure(sym)) {
      if (isPureFunction(tpe))
        CodeHint.LazyEvaluation(sym, loc) :: Nil
      else
        CodeHint.SuggestPurityForLazyEvaluation(sym, loc) :: Nil
    } else if (parallelWhenPure(sym)) {
      if (isPureFunction(tpe))
        CodeHint.ParallelEvaluation(sym, loc) :: Nil
      else
        CodeHint.SuggestPurityForParallelEvaluation(sym, loc) :: Nil
    } else {
      Nil
    }
  }

  /**
    * Returns `true` if the the given `sym` is marked being purity reflective
    * and uses lazy evaluation when given a pure function argument.
    */
  private def lazyWhenPure(sym: Symbol.DefnSym)(implicit root: Root): Boolean = {
    val defn = root.defs(sym)
    defn.spec.ann.isLazyWhenPure
  }

  /**
    * Returns `true` if the given `sym` is marked being purity reflective
    * and uses parallel evaluation when given a pure function argument.
    */
  private def parallelWhenPure(sym: Symbol.DefnSym)(implicit root: Root): Boolean = {
    val defn = root.defs(sym)
    defn.spec.ann.isParallelWhenPure
  }

  /**
    * Returns `true` if the given function type `tpe` is pure.
    */
  private def isPureFunction(tpe: Type): Boolean = tpe.typeConstructor match {
    case Some(TypeConstructor.Arrow(_)) => tpe.arrowEffectType == Type.Pure
    case _ => false
  }

  /**
    * Returns `true` if the given code `hint` should be included in the result.
    */
  private def include(hint: CodeHint, sources: Set[String]): Boolean =
    sources.contains(hint.loc.source.name)
}

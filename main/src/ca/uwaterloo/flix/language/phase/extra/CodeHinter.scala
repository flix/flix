/*
 * Copyright 2021 Magnus Madsen
 * Copyright 2024 Alexander Dybdahl Troelsen
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

import ca.uwaterloo.flix.api.lsp.acceptors.AllAcceptor
import ca.uwaterloo.flix.api.lsp.{Consumer, Visitor}
import ca.uwaterloo.flix.language.ast.TypedAst.*
import ca.uwaterloo.flix.language.ast.shared.SymUse.{DefSymUse, TraitSymUse}
import ca.uwaterloo.flix.language.ast.shared.{Annotations, SymUse, TraitConstraint}
import ca.uwaterloo.flix.language.ast.{SourceLocation, Symbol, Type, TypeConstructor, TypedAst}
import ca.uwaterloo.flix.language.errors.CodeHint

object CodeHinter {

  /**
    * Returns a collection of code quality hints for the given AST `root`.
    */
  def run(root: TypedAst.Root, sources: Set[String]): List[CodeHint] = {
    val uses = getUses(root)

    val traitHints = uses.traitUses.flatMap(considerTrait(root))
    val defHints = uses.defUses.flatMap(considerDef(root))
    val enumHints = uses.enumUses.flatMap{case (sym, loc) => considerEnum(root, sym, loc)}
    val defCalls = uses.defCalls.flatMap{case (sym, exps) => considerDefCall(root, sym, exps)}

    val hints = traitHints ++ defHints ++ enumHints ++ defCalls

    hints.filter(include(_, sources))
  }

  private def considerDefCall(root: Root, sym: Symbol.DefnSym, exps: List[Expr]): List[CodeHint] = {
    exps.flatMap(e => checkEffect(sym, e.tpe, e.loc)(root))
  }

  private case class Uses(traitUses: List[TraitSymUse], enumUses: List[(Symbol.EnumSym, SourceLocation)], defUses: List[DefSymUse], defCalls: List[(Symbol.DefnSym, List[Expr])])

  private def getUses(root: Root): Uses = {
    var traitUses: List[TraitSymUse] = Nil
    var defUses: List[DefSymUse] = Nil
    var enumUses: List[(Symbol.EnumSym, SourceLocation)] = Nil
    var defCalls: List[(Symbol.DefnSym, List[Expr])] = Nil

    object UseConsumer extends Consumer {
      override def consumeTraitSymUse(symUse: TraitSymUse): Unit = traitUses = symUse :: traitUses
      override def consumeTraitConstraintHead(tcHead: TraitConstraint.Head): Unit = traitUses = TraitSymUse(tcHead.sym, tcHead.loc) :: traitUses
      override def consumeDefSymUse(sym: DefSymUse): Unit = defUses = sym :: defUses
      override def consumeExpr(exp: Expr): Unit = exp match {
        case TypedAst.Expr.ApplyDef(symUse, exps, _, _, _, _) => defCalls = (symUse.sym, exps) :: defCalls
        case _ => ()
      }
      override def consumeType(tpe: Type): Unit = tpe match {
        case Type.Cst(TypeConstructor.Enum(sym, _), loc) => enumUses = (sym, loc) :: enumUses
        case _ => ()
      }
      override def consumeCaseSymUse(sym: SymUse.CaseSymUse): Unit = enumUses = (sym.sym.enumSym, sym.loc) :: enumUses
    }

    Visitor.visitRoot(root, UseConsumer, AllAcceptor)

    Uses(traitUses, enumUses, defUses, defCalls)
  }

  /**
    * Returns a collection of code quality hints related to a given trait's annotations.
    *
    * @param root The root AST node of the project.
    * @param sym  The [[Symbol.TraitSym]] for the trait in question.
    * @param loc  The [[SourceLocation]] for the occurrence of `sym`.
    * @return     A collection of code quality hints.
    */
  private def considerTrait(root: Root)(symUse: SymUse.TraitSymUse): List[CodeHint] = {
    val trt = root.traits(symUse.sym)
    val ann = trt.ann
    checkDeprecated(ann, symUse.loc) ++ checkExperimental(ann, symUse.loc)
  }

  /**
    * Returns a collection of code quality hints related to the given def's annotations.
    *
    * Note that hints related to `@LazyWhenPure` and `@ParallelWhenPure` are not included in this colleciton.
    *
    * @param root The root AST node for the Flix project.
    * @param sym  The [[Symbol.DefnSym]] for the Def in question.
    * @param loc  The [[SourceLocation]] for the occurrence of the `sym`.
    * @return     A collection of code quality hints
    */
  private def considerDef(root: Root)(symUse: SymUse.DefSymUse): List[CodeHint] = {
    val defn = root.defs(symUse.sym)
    val ann = defn.spec.ann
    checkDeprecated(ann, symUse.loc) ++
      checkExperimental(ann, symUse.loc) ++
      checkLazy(ann, symUse.loc) ++
      checkParallel(ann, symUse.loc)
  }

  /**
    * Returns a collection of code quality hints related to the given enum's annotations.
    *
    * @param root The root AST node for the Flix project.
    * @param sym  The [[Symbol.EnumSym]] for the enum in quesiton.
    * @param loc  The [[SourceLocation]] for the occurrence of `sym`.
    * @return     A collection of code hints.
    */
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
    val lazzy = if (lazyWhenPure(sym)) {
      if (isPureFunction(tpe))
        CodeHint.LazyEvaluation(sym, loc) :: Nil
      else
        CodeHint.SuggestPurityForLazyEvaluation(sym, loc) :: Nil
    } else {
      Nil
    }
    val parallel = if (parallelWhenPure(sym)) {
      if (isPureFunction(tpe))
        CodeHint.ParallelEvaluation(sym, loc) :: Nil
      else
        CodeHint.SuggestPurityForParallelEvaluation(sym, loc) :: Nil
    } else {
      Nil
    }

    lazzy ++ parallel
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

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
  def run(sources: Set[String])(implicit root: Root): List[CodeHint] = {
    val occurs = getOccurrences

    val applyDefHints = occurs.applyDefOccurs.flatMap{case (sym, exps) => visitApplyDef(sym, exps)}
    val defHints = occurs.defOccurs.flatMap(visitDefSymUse)
    val enumHints = occurs.enumOccurs.flatMap{case (sym, loc) => visitEnumSymUse(sym, loc)}
    val traitHints = occurs.traitOccurs.flatMap(visitTraitSymUse)

    val hints = applyDefHints ++ defHints ++ enumHints ++ traitHints

    hints.filter(include(_, sources))
  }

  /**
    * Returns a collection of code quality hints related to the given def application.
    *
    * @param sym  The [[Symbol.DefnSym]] for the function being applied.
    * @param exps The arguments the function is being applied to.
    * @param root The root AST node of the Flix project
    * @return     A collection of code quality hints.
    */
  private def visitApplyDef(sym: Symbol.DefnSym, exps: List[Expr])(implicit root: Root): List[CodeHint] = {
    exps.flatMap(e => checkEffect(sym, e.tpe, e.loc))
  }

  /**
    * Returns a collection of code quality hints related to the given def's annotations.
    *
    * Note that hints related to `@LazyWhenPure` and `@ParallelWhenPure` are not included in this colleciton.
    *
    * @param symUse The [[SymUse.DefSymUse]] for the occurrence of the def in question.
    * @param root   The root AST node for the Flix project.
    * @return       A collection of code quality hints
    */
  private def visitDefSymUse(symUse: SymUse.DefSymUse)(implicit root: Root): List[CodeHint] = {
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
  private def visitEnumSymUse(sym: Symbol.EnumSym, loc: SourceLocation)(implicit root: Root): List[CodeHint] = {
    val enm = root.enums(sym)
    val ann = enm.ann
    checkDeprecated(ann, loc) ++ checkExperimental(ann, loc)
  }

  /**
    * Returns a collection of code quality hints related to a given trait's annotations.
    *
    * @param symUse The [[SymUse.TraitSymUse]] for the occurrence of the trait in question.
    * @param root   The root AST node of the project.
    * @return       A collection of code quality hints.
    */
  private def visitTraitSymUse(symUse: SymUse.TraitSymUse)(implicit root: Root): List[CodeHint] = {
    val trt = root.traits(symUse.sym)
    val ann = trt.ann
    checkDeprecated(ann, symUse.loc) ++ checkExperimental(ann, symUse.loc)
  }

  /**
    * A [[Occurrences]] represents all elements that might warrant a code hint.
    *
    * @param applyDefOccurs All def applications.
    * @param defOccurs      All occurrences of defs.
    * @param enumOccurs     All occurrences of enums.
    * @param traitOccurs    All occurrences of traits.
    */
  private case class Occurrences(applyDefOccurs: List[(Symbol.DefnSym, List[Expr])], defOccurs: List[DefSymUse], enumOccurs: List[(Symbol.EnumSym, SourceLocation)], traitOccurs: List[TraitSymUse])

  /**
    * Returns a [[Occurrences]] for the Flix project.
    *
    * @param root The root AST node of the Flix project
    * @return     A [[Occurrences]] for the Flix project.
    */
  private def getOccurrences(implicit root: Root): Occurrences = {
    var applyDefOccurs: List[(Symbol.DefnSym, List[Expr])] = Nil
    var defOccurs: List[DefSymUse] = Nil
    var enumOccurs: List[(Symbol.EnumSym, SourceLocation)] = Nil
    var traitOccurs: List[TraitSymUse] = Nil

    object OccurConsumer extends Consumer {
      override def consumeCaseSymUse(sym: SymUse.CaseSymUse): Unit = enumOccurs = (sym.sym.enumSym, sym.loc) :: enumOccurs
      override def consumeDefSymUse(sym: DefSymUse): Unit = defOccurs = sym :: defOccurs
      override def consumeExpr(exp: Expr): Unit = exp match {
        case TypedAst.Expr.ApplyDef(symUse, exps, _, _, _, _) => applyDefOccurs = (symUse.sym, exps) :: applyDefOccurs
        case _ => ()
      }
      override def consumeTraitConstraintHead(tcHead: TraitConstraint.Head): Unit = traitOccurs = TraitSymUse(tcHead.sym, tcHead.loc) :: traitOccurs
      override def consumeTraitSymUse(symUse: TraitSymUse): Unit = traitOccurs = symUse :: traitOccurs
      override def consumeType(tpe: Type): Unit = tpe match {
        case Type.Cst(TypeConstructor.Enum(sym, _), loc) => enumOccurs = (sym, loc) :: enumOccurs
        case _ => ()
      }
    }

    Visitor.visitRoot(root, OccurConsumer, AllAcceptor)

    Occurrences(applyDefOccurs, defOccurs, enumOccurs, traitOccurs)
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

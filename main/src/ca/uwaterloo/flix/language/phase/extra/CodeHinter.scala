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
import ca.uwaterloo.flix.language.ast.TypedAst.Predicate.{Body, Head}
import ca.uwaterloo.flix.language.ast.TypedAst._
import ca.uwaterloo.flix.language.ast.{Ast, SourceLocation, Symbol, Type, TypeConstructor, TypedAst}
import ca.uwaterloo.flix.language.errors.CodeHint

object CodeHinter {

  /**
    * Returns a collection of code quality hints for the given AST `root`.
    */
  def run(root: TypedAst.Root, sources: Set[String])(implicit flix: Flix): Set[CodeHint] = {
    val defsHints = root.defs.values.flatMap(visitDef(_)(root)).toSet
    defsHints.filter(include(_, sources))
  }

  /**
    * Returns `true` if the given code `hint` should be included in the result.
    */
  private def include(hint: CodeHint, sources: Set[String]): Boolean =
    sources.contains(hint.loc.source.name)

  /**
    * Computes code quality hints for the given definition `def0`.
    */
  private def visitDef(def0: TypedAst.Def)(implicit root: Root): List[CodeHint] = {
    visitExp(def0.impl.exp)
  }

  /**
    * Computes code quality hints for the given expression `exp0`.
    */
  private def visitExp(exp0: Expression)(implicit root: Root): List[CodeHint] = exp0 match {
    case Expression.Wild(tpe, _) =>
      visitType(tpe)

    case Expression.Var(_, _, _) => Nil

    case Expression.Def(sym, tpe, loc) =>
      checkDeprecated(sym, loc) ++
        checkExperimental(sym, loc) ++
        checkParallel(sym, loc) ++
        checkUnsafe(sym, loc) ++
        checkLazy(sym, loc) ++
        visitSpec(root.defs(sym).spec) ++
        visitType(tpe)

    case Expression.Sig(sym, tpe, _) =>
      visitSpec(root.sigs(sym).spec) ++
        visitType(tpe)


    case Expression.Hole(_, tpe, _) =>
      visitType(tpe)

    case Expression.Unit(_) => Nil

    case Expression.Null(_, _) => Nil

    case Expression.True(_) => Nil

    case Expression.False(_) => Nil

    case Expression.Char(_, _) => Nil

    case Expression.Float32(_, _) => Nil

    case Expression.Float64(_, _) => Nil

    case Expression.Int8(_, _) => Nil

    case Expression.Int16(_, _) => Nil

    case Expression.Int32(_, _) => Nil

    case Expression.Int64(_, _) => Nil

    case Expression.BigInt(_, _) => Nil

    case Expression.Str(_, _) => Nil

    case Expression.Default(tpe, loc) =>
      visitTypeWithHintLocation(tpe, loc)

    case Expression.Lambda(fparam, exp, tpe, loc) =>
      visitFormalParameter(fparam) ++
        checkEffect(exp.eff, exp.loc) ++
        visitExp(exp) ++
        visitType(tpe)

    case Expression.Apply(exp, exps, tpe, eff, loc) =>
      val hints0 = (exp, exps) match {
        case (Expression.Def(sym, _, _), lambda :: _) =>
          checkPurity(sym, lambda.tpe, loc)
        case _ => Nil
      }
      val hints1 = checkEffect(eff, loc)
      hints0 ++ hints1 ++ visitExp(exp) ++ visitExps(exps) ++ visitType(tpe)

    case Expression.Unary(_, exp, _, _, _) =>
      visitExp(exp)

    case Expression.Binary(_, exp1, exp2, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2)

    case Expression.Let(_, _, exp1, exp2, tpe, eff, loc) =>
      checkEffect(eff, loc) ++ visitExp(exp1) ++ visitExp(exp2) ++ visitType(tpe)

    case Expression.LetRec(_, _, exp1, exp2, tpe, eff, loc) =>
      visitExp(exp1) ++ visitExp(exp2) ++ visitType(tpe)

    case Expression.Scope(_, exp, _, _, _) =>
      visitExp(exp)

    case Expression.IfThenElse(exp1, exp2, exp3, tpe, _, loc) =>
      visitExp(exp1) ++ visitExp(exp2) ++ visitExp(exp3) ++ visitType(tpe)

    case Expression.Stm(exp1, exp2, tpe, eff, loc) =>
      checkEffect(eff, loc) ++ visitExp(exp1) ++ visitExp(exp2) ++ visitType(tpe)

    case Expression.Match(matchExp, rules, _, _, _) =>
      visitExp(matchExp) ++ rules.flatMap {
        case MatchRule(pattern, guard, exp) =>
          visitTypeWithHintLocation(pattern.tpe, pattern.loc) ++ visitExp(guard) ++ visitExp(exp)
      }

    case Expression.Choose(exps, rules, _, _, _) =>
      visitExps(exps) ++ rules.flatMap {
        case ChoiceRule(_, exp) => visitExp(exp)
      }

    case Expression.Tag(sym, _, exp, _, _, loc) =>
      checkEnum(root.enums(sym), loc) ++ visitExp(exp)

    case Expression.Tuple(exps, tpe, _, loc) =>
      visitTypeWithHintLocation(tpe, loc) ++ visitExps(exps)

    case Expression.RecordEmpty(_, _) => Nil

    case Expression.RecordSelect(exp, _, _, _, _) =>
      visitExp(exp)

    case Expression.RecordExtend(_, exp1, exp2, _, _, _) =>
      visitExp(exp2) ++ visitExp(exp1)

    case Expression.RecordRestrict(_, exp, _, _, _) =>
      visitExp(exp)

    case Expression.ArrayLit(exps, _, _, _) =>
      visitExps(exps)

    case Expression.ArrayNew(exp1, exp2, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2)

    case Expression.ArrayLoad(exp1, exp2, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2)

    case Expression.ArrayStore(exp1, exp2, exp3, _) =>
      visitExp(exp1) ++ visitExp(exp2) ++ visitExp(exp3)

    case Expression.ArrayLength(exp, _, _) =>
      visitExp(exp)

    case Expression.ArraySlice(exp1, exp2, exp3, _, _) =>
      visitExp(exp1) ++ visitExp(exp2) ++ visitExp(exp3)

    case Expression.Ref(exp1, exp2,_, _, _) =>
      visitExp(exp1) ++ visitExp(exp2)

    case Expression.Deref(exp, _, _, _) =>
      visitExp(exp)

    case Expression.Assign(exp1, exp2, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2)

    case Expression.Ascribe(exp, tpe, _, _) =>
      visitExp(exp) ++ visitType(tpe)

    case Expression.Cast(exp, _, _, tpe, eff, loc) =>
      checkCast(tpe, eff, loc) ++ visitExp(exp) ++ visitType(tpe)

    case Expression.TryCatch(exp, rules, _, _, _) =>
      visitExp(exp) ++ rules.flatMap {
        case CatchRule(_, _, exp) => visitExp(exp)
      }

    case Expression.InvokeConstructor(_, args, _, _, _) =>
      visitExps(args)

    case Expression.InvokeMethod(_, exp, args, _, _, _) =>
      visitExp(exp) ++ visitExps(args)

    case Expression.InvokeStaticMethod(_, args, _, _, _) =>
      visitExps(args)

    case Expression.GetField(_, exp, _, _, _) =>
      visitExp(exp)

    case Expression.PutField(_, exp1, exp2, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2)

    case Expression.GetStaticField(_, _, _, _) =>
      Nil

    case Expression.PutStaticField(_, exp, _, _, _) =>
      visitExp(exp)

    case Expression.NewChannel(exp, _, _, _) =>
      visitExp(exp)

    case Expression.GetChannel(exp, _, _, _) =>
      visitExp(exp)

    case Expression.PutChannel(exp1, exp2, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2)

    case Expression.SelectChannel(rules, default, _, _, _) =>
      rules.flatMap {
        case SelectChannelRule(_, chan, exp) => visitExp(chan) ++ visitExp(exp)
      } ++ default.map(visitExp).getOrElse(Nil)

    case Expression.Spawn(exp, _, _, _) =>
      visitExp(exp)

    case Expression.Lazy(exp, _, _) =>
      visitExp(exp)

    case Expression.Force(exp, _, _, _) =>
      visitExp(exp)

    case Expression.FixpointConstraintSet(cs, _, _, _) =>
      cs.flatMap(visitConstraint)

    case Expression.FixpointMerge(exp1, exp2, _, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2)

    case Expression.FixpointSolve(exp, _, _, _, _) =>
      visitExp(exp)

    case Expression.FixpointFilter(_, exp, _, _, _) =>
      visitExp(exp)

    case Expression.FixpointProjectIn(exp, _, _, _, _) =>
      visitExp(exp)

    case Expression.FixpointProjectOut(_, exp, _, _, _) =>
      visitExp(exp)

    case Expression.Reify(_, _, _, _) =>
      Nil

    case Expression.ReifyType(_, _, _, _, _) =>
      Nil

    case Expression.ReifyEff(_, exp1, exp2, exp3, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2) ++ visitExp(exp3)
  }

  /**
    * Computes code quality hints for the given list of expressions `exps`.
    */
  private def visitExps(exps: List[Expression])(implicit root: Root): List[CodeHint] =
    exps.flatMap(visitExp)

  /**
    * Computes code quality hints for the given constraint `c`.
    */
  private def visitConstraint(c: Constraint)(implicit root: Root): List[CodeHint] =
    visitHeadPredicate(c.head) ++ c.body.flatMap(visitBodyPredicate)

  /**
    * Computes code quality hints for the given head predicate `p`.
    */
  private def visitHeadPredicate(p: TypedAst.Predicate.Head)(implicit root: Root): List[CodeHint] = p match {
    case Head.Atom(_, _, terms, _, _) => visitExps(terms)
  }

  /**
    * Computes code quality hints for the given body predicate `p`.
    */
  private def visitBodyPredicate(p: TypedAst.Predicate.Body)(implicit root: Root): List[CodeHint] = p match {
    case Body.Atom(_, _, _, _, _, _, _) => Nil
    case Body.Guard(exp, _) => visitExp(exp)
    case Body.Loop(_, exp, _) => visitExp(exp)
  }

  /**
    * Computes code quality hints for the given spec `spec`.
    */
  private def visitSpec(spec: Spec)(implicit root: Root): List[CodeHint] =
    spec.fparams.flatMap(visitFormalParameter(_)) ++ visitType(spec.retTpe)

  /**
    * Compute code quality hints for the given formal parameter `fparam`.
    */
  private def visitFormalParameter(fparam: FormalParam)(implicit root: Root): List[CodeHint] =
    visitType(fparam.tpe)

  /**
    * Compute code quality hints for a type `tpe` reported at location `loc`.
    */
  private def visitTypeWithHintLocation(tpe: Type, loc: SourceLocation)(implicit root: Root):  List[CodeHint] = {
    val typeArgumentsHints = tpe.typeArguments.flatMap(tpe => visitTypeWithHintLocation(tpe, tpe.loc))
    val typeHints = tpe match {
      case Type.Cst(TypeConstructor.KindedEnum(sym, _), _) => checkEnum(root.enums(sym), loc)
      case _ => Nil
    }
    typeHints ++ typeArgumentsHints
  }
  private def visitType(tpe: Type)(implicit root: Root):  List[CodeHint] = visitTypeWithHintLocation(tpe, tpe.loc)

  /**
    * Checks wether the given enum `enum` is experimental and/or deprecated.
    */
  private def checkEnum(enum: TypedAst.Enum, loc: SourceLocation)(implicit root: Root): List[CodeHint] = {
    val isDeprecated = enum.ann.exists(ann => ann.name.isInstanceOf[Ast.Annotation.Deprecated])
    val deprecated = if (isDeprecated) CodeHint.Deprecated(loc) :: Nil else Nil
    val isExperimental = enum.ann.exists(ann => ann.name.isInstanceOf[Ast.Annotation.Experimental])
    val experimental = if (isExperimental) CodeHint.Experimental(loc) :: Nil else Nil
    deprecated ++ experimental
  }

  /**
    * Checks whether `sym` would benefit from `tpe` being pure.
    */
  private def checkPurity(sym: Symbol.DefnSym, tpe: Type, loc: SourceLocation)(implicit root: Root): List[CodeHint] = {
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
    defn.spec.ann.exists(ann => ann.name.isInstanceOf[Ast.Annotation.LazyWhenPure])
  }

  /**
    * Returns `true` if the given `sym` is marked being purity reflective
    * and uses parallel evaluation when given a pure function argument.
    */
  private def parallelWhenPure(sym: Symbol.DefnSym)(implicit root: Root): Boolean = {
    val defn = root.defs(sym)
    defn.spec.ann.exists(ann => ann.name.isInstanceOf[Ast.Annotation.ParallelWhenPure])
  }

  /**
    * Checks whether `tpe` is a non-trivial effect.
    *
    * NB: Not currently checked for every expression.
    */
  private def checkEffect(tpe: Type, loc: SourceLocation): List[CodeHint] = {
    if (nonTrivialEffect(tpe)) {
      CodeHint.NonTrivialEffect(tpe, loc) :: Nil
    } else {
      Nil
    }
  }

  /**
    * Checks whether the given definition symbol `sym` is deprecated.
    */
  private def checkDeprecated(sym: Symbol.DefnSym, loc: SourceLocation)(implicit root: Root): List[CodeHint] = {
    val defn = root.defs(sym)
    val isDeprecated = defn.spec.ann.exists(ann => ann.name.isInstanceOf[Ast.Annotation.Deprecated])
    if (isDeprecated) {
      CodeHint.Deprecated(loc) :: Nil
    } else {
      Nil
    }
  }

  /**
    * Checks whether the given definition symbol `sym` is experimental.
    */
  private def checkExperimental(sym: Symbol.DefnSym, loc: SourceLocation)(implicit root: Root): List[CodeHint] = {
    val defn = root.defs(sym)
    val isExperimental = defn.spec.ann.exists(ann => ann.name.isInstanceOf[Ast.Annotation.Experimental])
    if (isExperimental) {
      CodeHint.Experimental(loc) :: Nil
    } else {
      Nil
    }
  }

  /**
    * Checks whether the given definition symbol `sym` is unsafe.
    */
  private def checkParallel(sym: Symbol.DefnSym, loc: SourceLocation)(implicit root: Root): List[CodeHint] = {
    val defn = root.defs(sym)
    val isParallel = defn.spec.ann.exists(ann => ann.name.isInstanceOf[Ast.Annotation.Parallel])
    if (isParallel) {
      CodeHint.Parallel(loc) :: Nil
    } else {
      Nil
    }
  }

  /**
    * Checks whether the given definition symbol `sym` is unsafe.
    */
  private def checkUnsafe(sym: Symbol.DefnSym, loc: SourceLocation)(implicit root: Root): List[CodeHint] = {
    val defn = root.defs(sym)
    val isUnsafe = defn.spec.ann.exists(ann => ann.name.isInstanceOf[Ast.Annotation.Unsafe])
    if (isUnsafe) {
      CodeHint.Unsafe(loc) :: Nil
    } else {
      Nil
    }
  }

  /**
    * Checks whether the given definition symbol `sym` is lazy.
    */
  private def checkLazy(sym: Symbol.DefnSym, loc: SourceLocation)(implicit root: Root): List[CodeHint] = {
    val defn = root.defs(sym)
    val isLazy = defn.spec.ann.exists(ann => ann.name.isInstanceOf[Ast.Annotation.Lazy])
    if (isLazy) {
      CodeHint.Lazy(loc) :: Nil
    } else {
      Nil
    }
  }

  /**
    * Checks whether a cast to the given `tpe` and `eff` is an unsafe purity cast.
    */
  private def checkCast(tpe: Type, eff: Type, loc: SourceLocation): List[CodeHint] = {
    eff.typeConstructor match {
      case Some(TypeConstructor.True) => CodeHint.UnsafePurityCast(eff.loc) :: Nil
      case _ => Nil
    }
  }

  /**
    * Returns `true` if the given function type `tpe` is pure.
    */
  private def isPureFunction(tpe: Type): Boolean = tpe.arrowEffectType == Type.Pure

  /**
    * Returns `true` if the given effect `tpe` is non-trivial.
    */
  private def nonTrivialEffect(tpe: Type): Boolean = Type.size(tpe) > 5

}

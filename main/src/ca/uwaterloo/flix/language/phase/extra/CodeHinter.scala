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
import ca.uwaterloo.flix.api.lsp.Index
import ca.uwaterloo.flix.language.ast.TypedAst.Predicate.{Body, Head}
import ca.uwaterloo.flix.language.ast.TypedAst._
import ca.uwaterloo.flix.language.ast.{Ast, SourceLocation, Symbol, Type, TypeConstructor, TypedAst}
import ca.uwaterloo.flix.language.errors.CodeHint
import ca.uwaterloo.flix.language.phase.unification.BoolTable

object CodeHinter {

  /**
    * Returns a collection of code quality hints for the given AST `root`.
    */
  def run(root: TypedAst.Root, sources: Set[String])(implicit flix: Flix, index: Index): List[CodeHint] = {
    val classHints = root.classes.values.flatMap(visitClass(_)(root, index)).toList
    val defsHints = root.defs.values.flatMap(visitDef(_)(root, flix)).toList
    val enumsHints = root.enums.values.flatMap(visitEnum(_)(root, index)).toList
    (classHints ++ defsHints ++ enumsHints).filter(include(_, sources))
  }

  /**
    * Returns `true` if the given code `hint` should be included in the result.
    */
  private def include(hint: CodeHint, sources: Set[String]): Boolean =
    sources.contains(hint.loc.source.name)

  /**
    * Computes code quality hints for the given enum `enum`.
    */
  private def visitEnum(enum0: TypedAst.Enum)(implicit root: Root, index: Index): List[CodeHint] = {
    val tagUses = enum0.cases.keys.flatMap(tag => index.usesOf(enum0.sym, tag))
    val enumUses = index.usesOf(enum0.sym)
    val uses = enumUses ++ tagUses
    val isDeprecated = enum0.ann.exists(ann => ann.name.isInstanceOf[Ast.Annotation.Deprecated])
    val isExperimental = enum0.ann.exists(ann => ann.name.isInstanceOf[Ast.Annotation.Experimental])
    val deprecated = if (isDeprecated) uses.map(CodeHint.Deprecated) else Nil
    val experimental = if (isExperimental) uses.map(CodeHint.Experimental) else Nil
    (deprecated ++ experimental).toList
  }

  /**
    * Computes code quality hints for the given class `typeclass`.
    */
  private def visitClass(typeclass: TypedAst.Class)(implicit root: Root, index: Index): List[CodeHint] = {
    val uses = index.usesOf(typeclass.sym)
    val isDeprecated = typeclass.ann.exists(ann => ann.name.isInstanceOf[Ast.Annotation.Deprecated])
    val deprecated = if (isDeprecated) uses.map(CodeHint.Deprecated) else Nil
    val isExperimental = typeclass.ann.exists(ann => ann.name.isInstanceOf[Ast.Annotation.Experimental])
    val experimental = if (isExperimental) uses.map(CodeHint.Experimental) else Nil
    (deprecated ++ experimental).toList
  }

  /**
    * Computes code quality hints for the given definition `def0`.
    */
  private def visitDef(def0: TypedAst.Def)(implicit root: Root, flix: Flix): List[CodeHint] = {
    visitExp(def0.impl.exp)
  }

  /**
    * Computes code quality hints for the given expression `exp0`.
    */
  private def visitExp(exp0: Expression)(implicit root: Root, flix: Flix): List[CodeHint] = exp0 match {
    case Expression.Wild(_, _) => Nil

    case Expression.Var(_, _, _) => Nil

    case Expression.Def(sym, _, loc) =>
      checkDeprecated(sym, loc) ++
        checkExperimental(sym, loc) ++
        checkParallel(sym, loc) ++
        checkUnsafe(sym, loc) ++
        checkLazy(sym, loc)

    case Expression.Sig(_, _, _) => Nil

    case Expression.Hole(_, _, _) => Nil

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

    case Expression.Default(_, _) => Nil

    case Expression.Lambda(_, exp, _, _) =>
      checkPurity(exp.pur, exp.loc) ++ visitExp(exp)

    case Expression.Apply(exp, exps, _, pur, _, loc) =>
      val hints0 = (exp, exps) match {
        case (Expression.Def(sym, _, _), lambda :: _) =>
          checkPurity(sym, lambda.tpe, loc)
        case _ => Nil
      }
      val hints1 = checkPurity(pur, loc)
      hints0 ++ hints1 ++ visitExp(exp) ++ visitExps(exps)

    case Expression.Unary(_, exp, _, _, _, _) =>
      visitExp(exp)

    case Expression.Binary(_, exp1, exp2, _, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2)

    case Expression.Let(_, _, exp1, exp2, _, pur, eff, loc) =>
      checkPurity(pur, loc) ++ visitExp(exp1) ++ visitExp(exp2)

    case Expression.LetRec(_, _, exp1, exp2, _, _, _, loc) =>
      visitExp(exp1) ++ visitExp(exp2)

    case Expression.Region(_, _) => Nil

    case Expression.Scope(_, _, exp, _, _, _, _) =>
      visitExp(exp)

    case Expression.IfThenElse(exp1, exp2, exp3, _, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2) ++ visitExp(exp3)

    case Expression.Stm(exp1, exp2, _, pur, eff, loc) =>
      checkPurity(pur, loc) ++ visitExp(exp1) ++ visitExp(exp2)

    case Expression.Discard(exp, _, _, _) =>
      visitExp(exp)

    case Expression.Match(matchExp, rules, _, _, _, _) =>
      visitExp(matchExp) ++ rules.flatMap {
        case MatchRule(_, guard, exp) => visitExp(guard) ++ visitExp(exp)
      }

    case Expression.Choose(exps, rules, _, _, _, _) =>
      visitExps(exps) ++ rules.flatMap {
        case ChoiceRule(_, exp) => visitExp(exp)
      }

    case Expression.Tag(_, _, exp, _, _, _, _) =>
      visitExp(exp)

    case Expression.Tuple(exps, _, _, _, _) =>
      visitExps(exps)

    case Expression.RecordEmpty(_, _) => Nil

    case Expression.RecordSelect(exp, _, _, _, _, _) =>
      visitExp(exp)

    case Expression.RecordExtend(_, exp1, exp2, _, _, _, _) =>
      visitExp(exp2) ++ visitExp(exp1)

    case Expression.RecordRestrict(_, exp, _, _, _, _) =>
      visitExp(exp)

    case Expression.ArrayLit(exps, exp, _, _, _, _) =>
      visitExps(exps) ++ visitExp(exp)

    case Expression.ArrayNew(exp1, exp2, exp3, _, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2) ++ visitExp(exp3)

    case Expression.ArrayLoad(exp1, exp2, _, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2)

    case Expression.ArrayStore(exp1, exp2, exp3, _, _) =>
      visitExp(exp1) ++ visitExp(exp2) ++ visitExp(exp3)

    case Expression.ArrayLength(exp, _, _, _) =>
      visitExp(exp)

    case Expression.ArraySlice(exp1, exp2, exp3, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2) ++ visitExp(exp3)

    case Expression.Ref(exp1, exp2, _, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2)

    case Expression.Deref(exp, _, _, _, _) =>
      visitExp(exp)

    case Expression.Assign(exp1, exp2, _, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2)

    case Expression.Ascribe(exp, _, _, _, _) =>
      visitExp(exp)

    case Expression.Cast(exp, _, _, _, tpe, pur, _, loc) =>
      checkCast(tpe, pur, loc) ++ visitExp(exp)

    case Expression.Without(exp, _, _, _, _, _) =>
      visitExp(exp)

    case Expression.TryCatch(exp, rules, _, _, _, _) =>
      visitExp(exp) ++ rules.flatMap {
        case CatchRule(_, _, exp) => visitExp(exp)
      }

    case Expression.TryWith(exp, _, rules, _, _, _, _) =>
      visitExp(exp) ++ rules.flatMap {
        case HandlerRule(_, _, e) => visitExp(e)
      }

    case Expression.Do(_, exps, _, _, _) =>
      exps.flatMap(visitExp)

    case Expression.Resume(exp, _, _) =>
      visitExp(exp)

    case Expression.InvokeConstructor(_, args, _, _, _, _) =>
      visitExps(args)

    case Expression.InvokeMethod(_, exp, args, _, _, _, _) =>
      visitExp(exp) ++ visitExps(args)

    case Expression.InvokeStaticMethod(_, args, _, _, _, _) =>
      visitExps(args)

    case Expression.GetField(_, exp, _, _, _, _) =>
      visitExp(exp)

    case Expression.PutField(_, exp1, exp2, _, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2)

    case Expression.GetStaticField(_, _, _, _, _) =>
      Nil

    case Expression.PutStaticField(_, exp, _, _, _, _) =>
      visitExp(exp)

    case Expression.NewObject(_, _, _, _, methods, _) =>
      methods.flatMap {
        case JvmMethod(_, _, exp, _, _, _, _) => visitExp(exp)
      }

    case Expression.NewChannel(exp, _, _, _, _) =>
      visitExp(exp)

    case Expression.GetChannel(exp, _, _, _, _) =>
      visitExp(exp)

    case Expression.PutChannel(exp1, exp2, _, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2)

    case Expression.SelectChannel(rules, default, _, _, _, _) =>
      rules.flatMap {
        case SelectChannelRule(_, chan, exp) => visitExp(chan) ++ visitExp(exp)
      } ++ default.map(visitExp).getOrElse(Nil)

    case Expression.Spawn(exp, _, _, _, _) =>
      visitExp(exp)

    case Expression.Lazy(exp, _, _) =>
      visitExp(exp)

    case Expression.Force(exp, _, _, _, _) =>
      visitExp(exp)

    case Expression.FixpointConstraintSet(cs, _, _, _) =>
      cs.flatMap(visitConstraint)

    case Expression.FixpointLambda(_, exp, _, _, _, _, _) =>
      visitExp(exp)

    case Expression.FixpointMerge(exp1, exp2, _, _, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2)

    case Expression.FixpointSolve(exp, _, _, _, _, _) =>
      visitExp(exp)

    case Expression.FixpointFilter(_, exp, _, _, _, _) =>
      visitExp(exp)

    case Expression.FixpointInject(exp, _, _, _, _, _) =>
      visitExp(exp)

    case Expression.FixpointProject(_, exp, _, _, _, _) =>
      visitExp(exp)

    case Expression.Reify(_, _, _, _, _) =>
      Nil

    case Expression.ReifyType(_, _, _, _, _, _) =>
      Nil

    case Expression.ReifyEff(_, exp1, exp2, exp3, _, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2) ++ visitExp(exp3)
  }

  /**
    * Computes code quality hints for the given list of expressions `exps`.
    */
  private def visitExps(exps: List[Expression])(implicit root: Root, flix: Flix): List[CodeHint] =
    exps.flatMap(visitExp)

  /**
    * Computes code quality hints for the given constraint `c`.
    */
  private def visitConstraint(c: Constraint)(implicit root: Root, flix: Flix): List[CodeHint] =
    visitHeadPredicate(c.head) ++ c.body.flatMap(visitBodyPredicate)

  /**
    * Computes code quality hints for the given head predicate `p`.
    */
  private def visitHeadPredicate(p: TypedAst.Predicate.Head)(implicit root: Root, flix: Flix): List[CodeHint] = p match {
    case Head.Atom(_, _, terms, _, _) => visitExps(terms)
  }

  /**
    * Computes code quality hints for the given body predicate `p`.
    */
  private def visitBodyPredicate(p: TypedAst.Predicate.Body)(implicit root: Root, flix: Flix): List[CodeHint] = p match {
    case Body.Atom(_, _, _, _, _, _, _) => Nil
    case Body.Guard(exp, _) => visitExp(exp)
    case Body.Loop(_, exp, _) => visitExp(exp)
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
  private def checkPurity(tpe: Type, loc: SourceLocation)(implicit flix: Flix): List[CodeHint] = {
    if (numberOfVarOccurs(tpe) < 5) {
      // Case 1: Formula is small. Good.
      Nil
    } else {
      // Case 2: Formula is big. Try to minimize it.
      val minType = BoolTable.minimizeType(tpe)
      if (numberOfVarOccurs(minType) < 5) {
        // Case 2.1: Formula is small. Good.
        Nil
      } else {
        // Case 2.2: Formula is still big. Report a code hint.
        CodeHint.NonTrivialEffect(minType, loc) :: Nil
      }
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
  private def isPureFunction(tpe: Type): Boolean = tpe.arrowPurityType == Type.Pure

  /**
    * Returns the total number of variable *occurrences* in the given type `tpe`.
    */
  private def numberOfVarOccurs(tpe: Type): Int = tpe match {
    case Type.KindedVar(_, _) => 1
    case Type.UnkindedVar(_, _) => 1
    case Type.Ascribe(tpe, _, _) => numberOfVarOccurs(tpe)
    case Type.Cst(_, _) => 0
    case Type.Apply(tpe1, tpe2, _) => numberOfVarOccurs(tpe1) + numberOfVarOccurs(tpe2)
    case Type.Alias(_, _, tpe, _) => numberOfVarOccurs(tpe)
    case Type.UnkindedArrow(_, _, _) => 0
    case Type.ReadWrite(_, _) => 0
  }

}

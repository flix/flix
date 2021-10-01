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
import ca.uwaterloo.flix.language.ast.{SourceLocation, Symbol, Type, TypeConstructor, TypedAst}
import ca.uwaterloo.flix.language.errors.CodeHint
import ca.uwaterloo.flix.util.Validation
import ca.uwaterloo.flix.util.Validation._

object CodeHinter {

  /**
    * A list of operations that support lazy evaluation when given a pure function.
    */
  private val LazyWhenPure: List[Symbol.DefnSym] = List(
    Symbol.mkDefnSym("LazyList.filter"),
    Symbol.mkDefnSym("LazyList.filterMap"),
    Symbol.mkDefnSym("LazyList.map"),
    Symbol.mkDefnSym("LazyList.flatMap"),
    Symbol.mkDefnSym("LazyList.mapWithIndex"),
    Symbol.mkDefnSym("LazyList.dropWhile"),
    Symbol.mkDefnSym("LazyList.takeWhile"),
    Symbol.mkDefnSym("Stream.filter"),
    Symbol.mkDefnSym("Stream.filterMap"),
    Symbol.mkDefnSym("Stream.map"),
    Symbol.mkDefnSym("Stream.flatMap"),
    Symbol.mkDefnSym("Stream.mapWithIndex"),
    Symbol.mkDefnSym("Stream.dropWhile"),
    Symbol.mkDefnSym("Stream.takeWhile")
  )

  /**
    * A list of operations that support parallel evaluation when given a pure function.
    */
  private val ParallelWhenPure: List[Symbol.DefnSym] = List(
    Symbol.mkDefnSym("Set.count"),
    Symbol.mkDefnSym("Set.exists"),
    Symbol.mkDefnSym("Set.forall"),
    Symbol.mkDefnSym("Set.maximumBy"),
    Symbol.mkDefnSym("Set.minimumBy"),
  )

  /**
    * Returns a collection of code quality hints for the given AST `root`.
    */
  def run(root: TypedAst.Root)(implicit flix: Flix): Validation[TypedAst.Root, CodeHint] = flix.phase("CodeQuality") {
    val hints = root.defs.values.flatMap(visitDef).toList
    if (hints.isEmpty)
      root.toSuccess
    else
      Failure(hints.to(LazyList))
  }

  /**
    * Computes code quality hints for the given definition `def0`.
    */
  private def visitDef(def0: TypedAst.Def): List[CodeHint] = {
    visitExp(def0.impl.exp)
  }

  /**
    * Computes code quality hints for the given expression `exp0`.
    */
  private def visitExp(exp0: Expression): List[CodeHint] = exp0 match {
    case Expression.Wild(_, _) => Nil

    case Expression.Var(_, _, _) => Nil

    case Expression.Def(_, _, _) => Nil

    case Expression.Sig(_, _, _) => Nil

    case Expression.Hole(_, _, _, _) => Nil

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
      visitExp(exp)

    case Expression.Apply(exp, exps, _, eff, loc) =>
      val hints0 = (exp, exps) match {
        case (Expression.Def(sym, _, _), lambda :: _) => checkPurity(sym, lambda.tpe, loc)
        case _ => Nil
      }
      hints0 ++ visitExp(exp) ++ visitExps(exps)

    case Expression.Unary(_, exp, _, _, _) =>
      visitExp(exp)

    case Expression.Binary(_, exp1, exp2, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2)

    case Expression.Let(_, _, exp1, exp2, _, eff, loc) =>
      checkEffect(eff, loc) ++ visitExp(exp1) ++ visitExp(exp2)

    case Expression.LetRegion(_, exp, _, _, _) =>
      visitExp(exp)

    case Expression.IfThenElse(exp1, exp2, exp3, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2) ++ visitExp(exp3)

    case Expression.Stm(exp1, exp2, _, eff, loc) =>
      checkEffect(eff, loc) ++ visitExp(exp1) ++ visitExp(exp2)

    case Expression.Match(matchExp, rules, _, _, _) =>
      visitExp(matchExp) ++ rules.flatMap {
        case MatchRule(_, guard, exp) => visitExp(guard) ++ visitExp(exp)
      }

    case Expression.Choose(exps, rules, _, _, _) =>
      visitExps(exps) ++ rules.flatMap {
        case ChoiceRule(_, exp) => visitExp(exp)
      }

    case Expression.Tag(_, _, exp, _, _, _) =>
      visitExp(exp)

    case Expression.Tuple(exps, _, _, _) =>
      visitExps(exps)

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

    case Expression.Ref(exp, _, _, _) =>
      visitExp(exp)

    case Expression.Deref(exp, _, _, _) =>
      visitExp(exp)

    case Expression.Assign(exp1, exp2, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2)

    case Expression.Existential(_, exp, _) =>
      visitExp(exp)

    case Expression.Universal(_, exp, _) =>
      visitExp(exp)

    case Expression.Ascribe(exp, _, _, _) =>
      visitExp(exp)

    case Expression.Cast(exp, _, _, _) =>
      visitExp(exp)

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
  }

  /**
    * Computes code quality hints for the given constraint `c`.
    */
  private def visitConstraint(c: Constraint): List[CodeHint] =
    visitHeadPredicate(c.head) ++ c.body.flatMap(visitBodyPredicate)

  /**
    * Computes code quality hints for the given head predicate `p`.
    */
  private def visitHeadPredicate(p: TypedAst.Predicate.Head): List[CodeHint] = p match {
    case Head.Atom(_, _, terms, _, _) => visitExps(terms)
  }

  /**
    * Computes code quality hints for the given body predicate `p`.
    */
  private def visitBodyPredicate(p: TypedAst.Predicate.Body): List[CodeHint] = p match {
    case Body.Atom(_, _, _, _, _, _) => Nil
    case Body.Guard(exp, _) => visitExp(exp)
  }

  /**
    * Computes code quality hints for the given list of expressions `exps`.
    */
  private def visitExps(exps: List[Expression]): List[CodeHint] =
    exps.flatMap(visitExp)

  /**
    * Checks whether `sym` would benefit from `tpe` being pure.
    */
  private def checkPurity(sym: Symbol.DefnSym, tpe: Type, loc: SourceLocation): List[CodeHint] = {
    if (LazyWhenPure.contains(sym) && nonPureFunction(tpe)) {
      CodeHint.LazyWhenPure(sym, loc) :: Nil
    } else if (ParallelWhenPure.contains(sym) && nonPureFunction(tpe)) {
      CodeHint.ParallelWhenPure(sym, loc) :: Nil
    } else {
      Nil
    }
  }

  /**
    * Checks whether `tpe` is a non-trivial effect.
    *
    * NB: Not currently checked for every expression.
    */
  private def checkEffect(tpe: Type, loc: SourceLocation): List[CodeHint] = {
    if (nonTrivialEffect(tpe)) {
      CodeHint.NonTrivialEffect(loc) :: Nil
    } else {
      Nil
    }
  }

  /**
    * Returns `true` if the given function type `tpe` is non-pure (impure or polymorphic).
    */
  private def nonPureFunction(tpe: Type): Boolean = tpe.arrowEffectType != Type.Pure

  /**
    * Returns `true` if the given effect `tpe` is non-trivial.
    */
  private def nonTrivialEffect(tpe: Type): Boolean = tpe match {
    case Type.KindedVar(_, _, _, _, _) => false
    case Type.Cst(TypeConstructor.True, _) => false
    case Type.Cst(TypeConstructor.False, _) => false
    case _ => true
  }

}

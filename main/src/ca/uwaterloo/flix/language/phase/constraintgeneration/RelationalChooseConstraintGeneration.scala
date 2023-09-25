/*
 * Copyright 2020-2023 Matthew Lutze, Magnus Madsen
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
package ca.uwaterloo.flix.language.phase.constraintgeneration

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast._
import ca.uwaterloo.flix.language.phase.ConstraintGeneration.{Context, unifyAllTypesM, unifyBoolM, unifyTypeM, visitExp}
import ca.uwaterloo.flix.language.phase.unification.ChoiceMatch

object RelationalChooseConstraintGeneration {
  def visitRelationalChoose(exp: KindedAst.Expr.RelationalChoose)(implicit c: Context, root: KindedAst.Root, flix: Flix): (Type, Type) = exp match {
    case KindedAst.Expr.RelationalChoose(star, exps0, rules0, tvar, loc) =>


      /**
        * Performs type inference on the given match expressions `exps` and nullity `vars`.
        *
        * Returns a pair of lists of the types and purects of the match expressions.
        */
      def visitMatchExps(exps: List[KindedAst.Expr], isAbsentVars: List[Type.Var], isPresentVars: List[Type.Var]): (List[Type], List[Type]) = {
        def visitMatchExp(exp: KindedAst.Expr, isAbsentVar: Type.Var, isPresentVar: Type.Var): (Type, Type) = {
          val freshElmVar = Type.freshVar(Kind.Star, loc)
          val (tpe, eff) = visitExp(exp)
          unifyTypeM(tpe, Type.mkChoice(freshElmVar, isAbsentVar, isPresentVar, loc), loc)
          val resTpe = freshElmVar
          val resEff = eff
          (resTpe, resEff)

        }

        exps.zip(isAbsentVars.zip(isPresentVars)).map {
          case (matchExp, (isAbsentVar, isPresentVar)) => visitMatchExp(matchExp, isAbsentVar, isPresentVar)
        }.unzip
      }

      /**
        * Performs type inference of the given null rules `rs`.
        *
        * Returns a pair of list of the types and purects of the rule expressions.
        */
      def visitRuleBodies(rs: List[KindedAst.RelationalChooseRule]): (List[Type], List[Type]) = {
        def visitRuleBody(r: KindedAst.RelationalChooseRule): (Type, Type) = r match {
          case KindedAst.RelationalChooseRule(_, exp0) => visitExp(exp0)
        }

        rs.map(visitRuleBody).unzip
      }

      /**
        * Returns a transformed result type that encodes the Boolean constraint of each row pattern in the result type.
        *
        * NB: Requires that the `ts` types are Choice-types.
        */
      def transformResultTypes(isAbsentVars: List[Type.Var], isPresentVars: List[Type.Var], rs: List[KindedAst.RelationalChooseRule], ts: List[Type], loc: SourceLocation): Type = {
        def visitRuleBody(r: KindedAst.RelationalChooseRule, resultType: Type): (Type, Type, Type) = r match {
          case KindedAst.RelationalChooseRule(r, exp0) =>
            val cond = mkOverApprox(isAbsentVars, isPresentVars, r)
            val innerType = Type.freshVar(Kind.Star, exp0.loc)
            val isAbsentVar = Type.freshVar(Kind.Bool, exp0.loc)
            val isPresentVar = Type.freshVar(Kind.Bool, exp0.loc)
            unifyTypeM(resultType, Type.mkChoice(innerType, isAbsentVar, isPresentVar, loc), loc)
            (Type.mkAnd(cond, isAbsentVar, loc), Type.mkAnd(cond, isPresentVar, loc), innerType)
        }

        ///
        /// Simply compute the mgu of the `ts` types if this is not a star relational_choose.
        ///
        if (!star) {
          return unifyAllTypesM(ts, Kind.Star, loc)
        }

        ///
        /// Otherwise construct a new Choice type with isAbsent and isPresent conditions that depend on each pattern row.
        ///
        val (isAbsentConds, isPresentConds, innerTypes) = rs.zip(ts).map(p => visitRuleBody(p._1, p._2)).unzip3
        val isAbsentCond = Type.mkOr(isAbsentConds, loc)
        val isPresentCond = Type.mkOr(isPresentConds, loc)
        val innerType = unifyAllTypesM(innerTypes, Kind.Star, loc)
        val resultType = Type.mkChoice(innerType, isAbsentCond, isPresentCond, loc)
        resultType
      }

      /**
        * Constructs a Boolean constraint for the given choice rule `r` which is an under-approximation.
        *
        * If a pattern is a wildcard it *must* always match.
        * If a pattern is `Absent`  its corresponding `isPresentVar` must be `false` (i.e. to prevent the value from being `Present`).
        * If a pattern is `Present` its corresponding `isAbsentVar`  must be `false` (i.e. to prevent the value from being `Absent`).
        */
      def mkUnderApprox(isAbsentVars: List[Type.Var], isPresentVars: List[Type.Var], r: List[KindedAst.RelationalChoosePattern]): Type =
        isAbsentVars.zip(isPresentVars).zip(r).foldLeft(Type.True) {
          case (acc, (_, KindedAst.RelationalChoosePattern.Wild(_))) =>
            // Case 1: No constraint is generated for a wildcard.
            acc
          case (acc, ((isAbsentVar, _), KindedAst.RelationalChoosePattern.Present(_, _, _))) =>
            // Case 2: A `Present` pattern forces the `isAbsentVar` to be equal to `false`.
            Type.mkAnd(acc, Type.mkEquiv(isAbsentVar, Type.False, loc), loc)
          case (acc, ((_, isPresentVar), KindedAst.RelationalChoosePattern.Absent(_))) =>
            // Case 3: An `Absent` pattern forces the `isPresentVar` to be equal to `false`.
            Type.mkAnd(acc, Type.mkEquiv(isPresentVar, Type.False, loc), loc)
        }

      /**
        * Constructs a Boolean constraint for the given choice rule `r` which is an over-approximation.
        *
        * If a pattern is a wildcard it *may* always match.
        * If a pattern is `Absent` it *may* match if its corresponding `isAbsent` is `true`.
        * If a pattern is `Present` it *may* match if its corresponding `isPresentVar`is `true`.
        */
      def mkOverApprox(isAbsentVars: List[Type.Var], isPresentVars: List[Type.Var], r: List[KindedAst.RelationalChoosePattern]): Type =
        isAbsentVars.zip(isPresentVars).zip(r).foldLeft(Type.True) {
          case (acc, (_, KindedAst.RelationalChoosePattern.Wild(_))) =>
            // Case 1: No constraint is generated for a wildcard.
            acc
          case (acc, ((isAbsentVar, _), KindedAst.RelationalChoosePattern.Absent(_))) =>
            // Case 2: An `Absent` pattern may match if the `isAbsentVar` is `true`.
            Type.mkAnd(acc, isAbsentVar, loc)
          case (acc, ((_, isPresentVar), KindedAst.RelationalChoosePattern.Present(_, _, _))) =>
            // Case 3: A `Present` pattern may match if the `isPresentVar` is `true`.
            Type.mkAnd(acc, isPresentVar, loc)
        }

      /**
        * Constructs a disjunction of the constraints of each choice rule.
        */
      def mkOuterDisj(m: List[List[KindedAst.RelationalChoosePattern]], isAbsentVars: List[Type.Var], isPresentVars: List[Type.Var]): Type =
        m.foldLeft(Type.False) {
          case (acc, rule) => Type.mkOr(acc, mkUnderApprox(isAbsentVars, isPresentVars, rule), loc)
        }

      /**
        * Performs type inference and unification with the `matchTypes` against the given choice rules `rs`.
        */
      def unifyMatchTypesAndRules(matchTypes: List[Type], rs: List[KindedAst.RelationalChooseRule]): List[List[Type]] = {
        def unifyWithRule(r: KindedAst.RelationalChooseRule): List[Type] = {
          matchTypes.zip(r.pat).map {
            case (matchType, KindedAst.RelationalChoosePattern.Wild(_)) =>
              // Case 1: The pattern is wildcard. No variable is bound and there is type to constrain.
              matchType
            case (matchType, KindedAst.RelationalChoosePattern.Absent(_)) =>
              // Case 2: The pattern is a `Absent`. No variable is bound and there is type to constrain.
              matchType
            case (matchType, KindedAst.RelationalChoosePattern.Present(sym, tvar, loc)) =>
              // Case 3: The pattern is `Present`. Must constraint the type of the local variable with the type of the match expression.
              unifyTypeM(matchType, sym.tvar, loc)
              unifyTypeM(sym.tvar, tvar, loc)
              matchType
          }
        }

        rs.map(unifyWithRule)
      }

      //
      // Introduce an isAbsent variable for each match expression in `exps`.
      //
      val isAbsentVars = exps0.map(exp0 => Type.freshVar(Kind.Bool, exp0.loc))

      //
      // Introduce an isPresent variable for each math expression in `exps`.
      //
      val isPresentVars = exps0.map(exp0 => Type.freshVar(Kind.Bool, exp0.loc))

      //
      // Extract the choice pattern match matrix.
      //
      val matrix = rules0.map(_.pat)

      //
      // Compute the saturated pattern match matrix..
      //
      val saturated = ChoiceMatch.saturate(matrix)

      //
      // Build the Boolean formula.
      //
      val formula = mkOuterDisj(saturated, isAbsentVars, isPresentVars)

      //
      // Put everything together.
      //
      unifyBoolM(formula, Type.True, loc)
      val (matchTyp, matchEff) = visitMatchExps(exps0, isAbsentVars, isPresentVars)
      unifyMatchTypesAndRules(matchTyp, rules0)
      val (ruleBodyTyp, ruleBodyEff) = visitRuleBodies(rules0)
      val resultTypes = transformResultTypes(isAbsentVars, isPresentVars, rules0, ruleBodyTyp, loc)
      unifyTypeM(tvar, resultTypes, loc)
      val resTpe = tvar
      val resEff = Type.mkUnion(matchEff ::: ruleBodyEff, loc)
      (resTpe, resEff)
  }
}

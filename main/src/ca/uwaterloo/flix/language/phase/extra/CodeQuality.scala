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
import ca.uwaterloo.flix.language.CompilationError
import ca.uwaterloo.flix.language.ast.TypedAst.Predicate.{Body, Head}
import ca.uwaterloo.flix.language.ast.TypedAst.{CatchRule, ChoiceRule, Constraint, Expression, MatchRule, SelectChannelRule}
import ca.uwaterloo.flix.language.ast.{SourceLocation, Symbol, Type, TypedAst}
import ca.uwaterloo.flix.language.errors.Severity
import ca.uwaterloo.flix.util.Validation
import ca.uwaterloo.flix.util.Validation._
import ca.uwaterloo.flix.util.vt.VirtualString.{Code, Line, NewLine}
import ca.uwaterloo.flix.util.vt.VirtualTerminal

object CodeQuality {

  /**
    * A list of operations that supports fusion or laziness when given pure function arguments.
    */
  private val WantsPureArg = List(
    Symbol.mkDefnSym("Stream.filter"),
    Symbol.mkDefnSym("Stream.map"),
    Symbol.mkDefnSym("Stream.flatMap")
  )

  // TODO: DOC
  sealed trait CodeQualityError extends CompilationError {
    def kind: String = "CodeQuality Hint"

    override def severity: Severity = Severity.Hint
  }

  // TODO: DOC
  case class UsePureFunction(sym: Symbol.DefnSym, loc: SourceLocation) extends CodeQualityError {
    override def summary: String = s"Use of impure function prevents laziness / fusion."

    override def message: VirtualTerminal = {
      val vt = new VirtualTerminal()
      vt << Line(kind, source.format) << NewLine // TODO
      vt << ">> TODO" << NewLine
      vt << NewLine
      vt << Code(loc, "TODO") << NewLine // TODO
    }
  }

  /**
    * Returns a collection of code quality hints for the given AST `root`.
    */
  def run(root: TypedAst.Root)(implicit flix: Flix): Validation[TypedAst.Root, CodeQualityError] = flix.phase("CodeQuality") {
    val results = root.defs.values.flatMap(visitDef).toList

    if (results.isEmpty)
      root.toSuccess
    else
      Failure(results.to(LazyList))
  }

  /**
    * Computes code quality hints for the given definition `def0`.
    */
  private def visitDef(def0: TypedAst.Def): List[CodeQualityError] = {
    visitExp(def0.impl.exp)
  }

  /**
    * Computes code quality hints for the given expression `exp0`.
    */
  private def visitExp(exp0: Expression): List[CodeQualityError] = exp0 match {
    case Expression.Wild(_, _) => Nil

    case Expression.Var(_, _, _) => Nil

    case Expression.Def(_, _, loc) => Nil

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

    case Expression.Apply(exp, exps, _, _, _) =>
      val hints0 = (exp, exps) match {
        case (Expression.Def(sym, _, _), lambda :: _) =>
          if (WantsPureArg.contains(sym) && !isPure(lambda.tpe))
            UsePureFunction(sym, sym.loc) :: Nil
          else
            Nil
        case _ => Nil
      }

      visitExp(exp) ++ visitExps(exps) ++ hints0

    case Expression.Unary(_, exp, _, _, _) =>
      visitExp(exp)

    case Expression.Binary(_, exp1, exp2, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2)

    case Expression.Let(_, _, exp1, exp2, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2)

    case Expression.LetRegion(_, exp, _, _, _) =>
      visitExp(exp)

    case Expression.IfThenElse(exp1, exp2, exp3, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2) ++ visitExp(exp3)

    case Expression.Stm(exp1, exp2, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2)

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
  private def visitConstraint(c: Constraint): List[CodeQualityError] =
    visitHeadPredicate(c.head) ++ c.body.flatMap(visitBodyPredicate)

  /**
    * Computes code quality hints for the given head predicate `p`.
    */
  private def visitHeadPredicate(p: TypedAst.Predicate.Head): List[CodeQualityError] = p match {
    case Head.Atom(_, _, terms, _, _) => visitExps(terms)
  }

  /**
    * Computes code quality hints for the given body predicate `p`.
    */
  private def visitBodyPredicate(p: TypedAst.Predicate.Body): List[CodeQualityError] = p match {
    case Body.Atom(_, _, _, _, _, _) => Nil
    case Body.Guard(exp, _) => visitExp(exp)
  }

  /**
    * Computes code quality hints for the given list of expressions `exps`.
    */
  private def visitExps(exps: List[Expression]): List[CodeQualityError] =
    exps.flatMap(visitExp)

  /**
    * Returns `true` if the given type `tpe` is a pure function type.
    */
  private def isPure(tpe: Type): Boolean = tpe.arrowEffectType == Type.Pure

}

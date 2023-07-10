/*
 * Copyright 2021 Matthew Lutze
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
package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.TypedAst.Predicate.{Body, Head}
import ca.uwaterloo.flix.language.ast.TypedAst._
import ca.uwaterloo.flix.language.ast.ops.TypedAstOps
import ca.uwaterloo.flix.util.Validation
import ca.uwaterloo.flix.util.Validation.ToSuccess

/**
  * Collects statistics about the AST and reports them to stdout.
  */
object Statistics {
  def run(root: Root)(implicit flix: Flix): Validation[Root, Nothing] = flix.phase("Statistics") {
    // Return early if stats have not been enabled.
    if (!flix.options.xstatistics) {
      return root.toSuccess
    }

    val defCounts = Counter.merge(root.defs.values.map(visitDef))
    val sigCounts = Counter.merge(root.sigs.values.map(visitSig))
    val instDefCounts = Counter.merge(TypedAstOps.instanceDefsOf(root).map(visitDef))

    val fullCounts = defCounts ++ sigCounts ++ instDefCounts
    printStats(fullCounts)

    root.toSuccess
  }

  /**
    * Prints the list of AST nodes from most to least common.
    */
  private def printStats(counter: Counter): Unit = {
    val ordered = counter.m.toList.sortBy(_._2).reverse
    ordered.foreach {
      case (name, count) => println(s"$name,$count")
    }
  }

  /**
    * Counts AST nodes in the given def.
    */
  private def visitDef(defn: Def): Counter = defn match {
    case Def(sym, spec, impl) => visitImpl(impl)
  }

  /**
    * Counts AST nodes in the given sig.
    */
  private def visitSig(sig: Sig): Counter = sig match {
    case Sig(sym, spec, impl) => Counter.merge(impl.map(visitImpl))
  }

  /**
    * Counts AST nodes in the given impl.
    */
  private def visitImpl(impl: Impl): Counter = impl match {
    case Impl(exp, inferredScheme) => visitExp(exp)
  }

  /**
    * Counts AST nodes in the given expression.
    */
  private def visitExp(exp0: Expr): Counter = {
    val base = Counter.of(getName(exp0))

    val subExprs = exp0 match {
      case Expr.Cst(_, _, _) => Counter.empty
      case Expr.Var(sym, tpe, loc) => Counter.empty
      case Expr.Def(sym, tpe, loc) => Counter.empty
      case Expr.Sig(sym, tpe, loc) => Counter.empty
      case Expr.Hole(sym, tpe, loc) => Counter.empty
      case Expr.HoleWithExp(exp, tpe, eff, loc) => visitExp(exp)
      case Expr.OpenAs(_, exp, _, _) => visitExp(exp)
      case Expr.Use(_, _, exp, _) => visitExp(exp)
      case Expr.Lambda(fparam, exp, tpe, loc) => visitExp(exp)
      case Expr.Apply(exp, exps, tpe, eff, loc) => visitExp(exp) ++ Counter.merge(exps.map(visitExp))
      case Expr.Unary(sop, exp, tpe, eff, loc) => visitExp(exp)
      case Expr.Binary(sop, exp1, exp2, tpe, eff, loc) => visitExp(exp1) ++ visitExp(exp2)
      case Expr.Let(sym, mod, exp1, exp2, tpe, eff, loc) => visitExp(exp1) ++ visitExp(exp2)
      case Expr.LetRec(sym, mod, exp1, exp2, tpe, eff, loc) => visitExp(exp1) ++ visitExp(exp2)
      case Expr.Region(tpe, loc) => Counter.empty
      case Expr.Scope(sym, regionVar, exp, tpe, eff, loc) => visitExp(exp)
      case Expr.ScopeExit(exp1, exp2, tpe, eff, loc) => visitExp(exp1) ++ visitExp(exp2)
      case Expr.IfThenElse(exp1, exp2, exp3, tpe, eff, loc) => visitExp(exp1) ++ visitExp(exp2) ++ visitExp(exp3)
      case Expr.Stm(exp1, exp2, tpe, eff, loc) => visitExp(exp1) ++ visitExp(exp2)
      case Expr.Discard(exp, eff, loc) => visitExp(exp)
      case Expr.Match(exp, rules, tpe, eff, loc) => visitExp(exp) ++ Counter.merge(rules.map(visitMatchRule))
      case Expr.TypeMatch(exp, rules, tpe, eff, loc) => visitExp(exp) ++ Counter.merge(rules.map(visitMatchTypeRule))
      case Expr.RelationalChoose(exps, rules, tpe, eff, loc) => Counter.merge(exps.map(visitExp)) ++ Counter.merge(rules.map(visitRelationalChooseRule))
      case Expr.RestrictableChoose(star, exp, rules, tpe, eff, loc) => visitExp(exp) ++ Counter.merge(rules.map(visitRestrictableChooseRule))
      case Expr.Tag(sym, exp, tpe, eff, loc) => visitExp(exp)
      case Expr.RestrictableTag(sym, exp, tpe, eff, loc) => visitExp(exp)
      case Expr.Tuple(elms, tpe, eff, loc) => Counter.merge(elms.map(visitExp))
      case Expr.RecordEmpty(tpe, loc) => Counter.empty
      case Expr.RecordSelect(exp, field, tpe, eff, loc) => visitExp(exp)
      case Expr.RecordExtend(field, value, rest, tpe, eff, loc) => visitExp(value) ++ visitExp(rest)
      case Expr.RecordRestrict(field, rest, tpe, eff, loc) => visitExp(rest)
      case Expr.ArrayLit(exps, exp, tpe, eff, loc) => Counter.merge(exps.map(visitExp)) ++ visitExp(exp)
      case Expr.ArrayNew(exp1, exp2, exp3, tpe, eff, loc) => visitExp(exp1) ++ visitExp(exp2) ++ visitExp(exp3)
      case Expr.ArrayLoad(base, index, tpe, eff, loc) => visitExp(base) ++ visitExp(index)
      case Expr.ArrayLength(base, eff, loc) => visitExp(base)
      case Expr.ArrayStore(base, index, elm, _, _) => visitExp(base) ++ visitExp(index) ++ visitExp(elm)
      case Expr.VectorLit(exps, _, _, _) => Counter.merge(exps.map(visitExp))
      case Expr.VectorLoad(exp1, exp2, _, _, _) => visitExp(exp1) ++ visitExp(exp2)
      case Expr.VectorLength(exp, _) => visitExp(exp)
      case Expr.Ref(exp1, exp2, tpe, eff, loc) => visitExp(exp1) ++ visitExp(exp2)
      case Expr.Deref(exp, tpe, eff, loc) => visitExp(exp)
      case Expr.Assign(exp1, exp2, tpe, eff, loc) => visitExp(exp1) ++ visitExp(exp2)
      case Expr.Ascribe(exp, tpe, eff, loc) => visitExp(exp)
      case Expr.InstanceOf(exp, _, _) => visitExp(exp)
      case Expr.CheckedCast(_, exp, _, _, _) => visitExp(exp)
      case Expr.UncheckedCast(exp, _, _, tpe, eff, loc) => visitExp(exp)
      case Expr.UncheckedMaskingCast(exp, tpe, eff, loc) => visitExp(exp)
      case Expr.Without(exp, _, _, _, _) => visitExp(exp)
      case Expr.TryCatch(exp, rules, tpe, eff, loc) => visitExp(exp) ++ Counter.merge(rules.map(visitCatchRule))
      case Expr.TryWith(exp, sym, rules, tpe, eff, loc) => visitExp(exp) ++ Counter.merge(rules.map(visitHandlerRule))
      case Expr.Do(sym, exps, tpe, eff, loc) => Counter.merge(exps.map(visitExp))
      case Expr.Resume(exp, tpe, loc) => visitExp(exp)
      case Expr.InvokeConstructor(constructor, args, tpe, eff, loc) => Counter.merge(args.map(visitExp))
      case Expr.InvokeMethod(method, exp, args, tpe, eff, loc) => visitExp(exp) ++ Counter.merge(args.map(visitExp))
      case Expr.InvokeStaticMethod(method, args, tpe, eff, loc) => Counter.merge(args.map(visitExp))
      case Expr.GetField(field, exp, tpe, eff, loc) => visitExp(exp)
      case Expr.PutField(field, exp1, exp2, tpe, eff, loc) => visitExp(exp1) ++ visitExp(exp2)
      case Expr.GetStaticField(field, tpe, eff, loc) => Counter.empty
      case Expr.PutStaticField(field, exp, tpe, eff, loc) => visitExp(exp)
      case Expr.NewObject(name, clazz, tpe, eff, methods, loc) => Counter.merge(methods.map(visitJvmMethod))
      case Expr.NewChannel(exp1, exp2, tpe, eff, loc) => visitExp(exp1) ++ visitExp(exp2)
      case Expr.GetChannel(exp, tpe, eff, loc) => visitExp(exp)
      case Expr.PutChannel(exp1, exp2, tpe, eff, loc) => visitExp(exp1) ++ visitExp(exp2)
      case Expr.SelectChannel(rules, default, tpe, eff, loc) => Counter.merge(rules.map(visitSelectChannelRule)) ++ Counter.merge(default.map(visitExp))
      case Expr.Spawn(exp1, exp2, tpe, eff, loc) => visitExp(exp1) ++ visitExp(exp2)
      case Expr.ParYield(frags, exp, tpe, eff, loc) => Counter.merge(frags.map(f => visitExp(f.exp))) ++ visitExp(exp)
      case Expr.Lazy(exp, tpe, loc) => visitExp(exp)
      case Expr.Force(exp, tpe, eff, loc) => visitExp(exp)
      case Expr.FixpointConstraintSet(cs, stf, tpe, loc) => Counter.merge(cs.map(visitConstraint))
      case Expr.FixpointLambda(pparams, exp, stf, tpe, eff, loc) => visitExp(exp)
      case Expr.FixpointMerge(exp1, exp2, stf, tpe, eff, loc) => visitExp(exp1) ++ visitExp(exp2)
      case Expr.FixpointSolve(exp, stf, tpe, eff, loc) => visitExp(exp)
      case Expr.FixpointFilter(pred, exp, tpe, eff, loc) => visitExp(exp)
      case Expr.FixpointInject(exp, pred, tpe, eff, loc) => visitExp(exp)
      case Expr.FixpointProject(pred, exp, tpe, eff, loc) => visitExp(exp)
      case Expr.Error(_, _, _) => Counter.empty
    }

    base ++ subExprs
  }

  /**
    * Counts AST nodes in the given rule.
    */
  private def visitMatchRule(rule: MatchRule): Counter = rule match {
    case MatchRule(pat, guard, exp) => Counter.merge(guard.map(visitExp)) ++ visitExp(exp)
  }

  /**
    * Counts AST nodes in the given rule.
    */
  private def visitMatchTypeRule(rule: TypeMatchRule): Counter = rule match {
    case TypeMatchRule(_, _, exp) => visitExp(exp)
  }

  /**
    * Counts AST nodes in the given rule.
    */
  private def visitRelationalChooseRule(rule: RelationalChooseRule): Counter = rule match {
    case RelationalChooseRule(pat, exp) => visitExp(exp)
  }

  /**
    * Counts AST nodes in the given rule.
    */
  private def visitRestrictableChooseRule(rule: RestrictableChooseRule): Counter = rule match {
    case RestrictableChooseRule(_, exp) => visitExp(exp)
  }


  /**
    * Counts AST nodes in the given rule.
    */
  private def visitCatchRule(rule: CatchRule): Counter = rule match {
    case CatchRule(sym, clazz, exp) => visitExp(exp)
  }

  /**
    * Counts AST nodes in the given rule.
    */
  private def visitHandlerRule(rule: HandlerRule): Counter = rule match {
    case HandlerRule(op, fparams, exp) => visitExp(exp)
  }

  /**
    * Counts AST nodes in the given rule.
    */
  private def visitSelectChannelRule(rule: SelectChannelRule): Counter = rule match {
    case SelectChannelRule(sym, chan, exp) => visitExp(chan) ++ visitExp(exp)
  }

  /**
    * Counts AST nodes in the given constraint.
    */
  private def visitConstraint(constr: Constraint): Counter = constr match {
    case Constraint(cparams, head, body, loc) => visitHeadPredicate(head) ++ Counter.merge(body.map(visitBodyPredicate))
  }

  /**
    * Counts AST nodes in the given predicate.
    */
  private def visitHeadPredicate(head: Predicate.Head): Counter = head match {
    case Head.Atom(pred, den, terms, tpe, loc) => Counter.merge(terms.map(visitExp))
  }

  /**
    * Counts AST nodes in the given predicate.
    */
  private def visitBodyPredicate(body: Predicate.Body): Counter = body match {
    case Body.Atom(_, _, _, _, _, _, _) => Counter.empty
    case Body.Functional(_, exp, _) => visitExp(exp)
    case Body.Guard(exp, _) => visitExp(exp)
  }

  /**
    * Counts AST nodes in the given JVMMethod
    */
  private def visitJvmMethod(method: JvmMethod): Counter = method match {
    case JvmMethod(_, _, exp, _, _, _) => visitExp(exp)
  }

  /**
    * Returns the name of the given expression.
    */
  private def getName(expression: Expr): String = expression.productPrefix

  /**
    * Maintains a count of the
    */
  private case class Counter(m: Map[String, Int]) {
    /**
      * Merges the two counters.
      */
    def ++(other: Counter): Counter = {
      val m1 = other.m.foldLeft(m) {
        case (acc, (k, v)) => acc.updated(k, acc.getOrElse(k, 0) + v)
      }
      Counter(m1)
    }
  }

  private object Counter {
    /**
      * The empty counter.
      */
    val empty: Counter = Counter(Map.empty)

    /**
      * Starts a counter with 1 of the given name.
      */
    def of(name: String): Counter = Counter(Map(name -> 1))

    /**
      * Merges an interable of counters.
      */
    def merge(counters: Iterable[Counter]): Counter = counters.foldLeft(Counter.empty)(_ ++ _)
  }

}

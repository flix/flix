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
  private def visitExp(exp0: Expression): Counter = {
    val base = Counter.of(getName(exp0))

    val subExprs = exp0 match {
      case Expression.Unit(loc) => Counter.empty
      case Expression.Null(tpe, loc) => Counter.empty
      case Expression.True(loc) => Counter.empty
      case Expression.False(loc) => Counter.empty
      case Expression.Char(lit, loc) => Counter.empty
      case Expression.Float32(lit, loc) => Counter.empty
      case Expression.Float64(lit, loc) => Counter.empty
      case Expression.Int8(lit, loc) => Counter.empty
      case Expression.Int16(lit, loc) => Counter.empty
      case Expression.Int32(lit, loc) => Counter.empty
      case Expression.Int64(lit, loc) => Counter.empty
      case Expression.BigInt(lit, loc) => Counter.empty
      case Expression.Str(lit, loc) => Counter.empty
      case Expression.Default(tpe, loc) => Counter.empty
      case Expression.Wild(tpe, loc) => Counter.empty
      case Expression.Var(sym, tpe, loc) => Counter.empty
      case Expression.Def(sym, tpe, loc) => Counter.empty
      case Expression.Sig(sym, tpe, loc) => Counter.empty
      case Expression.Hole(sym, tpe, loc) => Counter.empty
      case Expression.Lambda(fparam, exp, tpe, loc) => visitExp(exp)
      case Expression.Apply(exp, exps, tpe, pur, eff, loc) => visitExp(exp) ++ Counter.merge(exps.map(visitExp))
      case Expression.Unary(sop, exp, tpe, pur, eff, loc) => visitExp(exp)
      case Expression.Binary(sop, exp1, exp2, tpe, pur, eff, loc) => visitExp(exp1) ++ visitExp(exp2)
      case Expression.Let(sym, mod, exp1, exp2, tpe, pur, eff, loc) => visitExp(exp1) ++ visitExp(exp2)
      case Expression.LetRec(sym, mod, exp1, exp2, tpe, pur, eff, loc) => visitExp(exp1) ++ visitExp(exp2)
      case Expression.Region(tpe, loc) => Counter.empty
      case Expression.Scope(sym, regionVar, exp, tpe, pur, eff, loc) => visitExp(exp)
      case Expression.IfThenElse(exp1, exp2, exp3, tpe, pur, eff, loc) => visitExp(exp1) ++ visitExp(exp2) ++ visitExp(exp3)
      case Expression.Stm(exp1, exp2, tpe, pur, eff, loc) => visitExp(exp1) ++ visitExp(exp2)
      case Expression.Discard(exp, pur, eff, loc) => visitExp(exp)
      case Expression.Match(exp, rules, tpe, pur, eff, loc) => visitExp(exp) ++ Counter.merge(rules.map(visitMatchRule))
      case Expression.Choose(exps, rules, tpe, pur, eff, loc) => Counter.merge(exps.map(visitExp)) ++ Counter.merge(rules.map(visitChoiceRule))
      case Expression.Tag(sym, tag, exp, tpe, pur, eff, loc) => visitExp(exp)
      case Expression.Tuple(elms, tpe, pur, eff, loc) => Counter.merge(elms.map(visitExp))
      case Expression.RecordEmpty(tpe, loc) => Counter.empty
      case Expression.RecordSelect(exp, field, tpe, pur, eff, loc) => visitExp(exp)
      case Expression.RecordExtend(field, value, rest, tpe, pur, eff, loc) => visitExp(value) ++ visitExp(rest)
      case Expression.RecordRestrict(field, rest, tpe, pur, eff, loc) => visitExp(rest)
      case Expression.ArrayLit(exps, exp, tpe, pur, eff, loc) => Counter.merge(exps.map(visitExp)) ++ visitExp(exp)
      case Expression.ArrayNew(exp1, exp2, exp3, tpe, pur, eff, loc) => visitExp(exp1) ++ visitExp(exp2) ++ visitExp(exp3)
      case Expression.ArrayLoad(base, index, tpe, pur, eff, loc) => visitExp(base) ++ visitExp(index)
      case Expression.ArrayLength(base, pur, eff, loc) => visitExp(base)
      case Expression.ArrayStore(base, index, elm, eff, loc) => visitExp(base) ++ visitExp(index) ++ visitExp(elm)
      case Expression.ArraySlice(base, beginIndex, endIndex, tpe, eff, loc) => visitExp(base) ++ visitExp(beginIndex) ++ visitExp(endIndex)
      case Expression.Ref(exp1, exp2, tpe, pur, eff, loc) => visitExp(exp1) ++ visitExp(exp2)
      case Expression.Deref(exp, tpe, pur, eff, loc) => visitExp(exp)
      case Expression.Assign(exp1, exp2, tpe, pur, eff, loc) => visitExp(exp1) ++ visitExp(exp2)
      case Expression.Ascribe(exp, tpe, pur, eff, loc) => visitExp(exp)
      case Expression.Cast(exp, _, _, _, tpe, pur, eff, loc) => visitExp(exp)
      case Expression.Without(exp, _, _, _, _, _) => visitExp(exp)
      case Expression.TryCatch(exp, rules, tpe, pur, eff, loc) => visitExp(exp) ++ Counter.merge(rules.map(visitCatchRule))
      case Expression.TryWith(exp, sym, rules, tpe, pur, eff, loc) => visitExp(exp) ++ Counter.merge(rules.map(visitHandlerRule))
      case Expression.Do(sym, exps, pur, eff, loc) => Counter.merge(exps.map(visitExp))
      case Expression.Resume(exp, tpe, loc) => visitExp(exp)
      case Expression.InvokeConstructor(constructor, args, tpe, pur, eff, loc) => Counter.merge(args.map(visitExp))
      case Expression.InvokeMethod(method, exp, args, tpe, pur, eff, loc) => visitExp(exp) ++ Counter.merge(args.map(visitExp))
      case Expression.InvokeStaticMethod(method, args, tpe, pur, eff, loc) => Counter.merge(args.map(visitExp))
      case Expression.GetField(field, exp, tpe, pur, eff, loc) => visitExp(exp)
      case Expression.PutField(field, exp1, exp2, tpe, pur, eff, loc) => visitExp(exp1) ++ visitExp(exp2)
      case Expression.GetStaticField(field, tpe, pur, eff, loc) => Counter.empty
      case Expression.PutStaticField(field, exp, tpe, pur, eff, loc) => visitExp(exp)
      case Expression.NewObject(clazz, tpe, pur, eff, methods, loc) => Counter.merge(methods.map(visitJvmMethod))
      case Expression.NewChannel(exp, tpe, pur, eff, loc) => visitExp(exp)
      case Expression.GetChannel(exp, tpe, pur, eff, loc) => visitExp(exp)
      case Expression.PutChannel(exp1, exp2, tpe, pur, eff, loc) => visitExp(exp1) ++ visitExp(exp2)
      case Expression.SelectChannel(rules, default, tpe, pur, eff, loc) => Counter.merge(rules.map(visitSelectChannelRule)) ++ Counter.merge(default.map(visitExp))
      case Expression.Spawn(exp, tpe, pur, eff, loc) => visitExp(exp)
      case Expression.Lazy(exp, tpe, loc) => visitExp(exp)
      case Expression.Force(exp, tpe, pur, eff, loc) => visitExp(exp)
      case Expression.FixpointConstraintSet(cs, stf, tpe, loc) => Counter.merge(cs.map(visitConstraint))
      case Expression.FixpointLambda(pparams, exp, stf, tpe, pur, eff, loc) => visitExp(exp)
      case Expression.FixpointMerge(exp1, exp2, stf, tpe, pur, eff, loc) => visitExp(exp1) ++ visitExp(exp2)
      case Expression.FixpointSolve(exp, stf, tpe, pur, eff, loc) => visitExp(exp)
      case Expression.FixpointFilter(pred, exp, tpe, pur, eff, loc) => visitExp(exp)
      case Expression.FixpointInject(exp, pred, tpe, pur, eff, loc) => visitExp(exp)
      case Expression.FixpointProject(pred, exp, tpe, pur, eff, loc) => visitExp(exp)
      case Expression.Reify(t, tpe, pur, eff, loc) => Counter.empty
      case Expression.ReifyType(t, k, tpe, pur, eff, loc) => Counter.empty
      case Expression.ReifyEff(sym, exp1, exp2, exp3, tpe, pur, eff, loc) => visitExp(exp1) ++ visitExp(exp2) ++ visitExp(exp3)
    }

    base ++ subExprs
  }

  /**
    * Counts AST nodes in the given rule.
    */
  private def visitMatchRule(rule: MatchRule): Counter = rule match {
    case MatchRule(pat, guard, exp) => visitExp(guard) ++ visitExp(exp)
  }

  /**
    * Counts AST nodes in the given rule.
    */
  private def visitChoiceRule(rule: ChoiceRule): Counter = rule match {
    case ChoiceRule(pat, exp) => visitExp(exp)
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
    case Body.Guard(exp, loc) => visitExp(exp)
    case Body.Loop(varSyms, exp, loc) => visitExp(exp)
  }

  /**
    * Counts AST nodes in the given JVMMethod
    */
  private def visitJvmMethod(method: JvmMethod): Counter = method match {
    case JvmMethod(_, _, exp, _, _, _, _) => visitExp(exp)
  }

  /**
    * Returns the name of the given expression.
    */
  private def getName(expression: Expression): String = expression.productPrefix

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

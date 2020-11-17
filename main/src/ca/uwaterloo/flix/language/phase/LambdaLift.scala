/*
 * Copyright 2015-2016 Ming-Ho Yee
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
import ca.uwaterloo.flix.language.CompilationError
import ca.uwaterloo.flix.language.ast.SimplifiedAst._
import ca.uwaterloo.flix.language.ast.{Ast, SimplifiedAst, Symbol}
import ca.uwaterloo.flix.util.Validation._
import ca.uwaterloo.flix.util.{InternalCompilerException, Validation}

import scala.collection.mutable

object LambdaLift extends Phase[SimplifiedAst.Root, SimplifiedAst.Root] {

  /**
    * Mutable map of top level definitions.
    */
  private type TopLevel = mutable.Map[Symbol.DefnSym, SimplifiedAst.Def]

  /**
    * Performs lambda lifting on the given AST `root`.
    */
  def run(root: SimplifiedAst.Root)(implicit flix: Flix): Validation[SimplifiedAst.Root, CompilationError] = flix.phase("LambdaLift") {
    // A mutable map to hold lambdas that are lifted to the top level.
    val m: TopLevel = mutable.Map.empty

    // Definitions.
    val definitions = root.defs.map {
      case (sym, decl) => sym -> liftDef(decl, m)
    }

    // Properties.
    val properties = root.properties.map {
      property => liftProperty(property, m)
    }

    // Return the updated AST root.
    root.copy(defs = definitions ++ m, properties = properties).toSuccess
  }

  /**
    * Performs lambda lifting on the given definition `def0`.
    */
  private def liftDef(def0: SimplifiedAst.Def, m: TopLevel)(implicit flix: Flix): SimplifiedAst.Def = {
    // Lift the closure converted expression.
    val liftedExp = liftExp(def0.sym.namespace, def0.exp, def0.sym.name, m)

    // Reassemble the definition.
    def0.copy(exp = liftedExp)
  }

  /**
    * Performs lambda lifting on the given property `property0`.
    */
  private def liftProperty(property0: SimplifiedAst.Property, m: TopLevel)(implicit flix: Flix): SimplifiedAst.Property = {
    // Lift the closure converted expression.
    val liftedExp = liftExp(property0.defn.namespace, property0.exp, "property", m)

    // Reassemble the property.
    property0.copy(exp = liftedExp)
  }

  /**
    * Performs lambda lifting on the given expression `exp0` using the given `name` as part of the lifted name.
    */
  private def liftExp(ns: List[String], exp0: Expression, name: String, m: TopLevel)(implicit flix: Flix): SimplifiedAst.Expression = {
    /**
      * Performs closure conversion and lambda lifting on the given expression `exp0`.
      */
    def visitExp(e: Expression): Expression = e match {
      case SimplifiedAst.Expression.Unit => e

      case SimplifiedAst.Expression.Null(tpe) => e

      case SimplifiedAst.Expression.True => e

      case SimplifiedAst.Expression.False => e

      case SimplifiedAst.Expression.Char(lit) => e

      case SimplifiedAst.Expression.Float32(lit) => e

      case SimplifiedAst.Expression.Float64(lit) => e

      case SimplifiedAst.Expression.Int8(lit) => e

      case SimplifiedAst.Expression.Int16(lit) => e

      case SimplifiedAst.Expression.Int32(lit) => e

      case SimplifiedAst.Expression.Int64(lit) => e

      case SimplifiedAst.Expression.BigInt(lit) => e

      case SimplifiedAst.Expression.Str(lit) => e

      case SimplifiedAst.Expression.Var(sym, tpe, loc) => e

      case SimplifiedAst.Expression.Def(sym, tpe, loc) => e

      case SimplifiedAst.Expression.LambdaClosure(fparams, freeVars, exp, tpe, loc) =>
        // Recursively lift the inner expression.
        val liftedExp = visitExp(exp)

        // Generate a fresh symbol for the new lifted definition.
        val freshSymbol = Symbol.freshDefnSym(ns, name)

        // Construct annotations and modifiers for the fresh definition.
        val ann = Ast.Annotations.Empty
        val mod = Ast.Modifiers(Ast.Modifier.Synthetic :: Nil)

        // Construct a new definition.
        val defn = SimplifiedAst.Def(ann, mod, freshSymbol, fparams, liftedExp, tpe, loc)

        // Add the new definition to the map of lifted definitions.
        m += (freshSymbol -> defn)

        // Construct the closure expression.
        SimplifiedAst.Expression.Closure(freshSymbol, freeVars, tpe, loc)

      case SimplifiedAst.Expression.Closure(sym, freeVars, tpe, loc) => e

      case SimplifiedAst.Expression.Apply(exp, args, tpe, loc) =>
        val e = visitExp(exp)
        val as = args map visitExp
        Expression.Apply(e, as, tpe, loc)

      case SimplifiedAst.Expression.ApplyClo(exp, args, tpe, loc) =>
        val e = visitExp(exp)
        val as = args map visitExp
        Expression.ApplyClo(e, as, tpe, loc)

      case SimplifiedAst.Expression.ApplyDef(sym, args, tpe, loc) =>
        val as = args map visitExp
        Expression.ApplyDef(sym, as, tpe, loc)

      case SimplifiedAst.Expression.Unary(sop, op, exp, tpe, loc) =>
        val e = visitExp(exp)
        Expression.Unary(sop, op, e, tpe, loc)

      case SimplifiedAst.Expression.Binary(sop, op, exp1, exp2, tpe, loc) =>
        val e1 = visitExp(exp1)
        val e2 = visitExp(exp2)
        Expression.Binary(sop, op, e1, e2, tpe, loc)

      case SimplifiedAst.Expression.IfThenElse(exp1, exp2, exp3, tpe, loc) =>
        val e1 = visitExp(exp1)
        val e2 = visitExp(exp2)
        val e3 = visitExp(exp3)
        Expression.IfThenElse(e1, e2, e3, tpe, loc)

      case SimplifiedAst.Expression.Branch(exp, branches, tpe, loc) =>
        val e = visitExp(exp)
        val bs = branches map {
          case (sym, br) => sym -> visitExp(br)
        }
        Expression.Branch(e, bs, tpe, loc)

      case SimplifiedAst.Expression.JumpTo(sym, tpe, loc) =>
        Expression.JumpTo(sym, tpe, loc)

      case SimplifiedAst.Expression.Let(sym, exp1, exp2, tpe, loc) =>
        val e1 = visitExp(exp1)
        val e2 = visitExp(exp2)
        Expression.Let(sym, e1, e2, tpe, loc)

      case SimplifiedAst.Expression.Is(sym, tag, exp, loc) =>
        val e = visitExp(exp)
        Expression.Is(sym, tag, e, loc)

      case SimplifiedAst.Expression.Tag(enum, tag, exp, tpe, loc) =>
        val e = visitExp(exp)
        Expression.Tag(enum, tag, e, tpe, loc)

      case SimplifiedAst.Expression.Untag(sym, tag, exp, tpe, loc) =>
        val e = visitExp(exp)
        Expression.Untag(sym, tag, e, tpe, loc)

      case SimplifiedAst.Expression.Index(exp, offset, tpe, loc) =>
        val e = visitExp(exp)
        Expression.Index(e, offset, tpe, loc)

      case SimplifiedAst.Expression.Tuple(elms, tpe, loc) =>
        val es = elms map visitExp
        Expression.Tuple(es, tpe, loc)

      case SimplifiedAst.Expression.RecordEmpty(tpe, loc) =>
        Expression.RecordEmpty(tpe, loc)

      case SimplifiedAst.Expression.RecordSelect(exp, field, tpe, loc) =>
        val e = visitExp(exp)
        Expression.RecordSelect(e, field, tpe, loc)

      case SimplifiedAst.Expression.RecordExtend(field, value, rest, tpe, loc) =>
        val v = visitExp(value)
        val r = visitExp(rest)
        Expression.RecordExtend(field, v, r, tpe, loc)

      case SimplifiedAst.Expression.RecordRestrict(field, rest, tpe, loc) =>
        val r = visitExp(rest)
        Expression.RecordRestrict(field, r, tpe, loc)

      case SimplifiedAst.Expression.ArrayLit(elms, tpe, loc) =>
        val es = elms map visitExp
        Expression.ArrayLit(es, tpe, loc)

      case SimplifiedAst.Expression.ArrayNew(elm, len, tpe, loc) =>
        val e = visitExp(elm)
        val l = visitExp(len)
        Expression.ArrayNew(e, l, tpe, loc)

      case SimplifiedAst.Expression.ArrayLoad(base, index, tpe, loc) =>
        val b = visitExp(base)
        val i = visitExp(index)
        Expression.ArrayLoad(b, i, tpe, loc)

      case SimplifiedAst.Expression.ArrayStore(base, index, elm, tpe, loc) =>
        val b = visitExp(base)
        val i = visitExp(index)
        val e = visitExp(elm)
        Expression.ArrayStore(b, i, e, tpe, loc)

      case SimplifiedAst.Expression.ArrayLength(base, tpe, loc) =>
        val b = visitExp(base)
        Expression.ArrayLength(b, tpe, loc)

      case SimplifiedAst.Expression.ArraySlice(base, startIndex, endIndex, tpe, loc) =>
        val b = visitExp(base)
        val i1 = visitExp(startIndex)
        val i2 = visitExp(endIndex)
        Expression.ArraySlice(b, i1, i2, tpe, loc)

      case SimplifiedAst.Expression.Ref(exp, tpe, loc) =>
        val e = visitExp(exp)
        Expression.Ref(e, tpe, loc)

      case SimplifiedAst.Expression.Deref(exp, tpe, loc) =>
        val e = visitExp(exp)
        Expression.Deref(e, tpe, loc)

      case SimplifiedAst.Expression.Assign(exp1, exp2, tpe, loc) =>
        val e1 = visitExp(exp1)
        val e2 = visitExp(exp2)
        Expression.Assign(e1, e2, tpe, loc)

      case SimplifiedAst.Expression.Existential(params, exp, loc) =>
        Expression.Existential(params, visitExp(exp), loc)

      case SimplifiedAst.Expression.Universal(params, exp, loc) =>
        Expression.Universal(params, visitExp(exp), loc)

      case SimplifiedAst.Expression.Cast(exp, tpe, loc) =>
        val e = visitExp(exp)
        Expression.Cast(e, tpe, loc)

      case SimplifiedAst.Expression.TryCatch(exp, rules, tpe, loc) =>
        val e = visitExp(exp)
        val rs = rules map {
          case CatchRule(sym, clazz, body) =>
            val b = visitExp(body)
            CatchRule(sym, clazz, b)
        }
        Expression.TryCatch(e, rs, tpe, loc)

      case SimplifiedAst.Expression.InvokeConstructor(constructor, args, tpe, loc) =>
        val as = args.map(visitExp)
        Expression.InvokeConstructor(constructor, as, tpe, loc)

      case SimplifiedAst.Expression.InvokeMethod(method, exp, args, tpe, loc) =>
        val e = visitExp(exp)
        val as = args.map(visitExp)
        Expression.InvokeMethod(method, e, as, tpe, loc)

      case SimplifiedAst.Expression.InvokeStaticMethod(method, args, tpe, loc) =>
        val as = args.map(visitExp)
        Expression.InvokeStaticMethod(method, as, tpe, loc)

      case SimplifiedAst.Expression.GetField(field, exp, tpe, loc) =>
        val e = visitExp(exp)
        Expression.GetField(field, e, tpe, loc)

      case SimplifiedAst.Expression.PutField(field, exp1, exp2, tpe, loc) =>
        val e1 = visitExp(exp1)
        val e2 = visitExp(exp2)
        Expression.PutField(field, e1, e2, tpe, loc)

      case SimplifiedAst.Expression.GetStaticField(field, tpe, loc) =>
        Expression.GetStaticField(field, tpe, loc)

      case SimplifiedAst.Expression.PutStaticField(field, exp, tpe, loc) =>
        val e = visitExp(exp)
        Expression.PutStaticField(field, e, tpe, loc)

      case SimplifiedAst.Expression.NewChannel(exp, tpe, loc) =>
        val e = visitExp(exp)
        Expression.NewChannel(e, tpe, loc)

      case SimplifiedAst.Expression.GetChannel(exp, tpe, loc) =>
        val e = visitExp(exp)
        Expression.GetChannel(e, tpe, loc)

      case SimplifiedAst.Expression.PutChannel(exp1, exp2, tpe, loc) =>
        val e1 = visitExp(exp1)
        val e2 = visitExp(exp2)
        Expression.PutChannel(e1, e2, tpe, loc)

      case SimplifiedAst.Expression.SelectChannel(rules, default, tpe, loc) =>
        val rs = rules map {
          case SelectChannelRule(sym, chan, exp) =>
            val c = visitExp(chan)
            val e = visitExp(exp)
            SelectChannelRule(sym, c, e)
        }

        val d = default.map(visitExp)

        Expression.SelectChannel(rs, d, tpe, loc)

      case SimplifiedAst.Expression.Spawn(exp, tpe, loc) =>
        val e = visitExp(exp)
        Expression.Spawn(e, tpe, loc)

      case SimplifiedAst.Expression.Lazy(exp, tpe, loc) =>
        val e = visitExp(exp)
        Expression.Lazy(e, tpe, loc)

      case SimplifiedAst.Expression.Force(exp, tpe, loc) =>
        val e = visitExp(exp)
        Expression.Force(e, tpe, loc)

      case SimplifiedAst.Expression.FixpointConstraintSet(cs0, tpe, loc) =>
        val cs = cs0.map(visitConstraint)
        Expression.FixpointConstraintSet(cs, tpe, loc)

      case SimplifiedAst.Expression.FixpointCompose(exp1, exp2, tpe, loc) =>
        val e1 = visitExp(exp1)
        val e2 = visitExp(exp2)
        Expression.FixpointCompose(e1, e2, tpe, loc)

      case SimplifiedAst.Expression.FixpointSolve(exp, stf, tpe, loc) =>
        val e = visitExp(exp)
        Expression.FixpointSolve(e, stf, tpe, loc)

      case SimplifiedAst.Expression.FixpointProject(pred, exp, tpe, loc) =>
        val e = visitExp(exp)
        Expression.FixpointProject(pred, e, tpe, loc)

      case SimplifiedAst.Expression.FixpointEntails(exp1, exp2, tpe, loc) =>
        val e1 = visitExp(exp1)
        val e2 = visitExp(exp2)
        Expression.FixpointEntails(e1, e2, tpe, loc)

      case SimplifiedAst.Expression.FixpointFold(pred, exp1, exp2, exp3, tpe, loc) =>
        val e1 = visitExp(exp1)
        val e2 = visitExp(exp2)
        val e3 = visitExp(exp3)
        Expression.FixpointFold(pred, e1, e2, e3, tpe, loc)


      case SimplifiedAst.Expression.HoleError(sym, tpe, loc) => e

      case SimplifiedAst.Expression.MatchError(tpe, loc) => e

      case SimplifiedAst.Expression.Lambda(exp, args, tpe, loc) => throw InternalCompilerException(s"Unexpected lambda expression. Every lambda expression should have been converted to a LambdaClosure.")
      case SimplifiedAst.Expression.ApplyCloTail(exp, args, tpe, loc) => throw InternalCompilerException(s"Unexpected expression: '${exp0.getClass}'.")
      case SimplifiedAst.Expression.ApplyDefTail(sym, args, tpe, loc) => throw InternalCompilerException(s"Unexpected expression: '${exp0.getClass}'.")
      case SimplifiedAst.Expression.ApplySelfTail(sym, formals, actuals, tpe, loc) => throw InternalCompilerException(s"Unexpected expression: '${exp0.getClass}'.")
    }

    /**
      * Performs lambda lifting on the given head predicate `head0`.
      */
    def visitConstraint(c0: SimplifiedAst.Constraint): SimplifiedAst.Constraint = c0 match {
      case SimplifiedAst.Constraint(cparams, head0, body0, loc) =>
        val head = visitHeadPredicate(head0)
        val body = body0.map(visitBodyPredicate)
        SimplifiedAst.Constraint(cparams, head, body, loc)
    }

    /**
      * Performs lambda lifting on the given head predicate `head0`.
      */
    def visitHeadPredicate(head0: Predicate.Head): SimplifiedAst.Predicate.Head = head0 match {
      case Predicate.Head.Atom(pred, den, terms, tpe, loc) =>
        val ts = terms map visitHeadTerm
        Predicate.Head.Atom(pred, den, ts, tpe, loc)

      case Predicate.Head.Union(exp, tpe, loc) =>
        val e = visitExp(exp)
        Predicate.Head.Union(e, tpe, loc)
    }

    /**
      * Performs lambda lifting on the given body predicate `body0`.
      */
    def visitBodyPredicate(body0: Predicate.Body): SimplifiedAst.Predicate.Body = body0 match {
      case Predicate.Body.Atom(pred, den, polarity, terms, tpe, loc) =>
        val ts = terms.map(visitBodyTerm)
        Predicate.Body.Atom(pred, den, polarity, ts, tpe, loc)

      case Predicate.Body.Guard(exp, loc) =>
        val e = visitExp(exp)
        Predicate.Body.Guard(e, loc)
    }

    /**
      * Performs lambda lifting on the given head term `term0`.
      */
    def visitHeadTerm(term0: Term.Head): SimplifiedAst.Term.Head = term0 match {
      case Term.Head.QuantVar(sym, tpe, loc) => term0

      case Term.Head.CapturedVar(sym, tpe, loc) => term0

      case Term.Head.Lit(exp, tpe, loc) =>
        val e = visitExp(exp)
        Term.Head.Lit(e, tpe, loc)

      case Term.Head.App(exp, args, tpe, loc) =>
        val e = visitExp(exp)
        Term.Head.App(e, args, tpe, loc)
    }

    /**
      * Performs lambda lifting on the given body term `term0`.
      */
    def visitBodyTerm(term0: Term.Body): SimplifiedAst.Term.Body = term0 match {
      case Term.Body.Wild(tpe, loc) => term0

      case Term.Body.QuantVar(sym, tpe, loc) => term0

      case Term.Body.CapturedVar(sym, tpe, loc) => term0

      case Term.Body.Lit(exp, tpe, loc) =>
        val e = visitExp(exp)
        Term.Body.Lit(e, tpe, loc)
    }

    visitExp(exp0)
  }

}

/*
 * Copyright 2017 Magnus Madsen
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
import ca.uwaterloo.flix.language.ast.{SimplifiedAst, Symbol}
import ca.uwaterloo.flix.language.debug.PrettyPrinter
import ca.uwaterloo.flix.util.Validation._
import ca.uwaterloo.flix.util.vt._
import ca.uwaterloo.flix.util.{InternalCompilerException, Validation}

/**
  * The Optimization phase performs intra-procedural optimizations.
  *
  * Specifically,
  *
  * - Elimination of dead branches (e.g. if (true) e1 else e2).
  * - Copy propagation (e.g. let z = w; let y = z; let x = y; x -> w)
  */
object Optimizer extends Phase[SimplifiedAst.Root, SimplifiedAst.Root] {

  /**
    * Returns an optimized version of the given AST `root`.
    */
  def run(root: SimplifiedAst.Root)(implicit flix: Flix): Validation[SimplifiedAst.Root, CompilationError] = flix.phase("Optimizer") {

    /**
      * Performs intra-procedural optimization on the given expression `exp0` and substitution map `env0`.
      */
    def visitExp(exp0: Expression, env0: Map[Symbol.VarSym, Symbol.VarSym]): Expression = exp0 match {
      case Expression.Unit => exp0

      case Expression.True => exp0

      case Expression.False => exp0

      case Expression.Char(lit) => exp0

      case Expression.Float32(lit) => exp0

      case Expression.Float64(lit) => exp0

      case Expression.Int8(lit) => exp0

      case Expression.Int16(lit) => exp0

      case Expression.Int32(lit) => exp0

      case Expression.Int64(lit) => exp0

      case Expression.BigInt(lit) => exp0

      case Expression.Str(lit) => exp0

      case Expression.Var(sym, tpe, loc) =>
        // Lookup to see if the variable should be replaced by a copy.
        env0.get(sym) match {
          case None => Expression.Var(sym, tpe, loc)
          case Some(srcSym) => Expression.Var(srcSym, tpe, loc)
        }

      case Expression.Def(sym, tpe, loc) => Expression.Def(sym, tpe, loc)

      case Expression.Closure(sym, freeVars, tpe, loc) =>
        val fvs = freeVars map {
          case FreeVar(s, varType) => FreeVar(env0.getOrElse(s, s), varType)
        }
        Expression.Closure(sym, fvs, tpe, loc)

      case Expression.ApplyClo(exp, args, tpe, loc) =>
        val e = visitExp(exp, env0)
        val as = args map (visitExp(_, env0))
        Expression.ApplyClo(e, as, tpe, loc)

      case Expression.ApplyDef(sym, args, tpe, loc) =>
        val as = args map (visitExp(_, env0))
        Expression.ApplyDef(sym, as, tpe, loc)

      case Expression.ApplyCloTail(exp, args, tpe, loc) =>
        val e = visitExp(exp, env0)
        val as = args map (visitExp(_, env0))
        Expression.ApplyCloTail(e, as, tpe, loc)

      case Expression.ApplyDefTail(sym, args, tpe, loc) =>
        val as = args map (visitExp(_, env0))
        Expression.ApplyDefTail(sym, as, tpe, loc)

      case Expression.ApplySelfTail(sym, formals, actuals, tpe, loc) =>
        val as = actuals map (visitExp(_, env0))
        Expression.ApplySelfTail(sym, formals, as, tpe, loc)

      case Expression.Unary(sop, op, exp, tpe, loc) =>
        val e = visitExp(exp, env0)
        Expression.Unary(sop, op, e, tpe, loc)

      case Expression.Binary(sop, op, exp1, exp2, tpe, loc) =>
        val e1 = visitExp(exp1, env0)
        val e2 = visitExp(exp2, env0)
        Expression.Binary(sop, op, e1, e2, tpe, loc)

      case Expression.IfThenElse(exp1, exp2, exp3, tpe, loc) =>
        // Eliminate dead branches, if possible.
        val cond = visitExp(exp1, env0)
        val consequent = visitExp(exp2, env0)
        val alternative = visitExp(exp3, env0)
        cond match {
          case Expression.True => consequent
          case Expression.False => alternative
          case _ => Expression.IfThenElse(cond, consequent, alternative, tpe, loc)
        }

      case Expression.Branch(exp, branches, tpe, loc) =>
        val e = visitExp(exp, env0)
        val bs = branches map {
          case (sym, br) => sym -> visitExp(br, env0)
        }
        Expression.Branch(e, bs, tpe, loc)

      case Expression.JumpTo(sym, tpe, loc) =>
        Expression.JumpTo(sym, tpe, loc)

      case Expression.Let(sym, exp1, exp2, tpe, loc) =>
        // Visit the value expression.
        val e1 = visitExp(exp1, env0)

        // Check for copy propagation: let x = y; e ~~> e[x -> y]
        e1 match {
          case Expression.Var(srcSym, _, _) =>
            // The srcSym might itself originate from some other symbol.
            val originalSym = env0.getOrElse(srcSym, srcSym)
            visitExp(exp2, env0 + (sym -> originalSym))
          case _ =>
            val e2 = visitExp(exp2, env0)
            Expression.Let(sym, e1, e2, tpe, loc)
        }

      case Expression.Is(sym, tag, exp, loc) =>
        val e = visitExp(exp, env0)
        Expression.Is(sym, tag, e, loc)

      case Expression.Tag(sym, tag, exp, tpe, loc) =>
        val e = visitExp(exp, env0)
        Expression.Tag(sym, tag, e, tpe, loc)

      case Expression.Untag(sym, tag, exp, tpe, loc) =>
        val e = visitExp(exp, env0)
        Expression.Untag(sym, tag, e, tpe, loc)

      case Expression.Index(base, offset, tpe, loc) =>
        val b = visitExp(base, env0)
        Expression.Index(b, offset, tpe, loc)

      case Expression.Tuple(elms, tpe, loc) =>
        val es = elms map (visitExp(_, env0))
        Expression.Tuple(es, tpe, loc)

      case Expression.RecordEmpty(tpe, loc) =>
        Expression.RecordEmpty(tpe, loc)

      case Expression.RecordSelect(exp, label, tpe, loc) =>
        val e = visitExp(exp, env0)
        Expression.RecordSelect(e, label, tpe, loc)

      case Expression.RecordExtend(label, value, rest, tpe, loc) =>
        val v = visitExp(value, env0)
        val r = visitExp(rest, env0)
        Expression.RecordExtend(label, v, r, tpe, loc)

      case Expression.RecordRestrict(label, rest, tpe, loc) =>
        val r = visitExp(rest, env0)
        Expression.RecordRestrict(label, r, tpe, loc)

      case Expression.ArrayLit(elms, tpe, loc) =>
        val es = elms map (visitExp(_, env0))
        Expression.ArrayLit(es, tpe, loc)

      case Expression.ArrayNew(elm, len, tpe, loc) =>
        val e = visitExp(elm, env0)
        val ln = visitExp(len, env0)
        Expression.ArrayNew(e, ln, tpe, loc)

      case Expression.ArrayLoad(base, index, tpe, loc) =>
        val b = visitExp(base, env0)
        val i = visitExp(index, env0)
        Expression.ArrayLoad(b, i, tpe, loc)

      case Expression.ArrayStore(base, index, elm, tpe, loc) =>
        val b = visitExp(base, env0)
        val i = visitExp(index, env0)
        val e = visitExp(elm, env0)
        Expression.ArrayStore(b, i, e, tpe, loc)

      case Expression.ArrayLength(base, tpe, loc) =>
        val b = visitExp(base, env0)
        Expression.ArrayLength(b, tpe, loc)

      case Expression.ArraySlice(base, startIndex, endIndex, tpe, loc) =>
        val b = visitExp(base, env0)
        val i1 = visitExp(startIndex, env0)
        val i2 = visitExp(endIndex, env0)
        Expression.ArraySlice(b, i1, i2, tpe, loc)

      case Expression.Ref(exp, tpe, loc) =>
        val e = visitExp(exp, env0)
        Expression.Ref(e, tpe, loc)

      case Expression.Deref(exp, tpe, loc) =>
        val e = visitExp(exp, env0)
        Expression.Deref(e, tpe, loc)

      case Expression.Assign(exp1, exp2, tpe, loc) =>
        val e1 = visitExp(exp1, env0)
        val e2 = visitExp(exp2, env0)
        Expression.Assign(e1, e2, tpe, loc)

      case Expression.Existential(fparam, exp, loc) =>
        val e = visitExp(exp, env0)
        Expression.Existential(fparam, e, loc)

      case Expression.Universal(fparam, exp, loc) =>
        val e = visitExp(exp, env0)
        Expression.Universal(fparam, e, loc)

      case Expression.Cast(exp, tpe, loc) =>
        val e = visitExp(exp, env0)
        Expression.Cast(e, tpe, loc)

      case Expression.TryCatch(exp, rules, tpe, loc) =>
        val e = visitExp(exp, env0)
        val rs = rules map {
          case CatchRule(sym, clazz, body) =>
            val b = visitExp(body, env0)
            CatchRule(sym, clazz, b)
        }
        Expression.TryCatch(e, rs, tpe, loc)

      case Expression.InvokeConstructor(constructor, args, tpe, loc) =>
        val as = args map (visitExp(_, env0))
        Expression.InvokeConstructor(constructor, as, tpe, loc)

      case Expression.InvokeMethod(method, exp, args, tpe, loc) =>
        val e = visitExp(exp, env0)
        val as = args.map(visitExp(_, env0))
        Expression.InvokeMethod(method, e, as, tpe, loc)

      case Expression.InvokeStaticMethod(method, args, tpe, loc) =>
        val as = args.map(visitExp(_, env0))
        Expression.InvokeStaticMethod(method, as, tpe, loc)

      case Expression.GetField(field, exp, tpe, loc) =>
        val e = visitExp(exp, env0)
        Expression.GetField(field, e, tpe, loc)

      case Expression.PutField(field, exp1, exp2, tpe, loc) =>
        val e1 = visitExp(exp1, env0)
        val e2 = visitExp(exp2, env0)
        Expression.PutField(field, e1, e2, tpe, loc)

      case Expression.GetStaticField(field, tpe, loc) =>
        Expression.GetStaticField(field, tpe, loc)

      case Expression.PutStaticField(field, exp, tpe, loc) =>
        val e = visitExp(exp, env0)
        Expression.PutStaticField(field, e, tpe, loc)

      case Expression.NewChannel(exp, tpe, loc) =>
        val e = visitExp(exp, env0)
        Expression.NewChannel(e, tpe, loc)

      case Expression.GetChannel(exp, tpe, loc) =>
        val e = visitExp(exp, env0)
        Expression.GetChannel(e, tpe, loc)

      case Expression.PutChannel(exp1, exp2, tpe, loc) =>
        val e1 = visitExp(exp1, env0)
        val e2 = visitExp(exp2, env0)
        Expression.PutChannel(e1, e2, tpe, loc)

      case Expression.SelectChannel(rules, default, tpe, loc) =>
        val rs = rules map {
          case SelectChannelRule(sym, chan, exp) =>
            val c = visitExp(chan, env0)
            val e = visitExp(exp, env0)
            SelectChannelRule(sym, c, e)
        }

        val d = default.map(visitExp(_, env0))

        Expression.SelectChannel(rs, d, tpe, loc)

      case Expression.ProcessSpawn(exp, tpe, loc) =>
        val e = visitExp(exp, env0)
        Expression.ProcessSpawn(e, tpe, loc)

      case Expression.FixpointConstraintSet(cs0, tpe, loc) =>
        val cs = cs0.map(visitConstraint(_, env0))
        Expression.FixpointConstraintSet(cs, tpe, loc)

      case Expression.FixpointCompose(exp1, exp2, tpe, loc) =>
        val e1 = visitExp(exp1, env0)
        val e2 = visitExp(exp2, env0)
        Expression.FixpointCompose(e1, e2, tpe, loc)

      case Expression.FixpointSolve(exp, stf, tpe, loc) =>
        val e = visitExp(exp, env0)
        Expression.FixpointSolve(e, stf, tpe, loc)

      case Expression.FixpointProject(name, exp, tpe, loc) =>
        val e = visitExp(exp, env0)
        Expression.FixpointProject(name, e, tpe, loc)

      case Expression.FixpointEntails(exp1, exp2, tpe, loc) =>
        val e1 = visitExp(exp1, env0)
        val e2 = visitExp(exp2, env0)
        Expression.FixpointEntails(e1, e2, tpe, loc)

      case Expression.FixpointFold(name, exp1, exp2, exp3, tpe, loc) =>
        val e1 = visitExp(exp1, env0)
        val e2 = visitExp(exp2, env0)
        val e3 = visitExp(exp3, env0)
        Expression.FixpointFold(name, e1, e2, e3, tpe, loc)

      case Expression.HoleError(sym, tpe, loc) => Expression.HoleError(sym, tpe, loc)

      case Expression.MatchError(tpe, loc) => Expression.MatchError(tpe, loc)

      case Expression.LambdaClosure(fparams, freeVars, exp, tpe, loc) => throw InternalCompilerException(s"Unexpected expression: '${exp0.getClass}'.")

      case Expression.Lambda(args, body, tpe, loc) => throw InternalCompilerException(s"Unexpected expression: '${exp0.getClass}'.")

      case Expression.Apply(exp, args, tpe, loc) => throw InternalCompilerException(s"Unexpected expression: '${exp0.getClass}'.")
    }

    /**
      * Performs intra-procedural optimization on the given constraint `c0`.
      */
    def visitConstraint(c0: SimplifiedAst.Constraint, env0: Map[Symbol.VarSym, Symbol.VarSym]): SimplifiedAst.Constraint = c0 match {
      case SimplifiedAst.Constraint(cparams, head0, body0, loc) =>
        val head = visitHeadPred(head0, env0)
        val body = body0.map(visitBodyPred(_, env0))
        SimplifiedAst.Constraint(cparams, head, body, loc)
    }

    /**
      * Performs intra-procedural optimization on the terms of the given head predicate `p0`.
      */
    def visitHeadPred(p0: Predicate.Head, env0: Map[Symbol.VarSym, Symbol.VarSym]): Predicate.Head = p0 match {
      case Predicate.Head.Atom(name, den, terms, tpe, loc) =>
        val ts = terms.map(visitHeadTerm(_, env0))
        Predicate.Head.Atom(name, den, ts, tpe, loc)

      case Predicate.Head.Union(exp, tpe, loc) =>
        val e = visitExp(exp, env0)
        Predicate.Head.Union(e, tpe, loc)
    }

    /**
      * Performs intra-procedural optimization on the terms of the given body predicate `p0`.
      */
    def visitBodyPred(p0: Predicate.Body, env0: Map[Symbol.VarSym, Symbol.VarSym]): Predicate.Body = p0 match {
      case Predicate.Body.Atom(name, den, polarity, terms, tpe, loc) =>
        val ts = terms.map(visitBodyTerm(_, env0))
        Predicate.Body.Atom(name, den, polarity, ts, tpe, loc)

      case Predicate.Body.Guard(exp, loc) =>
        val e = visitExp(exp, env0)
        Predicate.Body.Guard(e, loc)
    }

    /**
      * Performs intra-procedural optimization on the given head term `t0`.
      */
    def visitHeadTerm(h0: Term.Head, env0: Map[Symbol.VarSym, Symbol.VarSym]): Term.Head = h0 match {
      case Term.Head.QuantVar(sym, tpe, loc) =>
        Term.Head.QuantVar(sym, tpe, loc)

      case Term.Head.CapturedVar(sym, tpe, loc) =>
        // Lookup to see if the variable should be replaced by a copy.
        env0.get(sym) match {
          case None => Term.Head.CapturedVar(sym, tpe, loc)
          case Some(srcSym) => Term.Head.CapturedVar(srcSym, tpe, loc)
        }

      case Term.Head.Lit(exp, tpe, loc) =>
        val e = visitExp(exp, env0)
        Term.Head.Lit(e, tpe, loc)

      case Term.Head.App(exp, args, tpe, loc) =>
        val e = visitExp(exp, env0)
        Term.Head.App(e, args, tpe, loc)
    }

    /**
      * Performs intra-procedural optimization on the given body term `t0`.
      */
    def visitBodyTerm(b0: Term.Body, env0: Map[Symbol.VarSym, Symbol.VarSym]): Term.Body = b0 match {
      case Term.Body.Wild(tpe, loc) => Term.Body.Wild(tpe, loc)
      case Term.Body.QuantVar(sym, tpe, loc) => Term.Body.QuantVar(sym, tpe, loc)
      case Term.Body.CapturedVar(sym, tpe, loc) => Term.Body.CapturedVar(sym, tpe, loc)
      case Term.Body.Lit(exp, tpe, loc) => Term.Body.Lit(visitExp(exp, Map.empty), tpe, loc)
    }

    // Visit every definition in the program.
    val defs = root.defs.map {
      case (sym, defn) => sym -> defn.copy(exp = visitExp(defn.exp, Map.empty))
    }

    // Visit every property in the program.
    val properties = root.properties.map {
      case property => property.copy(exp = visitExp(property.exp, Map.empty))
    }

    // Reassemble the ast root.
    val result = root.copy(
      defs = defs,
      properties = properties
    )

    // Print the ast if debugging is enabled.
    if (flix.options.debug) {
      println(PrettyPrinter.Simplified.fmtRoot(result).fmt(TerminalContext.AnsiTerminal))
    }

    result.toSuccess
  }

}

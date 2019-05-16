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
import ca.uwaterloo.flix.language.ast.{Ast, Kind, SourceLocation, Symbol, Type}
import ca.uwaterloo.flix.util.{InternalCompilerException, Validation}
import ca.uwaterloo.flix.util.Validation._

import scala.collection.mutable

object ClosureConv extends Phase[Root, Root] {

  /**
    * Performs closure conversion on the given AST `root`.
    */
  def run(root: Root)(implicit flix: Flix): Validation[Root, CompilationError] = flix.phase("ClosureConv") {

    // Definitions.
    val definitions = root.defs.map {
      case (sym, decl) => sym -> visitDef(decl)
    }

    // Handlers.
    val handlers = root.handlers.map {
      case (sym, handler) => sym -> visitHandler(handler)
    }

    // Properties.
    val properties = root.properties.map {
      property => visitProperty(property)
    }

    // Return the updated AST root.
    root.copy(defs = definitions, handlers = handlers, properties = properties).toSuccess
  }

  /**
    * Performs closure conversion on the given definition `def0`.
    */
  private def visitDef(def0: Def)(implicit flix: Flix): Def = {
    val convertedExp = visitExp(def0.exp)
    def0.copy(exp = convertedExp)
  }

  /**
    * Performs closure conversion on the given handler `handler0`.
    */
  private def visitHandler(handler0: Handler)(implicit flix: Flix): Handler = {
    val convertedExp = visitExp(handler0.exp)
    handler0.copy(exp = convertedExp)
  }

  /**
    * Performs closure conversion on the given property `property0`.
    */
  private def visitProperty(property0: Property)(implicit flix: Flix): Property = {
    val convertedExp = visitExp(property0.exp)

    // Reassemble the property.
    property0.copy(exp = convertedExp)
  }

  /**
    * Performs closure conversion on the given expression `exp0`.
    */
  private def visitExp(exp0: Expression)(implicit flix: Flix): Expression = exp0 match {
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

    case Expression.Var(sym, tpe, loc) => exp0

    case Expression.Def(sym, tpe, loc) =>
      // The Def expression did not occur in an Apply expression.
      // We must create a closure, without free variables, of the definition symbol.
      Expression.Closure(sym, List.empty, tpe, loc)

    case Expression.Eff(sym, tpe, loc) => ??? // TODO: ClosureConv Effect.

    case Expression.Lambda(args, body, tpe, loc) =>
      // Retrieve the type of the function.
      val ts = tpe.typeArguments
      val (targs, tresult) = (ts.init, ts.last)

      // Convert lambdas to closures. This is the main part of the `convert` function.
      // Closure conversion happens as follows:

      // First, we collect the free variables in the lambda expression.
      // NB: We pass the lambda expression (instead of its body) to account for bound arguments.
      val fvs = freeVars(exp0).toList

      // We prepend the free variables to the arguments list. Thus all variables within the lambda body will be treated
      // uniformly. The implementation will supply values for the free variables, without any effort from the caller.
      // We introduce new symbols for each introduced parameter and replace their occurrence in the body.
      val subst = mutable.Map.empty[Symbol.VarSym, Symbol.VarSym]
      val newArgs = fvs.map {
        case (oldSym, ptype) =>
          val newSym = Symbol.freshVarSym(oldSym)(flix.genSym)
          subst += (oldSym -> newSym)
          FormalParam(newSym, Ast.Modifiers.Empty, ptype, SourceLocation.Unknown)
      } ++ args

      // Update the lambda type.
      val argTpes = fvs.map(_._2) ++ targs
      val newTpe = Type.mkUncurriedArrow(argTpes, tresult)

      val newBody = visitExp(replace(body, subst.toMap))
      // We rewrite the lambda with its new arguments list and new body, with any nested lambdas also converted.
      val lambda = Expression.Lambda(newArgs, newBody, newTpe, loc)

      // At this point, `lambda` is the original lambda expression, but with all free variables converted to new
      // arguments, prepended to the original arguments list. Additionally, any lambdas within the body have also been
      // closure converted.

      // We return a MkClosure node, which contains `lambda` (rewritten to have extra arguments so there are no more
      // free variables) as well as the cached `freeVars`. The closure will actually be created at run time, where the
      // values for the free variables are bound and stored in the closure structure. When the closure is called, the
      // bound values are passed as arguments.
      // Note that MkClosure keeps the old lambda type.
      // In a later phase, we will lift the lambda to a top-level definition.
      //Expression.LambdaClosure(lambda, fvs.map(v => FreeVar(v._1, v._2)), tpe, loc)
      Expression.LambdaClosure(newArgs, fvs.map(v => FreeVar(v._1, v._2)), newBody, tpe, loc)

    case Expression.Apply(e, args, tpe, loc) =>
      // We're trying to call some expression `e`. If `e` is a Ref, then it's a top-level function, so we directly call
      // it with ApplyRef. We remove the Ref node and don't recurse on it to avoid creating a closure.
      // We do something similar if `e` is a Hook, where we transform Apply to ApplyHook.
      e match {
        case Expression.Def(sym, _, _) => Expression.ApplyDef(sym, args.map(visitExp), tpe, loc)
        case Expression.Eff(sym, _, _) => Expression.ApplyEff(sym, args.map(visitExp), tpe, loc)
        case _ => Expression.ApplyClo(visitExp(e), args.map(visitExp), tpe, loc)
      }

    case Expression.Unary(sop, op, e, tpe, loc) =>
      Expression.Unary(sop, op, visitExp(e), tpe, loc)

    case Expression.Binary(sop, op, e1, e2, tpe, loc) =>
      Expression.Binary(sop, op, visitExp(e1), visitExp(e2), tpe, loc)

    case Expression.IfThenElse(e1, e2, e3, tpe, loc) =>
      Expression.IfThenElse(visitExp(e1), visitExp(e2), visitExp(e3), tpe, loc)

    case Expression.Branch(exp, branches, tpe, loc) =>
      val e = visitExp(exp)
      val bs = branches map {
        case (sym, br) => sym -> visitExp(br)
      }
      Expression.Branch(e, bs, tpe, loc)

    case Expression.JumpTo(sym, tpe, loc) =>
      Expression.JumpTo(sym, tpe, loc)

    case Expression.Let(sym, e1, e2, tpe, loc) =>
      Expression.Let(sym, visitExp(e1), visitExp(e2), tpe, loc)

    case Expression.LetRec(sym, e1, e2, tpe, loc) =>
      Expression.LetRec(sym, visitExp(e1), visitExp(e2), tpe, loc)

    case Expression.Is(sym, tag, e, loc) =>
      Expression.Is(sym, tag, visitExp(e), loc)

    case Expression.Tag(enum, tag, e, tpe, loc) =>
      Expression.Tag(enum, tag, visitExp(e), tpe, loc)

    case Expression.Untag(sym, tag, e, tpe, loc) =>
      Expression.Untag(sym, tag, visitExp(e), tpe, loc)

    case Expression.Index(e, offset, tpe, loc) =>
      Expression.Index(visitExp(e), offset, tpe, loc)

    case Expression.Tuple(elms, tpe, loc) =>
      Expression.Tuple(elms.map(visitExp), tpe, loc)

    case Expression.RecordEmpty(tpe, loc) =>
      Expression.RecordEmpty(tpe, loc)

    case Expression.RecordSelect(exp, label, tpe, loc) =>
      val e = visitExp(exp)
      Expression.RecordSelect(e, label, tpe, loc)

    case Expression.RecordExtend(label, value, rest, tpe, loc) =>
      val v = visitExp(value)
      val r = visitExp(rest)
      Expression.RecordExtend(label, v, r, tpe, loc)

    case Expression.RecordRestrict(label, rest, tpe, loc) =>
      val r = visitExp(rest)
      Expression.RecordRestrict(label, r, tpe, loc)

    case Expression.ArrayLit(elms, tpe, loc) =>
      Expression.ArrayLit(elms.map(visitExp), tpe, loc)

    case Expression.ArrayNew(elm, len, tpe, loc) =>
      val e1 = visitExp(elm)
      val e2 = visitExp(len)
      Expression.ArrayNew(e1, e2, tpe, loc)

    case Expression.ArrayLoad(exp1, exp2, tpe, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      Expression.ArrayLoad(e1, e2, tpe, loc)

    case Expression.ArrayStore(exp1, exp2, exp3, tpe, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      val e3 = visitExp(exp3)
      Expression.ArrayStore(e1, e2, e3, tpe, loc)

    case Expression.ArrayLength(exp, tpe, loc) =>
      val e = visitExp(exp)
      Expression.ArrayLength(e, tpe, loc)

    case Expression.ArraySlice(exp1, exp2, exp3, tpe, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      val e3 = visitExp(exp3)
      Expression.ArraySlice(e1, e2, e3, tpe, loc)

    case Expression.Ref(exp, tpe, loc) =>
      val e = visitExp(exp)
      Expression.Ref(e, tpe, loc)

    case Expression.Deref(exp, tpe, loc) =>
      val e = visitExp(exp)
      Expression.Deref(e, tpe, loc)

    case Expression.Assign(exp1, exp2, tpe, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      Expression.Assign(e1, e2, tpe, loc)

    case Expression.HandleWith(exp, bindings, tpe, loc) =>
      val e = visitExp(exp)
      val bs = bindings map {
        case HandlerBinding(sym, handler) => HandlerBinding(sym, visitExp(handler))
      }
      Expression.HandleWith(e, bs, tpe, loc)

    case Expression.Existential(params, e, loc) =>
      Expression.Existential(params, visitExp(e), loc)

    case Expression.Universal(params, e, loc) =>
      Expression.Universal(params, visitExp(e), loc)

    case Expression.TryCatch(exp, rules, tpe, loc) =>
      val e = visitExp(exp)
      val rs = rules map {
        case CatchRule(sym, clazz, body) =>
          val b = visitExp(body)
          CatchRule(sym, clazz, b)
      }
      Expression.TryCatch(e, rs, tpe, loc)

    case Expression.NativeConstructor(constructor, args, tpe, loc) =>
      val as = args map visitExp
      Expression.NativeConstructor(constructor, as, tpe, loc)

    case Expression.NativeField(field, tpe, loc) => exp0

    case Expression.NativeMethod(method, args, tpe, loc) =>
      val as = args map visitExp
      Expression.NativeMethod(method, as, tpe, loc)

    case Expression.NewChannel(exp, tpe, loc) =>
      val e = visitExp(exp)
      Expression.NewChannel(e, tpe, loc)

    case Expression.GetChannel(exp, tpe, loc) =>
      val e = visitExp(exp)
      Expression.GetChannel(e, tpe, loc)

    case Expression.PutChannel(exp1, exp2, tpe, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      Expression.PutChannel(e1, e2, tpe, loc)

    case Expression.SelectChannel(rules, default, tpe, loc) =>
      val rs = rules map {
        case SelectChannelRule(sym, chan, exp) =>
          val c = visitExp(chan)
          val e = visitExp(exp)
          SelectChannelRule(sym, c, e)
      }

      val d = default.map(visitExp(_))

      Expression.SelectChannel(rs, d, tpe, loc)

    case Expression.Spawn(exp, tpe, loc) =>
      val e = visitExp(exp)
      Expression.Spawn(e, tpe, loc)

    case Expression.Sleep(exp, tpe, loc) =>
      val e = visitExp(exp)
      Expression.Sleep(e, tpe, loc)

    case Expression.FixpointConstraint(c0, tpe, loc) =>
      val Constraint(cparams0, head0, body0) = c0
      val head = visitHeadPredicate(head0)
      val body = body0 map visitBodyPredicate
      val c = Constraint(cparams0, head, body)
      Expression.FixpointConstraint(c, tpe, loc)

    case Expression.FixpointCompose(exp1, exp2, tpe, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      Expression.FixpointCompose(e1, e2, tpe, loc)

    case Expression.FixpointSolve(exp, tpe, loc) =>
      val e = visitExp(exp)
      Expression.FixpointSolve(e, tpe, loc)

    case Expression.FixpointProject(pred, exp, tpe, loc) =>
      val p = visitPredicateWithParam(pred)
      val e = visitExp(exp)
      Expression.FixpointProject(p, e, tpe, loc)

    case Expression.FixpointEntails(exp1, exp2, tpe, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      Expression.FixpointEntails(e1, e2, tpe, loc)

    case Expression.HoleError(sym, tpe, loc) => exp0
    case Expression.MatchError(tpe, loc) => exp0
    case Expression.SwitchError(tpe, loc) => exp0

    case Expression.Closure(ref, freeVars, tpe, loc) => throw InternalCompilerException(s"Unexpected expression: '${exp0.getClass.getSimpleName}'.")
    case Expression.LambdaClosure(fparams, freeVars, exp, tpe, loc) => throw InternalCompilerException(s"Unexpected expression: '${exp0.getClass.getSimpleName}'.")
    case Expression.ApplyClo(e, args, tpe, loc) => throw InternalCompilerException(s"Unexpected expression: '${exp0.getClass.getSimpleName}'.")
    case Expression.ApplyDef(name, args, tpe, loc) => throw InternalCompilerException(s"Unexpected expression: '${exp0.getClass.getSimpleName}'.")
    case Expression.ApplyEff(name, args, tpe, loc) => throw InternalCompilerException(s"Unexpected expression: '${exp0.getClass.getSimpleName}'.")
    case Expression.ApplyCloTail(e, args, tpe, loc) => throw InternalCompilerException(s"Unexpected expression: '${exp0.getClass.getSimpleName}'.")
    case Expression.ApplyDefTail(name, args, tpe, loc) => throw InternalCompilerException(s"Unexpected expression: '${exp0.getClass.getSimpleName}'.")
    case Expression.ApplyEffTail(name, args, tpe, loc) => throw InternalCompilerException(s"Unexpected expression: '${exp0.getClass.getSimpleName}'.")
    case Expression.ApplySelfTail(name, formals, actuals, tpe, loc) => throw InternalCompilerException(s"Unexpected expression: '${exp0.getClass.getSimpleName}'.")
  }

  /**
    * Performs closure conversion on the given head predicate `head0`.
    */
  private def visitHeadPredicate(head0: Predicate.Head)(implicit flix: Flix): Predicate.Head = head0 match {
    case Predicate.Head.Atom(pred, terms, tpe, loc) =>
      val p = visitPredicateWithParam(pred)
      val ts = terms map visitHeadTerm
      Predicate.Head.Atom(p, ts, tpe, loc)
  }

  /**
    * Performs closure conversion on the given body predicate `body0`.
    */
  private def visitBodyPredicate(body0: Predicate.Body)(implicit flix: Flix): Predicate.Body = body0 match {
    case Predicate.Body.Atom(pred, polarity, terms, tpe, loc) =>
      val p = visitPredicateWithParam(pred)
      val ts = terms map visitBodyTerm
      Predicate.Body.Atom(p, polarity, ts, tpe, loc)

    case Predicate.Body.Filter(sym, terms, loc) =>
      val ts = terms map visitBodyTerm
      Predicate.Body.Filter(sym, ts, loc)

    case Predicate.Body.Functional(sym, term, loc) =>
      val t = visitHeadTerm(term)
      Predicate.Body.Functional(sym, t, loc)
  }

  /**
    * Performs closure conversion on the given head term `term0`.
    */
  private def visitHeadTerm(term0: Term.Head)(implicit flix: Flix): Term.Head = term0 match {
    case Term.Head.QuantVar(sym, tpe, loc) => term0

    case Term.Head.CapturedVar(sym, tpe, loc) => term0

    case Term.Head.Lit(lit, tpe, loc) =>
      val e = visitExp(lit)
      Term.Head.Lit(e, tpe, loc)

    case Term.Head.App(sym, args, tpe, loc) => term0
  }

  /**
    * Performs closure conversion on the given body term `term0`.
    */
  private def visitBodyTerm(term0: Term.Body)(implicit flix: Flix): Term.Body = term0 match {
    case Term.Body.Wild(tpe, loc) => Term.Body.Wild(tpe, loc)
    case Term.Body.QuantVar(sym, tpe, loc) => Term.Body.QuantVar(sym, tpe, loc)
    case Term.Body.CapturedVar(sym, tpe, loc) => Term.Body.CapturedVar(sym, tpe, loc)
    case Term.Body.Lit(exp, tpe, loc) =>
      val e = visitExp(exp)
      Term.Body.Lit(e, tpe, loc)
  }

  /**
    * Performs closure conversion on the given predicate with parameter `p0`.
    */
  private def visitPredicateWithParam(p0: PredicateWithParam)(implicit flix: Flix): PredicateWithParam = p0 match {
    case PredicateWithParam(sym, exp) =>
      val e = visitExp(exp)
      PredicateWithParam(sym, e)
  }

  /**
    * Returns the free variables in the given expression `exp`.
    * Does a left-to-right traversal of the AST, collecting free variables in order, in a LinkedHashSet.
    */
  // TODO: Use immutable, but sorted data structure?
  private def freeVars(exp0: Expression): mutable.LinkedHashSet[(Symbol.VarSym, Type)] = exp0 match {
    case Expression.Unit => mutable.LinkedHashSet.empty
    case Expression.True => mutable.LinkedHashSet.empty
    case Expression.False => mutable.LinkedHashSet.empty
    case Expression.Char(lit) => mutable.LinkedHashSet.empty
    case Expression.Float32(lit) => mutable.LinkedHashSet.empty
    case Expression.Float64(lit) => mutable.LinkedHashSet.empty
    case Expression.Int8(lit) => mutable.LinkedHashSet.empty
    case Expression.Int16(lit) => mutable.LinkedHashSet.empty
    case Expression.Int32(lit) => mutable.LinkedHashSet.empty
    case Expression.Int64(lit) => mutable.LinkedHashSet.empty
    case Expression.BigInt(lit) => mutable.LinkedHashSet.empty
    case Expression.Str(lit) => mutable.LinkedHashSet.empty
    case Expression.Var(sym, tpe, loc) => mutable.LinkedHashSet((sym, tpe))
    case Expression.Def(sym, tpe, loc) => mutable.LinkedHashSet.empty
    case Expression.Eff(sym, tpe, loc) => mutable.LinkedHashSet.empty
    case Expression.Lambda(args, body, tpe, loc) =>
      val bound = args.map(_.sym)
      freeVars(body).filterNot { v => bound.contains(v._1) }
    case Expression.Apply(exp, args, tpe, loc) =>
      freeVars(exp) ++ args.flatMap(freeVars)
    case Expression.Unary(sop, op, exp, tpe, loc) => freeVars(exp)
    case Expression.Binary(sop, op, exp1, exp2, tpe, loc) =>
      freeVars(exp1) ++ freeVars(exp2)
    case Expression.IfThenElse(exp1, exp2, exp3, tpe, loc) =>
      freeVars(exp1) ++ freeVars(exp2) ++ freeVars(exp3)
    case Expression.Branch(exp, branches, tpe, loc) =>
      mutable.LinkedHashSet.empty ++ freeVars(exp) ++ (branches flatMap {
        case (sym, br) => freeVars(br)
      })
    case Expression.JumpTo(sym, tpe, loc) => mutable.LinkedHashSet.empty
    case Expression.Let(sym, exp1, exp2, tpe, loc) =>
      val bound = sym
      freeVars(exp1) ++ freeVars(exp2).filterNot { v => bound == v._1 }
    case Expression.LetRec(sym, exp1, exp2, tpe, loc) =>
      val bound = sym
      (freeVars(exp1) ++ freeVars(exp2)).filterNot { v => bound == v._1 }
    case Expression.Is(sym, tag, exp, loc) => freeVars(exp)
    case Expression.Untag(sym, tag, exp, tpe, loc) => freeVars(exp)
    case Expression.Tag(enum, tag, exp, tpe, loc) => freeVars(exp)
    case Expression.Index(base, offset, tpe, loc) => freeVars(base)
    case Expression.Tuple(elms, tpe, loc) => mutable.LinkedHashSet.empty ++ elms.flatMap(freeVars)
    case Expression.RecordEmpty(tpe, loc) => mutable.LinkedHashSet.empty
    case Expression.RecordSelect(exp, label, tpe, loc) => freeVars(exp)
    case Expression.RecordExtend(label, value, rest, tpe, loc) => freeVars(value) ++ freeVars(rest)
    case Expression.RecordRestrict(label, rest, tpe, loc) => freeVars(rest)
    case Expression.ArrayLit(elms, tpe, loc) => mutable.LinkedHashSet.empty ++ elms.flatMap(freeVars)
    case Expression.ArrayNew(elm, len, tpe, loc) => freeVars(elm) ++ freeVars(len)
    case Expression.ArrayLoad(base, index, tpe, loc) => freeVars(base) ++ freeVars(index)
    case Expression.ArrayStore(base, index, elm, tpe, loc) => freeVars(base) ++ freeVars(index) ++ freeVars(elm)
    case Expression.ArrayLength(base, tpe, loc) => freeVars(base)
    case Expression.ArraySlice(base, beginIndex, endIndex, tpe, loc) => freeVars(base) ++ freeVars(beginIndex) ++ freeVars(endIndex)
    case Expression.Ref(exp, tpe, loc) => freeVars(exp)
    case Expression.Deref(exp, tpe, loc) => freeVars(exp)
    case Expression.Assign(exp1, exp2, tpe, loc) => freeVars(exp1) ++ freeVars(exp2)
    case Expression.HandleWith(exp, bindings, tpe, loc) => freeVars(exp) ++ bindings.flatMap(b => freeVars(b.exp))
    case Expression.Existential(fparam, exp, loc) =>
      freeVars(exp).filterNot { v => v._1 == fparam.sym }
    case Expression.Universal(fparam, exp, loc) =>
      freeVars(exp).filterNot { v => v._1 == fparam.sym }

    case Expression.TryCatch(exp, rules, tpe, loc) => mutable.LinkedHashSet.empty ++ freeVars(exp) ++ rules.flatMap(r => freeVars(r.exp).filterNot(_._1 == r.sym))
    case Expression.NativeConstructor(constructor, args, tpe, loc) => mutable.LinkedHashSet.empty ++ args.flatMap(freeVars)
    case Expression.NativeField(field, tpe, loc) => mutable.LinkedHashSet.empty
    case Expression.NativeMethod(method, args, tpe, loc) => mutable.LinkedHashSet.empty ++ args.flatMap(freeVars)

    case Expression.NewChannel(exp, tpe, loc) => freeVars(exp)
    case Expression.GetChannel(exp, tpe, loc) => freeVars(exp)
    case Expression.PutChannel(exp1, exp2, tpe, loc) => freeVars(exp1) ++ freeVars(exp2)
    case Expression.SelectChannel(rules, default, tpe, loc) =>
      val rs = mutable.LinkedHashSet.empty ++ rules.flatMap {
        case SelectChannelRule(sym, chan, exp) => freeVars(chan).filter(n1 => !List(sym).contains(n1._1))
      }

      val d = default.map(freeVars).getOrElse(mutable.LinkedHashSet.empty)

      rs ++ d
    case Expression.Spawn(exp, tpe, loc) => freeVars(exp)
    case Expression.Sleep(exp, tpe, loc) => freeVars(exp)

    case Expression.FixpointConstraint(con, tpe, loc) =>
      val Constraint(cparams, head, body) = con
      freeVars(head) ++ body.flatMap(freeVars)

    case Expression.FixpointCompose(exp1, exp2, tpe, loc) => freeVars(exp1) ++ freeVars(exp2)
    case Expression.FixpointSolve(exp, tpe, loc) => freeVars(exp)
    case Expression.FixpointProject(pred, exp, tpe, loc) => freeVars(pred.exp) ++ freeVars(exp)
    case Expression.FixpointEntails(exp1, exp2, tpe, loc) => freeVars(exp1) ++ freeVars(exp2)

    case Expression.HoleError(sym, tpe, loc) => mutable.LinkedHashSet.empty
    case Expression.MatchError(tpe, loc) => mutable.LinkedHashSet.empty
    case Expression.SwitchError(tpe, loc) => mutable.LinkedHashSet.empty

    case Expression.LambdaClosure(fparams, freeVars, exp, tpe, loc) => throw InternalCompilerException(s"Unexpected expression: '${exp0.getClass.getSimpleName}'.")
    case Expression.Closure(ref, freeVars, tpe, loc) => throw InternalCompilerException(s"Unexpected expression: '${exp0.getClass.getSimpleName}'.")
    case Expression.ApplyClo(exp, args, tpe, loc) => throw InternalCompilerException(s"Unexpected expression: '${exp0.getClass.getSimpleName}'.")
    case Expression.ApplyDef(sym, args, tpe, loc) => throw InternalCompilerException(s"Unexpected expression: '${exp0.getClass.getSimpleName}'.")
    case Expression.ApplyEff(sym, args, tpe, loc) => throw InternalCompilerException(s"Unexpected expression: '${exp0.getClass.getSimpleName}'.")
    case Expression.ApplyCloTail(sym, args, tpe, loc) => throw InternalCompilerException(s"Unexpected expression: '${exp0.getClass.getSimpleName}'.")
    case Expression.ApplyDefTail(sym, args, tpe, loc) => throw InternalCompilerException(s"Unexpected expression: '${exp0.getClass.getSimpleName}'.")
    case Expression.ApplyEffTail(sym, args, tpe, loc) => throw InternalCompilerException(s"Unexpected expression: '${exp0.getClass.getSimpleName}'.")
    case Expression.ApplySelfTail(sym, formals, actuals, tpe, loc) => throw InternalCompilerException(s"Unexpected expression: '${exp0.getClass.getSimpleName}'.")
  }

  /**
    * Returns the free variables in the given head predicate `head0`.
    */
  private def freeVars(head0: Predicate.Head): mutable.LinkedHashSet[(Symbol.VarSym, Type)] = head0 match {
    case Predicate.Head.Atom(pred, terms, tpe, loc) =>
      freeVars(pred.exp) ++ terms.flatMap(freeVars)
  }

  /**
    * Returns the free variables in the given body predicate `body0`.
    */
  private def freeVars(body0: Predicate.Body): mutable.LinkedHashSet[(Symbol.VarSym, Type)] = body0 match {
    case Predicate.Body.Atom(pred, polarity, terms, tpe, loc) =>
      freeVars(pred.exp) ++ terms.flatMap(freeVars)

    case Predicate.Body.Filter(sym, terms, loc) =>
      mutable.LinkedHashSet.empty ++ terms.flatMap(freeVars)

    case Predicate.Body.Functional(sym, term, loc) =>
      freeVars(term)
  }

  /**
    * Returns the free variables in the given head term `term0`.
    */
  private def freeVars(term0: Term.Head): mutable.LinkedHashSet[(Symbol.VarSym, Type)] = term0 match {
    case Term.Head.QuantVar(sym, tpe, loc) =>
      // Quantified variables are never free.
      mutable.LinkedHashSet.empty
    case Term.Head.CapturedVar(sym, tpe, loc) =>
      // Captured variables are by definition free.
      mutable.LinkedHashSet((sym, tpe))
    case Term.Head.Lit(lit, tpe, loc) => mutable.LinkedHashSet.empty
    case Term.Head.App(sym, args, tpe, loc) => mutable.LinkedHashSet.empty
  }

  /**
    * Returns the free variables in the given body term `term0`.
    */
  private def freeVars(term0: Term.Body): mutable.LinkedHashSet[(Symbol.VarSym, Type)] = term0 match {
    case Term.Body.Wild(tpe, loc) => mutable.LinkedHashSet.empty
    case Term.Body.QuantVar(sym, tpe, loc) =>
      // Quantified variables are never free.
      mutable.LinkedHashSet.empty
    case Term.Body.CapturedVar(sym, tpe, loc) =>
      // Captured variables are by definition free.
      mutable.LinkedHashSet((sym, tpe))
    case Term.Body.Lit(exp, tpe, loc) => freeVars(exp)
  }

  /**
    * Applies the given substitution map `subst` to the given expression `e`.
    */
  private def replace(e0: Expression, subst: Map[Symbol.VarSym, Symbol.VarSym])(implicit flix: Flix): Expression = {

    def visitExp(e: Expression): Expression = e match {
      case Expression.Unit => e
      case Expression.True => e
      case Expression.False => e
      case Expression.Char(lit) => e
      case Expression.Float32(lit) => e
      case Expression.Float64(lit) => e
      case Expression.Int8(lit) => e
      case Expression.Int16(lit) => e
      case Expression.Int32(lit) => e
      case Expression.Int64(lit) => e
      case Expression.BigInt(lit) => e
      case Expression.Str(lit) => e

      case Expression.Var(sym, tpe, loc) => subst.get(sym) match {
        case None => Expression.Var(sym, tpe, loc)
        case Some(newSym) => Expression.Var(newSym, tpe, loc)
      }

      case Expression.Def(sym, tpe, loc) => e

      case Expression.Eff(sym, tpe, loc) => e

      case Expression.Lambda(fparams, exp, tpe, loc) =>
        val fs = fparams.map(fparam => replace(fparam, subst))
        val e = visitExp(exp)
        Expression.Lambda(fs, e, tpe, loc)

      case Expression.Closure(ref, freeVars, tpe, loc) => e

      case Expression.LambdaClosure(fparams, freeVars, exp, tpe, loc) =>
        val e = visitExp(exp).asInstanceOf[Expression.Lambda]
        Expression.LambdaClosure(fparams, freeVars, exp, tpe, loc)

      case Expression.ApplyClo(exp, args, tpe, loc) =>
        val e = visitExp(exp)
        val as = args map visitExp
        Expression.ApplyClo(e, as, tpe, loc)

      case Expression.ApplyDef(sym, args, tpe, loc) =>
        val as = args map visitExp
        Expression.ApplyDef(sym, as, tpe, loc)

      case Expression.ApplyEff(sym, args, tpe, loc) =>
        val as = args map visitExp
        Expression.ApplyEff(sym, as, tpe, loc)

      case Expression.Apply(exp, args, tpe, loc) =>
        val e = visitExp(exp)
        val as = args map visitExp
        Expression.Apply(e, as, tpe, loc)

      case Expression.Unary(sop, op, exp, tpe, loc) =>
        val e = visitExp(exp)
        Expression.Unary(sop, op, e, tpe, loc)

      case Expression.Binary(sop, op, exp1, exp2, tpe, loc) =>
        val e1 = visitExp(exp1)
        val e2 = visitExp(exp2)
        Expression.Binary(sop, op, e1, e2, tpe, loc)

      case Expression.IfThenElse(exp1, exp2, exp3, tpe, loc) =>
        val e1 = visitExp(exp1)
        val e2 = visitExp(exp2)
        val e3 = visitExp(exp3)
        Expression.IfThenElse(e1, e2, e3, tpe, loc)

      case Expression.Branch(exp, branches, tpe, loc) =>
        val e = visitExp(exp)
        val bs = branches map {
          case (sym, br) => sym -> visitExp(br)
        }
        Expression.Branch(e, bs, tpe, loc)

      case Expression.JumpTo(sym, tpe, loc) =>
        Expression.JumpTo(sym, tpe, loc)

      case Expression.Let(sym, exp1, exp2, tpe, loc) =>
        val e1 = visitExp(exp1)
        val e2 = visitExp(exp2)
        subst.get(sym) match {
          case None => Expression.Let(sym, e1, e2, tpe, loc)
          case Some(newSym) => Expression.Let(newSym, e1, e2, tpe, loc)
        }

      case Expression.LetRec(sym, exp1, exp2, tpe, loc) =>
        val e1 = visitExp(exp1)
        val e2 = visitExp(exp2)
        subst.get(sym) match {
          case None => Expression.LetRec(sym, e1, e2, tpe, loc)
          case Some(newSym) => Expression.LetRec(newSym, e1, e2, tpe, loc)
        }

      case Expression.Is(sym, tag, exp, loc) =>
        val e = visitExp(exp)
        Expression.Is(sym, tag, e, loc)

      case Expression.Untag(sym, tag, exp, tpe, loc) =>
        val e = visitExp(exp)
        Expression.Untag(sym, tag, e, tpe, loc)

      case Expression.Tag(enum, tag, exp, tpe, loc) =>
        val e = visitExp(exp)
        Expression.Tag(enum, tag, e, tpe, loc)

      case Expression.Index(exp, offset, tpe, loc) =>
        val e = visitExp(exp)
        Expression.Index(e, offset, tpe, loc)

      case Expression.Tuple(elms, tpe, loc) =>
        val es = elms map visitExp
        Expression.Tuple(es, tpe, loc)

      case Expression.RecordEmpty(tpe, loc) =>
        Expression.RecordEmpty(tpe, loc)

      case Expression.RecordSelect(base, label, tpe, loc) =>
        val b = visitExp(base)
        Expression.RecordSelect(b, label, tpe, loc)

      case Expression.RecordExtend(label, value, rest, tpe, loc) =>
        val v = visitExp(value)
        val r = visitExp(rest)
        Expression.RecordExtend(label, v, r, tpe, loc)

      case Expression.RecordRestrict(label, rest, tpe, loc) =>
        val r = visitExp(rest)
        Expression.RecordRestrict(label, r, tpe, loc)

      case Expression.ArrayLit(elms, tpe, loc) =>
        val es = elms map visitExp
        Expression.ArrayLit(es, tpe, loc)

      case Expression.ArrayNew(elm, len, tpe, loc) =>
        val e = visitExp(elm)
        val ln = visitExp(len)
        Expression.ArrayNew(e, ln, tpe, loc)

      case Expression.ArrayLoad(base, index, tpe, loc) =>
        val b = visitExp(base)
        val i = visitExp(index)
        Expression.ArrayLoad(b, i, tpe, loc)

      case Expression.ArrayStore(base, index, elm, tpe, loc) =>
        val b = visitExp(base)
        val i = visitExp(index)
        val e = visitExp(elm)
        Expression.ArrayStore(b, i, e, tpe, loc)

      case Expression.ArrayLength(base, tpe, loc) =>
        val b = visitExp(base)
        Expression.ArrayLength(b, tpe, loc)

      case Expression.ArraySlice(base, beginIndex, endIndex, tpe, loc) =>
        val b = visitExp(base)
        val i1 = visitExp(beginIndex)
        val i2 = visitExp(endIndex)
        Expression.ArraySlice(b, i1, i2, tpe, loc)

      case Expression.Ref(exp, tpe, loc) =>
        val e = visitExp(exp)
        Expression.Ref(e, tpe, loc)

      case Expression.Deref(exp, tpe, loc) =>
        val e = visitExp(exp)
        Expression.Deref(e, tpe, loc)

      case Expression.Assign(exp1, exp2, tpe, loc) =>
        val e1 = visitExp(exp1)
        val e2 = visitExp(exp2)
        Expression.Assign(e1, e2, tpe, loc)

      case Expression.HandleWith(exp, bindings, tpe, loc) =>
        val e = visitExp(exp)
        val bs = bindings map {
          case HandlerBinding(sym, handler) => HandlerBinding(sym, visitExp(handler))
        }
        Expression.HandleWith(e, bs, tpe, loc)

      case Expression.Existential(fparam, exp, loc) =>
        val fs = replace(fparam, subst)
        val e = visitExp(exp)
        Expression.Existential(fs, e, loc)

      case Expression.Universal(fparam, exp, loc) =>
        val fs = replace(fparam, subst)
        val e = visitExp(exp)
        Expression.Universal(fs, e, loc)

      case Expression.TryCatch(exp, rules, tpe, loc) =>
        val e = visitExp(exp)
        val rs = rules map {
          case CatchRule(sym, clazz, body) =>
            val b = visitExp(body)
            CatchRule(sym, clazz, b)
        }
        Expression.TryCatch(e, rs, tpe, loc)

      case Expression.NativeConstructor(constructor, args, tpe, loc) =>
        val es = args map visitExp
        Expression.NativeConstructor(constructor, es, tpe, loc)

      case Expression.NativeField(field, tpe, loc) => e

      case Expression.NativeMethod(method, args, tpe, loc) =>
        val es = args map visitExp
        Expression.NativeMethod(method, es, tpe, loc)

      case Expression.NewChannel(exp, tpe, loc) =>
        val e = visitExp(exp)
        Expression.NewChannel(e, tpe, loc)

      case Expression.GetChannel(exp, tpe, loc) =>
        val e = visitExp(exp)
        Expression.GetChannel(e, tpe, loc)

      case Expression.PutChannel(exp1, exp2, eff, loc) =>
        val e1 = visitExp(exp1)
        val e2 = visitExp(exp2)
        Expression.PutChannel(e1, e2, eff, loc)

      case Expression.SelectChannel(rules, default, tpe, loc) =>
        val rs = rules map {
          case SelectChannelRule(sym, chan, exp) =>
            val c = visitExp(chan)
            val e = visitExp(exp)
            SelectChannelRule(sym, c, e)
        }

        val d = default.map(visitExp(_))

        Expression.SelectChannel(rs, d, tpe, loc)

      case Expression.Spawn(exp, tpe, loc) =>
        val e = visitExp(exp)
        Expression.Spawn(e, tpe, loc)

      case Expression.Sleep(exp, tpe, loc) =>
        val e = visitExp(exp)
        Expression.Sleep(e, tpe, loc)

      case Expression.FixpointConstraint(con, tpe, loc) =>
        val Constraint(cparams0, head0, body0) = con
        val cs = cparams0 map {
          case ConstraintParam.HeadParam(s, t, l) => ConstraintParam.HeadParam(subst.getOrElse(s, s), t, l)
          case ConstraintParam.RuleParam(s, t, l) => ConstraintParam.RuleParam(subst.getOrElse(s, s), t, l)
        }
        val head = visitHeadPredicate(head0)
        val body = body0 map visitBodyPredicate
        val c = Constraint(cs, head, body)
        Expression.FixpointConstraint(c, tpe, loc)

      case Expression.FixpointCompose(exp1, exp2, tpe, loc) =>
        val e1 = visitExp(exp1)
        val e2 = visitExp(exp2)
        Expression.FixpointCompose(e1, e2, tpe, loc)

      case Expression.FixpointSolve(exp, tpe, loc) =>
        val e = visitExp(exp)
        Expression.FixpointSolve(e, tpe, loc)

      case Expression.FixpointProject(pred, exp, tpe, loc) =>
        val p = visitPredicateWithParam(pred)
        val e = visitExp(exp)
        Expression.FixpointProject(p, e, tpe, loc)

      case Expression.FixpointEntails(exp1, exp2, tpe, loc) =>
        val e1 = visitExp(exp1)
        val e2 = visitExp(exp2)
        Expression.FixpointEntails(e1, e2, tpe, loc)

      case Expression.HoleError(sym, tpe, loc) => e
      case Expression.MatchError(tpe, loc) => e
      case Expression.SwitchError(tpe, loc) => e

      case Expression.ApplyCloTail(exp, args, tpe, loc) => throw InternalCompilerException(s"Unexpected expression: '${e.getClass.getSimpleName}'.")
      case Expression.ApplyDefTail(sym, args, tpe, loc) => throw InternalCompilerException(s"Unexpected expression: '${e.getClass.getSimpleName}'.")
      case Expression.ApplyEffTail(sym, args, tpe, loc) => throw InternalCompilerException(s"Unexpected expression: '${e.getClass.getSimpleName}'.")
      case Expression.ApplySelfTail(sym, fparams, args, tpe, loc) => throw InternalCompilerException(s"Unexpected expression: '${e.getClass.getSimpleName}'.")
    }

    def visitHeadPredicate(head0: Predicate.Head): Predicate.Head = head0 match {
      case Predicate.Head.Atom(pred, terms, tpe, loc) =>
        val p = visitPredicateWithParam(pred)
        val ts = terms map visitHeadTerm
        Predicate.Head.Atom(pred, ts, tpe, loc)
    }

    def visitBodyPredicate(body0: Predicate.Body): Predicate.Body = body0 match {
      case Predicate.Body.Atom(pred, polarity, terms, tpe, loc) =>
        val p = visitPredicateWithParam(pred)
        val ts = terms map visitBodyTerm
        Predicate.Body.Atom(p, polarity, ts, tpe, loc)

      case Predicate.Body.Filter(sym, terms, loc) =>
        val ts = terms map visitBodyTerm
        Predicate.Body.Filter(sym, ts, loc)

      case Predicate.Body.Functional(sym, term, loc) =>
        val s = subst.getOrElse(sym, sym)
        val t = visitHeadTerm(term)
        Predicate.Body.Functional(s, t, loc)
    }

    def visitHeadTerm(term0: Term.Head): Term.Head = term0 match {
      case Term.Head.QuantVar(sym, tpe, loc) =>
        val s = subst.getOrElse(sym, sym)
        Term.Head.QuantVar(s, tpe, loc)

      case Term.Head.CapturedVar(sym, tpe, loc) =>
        val s = subst.getOrElse(sym, sym)
        Term.Head.CapturedVar(s, tpe, loc)

      case Term.Head.Lit(lit, tpe, loc) => Term.Head.Lit(lit, tpe, loc)

      case Term.Head.App(sym, args, tpe, loc) =>
        val as = args.map(s => subst.getOrElse(s, s))
        Term.Head.App(sym, as, tpe, loc)
    }

    def visitBodyTerm(term0: Term.Body): Term.Body = term0 match {
      case Term.Body.Wild(tpe, loc) => Term.Body.Wild(tpe, loc)
      case Term.Body.QuantVar(sym, tpe, loc) =>
        val s = subst.getOrElse(sym, sym)
        Term.Body.QuantVar(sym, tpe, loc)

      case Term.Body.CapturedVar(sym, tpe, loc) =>
        val s = subst.getOrElse(sym, sym)
        Term.Body.CapturedVar(s, tpe, loc)

      case Term.Body.Lit(exp, tpe, loc) =>
        val e = visitExp(exp)
        Term.Body.Lit(e, tpe, loc)
    }

    visitExp(e0)
  }

  /**
    * Applies the given substitution map `subst` to the given formal parameters `fs`.
    */
  private def replace(fparam: FormalParam, subst: Map[Symbol.VarSym, Symbol.VarSym]): FormalParam = fparam match {
    case FormalParam(sym, mod, tpe, loc) =>
      subst.get(sym) match {
        case None => FormalParam(sym, mod, tpe, loc)
        case Some(newSym) => FormalParam(newSym, mod, tpe, loc)
      }
  }

}

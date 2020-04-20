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

package ca.uwaterloo.flix.language.ast.ops

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.SimplifiedAst._
import ca.uwaterloo.flix.language.ast.Symbol.{LabelSym, VarSym}
import ca.uwaterloo.flix.language.ast.{Symbol, Type}
import ca.uwaterloo.flix.util.InternalCompilerException

object SimplifiedAstOps {

  // TODO: Use this somewhere.

  /**
    * Checks the invariants of the given SimplifiedAst `root`.
    */
  def check(root: Root): Root = {

    /**
      * Checks invariants of the given definition `defn0`.
      */
    def checkDefn(defn0: Def): Unit = {
      for (param <- defn0.fparams) {
        checkFormalParam(param)
      }
      val env0 = defn0.fparams.map {
        case FormalParam(sym, mod, tpe, loc) => sym
      }
      checkExp(exp0 = defn0.exp, env0 = env0.toSet, ienv0 = Set.empty)
    }

    /**
      * Checks invariants of the given expression `exp0` with the local variables in `env0` defined and the labels in `ienv0` defined.
      */
    def checkExp(exp0: Expression, env0: Set[Symbol.VarSym], ienv0: Set[Symbol.LabelSym]): Unit = exp0 match {
      case Expression.Unit => // nop

      case Expression.True => // nop

      case Expression.False => // nop

      case Expression.Char(lit) => // nop

      case Expression.Float32(lit) => // nop

      case Expression.Float64(lit) => // nop

      case Expression.Int8(lit) => // nop

      case Expression.Int16(lit) => // nop

      case Expression.Int32(lit) => // nop

      case Expression.Int64(lit) => // nop

      case Expression.BigInt(lit) => // nop

      case Expression.Str(lit) => // nop

      case Expression.Var(sym, tpe, loc) =>
        assert(env0 contains sym, s"Undefined local variable symbol: '$sym'.")
        checkType(tpe)

      case Expression.Def(sym, tpe, loc) =>
        assert(root.defs contains sym, s"Undefined def symbol: '$sym'.")
        checkType(tpe)

      case Expression.Lambda(fparams, exp, tpe, loc) =>
        checkExp(exp, env0 ++ fparams.map(_.sym), ienv0)

      case Expression.Closure(sym, freeVars, tpe, loc) =>
        checkType(tpe)

      case Expression.Apply(exp, args, tpe, loc) =>
        checkExp(exp, env0, ienv0)
        for (arg <- args) {
          checkExp(arg, env0, ienv0)
        }
        checkType(tpe)

      case Expression.LambdaClosure(fparams, freeVars, exp, tpe, loc) =>
        checkExp(exp0, env0, ienv0)
        checkType(tpe)

      case Expression.ApplyClo(exp, args, tpe, loc) =>
        checkExp(exp, env0, ienv0)
        for (arg <- args) {
          checkExp(arg, env0, ienv0)
        }
        checkType(tpe)

      case Expression.ApplyDef(sym, args, tpe, loc) =>
        assert(root.defs contains sym, s"Undefined def symbol: '$sym'.")
        for (arg <- args) {
          checkExp(arg, env0, ienv0)
        }
        checkType(tpe)

      case Expression.ApplyCloTail(exp, args, tpe, loc) =>
        checkExp(exp, env0, ienv0)
        for (arg <- args) {
          checkExp(arg, env0, ienv0)
        }
        checkType(tpe)

      case Expression.ApplyDefTail(sym, args, tpe, loc) =>
        assert(root.defs contains sym, s"Undefined def symbol: '$sym'.")
        for (arg <- args) {
          checkExp(arg, env0, ienv0)
        }
        checkType(tpe)

      case Expression.ApplySelfTail(sym, formals, actuals, tpe, loc) =>
        assert(root.defs contains sym, s"Undefined definition symbol: '$sym'.")
        for (param <- formals) {
          checkFormalParam(param)
        }
        for (arg <- actuals) {
          checkExp(arg, env0, ienv0)
        }
        checkType(tpe)

      case Expression.Unary(sop, op, exp, tpe, loc) =>
        checkExp(exp, env0, ienv0)
        checkType(tpe)

      case Expression.Binary(sop, op, exp1, exp2, tpe, loc) =>
        checkExp(exp1, env0, ienv0)
        checkExp(exp2, env0, ienv0)
        checkType(tpe)

      case Expression.IfThenElse(exp1, exp2, exp3, tpe, loc) =>
        checkExp(exp1, env0, ienv0)
        checkExp(exp2, env0, ienv0)
        checkExp(exp3, env0, ienv0)
        checkType(tpe)

      case Expression.Branch(exp, branches, tpe, loc) =>
        checkExp(exp, env0, ienv0)
        for ((label, branch) <- branches) {
          checkExp(branch, env0, ienv0 ++ branches.keySet)
        }
        checkType(tpe)

      case Expression.JumpTo(sym, tpe, loc) =>
        assert(ienv0 contains sym, s"Undefined label symbol: '$sym'.")
        checkType(tpe)

      case Expression.Let(sym, exp1, exp2, tpe, loc) =>
        checkExp(exp1, env0, ienv0)
        checkExp(exp2, env0 + sym, ienv0)
        checkType(tpe)

      case Expression.LetRec(sym, exp1, exp2, tpe, loc) =>
        checkExp(exp1, env0 + sym, ienv0)
        checkExp(exp2, env0 + sym, ienv0)
        checkType(tpe)

      case Expression.Is(sym, tag, exp, loc) =>
        assert(root.enums contains sym, s"Undefined enum symbol: '$sym'.")
        checkExp(exp, env0, ienv0)

      case Expression.Tag(sym, tag, exp, tpe, loc) =>
        assert(root.enums contains sym, s"Undefined enum symbol: '$sym'.")
        checkExp(exp, env0, ienv0)
        checkType(tpe)

      case Expression.Untag(sym, tag, exp, tpe, loc) =>
        assert(root.enums contains sym, s"Undefined enum symbol: '$sym'.")
        checkExp(exp, env0, ienv0)
        checkType(tpe)

      case Expression.Index(base, offset, tpe, loc) =>
        checkExp(base, env0, ienv0)
        checkType(tpe)

      case Expression.Tuple(elms, tpe, loc) =>
        for (elm <- elms) {
          checkExp(elm, env0, ienv0)
        }
        checkType(tpe)

      case Expression.RecordEmpty(tpe, loc) =>
        checkType(tpe)

      case Expression.RecordSelect(base, label, tpe, loc) =>
        checkExp(base, env0, ienv0)
        checkType(tpe)

      case Expression.RecordExtend(label, value, rest, tpe, loc) =>
        checkExp(value, env0, ienv0)
        checkExp(rest, env0, ienv0)
        checkType(tpe)

      case Expression.RecordRestrict(label, rest, tpe, loc) =>
        checkExp(rest, env0, ienv0)
        checkType(tpe)

      case Expression.ArrayLit(elms, tpe, loc) =>
        for (elm <- elms) {
          checkExp(elm, env0, ienv0)
        }
        checkType(tpe)

      case Expression.ArrayNew(elm, len, tpe, loc) =>
        checkExp(elm, env0, ienv0)
        checkExp(len, env0, ienv0)
        checkType(tpe)

      case Expression.ArrayLoad(base, index, tpe, loc) =>
        checkExp(base, env0, ienv0)
        checkExp(index, env0, ienv0)
        checkType(tpe)

      case Expression.ArrayStore(base, index, elm, tpe, loc) =>
        checkExp(base, env0, ienv0)
        checkExp(index, env0, ienv0)
        checkExp(elm, env0, ienv0)
        checkType(tpe)

      case Expression.ArrayLength(base, tpe, loc) =>
        checkExp(base, env0, ienv0)
        checkType(tpe)

      case Expression.ArraySlice(base, beginIndex, endIndex, tpe, loc) =>
        checkExp(base, env0, ienv0)
        checkExp(beginIndex, env0, ienv0)
        checkExp(endIndex, env0, ienv0)
        checkType(tpe)

      case Expression.Ref(exp, tpe, loc) =>
        checkExp(exp, env0, ienv0)
        checkType(tpe)

      case Expression.Deref(exp, tpe, loc) =>
        checkExp(exp, env0, ienv0)
        checkType(tpe)

      case Expression.Assign(exp1, exp2, tpe, loc) =>
        checkExp(exp1, env0, ienv0)
        checkExp(exp2, env0, ienv0)
        checkType(tpe)

      case Expression.Existential(fparam, exp, loc) =>
        checkFormalParam(fparam)
        checkExp(exp, env0 + fparam.sym, ienv0)

      case Expression.Universal(fparam, exp, loc) =>
        checkFormalParam(fparam)
        checkExp(exp, env0 + fparam.sym, ienv0)

      case Expression.Cast(exp, tpe, loc) =>
        checkExp(exp, env0, ienv0)
        checkType(tpe)

      case Expression.TryCatch(exp, rules, tpe, loc) =>
        checkExp(exp, env0, ienv0)
        for (CatchRule(sym, clazz, body) <- rules) {
          checkExp(body, env0 + sym, ienv0)
        }
        checkType(tpe)

      case Expression.InvokeConstructor(constructor, args, tpe, loc) =>
        for (arg <- args) {
          checkExp(arg, env0, ienv0)
        }
        checkType(tpe)

      case Expression.InvokeMethod(method, exp, args, tpe, loc) =>
        checkExp(exp, env0, ienv0)
        for (arg <- args) {
          checkExp(arg, env0, ienv0)
        }
        checkType(tpe)

      case Expression.InvokeStaticMethod(method, args, tpe, loc) =>
        for (arg <- args) {
          checkExp(arg, env0, ienv0)
        }
        checkType(tpe)

      case Expression.GetField(field, exp, tpe, loc) =>
        checkExp(exp, env0, ienv0)
        checkType(tpe)

      case Expression.PutField(field, exp1, exp2, tpe, loc) =>
        checkExp(exp1, env0, ienv0)
        checkExp(exp2, env0, ienv0)
        checkType(tpe)

      case Expression.GetStaticField(field, tpe, loc) =>
        checkType(tpe)

      case Expression.PutStaticField(field, exp, tpe, loc) =>
        checkExp(exp, env0, ienv0)
        checkType(tpe)

      case Expression.NewChannel(exp, tpe, loc) =>
        checkExp(exp, env0, ienv0)
        checkType(tpe)

      case Expression.GetChannel(exp, tpe, loc) =>
        checkExp(exp, env0, ienv0)
        checkType(tpe)

      case Expression.PutChannel(exp1, exp2, tpe, loc) =>
        checkExp(exp1, env0, ienv0)
        checkExp(exp2, env0, ienv0)
        checkType(tpe)

      case Expression.SelectChannel(rules, default, tpe, loc) =>
        for (rule <- rules) {
          checkExp(rule.chan, env0, ienv0)
          checkExp(rule.exp, env0, ienv0)
        }
        default.foreach(exp => checkExp(exp, env0, ienv0))
        checkType(tpe)

      case Expression.ProcessSpawn(exp, tpe, loc) =>
        checkExp(exp, env0, ienv0)
        checkType(tpe)

      case Expression.ProcessPanic(msg, tpe, loc) =>
        checkType(tpe)

      case Expression.FixpointConstraintSet(_, tpe, _) =>
        checkType(tpe)

      case Expression.FixpointCompose(exp1, exp2, tpe, loc) =>
        checkExp(exp1, env0, ienv0)
        checkExp(exp2, env0, ienv0)
        checkType(tpe)

      case Expression.FixpointSolve(exp, stf, tpe, loc) =>
        checkExp(exp, env0, ienv0)
        checkType(tpe)

      case Expression.FixpointProject(name, exp, tpe, loc) =>
        checkExp(exp, env0, ienv0)
        checkType(tpe)

      case Expression.FixpointEntails(exp1, exp2, tpe, loc) =>
        checkExp(exp1, env0, ienv0)
        checkExp(exp2, env0, ienv0)
        checkType(tpe)

      case Expression.FixpointFold(name, exp1, exp2, exp3, tpe, loc) =>
        checkExp(exp1, env0, ienv0)
        checkExp(exp2, env0, ienv0)
        checkExp(exp3, env0, ienv0)
        checkType(tpe)

      case Expression.HoleError(sym, tpe, loc) =>
        checkType(tpe)

      case Expression.MatchError(tpe, loc) =>
        checkType(tpe)
    }

    /**
      * Checks invariants of the given constraint `c0`.
      */
    def checkConstraint(c0: Constraint, env0: Set[Symbol.VarSym], ienv0: Set[Symbol.LabelSym]): Unit = {
      for (param <- c0.cparams) {
        checkConstraintParam(param)
      }
      val envHead = c0.cparams.collect {
        case ConstraintParam.HeadParam(sym, tpe, loc) => sym
      }
      val ruleEnv = c0.cparams.map {
        case ConstraintParam.HeadParam(sym, tpe, loc) => sym
        case ConstraintParam.RuleParam(sym, tpe, loc) => sym
      }
      checkHeadPred(c0.head, envHead.toSet, ienv0)
      for (bodyPred <- c0.body) {
        checkBodyPred(bodyPred, ruleEnv.toSet, ienv0)
      }
    }

    /**
      * Checks invariants of the given constraint parameter `p0`.
      */
    def checkConstraintParam(p0: ConstraintParam): Unit = p0 match {
      case ConstraintParam.HeadParam(sym, tpe, loc) =>
        checkType(tpe)
      case ConstraintParam.RuleParam(sym, tpe, loc) =>
        checkType(tpe)
    }

    /**
      * Checks invariants of the given head predicate `h0`.
      */
    def checkHeadPred(h0: Predicate.Head, env0: Set[Symbol.VarSym], ienv0: Set[Symbol.LabelSym]): Unit = h0 match {
      case Predicate.Head.Atom(name, den, terms, tpe, loc) =>
        for (term <- terms) {
          checkHeadTerm(term, env0)
        }
        checkType(tpe)

      case Predicate.Head.Union(exp, tpe, loc) =>
        checkExp(exp, env0, ienv0)
        checkType(tpe)

    }

    /**
      * Checks invariants of the given body predicate `b0`.
      */
    def checkBodyPred(b0: Predicate.Body, env0: Set[Symbol.VarSym], ienv0: Set[Symbol.LabelSym]): Unit = b0 match {
      case Predicate.Body.Atom(name, den, polarity, terms, tpe, loc) =>
        for (term <- terms) {
          checkBodyTerm(term, env0)
        }
        checkType(tpe)

      case Predicate.Body.Guard(exp, loc) =>
        checkExp(exp, env0, ienv0)
    }

    /**
      * Checks invariants of the given head term `t0`.
      */
    def checkHeadTerm(t0: Term.Head, env0: Set[Symbol.VarSym]): Unit = t0 match {
      case Term.Head.QuantVar(sym, tpe, loc) =>
        checkType(tpe)
      case Term.Head.CapturedVar(sym, tpe, loc) =>
        checkType(tpe)
      case Term.Head.Lit(exp, tpe, loc) =>
        checkExp(exp0 = exp, env0 = env0, ienv0 = Set.empty)
        checkType(tpe)
      case Term.Head.App(exp, args, tpe, loc) =>
        checkExp(exp0 = exp, env0 = env0, ienv0 = Set.empty)
        checkType(tpe)
    }

    /**
      * Checks invariants of the given body term `t0`.
      */
    def checkBodyTerm(t0: Term.Body, env0: Set[Symbol.VarSym]): Unit = t0 match {
      case Term.Body.Wild(tpe, loc) =>
      // TODO: What is the type allowed to be here?
      case Term.Body.QuantVar(sym, tpe, loc) =>
        checkType(tpe)
      case Term.Body.CapturedVar(sym, tpe, loc) =>
        checkType(tpe)
      case Term.Body.Lit(exp, tpe, loc) =>
        checkExp(exp0 = exp, env0 = env0, ienv0 = Set.empty)
        checkType(tpe)
    }

    /**
      * Checks invariants of the given attribute `a0`.
      */
    def checkAttribute(a0: Attribute): Unit = {
      checkType(a0.tpe)
    }

    /**
      * Checks invariants of the given formal parameter `p0`.
      */
    def checkFormalParam(p0: FormalParam): Unit = {
      checkType(p0.tpe)
    }

    /**
      * Checks invariants of the given type `tpe0`.
      */
    def checkType(tpe0: Type): Unit = tpe0 match {
      case Type.Var(id, kind) =>
        assert(assertion = false, "Unexpected type variable.")
      case Type.Apply(tpe1, tpe2) =>
        checkType(tpe1)
        checkType(tpe2)
      case _ => // OK
    }

    //
    // Check all definitions in the program.
    //
    for ((sym, defn) <- root.defs) {
      checkDefn(defn)
    }

    //
    // Check all lattices in the program.
    //
    for ((tpe1, LatticeOps(tpe2, bot, top, equ, leq, lub, glb, loc)) <- root.latticeOps) {
      assert(tpe1 == tpe2)
      checkType(tpe1)
      checkType(tpe2)
    }

    //
    // Check all properties in the program.
    //
    for (Property(law, defn, exp) <- root.properties) {
      checkExp(exp0 = exp, env0 = Set.empty, ienv0 = Set.empty)
    }

    // Success :)
    root
  }

  /**
   * Make fresh names for all variables bound in `exp0`.
   * @param vars0 Free variables in `exp0` will be looked up in `vars0`.
   * @return An Expression that have no bound variables in common with `exp0`.
   */
  def renameBoundVars(exp0: Expression, vars0: Map[VarSym, VarSym])(implicit flix: Flix): Expression = {
    def visitExp(exp0: Expression, vars0: Map[VarSym, VarSym], labels0: Map[LabelSym, LabelSym]): Expression =
      exp0 match {
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

        case Expression.Def(sym, tpe, loc) => Expression.Def(sym, tpe, loc)

        case Expression.Var(sym, tpe, loc) =>
          val s = vars0.getOrElse(sym, sym) // sym might not have been renamed e.g. if it's a free variable.
          Expression.Var(s, tpe, loc)

        case Expression.Lambda(args0, body, tpe, loc) =>
          val args1 = args0.map(fparam => fparam.copy(sym = Symbol.freshVarSym(fparam.sym)))
          val vars1 = args0.zip(args1).map {
            case (oldParam, newParam) => oldParam.sym -> newParam.sym
          }
          val b = visitExp(body, vars1.toMap ++ vars0, labels0)
          Expression.Lambda(args1, b, tpe, loc)

        case Expression.Apply(exp, args, tpe, loc) =>
          val e = visitExp(exp, vars0, labels0)
          val as = args.map(visitExp(_, vars0, labels0))
          Expression.Apply(e, as, tpe, loc)

        case Expression.Unary(sop, op, exp, tpe, loc) =>
          val e = visitExp(exp, vars0, labels0)
          Expression.Unary(sop, op, e, tpe, loc)

        case Expression.Binary(sop, op, exp1, exp2, tpe, loc) =>
          val e1 = visitExp(exp1, vars0, labels0)
          val e2 = visitExp(exp2, vars0, labels0)
          Expression.Binary(sop, op, e1, e2, tpe, loc)

        case Expression.IfThenElse(exp1, exp2, exp3, tpe, loc) =>
          val cond = visitExp(exp1, vars0, labels0)
          val consequent = visitExp(exp2, vars0, labels0)
          val alternative = visitExp(exp3, vars0, labels0)
          Expression.IfThenElse(cond, consequent, alternative, tpe, loc)

        case Expression.Branch(exp, branches, tpe, loc) =>
          // Need to rename all cases before we rename their bodies
          // because they can refer to each other.
          val labels1 = branches.foldLeft(labels0) {
            case (acc, (sym0, br)) =>
              val sym1 = Symbol.freshLabel(sym0)
              acc + (sym0 -> sym1)
          }
          val bs = branches map {
            case (sym, br) =>
              val b = visitExp(br, vars0, labels1)
              labels1(sym) -> b
          }
          val e = visitExp(exp, vars0, labels1)
          Expression.Branch(e, bs, tpe, loc)

        case Expression.JumpTo(sym, tpe, loc) =>
          val s = labels0(sym) // A JumpTo is always inside a Branch hence its label is guaranteed to exist.
          Expression.JumpTo(s, tpe, loc)

        case Expression.Let(sym, exp1, exp2, tpe, loc) =>
          val s = Symbol.freshVarSym(sym)
          val vars1 = vars0 + (sym -> s)
          val e1 = visitExp(exp1, vars0, labels0)
          val e2 = visitExp(exp2, vars1, labels0)
          Expression.Let(s, e1, e2, tpe, loc)

        case Expression.LetRec(sym, exp1, exp2, tpe, loc) =>
          val s = Symbol.freshVarSym(sym)
          val vars1 = vars0 + (sym -> s)
          val e1 = visitExp(exp1, vars1, labels0)
          val e2 = visitExp(exp2, vars1, labels0)
          Expression.LetRec(s, e1, e2, tpe, loc)

        case Expression.Is(sym, tag, exp, loc) =>
          val e = visitExp(exp, vars0, labels0)
          Expression.Is(sym, tag, e, loc)

        case Expression.Tag(sym, tag, exp, tpe, loc) =>
          val e = visitExp(exp, vars0, labels0)
          Expression.Tag(sym, tag, e, tpe, loc)

        case Expression.Untag(sym, tag, exp, tpe, loc) =>
          val e = visitExp(exp, vars0, labels0)
          Expression.Untag(sym, tag, e, tpe, loc)

        case Expression.Index(base, offset, tpe, loc) =>
          val b = visitExp(base, vars0, labels0)
          Expression.Index(b, offset, tpe, loc)

        case Expression.Tuple(elms, tpe, loc) =>
          val es = elms.map(visitExp(_, vars0, labels0))
          Expression.Tuple(es, tpe, loc)

        case Expression.RecordEmpty(tpe, loc) => Expression.RecordEmpty(tpe, loc)

        case Expression.RecordSelect(exp, label, tpe, loc) =>
          val e = visitExp(exp, vars0, labels0)
          Expression.RecordSelect(e, label, tpe, loc)

        case Expression.RecordExtend(label, value, rest, tpe, loc) =>
          val v = visitExp(value, vars0, labels0)
          val r = visitExp(rest, vars0, labels0)
          Expression.RecordExtend(label, v, r, tpe, loc)

        case Expression.RecordRestrict(label, rest, tpe, loc) =>
          val r = visitExp(rest, vars0, labels0)
          Expression.RecordRestrict(label, r, tpe, loc)

        case Expression.ArrayLit(elms, tpe, loc) =>
          val es = elms.map(visitExp(_, vars0, labels0))
          Expression.ArrayLit(es, tpe, loc)

        case Expression.ArrayNew(elm, len, tpe, loc) =>
          val e = visitExp(elm, vars0, labels0)
          val ln = visitExp(len, vars0, labels0)
          Expression.ArrayNew(e, ln, tpe, loc)

        case Expression.ArrayLoad(base, index, tpe, loc) =>
          val b = visitExp(base, vars0, labels0)
          val i = visitExp(index, vars0, labels0)
          Expression.ArrayLoad(b, i, tpe, loc)

        case Expression.ArrayStore(base, index, elm, tpe, loc) =>
          val b = visitExp(base, vars0, labels0)
          val i = visitExp(index, vars0, labels0)
          val e = visitExp(elm, vars0, labels0)
          Expression.ArrayStore(b, i, e, tpe, loc)

        case Expression.ArrayLength(base, tpe, loc) =>
          val b = visitExp(base, vars0, labels0)
          Expression.ArrayLength(b, tpe, loc)

        case Expression.ArraySlice(base, startIndex, endIndex, tpe, loc) =>
          val b = visitExp(base, vars0, labels0)
          val i1 = visitExp(startIndex, vars0, labels0)
          val i2 = visitExp(endIndex, vars0, labels0)
          Expression.ArraySlice(b, i1, i2, tpe, loc)

        case Expression.Ref(exp, tpe, loc) =>
          val e = visitExp(exp, vars0, labels0)
          Expression.Ref(e, tpe, loc)

        case Expression.Deref(exp, tpe, loc) =>
          val e = visitExp(exp, vars0, labels0)
          Expression.Deref(e, tpe, loc)

        case Expression.Assign(exp1, exp2, tpe, loc) =>
          val e1 = visitExp(exp1, vars0, labels0)
          val e2 = visitExp(exp2, vars0, labels0)
          Expression.Assign(e1, e2, tpe, loc)

        case Expression.Existential(fparam0, exp, loc) =>
          val fparam1 = fparam0.copy(sym = Symbol.freshVarSym(fparam0.sym))
          val vars1 = vars0 + (fparam0.sym -> fparam1.sym)
          val e = visitExp(exp, vars1, labels0)
          Expression.Existential(fparam1, e, loc)

        case Expression.Universal(fparam0, exp, loc) =>
          val fparam1 = fparam0.copy(sym = Symbol.freshVarSym(fparam0.sym))
          val vars1 = vars0 + (fparam0.sym -> fparam1.sym)
          val e = visitExp(exp, vars1, labels0)
          Expression.Universal(fparam1, e, loc)

        case Expression.Cast(exp, tpe, loc) =>
          val e = visitExp(exp, vars0, labels0)
          Expression.Cast(e, tpe, loc)

        case Expression.TryCatch(exp, rules, tpe, loc) =>
          val e = visitExp(exp, vars0, labels0)
          val rs = rules map {
            case CatchRule(sym, clazz, body) =>
              val s = Symbol.freshVarSym(sym)
              val vars1 = vars0 + (sym -> s)
              val b = visitExp(body, vars1, labels0)
              CatchRule(s, clazz, b)
          }
          Expression.TryCatch(e, rs, tpe, loc)

        case Expression.InvokeConstructor(constructor, args, tpe, loc) =>
          val as = args.map(visitExp(_, vars0, labels0))
          Expression.InvokeConstructor(constructor, as, tpe, loc)

        case Expression.InvokeMethod(method, exp, args, tpe, loc) =>
          val e = visitExp(exp, vars0, labels0)
          val as = args.map(visitExp(_, vars0, labels0))
          Expression.InvokeMethod(method, e, as, tpe, loc)

        case Expression.InvokeStaticMethod(method, args, tpe, loc) =>
          val as = args.map(visitExp(_, vars0, labels0))
          Expression.InvokeStaticMethod(method, as, tpe, loc)

        case Expression.GetField(field, exp, tpe, loc) =>
          val e = visitExp(exp, vars0, labels0)
          Expression.GetField(field, e, tpe, loc)

        case Expression.PutField(field, exp1, exp2, tpe, loc) =>
          val e1 = visitExp(exp1, vars0, labels0)
          val e2 = visitExp(exp2, vars0, labels0)
          Expression.PutField(field, e1, e2, tpe, loc)

        case Expression.GetStaticField(field, tpe, loc) => Expression.GetStaticField(field, tpe, loc)

        case Expression.PutStaticField(field, exp, tpe, loc) =>
          val e = visitExp(exp, vars0, labels0)
          Expression.PutStaticField(field, e, tpe, loc)

        case Expression.NewChannel(exp, tpe, loc) =>
          val e = visitExp(exp, vars0, labels0)
          Expression.NewChannel(e, tpe, loc)

        case Expression.GetChannel(exp, tpe, loc) =>
          val e = visitExp(exp, vars0, labels0)
          Expression.GetChannel(e, tpe, loc)

        case Expression.PutChannel(exp1, exp2, tpe, loc) =>
          val e1 = visitExp(exp1, vars0, labels0)
          val e2 = visitExp(exp2, vars0, labels0)
          Expression.PutChannel(e1, e2, tpe, loc)

        case Expression.SelectChannel(rules, default, tpe, loc) =>
          val rs = rules map {
            case SelectChannelRule(sym, chan, exp) =>
              val s = Symbol.freshVarSym(sym)
              val vars1 = vars0 + (sym -> s)
              val c = visitExp(chan, vars0, labels0)
              val e = visitExp(exp, vars1, labels0)
              SelectChannelRule(s, c, e)
          }
          val d = default.map(visitExp(_, vars0, labels0))
          Expression.SelectChannel(rs, d, tpe, loc)

        case Expression.ProcessSpawn(exp, tpe, loc) =>
          val e = visitExp(exp, vars0, labels0)
          Expression.ProcessSpawn(e, tpe, loc)

        case Expression.ProcessPanic(msg, tpe, loc) => Expression.ProcessPanic(msg, tpe, loc)

        case Expression.FixpointConstraintSet(cs0, tpe, loc) => Expression.FixpointConstraintSet(cs0, tpe, loc)

        case Expression.FixpointCompose(exp1, exp2, tpe, loc) =>
          val e1 = visitExp(exp1, vars0, labels0)
          val e2 = visitExp(exp2, vars0, labels0)
          Expression.FixpointCompose(e1, e2, tpe, loc)

        case Expression.FixpointSolve(exp, stf, tpe, loc) =>
          val e = visitExp(exp, vars0, labels0)
          Expression.FixpointSolve(e, stf, tpe, loc)

        case Expression.FixpointProject(sym, exp, tpe, loc) =>
          val e = visitExp(exp, vars0, labels0)
          Expression.FixpointProject(sym, e, tpe, loc)

        case Expression.FixpointEntails(exp1, exp2, tpe, loc) =>
          val e1 = visitExp(exp1, vars0, labels0)
          val e2 = visitExp(exp2, vars0, labels0)
          Expression.FixpointEntails(e1, e2, tpe, loc)

        case Expression.FixpointFold(sym, exp1, exp2, exp3, tpe, loc) =>
          val e1 = visitExp(exp1, vars0, labels0)
          val e2 = visitExp(exp2, vars0, labels0)
          val e3 = visitExp(exp3, vars0, labels0)
          Expression.FixpointFold(sym, e1, e2, e3, tpe, loc)

        case Expression.HoleError(sym, tpe, loc) => Expression.HoleError(sym, tpe, loc)
        case Expression.MatchError(tpe, loc) => Expression.MatchError(tpe, loc)

        case Expression.LambdaClosure(fparams, freeVars, exp, tpe, loc) => throw InternalCompilerException(s"Unexpected expression: '${exp0.getClass}'.")
        case Expression.Closure(sym, freeVars, tpe, loc) => throw InternalCompilerException(s"Unexpected expression: '${exp0.getClass}'.")
        case Expression.ApplyClo(exp, args, tpe, loc) => throw InternalCompilerException(s"Unexpected expression: '${exp0.getClass}'.")
        case Expression.ApplyDef(sym, args, tpe, loc) => throw InternalCompilerException(s"Unexpected expression: '${exp0.getClass}'.")
        case Expression.ApplyCloTail(exp, args, tpe, loc) => throw InternalCompilerException(s"Unexpected expression: '${exp0.getClass}'.")
        case Expression.ApplyDefTail(sym, args, tpe, loc) => throw InternalCompilerException(s"Unexpected expression: '${exp0.getClass}'.")
        case Expression.ApplySelfTail(sym, formals, actuals, tpe, loc) => throw InternalCompilerException(s"Unexpected expression: '${exp0.getClass}'.")
      }

    visitExp(exp0, vars0, Map.empty)
  }
}

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

import ca.uwaterloo.flix.language.ast.SimplifiedAst._
import ca.uwaterloo.flix.language.ast.{Symbol, Type}
import ca.uwaterloo.flix.util.InternalCompilerException

import scala.collection.mutable

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

      case Expression.FixpointProject(sym, exp, tpe, loc) =>
        checkExp(exp, env0, ienv0)
        checkType(tpe)

      case Expression.FixpointEntails(exp1, exp2, tpe, loc) =>
        checkExp(exp1, env0, ienv0)
        checkExp(exp2, env0, ienv0)
        checkType(tpe)

      case Expression.FixpointFold(sym, exp1, exp2, exp3, tpe, loc) =>
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
      case Predicate.Head.Atom(sym, den, terms, tpe, loc) =>
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
      case Predicate.Body.Atom(sym, den, polarity, terms, tpe, loc) =>
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
    for ((tpe1, LatticeComponents(tpe2, bot, top, equ, leq, lub, glb, loc)) <- root.latticeComponents) {
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

    //
    // Check all relations in the program.
    //
    for ((sym1, Relation(_, sym2, attr, _)) <- root.relations) {
      assert(sym1 == sym2)
      for (attribute <- attr) {
        checkAttribute(attribute)
      }
    }

    //
    // Check all lattices in the program.
    //
    for ((sym1, Lattice(_, sym2, attr, _)) <- root.lattices) {
      assert(sym1 == sym2)
      for (attribute <- attr) {
        checkAttribute(attribute)
      }
    }

    // Success :)
    root
  }

  /**
   * Returns the free variables in the given expression `exp`.
   * Does a left-to-right traversal of the AST, collecting free variables in order, in a LinkedHashSet.
   */
  // TODO: Use immutable, but sorted data structure?
  def freeVars(exp0: Expression): mutable.LinkedHashSet[(Symbol.VarSym, Type)] = exp0 match {
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
        case SelectChannelRule(sym, chan, exp) => (freeVars(chan) ++ freeVars(exp)).filter(p => p._1 != sym)
      }

      val d = default.map(freeVars).getOrElse(mutable.LinkedHashSet.empty)

      rs ++ d

    case Expression.ProcessSpawn(exp, tpe, loc) => freeVars(exp)

    case Expression.ProcessSleep(exp, tpe, loc) => freeVars(exp)

    case Expression.ProcessPanic(msg, tpe, loc) => mutable.LinkedHashSet.empty

    case Expression.FixpointConstraintSet(cs, tpe, loc) =>
      cs.foldLeft(mutable.LinkedHashSet.empty[(Symbol.VarSym, Type)]) {
        case (m, c) => m ++ freeVars(c)
      }

    case Expression.FixpointCompose(exp1, exp2, tpe, loc) => freeVars(exp1) ++ freeVars(exp2)
    case Expression.FixpointSolve(exp, stf, tpe, loc) => freeVars(exp)
    case Expression.FixpointProject(sym, exp, tpe, loc) => freeVars(exp)
    case Expression.FixpointEntails(exp1, exp2, tpe, loc) => freeVars(exp1) ++ freeVars(exp2)
    case Expression.FixpointFold(sym, exp1, exp2, exp3, tpe, loc) => freeVars(exp1) ++ freeVars(exp2) ++ freeVars(exp3)

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
   * Returns the free variables in the given constraint `c0`.
   */
  private def freeVars(c0: Constraint): mutable.LinkedHashSet[(Symbol.VarSym, Type)] = {
    val Constraint(cparams, head, body) = c0
    freeVars(head) ++ body.flatMap(freeVars)
  }

  /**
   * Returns the free variables in the given head predicate `head0`.
   */
  private def freeVars(head0: Predicate.Head): mutable.LinkedHashSet[(Symbol.VarSym, Type)] = head0 match {
    case Predicate.Head.Atom(sym, den, terms, tpe, loc) =>
      mutable.LinkedHashSet.empty ++ terms.flatMap(freeVars)

    case Predicate.Head.Union(exp, tpe, loc) =>
      freeVars(exp)
  }

  /**
   * Returns the free variables in the given body predicate `body0`.
   */
  private def freeVars(body0: Predicate.Body): mutable.LinkedHashSet[(Symbol.VarSym, Type)] = body0 match {
    case Predicate.Body.Atom(sym, den, polarity, terms, tpe, loc) =>
      mutable.LinkedHashSet.empty ++ terms.flatMap(freeVars)

    case Predicate.Body.Guard(exp, loc) =>
      freeVars(exp)
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
    case Term.Head.App(exp, args, tpe, loc) => freeVars(exp)
  }
}

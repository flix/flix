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
      //
      // Literal Expressions.
      //
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

      //
      // Variable Expressions.
      //
      case Expression.Var(sym, tpe, loc) =>
        assert(env0 contains sym, s"Undefined local variable symbol: '$sym'.")
        checkType(tpe)

      //
      // Def Expressions.
      //
      case Expression.Def(sym, tpe, loc) =>
        assert(root.defs contains sym, s"Undefined def symbol: '$sym'.")
        checkType(tpe)

      //
      // Eff Expressions.
      //
      case Expression.Eff(sym, tpe, loc) =>
        assert(root.effs contains sym, s"Undefined effect symbol: '$sym'.")
        checkType(tpe)

      //
      // Lambda Expressions.
      //
      case Expression.Lambda(fparams, exp, tpe, loc) =>
        checkExp(exp, env0 ++ fparams.map(_.sym), ienv0)

      //
      // Closure Expressions.
      //
      case Expression.Closure(sym, freeVars, tpe, loc) =>
        checkType(tpe)

      //
      // Apply Expressions.
      //
      case Expression.Apply(exp, args, tpe, loc) =>
        checkExp(exp, env0, ienv0)
        for (arg <- args) {
          checkExp(arg, env0, ienv0)
        }
        checkType(tpe)

      //
      // LambdaClosure.
      //
      case Expression.LambdaClosure(fparams, freeVars, exp, tpe, loc) =>
        checkExp(exp0, env0, ienv0)
        checkType(tpe)

      //
      // ApplyClo Expressions.
      //
      case Expression.ApplyClo(exp, args, tpe, loc) =>
        checkExp(exp, env0, ienv0)
        for (arg <- args) {
          checkExp(arg, env0, ienv0)
        }
        checkType(tpe)

      //
      // ApplyDef Expressions.
      //
      case Expression.ApplyDef(sym, args, tpe, loc) =>
        assert(root.defs contains sym, s"Undefined def symbol: '$sym'.")
        for (arg <- args) {
          checkExp(arg, env0, ienv0)
        }
        checkType(tpe)

      //
      // ApplyEff Expressions.
      //
      case Expression.ApplyEff(sym, args, tpe, loc) =>
        assert(root.effs contains sym, s"Undefined eff symbol: '$sym'.")
        for (arg <- args) {
          checkExp(arg, env0, ienv0)
        }
        checkType(tpe)

      //
      // ApplyCloTail Expressions.
      //
      case Expression.ApplyCloTail(exp, args, tpe, loc) =>
        checkExp(exp, env0, ienv0)
        for (arg <- args) {
          checkExp(arg, env0, ienv0)
        }
        checkType(tpe)

      //
      // ApplyDefTail Expressions.
      //
      case Expression.ApplyDefTail(sym, args, tpe, loc) =>
        assert(root.defs contains sym, s"Undefined def symbol: '$sym'.")
        for (arg <- args) {
          checkExp(arg, env0, ienv0)
        }
        checkType(tpe)

      //
      // ApplyEffTail Expressions.
      //
      case Expression.ApplyEffTail(sym, args, tpe, loc) =>
        assert(root.effs contains sym, s"Undefined eff symbol: '$sym'.")
        for (arg <- args) {
          checkExp(arg, env0, ienv0)
        }
        checkType(tpe)

      //
      // ApplySelfTail Expressions.
      //
      case Expression.ApplySelfTail(sym, formals, actuals, tpe, loc) =>
        assert(root.defs contains sym, s"Undefined definition symbol: '$sym'.")
        for (param <- formals) {
          checkFormalParam(param)
        }
        for (arg <- actuals) {
          checkExp(arg, env0, ienv0)
        }
        checkType(tpe)

      //
      // Unary Expressions.
      //
      case Expression.Unary(sop, op, exp, tpe, loc) =>
        checkExp(exp, env0, ienv0)
        checkType(tpe)

      //
      // Binary Expressions.
      //
      case Expression.Binary(sop, op, exp1, exp2, tpe, loc) =>
        checkExp(exp1, env0, ienv0)
        checkExp(exp2, env0, ienv0)
        checkType(tpe)

      //
      // If-then-else Expressions.
      //
      case Expression.IfThenElse(exp1, exp2, exp3, tpe, loc) =>
        checkExp(exp1, env0, ienv0)
        checkExp(exp2, env0, ienv0)
        checkExp(exp3, env0, ienv0)
        checkType(tpe)

      //
      // Block Expressions.
      //
      case Expression.Branch(exp, branches, tpe, loc) =>
        checkExp(exp, env0, ienv0)
        for ((label, branch) <- branches) {
          checkExp(branch, env0, ienv0 ++ branches.keySet)
        }
        checkType(tpe)

      //
      // Jump Expressions.
      //
      case Expression.JumpTo(sym, tpe, loc) =>
        assert(ienv0 contains sym, s"Undefined label symbol: '$sym'.")
        checkType(tpe)

      //
      // Let Expressions.
      //
      case Expression.Let(sym, exp1, exp2, tpe, loc) =>
        checkExp(exp1, env0, ienv0)
        checkExp(exp2, env0 + sym, ienv0)
        checkType(tpe)

      //
      // LetRec Expressions.
      //
      case Expression.LetRec(sym, exp1, exp2, tpe, loc) =>
        checkExp(exp1, env0 + sym, ienv0)
        checkExp(exp2, env0 + sym, ienv0)
        checkType(tpe)

      //
      // Is Expressions.
      //
      case Expression.Is(sym, tag, exp, loc) =>
        assert(root.enums contains sym, s"Undefined enum symbol: '$sym'.")
        checkExp(exp, env0, ienv0)

      //
      // Tag Expressions.
      //
      case Expression.Tag(sym, tag, exp, tpe, loc) =>
        assert(root.enums contains sym, s"Undefined enum symbol: '$sym'.")
        checkExp(exp, env0, ienv0)
        checkType(tpe)

      //
      // Check if this is a single-case enum subject to elimination.
      //
      case Expression.Untag(sym, tag, exp, tpe, loc) =>
        assert(root.enums contains sym, s"Undefined enum symbol: '$sym'.")
        checkExp(exp, env0, ienv0)
        checkType(tpe)

      //
      // Index Expressions.
      //
      case Expression.Index(base, offset, tpe, loc) =>
        checkExp(base, env0, ienv0)
        checkType(tpe)

      //
      // Tuple Expressions.
      //
      case Expression.Tuple(elms, tpe, loc) =>
        for (elm <- elms) {
          checkExp(elm, env0, ienv0)
        }
        checkType(tpe)

      //
      // RecordEmpty Expressions.
      //
      case Expression.RecordEmpty(tpe, loc) =>
        checkType(tpe)

      //
      // RecordSelect Expressions.
      //
      case Expression.RecordSelect(base, label, tpe, loc) =>
        checkExp(base, env0, ienv0)
        checkType(tpe)

      //
      // RecordExtend Expressions.
      //
      case Expression.RecordExtend(label, value, rest, tpe, loc) =>
        checkExp(value, env0, ienv0)
        checkExp(rest, env0, ienv0)
        checkType(tpe)

      //
      // RecordRestrict Expressions.
      //
      case Expression.RecordRestrict(label, rest, tpe, loc) =>
        checkExp(rest, env0, ienv0)
        checkType(tpe)

      //
      // Array Expressions.
      //
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

      //
      // Reference Expressions.
      //
      case Expression.Ref(exp, tpe, loc) =>
        checkExp(exp, env0, ienv0)
        checkType(tpe)

      //
      // Dereference Expressions.
      //
      case Expression.Deref(exp, tpe, loc) =>
        checkExp(exp, env0, ienv0)
        checkType(tpe)

      //
      // Assign Expressions.
      //
      case Expression.Assign(exp1, exp2, tpe, loc) =>
        checkExp(exp1, env0, ienv0)
        checkExp(exp2, env0, ienv0)
        checkType(tpe)

      //
      // HandleWith Expressions.
      //
      case Expression.HandleWith(exp, bindings, tpe, loc) =>
        checkExp(exp, env0, ienv0)
        for (HandlerBinding(sym, handler) <- bindings) {
          checkExp(handler, env0, ienv0)
        }
        checkType(tpe)

      //
      // Existential Expressions.
      //
      case Expression.Existential(fparam, exp, loc) =>
        checkFormalParam(fparam)
        checkExp(exp, env0 + fparam.sym, ienv0)

      //
      // Universal Expressions.
      //
      case Expression.Universal(fparam, exp, loc) =>
        checkFormalParam(fparam)
        checkExp(exp, env0 + fparam.sym, ienv0)

      //
      // Try Catch
      //
      case Expression.TryCatch(exp, rules, tpe, loc) =>
        checkExp(exp, env0, ienv0)
        for (CatchRule(sym, clazz, body) <- rules) {
          checkExp(body, env0 + sym, ienv0)
        }
        checkType(tpe)

      //
      // Native Constructor.
      //
      case Expression.NativeConstructor(constructor, args, tpe, loc) =>
        for (arg <- args) {
          checkExp(arg, env0, ienv0)
        }
        checkType(tpe)

      //
      // Native Field.
      //
      case Expression.NativeField(field, tpe, loc) =>
        checkType(tpe)

      //
      // Native Method.
      //
      case Expression.NativeMethod(method, args, tpe, loc) =>
        for (arg <- args) {
          checkExp(arg, env0, ienv0)
        }
        checkType(tpe)

      //
      // New Channel.
      //
      case Expression.NewChannel(exp, tpe, loc) =>
        checkExp(exp, env0, ienv0)
        checkType(tpe)

      //
      // Get Channel.
      //
      case Expression.GetChannel(exp, tpe, loc) =>
        checkExp(exp, env0, ienv0)
        checkType(tpe)

      //
      // Put Channel.
      //
      case Expression.PutChannel(exp1, exp2, tpe, loc) =>
        checkExp(exp1, env0, ienv0)
        checkExp(exp2, env0, ienv0)
        checkType(tpe)

      //
      // Select Channel.
      //
      case Expression.SelectChannel(rules, default, tpe, loc) =>
        for (rule <- rules) {
          checkExp(rule.chan, env0, ienv0)
          checkExp(rule.exp, env0, ienv0)
        }
        default.foreach(exp => checkExp(exp, env0, ienv0))
        checkType(tpe)

      //
      // ProcessSpawn.
      //
      case Expression.ProcessSpawn(exp, tpe, loc) =>
        checkExp(exp, env0, ienv0)
        checkType(tpe)

      //
      // ProcessSleep.
      //
      case Expression.ProcessSleep(exp, tpe, loc) =>
        checkExp(exp, env0, ienv0)
        checkType(tpe)

      //
      // ProcessPanic.
      //
      case Expression.ProcessPanic(msg, tpe, loc) =>
        checkType(tpe)

      //
      // Constraint.
      //
      case Expression.FixpointConstraint(c, tpe, loc) =>
        checkType(tpe)

      //
      // ConstraintUnion.
      //
      case Expression.FixpointCompose(exp1, exp2, tpe, loc) =>
        checkExp(exp1, env0, ienv0)
        checkExp(exp2, env0, ienv0)
        checkType(tpe)

      //
      // Fixpoint Solve.
      //
      case Expression.FixpointSolve(exp, tpe, loc) =>
        checkExp(exp, env0, ienv0)
        checkType(tpe)

      //
      // Fixpoint Project.
      //
      case Expression.FixpointProject(pred, exp, tpe, loc) =>
        checkPredicateWithParam(pred, env0, ienv0)
        checkExp(exp, env0, ienv0)
        checkType(tpe)

      //
      // Fixpoint Project.
      //
      case Expression.FixpointEntails(exp1, exp2, tpe, loc) =>
        checkExp(exp1, env0, ienv0)
        checkExp(exp2, env0, ienv0)
        checkType(tpe)

      //
      // Error Expressions.
      //
      case Expression.HoleError(sym, tpe, loc) => checkType(tpe)
      case Expression.MatchError(tpe, loc) => checkType(tpe)
      case Expression.SwitchError(tpe, loc) => checkType(tpe)
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
      case Predicate.Head.Atom(pred, terms, tpe, loc) =>
        checkPredicateWithParam(pred, env0, ienv0)
        for (term <- terms) {
          checkHeadTerm(term, env0)
        }
        checkType(tpe)
    }

    /**
      * Checks invariants of the given body predicate `b0`.
      */
    def checkBodyPred(b0: Predicate.Body, env0: Set[Symbol.VarSym], ienv0: Set[Symbol.LabelSym]): Unit = b0 match {
      case Predicate.Body.Atom(pred, polarity, terms, tpe, loc) =>
        checkPredicateWithParam(pred, env0, ienv0)
        for (term <- terms) {
          checkBodyTerm(term, env0)
        }
        checkType(tpe)

      case Predicate.Body.Filter(sym, terms, loc) =>
        for (term <- terms) {
          checkBodyTerm(term, env0)
        }

      case Predicate.Body.Functional(sym, term, loc) =>
        checkHeadTerm(term, env0)
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
      case Term.Head.App(sym, args, tpe, loc) =>
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
      * Checks invariants of the given predicate with param `p`.
      */
    def checkPredicateWithParam(p: PredicateWithParam, env0: Set[Symbol.VarSym], ienv0: Set[Symbol.LabelSym]): Unit = p match {
      case PredicateWithParam(sym, exp) => checkExp(exp, env0, ienv0)
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

}

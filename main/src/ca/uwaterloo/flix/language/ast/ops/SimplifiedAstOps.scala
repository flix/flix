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
      case Expression.LambdaClosure(lambda, freeVars, tpe, loc) =>
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
      // Error Expressions.
      //
      case Expression.UserError(tpe, loc) => checkType(tpe)
      case Expression.HoleError(sym, tpe, eff, loc) => checkType(tpe)
      case Expression.MatchError(tpe, loc) => checkType(tpe)
      case Expression.SwitchError(tpe, loc) => checkType(tpe)
    }

    /**
      * Checks invariants of the given constraint `c0`.
      */
    def checkConstraint(c0: Constraint): Unit = {
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
      checkHeadPred(c0.head, envHead.toSet)
      for (bodyPred <- c0.body) {
        checkBodyPred(bodyPred, ruleEnv.toSet)
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
    def checkHeadPred(h0: Predicate.Head, env0: Set[Symbol.VarSym]): Unit = h0 match {
      case Predicate.Head.True(loc) => // nop
      case Predicate.Head.False(loc) => // nop
      case Predicate.Head.Atom(sym, terms, loc) =>
        for (term <- terms) {
          checkHeadTerm(term, env0)
        }
    }

    /**
      * Checks invariants of the given body predicate `b0`.
      */
    def checkBodyPred(b0: Predicate.Body, env0: Set[Symbol.VarSym]): Unit = b0 match {
      case Predicate.Body.Atom(sym, polarity, terms, loc) =>
        for (term <- terms) {
          checkBodyTerm(term, env0)
        }
      case Predicate.Body.Filter(sym, terms, loc) =>
        for (term <- terms) {
          checkBodyTerm(term, env0)
        }
      case Predicate.Body.Loop(sym, term, loc) =>
        checkHeadTerm(term, env0)
    }

    /**
      * Checks invariants of the given head term `t0`.
      */
    def checkHeadTerm(t0: Term.Head, env0: Set[Symbol.VarSym]): Unit = t0 match {
      case Term.Head.Var(sym, tpe, loc) =>
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
    def checkBodyTerm(t0: Term.Body, env0: Set[Symbol.VarSym]) = t0 match {
      case Term.Body.Wild(tpe, loc) =>
      // TODO: What is the type allowed to be here?
      case Term.Body.Var(sym, tpe, loc) =>
        checkType(tpe)
      case Term.Body.Lit(exp, tpe, loc) =>
        checkExp(exp0 = exp, env0 = env0, ienv0 = Set.empty)
        checkType(tpe)
      case Term.Body.Pat(pat, tpe, loc) =>
        checkPat(pat)
        checkType(tpe)
    }

    /**
      * Checks invariants of the given pattern `pat0`.
      */
    def checkPat(pat0: Pattern): Unit = pat0 match {
      case Pattern.Wild(tpe, loc) =>
        checkType(tpe)
      case Pattern.Var(sym, tpe, loc) =>
        checkType(tpe)
      case Pattern.Unit(loc) => // nop
      case Pattern.True(loc) => // nop
      case Pattern.False(loc) => // nop
      case Pattern.Char(lit, loc) => // nop
      case Pattern.Float32(lit, loc) => // nop
      case Pattern.Float64(lit, loc) => // nop
      case Pattern.Int8(lit, loc) => // nop
      case Pattern.Int16(lit, loc) => // nop
      case Pattern.Int32(lit, loc) => // nop
      case Pattern.Int64(lit, loc) => // nop
      case Pattern.BigInt(lit, loc) => // nop
      case Pattern.Str(lit, loc) => // nop
      case Pattern.Tag(sym, tag, pat, tpe, loc) =>
        checkPat(pat)
        checkType(tpe)
      case Pattern.Tuple(elms, tpe, loc) =>
        for (elm <- elms) {
          checkPat(elm)
        }
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
    for ((tpe1, Lattice(tpe2, bot, top, equ, leq, lub, glb, loc)) <- root.lattices) {
      assert(tpe1 == tpe2)
      checkType(tpe1)
      checkType(tpe2)
      checkExp(exp0 = bot, env0 = Set.empty, ienv0 = Set.empty)
      checkExp(exp0 = top, env0 = Set.empty, ienv0 = Set.empty)
      checkExp(exp0 = equ, env0 = Set.empty, ienv0 = Set.empty)
      checkExp(exp0 = leq, env0 = Set.empty, ienv0 = Set.empty)
      checkExp(exp0 = lub, env0 = Set.empty, ienv0 = Set.empty)
      checkExp(exp0 = glb, env0 = Set.empty, ienv0 = Set.empty)
    }

    //
    // Check all properties in the program.
    //
    for (Property(law, defn, exp) <- root.properties) {
      checkExp(exp0 = exp, env0 = Set.empty, ienv0 = Set.empty)
    }

    //
    // Check all tables in the program.
    //
    for ((sym1, table) <- root.tables) {
      table match {
        case Table.Relation(sym2, attributes, loc) =>
          assert(sym1 == sym2)
          for (attribute <- attributes) {
            checkAttribute(attribute)
          }
        case Table.Lattice(sym2, keys, value, loc) =>
          assert(sym1 == sym2)
          for (attribute <- keys) {
            checkAttribute(attribute)
          }
          checkAttribute(value)
      }
    }

    //
    // Check all constraints in the program.
    //
    for (Stratum(constraints) <- root.strata) {
      for (constraint <- constraints) {
        checkConstraint(constraint)
      }
    }

    // Success :)
    root
  }

}

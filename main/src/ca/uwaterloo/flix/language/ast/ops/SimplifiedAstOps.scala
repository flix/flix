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
import ca.uwaterloo.flix.language.ast.Type
import ca.uwaterloo.flix.util.InternalCompilerException

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
      checkExp(defn0.exp)
    }

    /**
      * Checks invariants of the given expression `exp0`.
      */
    def checkExp(exp0: Expression): Unit = exp0 match {
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
        checkType(tpe)

      //
      // Def Expressions.
      //
      case Expression.Def(sym, tpe, loc) =>
        checkType(tpe)

      //
      // Closure Expressions.
      //
      case Expression.Closure(exp, freeVars, tpe, loc) =>
        checkExp(exp)
        checkType(tpe)

      //
      // ApplyClo Expressions.
      //
      case Expression.ApplyClo(exp, args, tpe, loc) =>
        checkExp(exp)
        for (arg <- args) {
          checkExp(arg)
        }
        checkType(tpe)

      //
      // ApplyDef Expressions.
      //
      case Expression.ApplyDef(sym, args, tpe, loc) =>
        for (arg <- args) {
          checkExp(arg)
        }
        checkType(tpe)

      //
      // ApplyCloTail Expressions.
      //
      case Expression.ApplyCloTail(exp, args, tpe, loc) =>
        checkExp(exp)
        for (arg <- args) {
          checkExp(arg)
        }
        checkType(tpe)

      //
      // ApplyDefTail Expressions.
      //
      case Expression.ApplyDefTail(sym, args, tpe, loc) =>
        for (arg <- args) {
          checkExp(arg)
        }
        checkType(tpe)

      //
      // ApplySelfTail Expressions.
      //
      case Expression.ApplySelfTail(sym, formals, actuals, tpe, loc) =>
        for (param <- formals) {
          checkFormalParam(param)
        }
        for (arg <- actuals) {
          checkExp(arg)
        }
        checkType(tpe)

      //
      // ApplyHook Expressions.
      //
      case Expression.ApplyHook(hook, args, tpe, loc) =>
        for (arg <- args) {
          checkExp(arg)
        }

      //
      // Unary Expressions.
      //
      case Expression.Unary(sop, op, exp, tpe, loc) =>
        checkExp(exp)
        checkType(tpe)

      //
      // Binary Expressions.
      //
      case Expression.Binary(sop, op, exp1, exp2, tpe, loc) =>
        checkExp(exp1)
        checkExp(exp2)
        checkType(tpe)

      //
      // If-then-else Expressions.
      //
      case Expression.IfThenElse(exp1, exp2, exp3, tpe, loc) =>
        checkExp(exp1)
        checkExp(exp2)
        checkExp(exp3)
        checkType(tpe)

      //
      // Block Expressions.
      //
      case Expression.Branch(exp, branches, tpe, loc) =>
        checkExp(exp)
        for ((label, branch) <- branches) {
          checkExp(branch)
        }
        checkType(tpe)

      //
      // Jump Expressions.
      //
      case Expression.JumpTo(sym, tpe, loc) =>
        checkType(tpe)

      //
      // Let Expressions.
      //
      case Expression.Let(sym, exp1, exp2, tpe, loc) =>
        checkExp(exp1)
        checkExp(exp2)
        checkType(tpe)

      //
      // LetRec Expressions.
      //
      case Expression.LetRec(sym, exp1, exp2, tpe, loc) =>
        checkExp(exp1)
        checkExp(exp2)
        checkType(tpe)

      //
      // Is Expressions.
      //
      case Expression.Is(sym, tag, exp, loc) =>
        checkExp(exp)

      //
      // Tag Expressions.
      //
      case Expression.Tag(sym, tag, exp, tpe, loc) =>
        checkExp(exp)
        checkType(tpe)

      //
      // Check if this is a single-case enum subject to elimination.
      //
      case Expression.Untag(sym, tag, exp, tpe, loc) =>
        checkExp(exp)
        checkType(tpe)

      //
      // Index Expressions.
      //
      case Expression.Index(base, offset, tpe, loc) =>
        checkExp(base)
        checkType(tpe)

      //
      // Tuple Expressions.
      //
      case Expression.Tuple(elms, tpe, loc) =>
        for (elm <- elms) {
          checkExp(elm)
        }
        checkType(tpe)

      //
      // Reference Expressions.
      //
      case Expression.Ref(exp, tpe, loc) =>
        checkExp(exp)
        checkType(tpe)

      //
      // Dereference Expressions.
      //
      case Expression.Deref(exp, tpe, loc) =>
        checkExp(exp)
        checkType(tpe)

      //
      // Assign Expressions.
      //
      case Expression.Assign(exp1, exp2, tpe, loc) =>
        checkExp(exp1)
        checkExp(exp2)
        checkType(tpe)

      //
      // Existential Expressions.
      //
      case Expression.Existential(fparam, exp, loc) =>
        checkFormalParam(fparam)
        checkExp(exp)

      //
      // Universal Expressions.
      //
      case Expression.Universal(fparam, exp, loc) =>
        checkFormalParam(fparam)
        checkExp(exp)

      //
      // Native Constructor.
      //
      case Expression.NativeConstructor(constructor, args, tpe, loc) =>
        for (arg <- args) {
          checkExp(arg)
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
          checkExp(arg)
        }
        checkType(tpe)

      //
      // Error Expressions.
      //
      case Expression.UserError(tpe, loc) => checkType(tpe)
      case Expression.MatchError(tpe, loc) => checkType(tpe)
      case Expression.SwitchError(tpe, loc) => checkType(tpe)

      //
      // Unexpected Expressions.
      //
      // TODO: These should be allowed.
      case Expression.LambdaClosure(lambda, freeVars, tpe, loc) => throw InternalCompilerException(s"Unexpected expression: '${exp0.getClass.getSimpleName}'.")
      case Expression.Lambda(args, body, tpe, loc) => throw InternalCompilerException(s"Unexpected expression: '${exp0.getClass.getSimpleName}'.")
      case Expression.Hook(hook, tpe, loc) => throw InternalCompilerException(s"Unexpected expression: '${exp0.getClass.getSimpleName}'.")
      case Expression.Apply(exp, args, tpe, loc) => throw InternalCompilerException(s"Unexpected expression: '${exp0.getClass.getSimpleName}'.")
    }

    /**
      * Checks invariants of the given constraint `c0`.
      */
    def checkConstraint(c0: Constraint): Unit = {
      for (param <- c0.cparams) {
        checkConstraintParam(param)
      }
      checkHeadPred(c0.head)
      for (bodyPred <- c0.body) {
        checkBodyPred(bodyPred)
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
    def checkHeadPred(h0: Predicate.Head): Unit = h0 match {
      case Predicate.Head.True(loc) => // nop
      case Predicate.Head.False(loc) => // nop
      case Predicate.Head.Positive(sym, terms, loc) =>
        for (term <- terms) {
          checkHeadTerm(term)
        }
      case Predicate.Head.Negative(sym, term, loc) => ??? // TODO: Impossible
    }

    /**
      * Checks invariants of the given body predicate `b0`.
      */
    def checkBodyPred(b0: Predicate.Body): Unit = b0 match {
      case Predicate.Body.Positive(sym, terms, loc) =>
        for (term <- terms) {
          checkBodyTerm(term)
        }
      case Predicate.Body.Negative(sym, terms, loc) =>
        for (term <- terms) {
          checkBodyTerm(term)
        }
      case Predicate.Body.Filter(sym, terms, loc) =>
        for (term <- terms) {
          checkBodyTerm(term)
        }
      case Predicate.Body.Loop(sym, term, loc) =>
        checkHeadTerm(term)
    }

    /**
      * Checks invariants of the given head term `t0`.
      */
    def checkHeadTerm(t0: Term.Head): Unit = t0 match {
      case Term.Head.Var(sym, tpe, loc) =>
        checkType(tpe)
      case Term.Head.Lit(exp, tpe, loc) =>
        checkExp(exp)
        checkType(tpe)
      case Term.Head.App(sym, args, tpe, loc) =>
        checkType(tpe)
    }

    /**
      * Checks invariants of the given body term `t0`.
      */
    def checkBodyTerm(t0: Term.Body) = t0 match {
      case Term.Body.Wild(tpe, loc) =>
        checkType(tpe)
      case Term.Body.Var(sym, tpe, loc) =>
        checkType(tpe)
      case Term.Body.Lit(exp, tpe, loc) =>
        checkExp(exp)
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
      case Type.Var(id, kind) => assert(assertion = false, "Unexpected type variable.")
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
      checkExp(bot)
      checkExp(top)
      checkExp(equ)
      checkExp(leq)
      checkExp(lub)
      checkExp(glb)
    }

    //
    // Check all properties in the program.
    //
    for (Property(law, defn, exp) <- root.properties) {
      checkExp(exp)
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

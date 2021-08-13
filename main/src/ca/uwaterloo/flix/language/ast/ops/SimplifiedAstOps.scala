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
      case Expression.Unit(_) => // nop

      case Expression.Null(tpe, _) => checkType(tpe)

      case Expression.True(_) => // nop

      case Expression.False(_) => // nop

      case Expression.Char(lit, _) => // nop

      case Expression.Float32(lit, _) => // nop

      case Expression.Float64(lit, _) => // nop

      case Expression.Int8(lit, _) => // nop

      case Expression.Int16(lit, _) => // nop

      case Expression.Int32(lit, _) => // nop

      case Expression.Int64(lit, _) => // nop

      case Expression.BigInt(lit, _) => // nop

      case Expression.Str(lit, _) => // nop

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

      case Expression.RecordSelect(base, field, tpe, loc) =>
        checkExp(base, env0, ienv0)
        checkType(tpe)

      case Expression.RecordExtend(field, value, rest, tpe, loc) =>
        checkExp(value, env0, ienv0)
        checkExp(rest, env0, ienv0)
        checkType(tpe)

      case Expression.RecordRestrict(field, rest, tpe, loc) =>
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

      case Expression.Spawn(exp, tpe, loc) =>
        checkExp(exp, env0, ienv0)
        checkType(tpe)

      case Expression.Lazy(exp, tpe, loc) =>
        checkExp(exp, env0, ienv0)
        checkType(tpe)

      case Expression.Force(exp, tpe, loc) =>
        checkExp(exp, env0, ienv0)
        checkType(tpe)

      case Expression.HoleError(sym, tpe, loc) =>
        checkType(tpe)

      case Expression.MatchError(tpe, loc) =>
        checkType(tpe)
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
    def checkType(tpe0: Type): Unit =
      assert(Type.Kinded.typeVars(tpe0).isEmpty, "Unexpected type variable.")

    //
    // Check all definitions in the program.
    //
    for ((sym, defn) <- root.defs) {
      checkDefn(defn)
    }

    // Success :)
    root
  }

}

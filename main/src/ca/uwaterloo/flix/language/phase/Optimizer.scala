/*
 * Copyright 2015-2016 Magnus Madsen
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

import ca.uwaterloo.flix.language.ast.BinaryOperator
import ca.uwaterloo.flix.language.ast.SimplifiedAst.Definition.Constant
import ca.uwaterloo.flix.language.ast.SimplifiedAst.Expression._
import ca.uwaterloo.flix.language.ast.SimplifiedAst.{Expression, Root}
import ca.uwaterloo.flix.util.InternalCompilerException

object Optimizer {

  def optimize(root: Root): Root = {
    val constants = root.constants.map {
      case (name, defn) =>
        var fn = defn

        // Do simple copy propagation to get rid of targets of direct assignment
        fn = CopyPropagation.optimize(fn)

        // Replace Unit == Unit checks
        fn = EliminateUnitChecks.optimize(fn)

        // Clean up by removing code
        fn = DeadCodeElimination.optimize(fn)

        name -> fn
    }

    root.copy(constants = constants)
  }

  object EliminateUnitChecks {

    def optimize(f: Constant): Constant = {
      def replaceUnitCheckWithTrue(e: Expression): Expression = e match {
        case Unit => e

        case True => e
        case False => e

        case Char(_) => e

        case Float32(_) => e
        case Float64(_) => e

        case Int8(_) => e
        case Int16(_) => e
        case Int32(_) => e
        case Int64(_) => e
        case BigInt(_) => e

        case Str(_) => e

        case LoadBool(exp, offset) =>
          val nexp = replaceUnitCheckWithTrue(exp)

          LoadBool(nexp, offset)

        case LoadInt8(exp, offset) =>
          val nexp = replaceUnitCheckWithTrue(exp)

          LoadInt8(nexp, offset)

        case LoadInt16(exp, offset) =>
          val nexp = replaceUnitCheckWithTrue(exp)

          LoadInt16(nexp, offset)

        case LoadInt32(exp, offset) =>
          val nexp = replaceUnitCheckWithTrue(exp)

          LoadInt32(nexp, offset)

        case StoreBool(exp, offset, v) =>
          val nexp = replaceUnitCheckWithTrue(exp)
          val nv = replaceUnitCheckWithTrue(v)

          StoreBool(nexp, offset, nv)

        case StoreInt8(exp, offset, v) =>
          val nexp = replaceUnitCheckWithTrue(exp)
          val nv = replaceUnitCheckWithTrue(v)

          StoreInt8(nexp, offset, nv)

        case StoreInt16(exp, offset, v) =>
          val nexp = replaceUnitCheckWithTrue(exp)
          val nv = replaceUnitCheckWithTrue(v)

          StoreInt16(nexp, offset, nv)

        case StoreInt32(exp, offset, v) =>
          val nexp = replaceUnitCheckWithTrue(exp)
          val nv = replaceUnitCheckWithTrue(v)

          StoreInt32(nexp, offset, nv)

        case Var(_, _, _, _) => e

        case Ref(_, _, _) => e

        case Lambda(_, _, _, _) =>
          throw InternalCompilerException("Lamdas should have been converted to closures and lifted.")

        case Hook(_, _, _) => e

        case MkClosure(_, _, _, _) =>
          throw InternalCompilerException("MkClosure should have been replaced by MkClosureRef after lambda lifting.")

        case MkClosureRef(_, _, _, _) => e

        case ApplyRef(name, args, tpe, loc) =>
          val nargs = args.map(exp => replaceUnitCheckWithTrue(exp))
          ApplyRef(name, nargs, tpe, loc)

        case ApplyHook(hook, args, tpe, loc) =>
          val nargs = args.map(exp => replaceUnitCheckWithTrue(exp))
          ApplyHook(hook, nargs, tpe, loc)

        case Apply(exp, args, tpe, loc) =>
          val nexp = replaceUnitCheckWithTrue(exp)
          val nargs = args.map(exp => replaceUnitCheckWithTrue(exp))
          Apply(nexp, nargs, tpe, loc)

        case Unary(op, exp, tpe, loc) =>
          val nexp = replaceUnitCheckWithTrue(exp)
          Unary(op, nexp, tpe, loc)

        case Binary(BinaryOperator.Equal, exp1, exp2, tpe, loc) =>
          // Unit == Unit is always true
          if (exp1.tpe == Unit.tpe) {
            True
          } else {
            val e1 = replaceUnitCheckWithTrue(exp1)
            val e2 = replaceUnitCheckWithTrue(exp2)
            Binary(BinaryOperator.Equal, e1, e2, tpe, loc)
          }

        case Binary(op, exp1, exp2, tpe, loc) =>
          val e1 = replaceUnitCheckWithTrue(exp1)
          val e2 = replaceUnitCheckWithTrue(exp2)
          Binary(op, e1, e2, tpe, loc)

        case IfThenElse(exp1, exp2, exp3, tpe, loc) =>
          val e1 = replaceUnitCheckWithTrue(exp1)
          val e2 = replaceUnitCheckWithTrue(exp2)
          val e3 = replaceUnitCheckWithTrue(exp3)
          IfThenElse(e1, e2, e3, tpe, loc)

        case Let(ident, offset, exp1, exp2, tpe, loc) =>
          val e1 = replaceUnitCheckWithTrue(exp1)
          val e2 = replaceUnitCheckWithTrue(exp2)
          Let(ident, offset, e1, e2, tpe, loc)

        case CheckTag(tag, exp, loc) =>
          val nexp = replaceUnitCheckWithTrue(exp)
          CheckTag(tag, nexp, loc)

        case GetTagValue(tag, exp, tpe, loc) =>
          val nexp = replaceUnitCheckWithTrue(exp)
          GetTagValue(tag, nexp, tpe, loc)

        case Tag(enum, tag, exp, tpe, loc) =>
          val nexp = replaceUnitCheckWithTrue(exp)
          Tag(enum, tag, nexp, tpe, loc)

        case GetTupleIndex(base, offset, tpe, loc) =>
          val nbase = replaceUnitCheckWithTrue(base)
          GetTupleIndex(nbase, offset, tpe, loc)

        case Tuple(elms, tpe, loc) =>
          val nelms = elms.map(exp => replaceUnitCheckWithTrue(exp))
          Tuple(nelms, tpe, loc)

        case CheckNil(exp, loc) =>
          val nexp = replaceUnitCheckWithTrue(exp)
          CheckNil(nexp, loc)

        case CheckCons(exp, loc) =>
          val nexp = replaceUnitCheckWithTrue(exp)
          CheckCons(nexp, loc)

        case FSet(elms, tpe, loc) =>
          val nelms = elms.map(exp => replaceUnitCheckWithTrue(exp))
          FSet(nelms, tpe, loc)

        case Existential(params, exp, loc) =>
          val nexp = replaceUnitCheckWithTrue(exp)
          Existential(params, nexp, loc)

        case Universal(params, exp, loc) =>
          val nexp = replaceUnitCheckWithTrue(exp)
          Universal(params, nexp, loc)

        case UserError(_, _) => e
        case MatchError(_, _) => e
        case SwitchError(_, _) => e

      }

      f.copy(exp = replaceUnitCheckWithTrue(f.exp))
    }

  }

  object DeadCodeElimination {

    def optimize(f: Constant): Constant = f

  }

  object CopyPropagation {

    def optimize(f: Constant): Constant = f

  }

}

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
import ca.uwaterloo.flix.language.ast._
import ca.uwaterloo.flix.language.ast.TypedAst.Expression
import ca.uwaterloo.flix.util.Validation
import ca.uwaterloo.flix.util.Validation._
import ca.uwaterloo.flix.util.vt.VirtualString._
import ca.uwaterloo.flix.util.vt.VirtualTerminal

/**
  * A phase that computes the effect of every expression in the program.
  */
object Effects extends Phase[TypedAst.Root, TypedAst.Root] {

  // TODO: The order of effects might become significantly more clear if the AST was in administrative normal form (ANF).

  /**
    * An error raised to indicate that the expected effects of an expression does not match its actual effects.
    *
    * @param loc the location where the error occurred.
    */
  case class EffectError(expected: Eff, actual: Eff, loc: SourceLocation) extends CompilationError {
    val kind: String = "Effect Error"
    val source: SourceInput = loc.source
    val message: VirtualTerminal = {
      // TODO: Improve error message.
      val vt = new VirtualTerminal()
      vt << Line(kind, source.format) << NewLine
      vt << ">> Expression effect to have " << expected.toString << "effects but has " << actual.toString << " effects." << NewLine
    }
  }

  /**
    * Performs effect inference on the given AST `root`.
    */
  def run(root: TypedAst.Root)(implicit flix: Flix): Validation[TypedAst.Root, EffectError] = {
    /**
      * Infer effects for definitions.
      */
    val definitionsVal = root.definitions.map {
      case (sym, defn0) => Declarations.infer(defn0, root) map {
        case defn => sym -> defn
      }
    }

    // TODO: Infer effects for constraints.

    for {
      definitions <- seqM(definitionsVal)
    } yield {
      // TODO: Time
      root.copy(definitions = definitions.toMap)
    }
  }

  object Declarations {

    /**
      * Infers the effects of the given definition `defn0`.
      */
    def infer(defn0: TypedAst.Declaration.Definition, root: TypedAst.Root): Validation[TypedAst.Declaration.Definition, EffectError] = {
      // TODO: Introduce EffectParam

      for {
        e <- Expressions.infer(defn0.exp, root)
      } yield {
        defn0.copy(exp = e)
      }
    }
  }

  object Expressions {


    /**
      * Infers the effects of the given expression `exp0`.
      */
    def infer(exp0: Expression, root: TypedAst.Root): Validation[Expression, EffectError] = {
      /**
        * Local visitor.
        */
      def visitExp(e0: Expression, env0: Map[Symbol.VarSym, Eff]): Validation[Expression, EffectError] = e0 match {
        /**
          * Literal Expressions.
          */
        case Expression.Unit(loc) => e0.toSuccess
        case Expression.True(loc) => e0.toSuccess
        case Expression.False(loc) => e0.toSuccess
        case Expression.Char(lit, loc) => e0.toSuccess
        case Expression.Float32(lit, loc) => e0.toSuccess
        case Expression.Float64(lit, loc) => e0.toSuccess
        case Expression.Int8(lit, loc) => e0.toSuccess
        case Expression.Int16(lit, loc) => e0.toSuccess
        case Expression.Int32(lit, loc) => e0.toSuccess
        case Expression.Int64(lit, loc) => e0.toSuccess
        case Expression.BigInt(lit, loc) => e0.toSuccess
        case Expression.Str(lit, loc) => e0.toSuccess

        /**
          * Wildcard Expressions.
          */
        case Expression.Wild(tpe, _, loc) =>
          // Wildcards are pure.
          Expression.Wild(tpe, Eff.Pure, loc).toSuccess

        /**
          * Variable Expressions.
          */
        case Expression.Var(sym, tpe, _, loc) =>
          // TODO: Lookup the effect in the effect environment.
          val eff = Eff.Pure
          Expression.Var(sym, tpe, eff, loc).toSuccess

        /**
          * Ref Expressions.
          */
        case Expression.Ref(sym, tpe, _, loc) =>
          ??? // TODO

        /**
          * Hook Expressions.
          */
        case Expression.Hook(hook, tpe, _, loc) =>
          ??? // TODO

        /**
          * Lambda Expressions.
          */
        case Expression.Lambda(args, body, tpe, _, loc) =>
          ??? // TODO

        case Expression.Apply(lambda, args, tpe, _, loc) =>
          ???

        /**
          * Unary Expressions.
          */
        case Expression.Unary(op, exp, tpe, _, loc) =>
          for {
            e <- visitExp(exp, env0)
          } yield {
            val eff = e.eff
            Expression.Unary(op, e, tpe, eff, loc)
          }

        /**
          * Binary Expressions.
          */
        case Expression.Binary(op, exp1, exp2, tpe, _, loc) =>
          for {
            e1 <- visitExp(exp1, env0)
            e2 <- visitExp(exp2, env0)
          } yield {
            // The effects of e1 happen before the effects of e2.
            val eff = Eff.seq(e1.eff, e2.eff)
            Expression.Binary(op, exp1, exp2, tpe, eff, loc)
          }

        /**
          * Let Expressions.
          */
        case Expression.Let(sym, exp1, exp2, tpe, _, loc) =>
          for {
            e1 <- visitExp(exp1, env0)
            // TODO: To what extend should the environment be extended with the effect of e1?
            e2 <- visitExp(exp2, env0 + (sym -> e1.eff))
          } yield {
            // The effects of e1 happen before e2.
            val eff = Eff.seq(e1.eff, e2.eff)
            Expression.Let(sym, exp1, exp2, tpe, eff, loc)
          }

        /**
          * LetRec Expressions.
          */
        case Expression.LetRec(sym, exp1, exp2, tpe, _, loc) =>
          for {
          // TODO: To what extend should the environment be extended with the effect of e1?
            e1 <- visitExp(exp1, env0)
            e2 <- visitExp(exp2, env0 + (sym -> e1.eff))
          } yield {
            // The effects of e1 happen before e2.
            val eff = Eff.seq(e1.eff, e2.eff)
            Expression.LetRec(sym, exp1, exp2, tpe, eff, loc)
          }

        /**
          * If-Then-Else Expressions.
          */
        case Expression.IfThenElse(exp1, exp2, exp3, tpe, _, loc) =>
          for {
            e1 <- visitExp(exp1, env0)
            e2 <- visitExp(exp2, env0)
            e3 <- visitExp(exp3, env0)
          } yield {
            // The effects of e1 happen before the effects of e2 and e3.
            val seq1 = Eff.seq(e1.eff, e2.eff)
            val seq2 = Eff.seq(e1.eff, e3.eff)

            // The effects of the overall expression is the least upper bound of the two effects above.
            val eff = Eff.lub(seq1, seq2)
            Expression.IfThenElse(e1, e2, e3, tpe, eff, loc)
          }

        /**
          * Match Expressions.
          */
        case Expression.Match(exp, rules, tpe, _, loc) =>
          ??? // TODO

        /**
          * Switch Expressions.
          */
        case Expression.Switch(rules, tpe, _, loc) =>
          ??? // TODO

        /**
          * Tag Expressions.
          */
        case Expression.Tag(sym, tag, exp, tpe, _, loc) =>
          for {
            e <- visitExp(exp, env0)
          } yield {
            val eff = exp.eff
            Expression.Tag(sym, tag, e, tpe, eff, loc)
          }

        /**
          * Tuple Expressions.
          */
        case Expression.Tuple(elms, tpe, _, loc) =>
          ??? // TODO

        /**
          * Existential Expressions.
          */
        case Expression.Existential(fparam, exp, _, loc) =>
          // An existential expression must be pure.
          for {
            e <- visitExp(exp, env0)
            _ <- leqM(e.eff, Eff.Pure)
          } yield {
            Expression.Existential(fparam, exp, Eff.Pure, loc)
          }

        /**
          * Universal Expressions.
          */
        case Expression.Universal(fparam, exp, _, loc) =>
          ??? // TODO

        /**
          * Native Constructor Expressions.
          */
        case Expression.NativeConstructor(constructor, args, tpe, _, loc) =>
          ??? // TODO

        /**
          * Native Field Expressions.
          */
        case Expression.NativeField(field, tpe, _, loc) =>
          ??? // TODO

        /**
          * Native Method Expressions.
          */
        case Expression.NativeMethod(method, args, tpe, _, loc) =>
          ??? // TODO

        /**
          * User Error Expressions.
          */
        case Expression.UserError(tpe, _, loc) =>
          ??? // TODO
      }

      /**
        * Local visitor.
        */
      def visitExps(es: List[Expression], env0: Map[Symbol.VarSym, Eff]): Validation[Expression, EffectError] = ???

      visitExp(exp0, /* TODO what effect environment?*/ Map.empty)
    }

  }

  // TODO: Change signature to take and return exp?

  def pureM(eff1: Eff, eff2: Eff): Validation[Unit, EffectError] = ???


  def leqM(eff1: Eff, eff2: Eff): Validation[Unit, EffectError] = ???

}

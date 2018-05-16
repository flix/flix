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
import ca.uwaterloo.flix.language.ast
import ca.uwaterloo.flix.language.ast.TypedAst._
import ca.uwaterloo.flix.language.ast._
import ca.uwaterloo.flix.language.errors.EffectError
import ca.uwaterloo.flix.util.Validation._
import ca.uwaterloo.flix.util.{Timer, Validation}

/**
  * A phase that computes the effect of every expression in the program.
  */
object Effects extends Phase[Root, Root] {

  /**
    * Performs effect inference on the given AST `root`.
    */
  def run(root: Root)(implicit flix: Flix): Validation[Root, EffectError] = {

    // TODO: Effects currently disabled:
    return root.toSuccess

    val timer = new Timer({
      /**
        * Infer effects for definitions.
        */
      val definitionsVal = root.defs.map {
        case (sym, defn0) => infer(defn0, root) map {
          case defn => sym -> defn
        }
      }

      // TODO: [Effects]: Infer effects for constraints.

      for {
        definitions <- seqM(definitionsVal)
      } yield {
        root.copy(defs = definitions.toMap)
      }
    })

    timer.getResult.map(root => root.copy(time = root.time.copy(effects = timer.getDuration)))
  }

  /**
    * Infers the effects of the given definition `defn0`.
    */
  def infer(defn0: TypedAst.Def, root: Root): Validation[TypedAst.Def, EffectError] = {

    // TODO: [Effects] Introduce EffectParam for polymorphic effects.

    /*
     * Infer the effects of the formal parameters.
     */
    val env0 = defn0.fparams.foldLeft(Map.empty[Symbol.VarSym, ast.Eff]) {
      case (macc, TypedAst.FormalParam(sym, _, tpe, _)) => macc // TODO
    }

    /*
     * The expected effect of the definition.
     */
    val expectedEff = defn0.eff

    /*
     * Infer the effect of the expression.
     */
    Expressions.infer(defn0.exp, env0, root) flatMap {
      case e =>
        /*
         * Check that the declared effect matches the actual effect.
         */
        val actualEff = e.eff
        if (actualEff leq expectedEff)
          defn0.copy(exp = e).toSuccess
        else
          EffectError(expectedEff, actualEff, defn0.exp.loc).toFailure
    }

  }

  object Expressions {

    /**
      * Infers the effects of the given expression `exp0`.
      */
    def infer(exp0: Expression, initialEnv: Map[Symbol.VarSym, ast.Eff], root: Root): Validation[Expression, EffectError] = {

      /**
        * Local visitor.
        */
      def visitExp(e0: Expression, env0: Map[Symbol.VarSym, ast.Eff]): Validation[Expression, EffectError] = e0 match {
        /**
          * Literal Expression.
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
          * Wildcard Expression.
          */
        case Expression.Wild(tpe, _, loc) =>
          // Wildcards are pure.
          Expression.Wild(tpe, ast.Eff.Pure, loc).toSuccess

        /**
          * Variable Expression.
          */
        case Expression.Var(sym, tpe, _, loc) =>
          // Lookup the effect in the effect environment.
          val eff = env0.getOrElse(sym, ast.Eff.Pure)
          Expression.Var(sym, tpe, eff, loc).toSuccess

        /**
          * Hole Expression.
          */
        case Expression.Hole(sym, tpe, eff, loc) => ??? // TODO

        /**
          * Def Expression.
          */
        case Expression.Def(sym, tpe, _, loc) =>
          // The effect of a ref is its declared effect.
          val defn = root.defs(sym)
          val latent = defn.eff.eff
          val eff = ast.Eff.Arrow(ast.Eff.Pure, latent, ast.Eff.Pure, EffectSet.Bot)
          Expression.Def(sym, tpe, eff, loc).toSuccess

        /**
          * Def Expression.
          */
        case Expression.Eff(sym, tpe, _, loc) => ??? // TODO

        /**
          * Lambda Expression.
          */
        case Expression.Lambda(args, body, tpe, _, loc) =>
          for {
            e <- visitExp(body, env0)
          } yield {
            // TODO: [Effects]: Take the number of arguments into account.
            val eff = ast.Eff.Arrow(ast.Eff.Pure, e.eff.eff, ast.Eff.Pure, EffectSet.Bot)
            Expression.Lambda(args, body, tpe, eff, loc)
          }

        /**
          * Apply Expression.
          */
        case Expression.Apply(lambda, args, tpe, _, loc) =>
          for {
            e <- visitExp(lambda, env0)
            es <- seqM(args.map(e => visitExp(e, env0)))
          } yield {
            // TODO: [Effects]: Take the number of arguments into account.
            val ast.Eff.Arrow(_, latent, e2, eff) = e.eff

            // Effects of arguments.
            val argumentEffect = es.foldLeft(ast.Eff.Pure) {
              case (eacc, exp) => eacc seq exp.eff
            }

            // The effects of the lambda expression happen before the effects the arguments.
            // Then the effects of applying the lambda happens.
            val resultEff = ast.Eff.Box(latent) seq argumentEffect
            Expression.Apply(e, es, tpe, resultEff, loc)
          }

        /**
          * Unary Expression.
          */
        case Expression.Unary(op, exp, tpe, _, loc) =>
          for {
            e <- visitExp(exp, env0)
          } yield {
            val eff = e.eff
            Expression.Unary(op, e, tpe, eff, loc)
          }

        /**
          * Binary Expression.
          */
        case Expression.Binary(op, exp1, exp2, tpe, _, loc) =>
          for {
            e1 <- visitExp(exp1, env0)
            e2 <- visitExp(exp2, env0)
          } yield {
            // The effects of e1 happen before the effects of e2.
            val eff = e1.eff seq e2.eff
            Expression.Binary(op, exp1, exp2, tpe, eff, loc)
          }

        /**
          * Let Expression.
          */
        case Expression.Let(sym, exp1, exp2, tpe, _, loc) =>
          for {
            e1 <- visitExp(exp1, env0)
            e2 <- visitExp(exp2, env0 + (sym -> e1.eff.restrict))
          } yield {
            // The effects of e1 happen before e2.
            val eff = e1.eff seq e2.eff
            Expression.Let(sym, exp1, exp2, tpe, eff, loc)
          }

        /**
          * LetRec Expression.
          */
        case Expression.LetRec(sym, exp1, exp2, tpe, _, loc) =>
          for {
            e1 <- visitExp(exp1, env0)
            e2 <- visitExp(exp2, env0 + (sym -> e1.eff.restrict))
          } yield {
            // The effects of e1 happen before e2.
            val eff = e1.eff seq e2.eff
            Expression.LetRec(sym, exp1, exp2, tpe, eff, loc)
          }

        /**
          * If-Then-Else Expression.
          */
        case Expression.IfThenElse(exp1, exp2, exp3, tpe, _, loc) =>
          for {
            e1 <- visitExp(exp1, env0)
            e2 <- visitExp(exp2, env0)
            e3 <- visitExp(exp3, env0)
          } yield {
            // The effects of e1 happen before the effects of e2 and e3.
            val seq1 = e1.eff seq e2.eff
            val seq2 = e1.eff seq e3.eff

            // The effects of the overall expression is the least upper bound of the two effects above.
            val eff = seq1 lub seq2
            Expression.IfThenElse(e1, e2, e3, tpe, eff, loc)
          }

        /**
          * Match Expression.
          */
        case Expression.Match(exp, rules, tpe, _, loc) =>
          // Infer the effects of each rule.
          val rulesVal = rules.map {
            case MatchRule(pat, guard, body) =>
              for {
                g <- visitExp(guard, env0)
                b <- visitExp(body, env0)
              } yield {
                MatchRule(pat, g, b)
              }
          }

          // Infer the effects of the entire match expression.
          for {
            e <- visitExp(exp, env0)
            rs <- seqM(rulesVal)
          } yield {
            // Compute the effects of the match value expression.
            val matchEffect = e.eff

            // Compute the total effects of all the rules.
            val rulesEffect = rs.foldLeft(ast.Eff.Bot) {
              case (eacc, MatchRule(pat, guard, body)) =>
                // The effect of the guard happens before the effect of the body.
                eacc lub (guard.eff seq body.eff)
            }

            // The effect of the match value expression happens before any effects of the rules.
            val eff = e.eff seq rulesEffect
            Expression.Match(e, rs, tpe, eff, loc)
          }

        /**
          * Switch Expression.
          */
        case Expression.Switch(rules, tpe, _, loc) =>
          // Infer the effects of each rule.
          val rulesVal = rules.map {
            case (guard, body) =>
              for {
                g <- visitExp(guard, env0)
                b <- visitExp(body, env0)
              } yield {
                (g, b)
              }
          }

          // Infer the effects.
          for {
            rs <- seqM(rulesVal)
          } yield {
            val eff = rs.foldLeft(ast.Eff.Bot) {
              case (eacc, (guard, body)) =>
                eacc lub (guard.eff seq body.eff)
            }
            Expression.Switch(rs, tpe, eff, loc)
          }

        /**
          * Tag Expression.
          */
        case Expression.Tag(sym, tag, exp, tpe, _, loc) =>
          for {
            e <- visitExp(exp, env0)
          } yield {
            val eff = exp.eff
            Expression.Tag(sym, tag, e, tpe, eff, loc)
          }

        /**
          * Tuple Expression.
          */
        case Expression.Tuple(elms, tpe, _, loc) =>
          for {
            es <- seqM(elms.map(e => visitExp(e, env0)))
          } yield {
            // The effects of the each element expression happen in sequence.
            val eff = es.foldLeft(ast.Eff.Bot) {
              case (eacc, e) => eacc seq e.eff
            }
            Expression.Tuple(elms, tpe, eff, loc)
          }
        
        /**
          * ArrayLit Expression.
          */
        case Expression.ArrayLit(elms, tpe, _, loc) =>
          for {
            es <- seqM(elms.map(e => visitExp(e, env0)))
          } yield {
            val eff = es.foldLeft(ast.Eff.Bot) {
              case(eacc, e) => eacc seq e.eff
            }
            Expression.ArrayLit(elms, tpe, eff, loc)
          }

        /**
          * ArrayNew Expression.
          */
        case Expression.ArrayNew(elm, len, tpe, _, loc) =>
          for {
            e <- visitExp(elm, env0)
            ln <- visitExp(len, env0)
          } yield {
            val eff = elm.eff seq len.eff
            Expression.ArrayNew(e, ln, tpe, eff, loc)
          }


        /**
          * ArrayLoad Expression.
          */
        case Expression.ArrayLoad(base, index, tpe, _, loc) =>
          for {
            b <- visitExp(base, env0)
            i <- visitExp(index, env0)
          } yield {
            val eff = base.eff seq index.eff
            Expression.ArrayLoad(b, i, tpe, eff, loc)
          }

        /**
          * ArrayStore Expression.
          */
        case Expression.ArrayStore(base, index, elm, tpe, _, loc) =>
          for {
          b <- visitExp(base, env0)
          i <- visitExp(index, env0)
          e <- visitExp(elm, env0)
        } yield {
          val eff = base.eff seq index.eff seq  elm.eff
          Expression.ArrayStore(b, i, e, tpe, eff, loc)
        }

        /**
          * ArrayLength Expression.
          */
        case Expression.ArrayLength(base, tpe, _, loc) =>
          for {
            b <- visitExp(base, env0)
          } yield {
            val eff = base.eff
            Expression.ArrayLength(b, tpe, eff, loc)
          }

        /**
          * ArraySlice Expression.
          */
        case Expression.ArraySlice(base, startIndex, endIndex, tpe, _, loc) =>
          for {
            b <- visitExp(base, env0)
            i1 <- visitExp(startIndex, env0)
            i2 <- visitExp(endIndex, env0)
          } yield {
            val eff = base.eff seq startIndex.eff seq  endIndex.eff
            Expression.ArraySlice(b, i1, i2, tpe, eff, loc)
          }

        /**
          * VectorLit Expression.
          */
        case Expression.VectorLit(elms, tpe, _, loc) =>
          for (
            es <- seqM(elms.map(e => visitExp(e, env0)))
          ) yield {
            val eff = es.foldLeft(ast.Eff.Bot) {
              case (eacc, e) => eacc seq e.eff
            }
            Expression.VectorLit(elms, tpe, eff, loc)
          }

        /**
          * VectorNew Expression
          * */
        case Expression.VectorNew(elm, len, tpe, _, loc) =>
          for {
            e <- visitExp(elm, env0)
          } yield {
            val eff = elm.eff
            Expression.VectorNew(e, len, tpe, eff, loc)
          }

        /**
          * VectorLoad Expression
          * */
        case Expression.VectorLoad(base, index, tpe, _, loc) =>
          for {
            b <- visitExp(base, env0)
          } yield {
            val eff = base.eff
            Expression.VectorLoad(b, index, tpe, eff, loc)
          }

          /**
            * VectorStore Expression
            * */
        case Expression.VectorStore(base, index, elm, tpe, _, loc) =>
          for {
            b <- visitExp(base, env0)
            e <- visitExp(elm, env0)
          } yield {
            val eff = base.eff seq elm.eff
            Expression.VectorStore(b, index, e, tpe, eff, loc)
          }

          /**
            * VectorLength Expression
            * */
        case Expression.VectorLength(base, tpe, _, loc) =>
          for {
            b <- visitExp(base, env0)
          } yield {
            val eff = base.eff
            Expression.VectorLength(b, tpe, eff, loc)
          }

          /**
            * VectorSlice Expression
            * */
        case Expression.VectorSlice(base, startIndex, endIndex, tpe, _, loc) =>
          for {
            b <- visitExp(base, env0)
            i2 <- visitExp(endIndex, env0)
          } yield {
            val eff = base.eff seq endIndex.eff
            Expression.VectorSlice(b, startIndex, i2, tpe, eff, loc)
          }

          /**
            *  Unique Expression
            * */
        case Expression.Unique(exp, tpe, _, loc) =>
          for {
            e <- visitExp(exp, env0)
          } yield {
            val eff = exp.eff
            Expression.Unique(e, tpe, eff, loc)
          }

            /**
          * Reference Expression.
          */
        case Expression.Ref(exp, tpe, eff, loc) =>
          for {
            e <- visitExp(exp, env0)
          } yield {
            // TODO: [Effects]: Ref
            Expression.Ref(e, tpe, eff, loc)
          }

        /**
          * Dereference Expression.
          */
        case Expression.Deref(exp, tpe, eff, loc) =>
          for {
            e <- visitExp(exp, env0)
          } yield {
            // TODO: [Effects]: Deref
            Expression.Deref(e, tpe, eff, loc)
          }

        /**
          * Assignment Expression.
          */
        case Expression.Assign(exp1, exp2, tpe, eff, loc) =>
          for {
            e1 <- visitExp(exp1, env0)
            e2 <- visitExp(exp2, env0)
          } yield {
            // TODO: [Effects]: Assign
            Expression.Assign(e1, e2, tpe, eff, loc)
          }

        /**
          * HandleWith Expression.
          */
        case Expression.HandleWith(exp, bindings, tpe, eff, loc) => ??? // TODO

        /**
          * Existential Expression.
          */
        case Expression.Existential(fparam, exp, _, loc) =>
          // An existential expression must be pure.
          for {
            e <- visitExp(exp, env0)
            f <- assertPure(e)
          } yield {
            Expression.Existential(fparam, exp, f, loc)
          }

        /**
          * Universal Expression.
          */
        case Expression.Universal(fparam, exp, _, loc) =>
          // A universal expression must be pure.
          for {
            e <- visitExp(exp, env0)
            f <- assertPure(e)
          } yield {
            Expression.Universal(fparam, exp, f, loc)
          }

        /**
          * Ascribe Expression.
          */
        case Expression.Ascribe(exp, tpe, eff, loc) =>
          // An ascribe expression is sound; the effect system checks that the declared effect matches the inferred effect.
          for {
            e <- visitExp(exp, env0)
            _ <- assertLeq(exp, eff)
          } yield {
            Expression.Ascribe(e, tpe, eff, loc)
          }

        /**
          * Ascribe Expression.
          */
        case Expression.Cast(exp, tpe, eff, loc) =>
          // An cast expression is unsound; the effect system assumes the declared effect is correct.
          for {
            e <- visitExp(exp, env0)
          } yield {
            Expression.Cast(e, tpe, eff, loc)
          }

        /**
          * Native Constructor Expression.
          */
        case Expression.NativeConstructor(constructor, args, tpe, _, loc) =>
          // A native constructor can have any effect.
          val eff = ast.Eff.Top
          Expression.NativeConstructor(constructor, args, tpe, eff, loc).toSuccess

        /**
          * Native Field Expression.
          */
        case Expression.NativeField(field, tpe, _, loc) =>
          // A native field can have any effect.
          val eff = ast.Eff.Top
          Expression.NativeField(field, tpe, eff, loc).toSuccess

        /**
          * Native Method Expression.
          */
        case Expression.NativeMethod(method, args, tpe, _, loc) =>
          // A native method can have any effect.
          val eff = ast.Eff.Top
          Expression.NativeMethod(method, args, tpe, eff, loc).toSuccess

        /**
          * User Error Expression.
          */
        case Expression.UserError(tpe, _, loc) =>
          // Unsoundly assume that a user error exception has no effect.
          Expression.UserError(tpe, ast.Eff.Pure, loc).toSuccess
      }

      visitExp(exp0, initialEnv)
    }
  }

  /**
    * Returns [[Success]] with the bottom effect if the given expression `e0` has the bottom effect.
    *
    * Otherwise returns [[Failure]] with an [[EffectError]].
    */
  private def assertPure(e0: Expression): Validation[ast.Eff, EffectError] = assertLeq(e0, ast.Eff.Pure)

  /**
    * Returns [[Success]] with the given effect `eff` if the effect of the
    * given expression `e0` is fully contained in the given effect `eff`.
    *
    * Otherwise returns [[Failure]] with an [[EffectError]].
    */
  private def assertLeq(e0: Expression, eff: ast.Eff): Validation[ast.Eff, EffectError] = {
    if (e0.eff leq eff)
      eff.toSuccess
    else
      EffectError(eff, e0.eff, e0.loc).toFailure
  }

}

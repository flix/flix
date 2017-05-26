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
import ca.uwaterloo.flix.util.vt.VirtualString.{Line, NewLine, Red}
import ca.uwaterloo.flix.util.vt.VirtualTerminal

object Effects extends Phase[TypedAst.Root, TypedAst.Root] {

  // TODO: The values of ANF becomes more clear when we start to consider the order of effects.

  /**
    * TODO: DOC
    *
    * @param loc the location where the error occurred.
    */
  case class EffectError(loc: SourceLocation) extends CompilationError {
    val kind: String = "Effect Error"
    val source: SourceInput = loc.source
    val message: VirtualTerminal = {
      val vt = new VirtualTerminal()
      vt << Line(kind, source.format) << NewLine
      vt << ">> Effect Error" << NewLine
    }
  }


  /**
    * Performs effect inference on the given AST `root`.
    */
  def run(root: TypedAst.Root)(implicit flix: Flix): Validation[TypedAst.Root, EffectError] = {
    // TODO

    // TODO: EffectParam

    root.toSuccess
  }

  object Expressions {

    /**
      * Infers the effects of the given expression `exp0`.
      */
    def infer(exp0: Expression, root: TypedAst.Root): Validation[Expression, EffectError] = {
      /**
        * Local visitor.
        */
      // TODO: Does this need to happen inside validation or not?
      def visit(e0: Expression): Validation[Expression, EffectError] = exp0 match {
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


        //          case class Wild(tpe: Type, eff: Eff, loc: SourceLocation) extends TypedAst.Expression
        //
        //          case class Var(sym: Symbol.VarSym, tpe: Type, eff: Eff, loc: SourceLocation) extends TypedAst.Expression
        //
        //          case class Ref(sym: Symbol.DefnSym, tpe: Type, eff: Eff, loc: SourceLocation) extends TypedAst.Expression
        //
        //          case class Hook(hook: Ast.Hook, tpe: Type, eff: Eff, loc: SourceLocation) extends TypedAst.Expression
        //
        //          case class Lambda(args: List[TypedAst.FormalParam], body: TypedAst.Expression, tpe: Type, eff: Eff, loc: SourceLocation) extends TypedAst.Expression
        //
        //          case class Apply(exp: TypedAst.Expression, args: List[TypedAst.Expression], tpe: Type, eff: Eff, loc: SourceLocation) extends TypedAst.Expression


        case Expression.Apply(lambda, args, tpe, _, loc) =>
          val e = visit(lambda)
          val es = args.map(visit)

          // The effect is the effect of the individual arguments and the latent effect of the lambda.
          val eff = ???
          //Expression.Apply(e, es, tpe, eff, loc)
          ???


        /**
          * Unary Expressions.
          */
        case Expression.Unary(op, exp, tpe, _, loc) =>
          for {
            e <- visit(exp)
          } yield {
            val eff = e.eff
            Expression.Unary(op, e, tpe, eff, loc)
          }

        /**
          * Binary Expressions.
          */
        case Expression.Binary(op, exp1, exp2, tpe, _, loc) =>
          for {
            e1 <- visit(exp1)
            e2 <- visit(exp2)
          } yield {
            // The effects of e1 happen before the effects of e2.
            val eff = Eff.seq(e1.eff, e2.eff)
            Expression.Binary(op, exp1, exp2, tpe, eff, loc)
          }



        //
        //          case class Let(sym: Symbol.VarSym, exp1: TypedAst.Expression, exp2: TypedAst.Expression, tpe: Type, eff: Eff, loc: SourceLocation) extends TypedAst.Expression
        //
        //          case class LetRec(sym: Symbol.VarSym, exp1: TypedAst.Expression, exp2: TypedAst.Expression, tpe: Type, eff: Eff, loc: SourceLocation) extends TypedAst.Expression
        //

        /**
          * If-Then-Else Expressions.
          */
        case Expression.IfThenElse(exp1, exp2, exp3, tpe, _, loc) =>
          for {
            e1 <- visit(exp1)
            e2 <- visit(exp2)
            e3 <- visit(exp3)
          } yield {
            // The effects of e1 happen before the effects of e2 and e3.
            val seq1 = Eff.seq(e1.eff, e2.eff)
            val seq2 = Eff.seq(e1.eff, e3.eff)

            // The effects of the overall expression is the least upper bound of the two effects above.
            val eff = Eff.lub(seq1, seq2)
            Expression.IfThenElse(e1, e2, e3, tpe, eff, loc)
          }

        //
        //          case class Match(exp: TypedAst.Expression, rules: List[TypedAst.MatchRule], tpe: Type, eff: Eff, loc: SourceLocation) extends TypedAst.Expression
        //
        //          case class Switch(rules: List[(TypedAst.Expression, TypedAst.Expression)], tpe: Type, eff: Eff, loc: SourceLocation) extends TypedAst.Expression


        //
        //          case class Tag(sym: Symbol.EnumSym, tag: String, exp: TypedAst.Expression, tpe: Type, eff: Eff, loc: SourceLocation) extends TypedAst.Expression

        /**
          * Tag Expressions.
          */
        case Expression.Tag(sym, tag, exp, tpe, _, loc) =>
          for {
            e <- visit(exp)
          } yield {
            val eff = exp.eff
            Expression.Tag(sym, tag, e, tpe, eff, loc)
          }


        //
        //          case class Tuple(elms: List[TypedAst.Expression], tpe: Type, eff: Eff, loc: SourceLocation) extends TypedAst.Expression
        //
        //          case class Existential(fparam: TypedAst.FormalParam, exp: TypedAst.Expression, eff: Eff, loc: SourceLocation) extends TypedAst.Expression {
        //            def tpe: Type = Type.Bool
        //          }
        //
        //          case class Universal(fparam: TypedAst.FormalParam, exp: TypedAst.Expression, eff: Eff, loc: SourceLocation) extends TypedAst.Expression {
        //            def tpe: Type = Type.Bool
        //          }
        //
        //          case class NativeConstructor(constructor: Constructor[_], args: List[TypedAst.Expression], tpe: Type, eff: Eff, loc: SourceLocation) extends TypedAst.Expression
        //
        //          case class NativeField(field: Field, tpe: Type, eff: Eff, loc: SourceLocation) extends TypedAst.Expression
        //
        //          case class NativeMethod(method: Method, args: List[TypedAst.Expression], tpe: Type, eff: Eff, loc: SourceLocation) extends TypedAst.Expression
        //
        //          case class UserError(tpe: Type, eff: Eff, loc: SourceLocation) extends TypedAst.Expression


        case _ => ??? // TODO: Remove once all cases have been added.

      }

      visit(exp0)
    }

  }

}

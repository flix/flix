/*
 * Copyright 2015-2016 Ming-Ho Yee
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
import ca.uwaterloo.flix.language.{CompilationError, GenSym}
import ca.uwaterloo.flix.language.ast.SimplifiedAst.Expression
import ca.uwaterloo.flix.language.ast.{Ast, SimplifiedAst, Symbol}
import ca.uwaterloo.flix.util.InternalCompilerException
import ca.uwaterloo.flix.util.Validation
import ca.uwaterloo.flix.util.Validation._

import scala.collection.mutable

object LambdaLift extends Phase[SimplifiedAst.Root, SimplifiedAst.Root] {

  /**
    * Mutable map of top level definitions.
    */
  private type TopLevel = mutable.Map[Symbol.DefnSym, SimplifiedAst.Definition.Constant]

  /**
    * Performs lambda lifting on all definitions in the AST.
    */
  def run(root: SimplifiedAst.Root)(implicit flix: Flix): Validation[SimplifiedAst.Root, CompilationError] = {
    implicit val _ = flix.genSym

    val t = System.nanoTime()

    // A mutable map to hold lambdas that are lifted to the top level.
    val m: TopLevel = mutable.Map.empty

    val definitions = root.definitions.map {
      case (name, decl) => name -> lift(decl, m)
    }
    val properties = root.properties.map(p => lift(p, m))

    // Return the updated AST root.
    val e = System.nanoTime() - t
    root.copy(definitions = definitions ++ m, properties = properties, time = root.time.copy(lambdaLift = e)).toSuccess
  }

  /**
    * Performs lambda lifting on the given declaration `decl`.
    *
    * The definition's expression is closure converted, and then lifted definitions are added to the mutable map `m`.
    * The updated definition is then returned.
    */
  private def lift(decl: SimplifiedAst.Definition.Constant, m: TopLevel)(implicit genSym: GenSym): SimplifiedAst.Definition.Constant = {
    val convExp = ClosureConv.convert(decl.exp)
    val liftExp = lift(convExp, m, Some(decl.sym))
    decl.copy(exp = liftExp)
  }

  /**
    * Performs lambda lifting on the given property `prop`.
    *
    * The property's expression is closure converted, and then the lifted definitions are added to the mutable map `m`.
    * The updated definition is then returned.
    */
  private def lift(prop: SimplifiedAst.Property, m: TopLevel)(implicit genSym: GenSym): SimplifiedAst.Property = {
    val convExp = ClosureConv.convert(prop.exp)
    val liftExp = lift(convExp, m, None)
    prop.copy(exp = liftExp)
  }

  /**
    * Performs lambda lifting on the given expression `exp0`.
    *
    * Adds new top-level definitions to the mutable map `m`.
    */
  private def lift(exp0: Expression, m: TopLevel, symOpt: Option[Symbol.DefnSym])(implicit genSym: GenSym): Expression = {
    def visit(e: Expression): Expression = e match {
      case Expression.Unit => e
      case Expression.True => e
      case Expression.False => e
      case Expression.Char(lit) => e
      case Expression.Float32(lit) => e
      case Expression.Float64(lit) => e
      case Expression.Int8(lit) => e
      case Expression.Int16(lit) => e
      case Expression.Int32(lit) => e
      case Expression.Int64(lit) => e
      case Expression.BigInt(lit) => e
      case Expression.Str(lit) => e
      case Expression.LoadBool(n, o) => e
      case Expression.LoadInt8(b, o) => e
      case Expression.LoadInt16(b, o) => e
      case Expression.LoadInt32(b, o) => e
      case Expression.StoreBool(b, o, v) => e
      case Expression.StoreInt8(b, o, v) => e
      case Expression.StoreInt16(b, o, v) => e
      case Expression.StoreInt32(b, o, v) => e
      case Expression.Var(sym, tpe, loc) => e
      case Expression.Ref(name, tpe, loc) => e

      case Expression.Lambda(fparams, body, tpe, loc) =>
        // Lift the lambda to a top-level definition, and replacing the Lambda expression with a Ref.

        // First, recursively visit the lambda body, lifting any inner lambdas.
        val liftedBody = visit(body)

        // Generate a fresh symbol for the definition.
        val freshSymbol = symOpt match {
          case None => Symbol.freshDefnSym("none") // TODO: This seems suspicious.
          case Some(oldSym) => Symbol.freshDefnSym(oldSym)
        }

        // Create a new top-level definition, using the fresh name and lifted body.
        val defn = SimplifiedAst.Definition.Constant(Ast.Annotations(Nil), freshSymbol, fparams, liftedBody, isSynthetic = true, tpe, loc)

        // Update the map that holds newly-generated definitions
        m += (freshSymbol -> defn)

        // Finally, replace this current Lambda node with a Ref to the newly-generated name.
        SimplifiedAst.Expression.Ref(freshSymbol, tpe, loc)

      case Expression.Hook(hook, tpe, loc) => e
      case Expression.MkClosureRef(ref, freeVars, tpe, loc) => e

      case SimplifiedAst.Expression.MkClosure(lambda, freeVars, tpe, loc) =>
        // Replace the MkClosure node with a MkClosureRef node, since the Lambda has been replaced by a Ref.
        visit(lambda) match {
          case ref: SimplifiedAst.Expression.Ref =>
            SimplifiedAst.Expression.MkClosureRef(ref, freeVars, tpe, loc)
          case _ => throw InternalCompilerException(s"Unexpected expression: '$lambda'.")
        }

      case Expression.ApplyRef(name, args, tpe, loc) =>
        Expression.ApplyRef(name, args.map(visit), tpe, loc)
      case Expression.ApplyTail(name, formals, args, tpe, loc) =>
        Expression.ApplyTail(name, formals, args.map(visit), tpe, loc)
      case Expression.ApplyHook(hook, args, tpe, loc) =>
        Expression.ApplyHook(hook, args.map(visit), tpe, loc)
      case Expression.Apply(exp, args, tpe, loc) =>
        Expression.Apply(visit(exp), args.map(visit), tpe, loc)
      case Expression.Unary(op, exp, tpe, loc) =>
        Expression.Unary(op, visit(exp), tpe, loc)
      case Expression.Binary(op, exp1, exp2, tpe, loc) =>
        Expression.Binary(op, visit(exp1), visit(exp2), tpe, loc)
      case Expression.IfThenElse(exp1, exp2, exp3, tpe, loc) =>
        Expression.IfThenElse(visit(exp1), visit(exp2), visit(exp3), tpe, loc)
      case Expression.Let(sym, exp1, exp2, tpe, loc) =>
        Expression.Let(sym, visit(exp1), visit(exp2), tpe, loc)
      case Expression.LetRec(sym, exp1, exp2, tpe, loc) =>
        Expression.LetRec(sym, visit(exp1), visit(exp2), tpe, loc)
      case Expression.Is(sym, tag, exp, loc) =>
        Expression.Is(sym, tag, visit(exp), loc)
      case Expression.Tag(enum, tag, exp, tpe, loc) =>
        Expression.Tag(enum, tag, visit(exp), tpe, loc)
      case Expression.Untag(sym, tag, exp, tpe, loc) =>
        Expression.Untag(sym, tag, visit(exp), tpe, loc)
      case Expression.Index(exp, offset, tpe, loc) =>
        Expression.Index(visit(exp), offset, tpe, loc)
      case Expression.Tuple(elms, tpe, loc) =>
        Expression.Tuple(elms.map(visit), tpe, loc)
      case Expression.Existential(params, exp, loc) =>
        Expression.Existential(params, visit(exp), loc)
      case Expression.Universal(params, exp, loc) =>
        Expression.Universal(params, visit(exp), loc)
      case Expression.NativeConstructor(constructor, args, tpe, loc) =>
        val es = args.map(e => visit(e))
        Expression.NativeConstructor(constructor, es, tpe, loc)
      case Expression.NativeField(field, tpe, loc) => e
      case Expression.NativeMethod(method, args, tpe, loc) =>
        val es = args.map(e => visit(e))
        Expression.NativeMethod(method, es, tpe, loc)
      case Expression.UserError(tpe, loc) => e
      case Expression.MatchError(tpe, loc) => e
      case Expression.SwitchError(tpe, loc) => e
    }

    visit(exp0)
  }

}

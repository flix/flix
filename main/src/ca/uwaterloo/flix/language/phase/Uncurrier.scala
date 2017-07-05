/*
 * Copyright 2017 Jason Mittertreiner
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
import ca.uwaterloo.flix.language.ast.SimplifiedAst.Expression._
import ca.uwaterloo.flix.language.ast.SimplifiedAst.{LoadExpression, StoreExpression}
import ca.uwaterloo.flix.language.{CompilationError, GenSym}
import ca.uwaterloo.flix.language.ast.{Type, _}
import ca.uwaterloo.flix.util.{InternalCompilerException, Validation}
import ca.uwaterloo.flix.util.Validation._

/**
  * A phase that merges curried definitions into definitions which take multiple
  * arguments.
  *
  * For example, consider
  *
  *   |def add(x: Int): Int -> Int = {
  *   |   z -> x + z
  *   |}
  *   |
  *   |def a: Int = (add(3))(4)
  *
  * The uncurrier will create a function
  *
  *   |def add$uncurried(x: Int, z: Int): Int = {
  *   |    x + z
  *   |}
  *
  * And change the call site to
  *   |def a: Int = add$uncurried(3,4)
  */
object Uncurrier extends Phase[SimplifiedAst.Root, SimplifiedAst.Root] {

  def run(root: SimplifiedAst.Root)(implicit flix: Flix): Validation[SimplifiedAst.Root, CompilationError] = {
    implicit val _ = flix.genSym
    val startTime = System.nanoTime()

    val (defns, uncurriedSyms) = root.definitions.foldLeft(Map.empty[Symbol.DefnSym, SimplifiedAst.Definition.Constant],
      Map.empty[Symbol.DefnSym, Map[Int, Symbol.DefnSym]])((acc, defnEntry) => {
      val (defs, uncurriedSyms) = Definitions.mkUncurriedDef(defnEntry._1, defnEntry._2, 1, acc._2)
      (acc._1 ++ defs, uncurriedSyms)
    })

    val uncurriedDef = defns map { case (k, v) => k -> Definitions.uncurry(v, uncurriedSyms, root) }

    val currentTime = System.nanoTime()
    val time = root.time.copy(uncurrier = currentTime - startTime)
    root.copy(definitions = uncurriedDef).copy(time = time).toSuccess
  }

  object Definitions {
    /**
      * Creates all the uncurried versions of a function
      *
      * @return the update functions, as well as a map from functions to their uncurried versions by level of
      *         "uncurriedness"
      */
    def mkUncurriedDef(sym: Symbol.DefnSym,
                       curriedDef: SimplifiedAst.Definition.Constant, uncurryLevel: Int,
                       newSyms0: Map[Symbol.DefnSym, Map[Int, Symbol.DefnSym]])(implicit genSym: GenSym)
    : (Map[Symbol.DefnSym, SimplifiedAst.Definition.Constant], Map[Symbol.DefnSym, Map[Int, Symbol.DefnSym]]) = {
      curriedDef.exp match {
        // If the body is a lambda, then we have a curried function
        // definition, so we need to create an uncurried version.
        // To do this, we'll take make the body of the function the body
        // of the lambda, then add the arguments of the lambda to the
        // formal parameters
        case Lambda(args, body, _, _) =>
          // Create a new definition for our function
          val uncurriedSym = Symbol.freshDefnSym(sym)

          // Insert the created symbol into our map of uncurried symbols
          val newSyms1 = newSyms0.get(sym) match {
            case Some(e) => newSyms0 + (sym -> (e + (uncurryLevel -> uncurriedSym)))
            case None => newSyms0 + (sym -> (Map.empty[Int, Symbol.DefnSym] + (uncurryLevel -> uncurriedSym)))
          }

          // Create new VarSyms for our function
          val newFormals = (curriedDef.formals ::: args).map(f => f.copy(sym = Symbol.freshVarSym(f.sym)))
          // Replace the old VarSyms in the body
          val varMapping = (curriedDef.formals ::: args).map(f => f.sym).zip(newFormals.map(f => f.sym)).toMap
          val newBody = Expressions.substitute(body, varMapping)

          val unCurriedDef = SimplifiedAst.Definition.Constant(
            curriedDef.ann,
            curriedDef.mod,
            uncurriedSym,
            newFormals,
            newBody,
            curriedDef.isSynthetic,
            uncurryType(curriedDef.tpe),
            curriedDef.loc)

          // Create the other levels of uncurrying
          val (uncurriedDefs, newSyms2) = mkUncurriedDef(sym, unCurriedDef, uncurryLevel + 1, newSyms1)
          (uncurriedDefs + (uncurriedSym -> unCurriedDef), newSyms2)
        case _ => (Map.empty[Symbol.DefnSym, SimplifiedAst.Definition.Constant] + (sym -> curriedDef), newSyms0)
      }
    }

    /**
      * Uncurry a definition constant
      */
    def uncurry(cst: SimplifiedAst.Definition.Constant, newSyms0: Map[Symbol.DefnSym, Map[Int, Symbol.DefnSym]], root: SimplifiedAst.Root): SimplifiedAst.Definition.Constant = {
      cst.copy(exp = Expressions.uncurry(cst.exp, newSyms0, root))
    }
  }

  object Expressions {
    /**
      * Recursively replace all the variables in exp0 with new variables using the
      * pairs in replaceSyms. This ensure that the new functions created don't
      * reference the variables of the original functions.
      *
      * @param exp0 The expression to replace
      * @param env0 The v
      * @return
      */
    def substitute(exp0: SimplifiedAst.Expression, env0: Map[Symbol.VarSym, Symbol.VarSym])(implicit genSym: GenSym): SimplifiedAst.Expression = {
      def replace(oldSym: Symbol.VarSym): Symbol.VarSym =
        env0.getOrElse(oldSym, oldSym)

      exp0 match {
        case e: LoadExpression => e
        case e: StoreExpression => e
        case Unit => Unit
        case True => True
        case False => False
        case Char(lit) => Char(lit)
        case Float32(lit) => Float32(lit)
        case Float64(lit) => Float64(lit)
        case Int8(lit) => Int8(lit)
        case Int16(lit) => Int16(lit)
        case Int32(lit) => Int32(lit)
        case Int64(lit) => Int64(lit)
        case BigInt(lit) => BigInt(lit)
        case Str(lit) => Str(lit)
        case Var(sym, tpe, loc) => Var(replace(sym), tpe, loc)
        case e: Ref => e
        // For lambdas, we have to replace the formal arguments with new formal
        // parameters
        case Lambda(args, body, tpe, loc) =>
          val newFormals = args.map(f => f.copy(sym = Symbol.freshVarSym(f.sym)))
          val newMapping = args.map(f => f.sym).zip(newFormals.map(f => f.sym)).toMap
          Lambda(newFormals, substitute(body, env0 ++ newMapping), tpe, loc)
        case e: Hook => e
        case MkClosure(lambda, freeVars, tpe, loc) => MkClosure(substitute(lambda, env0).asInstanceOf[SimplifiedAst.Expression.Lambda], freeVars, tpe, loc)
        case e: MkClosureRef => e
        case ApplyRef(sym, args, tpe, loc) => ApplyRef(sym, args.map(substitute(_, env0)), tpe, loc)
        case ApplyTail(sym, formals, actuals, tpe, loc) => ApplyTail(sym, formals, actuals.map(substitute(_, env0)), tpe, loc)
        case ApplyHook(hook, args, tpe, loc) => ApplyHook(hook, args.map(substitute(_, env0)), tpe, loc)
        case Apply(exp, args, tpe, loc) => Apply(substitute(exp, env0), args.map(a => substitute(a, env0)), tpe, loc)
        case Unary(op, exp, tpe, loc) => Unary(op, substitute(exp, env0), tpe, loc)
        case Binary(op, exp1, exp2, tpe, loc) => Binary(op, substitute(exp1, env0), substitute(exp2, env0), tpe, loc)
        case IfThenElse(exp1, exp2, exp3, tpe, loc) => IfThenElse(substitute(exp1, env0), substitute(exp1, env0), substitute(exp3, env0), tpe, loc)
        case Let(sym, exp1, exp2, tpe, loc) => Let(replace(sym), substitute(exp1, env0), substitute(exp2, env0), tpe, loc)
        case LetRec(sym, exp1, exp2, tpe, loc) => LetRec(replace(sym), substitute(exp1, env0), substitute(exp2, env0), tpe, loc)
        case Is(sym, tag, exp, loc) => Is(sym, tag, substitute(exp, env0), loc)
        case Tag(sym, tag, exp, tpe, loc) => Tag(sym, tag, substitute(exp, env0), tpe, loc)
        case Untag(sym, tag, exp, tpe, loc) => Untag(sym, tag, substitute(exp, env0), tpe, loc)
        case Index(base, offset, tpe, loc) => Index(substitute(base, env0), offset, tpe, loc)
        case Tuple(elms, tpe, loc) => Tuple(elms.map(e => substitute(e, env0)), tpe, loc)
        case Existential(fparam, exp, loc) => Existential(fparam, substitute(exp, env0), loc)
        case Universal(fparam, exp, loc) => Universal(fparam, substitute(exp, env0), loc)
        case NativeConstructor(constructor, args, tpe, loc) => NativeConstructor(constructor, args.map(a => substitute(a, env0)), tpe, loc)
        case e: NativeField => e
        case NativeMethod(method, args, tpe, loc) => NativeMethod(method, args.map(a => substitute(a, env0)), tpe, loc)
        case e: UserError => e
        case e: MatchError => e
        case e: SwitchError => e
      }
    }

    /**
      * Uncurry an expression
      *
      * @return The expression modified to have all curried calls uncurried
      */
    def uncurry(exp0: SimplifiedAst.Expression, newSyms: Map[Symbol.DefnSym, Map[Int, Symbol.DefnSym]], root: SimplifiedAst.Root): SimplifiedAst.Expression = exp0 match {
      case _: LoadExpression => exp0
      case _: StoreExpression => exp0
      case Unit => exp0
      case True => exp0
      case False => exp0
      case Char(lit) => exp0
      case Float32(lit) => exp0
      case Float64(lit) => exp0
      case Int8(lit) => exp0
      case Int16(lit) => exp0
      case Int32(lit) => exp0
      case Int64(lit) => exp0
      case BigInt(lit) => exp0
      case Str(lit) => exp0
      case Var(sym, tpe, loc) => exp0
      case Ref(sym, tpe, loc) => exp0
      case Lambda(args, body, tpe, loc) => Lambda(args, uncurry(body, newSyms, root), tpe, loc)
      case Hook(hook, tpe, loc) => exp0
      case MkClosure(lambda, freeVars, tpe, loc) => exp0
      case MkClosureRef(ref, freeVars, tpe, loc) => exp0
      case ApplyRef(sym, args, tpe, loc) => exp0
      case ApplyTail(sym, formals, actuals, tpe, loc) => exp0
      case ApplyHook(hook, args, tpe, loc) => exp0
      case a: Apply =>
        val uncurryCount = maximalUncurry(a)
        uncurryN(exp0, uncurryCount) match {
        case Apply(Ref(sym1, tp1, loc1), args2, tpe2, loc2) =>
          newSyms.get (sym1) match {
            case Some (m) => m.get (uncurryCount) match {
              case Some (newSym) => {
                Apply(Ref(newSym, tp1, loc1), args2 map {
                  uncurry(_, newSyms, root)
                }, tpe2, loc2)
              }
              case None => a
            }
            case None => a
          }
        case _ => a
        }
      case Unary(op, exp, tpe, loc) => Unary(op, uncurry(exp, newSyms, root), tpe, loc)
      case Binary(op, exp1, exp2, tpe, loc) => Binary(op, uncurry(exp1, newSyms, root), uncurry(exp2, newSyms, root), tpe, loc)
      case IfThenElse(exp1, exp2, exp3, tpe, loc) => IfThenElse(uncurry(exp1, newSyms, root), uncurry(exp2, newSyms, root), uncurry(exp3, newSyms, root), tpe, loc)
      case Let(sym, exp1, exp2, tpe, loc) => Let(sym, uncurry(exp1, newSyms, root), uncurry(exp2, newSyms, root), tpe, loc)
      case LetRec(sym, exp1, exp2, tpe, loc) => LetRec(sym, uncurry(exp1, newSyms, root), uncurry(exp2, newSyms, root), tpe, loc)
      case Is(sym, tag, exp, loc) => Is(sym, tag, uncurry(exp, newSyms, root), loc)
      case Tag(sym, tag, exp, tpe, loc) => Tag(sym, tag, uncurry(exp, newSyms, root), tpe, loc)
      case Untag(sym, tag, exp, tpe, loc) => Untag(sym, tag, uncurry(exp, newSyms, root), tpe, loc)
      case Index(base, offset, tpe, loc) => exp0
      case Tuple(elms, tpe, loc) => Tuple(elms map {
        uncurry(_, newSyms, root)
      }, tpe, loc)
      case Existential(fparam, exp, loc) => Existential(fparam, uncurry(exp, newSyms, root), loc)
      case Universal(fparam, exp, loc) => Universal(fparam, uncurry(exp, newSyms, root), loc)
      case NativeConstructor(constructor, args, tpe, loc) => NativeConstructor(constructor, args map {
        uncurry(_, newSyms, root)
      }, tpe, loc)
      case NativeField(field, tpe, loc) => exp0
      case NativeMethod(method, args, tpe, loc) => NativeMethod(method, args map {
        uncurry(_, newSyms, root)
      }, tpe, loc)
      case UserError(tpe, loc) => exp0
      case MatchError(tpe, loc) => exp0
      case SwitchError(tpe, loc) => exp0
    }

    def uncurryN(exp0: SimplifiedAst.Expression, count: Int): SimplifiedAst.Expression = count match {
      case 0 => exp0
      case n => exp0 match {
        case Apply(exp2, args2, tpe2, loc2) =>
          val Apply(Ref(sym4, tpe4, loc4), args3, _, _) = uncurryN(exp2, n - 1)
          Apply(Ref(sym4, uncurryType(tpe4), loc4), args3 ::: args2, tpe2, loc2)
        case _ => throw InternalCompilerException(s"Can't uncurry expression : $exp0")
      }
    }

    /**
      * Counts the number of times a function can be uncurried
      */

    def maximalUncurry(exp0: SimplifiedAst.Expression.Apply): Int = exp0 match {
      case Apply(exp1, _, _, _) =>
        exp1 match {
          case _: LoadExpression => 0
          case _: StoreExpression => 0
          case Unit => 0
          case True => 0
          case False => 0
          case _: Char => 0
          case _: Float32 => 0
          case _: Float64 => 0
          case _: Int8 => 0
          case _: Int16 => 0
          case _: Int32 => 0
          case _: Int64 => 0
          case _: BigInt => 0
          case _: Str => 0
          case _: Var => 0
          case _: Ref => 0
          case _: Lambda => 0
          case _: Hook => 0
          case _: MkClosure => 0
          case _: MkClosureRef => 0
          case _: ApplyRef => 0
          case _: ApplyTail => 0
          case _: ApplyHook => 0
          case a: Apply => 1 + maximalUncurry(a)
          case _: Unary => 0
          case _: Binary => 0
          case _: IfThenElse => 0
          case _: Let => 0
          case _: LetRec => 0
          case _: Is => 0
          case _: Tag => 0
          case _: Untag => 0
          case _: Index => 0
          case _: Tuple => 0
          case _: Existential => 0
          case _: Universal => 0
          case _: NativeConstructor => 0
          case _: NativeField => 0
          case _: NativeMethod => 0
          case _: UserError => 0
          case _: MatchError => 0
          case _: SwitchError => 0
        }
    }
  }

    /**
      * Uncurry the type of a function. Given a type like a x b -> (c -> d), turn it
      * into a x b x c -> d
      *
      * @param tpe The type to uncurry
      * @return The uncurried type
      */
    def uncurryType(tpe: Type): Type = tpe match {
      case Type.Apply(Type.Arrow(len), ts) =>
        // We're given an application which looks like
        // List(From, From, From, To), where there are one are more From
        // types, and the result is the To type.
        //
        // When we are uncurrying, the To type will also be an apply, we then
        // transform List(From1, From2, List(From3, From4, To)) to
        // List(From1, From2, From3, From4, To)
        //
        val from = ts.take(ts.size - 1)
        val to = ts.last
        ts.last match {
          case Type.Apply(_, ts2) => Type.Apply(Type.Arrow(len + 1), from ::: ts2)
          case _ => throw InternalCompilerException(s"Cannot uncurry type $tpe")
        }
      case _ => throw InternalCompilerException(s"Cannot uncurry type $tpe")
    }
  }

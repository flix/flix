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
import ca.uwaterloo.flix.language.ast.Type.Arrow
import ca.uwaterloo.flix.language.{CompilationError, GenSym}
import ca.uwaterloo.flix.language.ast._
import ca.uwaterloo.flix.util.{InternalCompilerException, Validation}
import ca.uwaterloo.flix.util.Validation._

/**
  * A phase that uncurries function definitions and applications.
  *
  * For example, consider:
  *
  * def add(x: Int): Int -> Int = z -> x + z
  * def r: Int = add(3)(4)
  *
  * The uncurrier will create a function:
  *
  * def add$uncurried(x: Int, z: Int): Int = x + z
  *
  * And change the call site to:
  *
  * def r: Int = add$uncurried(3, 4)
  */
object Uncurrier extends Phase[SimplifiedAst.Root, SimplifiedAst.Root] {

  def run(root: SimplifiedAst.Root)(implicit flix: Flix): Validation[SimplifiedAst.Root, CompilationError] = {

    implicit val _ = flix.genSym
    val startTime = System.nanoTime()

    /*
     * We generate uncurried versions of each function definition. We
     * generate two mappings:
     * - a map of the created symbols to the created bodies
     * - a map of the old symbols to the corresponding uncurried definitions
     */
    val (createdDefs, defsToUncurriedDefs) = root.defs.foldLeft(
      Map.empty[Symbol.DefnSym, SimplifiedAst.Def],
      Map.empty[Symbol.DefnSym, Map[Int, Symbol.DefnSym]])((acc, defnEntry) => {
      val (defs, uncurriedSyms) = Defs.mkUncurriedDef(defnEntry._1, defnEntry._2)
      (acc._1 ++ defs, acc._2 ++ uncurriedSyms)
    })
    val defs1 = root.defs ++ createdDefs

    // Now using the uncurried version of the functions we made, we replace function calls in the actual definition
    // as needed
    val uncurriedDefs = defs1 map { case (k, v) => k -> Defs.uncurry(v, defsToUncurriedDefs) }

    val time = root.time.copy(uncurrier = System.nanoTime() - startTime)
    root.copy(defs = uncurriedDefs).copy(time = time).toSuccess
  }

  object Defs {
    /**
      * Insert a new function in to the uncurried definitions map
      */
    def insertSym(curriedSym: Symbol.DefnSym,
                  uncurriedSym: Symbol.DefnSym,
                  uncurriedLvl: Int,
                  defns: Map[Symbol.DefnSym, Map[Int, Symbol.DefnSym]]):
    Map[Symbol.DefnSym, Map[Int, Symbol.DefnSym]] =
      defns + (defns.get(curriedSym) match {
        case Some(e) => curriedSym -> (e + (uncurriedLvl -> uncurriedSym))
        case None => curriedSym -> (Map.empty + (uncurriedLvl -> uncurriedSym))
      })


    /**
      * Wrapper function to create all the uncurried versions of a function
      *
      * @return the update functions, as well as a map from functions to their uncurried versions by level of
      *         "uncurriedness"
      */
    def mkUncurriedDef(sym: Symbol.DefnSym,
                       curriedDef: SimplifiedAst.Def)
                      (implicit genSym: GenSym)
    : (Map[Symbol.DefnSym, SimplifiedAst.Def], Map[Symbol.DefnSym, Map[Int, Symbol.DefnSym]]) = {
      mkUncurriedDef(sym, curriedDef, 1)
    }

    /**
      * Creates all the uncurried versions of a function
      *
      * @return the update functions, as well as a map from functions to their uncurried versions by level of
      *         "uncurriedness"
      */
    def mkUncurriedDef(baseSym: Symbol.DefnSym,
                       curriedDef: SimplifiedAst.Def,
                       uncurryLevel: Int)
                      (implicit genSym: GenSym)
    : (Map[Symbol.DefnSym, SimplifiedAst.Def], Map[Symbol.DefnSym, Map[Int, Symbol.DefnSym]]) =
      curriedDef.exp match {
        // If the body is a lambda, then we have a curried function
        // definition, so we need to create an uncurried version.
        // To do this, we'll take make the body of the function the body
        // of the lambda, then add the arguments of the lambda to the
        // formal parameters
        case Lambda(args, body0, _, _) =>
          // Create a new definition for our function
          val uncurriedSym = Symbol.freshDefnSym(baseSym)


          // Create new VarSyms for our function
          val formals1 = (curriedDef.fparams ::: args).map(f => f.copy(sym = Symbol.freshVarSym(f.sym)))
          // Replace the old VarSyms in the body
          val varMapping = (curriedDef.fparams ::: args).map(_.sym).zip(formals1.map(_.sym)).toMap
          val body1 = Expressions.substitute(body0, varMapping)

          // Assemble the new definition
          val unCurriedDef = SimplifiedAst.Def(
            curriedDef.ann,
            curriedDef.mod,
            uncurriedSym,
            formals1,
            body1,
            uncurryType(curriedDef.tpe),
            curriedDef.loc)

          // Create the other levels of uncurrying
          val (uncurriedDefs, uncurriedMapping0) = mkUncurriedDef(baseSym, unCurriedDef, uncurryLevel + 1)
          // Insert the created symbol into our map of uncurried symbols
          val uncurriedMapping1 = insertSym(baseSym, uncurriedSym, uncurryLevel, uncurriedMapping0)
          val createdFunctions = uncurriedDefs + (uncurriedSym -> unCurriedDef)
          (createdFunctions, uncurriedMapping1)

        case _ => (Map.empty, Map.empty)
      }


    /**
      * Uncurry a definition constant
      */
    def uncurry(cst: SimplifiedAst.Def, newSyms0: Map[Symbol.DefnSym, Map[Int, Symbol.DefnSym]]): SimplifiedAst.Def = {
      cst.copy(exp = Expressions.uncurry(cst.exp, newSyms0))
    }
  }

  object Expressions {
    /**
      * Recursively replace all the variables in exp0 with new variables using the
      * pairs in replaceSyms. This ensure that the new functions created don't
      * reference the variables of the original functions.
      */
    def substitute(exp0: SimplifiedAst.Expression, env0: Map[Symbol.VarSym, Symbol.VarSym])(implicit genSym: GenSym): SimplifiedAst.Expression = {
      def replace(oldSym: Symbol.VarSym): Symbol.VarSym =
        env0.getOrElse(oldSym, oldSym)

      exp0 match {
        case Unit => Unit
        case True => True
        case False => False
        case e: Char => e
        case e: Float32 => e
        case e: Float64 => e
        case e: Int8 => e
        case e: Int16 => e
        case e: Int32 => e
        case e: Int64 => e
        case e: BigInt => e
        case e: Str => e
        case Var(sym, tpe, loc) => Var(replace(sym), tpe, loc)
        case e: Def => e
        // For lambdas, we have to replace the formal arguments with new formal
        // parameters
        case Lambda(args, body, tpe, loc) =>
          val newFormals = args.map(f => f.copy(sym = Symbol.freshVarSym(f.sym)))
          val newMapping = args.map(f => f.sym).zip(newFormals.map(f => f.sym)).toMap
          Lambda(newFormals, substitute(body, env0 ++ newMapping), tpe, loc)
        case e: Hook => e
        case LambdaClosure(lambda, freeVars, tpe, loc) => LambdaClosure(substitute(lambda, env0).asInstanceOf[SimplifiedAst.Expression.Lambda], freeVars, tpe, loc)
        case e: Closure => e
        case ApplyHook(hook, args, tpe, loc) => ApplyHook(hook, args.map(substitute(_, env0)), tpe, loc)
        case Apply(exp, args, tpe, loc) => Apply(substitute(exp, env0), args.map(a => substitute(a, env0)), tpe, loc)
        case Unary(sop, op, exp, tpe, loc) => Unary(sop, op, substitute(exp, env0), tpe, loc)
        case Binary(sop, op, exp1, exp2, tpe, loc) => Binary(sop, op, substitute(exp1, env0), substitute(exp2, env0), tpe, loc)

        case IfThenElse(exp1, exp2, exp3, tpe, loc) => IfThenElse(substitute(exp1, env0), substitute(exp2, env0), substitute(exp3, env0), tpe, loc)

        case Branch(exp, branches, tpe, loc) =>
          val e = substitute(exp, env0)
          val bs = branches map {
            case (sym, br) => sym -> substitute(br, env0)
          }
          Branch(e, bs, tpe, loc)

        case JumpTo(sym, tpe, loc) => JumpTo(sym, tpe, loc)

        case Let(sym, exp1, exp2, tpe, loc) => Let(replace(sym), substitute(exp1, env0), substitute(exp2, env0), tpe, loc)
        case LetRec(sym, exp1, exp2, tpe, loc) => LetRec(replace(sym), substitute(exp1, env0), substitute(exp2, env0), tpe, loc)
        case Is(sym, tag, exp, loc) => Is(sym, tag, substitute(exp, env0), loc)
        case Tag(sym, tag, exp, tpe, loc) => Tag(sym, tag, substitute(exp, env0), tpe, loc)
        case Untag(sym, tag, exp, tpe, loc) => Untag(sym, tag, substitute(exp, env0), tpe, loc)
        case Index(base, offset, tpe, loc) => Index(substitute(base, env0), offset, tpe, loc)
        case Tuple(elms, tpe, loc) => Tuple(elms.map(e => substitute(e, env0)), tpe, loc)
        case Ref(exp, tpe, loc) => Ref(substitute(exp, env0), tpe, loc)
        case Deref(exp, tpe, loc) => Deref(substitute(exp, env0), tpe, loc)
        case Assign(exp1, exp2, tpe, loc) => Assign(substitute(exp1, env0), substitute(exp2, env0), tpe, loc)
        case Existential(fparam, exp, loc) => Existential(fparam, substitute(exp, env0), loc)
        case Universal(fparam, exp, loc) => Universal(fparam, substitute(exp, env0), loc)
        case NativeConstructor(constructor, args, tpe, loc) => NativeConstructor(constructor, args.map(a => substitute(a, env0)), tpe, loc)
        case e: NativeField => e
        case NativeMethod(method, args, tpe, loc) => NativeMethod(method, args.map(a => substitute(a, env0)), tpe, loc)
        case e: UserError => e
        case e: MatchError => e
        case e: SwitchError => e

        case _: ApplyClo => throw InternalCompilerException(s"Unexpected expression: '${exp0.getClass.getSimpleName}'.")
        case _: ApplyDef => throw InternalCompilerException(s"Unexpected expression: '${exp0.getClass.getSimpleName}'.")
        case _: ApplyCloTail => throw InternalCompilerException(s"Unexpected expression: '${exp0.getClass.getSimpleName}'.")
        case _: ApplyDefTail => throw InternalCompilerException(s"Unexpected expression: '${exp0.getClass.getSimpleName}'.")
        case _: ApplySelfTail => throw InternalCompilerException(s"Unexpected expression: '${exp0.getClass.getSimpleName}'.")
      }
    }

    /**
      * Uncurry an expression
      */
    def uncurry(exp0: SimplifiedAst.Expression, newSyms: Map[Symbol.DefnSym, Map[Int, Symbol.DefnSym]]): SimplifiedAst.Expression = exp0 match {
      case Unit => Unit
      case True => True
      case False => False
      case e: Char => e
      case e: Float32 => e
      case e: Float64 => e
      case e: Int8 => e
      case e: Int16 => e
      case e: Int32 => e
      case e: Int64 => e
      case e: BigInt => e
      case e: Str => e
      case e: Var => e
      case e: Def => e
      case Lambda(args, body, tpe, loc) => Lambda(args, uncurry(body, newSyms), tpe, loc)
      case e: Hook => e
      case e: LambdaClosure => e
      case e: Closure => e
      case e: ApplyHook => e
      case e: Apply =>
        val uncurryCount = maximalUncurry(e)
        uncurryN(e, uncurryCount) match {
          case Apply(Def(sym1, tp1, loc1), args2, tpe2, loc2) =>
            newSyms.get(sym1) match {
              case Some(m) => m.get(uncurryCount) match {
                case Some(newSym) =>
                  Apply(Def(newSym, tp1, loc1), args2 map {
                    uncurry(_, newSyms)
                  }, tpe2, loc2)
                case None => e
              }
              case None => e
            }
          case _ => e
        }
      case Unary(sop, op, exp, tpe, loc) => Unary(sop, op, uncurry(exp, newSyms), tpe, loc)
      case Binary(sop, op, exp1, exp2, tpe, loc) => Binary(sop, op, uncurry(exp1, newSyms), uncurry(exp2, newSyms), tpe, loc)
      case IfThenElse(exp1, exp2, exp3, tpe, loc) => IfThenElse(uncurry(exp1, newSyms), uncurry(exp2, newSyms), uncurry(exp3, newSyms), tpe, loc)
      case Branch(exp, branches, tpe, loc) =>
        val e = uncurry(exp, newSyms)
        val bs = branches map {
          case (sym, br) => sym -> uncurry(br, newSyms)
        }
        Branch(e, bs, tpe, loc)
      case e: JumpTo => e
      case Let(sym, exp1, exp2, tpe, loc) => Let(sym, uncurry(exp1, newSyms), uncurry(exp2, newSyms), tpe, loc)
      case LetRec(sym, exp1, exp2, tpe, loc) => LetRec(sym, uncurry(exp1, newSyms), uncurry(exp2, newSyms), tpe, loc)
      case Is(sym, tag, exp, loc) => Is(sym, tag, uncurry(exp, newSyms), loc)
      case Tag(sym, tag, exp, tpe, loc) => Tag(sym, tag, uncurry(exp, newSyms), tpe, loc)
      case Untag(sym, tag, exp, tpe, loc) => Untag(sym, tag, uncurry(exp, newSyms), tpe, loc)
      case e: Index => e
      case Tuple(elms, tpe, loc) => Tuple(elms map {
        uncurry(_, newSyms)
      }, tpe, loc)
      case Ref(exp, tpe, loc) => Ref(uncurry(exp, newSyms), tpe, loc)
      case Deref(exp, tpe, loc) => Deref(uncurry(exp, newSyms), tpe, loc)
      case Assign(exp1, exp2, tpe, loc) =>
        val e1 = uncurry(exp1, newSyms)
        val e2 = uncurry(exp2, newSyms)
        Assign(e1, e2, tpe, loc)
      case Existential(fparam, exp, loc) => Existential(fparam, uncurry(exp, newSyms), loc)
      case Universal(fparam, exp, loc) => Universal(fparam, uncurry(exp, newSyms), loc)
      case NativeConstructor(constructor, args, tpe, loc) => NativeConstructor(constructor, args map {
        uncurry(_, newSyms)
      }, tpe, loc)
      case e: NativeField => e
      case NativeMethod(method, args, tpe, loc) => NativeMethod(method, args map {
        uncurry(_, newSyms)
      }, tpe, loc)
      case e: UserError => e
      case e: MatchError => e
      case e: SwitchError => e
      case e: ApplyClo => throw InternalCompilerException(s"Unexpected expression: '${e.getClass.getSimpleName}'.")
      case e: ApplyDef => throw InternalCompilerException(s"Unexpected expression: '${e.getClass.getSimpleName}'.")
      case e: ApplyCloTail => throw InternalCompilerException(s"Unexpected expression: '${e.getClass.getSimpleName}'.")
      case e: ApplyDefTail => throw InternalCompilerException(s"Unexpected expression: '${e.getClass.getSimpleName}'.")
      case e: ApplySelfTail => throw InternalCompilerException(s"Unexpected expression: '${e.getClass.getSimpleName}'.")
    }

    /**
      * Uncurry an expression n times
      */
    def uncurryN(exp0: SimplifiedAst.Expression, count: Int): SimplifiedAst.Expression = count match {
      case 0 => exp0
      case n => exp0 match {
        case Apply(exp1, args1, tpe1, loc1) =>
          uncurryN(exp1, n - 1) match {
            // Transform add(3)(4) -> add$uncurried(3,4)
            case Apply(Def(sym2, tpe2, loc2), args2, _, _) =>
              Apply(Def(sym2, tpe2, loc2), args2 ::: args1, tpe1, loc1)
            // Transform ((x,y) -> x+y)(3)(4) -> ((x,y) -> x+y)(3,4)
            case Apply(Lambda(args, body, tpe, loc), args3, _, _) =>
              Apply(Lambda(args, body, tpe, loc), args3 ::: args1, tpe1, loc1)
            // Transform var(3)(4) -> var(3,4)
            case Apply(Var(sym, tpe, loc), args3, _, _) =>
              Apply(Var(sym, tpe, loc), args3 ::: args1, tpe1, loc1)

            case _ => throw InternalCompilerException(s"Can't uncurry expression : $exp0")
          }
        case _ => throw InternalCompilerException(s"Can't uncurry expression : $exp0")
      }
    }

    /**
      * Counts the number of times a function can be uncurried
      */
    def maximalUncurry(exp0: SimplifiedAst.Expression.Apply): Int = exp0 match {
      case Apply(exp1, _, _, _) =>
        exp1 match {
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
          case _: Def => 0
          case _: Lambda => 0
          case _: Hook => 0
          case _: LambdaClosure => 0
          case _: Closure => 0
          case _: ApplyHook => 0
          case e: Apply => 1 + maximalUncurry(e)
          case _: Unary => 0
          case _: Binary => 0
          case _: IfThenElse => 0
          case _: Branch => 0
          case _: JumpTo => 0
          case _: Let => 0
          case _: LetRec => 0
          case _: Is => 0
          case _: Tag => 0
          case _: Untag => 0
          case _: Index => 0
          case _: Tuple => 0
          case _: Ref => 0
          case _: Deref => 0
          case _: Assign => 0
          case _: Existential => 0
          case _: Universal => 0
          case _: NativeConstructor => 0
          case _: NativeField => 0
          case _: NativeMethod => 0
          case _: UserError => 0
          case _: MatchError => 0
          case _: SwitchError => 0
          case _: ApplyClo => throw InternalCompilerException(s"Unexpected expression: '${exp0.getClass.getSimpleName}'.")
          case _: ApplyDef => throw InternalCompilerException(s"Unexpected expression: '${exp0.getClass.getSimpleName}'.")
          case _: ApplyCloTail => throw InternalCompilerException(s"Unexpected expression: '${exp0.getClass.getSimpleName}'.")
          case _: ApplyDefTail => throw InternalCompilerException(s"Unexpected expression: '${exp0.getClass.getSimpleName}'.")
          case _: ApplySelfTail => throw InternalCompilerException(s"Unexpected expression: '${exp0.getClass.getSimpleName}'.")
        }
    }
  }

  /**
    * Uncurry the type of a function. For example, given a function like f(a, b)(c) -> d, turn it
    * into f(a,b,c) -> d. Alternatively, Given Arrow3[Int, Int, Arrow2[Int, Int]],
    * turn in into Arrow4[Int, Int, Int, Int].
    */
  def uncurryType(tpe: Type): Type = tpe match {
    // We're looking to uncurry a type like Apply(Apply(Arrow(2), Char), Apply(Apply(Arrow(2), Int), Bool) (Char -> Int -> Bool)
    // to a type like Apply(Apply(Apply(Arrow(3), Char), Int), Bool) (Char x Int -> Bool)
    // Simple case a -> b -> c => a x b -> c
    case Type.Apply(a, b) =>
      Type.Apply(Type.Apply(incrementArrow(a), getInnerMostType(b)), decrementArrow(b))
    case _ => throw InternalCompilerException(s"Cannot uncurry type $tpe")
  }

  /**
    * Given a function type, increase its arity
    *
    * e.g. Arrow2[a][b] -> Arrow3[a][b]
    */
  def incrementArrow(tpe: Type): Type = tpe match {
    case Arrow(n) => Arrow(n + 1)
    case Type.Apply(tpe1, tpe2) => Type.Apply(incrementArrow(tpe1), tpe2)
    case _ => throw InternalCompilerException(s"Cannot increment the arrow arity of type '$tpe'")
  }

  /**
    * Given a function type, decrease its arity and drop it's inner most argument
    *
    * e.g. Arrow3[a][b][c] -> Arrow3[b][c]
    */
  def decrementArrow(tpe: Type): Type = tpe match {
    case Type.Apply(Arrow(n), a) => Arrow(n - 1)
    case Type.Apply(tpe1, tpe2) => decrementArrow(tpe1) match {
      case Arrow(1) => tpe2
      case t => Type.Apply(t, tpe2)
    }
    case _ => throw InternalCompilerException(s"Cannot decrement the arrow arity of type '$tpe'")
  }

  /**
    * Get the first applied type to a function type
    *
    * e.g. Arrow3[a][b][c] -> a
    */
  def getInnerMostType(tpe: Type): Type = tpe match {
    case Type.Apply(Arrow(n), a) => a
    case Type.Apply(a, _) => getInnerMostType(a)
    case _ => throw InternalCompilerException(s"Cannot get the innermost type of '$tpe'")
  }
}

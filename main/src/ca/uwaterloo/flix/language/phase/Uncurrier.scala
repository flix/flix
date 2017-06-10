/*
 * Copyright 2015-2016 Jason Mittertreiner
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

    val defns = root.definitions.foldLeft(Map.empty[Symbol.DefnSym, SimplifiedAst.Definition.Constant])(Definitions.mkUncurriedDefs)
    val uncurriedDef = defns map {case (k,v) => k -> Definitions.uncurry(v, root) }

    val currentTime = System.nanoTime()
    val time = root.time.copy(uncurrier = currentTime - startTime)
    root.copy(definitions = uncurriedDef).copy(time = time).toSuccess
  }

  object Definitions {
    /**
      * Create uncurried versions of all curried functions
      *
      * @param map The old map of function definitions
      * @param pair The entry which to make uncurried versions of
      * @return A new map with the uncurried versions added
      */
    def mkUncurriedDefs(map: Map[Symbol.DefnSym, SimplifiedAst.Definition.Constant], pair: (Symbol.DefnSym, SimplifiedAst.Definition.Constant))(implicit genSym: GenSym): Map[Symbol.DefnSym, SimplifiedAst.Definition.Constant] = {
      map ++ mkUncurriedDef(pair)
    }

    /**
      * Creates all the uncurried versions of a function
      *
      * @param pair The dfnsym definition pair to base the functions on
      * @return A map containing all the new definitions
      */
    def mkUncurriedDef(pair: (Symbol.DefnSym, SimplifiedAst.Definition.Constant))(implicit genSym: GenSym): Map[Symbol.DefnSym, SimplifiedAst.Definition.Constant] = {
      val sym = pair._1
      val curriedDef = pair._2
      curriedDef.exp match {
        // If the body is a lambda, then we have a curried function
        // definition, so we need to create an uncurried version.
        // To do this, we'll take make the body of the function the body
        // of the lambda, then add the arguments of the lambda to the
        // formal parameters
        case Lambda(args, body, _, _) =>
          // Create a new definition for our function
          val uncurriedSym = Symbol.mkDefnSym(pair._1.name + "$uncurried")
          // Create new VarSyms for our function
          val newFormals = (curriedDef.formals ::: args).map(f => f.copy(sym = Symbol.freshVarSym(f.sym)))
          // Replace the old VarSyms in the body
          val varMapping = (curriedDef.formals ::: args).map(f => f.sym).zip(newFormals.map(f => f.sym)).toMap
          val newBody = Expressions.replaceVars(body, varMapping)

          val unCurriedDef = SimplifiedAst.Definition.Constant(
            curriedDef.ann,
            curriedDef.mod,
            uncurriedSym,
            newFormals,
            newBody,
            curriedDef.isSynthetic,
            uncurryType(curriedDef.tpe),
            curriedDef.loc)
          (Map.empty[Symbol.DefnSym, SimplifiedAst.Definition.Constant] + pair + (uncurriedSym -> unCurriedDef)) ++ mkUncurriedDef(uncurriedSym, unCurriedDef)
        case _ => Map.empty[Symbol.DefnSym, SimplifiedAst.Definition.Constant] + pair
      }
    }

    /**
      * Uncurry a definition constant
      *
      * @param cst
      * @param root
      * @return
      */
    def uncurry(cst: SimplifiedAst.Definition.Constant, root: SimplifiedAst.Root): SimplifiedAst.Definition.Constant = {
      cst.copy(exp = Expressions.uncurry(cst.exp, root))
    }
  }

  object Expressions {
    /**
      * Recursively replace all the variables in tast with new variables using the
      * pairs in replaceSyms. This ensure that the new functions created don't
      * reference the variables of the original functions.
      *
      * @param oldExp The expression to replace
      * @param replaceSyms The v
      * @return
      */
    def replaceVars(oldExp: SimplifiedAst.Expression, replaceSyms : Map[Symbol.VarSym, Symbol.VarSym])(implicit genSym: GenSym): SimplifiedAst.Expression =  {
      def replace(oldSym : Symbol.VarSym): Symbol.VarSym =
        replaceSyms.getOrElse(oldSym, oldSym)
      oldExp match {
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
        case e:Ref => e
        // For lambdas, we have to replace the formal arguments with new formal
        // parameters
        case Lambda(args, body, tpe, loc) =>
          val newFormals = args.map(f => f.copy(sym = Symbol.freshVarSym(f.sym)))
          val newMapping = args.map(f => f.sym).zip(newFormals.map(f => f.sym)).toMap
          Lambda(newFormals, replaceVars(body, replaceSyms ++ newMapping), tpe, loc)
        case e:Hook => e
        case MkClosure(lambda, freeVars, tpe, loc) => MkClosure(replaceVars(lambda, replaceSyms).asInstanceOf[SimplifiedAst.Expression.Lambda], freeVars, tpe, loc)
        case e: MkClosureRef => e
        case ApplyRef(sym, args, tpe, loc) => ApplyRef(sym, args.map(replaceVars(_, replaceSyms)), tpe, loc)
        case ApplyTail(sym, formals, actuals, tpe, loc) => ApplyTail(sym, formals, actuals.map(replaceVars(_, replaceSyms)), tpe, loc)
        case ApplyHook(hook, args, tpe, loc) => ApplyHook(hook, args.map(replaceVars(_, replaceSyms)), tpe, loc)
        case Apply(exp, args, tpe, loc) => Apply(replaceVars(exp, replaceSyms), args.map(a => replaceVars(a, replaceSyms)), tpe, loc)
        case Unary(op, exp, tpe, loc) => Unary(op, replaceVars(exp, replaceSyms), tpe, loc)
        case Binary(op, exp1, exp2, tpe, loc) => Binary(op, replaceVars(exp1, replaceSyms), replaceVars(exp2, replaceSyms), tpe, loc)
        case IfThenElse(exp1, exp2, exp3, tpe, loc) => IfThenElse(replaceVars(exp1, replaceSyms), replaceVars(exp1, replaceSyms), replaceVars(exp3, replaceSyms), tpe, loc)
        case Let(sym, exp1, exp2, tpe, loc) => Let(replace(sym), replaceVars(exp1, replaceSyms), replaceVars(exp2, replaceSyms), tpe, loc)
        case LetRec(sym, exp1, exp2, tpe, loc) => LetRec(replace(sym), replaceVars(exp1, replaceSyms), replaceVars(exp2, replaceSyms), tpe, loc)
        case Is(sym, tag, exp, loc) => Is(sym, tag, replaceVars(exp, replaceSyms), loc)
        case Tag(sym, tag, exp, tpe, loc) => Tag(sym, tag, replaceVars(exp, replaceSyms), tpe, loc)
        case Untag(sym, tag, exp, tpe, loc) => Untag(sym, tag, replaceVars(exp, replaceSyms), tpe, loc)
        case Index(base, offset, tpe, loc) => Index(replaceVars(base, replaceSyms), offset, tpe, loc)
        case Tuple(elms, tpe, loc) => Tuple(elms.map(e => replaceVars(e, replaceSyms)), tpe, loc)
        case Existential(fparam, exp, loc) => Existential(fparam, replaceVars(exp, replaceSyms), loc)
        case Universal(fparam, exp, loc) => Universal(fparam, replaceVars(exp, replaceSyms), loc)
        case NativeConstructor(constructor, args, tpe, loc) => NativeConstructor(constructor, args.map(a => replaceVars(a, replaceSyms)), tpe, loc)
        case e:NativeField => e
        case NativeMethod(method, args, tpe, loc) => NativeMethod(method, args.map(a => replaceVars(a, replaceSyms)), tpe, loc)
        case e:UserError => e
        case e:MatchError => e
        case e:SwitchError => e
      }
    }

    /**
      * Uncurry an expression
      *
      * @param tast The expression to uncurry
      * @return The expression modified to have all curried calls uncurried
      */
    def uncurry(tast: SimplifiedAst.Expression, root: SimplifiedAst.Root): SimplifiedAst.Expression = tast match {
      // When we see an apply on an apply, this is a curried function
      // call (think `(foo(3))(4)`). Transform it into a call with multiple arguments
      // as a single apply
      case Apply(exp0, args0, tpe0, loc0) =>
        val inner = uncurry(exp0, root)
        inner match {
          case Apply(exp1, args1, tpe1, loc1) => exp1 match {
            case Ref(sym2, tpe2, loc2) =>
              // Only call the uncurried if it exists
              val sym = Symbol.mkDefnSym(sym2.name + "$uncurried")
              if (root.definitions.contains(sym)) {
                Apply(Ref(sym, uncurryType(tpe2), loc2), args1 ::: args0, tpe0, loc0)
              } else {
                tast
              }
            case _ => Apply(inner, args0, tpe0, loc0)
          }
          case _ => tast
        }
      case _ => tast
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

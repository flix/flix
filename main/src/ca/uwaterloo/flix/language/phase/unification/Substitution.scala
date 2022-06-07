/*
 *  Copyright 2020 Magnus Madsen
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */
package ca.uwaterloo.flix.language.phase.unification

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.{Ast, Kind, Rigidity, Scheme, Symbol, Type, TypeConstructor}
import ca.uwaterloo.flix.language.phase.unification.Unification.unifyTypes
import ca.uwaterloo.flix.util.InternalCompilerException
import ca.uwaterloo.flix.util.Result.{Err, Ok}

/**
  * Companion object for the [[Substitution]] class.
  */
object Substitution {
  /**
    * Returns the empty substitution.
    */
  val empty: Substitution = Substitution(Map.empty)

  /**
    * Returns the singleton substitution mapping the type variable `x` to `tpe`.
    */
  def singleton(x: Symbol.TypeVarSym, tpe: Type): Substitution = {
    // Ensure that we do not add any x -> x mappings.
    tpe match {
      case y: Type.Var if x.id == y.sym.id => empty
      case y: Type.Var if y.sym.text isStrictlyLessPreciseThan x.text => Substitution(Map(x -> y.withText(x.text)))
      case y: Type.Var if x.text isStrictlyLessPreciseThan y.sym.text => Substitution(Map(x.withText(y.sym.text) -> y))
      case _ => Substitution(Map(x -> tpe))
    }
  }

}

/**
  * A substitution is a map from type variables to types.
  */
case class Substitution(m: Map[Symbol.TypeVarSym, Type]) {

  /**
    * Returns `true` if `this` is the empty substitution.
    */
  val isEmpty: Boolean = m.isEmpty

  /**
    * Applies `this` substitution to the given type `tpe0`.
    */
  def apply(tpe0: Type): Type = {
    // NB: The order of cases has been determined by code coverage analysis.
    def visit(t: Type): Type =
      t match {
        case x: Type.Var => m.getOrElse(x.sym, x)
        case Type.Cst(tc, _) => t
        case Type.Apply(t1, t2, loc) =>
          val y = visit(t2)
          visit(t1) match {
            // Simplify boolean equations.
            case Type.Cst(TypeConstructor.Not, _) => BoolUnification.mkNot(y)
            case Type.Apply(Type.Cst(TypeConstructor.And, _), x, _) => BoolUnification.mkAnd(x, y)
            case Type.Apply(Type.Cst(TypeConstructor.Or, _), x, _) => BoolUnification.mkOr(x, y)
            case x => Type.Apply(x, y, loc)
          }
        case Type.Alias(sym, args0, tpe0, loc) =>
          val args = args0.map(visit)
          val tpe = visit(tpe0)
          Type.Alias(sym, args, tpe, loc)
        case _: Type.UnkindedArrow => t
        case Type.ReadWrite(tpe, loc) => Type.ReadWrite(apply(tpe), loc)
        case _: Type.Ascribe => throw InternalCompilerException(s"Unexpected type '$tpe0'.")
      }

    // Optimization: Return the type if the substitution is empty. Otherwise visit the type.
    if (isEmpty) tpe0 else visit(tpe0)
  }

  /**
    * Applies `this` substitution to the given types `ts`.
    */
  def apply(ts: List[Type]): List[Type] = if (isEmpty) ts else ts map apply

  /**
    * Applies `this` substitution to the given type constraint `tc`.
    */
  def apply(tc: Ast.TypeConstraint): Ast.TypeConstraint = if (isEmpty) tc else tc.copy(arg = apply(tc.arg))

  /**
    * Applies `this` substitution to the given type scheme `sc`.
    *
    * NB: Throws an InternalCompilerException if quantifiers are present in the substitution.
    */
  def apply(sc: Scheme): Scheme = sc match {
    case Scheme(quantifiers, constraints, base) =>
      if (sc.quantifiers.exists(m.contains)) {
        throw InternalCompilerException("Quantifier in substitution.")
      }
      Scheme(quantifiers, constraints.map(apply), apply(base))
  }

  /**
    * Removes the binding for the given type variable `tvar` (if it exists).
    */
  def unbind(tvar: Symbol.TypeVarSym): Substitution = Substitution(m - tvar)

  /**
    * Returns the left-biased composition of `this` substitution with `that` substitution.
    */
  def ++(that: Substitution): Substitution = {
    if (this.isEmpty) {
      that
    } else if (that.isEmpty) {
      this
    } else {
      Substitution(
        this.m ++ that.m.filter(kv => !this.m.contains(kv._1))
      )
    }
  }

  /**
    * Returns the composition of `this` substitution with `that` substitution.
    */
  def @@(that: Substitution): Substitution = {
    // Case 1: Return `that` if `this` is empty.
    if (this.isEmpty) {
      return that
    }

    // Case 2: Return `this` if `that` is empty.
    if (that.isEmpty) {
      return this
    }

    // Case 3: Merge the two substitutions.

    // NB: Use of mutability improve performance.
    import scala.collection.mutable
    val newTypeMap = mutable.Map.empty[Symbol.TypeVarSym, Type]

    // Add all bindings in `that`. (Applying the current substitution).
    for ((x, t) <- that.m) {
      newTypeMap.update(x, this.apply(t))
    }

    // Add all bindings in `this` that are not in `that`.
    for ((x, t) <- this.m) {
      if (!that.m.contains(x)) {
        newTypeMap.update(x, t)
      }
    }

    Substitution(newTypeMap.toMap) ++ this
  }

  /**
    * Propagates type variable *names* within a substitution.
    *
    * During type inference we may construct bindings such as:
    *
    * x -> y
    * u -> v
    *
    * Here `x` may have a name ("text") but `y` may not.
    * Conversely, `v` may have a name ("text"), but not `u`.
    *
    * The idea is to propagate these names across these bindings.
    *
    * This process does *not* have to be transitive, because the substitution
    * (if computed by inference) should already be transitive.
    */
  def propagate: Substitution = {
    ///
    /// A map from type variables (without a name) to a string name ("text").
    ///
    var replacement = Map.empty[Symbol.TypeVarSym, Ast.VarText]

    //
    // Compute all bindings where there is a name to be propagated.
    //
    for ((tvar1, tpe) <- m) {
      tpe match {
        case tvar2: Type.KindedVar =>
          (tvar1.text, tvar2.sym.text) match {
            case (text1, text2) if text1 isStrictlyLessPreciseThan text2 =>
              replacement = replacement + (tvar1 -> text2)
            case (text1, text2) if text2 isStrictlyLessPreciseThan text1 =>
              replacement = replacement + (tvar2.sym -> text1)
            case _ => // nop
          }
        case _ => // nop
      }
    }

    /**
      * A utility function to replace the text in `tvar` using the computed map.
      */
    def replace(tvar: Type.KindedVar): Type = replacement.get(tvar.sym) match {
      case None => tvar
      case Some(text) => tvar.withText(text)
    }

    ///
    /// Computes the new substitution. It is equivalent to the old one, but with updated names.
    ///
    val m2 = m.foldLeft(Map.empty[Symbol.TypeVarSym, Type]) {
      case (acc, (tvar, tpe)) =>
        val t = tpe.map(replace)
        acc + (tvar -> t)
    }

    Substitution(m2)
  }

  /**
    * Computes an equ-most general substitution with the given type variable as `rigid`.
    *
    * That is, the resulting subst has `sym = sym`.
    * (which actually means `sym` does not appear in the substitution).
    */
  def pivot(sym0: Symbol.KindedTypeVarSym)(implicit flix: Flix): Substitution = {
    val newSubst = m.get(sym0) match {
      // Case 1: The variable is replaced. Need to process it.
      case Some(tpe) =>
        val rigidSym = sym0.withRigidity(Rigidity.Rigid)
        unifyTypes(Type.KindedVar(rigidSym, sym0.loc), tpe, Rigidity.emptyEnv) match { // TODO we can use the empty renv here since we'll get rid of pivoting when renvs are activated
          case Ok(rigidSubst) =>
            // de-rigidify the substitution
            val flexMap = rigidSubst.m.map {
              case (k, v) =>
                val v2 = v.map {
                  case Type.KindedVar(sym, loc) if sym == rigidSym => Type.KindedVar(rigidSym.withRigidity(Rigidity.Flexible), loc)
                  case otherVar => otherVar
                }
                (k, v2)
            }
            Substitution(flexMap)
          case Err(_) => throw InternalCompilerException("Unexpected unification failure.")
        }
      // Case 2: The variable is not replaced. Nothing to do.
      case None => Substitution.empty
    }
    newSubst @@ this.unbind(sym0)
  }
}

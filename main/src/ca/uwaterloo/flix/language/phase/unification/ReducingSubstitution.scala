/*
 * Copyright 2023 Matthew Lutze
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
package ca.uwaterloo.flix.language.phase.unification
import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.{Ast, SourceLocation, Symbol, Type, TypeConstructor}
import ca.uwaterloo.flix.util.InternalCompilerException
import ca.uwaterloo.flix.util.Result.{Err, Ok}
import ca.uwaterloo.flix.util.collection.ListMap

case class ReducingSubstitution(m: Map[Symbol.KindedTypeVarSym, Type], eqEnv: ListMap[Symbol.AssocTypeSym, Ast.AssocTypeDef])(implicit flix: Flix) {

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
            case Type.Cst(TypeConstructor.Not, _) => Type.mkNot(y, loc)
            case Type.Apply(Type.Cst(TypeConstructor.And, _), x, _) => Type.mkAnd(x, y, loc)
            case Type.Apply(Type.Cst(TypeConstructor.Or, _), x, _) => Type.mkOr(x, y, loc)

            // Simplify set expressions
            case Type.Cst(TypeConstructor.Complement, _) => SetUnification.mkComplement(y)
            case Type.Apply(Type.Cst(TypeConstructor.Intersection, _), x, _) => SetUnification.mkIntersection(x, y)
            case Type.Apply(Type.Cst(TypeConstructor.Union, _), x, _) => SetUnification.mkUnion(x, y)

            case Type.Cst(TypeConstructor.CaseComplement(sym), _) => Type.mkCaseComplement(y, sym, loc)
            case Type.Apply(Type.Cst(TypeConstructor.CaseIntersection(sym), _), x, _) => Type.mkCaseIntersection(x, y, sym, loc)
            case Type.Apply(Type.Cst(TypeConstructor.CaseUnion(sym), _), x, _) => Type.mkCaseUnion(x, y, sym, loc)

            // Else just apply
            case x => Type.Apply(x, y, loc)
          }
        case Type.Alias(sym, args0, tpe0, loc) =>
          val args = args0.map(visit)
          val tpe = visit(tpe0)
          Type.Alias(sym, args, tpe, loc)
        case Type.AssocType(cst, arg0, _, loc) =>
          val arg = visit(arg0)
          EqualityEnvironment.reduceAssocType(cst, arg, eqEnv) match {
            case Ok(t) => t
            case Err(_) => throw InternalCompilerException("unexpected associated type reduction failure", loc)
          }
      }

    // Optimization: Return the type if the substitution is empty. Otherwise visit the type.
    if (isEmpty) tpe0 else visit(tpe0)
  }
}


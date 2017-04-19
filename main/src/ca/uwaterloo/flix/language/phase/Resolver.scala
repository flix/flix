/*
 *  Copyright 2017 Magnus Madsen
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

package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.{NamedAst, ResolvedAst}
import ca.uwaterloo.flix.language.errors.ResolutionError
import ca.uwaterloo.flix.util.Validation
import ca.uwaterloo.flix.util.Validation._

// TODO: DOC
object Resolver extends Phase[NamedAst.Program, NamedAst.Program] { // TODO: Change types

  // TODO: DOC
  def run(program: NamedAst.Program)(implicit flix: Flix): Validation[NamedAst.Program, ResolutionError] = {


    val definitions = program.definitions.flatMap {
      case (ns, defs) => defs.map {
        case (name, defn) => Declarations.resolve(defn) // TODO: Need ns, name?
      }
    }


    program.toSuccess

  }

  object Declarations {

    def resolve(decl: NamedAst.Declaration.Definition): Validation[ResolvedAst.Declaration.Definition, ResolutionError] = {
      Expressions.resolve(decl.exp)
      ???
    }

  }

  object Expressions {

    def resolve(e0: NamedAst.Expression): Validation[ResolvedAst.Expression, ResolutionError] = e0 match {
      case NamedAst.Expression.Wild(tpe, loc) => ???

      case NamedAst.Expression.Var(sym, loc) => ???

      case NamedAst.Expression.Ref(ref, tvar, loc) =>
        ???
      //        Disambiguation.lookupRef(ref, ns0, program) match {
      //          case Ok(RefTarget.Defn(ns, defn)) =>
      //            Disambiguation.resolve(defn.sc, ns, program) match {
      //              case Ok(scheme) => unifyM(tvar, Scheme.instantiate(scheme), loc)
      //              case Err(e) => failM(e)
      //            }
      //          case Ok(RefTarget.Hook(hook)) => liftM(hook.tpe)
      //          case Err(e) => failM(e)
      //        }

      case NamedAst.Expression.Unit(loc) => ???

      case NamedAst.Expression.True(loc) => ???

      case NamedAst.Expression.False(loc) => ???

      case NamedAst.Expression.Char(lit, loc) => ???

      case NamedAst.Expression.Float32(lit, loc) => ???

      case NamedAst.Expression.Float64(lit, loc) => ???

      case NamedAst.Expression.Int8(lit, loc) => ???

      case NamedAst.Expression.Int16(lit, loc) => ???

      case NamedAst.Expression.Int32(lit, loc) => ???

      case NamedAst.Expression.Int64(lit, loc) => ???

      case NamedAst.Expression.BigInt(lit, loc) => ???

      case NamedAst.Expression.Str(lit, loc) => ???

      case NamedAst.Expression.Apply(lambda, args, tvar, loc) => ???

      case NamedAst.Expression.Lambda(params, exp, tvar, loc) => ???

      case NamedAst.Expression.Unary(op, exp, tvar, loc) => ???

      case NamedAst.Expression.Binary(op, exp1, exp2, tvar, loc) => ???

      case NamedAst.Expression.IfThenElse(exp1, exp2, exp3, tvar, loc) => ???

      case NamedAst.Expression.Let(sym, exp1, exp2, tvar, loc) => ???

      case NamedAst.Expression.Match(exp, rules, tvar, loc) => ???

      case NamedAst.Expression.Switch(rules, tvar, loc) => ???

      case NamedAst.Expression.Tag(enum, tag, exp, tvar, loc) => ???

      case NamedAst.Expression.Tuple(elms, tvar, loc) => ???

      case NamedAst.Expression.Existential(fparam, exp, loc) => ???

      case NamedAst.Expression.Universal(fparam, exp, loc) => ???

      case NamedAst.Expression.Ascribe(exp, tpe, loc) => ???

      case NamedAst.Expression.NativeConstructor(method, args, tpe, loc) => ???

      case NamedAst.Expression.NativeField(field, tpe, loc) => ???

      case NamedAst.Expression.NativeMethod(method, args, tpe, loc) => ???

      case NamedAst.Expression.UserError(tvar, loc) => ???
    }

  }


}

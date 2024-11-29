/*
 * Copyright 2023 Jonathan Lindegaard Starup
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

package ca.uwaterloo.flix.language.dbg.printer

import ca.uwaterloo.flix.language.ast.{Type, TypeConstructor}
import ca.uwaterloo.flix.language.dbg.DocAst
import ca.uwaterloo.flix.language.fmt.{FormatOptions, FormatType}

import scala.annotation.tailrec

object TypePrinter {

  /** Returns the [[DocAst.Type]] representation of `tpe`. */
  def print(tpe: Type): DocAst.Type = {
    val (base, args) = collectApp(tpe)
    // Make the well-kinded types pretty.
    (base, args) match {
      case (Type.Var(sym, _), _) => mkApp(DocAst.Type.Var(sym), args.map(print))
      case (Type.Cst(TypeConstructor.Arrow(arity), _), _) if args.lengthIs == arity + 1 && arity >= 2 =>
        // `(a1, a2, ..) -> b \ ef` is represented as List(ef, a1, a2, .., b)
        // safe match because of the case guard
        val (arrowEff :: arrowArgs, List(arrowRes)) = args.splitAt(args.length-1)
        DocAst.Type.ArrowEff(arrowArgs.map(print), print(arrowRes), print(arrowEff))
      case (Type.Cst(TypeConstructor.RecordRowExtend(label), _), List(arg0, arg1)) =>
        DocAst.Type.RecordRowExtend(label.toString, print(arg0), print(arg1))
      case (Type.Cst(TypeConstructor.SchemaRowExtend(label), _), List(arg0, arg1)) =>
        DocAst.Type.SchemaRowExtend(label.toString, print(arg0), print(arg1))
      case (Type.Cst(TypeConstructor.Lazy, _), List(arg0)) =>
        DocAst.Type.Lazy(print(arg0))
      case (Type.Cst(TypeConstructor.Tuple(arity), _), _) if args.lengthIs == arity =>
        DocAst.Type.Tuple(args.map(print))
      // TODO not, and, or, pure?, univ?, complement, union, intersection, caseXOp
      case (Type.Cst(tc, _), _) => mkApp(TypeConstructorPrinter.print(tc), args.map(print))
      case (Type.Alias(cst, aliasArgs, _, _), _) => mkApp(DocAst.Type.Alias(cst.sym, aliasArgs.map(print)), args.map(print))
      case (Type.AssocType(cst, arg, _, _), _) => mkApp(DocAst.Type.AssocType(cst.sym, print(arg)), args.map(print))
      case (Type.JvmToType(tpe, _), _) => mkApp(mkApp(DocAst.Type.AsIs("JvmToType"), List(print(tpe))), args.map(print))
      case (Type.JvmToEff(tpe, _), _) => mkApp(mkApp(DocAst.Type.AsIs("JvmToEff"), List(print(tpe))), args.map(print))
      case (Type.UnresolvedJvmType(member, _), _) => mkApp(DocAst.Type.App(printJvmMember(member), List(print(tpe))), args.map(print))
      case (Type.Apply(_, _, _), _) =>
        // `collectApp` does not return Apply as base.
        DocAst.Type.Meta("bug in TypePrinter")
    }
  }

  private def mkApp(base: DocAst.Type, args: List[DocAst.Type]): DocAst.Type = args match {
    case Nil => base
    case other => DocAst.Type.App(base, other)
  }

  private def printJvmMember(member: Type.JvmMember): DocAst.Type = DocAst.Type.Meta("JvmMember")

  /** Returns e.g. `App(App(Tuple, Char), Char)` as `(Tuple, List(Char, Char))`. */
  private def collectApp(tpe: Type): (Type, List[Type]) = {
    @tailrec
    def helper(tpe0: Type, acc: List[Type]): (Type, List[Type]) = tpe0 match {
      case Type.Apply(tpe1, tpe2, _) => helper(tpe1, tpe2 :: acc)
      case _ => (tpe0, acc)
    }
    helper(tpe, Nil)
  }

}

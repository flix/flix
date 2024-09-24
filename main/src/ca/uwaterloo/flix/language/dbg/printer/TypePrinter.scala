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

  /**
    * Returns the [[DocAst.Type]] representation of `tpe`.
    */
  def print(tpe: Type): DocAst.Type = {
    // Temporarily use existing type formatting
    // try to unpack one arrow type for the sake of signatures
    // try to unpack one tuple type for the sake of enums
    // try to unpack one unit type for the sake of enums
    tpe.baseType match {
      case Type.Cst(TypeConstructor.Arrow(_), _) =>
        val args = tpe.arrowArgTypes
        val res = tpe.arrowResultType
        DocAst.Type.Arrow(args.map(printSimple), printSimple(res))
      case Type.Cst(TypeConstructor.Tuple(_), _) =>
        val args = tpe.typeArguments
        DocAst.Type.Tuple(args.map(printSimple))
      case Type.Cst(TypeConstructor.Unit, _) =>
        DocAst.Type.Unit
      case _ => printSimple(tpe)
    }
  }

  /**
    * Returns the [[DocAst.Eff]] representation of `tpe`.
    */
  def printAsEffect(tpe: Type): DocAst.Eff = tpe match {
    case Type.Cst(TypeConstructor.Pure, _) => DocAst.Eff.Pure
    case Type.Cst(TypeConstructor.Univ, _) => DocAst.Eff.Univ
    case _ => DocAst.Eff.AsIs(typeToString(tpe))
  }

  /** Print type without formatting as-is */
  private def printSimple(tpe: Type): DocAst.Type = {
    DocAst.Type.AsIs(typeToString(tpe))
  }

  /** Returns the type as a simple string */
  private def typeToString(tpe: Type): String = {
    FormatType.formatTypeWithOptions(tpe, FormatOptions(FormatOptions.VarName.NameBased))
  }

  def printTpe(tpe: Type): DocAst.Type = {
    val (base, args0) = collectApp(tpe)
    val args = args0.map(print)
    base match {
      case Type.Var(sym, _) => DocAst.Type.App(DocAst.Type.Var(sym), args)
      case Type.Cst(TypeConstructor.Arrow(arity), _) if args.lengthIs == arity + 1 && arity >= 3 =>
        // could be optimized
        // (eff, args.., res)
        if (args0.head == Type.Pure) {
          val res = args.last
          val actualArgs = args.dropRight(1).drop(1)
          DocAst.Type.Arrow(actualArgs, res)
        } else {
          val eff = args.head
          val res = args.last
          val actualArgs = args.dropRight(1).drop(1)
          DocAst.Type.ArrowEff(actualArgs, res, eff)
        }
      case Type.Cst(TypeConstructor.RecordRowExtend(label), _) if args.lengthIs == 2 =>
        val List(arg0, arg1) = args
        DocAst.Type.RecordRowExtend(label.toString, arg0, arg1)
      case Type.Cst(TypeConstructor.SchemaRowExtend(label), _) if args.lengthIs == 2 =>
        val List(arg0, arg1) = args
        DocAst.Type.SchemaRowExtend(label.toString, arg0, arg1)
      case Type.Cst(TypeConstructor.Lazy, _) if args.lengthIs == 1 =>
        val List(arg0) = args
        DocAst.Type.Lazy(arg0)
      case Type.Cst(TypeConstructor.Tuple(arity), _) if args.lengthIs == arity =>
        DocAst.Type.Tuple(args)
      // not, and, or, pure?, univ?, complement, union, intersection, caseXOp
      case Type.Cst(tc, _) => DocAst.Type.App(TypeConstructorPrinter.print(tc), args)
      case Type.Alias(cst, aliasArgs, _, _) => DocAst.Type.App(DocAst.Type.Alias(cst.sym, aliasArgs.map(print)), args)
      case Type.AssocType(cst, arg, _, _) => DocAst.Type.App(DocAst.Type.AssocType(cst.sym, print(arg)), args)
      case Type.JvmToType(tpe, _) => DocAst.Type.App(DocAst.Type.App(DocAst.Type.AsIs("JvmToType"), List(print(tpe))), args)
      case Type.JvmToEff(tpe, _) => DocAst.Type.App(DocAst.Type.App(DocAst.Type.AsIs("JvmToEff"), List(print(tpe))), args)
      case Type.UnresolvedJvmType(member, _) => DocAst.Type.App(DocAst.Type.App(printJvmMember(member), List(print(tpe))), args)
      case Type.Apply(_, _, _) =>
        // `collectApp` does not return Apply as base.
        DocAst.Type.Meta("bug in TypePrinter")
    }
  }

  private def printJvmMember(member: Type.JvmMember): DocAst.Type = DocAst.Type.Meta("JvmMember")

  /** Returns e.g. `App(App(Tuple, Char), Char)` as `(Type, List(Char, Char))`. */
  private def collectApp(tpe: Type): (Type, List[Type]) = {
    @tailrec
    def helper(tpe0: Type, acc: List[Type]): (Type, List[Type]) = tpe0 match {
      case Type.Apply(tpe1, tpe2, _) => helper(tpe1, tpe2 :: acc)
      case _ => (tpe0, acc.reverse)
    }
    helper(tpe, Nil)
  }

}

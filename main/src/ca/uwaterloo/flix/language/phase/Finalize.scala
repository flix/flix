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
import ca.uwaterloo.flix.language.ast._
import ca.uwaterloo.flix.util.InternalCompilerException

object Finalize {

  def run(root: ControlAst.Root)(implicit flix: Flix): FinalAst.Root = flix.phase("Finalize") {
    val defs = root.defs.map {
      case (k, v) => k -> visitDef(v)
    }

    val enums = root.enums.map {
      case (sym, enum) => sym -> visitEnum(enum)
    }

    FinalAst.Root(defs, enums, root.entryPoint, root.sources)
  }

  private def visitDef(def0: ControlAst.Def)(implicit flix: Flix): FinalAst.Def = {
    val fs = def0.fparams.map(visitFormalParam)
    val e = visitExp(def0.exp)
    val tpe = visitType(def0.tpe)
    FinalAst.Def(def0.ann, def0.mod, def0.sym, fs, e, tpe, def0.loc)
  }

  private def visitEnum(enum0: ControlAst.Enum)(implicit flix: Flix): FinalAst.Enum = enum0 match {
    case ControlAst.Enum(ann, mod, sym, cases0, tpe0, loc) =>
      val cases = cases0.map {
        case (tag, ControlAst.Case(enumSym, tagType, tagLoc)) =>
          val tpe = visitType(tagType)
          tag -> FinalAst.Case(enumSym, tpe, tagLoc)
      }
      val tpe = visitType(tpe0)
      FinalAst.Enum(ann, mod, sym, cases, tpe, loc)
  }

  private def visitExp(exp0: ControlAst.Expr)(implicit flix: Flix): FinalAst.Expr = exp0 match {
    case ControlAst.Expr.Cst(cst, tpe, loc) =>
      val t = visitType(tpe)
      FinalAst.Expr.Cst(cst, t, loc)

    case ControlAst.Expr.Var(sym, tpe, loc) =>
      val t = visitType(tpe)
      FinalAst.Expr.Var(sym, t, loc)

    case ControlAst.Expr.Closure(sym, exps, tpe, loc) =>
      val op = AtomicOp.Closure(sym)
      val es = exps.map(visitExp)
      val t = visitType(tpe)
      FinalAst.Expr.ApplyAtomic(op, es, t, loc)

    case ControlAst.Expr.ApplyAtomic(op, exps, tpe, purity, loc) =>
      val es = exps.map(visitExp)
      val t = visitType(tpe)
      FinalAst.Expr.ApplyAtomic(op, es, t, loc)

    case ControlAst.Expr.ApplyClo(exp, exps, ct, tpe, _, loc) =>
      val es = exps map visitExp
      val t = visitType(tpe)
      FinalAst.Expr.ApplyClo(visitExp(exp), es, ct, t, loc)

    case ControlAst.Expr.ApplyDef(sym, exps, ct, tpe, _, loc) =>
      val es = exps map visitExp
      val t = visitType(tpe)
      FinalAst.Expr.ApplyDef(sym, es, ct, t, loc)

    case ControlAst.Expr.ApplySelfTail(name, formals, actuals, tpe, _, loc) =>
      val fs = formals.map(visitFormalParam)
      val as = actuals.map(visitExp)
      val t = visitType(tpe)
      FinalAst.Expr.ApplySelfTail(name, fs, as, t, loc)

    case ControlAst.Expr.IfThenElse(exp1, exp2, exp3, tpe, _, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      val v3 = visitExp(exp3)
      val t = visitType(tpe)
      FinalAst.Expr.IfThenElse(e1, e2, v3, t, loc)

    case ControlAst.Expr.Branch(exp, branches, tpe, _, loc) =>
      val e = visitExp(exp)
      val bs = branches map {
        case (sym, br) => sym -> visitExp(br)
      }
      val t = visitType(tpe)
      FinalAst.Expr.Branch(e, bs, t, loc)

    case ControlAst.Expr.JumpTo(sym, tpe, _, loc) =>
      val t = visitType(tpe)
      FinalAst.Expr.JumpTo(sym, t, loc)

    case ControlAst.Expr.Let(sym, exp1, exp2, tpe, _, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      val t = visitType(tpe)
      FinalAst.Expr.Let(sym, e1, e2, t, loc)

    case ControlAst.Expr.LetRec(varSym, index, defSym, exp1, exp2, tpe, _, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      val t = visitType(tpe)
      FinalAst.Expr.LetRec(varSym, index, defSym, e1, e2, t, loc)

    case ControlAst.Expr.Region(tpe, loc) =>
      val op = AtomicOp.Region
      val t = visitType(tpe)
      FinalAst.Expr.ApplyAtomic(op, Nil, t, loc)

    case ControlAst.Expr.Scope(sym, exp, tpe, _, loc) =>
      val e = visitExp(exp)
      val t = visitType(tpe)
      FinalAst.Expr.Scope(sym, e, t, loc)

    case ControlAst.Expr.ScopeExit(exp1, exp2, tpe, _, loc) =>
      val op = AtomicOp.ScopeExit
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      val t = visitType(tpe)
      FinalAst.Expr.ApplyAtomic(op, List(e1, e2), t, loc)

    case ControlAst.Expr.TryCatch(exp, rules, tpe, _, loc) =>
      val e = visitExp(exp)
      val rs = rules map {
        case ControlAst.CatchRule(sym, clazz, body) =>
          val b = visitExp(body)
          FinalAst.CatchRule(sym, clazz, b)
      }
      val t = visitType(tpe)
      FinalAst.Expr.TryCatch(e, rs, t, loc)

    case ControlAst.Expr.NewObject(name, clazz, tpe, _, methods, loc) =>
      val t = visitType(tpe)
      val ms = methods.map(visitJvmMethod(_))
      FinalAst.Expr.NewObject(name, clazz, t, ms, loc)

    case ControlAst.Expr.Spawn(exp1, exp2, tpe, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      val t = visitType(tpe)
      FinalAst.Expr.Spawn(e1, e2, t, loc)
  }

  def visitType(tpe0: Type): MonoType = {

    def visit(t0: Type): MonoType = {

      val base = t0.typeConstructor
      val args = t0.typeArguments.map(visit)

      base match {
        case None => t0 match {
          case Type.Var(sym, _) => MonoType.Var(sym.id)
          case _ => throw InternalCompilerException(s"Unexpected type: $t0", t0.loc)
        }

        case Some(tc) =>
          tc match {
            case TypeConstructor.Unit => MonoType.Unit

            case TypeConstructor.Null => MonoType.Unit

            case TypeConstructor.Bool => MonoType.Bool

            case TypeConstructor.Char => MonoType.Char

            case TypeConstructor.Float32 => MonoType.Float32

            case TypeConstructor.Float64 => MonoType.Float64

            case TypeConstructor.BigDecimal => MonoType.BigDecimal

            case TypeConstructor.Int8 => MonoType.Int8

            case TypeConstructor.Int16 => MonoType.Int16

            case TypeConstructor.Int32 => MonoType.Int32

            case TypeConstructor.Int64 => MonoType.Int64

            case TypeConstructor.BigInt => MonoType.BigInt

            case TypeConstructor.Str => MonoType.Str

            case TypeConstructor.Regex => MonoType.Regex

            case TypeConstructor.RecordRowEmpty => MonoType.RecordEmpty()

            case TypeConstructor.Sender => throw InternalCompilerException("Unexpected Sender", tpe0.loc)

            case TypeConstructor.Receiver => throw InternalCompilerException("Unexpected Receiver", tpe0.loc)

            case TypeConstructor.Lazy => MonoType.Lazy(args.head)

            case TypeConstructor.Enum(sym, _) => MonoType.Enum(sym, args)

            case TypeConstructor.RestrictableEnum(sym, _) =>
              val enumSym = new Symbol.EnumSym(sym.namespace, sym.name, sym.loc)
              MonoType.Enum(enumSym, args)

            case TypeConstructor.Native(clazz) => MonoType.Native(clazz)

            case TypeConstructor.Array => MonoType.Array(args.head)

            case TypeConstructor.Vector => MonoType.Array(args.head)

            case TypeConstructor.Ref => MonoType.Ref(args.head)

            case TypeConstructor.RegionToStar => MonoType.Region

            case TypeConstructor.Tuple(l) => MonoType.Tuple(args)

            case TypeConstructor.Arrow(l) => MonoType.Arrow(args.drop(2).init, args.last)

            case TypeConstructor.RecordRowExtend(field) => MonoType.RecordExtend(field.name, args.head, args(1))

            case TypeConstructor.Record => args.head

            case TypeConstructor.True => MonoType.Unit

            case TypeConstructor.False => MonoType.Unit

            case TypeConstructor.Not => MonoType.Unit

            case TypeConstructor.And => MonoType.Unit

            case TypeConstructor.Or => MonoType.Unit

            case TypeConstructor.Complement => MonoType.Unit

            case TypeConstructor.Union => MonoType.Unit

            case TypeConstructor.Intersection => MonoType.Unit

            case TypeConstructor.Effect(_) => MonoType.Unit

            case TypeConstructor.Empty => MonoType.Unit

            case TypeConstructor.All => MonoType.Unit

            case TypeConstructor.CaseSet(sym, enumSym) => MonoType.Unit
            case TypeConstructor.CaseComplement(sym) => MonoType.Unit
            case TypeConstructor.CaseIntersection(sym) => MonoType.Unit
            case TypeConstructor.CaseUnion(sym) => MonoType.Unit

            case TypeConstructor.Relation =>
              throw InternalCompilerException(s"Unexpected type: '$t0'.", t0.loc)

            case TypeConstructor.Lattice =>
              throw InternalCompilerException(s"Unexpected type: '$t0'.", t0.loc)

            case TypeConstructor.SchemaRowEmpty =>
              throw InternalCompilerException(s"Unexpected type: '$t0'.", t0.loc)

            case TypeConstructor.SchemaRowExtend(pred) =>
              throw InternalCompilerException(s"Unexpected type: '$t0'.", t0.loc)

            case TypeConstructor.Schema =>
              throw InternalCompilerException(s"Unexpected type: '$t0'.", t0.loc)
          }
      }
    }

    visit(Type.eraseAliases(tpe0))
  }

  private def visitFormalParam(p0: ControlAst.FormalParam): FinalAst.FormalParam = {
    val tpe = visitType(p0.tpe)
    FinalAst.FormalParam(p0.sym, tpe)
  }

  private def visitJvmMethod(method: ControlAst.JvmMethod)(implicit flix: Flix): FinalAst.JvmMethod = method match {
    case ControlAst.JvmMethod(ident, fparams, clo, retTpe, purity, loc) =>
      val f = fparams.map(visitFormalParam)
      val c = visitExp(clo)
      val t = visitType(retTpe)
      FinalAst.JvmMethod(ident, f, c, t, loc)
  }
}

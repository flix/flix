/*
 * Copyright 2023 Magnus Madsen
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

import scala.collection.mutable

object Reducer {

  def run(root: LiftedAst.Root)(implicit flix: Flix): ReducedAst.Root = flix.phase("Reducer") {

    implicit val ctx: Context = Context(mutable.ListBuffer.empty)

    val newDefs = root.defs.map {
      case (sym, d) => sym -> visitDef(d)
    }
    val newEnums = root.enums.map {
      case (sym, d) => sym -> visitEnum(d)
    }

    ReducedAst.Root(newDefs, newEnums, ctx.anonClasses.toList, root.entryPoint, root.sources)
  }

  private def visitDef(d: LiftedAst.Def)(implicit ctx: Context): ReducedAst.Def = d match {
    case LiftedAst.Def(ann, mod, sym, cparams, fparams, exp, tpe, purity, loc) =>
      val cs = cparams.map(visitFormalParam)
      val fs = fparams.map(visitFormalParam)
      val e = visitExpr(exp)
      val stmt = ReducedAst.Stmt.Ret(e, e.tpe, e.loc)
      val t = visitType(tpe)
      ReducedAst.Def(ann, mod, sym, cs, fs, stmt, t, purity, loc)
  }

  private def visitEnum(d: LiftedAst.Enum): ReducedAst.Enum = d match {
    case LiftedAst.Enum(ann, mod, sym, cases0, tpe, loc) =>
      val cases = cases0.map {
        case (sym, caze) => sym -> visitCase(caze)
      }
      val t = visitType(tpe)
      ReducedAst.Enum(ann, mod, sym, cases, t, loc)
  }

  private def visitExpr(exp0: LiftedAst.Expr)(implicit ctx: Context): ReducedAst.Expr = exp0 match {
    case LiftedAst.Expr.Cst(cst, tpe, loc) =>
      val t = visitType(tpe)
      ReducedAst.Expr.Cst(cst, t, loc)

    case LiftedAst.Expr.Var(sym, tpe, loc) =>
      val t = visitType(tpe)
      ReducedAst.Expr.Var(sym, t, loc)

    case LiftedAst.Expr.ApplyAtomic(op, exps, tpe, purity, loc) =>
      val es = exps.map(visitExpr)
      val t = visitType(tpe)
      ReducedAst.Expr.ApplyAtomic(op, es, t, purity, loc)

    case LiftedAst.Expr.ApplyClo(exp, exps, ct, tpe, purity, loc) =>
      val e = visitExpr(exp)
      val es = exps.map(visitExpr)
      val t = visitType(tpe)
      ReducedAst.Expr.ApplyClo(e, es, ct, t, purity, loc)

    case LiftedAst.Expr.ApplyDef(sym, exps, ct, tpe, purity, loc) =>
      val es = exps.map(visitExpr)
      val t = visitType(tpe)
      ReducedAst.Expr.ApplyDef(sym, es, ct, t, purity, loc)

    case LiftedAst.Expr.ApplySelfTail(sym, formals, exps, tpe, purity, loc) =>
      val fs = formals.map(visitFormalParam)
      val as = exps.map(visitExpr)
      val t = visitType(tpe)
      ReducedAst.Expr.ApplySelfTail(sym, fs, as, t, purity, loc)

    case LiftedAst.Expr.IfThenElse(exp1, exp2, exp3, tpe, purity, loc) =>
      val e1 = visitExpr(exp1)
      val e2 = visitExpr(exp2)
      val e3 = visitExpr(exp3)
      val t = visitType(tpe)
      ReducedAst.Expr.IfThenElse(e1, e2, e3, t, purity, loc)

    case LiftedAst.Expr.Branch(exp, branches, tpe, purity, loc) =>
      val e = visitExpr(exp)
      val bs = branches map {
        case (label, body) => label -> visitExpr(body)
      }
      val t = visitType(tpe)
      ReducedAst.Expr.Branch(e, bs, t, purity, loc)

    case LiftedAst.Expr.JumpTo(sym, tpe, purity, loc) =>
      val t = visitType(tpe)
      ReducedAst.Expr.JumpTo(sym, t, purity, loc)

    case LiftedAst.Expr.Let(sym, exp1, exp2, tpe, purity, loc) =>
      val e1 = visitExpr(exp1)
      val e2 = visitExpr(exp2)
      val t = visitType(tpe)
      ReducedAst.Expr.Let(sym, e1, e2, t, purity, loc)

    case LiftedAst.Expr.LetRec(varSym, index, defSym, exp1, exp2, tpe, purity, loc) =>
      val e1 = visitExpr(exp1)
      val e2 = visitExpr(exp2)
      val t = visitType(tpe)
      ReducedAst.Expr.LetRec(varSym, index, defSym, e1, e2, t, purity, loc)

    case LiftedAst.Expr.Scope(sym, exp, tpe, purity, loc) =>
      val e = visitExpr(exp)
      val t = visitType(tpe)
      ReducedAst.Expr.Scope(sym, e, t, purity, loc)

    case LiftedAst.Expr.TryCatch(exp, rules, tpe, purity, loc) =>
      val e = visitExpr(exp)
      val rs = rules map {
        case LiftedAst.CatchRule(sym, clazz, body) =>
          val b = visitExpr(body)
          ReducedAst.CatchRule(sym, clazz, b)
      }
      val t = visitType(tpe)
      ReducedAst.Expr.TryCatch(e, rs, t, purity, loc)

    case LiftedAst.Expr.TryWith(exp, effUse, rules, tpe, purity, loc) =>
      // TODO AE erasing to unit for now
      ReducedAst.Expr.Cst(Ast.Constant.Unit, MonoType.Unit, loc)

    case LiftedAst.Expr.Do(op, exps, tpe, purity, loc) =>
      // TODO AE erasing to unit for now
      ReducedAst.Expr.Cst(Ast.Constant.Unit, MonoType.Unit, loc)

    case LiftedAst.Expr.Resume(exp, tpe, loc) =>
      // TODO AE erasing to unit for now
      ReducedAst.Expr.Cst(Ast.Constant.Unit, MonoType.Unit, loc)

    case LiftedAst.Expr.NewObject(name, clazz, tpe, purity, methods, loc) =>
      val es = methods.map(m => visitExpr(m.clo))
      val t = visitType(tpe)
      val specs = methods.map {
        case LiftedAst.JvmMethod(ident, fparams, _, retTpe, purity, loc) =>
          val f = fparams.map(visitFormalParam)
          val rt = visitType(retTpe)
          ReducedAst.JvmMethod(ident, f, rt, purity, loc)
      }
      ctx.anonClasses += ReducedAst.AnonClass(name, clazz, t, specs, loc)

      ReducedAst.Expr.NewObject(name, clazz, t, purity, specs, es, loc)

  }

  private def visitType(tpe: Type): MonoType = {
    val base = tpe.typeConstructor
    val args = tpe.typeArguments.map(visitType)

    base match {
      case None => tpe match {
        case _ => throw InternalCompilerException(s"Unexpected type: $tpe", tpe.loc)
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

          case TypeConstructor.Sender => throw InternalCompilerException("Unexpected Sender", tpe.loc)

          case TypeConstructor.Receiver => throw InternalCompilerException("Unexpected Receiver", tpe.loc)

          case TypeConstructor.Lazy => MonoType.Lazy(args.head)

          case TypeConstructor.Enum(sym, _) => MonoType.Enum(sym)

          case TypeConstructor.RestrictableEnum(sym, _) =>
            val enumSym = new Symbol.EnumSym(None, sym.namespace, sym.name, sym.loc)
            MonoType.Enum(enumSym)

          case TypeConstructor.Native(clazz) => MonoType.Native(clazz)

          case TypeConstructor.Array => MonoType.Array(args.head)

          case TypeConstructor.Vector => MonoType.Array(args.head)

          case TypeConstructor.Ref => MonoType.Ref(args.head)

          case TypeConstructor.RegionToStar => MonoType.Region

          case TypeConstructor.Tuple(_) => MonoType.Tuple(args)

          case TypeConstructor.Arrow(_) => MonoType.Arrow(args.drop(1).init, args.last) // Erase the purity

          case TypeConstructor.RecordRowExtend(field) => MonoType.RecordExtend(field.name, args.head, args(1))

          case TypeConstructor.Record => args.head

          case TypeConstructor.True => MonoType.Unit
          case TypeConstructor.False => MonoType.Unit
          case TypeConstructor.Not => MonoType.Unit
          case TypeConstructor.And => MonoType.Unit
          case TypeConstructor.Or => MonoType.Unit

          case TypeConstructor.Pure => MonoType.Unit
          case TypeConstructor.EffUniv => MonoType.Unit
          case TypeConstructor.Complement => MonoType.Unit
          case TypeConstructor.Union => MonoType.Unit
          case TypeConstructor.Intersection => MonoType.Unit
          case TypeConstructor.Effect(_) => MonoType.Unit
          case TypeConstructor.CaseSet(_, _) => MonoType.Unit
          case TypeConstructor.CaseComplement(_) => MonoType.Unit
          case TypeConstructor.CaseIntersection(_) => MonoType.Unit
          case TypeConstructor.CaseUnion(_) => MonoType.Unit

          case TypeConstructor.Relation =>
            throw InternalCompilerException(s"Unexpected type: '$tpe'.", tpe.loc)

          case TypeConstructor.Lattice =>
            throw InternalCompilerException(s"Unexpected type: '$tpe'.", tpe.loc)

          case TypeConstructor.SchemaRowEmpty =>
            throw InternalCompilerException(s"Unexpected type: '$tpe'.", tpe.loc)

          case TypeConstructor.SchemaRowExtend(_) =>
            throw InternalCompilerException(s"Unexpected type: '$tpe'.", tpe.loc)

          case TypeConstructor.Schema =>
            throw InternalCompilerException(s"Unexpected type: '$tpe'.", tpe.loc)
        }
    }
  }

  private def visitCase(caze: LiftedAst.Case): ReducedAst.Case = caze match {
    case LiftedAst.Case(sym, tpe, loc) =>
      val t = visitType(tpe)
      ReducedAst.Case(sym, t, loc)
  }

  private def visitFormalParam(fparam: LiftedAst.FormalParam): ReducedAst.FormalParam = fparam match {
    case LiftedAst.FormalParam(sym, mod, tpe, loc) =>
      val t = visitType(tpe)
      ReducedAst.FormalParam(sym, mod, t, loc)
  }

  private case class Context(anonClasses: mutable.ListBuffer[ReducedAst.AnonClass])

}

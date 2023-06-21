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
import ca.uwaterloo.flix.language.ast.ReducedAst.Stmt
import ca.uwaterloo.flix.language.ast._
import ca.uwaterloo.flix.util.InternalCompilerException

object MonoTyper {

  def run(root: ReducedAst.Root)(implicit flix: Flix): MonoTypedAst.Root = flix.phase("Finalize") {
    val defs = root.defs.map {
      case (k, v) => k -> visitDef(v)
    }

    val enums = root.enums.map {
      case (sym, enum) => sym -> visitEnum(enum)
    }

    MonoTypedAst.Root(defs, enums, root.entryPoint, root.sources)
  }

  private def visitDef(def0: ReducedAst.Def)(implicit flix: Flix): MonoTypedAst.Def = {
    val cs = def0.cparams.map(visitFormalParam)
    val fs = def0.fparams.map(visitFormalParam)
    val s = visitStmt(def0.stmt)
    val tpe0 = visitType(def0.tpe)
    val tpe = MonoType.Arrow(fs.map(_.tpe), tpe0)
    MonoTypedAst.Def(def0.ann, def0.mod, def0.sym, cs, fs, s, tpe, def0.loc)
  }

  private def visitEnum(enum0: ReducedAst.Enum)(implicit flix: Flix): MonoTypedAst.Enum = enum0 match {
    case ReducedAst.Enum(ann, mod, sym, cases0, tpe0, loc) =>
      val cases = cases0.map {
        case (tag, ReducedAst.Case(enumSym, tagType, tagLoc)) =>
          val tpe = visitType(tagType)
          tag -> MonoTypedAst.Case(enumSym, tpe, tagLoc)
      }
      val tpe = visitType(tpe0)
      MonoTypedAst.Enum(ann, mod, sym, cases, tpe, loc)
  }

  private def visitExpr(exp0: ReducedAst.Expr)(implicit flix: Flix): MonoTypedAst.Expr = exp0 match {
    case ReducedAst.Expr.Cst(cst, tpe, loc) =>
      val t = visitType(tpe)
      MonoTypedAst.Expr.Cst(cst, t, loc)

    case ReducedAst.Expr.Var(sym, tpe, loc) =>
      val t = visitType(tpe)
      MonoTypedAst.Expr.Var(sym, t, loc)

    case ReducedAst.Expr.Closure(sym, exps, tpe, loc) =>
      val op = AtomicOp.Closure(sym)
      val es = exps.map(visitExpr)
      val t = visitType(tpe)
      MonoTypedAst.Expr.ApplyAtomic(op, es, t, loc)

    case ReducedAst.Expr.ApplyAtomic(op, exps, tpe, _, loc) =>
      val es = exps.map(visitExpr)
      val t = visitType(tpe)
      MonoTypedAst.Expr.ApplyAtomic(op, es, t, loc)

    case ReducedAst.Expr.ApplyClo(exp, exps, ct, tpe, _, loc) =>
      val es = exps map visitExpr
      val t = visitType(tpe)
      MonoTypedAst.Expr.ApplyClo(visitExpr(exp), es, ct, t, loc)

    case ReducedAst.Expr.ApplyDef(sym, exps, ct, tpe, _, loc) =>
      val es = exps map visitExpr
      val t = visitType(tpe)
      MonoTypedAst.Expr.ApplyDef(sym, es, ct, t, loc)

    case ReducedAst.Expr.ApplySelfTail(name, formals, actuals, tpe, _, loc) =>
      val fs = formals.map(visitFormalParam)
      val as = actuals.map(visitExpr)
      val t = visitType(tpe)
      MonoTypedAst.Expr.ApplySelfTail(name, fs, as, t, loc)

    case ReducedAst.Expr.IfThenElse(exp1, exp2, exp3, tpe, _, loc) =>
      val e1 = visitExpr(exp1)
      val e2 = visitExpr(exp2)
      val v3 = visitExpr(exp3)
      val t = visitType(tpe)
      MonoTypedAst.Expr.IfThenElse(e1, e2, v3, t, loc)

    case ReducedAst.Expr.Branch(exp, branches, tpe, _, loc) =>
      val e = visitExpr(exp)
      val bs = branches map {
        case (sym, br) => sym -> visitExpr(br)
      }
      val t = visitType(tpe)
      MonoTypedAst.Expr.Branch(e, bs, t, loc)

    case ReducedAst.Expr.JumpTo(sym, tpe, _, loc) =>
      val t = visitType(tpe)
      MonoTypedAst.Expr.JumpTo(sym, t, loc)

    case ReducedAst.Expr.Let(sym, exp1, exp2, tpe, _, loc) =>
      val e1 = visitExpr(exp1)
      val e2 = visitExpr(exp2)
      val t = visitType(tpe)
      MonoTypedAst.Expr.Let(sym, e1, e2, t, loc)

    case ReducedAst.Expr.LetRec(varSym, index, defSym, exp1, exp2, tpe, _, loc) =>
      val e1 = visitExpr(exp1)
      val e2 = visitExpr(exp2)
      val t = visitType(tpe)
      MonoTypedAst.Expr.LetRec(varSym, index, defSym, e1, e2, t, loc)

    case ReducedAst.Expr.Scope(sym, exp, tpe, _, loc) =>
      val e = visitExpr(exp)
      val t = visitType(tpe)
      MonoTypedAst.Expr.Scope(sym, e, t, loc)

    case ReducedAst.Expr.TryCatch(exp, rules, tpe, _, loc) =>
      val e = visitExpr(exp)
      val rs = rules map {
        case ReducedAst.CatchRule(sym, clazz, body) =>
          val b = visitExpr(body)
          MonoTypedAst.CatchRule(sym, clazz, b)
      }
      val t = visitType(tpe)
      MonoTypedAst.Expr.TryCatch(e, rs, t, loc)

    case ReducedAst.Expr.NewObject(name, clazz, tpe, _, methods, loc) =>
      val t = visitType(tpe)
      val ms = methods.map(visitJvmMethod(_))
      MonoTypedAst.Expr.NewObject(name, clazz, t, ms, loc)

  }

  private def visitStmt(stmt0: ReducedAst.Stmt)(implicit flix: Flix): MonoTypedAst.Stmt = stmt0 match {
    case Stmt.Ret(exp, tpe, loc) =>
      val e = visitExpr(exp)
      val t = visitType(tpe)
      MonoTypedAst.Stmt.Ret(e, t, loc)
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

  private def visitFormalParam(p0: ReducedAst.FormalParam): MonoTypedAst.FormalParam = {
    val tpe = visitType(p0.tpe)
    MonoTypedAst.FormalParam(p0.sym, tpe)
  }

  private def visitJvmMethod(method: ReducedAst.JvmMethod)(implicit flix: Flix): MonoTypedAst.JvmMethod = method match {
    case ReducedAst.JvmMethod(ident, fparams, clo, retTpe, _, loc) =>
      val f = fparams.map(visitFormalParam)
      val c = visitExpr(clo)
      val t = visitType(retTpe)
      MonoTypedAst.JvmMethod(ident, f, c, t, loc)
  }
}

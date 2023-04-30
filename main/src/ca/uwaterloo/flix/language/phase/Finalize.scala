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

  private def visitExp(exp0: ControlAst.Expression)(implicit flix: Flix): FinalAst.Expression = {

    def visit(e0: ControlAst.Expression): FinalAst.Expression = e0 match {
      case ControlAst.Expression.Cst(cst, tpe, loc) =>
        val t = visitType(tpe)
        FinalAst.Expression.Cst(cst, t, loc)

      case ControlAst.Expression.Var(sym, tpe, loc) =>
        val t = visitType(tpe)
        FinalAst.Expression.Var(sym, t, loc)

      case ControlAst.Expression.Closure(sym, exps, tpe, loc) =>
        val op = AtomicOp.Closure(sym)
        val es = exps.map(visit)
        val t = visitType(tpe)
        FinalAst.Expression.ApplyAtomic(op, es, t, loc)

      case ControlAst.Expression.ApplyAtomic(op, exps, tpe, purity, loc) =>
        val es = exps.map(visit)
        val t = visitType(tpe)
        FinalAst.Expression.ApplyAtomic(op, es, t, loc)

      case ControlAst.Expression.ApplyClo(exp, args, tpe, _, loc) =>
        val as = args map visit
        val t = visitType(tpe)
        FinalAst.Expression.ApplyClo(visit(exp), as, t, loc)

      case ControlAst.Expression.ApplyDef(name, args, tpe, _, loc) =>
        val as = args map visit
        val t = visitType(tpe)
        FinalAst.Expression.ApplyDef(name, as, t, loc)

      case ControlAst.Expression.ApplyCloTail(exp, args, tpe, _, loc) =>
        val e = visit(exp)
        val rs = args map visit
        val t = visitType(tpe)
        FinalAst.Expression.ApplyCloTail(e, rs, t, loc)

      case ControlAst.Expression.ApplyDefTail(sym, args, tpe, _, loc) =>
        val as = args map visit
        val t = visitType(tpe)
        FinalAst.Expression.ApplyDefTail(sym, as, t, loc)

      case ControlAst.Expression.ApplySelfTail(name, formals, actuals, tpe, _, loc) =>
        val fs = formals.map(visitFormalParam)
        val as = actuals.map(visit)
        val t = visitType(tpe)
        FinalAst.Expression.ApplySelfTail(name, fs, as, t, loc)

      case ControlAst.Expression.IfThenElse(exp1, exp2, exp3, tpe, _, loc) =>
        val e1 = visit(exp1)
        val e2 = visit(exp2)
        val v3 = visit(exp3)
        val t = visitType(tpe)
        FinalAst.Expression.IfThenElse(e1, e2, v3, t, loc)

      case ControlAst.Expression.Branch(exp, branches, tpe, _, loc) =>
        val e = visit(exp)
        val bs = branches map {
          case (sym, br) => sym -> visit(br)
        }
        val t = visitType(tpe)
        FinalAst.Expression.Branch(e, bs, t, loc)

      case ControlAst.Expression.JumpTo(sym, tpe, _, loc) =>
        val t = visitType(tpe)
        FinalAst.Expression.JumpTo(sym, t, loc)

      case ControlAst.Expression.Let(sym, exp1, exp2, tpe, _, loc) =>
        val e1 = visit(exp1)
        val e2 = visit(exp2)
        val t = visitType(tpe)
        FinalAst.Expression.Let(sym, e1, e2, t, loc)

      case ControlAst.Expression.LetRec(varSym, index, defSym, exp1, exp2, tpe, _, loc) =>
        val e1 = visit(exp1)
        val e2 = visit(exp2)
        val t = visitType(tpe)
        FinalAst.Expression.LetRec(varSym, index, defSym, e1, e2, t, loc)

      case ControlAst.Expression.Region(tpe, loc) =>
        val op = AtomicOp.Region
        val t = visitType(tpe)
        FinalAst.Expression.ApplyAtomic(op, Nil, t, loc)

      case ControlAst.Expression.Scope(sym, exp, tpe, _, loc) =>
        val e = visit(exp)
        val t = visitType(tpe)
        FinalAst.Expression.Scope(sym, e, t, loc)

      case ControlAst.Expression.ScopeExit(exp1, exp2, tpe, _, loc) =>
        val op = AtomicOp.ScopeExit
        val e1 = visit(exp1)
        val e2 = visit(exp2)
        val t = visitType(tpe)
        FinalAst.Expression.ApplyAtomic(op, List(e1, e2), t, loc)

      case ControlAst.Expression.Is(sym, exp, _, loc) =>
        val op = AtomicOp.Is(sym)
        val e = visit(exp)
        FinalAst.Expression.ApplyAtomic(op, List(e), MonoType.Bool, loc)

      case ControlAst.Expression.Tag(sym, exp, tpe, _, loc) =>
        val op = AtomicOp.Tag(sym)
        val e = visit(exp)
        val t = visitType(tpe)
        FinalAst.Expression.ApplyAtomic(op, List(e), t, loc)

      case ControlAst.Expression.Untag(sym, exp, tpe, _, loc) =>
        val op = AtomicOp.Untag(sym)
        val e = visit(exp)
        val t = visitType(tpe)
        FinalAst.Expression.ApplyAtomic(op, List(e), t, loc)

      case ControlAst.Expression.Ref(exp, tpe, loc) =>
        val e = visit(exp)
        val t = visitType(tpe)
        FinalAst.Expression.Ref(e, t, loc)

      case ControlAst.Expression.Deref(exp, tpe, loc) =>
        val e = visit(exp)
        val t = visitType(tpe)
        FinalAst.Expression.Deref(e, t, loc)

      case ControlAst.Expression.Assign(exp1, exp2, tpe, loc) =>
        val e1 = visit(exp1)
        val e2 = visit(exp2)
        val t = visitType(tpe)
        FinalAst.Expression.Assign(e1, e2, t, loc)

      case ControlAst.Expression.InstanceOf(exp, clazz, loc) =>
        val e = visit(exp)
        FinalAst.Expression.InstanceOf(e, clazz, loc)

      case ControlAst.Expression.Cast(exp, tpe, _, loc) =>
        val e = visit(exp)
        val t = visitType(tpe)
        FinalAst.Expression.Cast(e, t, loc)

      case ControlAst.Expression.TryCatch(exp, rules, tpe, _, loc) =>
        val e = visit(exp)
        val rs = rules map {
          case ControlAst.CatchRule(sym, clazz, body) =>
            val b = visit(body)
            FinalAst.CatchRule(sym, clazz, b)
        }
        val t = visitType(tpe)
        FinalAst.Expression.TryCatch(e, rs, t, loc)

      case ControlAst.Expression.NewObject(name, clazz, tpe, _, methods, loc) =>
        val t = visitType(tpe)
        val ms = methods.map(visitJvmMethod(_))
        FinalAst.Expression.NewObject(name, clazz, t, ms, loc)

      case ControlAst.Expression.Spawn(exp1, exp2, tpe, loc) =>
        val e1 = visit(exp1)
        val e2 = visit(exp2)
        val t = visitType(tpe)
        FinalAst.Expression.Spawn(e1, e2, t, loc)

      case ControlAst.Expression.Lazy(exp, tpe, loc) =>
        val e = visit(exp)
        val t = visitType(tpe)
        FinalAst.Expression.Lazy(e, t, loc)

      case ControlAst.Expression.Force(exp, tpe, loc) =>
        val e = visit(exp)
        val t = visitType(tpe)
        FinalAst.Expression.Force(e, t, loc)

      case ControlAst.Expression.HoleError(sym, tpe, loc) =>
        val t = visitType(tpe)
        FinalAst.Expression.HoleError(sym, t, loc)

      case ControlAst.Expression.MatchError(tpe, loc) =>
        val t = visitType(tpe)
        FinalAst.Expression.MatchError(t, loc)
    }

    visit(exp0)
  }

  private def visitFormalParam(p0: ControlAst.FormalParam): FinalAst.FormalParam = {
    val tpe = visitType(p0.tpe)
    FinalAst.FormalParam(p0.sym, tpe)
  }

  // TODO: Should be private
  // TODO: Remove

  /**
    * Finalizes the given type.
    */
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

  private def visitJvmMethod(method: ControlAst.JvmMethod)(implicit flix: Flix): FinalAst.JvmMethod = method match {
    case ControlAst.JvmMethod(ident, fparams, clo, retTpe, purity, loc) =>
      val f = fparams.map(visitFormalParam)
      val c = visitExp(clo)
      val t = visitType(retTpe)
      FinalAst.JvmMethod(ident, f, c, t, loc)
  }
}

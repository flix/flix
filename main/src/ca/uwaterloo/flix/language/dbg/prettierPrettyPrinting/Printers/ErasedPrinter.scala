package ca.uwaterloo.flix.language.dbg.prettierPrettyPrinting.Printers

import ca.uwaterloo.flix.language.ast.ErasedAst
import ca.uwaterloo.flix.language.ast.ErasedAst.Expression._
import ca.uwaterloo.flix.language.ast.ErasedAst._
import ca.uwaterloo.flix.language.dbg.prettierPrettyPrinting.DocAst

object ErasedPrinter {

  /**
    * Returns the [[DocAst.Program]] representation of `root`.
    */
  def print(root: ErasedAst.Root): DocAst.Program = {
    val enums = root.enums.values.map {
      case ErasedAst.Enum(ann, mod, sym, cases0, _, _) =>
        val cases = cases0.values.map {
          case ErasedAst.Case(sym, _, _) => DocAst.Case(sym)
        }.toList
        DocAst.Enum(ann, mod, sym, cases)
    }.toList
    val defs = root.defs.values.map {
      case ErasedAst.Def(ann, mod, sym, formals, exp, tpe, _) =>
        DocAst.Def(
          ann,
          mod,
          sym,
          formals.map(printFormalParam),
          MonoTypePrinter.print(tpe),
          print(exp)
        )
    }.toList
    DocAst.Program(enums, defs)
  }

  /**
    * Returns the [[DocAst]] representation of `e`.
    */
  def print(e: ErasedAst.Expression): DocAst = e match {
    case Var(sym, _, _) => DocAst.VarWithOffset(sym)
    case Unary(sop, exp, _, _) => DocAst.Unary(OperatorPrinter.print(sop), print(exp))
    case Binary(sop, _, exp1, exp2, _, _) => DocAst.Binary(print(exp1), OperatorPrinter.print(sop), print(exp2))
    case IfThenElse(exp1, exp2, exp3, _, _) => DocAst.IfThenElse(print(exp1), print(exp2), print(exp3))
    case Branch(exp, branches, _, _) => DocAst.Branch(print(exp), branches.view.mapValues(print).toMap)
    case JumpTo(sym, _, _) => DocAst.JumpTo(sym)
    case Let(sym, exp1, exp2, _, _) => DocAst.Let(DocAst.VarWithOffset(sym), None, print(exp1), print(exp2))
    case LetRec(varSym, _, _, exp1, exp2, _, _) => DocAst.LetRec(DocAst.VarWithOffset(varSym), None, print(exp1), print(exp2))
    case Scope(sym, exp, _, _) => DocAst.Scope(DocAst.VarWithOffset(sym), print(exp))
    case ScopeExit(exp1, exp2, _, _) => DocAst.ScopeExit(print(exp1), print(exp2))
    case TryCatch(exp, rules, _, _) => DocAst.TryCatch(print(exp), rules.map(r => (r.sym, r.clazz, print(r.exp))))
    case NewObject(name, clazz, tpe, methods, _) =>
      DocAst.NewObject(name, clazz, MonoTypePrinter.print(tpe), methods.map {
        case JvmMethod(ident, fparams, clo, retTpe, _) =>
          DocAst.JvmMethod(ident, fparams.map(printFormalParam), print(clo), MonoTypePrinter.print(retTpe))
      })
    case Intrinsic0(op, _, _) => op match {
      case IntrinsicOperator0.Cst(cst) => DocAst.Cst(cst)
      case IntrinsicOperator0.Region => DocAst.Region
      case IntrinsicOperator0.RecordEmpty => DocAst.RecordEmpty
      case IntrinsicOperator0.GetStaticField(field) => DocAst.JavaGetStaticField(field)
      case IntrinsicOperator0.HoleError(sym) => DocAst.HoleError(sym)
      case IntrinsicOperator0.MatchError => DocAst.MatchError
    }
    case Intrinsic1(op, exp, tpe, _) =>
      val d = print(exp)
      op match {
        case IntrinsicOperator1.Is(sym) => DocAst.IsTag(sym, d)
        case IntrinsicOperator1.Tag(sym) => DocAst.Tag(sym, List(d))
        case IntrinsicOperator1.Untag(sym) => DocAst.Untag(sym, d)
        case IntrinsicOperator1.Cast => DocAst.Cast(d, MonoTypePrinter.print(tpe))
        case IntrinsicOperator1.Index(idx) => DocAst.Index(idx, d)
        case IntrinsicOperator1.RecordSelect(field) => DocAst.RecordSelect(field, d)
        case IntrinsicOperator1.RecordRestrict(field) => DocAst.RecordRestrict(field, d)
        case IntrinsicOperator1.Ref => DocAst.Ref(d)
        case IntrinsicOperator1.Deref => DocAst.Deref(d)
        case IntrinsicOperator1.ArrayLength => DocAst.ArrayLength(d)
        case IntrinsicOperator1.Lazy => DocAst.Lazy(d)
        case IntrinsicOperator1.Force => DocAst.Force(d)
        case IntrinsicOperator1.GetField(field) => DocAst.JavaGetField(field, d)
        case IntrinsicOperator1.PutStaticField(field) => DocAst.JavaPutStaticField(field, d)
        case IntrinsicOperator1.BoxBool => DocAst.Box(d)
        case IntrinsicOperator1.BoxInt8 => DocAst.Box(d)
        case IntrinsicOperator1.BoxInt16 => DocAst.Box(d)
        case IntrinsicOperator1.BoxInt32 => DocAst.Box(d)
        case IntrinsicOperator1.BoxInt64 => DocAst.Box(d)
        case IntrinsicOperator1.BoxChar => DocAst.Box(d)
        case IntrinsicOperator1.BoxFloat32 => DocAst.Box(d)
        case IntrinsicOperator1.BoxFloat64 => DocAst.Box(d)
        case IntrinsicOperator1.UnboxBool => DocAst.Unbox(d)
        case IntrinsicOperator1.UnboxInt8 => DocAst.Unbox(d)
        case IntrinsicOperator1.UnboxInt16 => DocAst.Unbox(d)
        case IntrinsicOperator1.UnboxInt32 => DocAst.Unbox(d)
        case IntrinsicOperator1.UnboxInt64 => DocAst.Unbox(d)
        case IntrinsicOperator1.UnboxChar => DocAst.Unbox(d)
        case IntrinsicOperator1.UnboxFloat32 => DocAst.Unbox(d)
        case IntrinsicOperator1.UnboxFloat64 => DocAst.Unbox(d)
      }
    case Intrinsic2(op, exp1, exp2, _, _) =>
      val d1 = print(exp1)
      val d2 = print(exp2)
      op match {
        case IntrinsicOperator2.RecordExtend(field) => DocAst.RecordExtend(field, d1, d2)
        case IntrinsicOperator2.Assign => DocAst.Assign(d1, d2)
        case IntrinsicOperator2.ArrayNew => DocAst.ArrayNew(d1, d2)
        case IntrinsicOperator2.ArrayLoad => DocAst.ArrayLoad(d1, d2)
        case IntrinsicOperator2.Spawn => DocAst.Spawn(d1, d2)
        case IntrinsicOperator2.PutField(field) => DocAst.JavaPutField(field, d1, d2)
      }
    case Intrinsic3(op, exp1, exp2, exp3, _, _) =>
      val d1 = print(exp1)
      val d2 = print(exp2)
      val d3 = print(exp3)
      op match {
        case IntrinsicOperator3.ArrayStore => DocAst.ArrayStore(d1, d2, d3)
      }
    case IntrinsicN(op, exps, _, _) =>
      val ds = exps.map(print)
      op match {
        case IntrinsicOperatorN.Closure(sym) => DocAst.ClosureLifted(sym, ds)
        case IntrinsicOperatorN.ApplyDef(sym) => DocAst.App(sym, ds)
        case IntrinsicOperatorN.ApplyDefTail(sym) => DocAst.AppDefTail(sym, ds)
        case IntrinsicOperatorN.ApplySelfTail(sym, _) => DocAst.AppSelfTail(sym, ds)
        case IntrinsicOperatorN.Tuple => DocAst.Tuple(ds)
        case IntrinsicOperatorN.ArrayLit => DocAst.ArrayLit(ds)
        case IntrinsicOperatorN.InvokeConstructor(constructor) => DocAst.JavaInvokeConstructor(constructor, ds)
        case IntrinsicOperatorN.InvokeStaticMethod(method) => DocAst.JavaInvokeStaticMethod(method, ds)
      }
    case Intrinsic1N(op, exp, exps, _, _) =>
      val d = print(exp)
      val ds = exps.map(print)
      op match {
        case IntrinsicOperator1N.ApplyClo => DocAst.AppClo(d, ds)
        case IntrinsicOperator1N.ApplyCloTail => DocAst.AppCloTail(d, ds)
        case IntrinsicOperator1N.InvokeMethod(method) => DocAst.JavaInvokeMethod(method, d, ds)
      }
  }

  /**
    * Returns the [[DocAst.Ascription]] representation of `fp`.
    */
  private def printFormalParam(fp: ErasedAst.FormalParam): DocAst.Ascription = {
    val ErasedAst.FormalParam(sym, tpe) = fp
    DocAst.Ascription(DocAst.VarWithOffset(sym), MonoTypePrinter.print(tpe))
  }

}

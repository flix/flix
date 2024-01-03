package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.LiftedAst.Expr._
import ca.uwaterloo.flix.language.ast.LiftedAst.{CatchRule, Def, Expr, FormalParam, HandlerRule, JvmMethod, Root}
import ca.uwaterloo.flix.language.ast.MonoType._
import ca.uwaterloo.flix.language.ast.{AtomicOp, MonoType, Symbol}
import ca.uwaterloo.flix.util.ParOps

/**
  * Erase types and introduce corresponding casting
  * - Enum tag values are erased (todo enum def)
  * - `A -> B` types become `A -> Obj` (TODO)
  * - non-primitive function return values are `Obj`
  */
object Eraser {

  def run(root: Root)(implicit flix: Flix): Root = flix.phase("Eraser") {
    val newDefs = ParOps.parMapValues(root.defs)(visitDef)
    root.copy(defs = newDefs)
  }

  private def visitDef(defn: Def): Def = defn match {
    case Def(ann, mod, sym, cparams, fparams, exp, tpe, purity, loc) =>
      Def(ann, mod, sym, cparams.map(visitParam), fparams.map(visitParam), visitExp(exp), visitType(tpe), purity, loc)
  }

  private def visitParam(fp: FormalParam): FormalParam = fp match {
    case FormalParam(sym, mod, tpe, loc) =>
      FormalParam(sym, mod, visitType(tpe), loc)
  }

  private def visitBranch(branch: (Symbol.LabelSym, Expr)): (Symbol.LabelSym, Expr) = branch match {
    case (sym, exp) =>
      (sym, visitExp(exp))
  }

  private def visitCatchRule(rule: CatchRule): CatchRule = rule match {
    case CatchRule(sym, clazz, exp) =>
      CatchRule(sym, clazz, visitExp(exp))
  }

  private def visitHandlerRule(rule: HandlerRule): HandlerRule = rule match {
    case HandlerRule(op, fparams, exp) =>
      HandlerRule(op, fparams.map(visitParam), visitExp(exp))
  }

  private def visitJvmMethod(method: JvmMethod): JvmMethod = method match {
    case JvmMethod(ident, fparams, clo, retTpe, purity, loc) =>
      JvmMethod(ident, fparams.map(visitParam), visitExp(clo), visitType(retTpe), purity, loc)
  }

  private def visitExp(exp: Expr): Expr = exp match {
    case Cst(cst, tpe, loc) =>
      Cst(cst, visitType(tpe), loc)
    case Var(sym, tpe, loc) =>
      Var(sym, visitType(tpe), loc)
    case ApplyAtomic(op, exps, tpe, purity, loc) =>
      val es = exps.map(visitExp)
      val t = visitType(tpe)
      val aa = ApplyAtomic(op, es, t, purity, loc)
      op match {
        case AtomicOp.Closure(sym) => aa
        case AtomicOp.Unary(sop) => aa
        case AtomicOp.Binary(sop) => aa
        case AtomicOp.Region => aa
        case AtomicOp.ScopeExit => aa
        case AtomicOp.Is(sym) => aa
        case AtomicOp.Tag(_) => aa
        case AtomicOp.Untag(_) =>
          Expr.ApplyAtomic(AtomicOp.Cast, List(aa), t, purity, loc.asSynthetic)
        case AtomicOp.Index(idx) => aa
        case AtomicOp.Tuple => aa
        case AtomicOp.RecordEmpty => aa
        case AtomicOp.RecordSelect(label) => aa
        case AtomicOp.RecordExtend(label) => aa
        case AtomicOp.RecordRestrict(label) => aa
        case AtomicOp.ArrayLit => aa
        case AtomicOp.ArrayNew => aa
        case AtomicOp.ArrayLoad => aa
        case AtomicOp.ArrayStore => aa
        case AtomicOp.ArrayLength => aa
        case AtomicOp.Ref => aa
        case AtomicOp.Deref => aa
        case AtomicOp.Assign => aa
        case AtomicOp.InstanceOf(clazz) => aa
        case AtomicOp.Cast => aa
        case AtomicOp.InvokeConstructor(constructor) => aa
        case AtomicOp.InvokeMethod(method) => aa
        case AtomicOp.InvokeStaticMethod(method) => aa
        case AtomicOp.GetField(field) => aa
        case AtomicOp.PutField(field) => aa
        case AtomicOp.GetStaticField(field) => aa
        case AtomicOp.PutStaticField(field) => aa
        case AtomicOp.Spawn => aa
        case AtomicOp.Lazy => aa
        case AtomicOp.Force => aa
        case AtomicOp.BoxBool => aa
        case AtomicOp.BoxInt8 => aa
        case AtomicOp.BoxInt16 => aa
        case AtomicOp.BoxInt32 => aa
        case AtomicOp.BoxInt64 => aa
        case AtomicOp.BoxChar => aa
        case AtomicOp.BoxFloat32 => aa
        case AtomicOp.BoxFloat64 => aa
        case AtomicOp.UnboxBool => aa
        case AtomicOp.UnboxInt8 => aa
        case AtomicOp.UnboxInt16 => aa
        case AtomicOp.UnboxInt32 => aa
        case AtomicOp.UnboxInt64 => aa
        case AtomicOp.UnboxChar => aa
        case AtomicOp.UnboxFloat32 => aa
        case AtomicOp.UnboxFloat64 => aa
        case AtomicOp.HoleError(sym) => aa
        case AtomicOp.MatchError => aa
      }

    case ApplyClo(exp, exps, ct, tpe, purity, loc) =>
      ApplyClo(visitExp(exp), exps.map(visitExp), ct, visitType(tpe), purity, loc)
    case ApplyDef(sym, exps, ct, tpe, purity, loc) =>
      ApplyDef(sym, exps.map(visitExp), ct, visitType(tpe), purity, loc)
    case ApplySelfTail(sym, formals, actuals, tpe, purity, loc) =>
      ApplySelfTail(sym, formals.map(visitParam), actuals.map(visitExp), visitType(tpe), purity, loc)
    case IfThenElse(exp1, exp2, exp3, tpe, purity, loc) =>
      IfThenElse(visitExp(exp1), visitExp(exp2), visitExp(exp3), visitType(tpe), purity, loc)
    case Branch(exp, branches, tpe, purity, loc) =>
      Branch(visitExp(exp), branches.map(visitBranch), visitType(tpe), purity, loc)
    case JumpTo(sym, tpe, purity, loc) =>
      JumpTo(sym, visitType(tpe), purity, loc)
    case Let(sym, exp1, exp2, tpe, purity, loc) =>
      Let(sym, visitExp(exp1), visitExp(exp2), visitType(tpe), purity, loc)
    case LetRec(varSym, index, defSym, exp1, exp2, tpe, purity, loc) =>
      LetRec(varSym, index, defSym, visitExp(exp1), visitExp(exp2), visitType(tpe), purity, loc)
    case Scope(sym, exp, tpe, purity, loc) =>
      Scope(sym, visitExp(exp), visitType(tpe), purity, loc)
    case TryCatch(exp, rules, tpe, purity, loc) =>
      TryCatch(visitExp(exp), rules.map(visitCatchRule), visitType(tpe), purity, loc)
    case TryWith(exp, effUse, rules, tpe, purity, loc) =>
      TryWith(visitExp(exp), effUse, rules.map(visitHandlerRule), visitType(tpe), purity, loc)
    case Do(op, exps, tpe, purity, loc) =>
      Do(op, exps.map(visitExp), visitType(tpe), purity, loc)
    case Resume(exp, tpe, loc) =>
      Resume(visitExp(exp), visitType(tpe), loc)
    case NewObject(name, clazz, tpe, purity, methods, loc) =>
      NewObject(name, clazz, visitType(tpe), purity, methods.map(visitJvmMethod), loc)
  }

  private def visitType(tpe: MonoType): MonoType = tpe match {
    case Unit =>
      Unit
    case Bool =>
      Bool
    case Char =>
      Char
    case Float32 =>
      Float32
    case Float64 =>
      Float64
    case BigDecimal =>
      BigDecimal
    case Int8 =>
      Int8
    case Int16 =>
      Int16
    case Int32 =>
      Int32
    case Int64 =>
      Int64
    case BigInt =>
      BigInt
    case String =>
      String
    case Regex =>
      Regex
    case Region =>
      Region
    case Array(tpe) =>
      Array(visitType(tpe))
    case Lazy(tpe) =>
      Lazy(visitType(tpe))
    case Ref(tpe) =>
      Ref(visitType(tpe))
    case Tuple(elms) =>
      Tuple(elms.map(visitType))
    case Enum(sym) =>
      Enum(sym)
    case Arrow(args, result) =>
      Arrow(args.map(visitType), visitType(result))
    case RecordEmpty =>
      RecordEmpty
    case RecordExtend(label, value, rest) =>
      RecordExtend(label, visitType(value), visitType(rest))
    case SchemaEmpty =>
      SchemaEmpty
    case SchemaExtend(name, tpe, rest) =>
      SchemaExtend(name, visitType(tpe), visitType(rest))
    case Native(clazz) =>
      Native(clazz)
  }

}

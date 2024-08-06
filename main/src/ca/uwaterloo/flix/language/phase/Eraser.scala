package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.ReducedAst.Expr._
import ca.uwaterloo.flix.language.ast.ReducedAst._
import ca.uwaterloo.flix.language.ast.{AtomicOp, MonoType, Purity, SourceLocation, Symbol}
import ca.uwaterloo.flix.language.dbg.AstPrinter.DebugReducedAst
import ca.uwaterloo.flix.util.ParOps

/**
  * Erase types and introduce corresponding casting
  *
  * Protocol is that casting should happen as soon as possible, not lazily.
  * This means that expressions should cast their output but assume correct
  * input types.
  *
  * - Ref
  *   - component type erasure
  *   - deref casting
  * - Tuple
  *   - component type erasure
  *   - index casting
  * - Record
  *   - component type erasure
  *   - select casting
  * - Lazy
  *   - component type erasure
  *   - force casting
  * - Function
  *   - result type boxing, this includes return types of defs and their applications
  *   - function call return value casting
  */
object Eraser {

  def run(root: Root)(implicit flix: Flix): Root = flix.phase("Eraser") {
    val newDefs = ParOps.parMapValues(root.defs)(visitDef)
    val newEffects = ParOps.parMapValues(root.effects)(visitEffect)
    root.copy(defs = newDefs, effects = newEffects)
  }

  private def visitDef(defn: Def): Def = defn match {
    case Def(ann, mod, sym, cparams, fparams, lparams, pcPoints, exp, tpe, originalTpe, purity, loc) =>
      val eNew = visitExp(exp)
      val e = Expr.ApplyAtomic(AtomicOp.Box, List(eNew), box(tpe), purity, loc)
      Def(ann, mod, sym, cparams.map(visitParam), fparams.map(visitParam), lparams.map(visitLocalParam), pcPoints, e, box(tpe), UnboxedType(erase(originalTpe.tpe)), purity, loc)
  }

  private def visitParam(fp: FormalParam): FormalParam = fp match {
    case FormalParam(sym, mod, tpe, loc) =>
      FormalParam(sym, mod, visitType(tpe), loc)
  }

  private def visitLocalParam(p: LocalParam): LocalParam = p match {
    case LocalParam(sym, tpe) =>
      LocalParam(sym, visitType(tpe))
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
      // return type is not erased to maintain class signatures
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
      op match {
        case AtomicOp.Closure(_) => ApplyAtomic(op, es, t, purity, loc)
        case AtomicOp.Unary(_) => ApplyAtomic(op, es, t, purity, loc)
        case AtomicOp.Binary(_) => ApplyAtomic(op, es, t, purity, loc)
        case AtomicOp.Region => ApplyAtomic(op, es, t, purity, loc)
        case AtomicOp.Is(_) => ApplyAtomic(op, es, t, purity, loc)
        case AtomicOp.Tag(_) => ApplyAtomic(op, es, t, purity, loc)
        case AtomicOp.Untag(_) => ApplyAtomic(op, es, t, purity, loc)
        case AtomicOp.Index(_) =>
          castExp(ApplyAtomic(op, es, erase(tpe), purity, loc), t, purity, loc)
        case AtomicOp.Tuple => ApplyAtomic(op, es, t, purity, loc)
        case AtomicOp.RecordEmpty => ApplyAtomic(op, es, t, purity, loc)
        case AtomicOp.RecordSelect(_) =>
          castExp(ApplyAtomic(op, es, erase(tpe), purity, loc), t, purity, loc)
        case AtomicOp.RecordExtend(_) => ApplyAtomic(op, es, t, purity, loc)
        case AtomicOp.RecordRestrict(_) => ApplyAtomic(op, es, t, purity, loc)
        case AtomicOp.ArrayLit => ApplyAtomic(op, es, t, purity, loc)
        case AtomicOp.ArrayNew => ApplyAtomic(op, es, t, purity, loc)
        case AtomicOp.ArrayLoad => ApplyAtomic(op, es, t, purity, loc)
        case AtomicOp.ArrayStore => ApplyAtomic(op, es, t, purity, loc)
        case AtomicOp.ArrayLength => ApplyAtomic(op, es, t, purity, loc)
        case AtomicOp.StructNew(_, _) => throw new RuntimeException("JOE TBD")
        case AtomicOp.StructGet(_, _) => throw new RuntimeException("JOE TBD")
        case AtomicOp.StructPut(_, _) => throw new RuntimeException("JOE TBD")
        case AtomicOp.Ref => ApplyAtomic(op, es, t, purity, loc)
        case AtomicOp.Deref =>
          castExp(ApplyAtomic(op, es, erase(tpe), purity, loc), t, purity, loc)
        case AtomicOp.Assign => ApplyAtomic(op, es, t, purity, loc)
        case AtomicOp.InstanceOf(_) => ApplyAtomic(op, es, t, purity, loc)
        case AtomicOp.Cast => ApplyAtomic(op, es, t, purity, loc)
        case AtomicOp.Unbox => ApplyAtomic(op, es, t, purity, loc)
        case AtomicOp.Box => ApplyAtomic(op, es, t, purity, loc)
        case AtomicOp.InvokeConstructor(_) => ApplyAtomic(op, es, t, purity, loc)
        case AtomicOp.InvokeMethod(_) => ApplyAtomic(op, es, t, purity, loc)
        case AtomicOp.InvokeStaticMethod(_) => ApplyAtomic(op, es, t, purity, loc)
        case AtomicOp.GetField(_) => ApplyAtomic(op, es, t, purity, loc)
        case AtomicOp.PutField(_) => ApplyAtomic(op, es, t, purity, loc)
        case AtomicOp.GetStaticField(_) => ApplyAtomic(op, es, t, purity, loc)
        case AtomicOp.PutStaticField(_) => ApplyAtomic(op, es, t, purity, loc)
        case AtomicOp.Throw => throw new RuntimeException("JOE TBD")
        case AtomicOp.Spawn => ApplyAtomic(op, es, t, purity, loc)
        case AtomicOp.Lazy => ApplyAtomic(op, es, t, purity, loc)
        case AtomicOp.Force =>
          castExp(ApplyAtomic(op, es, erase(tpe), purity, loc), t, purity, loc)
        case AtomicOp.HoleError(_) => ApplyAtomic(op, es, t, purity, loc)
        case AtomicOp.MatchError => ApplyAtomic(op, es, t, purity, loc)
      }

    case ApplyClo(exp, exps, ct, tpe, purity, loc) =>
      val ac = ApplyClo(visitExp(exp), exps.map(visitExp), ct, box(tpe), purity, loc)
      castExp(unboxExp(ac, erase(tpe), purity, loc), visitType(tpe), purity, loc)
    case ApplyDef(sym, exps, ct, tpe, purity, loc) =>
      val ad = ApplyDef(sym, exps.map(visitExp), ct, box(tpe), purity, loc)
      castExp(unboxExp(ad, erase(tpe), purity, loc), visitType(tpe), purity, loc)
    case ApplySelfTail(sym, actuals, tpe, purity, loc) =>
      ApplySelfTail(sym, actuals.map(visitExp), visitType(tpe), purity, loc)
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
    case Stmt(exp1, exp2, tpe, purity, loc) =>
      Stmt(visitExp(exp1), visitExp(exp2), visitType(tpe), purity, loc)
    case Scope(sym, exp, tpe, purity, loc) =>
      Scope(sym, visitExp(exp), visitType(tpe), purity, loc)
    case TryCatch(exp, rules, tpe, purity, loc) =>
      TryCatch(visitExp(exp), rules.map(visitCatchRule), visitType(tpe), purity, loc)
    case TryWith(exp, effUse, rules, ct, tpe, purity, loc) =>
      val tw = TryWith(visitExp(exp), effUse, rules.map(visitHandlerRule), ct, box(tpe), purity, loc)
      castExp(unboxExp(tw, erase(tpe), purity, loc), visitType(tpe), purity, loc)
    case Do(op, exps, tpe, purity, loc) =>
      Do(op, exps.map(visitExp), visitType(tpe), purity, loc)
    case NewObject(name, clazz, tpe, purity, methods, loc) =>
      NewObject(name, clazz, visitType(tpe), purity, methods.map(visitJvmMethod), loc)
  }

  private def castExp(exp: Expr, t: MonoType, purity: Purity, loc: SourceLocation): Expr = {
    Expr.ApplyAtomic(AtomicOp.Cast, List(exp), t, purity, loc.asSynthetic)
  }

  private def unboxExp(exp: Expr, t: MonoType, purity: Purity, loc: SourceLocation): Expr = {
    Expr.ApplyAtomic(AtomicOp.Unbox, List(exp), t, purity, loc.asSynthetic)
  }

  private def visitEffect(eff: Effect): Effect = eff match {
    case Effect(ann, mod, sym, ops, loc) =>
      Effect(ann, mod, sym, ops.map(visitOp), loc)
  }

  private def visitOp(op: Op): Op = op match {
    case Op(sym, ann, mod, fparams, tpe, purity, loc) =>
      Op(sym, ann, mod, fparams.map(visitParam), erase(tpe), purity, loc)
  }

  private def visitType(tpe: MonoType): MonoType = {
    import MonoType._
    tpe match {
      case Void => Void
      case AnyType => AnyType
      case Unit => Unit
      case Bool => Bool
      case Char => Char
      case Float32 => Float32
      case Float64 => Float64
      case BigDecimal => BigDecimal
      case Int8 => Int8
      case Int16 => Int16
      case Int32 => Int32
      case Int64 => Int64
      case BigInt => BigInt
      case String => String
      case Regex => Regex
      case Region => Region
      case Null => Null
      case Array(tpe) => Array(visitType(tpe))
      case Lazy(tpe) => Lazy(erase(tpe))
      case Ref(tpe) => Ref(erase(tpe))
      case Tuple(elms) => Tuple(elms.map(erase))
      case MonoType.Enum(sym) => MonoType.Enum(sym)
      case MonoType.Struct(sym, elmTpes, targs) => MonoType.Struct(sym, elmTpes, targs)
      case Arrow(args, result) => Arrow(args.map(visitType), box(result))
      case RecordEmpty => RecordEmpty
      case RecordExtend(label, value, rest) => RecordExtend(label, erase(value), visitType(rest))
      case Native(clazz) => Native(clazz)
    }
  }

  private def erase(tpe: MonoType): MonoType = {
    import MonoType._
    tpe match {
      case Bool => Bool
      case Char => Char
      case Float32 => Float32
      case Float64 => Float64
      case Int8 => Int8
      case Int16 => Int16
      case Int32 => Int32
      case Int64 => Int64
      case Void | AnyType | Unit | BigDecimal | BigInt | String | Regex |
           Region | Array(_) | Lazy(_) | Ref(_) | Tuple(_) | MonoType.Enum(_) |
           MonoType.Struct(_, _, _) | Arrow(_, _) | RecordEmpty | RecordExtend(_, _, _) | Native(_) | Null =>
        MonoType.Object
    }
  }

  private def box(tpe: MonoType): MonoType = MonoType.Object

}

package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.SimpleType.erase
import ca.uwaterloo.flix.language.ast.{AtomicOp, Purity, ReducedAst, SimpleType, SourceLocation, Symbol, Type, TypeConstructor}
import ca.uwaterloo.flix.language.dbg.AstPrinter.DebugReducedAst
import ca.uwaterloo.flix.util.collection.MapOps
import ca.uwaterloo.flix.util.{InternalCompilerException, ParOps}

import scala.annotation.unused

/**
  * Erase types and introduce corresponding casting
  *
  * Protocol is that casting should happen as soon as possible, not lazily.
  * This means that expressions should cast their output but assume correct
  * input types.
  *
  *   - Ref
  *     - component type erasure
  *   - Tuple
  *     - component type erasure
  *     - index casting
  *   - Record
  *     - component type erasure
  *     - select casting
  *   - Lazy
  *     - component type erasure
  *     - force casting
  *   - Function
  *     - result type boxing, this includes return types of defs and their applications
  *     - function call return value casting
  *   - Enums and Structs
  *     - type arguments are erased
  *     - polymorphic term types in the declaration are polymorphically erased (see [[polymorphicErasure]])
  */
object Eraser {

  def run(root: ReducedAst.Root)(implicit flix: Flix): ReducedAst.Root = flix.phase("Eraser") {
    val newDefs = ParOps.parMapValues(root.defs)(visitDef)
    val newEnums = ParOps.parMapValues(root.enums)(visitEnum)
    val newStructs = ParOps.parMapValues(root.structs)(visitStruct)
    val newEffects = ParOps.parMapValues(root.effects)(visitEffect)
    root.copy(defs = newDefs, enums = newEnums, structs = newStructs, effects = newEffects)
  }

  private def visitDef(defn: ReducedAst.Def): ReducedAst.Def = defn match {
    case ReducedAst.Def(ann, mod, sym, cparams, fparams, exp, tpe, originalTpe, loc) =>
      val eNew = visitExp(exp)
      val e = ReducedAst.Expr.ApplyAtomic(AtomicOp.Box, List(eNew), box(tpe), exp.purity, loc)
      ReducedAst.Def(ann, mod, sym, cparams.map(visitParam), fparams.map(visitParam), e, box(tpe), ReducedAst.UnboxedType(erase(originalTpe.tpe)), loc)
  }

  private def visitParam(fp: ReducedAst.FormalParam): ReducedAst.FormalParam = fp match {
    case ReducedAst.FormalParam(sym, tpe) =>
      ReducedAst.FormalParam(sym, visitType(tpe))
  }

  private def visitEnum(enm: ReducedAst.Enum): ReducedAst.Enum = enm match {
    case ReducedAst.Enum(ann, mod, sym, tparams, cases0, loc) =>
      val cases = MapOps.mapValues(cases0)(visitEnumTag)
      ReducedAst.Enum(ann, mod, sym, tparams, cases, loc)
  }

  private def visitEnumTag(caze: ReducedAst.Case): ReducedAst.Case = caze match {
    case ReducedAst.Case(sym, tpes, loc) =>
      ReducedAst.Case(sym, tpes.map(polymorphicErasure), loc)
  }

  private def visitStruct(struct: ReducedAst.Struct): ReducedAst.Struct = struct match {
    case ReducedAst.Struct(ann, mod, sym, tparams, fields0, loc) =>
      val fields = fields0.map(visitStructField)
      ReducedAst.Struct(ann, mod, sym, tparams, fields, loc)
  }

  private def visitStructField(field: ReducedAst.StructField): ReducedAst.StructField = field match {
    case ReducedAst.StructField(sym, tpe, loc) =>
      ReducedAst.StructField(sym, polymorphicErasure(tpe), loc)
  }

  private def visitBranch(branch: (Symbol.LabelSym, ReducedAst.Expr)): (Symbol.LabelSym, ReducedAst.Expr) = branch match {
    case (sym, exp) =>
      (sym, visitExp(exp))
  }

  private def visitCatchRule(rule: ReducedAst.CatchRule): ReducedAst.CatchRule = rule match {
    case ReducedAst.CatchRule(sym, clazz, exp) =>
      ReducedAst.CatchRule(sym, clazz, visitExp(exp))
  }

  private def visitHandlerRule(rule: ReducedAst.HandlerRule): ReducedAst.HandlerRule = rule match {
    case ReducedAst.HandlerRule(op, fparams, exp) =>
      ReducedAst.HandlerRule(op, fparams.map(visitParam), visitExp(exp))
  }

  private def visitJvmMethod(method: ReducedAst.JvmMethod): ReducedAst.JvmMethod = method match {
    case ReducedAst.JvmMethod(ident, fparams, clo, retTpe, purity, loc) =>
      // return type is not erased to maintain class signatures
      ReducedAst.JvmMethod(ident, fparams.map(visitParam), visitExp(clo), visitType(retTpe), purity, loc)
  }

  private def visitExp(exp0: ReducedAst.Expr): ReducedAst.Expr = exp0 match {
    case ReducedAst.Expr.Cst(cst, loc) =>
      ReducedAst.Expr.Cst(cst, loc)
    case ReducedAst.Expr.Var(sym, tpe, loc) =>
      ReducedAst.Expr.Var(sym, visitType(tpe), loc)
    case ReducedAst.Expr.ApplyAtomic(op, exps, tpe, purity, loc) =>
      val es = exps.map(visitExp)
      val t = visitType(tpe)
      op match {
        case AtomicOp.Closure(_) => ReducedAst.Expr.ApplyAtomic(op, es, t, purity, loc)
        case AtomicOp.Unary(_) => ReducedAst.Expr.ApplyAtomic(op, es, t, purity, loc)
        case AtomicOp.Binary(_) => ReducedAst.Expr.ApplyAtomic(op, es, t, purity, loc)
        case AtomicOp.Is(_) => ReducedAst.Expr.ApplyAtomic(op, es, t, purity, loc)
        case AtomicOp.Tag(_) => ReducedAst.Expr.ApplyAtomic(op, es, t, purity, loc)
        case AtomicOp.Untag(_, _) =>
          castExp(ReducedAst.Expr.ApplyAtomic(op, es, erase(tpe), purity, loc), t, purity, loc)
        case AtomicOp.Index(_) =>
          castExp(ReducedAst.Expr.ApplyAtomic(op, es, erase(tpe), purity, loc), t, purity, loc)
        case AtomicOp.Tuple => ReducedAst.Expr.ApplyAtomic(op, es, t, purity, loc)
        case AtomicOp.RecordSelect(_) =>
          castExp(ReducedAst.Expr.ApplyAtomic(op, es, erase(tpe), purity, loc), t, purity, loc)
        case AtomicOp.RecordExtend(_) => ReducedAst.Expr.ApplyAtomic(op, es, t, purity, loc)
        case AtomicOp.RecordRestrict(_) => ReducedAst.Expr.ApplyAtomic(op, es, t, purity, loc)
        case AtomicOp.ExtIs(_) => ReducedAst.Expr.ApplyAtomic(op, es, t, purity, loc)
        case AtomicOp.ExtTag(_) => ReducedAst.Expr.ApplyAtomic(op, es, t, purity, loc)
        case AtomicOp.ExtUntag(_, _) => ReducedAst.Expr.ApplyAtomic(op, es, t, purity, loc)
        case AtomicOp.ArrayLit => ReducedAst.Expr.ApplyAtomic(op, es, t, purity, loc)
        case AtomicOp.ArrayNew => ReducedAst.Expr.ApplyAtomic(op, es, t, purity, loc)
        case AtomicOp.ArrayLoad => ReducedAst.Expr.ApplyAtomic(op, es, t, purity, loc)
        case AtomicOp.ArrayStore => ReducedAst.Expr.ApplyAtomic(op, es, t, purity, loc)
        case AtomicOp.ArrayLength => ReducedAst.Expr.ApplyAtomic(op, es, t, purity, loc)
        case AtomicOp.StructNew(_, _) => ReducedAst.Expr.ApplyAtomic(op, es, t, purity, loc)
        case AtomicOp.StructGet(_) => castExp(ReducedAst.Expr.ApplyAtomic(op, es, erase(tpe), purity, loc), t, purity, loc)
        case AtomicOp.StructPut(_) => ReducedAst.Expr.ApplyAtomic(op, es, t, purity, loc)
        case AtomicOp.InstanceOf(_) => ReducedAst.Expr.ApplyAtomic(op, es, t, purity, loc)
        case AtomicOp.Cast => ReducedAst.Expr.ApplyAtomic(op, es, t, purity, loc)
        case AtomicOp.Unbox => ReducedAst.Expr.ApplyAtomic(op, es, t, purity, loc)
        case AtomicOp.Box => ReducedAst.Expr.ApplyAtomic(op, es, t, purity, loc)
        case AtomicOp.InvokeConstructor(_) => ReducedAst.Expr.ApplyAtomic(op, es, t, purity, loc)
        case AtomicOp.InvokeMethod(_) => ReducedAst.Expr.ApplyAtomic(op, es, t, purity, loc)
        case AtomicOp.InvokeStaticMethod(_) => ReducedAst.Expr.ApplyAtomic(op, es, t, purity, loc)
        case AtomicOp.GetField(_) => ReducedAst.Expr.ApplyAtomic(op, es, t, purity, loc)
        case AtomicOp.PutField(_) => ReducedAst.Expr.ApplyAtomic(op, es, t, purity, loc)
        case AtomicOp.GetStaticField(_) => ReducedAst.Expr.ApplyAtomic(op, es, t, purity, loc)
        case AtomicOp.PutStaticField(_) => ReducedAst.Expr.ApplyAtomic(op, es, t, purity, loc)
        case AtomicOp.Throw => ReducedAst.Expr.ApplyAtomic(op, es, t, purity, loc)
        case AtomicOp.Spawn => ReducedAst.Expr.ApplyAtomic(op, es, t, purity, loc)
        case AtomicOp.Lazy => ReducedAst.Expr.ApplyAtomic(op, es, t, purity, loc)
        case AtomicOp.Force =>
          castExp(ReducedAst.Expr.ApplyAtomic(op, es, erase(tpe), purity, loc), t, purity, loc)
        case AtomicOp.HoleError(_) => ReducedAst.Expr.ApplyAtomic(op, es, t, purity, loc)
        case AtomicOp.MatchError => ReducedAst.Expr.ApplyAtomic(op, es, t, purity, loc)
        case AtomicOp.CastError(_, _) => ReducedAst.Expr.ApplyAtomic(op, es, t, purity, loc)
      }

    case ReducedAst.Expr.ApplyClo(exp1, exp2, ct, tpe, purity, loc) =>
      val ac = ReducedAst.Expr.ApplyClo(visitExp(exp1), visitExp(exp2), ct, box(tpe), purity, loc)
      castExp(unboxExp(ac, erase(tpe), purity, loc), visitType(tpe), purity, loc)
    case ReducedAst.Expr.ApplyDef(sym, exps, ct, tpe, purity, loc) =>
      val ad = ReducedAst.Expr.ApplyDef(sym, exps.map(visitExp), ct, box(tpe), purity, loc)
      castExp(unboxExp(ad, erase(tpe), purity, loc), visitType(tpe), purity, loc)
    case ReducedAst.Expr.ApplyOp(sym, exps, tpe, purity, loc) =>
      ReducedAst.Expr.ApplyOp(sym, exps.map(visitExp), visitType(tpe), purity, loc)
    case ReducedAst.Expr.ApplySelfTail(sym, actuals, tpe, purity, loc) =>
      ReducedAst.Expr.ApplySelfTail(sym, actuals.map(visitExp), visitType(tpe), purity, loc)
    case ReducedAst.Expr.IfThenElse(exp1, exp2, exp3, tpe, purity, loc) =>
      ReducedAst.Expr.IfThenElse(visitExp(exp1), visitExp(exp2), visitExp(exp3), visitType(tpe), purity, loc)
    case ReducedAst.Expr.Branch(exp, branches, tpe, purity, loc) =>
      ReducedAst.Expr.Branch(visitExp(exp), branches.map(visitBranch), visitType(tpe), purity, loc)
    case ReducedAst.Expr.JumpTo(sym, tpe, purity, loc) =>
      ReducedAst.Expr.JumpTo(sym, visitType(tpe), purity, loc)
    case ReducedAst.Expr.Let(sym, exp1, exp2, loc) =>
      ReducedAst.Expr.Let(sym, visitExp(exp1), visitExp(exp2), loc)
    case ReducedAst.Expr.Stmt(exp1, exp2, loc) =>
      ReducedAst.Expr.Stmt(visitExp(exp1), visitExp(exp2), loc)
    case ReducedAst.Expr.Region(sym, exp, tpe, purity, loc) =>
      ReducedAst.Expr.Region(sym, visitExp(exp), visitType(tpe), purity, loc)
    case ReducedAst.Expr.TryCatch(exp, rules, tpe, purity, loc) =>
      ReducedAst.Expr.TryCatch(visitExp(exp), rules.map(visitCatchRule), visitType(tpe), purity, loc)
    case ReducedAst.Expr.RunWith(exp, effUse, rules, ct, tpe, purity, loc) =>
      val tw = ReducedAst.Expr.RunWith(visitExp(exp), effUse, rules.map(visitHandlerRule), ct, box(tpe), purity, loc)
      castExp(unboxExp(tw, erase(tpe), purity, loc), visitType(tpe), purity, loc)
    case ReducedAst.Expr.NewObject(name, clazz, tpe, purity, methods, loc) =>
      ReducedAst.Expr.NewObject(name, clazz, visitType(tpe), purity, methods.map(visitJvmMethod), loc)
  }

  private def castExp(exp: ReducedAst.Expr, t: SimpleType, purity: Purity, loc: SourceLocation): ReducedAst.Expr = {
    ReducedAst.Expr.ApplyAtomic(AtomicOp.Cast, List(exp), t, purity, loc.asSynthetic)
  }

  private def unboxExp(exp: ReducedAst.Expr, t: SimpleType, purity: Purity, loc: SourceLocation): ReducedAst.Expr = {
    ReducedAst.Expr.ApplyAtomic(AtomicOp.Unbox, List(exp), t, purity, loc.asSynthetic)
  }

  private def visitEffect(eff: ReducedAst.Effect): ReducedAst.Effect = eff match {
    case ReducedAst.Effect(ann, mod, sym, ops, loc) =>
      ReducedAst.Effect(ann, mod, sym, ops.map(visitOp), loc)
  }

  private def visitOp(op: ReducedAst.Op): ReducedAst.Op = op match {
    case ReducedAst.Op(sym, ann, mod, fparams, tpe, purity, loc) =>
      ReducedAst.Op(sym, ann, mod, fparams.map(visitParam), erase(tpe), purity, loc)
  }

  private def visitType(tpe0: SimpleType): SimpleType = {
    import SimpleType.*
    tpe0 match {
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
      case Array(tpe) => SimpleType.mkArray(visitType(tpe))
      case Lazy(tpe) => Lazy(erase(tpe))
      case Tuple(elms) => SimpleType.mkTuple(elms.map(erase))
      case SimpleType.Enum(sym, targs) => SimpleType.mkEnum(sym, targs.map(erase))
      case SimpleType.Struct(sym, tparams) => SimpleType.Struct(sym, tparams.map(erase))
      case Arrow(args, result) => SimpleType.mkArrow(args.map(visitType), box(result))
      case RecordEmpty => RecordEmpty
      case RecordExtend(label, value, rest) => RecordExtend(label, erase(value), visitType(rest))
      case ExtensibleExtend(cons, tpes, rest) => ExtensibleExtend(cons, tpes.map(erase), visitType(rest))
      case ExtensibleEmpty => ExtensibleEmpty
      case Native(clazz) => Native(clazz)
    }
  }

  /**
    * Erases the polymorphic `tpe`. The returned type is either [[Type.Var]], [[Type.Cst]] of a
    * primitive type, or [[Type.Cst]] of `java.lang.Object`.
    *
    *   - `polymorphicErasure(a) = a`
    *   - `polymorphicErasure(Int32) = Int32`
    *   - `polymorphicErasure(String) = Object`
    *   - `polymorphicErasure(Option[a]) = Object`
    *   - `polymorphicErasure(a[Int32]) = Object`
    *
    * We do not have aliases, associated types, and the like, so any [[Type.Apply]] will be
    * *building* a larger type, and can therefore not be a primitive type.
    */
  private def polymorphicErasure(tpe: Type): Type = tpe match {
    case v@Type.Var(_, _) => v
    case c@Type.Cst(tc, loc) => tc match {
      case TypeConstructor.Bool => c
      case TypeConstructor.Char => c
      case TypeConstructor.Float32 => c
      case TypeConstructor.Float64 => c
      case TypeConstructor.Int8 => c
      case TypeConstructor.Int16 => c
      case TypeConstructor.Int32 => c
      case TypeConstructor.Int64 => c
      // All primitive types are covered, so the rest can only be erased to Object.
      case _ => Type.Cst(TypeConstructor.Native(classOf[Object]), loc)
    }
    case Type.Apply(_, _, loc) => Type.Cst(TypeConstructor.Native(classOf[Object]), loc)

    case Type.Alias(_, _, _, _) => throw InternalCompilerException(s"Unexpected type $tpe", tpe.loc)
    case Type.AssocType(_, _, _, _) => throw InternalCompilerException(s"Unexpected type $tpe", tpe.loc)
    case Type.JvmToType(_, _) => throw InternalCompilerException(s"Unexpected type $tpe", tpe.loc)
    case Type.JvmToEff(_, _) => throw InternalCompilerException(s"Unexpected type $tpe", tpe.loc)
    case Type.UnresolvedJvmType(_, _) => throw InternalCompilerException(s"Unexpected type $tpe", tpe.loc)
  }

  private def box(@unused tpe: SimpleType): SimpleType = SimpleType.Object

}

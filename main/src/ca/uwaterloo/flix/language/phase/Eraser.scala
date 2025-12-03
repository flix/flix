package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.SimpleType.erase
import ca.uwaterloo.flix.language.ast.{AtomicOp, ErasedAst, Purity, ReducedAst, SimpleType, SourceLocation, Symbol, Type, TypeConstructor}
import ca.uwaterloo.flix.language.dbg.AstPrinter.DebugNoOp
import ca.uwaterloo.flix.util.collection.ListOps
import ca.uwaterloo.flix.util.{InternalCompilerException, ParOps}

import java.util.concurrent.ConcurrentHashMap
import java.util.function.BiConsumer
import scala.annotation.unused
import scala.collection.mutable

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
  *     - declaration specialization, based on the actual uses of each type (see [[SharedContext]], [[specializeEnums]], and [[specializeStructs]]).
  *     - component type erasure and untag/access casting
  */
object Eraser {

  def run(root: ReducedAst.Root)(implicit flix: Flix): ErasedAst.Root = flix.phase("Eraser") {
    implicit val r: ReducedAst.Root = root
    implicit val ctx: SharedContext = new SharedContext()
    val newDefs = ParOps.parMapValues(root.defs)(visitDef)
    val newEffects = ParOps.parMapValues(root.effects)(visitEffect)
    // Specializations must happen after all other types and expressions are visited.
    val newEnums = specializeEnums(ctx.getEnumSpecializations)
    val newStructs = specializeStructs(ctx.getStructSpecializations)
    ErasedAst.Root(newDefs, newEnums, newStructs, newEffects, root.mainEntryPoint, root.entryPoints, root.sources)
  }(DebugNoOp())

  private def visitDef(defn: ReducedAst.Def)(implicit ctx: SharedContext, flix: Flix): ErasedAst.Def = defn match {
    case ReducedAst.Def(ann, mod, sym, cparams, fparams, exp, tpe, originalTpe, loc) =>
      val eNew = visitExp(exp)
      val e = ErasedAst.Expr.ApplyAtomic(AtomicOp.Box, List(eNew), box(tpe), exp.purity, loc)
      ErasedAst.Def(ann, mod, sym, cparams.map(visitParam), fparams.map(visitParam), e, box(tpe), ErasedAst.UnboxedType(erase(originalTpe.tpe)), loc)
  }

  private def specializeEnums(specializations: List[(Symbol.EnumSym, List[SimpleType], Symbol.EnumSym)])(implicit root: ReducedAst.Root, flix: Flix): Map[Symbol.EnumSym, ErasedAst.Enum] = {
    ParOps.parMap(specializations) {
      case (sym, targs, newSym) =>
        val enm0 = root.enums(sym)
        val subst = ListOps.zip(enm0.tparams.map(_.sym), targs).toMap
        val cases = specializeCases(enm0.cases, newSym, subst)
        val enm = ErasedAst.Enum(enm0.ann, enm0.mod, newSym, cases, enm0.loc)
        enm.sym -> enm
    }.toMap
  }

  private def specializeCases(cases: Map[Symbol.CaseSym, ReducedAst.Case], newSym: Symbol.EnumSym, subst: Map[Symbol.KindedTypeVarSym, SimpleType]): Map[Symbol.CaseSym, ErasedAst.Case] = {
    cases.values.map {
      case caze0 =>
        val caze = specializeCase(caze0, newSym, subst)
        caze.sym -> caze
    }.toMap
  }

  private def specializeCase(caze: ReducedAst.Case, newSym: Symbol.EnumSym, subst: Map[Symbol.KindedTypeVarSym, SimpleType]): ErasedAst.Case = {
    val sym = new Symbol.CaseSym(newSym, caze.sym.name, caze.sym.loc)
    ErasedAst.Case(sym, caze.tpes.map(instantiateAndEraseType(subst, _)), caze.loc)
  }

  private def specializeStructs(specializations: List[(Symbol.StructSym, List[SimpleType], Symbol.StructSym)])(implicit root: ReducedAst.Root, flix: Flix): Map[Symbol.StructSym, ErasedAst.Struct] = {
    ParOps.parMap(specializations) {
      case (sym, targs, newSym) =>
        val struct = root.structs(sym)
        val subst = ListOps.zip(struct.tparams.map(_.sym), targs).toMap
        val fields = struct.fields.map(specializeStructField(_, newSym, subst))
        ErasedAst.Struct(struct.ann, struct.mod, newSym, fields, struct.loc)
    }.map(enm => enm.sym -> enm).toMap
  }

  private def specializeStructField(field: ReducedAst.StructField, newSym: Symbol.StructSym, subst: Map[Symbol.KindedTypeVarSym, SimpleType]): ErasedAst.StructField = {
    val sym = new Symbol.StructFieldSym(newSym, field.sym.name, field.sym.loc)
    ErasedAst.StructField(sym, instantiateAndEraseType(subst, field.tpe), field.loc)
  }

  private def visitParam(fp: ReducedAst.FormalParam)(implicit ctx: SharedContext, flix: Flix): ErasedAst.FormalParam = fp match {
    case ReducedAst.FormalParam(sym, tpe) =>
      ErasedAst.FormalParam(sym, visitType(tpe))
  }

  private def visitBranch(branch: (Symbol.LabelSym, ReducedAst.Expr))(implicit ctx: SharedContext, flix: Flix): (Symbol.LabelSym, ErasedAst.Expr) = branch match {
    case (sym, exp) =>
      (sym, visitExp(exp))
  }

  private def visitCatchRule(rule: ReducedAst.CatchRule)(implicit ctx: SharedContext, flix: Flix): ErasedAst.CatchRule = rule match {
    case ReducedAst.CatchRule(sym, clazz, exp) =>
      ErasedAst.CatchRule(sym, clazz, visitExp(exp))
  }

  private def visitHandlerRule(rule: ReducedAst.HandlerRule)(implicit ctx: SharedContext, flix: Flix): ErasedAst.HandlerRule = rule match {
    case ReducedAst.HandlerRule(op, fparams, exp) =>
      ErasedAst.HandlerRule(op, fparams.map(visitParam), visitExp(exp))
  }

  private def visitJvmMethod(method: ReducedAst.JvmMethod)(implicit ctx: SharedContext, flix: Flix): ErasedAst.JvmMethod = method match {
    case ReducedAst.JvmMethod(ident, fparams, clo, retTpe, purity, loc) =>
      // return type is not erased to maintain class signatures
      ErasedAst.JvmMethod(ident, fparams.map(visitParam), visitExp(clo), visitType(retTpe), purity, loc)
  }

  private def visitExp(exp0: ReducedAst.Expr)(implicit ctx: SharedContext, flix: Flix): ErasedAst.Expr = exp0 match {
    case ReducedAst.Expr.Cst(cst, loc) =>
      ErasedAst.Expr.Cst(cst, loc)
    case ReducedAst.Expr.Var(sym, tpe, loc) =>
      ErasedAst.Expr.Var(sym, visitType(tpe), loc)
    case ReducedAst.Expr.ApplyAtomic(op, exps, tpe, purity, loc) =>
      val es = exps.map(visitExp)
      val t = visitType(tpe)
      op match {
        case AtomicOp.Closure(_) => ErasedAst.Expr.ApplyAtomic(op, es, t, purity, loc)
        case AtomicOp.Unary(_) => ErasedAst.Expr.ApplyAtomic(op, es, t, purity, loc)
        case AtomicOp.Binary(_) => ErasedAst.Expr.ApplyAtomic(op, es, t, purity, loc)
        case AtomicOp.Is(sym) =>
          val List(e) = es
          val specializedEnum = e.tpe.asInstanceOf[SimpleType.Enum]
          val specializedSym = specializedCaseSym(sym, specializedEnum)
          ErasedAst.Expr.ApplyAtomic(AtomicOp.Is(specializedSym), es, t, purity, loc)
        case AtomicOp.Tag(sym) =>
          val specializedEnum = t.asInstanceOf[SimpleType.Enum]
          val specializedSym = specializedCaseSym(sym, specializedEnum)
          ErasedAst.Expr.ApplyAtomic(AtomicOp.Tag(specializedSym), es, t, purity, loc)
        case AtomicOp.Untag(sym, idx) =>
          val List(e) = es
          val specializedEnum = e.tpe.asInstanceOf[SimpleType.Enum]
          val specializedSym = specializedCaseSym(sym, specializedEnum)
          castExp(ErasedAst.Expr.ApplyAtomic(AtomicOp.Untag(specializedSym, idx), es, erase(tpe), purity, loc), t, purity, loc)
        case AtomicOp.Index(_) =>
          castExp(ErasedAst.Expr.ApplyAtomic(op, es, erase(tpe), purity, loc), t, purity, loc)
        case AtomicOp.Tuple => ErasedAst.Expr.ApplyAtomic(op, es, t, purity, loc)
        case AtomicOp.RecordSelect(_) =>
          castExp(ErasedAst.Expr.ApplyAtomic(op, es, erase(tpe), purity, loc), t, purity, loc)
        case AtomicOp.RecordExtend(_) => ErasedAst.Expr.ApplyAtomic(op, es, t, purity, loc)
        case AtomicOp.RecordRestrict(_) => ErasedAst.Expr.ApplyAtomic(op, es, t, purity, loc)
        case AtomicOp.ExtIs(_) => ErasedAst.Expr.ApplyAtomic(op, es, t, purity, loc)
        case AtomicOp.ExtTag(_) => ErasedAst.Expr.ApplyAtomic(op, es, t, purity, loc)
        case AtomicOp.ExtUntag(_, _) => ErasedAst.Expr.ApplyAtomic(op, es, t, purity, loc)
        case AtomicOp.ArrayLit => ErasedAst.Expr.ApplyAtomic(op, es, t, purity, loc)
        case AtomicOp.ArrayNew => ErasedAst.Expr.ApplyAtomic(op, es, t, purity, loc)
        case AtomicOp.ArrayLoad => ErasedAst.Expr.ApplyAtomic(op, es, t, purity, loc)
        case AtomicOp.ArrayStore => ErasedAst.Expr.ApplyAtomic(op, es, t, purity, loc)
        case AtomicOp.ArrayLength => ErasedAst.Expr.ApplyAtomic(op, es, t, purity, loc)
        case AtomicOp.StructNew(_, mutability, fields) =>
          val specializedStruct = t.asInstanceOf[SimpleType.Struct]
          val specializedFields = fields.map(fieldSym => specializedFieldSym(fieldSym, specializedStruct))
          ErasedAst.Expr.ApplyAtomic(AtomicOp.StructNew(specializedStruct.sym, mutability, specializedFields), es, t, purity, loc)
        case AtomicOp.StructGet(sym) =>
          val List(e) = es
          val specializedStruct = e.tpe.asInstanceOf[SimpleType.Struct]
          val specializedSym = specializedFieldSym(sym, specializedStruct)
          castExp(ErasedAst.Expr.ApplyAtomic(AtomicOp.StructGet(specializedSym), es, erase(tpe), purity, loc), t, purity, loc)
        case AtomicOp.StructPut(sym) =>
          val List(e, _) = es
          val specializedStruct = e.tpe.asInstanceOf[SimpleType.Struct]
          val specializedSym = specializedFieldSym(sym, specializedStruct)
          ErasedAst.Expr.ApplyAtomic(AtomicOp.StructPut(specializedSym), es, t, purity, loc)
        case AtomicOp.InstanceOf(_) => ErasedAst.Expr.ApplyAtomic(op, es, t, purity, loc)
        case AtomicOp.Cast => ErasedAst.Expr.ApplyAtomic(op, es, t, purity, loc)
        case AtomicOp.Unbox => ErasedAst.Expr.ApplyAtomic(op, es, t, purity, loc)
        case AtomicOp.Box => ErasedAst.Expr.ApplyAtomic(op, es, t, purity, loc)
        case AtomicOp.InvokeConstructor(_) => ErasedAst.Expr.ApplyAtomic(op, es, t, purity, loc)
        case AtomicOp.InvokeMethod(_) => ErasedAst.Expr.ApplyAtomic(op, es, t, purity, loc)
        case AtomicOp.InvokeStaticMethod(_) => ErasedAst.Expr.ApplyAtomic(op, es, t, purity, loc)
        case AtomicOp.GetField(_) => ErasedAst.Expr.ApplyAtomic(op, es, t, purity, loc)
        case AtomicOp.PutField(_) => ErasedAst.Expr.ApplyAtomic(op, es, t, purity, loc)
        case AtomicOp.GetStaticField(_) => ErasedAst.Expr.ApplyAtomic(op, es, t, purity, loc)
        case AtomicOp.PutStaticField(_) => ErasedAst.Expr.ApplyAtomic(op, es, t, purity, loc)
        case AtomicOp.Throw => ErasedAst.Expr.ApplyAtomic(op, es, t, purity, loc)
        case AtomicOp.Spawn => ErasedAst.Expr.ApplyAtomic(op, es, t, purity, loc)
        case AtomicOp.Lazy => ErasedAst.Expr.ApplyAtomic(op, es, t, purity, loc)
        case AtomicOp.Force =>
          castExp(ErasedAst.Expr.ApplyAtomic(op, es, erase(tpe), purity, loc), t, purity, loc)
        case AtomicOp.HoleError(_) => ErasedAst.Expr.ApplyAtomic(op, es, t, purity, loc)
        case AtomicOp.MatchError => ErasedAst.Expr.ApplyAtomic(op, es, t, purity, loc)
        case AtomicOp.CastError(_, _) => ErasedAst.Expr.ApplyAtomic(op, es, t, purity, loc)
      }

    case ReducedAst.Expr.ApplyClo(exp1, exp2, ct, tpe, purity, loc) =>
      val ac = ErasedAst.Expr.ApplyClo(visitExp(exp1), visitExp(exp2), ct, box(tpe), purity, loc)
      castExp(unboxExp(ac, erase(tpe), purity, loc), visitType(tpe), purity, loc)
    case ReducedAst.Expr.ApplyDef(sym, exps, ct, tpe, purity, loc) =>
      val ad = ErasedAst.Expr.ApplyDef(sym, exps.map(visitExp), ct, box(tpe), purity, loc)
      castExp(unboxExp(ad, erase(tpe), purity, loc), visitType(tpe), purity, loc)
    case ReducedAst.Expr.ApplyOp(sym, exps, tpe, purity, loc) =>
      ErasedAst.Expr.ApplyOp(sym, exps.map(visitExp), visitType(tpe), purity, loc)
    case ReducedAst.Expr.ApplySelfTail(sym, actuals, tpe, purity, loc) =>
      ErasedAst.Expr.ApplySelfTail(sym, actuals.map(visitExp), visitType(tpe), purity, loc)
    case ReducedAst.Expr.IfThenElse(exp1, exp2, exp3, tpe, purity, loc) =>
      ErasedAst.Expr.IfThenElse(visitExp(exp1), visitExp(exp2), visitExp(exp3), visitType(tpe), purity, loc)
    case ReducedAst.Expr.Branch(exp, branches, tpe, purity, loc) =>
      ErasedAst.Expr.Branch(visitExp(exp), branches.map(visitBranch), visitType(tpe), purity, loc)
    case ReducedAst.Expr.JumpTo(sym, tpe, purity, loc) =>
      ErasedAst.Expr.JumpTo(sym, visitType(tpe), purity, loc)
    case ReducedAst.Expr.Let(sym, exp1, exp2, loc) =>
      ErasedAst.Expr.Let(sym, visitExp(exp1), visitExp(exp2), loc)
    case ReducedAst.Expr.Stmt(exp1, exp2, loc) =>
      ErasedAst.Expr.Stmt(visitExp(exp1), visitExp(exp2), loc)
    case ReducedAst.Expr.Region(sym, exp, tpe, purity, loc) =>
      ErasedAst.Expr.Region(sym, visitExp(exp), visitType(tpe), purity, loc)
    case ReducedAst.Expr.TryCatch(exp, rules, tpe, purity, loc) =>
      ErasedAst.Expr.TryCatch(visitExp(exp), rules.map(visitCatchRule), visitType(tpe), purity, loc)
    case ReducedAst.Expr.RunWith(exp, effUse, rules, ct, tpe, purity, loc) =>
      val tw = ErasedAst.Expr.RunWith(visitExp(exp), effUse, rules.map(visitHandlerRule), ct, box(tpe), purity, loc)
      castExp(unboxExp(tw, erase(tpe), purity, loc), visitType(tpe), purity, loc)
    case ReducedAst.Expr.NewObject(name, clazz, tpe, purity, methods, loc) =>
      ErasedAst.Expr.NewObject(name, clazz, visitType(tpe), purity, methods.map(visitJvmMethod), loc)
  }

  /**
    * Returns a copy of `sym` that refers to the enum of `tpe`.
    *
    * {{{
    *   specializedCaseSym("Option.Some", "Option$42") = "Option$42.Some"
    * }}}
    */
  private def specializedCaseSym(sym0: Symbol.CaseSym, tpe: SimpleType.Enum): Symbol.CaseSym = {
    new Symbol.CaseSym(tpe.sym, sym0.name, sym0.loc)
  }

  /**
    * Returns a copy of `sym` that refers to the struct of `tpe`.
    *
    * {{{
    *   specializedFieldSym("MutList.length", "MutList$42") = "MutList$42.length"
    * }}}
    */
  private def specializedFieldSym(sym0: Symbol.StructFieldSym, tpe: SimpleType.Struct): Symbol.StructFieldSym = {
    new Symbol.StructFieldSym(tpe.sym, sym0.name, sym0.loc)
  }

  private def castExp(exp: ErasedAst.Expr, t: SimpleType, purity: Purity, loc: SourceLocation): ErasedAst.Expr = {
    ErasedAst.Expr.ApplyAtomic(AtomicOp.Cast, List(exp), t, purity, loc.asSynthetic)
  }

  private def unboxExp(exp: ErasedAst.Expr, t: SimpleType, purity: Purity, loc: SourceLocation): ErasedAst.Expr = {
    ErasedAst.Expr.ApplyAtomic(AtomicOp.Unbox, List(exp), t, purity, loc.asSynthetic)
  }

  private def visitEffect(eff: ReducedAst.Effect)(implicit ctx: SharedContext, flix: Flix): ErasedAst.Effect = eff match {
    case ReducedAst.Effect(ann, mod, sym, ops, loc) =>
      ErasedAst.Effect(ann, mod, sym, ops.map(visitOp), loc)
  }

  private def visitOp(op: ReducedAst.Op)(implicit ctx: SharedContext, flix: Flix): ErasedAst.Op = op match {
    case ReducedAst.Op(sym, ann, mod, fparams, tpe, purity, loc) =>
      ErasedAst.Op(sym, ann, mod, fparams.map(visitParam), erase(tpe), purity, loc)
  }

  private def visitType(tpe0: SimpleType)(implicit ctx: SharedContext, flix: Flix): SimpleType = {
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
      case Enum(sym, targs) =>
        // `Option[String]` erases to `Option[Object]` and is then specialized to `Option$42`.
        val specializedSym = ctx.getSpecializedEnumName(sym, targs.map(erase))
        SimpleType.mkEnum(specializedSym, Nil)
      case Struct(sym, targs) =>
        // `MutList[String, r]` erases to `MutList[Object, Object]` and is then specialized to `MutList$42`.
        val specializedSym = ctx.getSpecializedStructName(sym, targs.map(erase))
        SimpleType.Struct(specializedSym, Nil)
      case Arrow(args, result) => SimpleType.mkArrow(args.map(visitType), box(result))
      case RecordEmpty => RecordEmpty
      case RecordExtend(label, value, rest) => RecordExtend(label, erase(value), visitType(rest))
      case ExtensibleExtend(cons, tpes, rest) => ExtensibleExtend(cons, tpes.map(erase), visitType(rest))
      case ExtensibleEmpty => ExtensibleEmpty
      case Native(clazz) => Native(clazz)
    }
  }

  /**
    * Instantiates `tpe` given the variable map `subst` and erases the result.
    *
    * Examples:
    *   - `instantiateAndEraseType([x -> Int32], x) = Int32`
    *   - `instantiateAndEraseType(_, Int32) = Int32`
    *   - `instantiateAndEraseType(_, Object) = Object`
    *   - `instantiateAndEraseType([x -> String], x) = Object`
    *   - `instantiateAndEraseType(_, Option[Int32]) = Object`
    *   - `instantiateAndEraseType([x -> Int32], y) = crash!`
    */
  private def instantiateAndEraseType(subst: Map[Symbol.KindedTypeVarSym, SimpleType], tpe: Type) = tpe match {
    case Type.Var(sym, _) => erase(subst(sym))
    case Type.Cst(tc, _) => tc match {
      case TypeConstructor.Bool => SimpleType.Bool
      case TypeConstructor.Char => SimpleType.Char
      case TypeConstructor.Float32 => SimpleType.Float32
      case TypeConstructor.Float64 => SimpleType.Float64
      case TypeConstructor.Int8 => SimpleType.Int8
      case TypeConstructor.Int16 => SimpleType.Int16
      case TypeConstructor.Int32 => SimpleType.Int32
      case TypeConstructor.Int64 => SimpleType.Int64
      // All primitive types are covered, so the rest can only be erased to Object.
      case _ => SimpleType.Object
    }
    // Any type application will result in an Object type.
    case Type.Apply(_, _, _) => SimpleType.Object

    case Type.Alias(_, _, _, _) => throw InternalCompilerException(s"Unexpected type $tpe", tpe.loc)
    case Type.AssocType(_, _, _, _) => throw InternalCompilerException(s"Unexpected type $tpe", tpe.loc)
    case Type.JvmToType(_, _) => throw InternalCompilerException(s"Unexpected type $tpe", tpe.loc)
    case Type.JvmToEff(_, _) => throw InternalCompilerException(s"Unexpected type $tpe", tpe.loc)
    case Type.UnresolvedJvmType(_, _) => throw InternalCompilerException(s"Unexpected type $tpe", tpe.loc)
  }

  private def box(@unused tpe: SimpleType): SimpleType = SimpleType.Object

  private final class SharedContext {

    /**
      * `(Option, List(Int32)) -> Option$42` means that `Option` is specialized wrt. `Int32` under the name `Option$42`.
      */
    private val enumSpecializations: ConcurrentHashMap[(Symbol.EnumSym, List[SimpleType]), Symbol.EnumSym] =
      new ConcurrentHashMap()

    /**
      * `(MutList, List(Int32)) -> MutList$42` means that `MutList` is specialized wrt. `Int32` under the name `MutList$42`.
      */
    private val structSpecializations: ConcurrentHashMap[(Symbol.StructSym, List[SimpleType]), Symbol.StructSym] =
      new ConcurrentHashMap()

    /** Returns the specialized version of `sym` according to `targs`, creating a new symbol if not done already. */
    def getSpecializedEnumName(sym: Symbol.EnumSym, targs: List[SimpleType])(implicit flix: Flix): Symbol.EnumSym = {
      targs match {
        case Nil =>
          // No specialization required.
          enumSpecializations.computeIfAbsent((sym, targs), _ => sym)
        case _ =>
          // Do specialization.
          enumSpecializations.computeIfAbsent((sym, targs), _ => Symbol.freshEnumSym(sym))
      }
    }

    /**
      * Returns all enum specializations `(sym, targs, newSym)` where `sym` should be
      * specialized wrt. `targs` under the name `newSym`.
      */
    def getEnumSpecializations: List[(Symbol.EnumSym, List[SimpleType], Symbol.EnumSym)] =
      toList(enumSpecializations)

    /** Returns the specialized version of `sym` according to `targs`, creating a new symbol if not done already. */
    def getSpecializedStructName(sym: Symbol.StructSym, targs: List[SimpleType])(implicit flix: Flix): Symbol.StructSym = {
      targs match {
        case Nil =>
          // No specialization required.
          structSpecializations.computeIfAbsent((sym, targs), _ => sym)
        case _ =>
          // Do specialization.
          structSpecializations.computeIfAbsent((sym, targs), _ => Symbol.freshStructSym(sym))
      }
    }

    /**
      * Returns all struct specializations `(sym, targs, newSym)` where `sym` should be
      * specialized wrt. `targs` under the name `newSym`.
      */
    def getStructSpecializations: List[(Symbol.StructSym, List[SimpleType], Symbol.StructSym)] =
      toList(structSpecializations)

    /** Returns the entries of `m`. */
    private def toList[A, B](m: ConcurrentHashMap[(A, B), A]): List[(A, B, A)] = {
      val res = mutable.ListBuffer.empty[(A, B, A)]
      m.forEach(new BiConsumer[(A, B), A] {
        override def accept(t: (A, B), u: A): Unit = res.append((t._1, t._2, u))
      })
      res.toList
    }

  }

}

package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.MonoAst.{Case, CatchRule, Def, Effect, Enum, Expr, FormalParam, HandlerRule, JvmMethod, MatchRule, Occur, Op, Pattern, Root, Spec, Struct, StructField, TypeParam}
import ca.uwaterloo.flix.language.ast.shared.{BoundBy, Constant, Scope}
import ca.uwaterloo.flix.language.ast.{AtomicOp, Kind, LoweredAst, Symbol, Type, TypeConstructor}
import ca.uwaterloo.flix.language.dbg.AstPrinter
import ca.uwaterloo.flix.language.phase.unification.Substitution
import ca.uwaterloo.flix.util.collection.{MapOps, MultiMap}
import ca.uwaterloo.flix.util.{InternalCompilerException, ParOps}

import scala.collection.mutable

/**
  * For an enums with a single constructor of a single value, like
  * {{{
  *   enum Wrap[t, ef] {
  *       case Single(Unit -> t \ ef)
  *   }
  * }}}
  * the following transformations will happen
  * {{{
  *   match (e: Wrap[t, ef]) { case Wrap.Single(f) => .. f .. }
  *   // becomes
  *   let f = e; .. f ..
  * }}}
  * {{{
  *   Wrap.Single(f)
  *   // becomes
  *   f
  * }}}
  * {{{
  *   Wrap[t, ef]
  *   // becomes
  *   Unit -> t \ ef
  * }}}
  *
  * Optimization of wrapper enums CANNOT occur when:
  *   - The enum refers to itself, directly or transitively. In that case you have to make sure that
  *     only a subset of the enums in the cycle is optimized away to avoid infinite types.
  *   - The enum type is used partially applied. To allow this we would need to erase the partial
  *     enum type into a type function, representing the remaining transformation.
  */
object EnumOptimizer {

  implicit val scope: Scope = Scope.Top

  type Enums = Map[Symbol.EnumSym, (List[TypeParam], Type)]

  def run(root: Root)(implicit flix: Flix): Root = flix.phase("EnumOptimizer") {
    implicit val wrapperEnums: Enums = getWrapperEnums(root)

    val enums = MapOps.mapValues(root.enums.filterNot {
      case (sym, _) => wrapperEnums.contains(sym)
    })(visitEnum)
    val defs = ParOps.parMapValues(root.defs)(visitDef)
    val structs = ParOps.parMapValues(root.structs)(visitStruct)
    val effects = ParOps.parMapValues(root.effects)(visitEffect)

    Root(defs, enums, structs, effects, root.mainEntryPoint, root.entryPoints, root.sources)
  }(AstPrinter.DebugMonoAst)

  private def getWrapperEnums(root: Root): Map[Symbol.EnumSym, (List[TypeParam], Type)] = {
    val enumGraph = enumDeps(root)
    root.enums.values.flatMap {
      case Enum(_, _, _, sym, tparams, cases, _) =>
        cases.values.toList match {
          case List(Case(_, List(tpe), _)) if !isCyclic(sym, enumGraph) => Some((sym, (tparams, tpe)))
          case _ => None
        }
    }.toMap
  }

  private def enumDeps(root: Root): MultiMap[Symbol.EnumSym, Symbol.EnumSym] = {
    var m = MultiMap.empty[Symbol.EnumSym, Symbol.EnumSym]

    def add(sym1: Symbol.EnumSym, sym2: Symbol.EnumSym): Unit = m = m + (sym1 -> sym2)

    for (enm <- root.enums.values) {
      enm.cases.values.foreach(caze => caze.tpes.flatMap(enumsOf).foreach(sym => add(enm.sym, sym)))
    }
    m
  }

  private def enumsOf(tpe: Type): Set[Symbol.EnumSym] = tpe match {
    case Type.Var(_, _) => Set.empty
    case Type.Cst(TypeConstructor.Enum(sym, _), _) => Set(sym)
    case Type.Cst(_, _) => Set.empty
    case Type.Apply(tpe1, tpe2, _) => enumsOf(tpe1) ++ enumsOf(tpe2)
    case Type.Alias(_, _, _, _) => throw unexpectedTypeError(tpe)
    case Type.AssocType(_, _, _, _) => throw unexpectedTypeError(tpe)
    case Type.JvmToType(_, _) => throw unexpectedTypeError(tpe)
    case Type.JvmToEff(_, _) => throw unexpectedTypeError(tpe)
    case Type.UnresolvedJvmType(_, _) => throw unexpectedTypeError(tpe)
  }

  private def isCyclic(symToFind: Symbol.EnumSym, enumGraph: MultiMap[Symbol.EnumSym, Symbol.EnumSym]): Boolean = {
    val seen = mutable.Set.empty[Symbol.EnumSym]
    val todo = mutable.ArrayDeque.empty[Symbol.EnumSym]
    todo.addOne(symToFind)
    while (todo.nonEmpty) {
      val curr = todo.removeHead()
      seen.add(curr)
      for (next <- enumGraph(curr)) {
        if (next == symToFind) return true
        else if (seen.contains(next)) () // nothing
        else todo.append(next)
      }
    }
    false
  }

  private def visitDef(defn: Def)(implicit wrappers: Enums, flix: Flix): Def = {
    val spec = visitSpec(defn.spec, isGround = true)
    val exp = visitExp(defn.exp)
    Def(defn.sym, spec, exp, defn.loc)
  }

  private def visitSpec(spec: Spec, isGround: Boolean)(implicit wrappers: Enums, flix: Flix): Spec = {
    val fparams = spec.fparams.map(visitFparam(_, isGround))
    val functionType = visitType(spec.functionType, isGround)
    val retTpe = visitType(spec.retTpe, isGround)
    val eff = visitEff(spec.eff, isGround)
    Spec(spec.doc, spec.ann, spec.mod, fparams, functionType, retTpe, eff, spec.defContext)
  }

  private def visitStruct(struct: Struct)(implicit wrappers: Enums, flix: Flix): Struct = {
    val fields = struct.fields.map(visitStructField)
    Struct(struct.doc, struct.ann, struct.mod, struct.sym, struct.tparams, fields, struct.loc)
  }

  private def visitStructField(field: StructField)(implicit wrappers: Enums, flix: Flix): StructField = {
    val tpe = visitType(field.tpe, isGround = false)
    StructField(field.sym, tpe, field.loc)
  }

  private def visitEffect(effect: Effect)(implicit wrappers: Enums, flix: Flix): Effect = {
    val ops = effect.ops.map(visitOp)
    Effect(effect.doc, effect.ann, effect.mod, effect.sym, ops, effect.loc)
  }

  private def visitOp(op: Op)(implicit wrappers: Enums, flix: Flix): Op = {
    val spec = visitSpec(op.spec, isGround = true)
    Op(op.sym, spec, op.loc)
  }

  private def visitEnum(enm: Enum)(implicit wrappers: Enums, flix: Flix): Enum = {
    val cases = MapOps.mapValues(enm.cases)(visitCase)
    Enum(enm.doc, enm.ann, enm.mod, enm.sym, enm.tparams, cases, enm.loc)
  }

  private def visitCase(caze: Case)(implicit wrappers: Enums, flix: Flix): Case = {
    val tpes = caze.tpes.map(visitType(_, isGround = false))
    Case(caze.sym, tpes, caze.loc)
  }

  private def visitFparam(fp: FormalParam, isGround: Boolean)(implicit wrappers: Enums, flix: Flix): FormalParam = {
    val tpe = visitType(fp.tpe, isGround)
    FormalParam(fp.sym, fp.mod, tpe, fp.src, fp.occur, fp.loc)
  }

  private def visitExp(expr: Expr)(implicit wrappers: Enums, flix: Flix): Expr = expr match {
    case Expr.Cst(cst, tpe, loc) =>
      val t = visitType(tpe, isGround = true)
      Expr.Cst(cst, t, loc)
    case Expr.Var(sym, tpe, loc) =>
      val t = visitType(tpe, isGround = true)
      Expr.Var(sym, t, loc)
    case Expr.Lambda(fparam, exp, tpe, loc) =>
      val fp = visitFparam(fparam, isGround = true)
      val e = visitExp(exp)
      val t = visitType(tpe, isGround = true)
      Expr.Lambda(fp, e, t, loc)
    case Expr.ApplyAtomic(op, exps, tpe, eff, loc) =>
      val es = exps.map(visitExp)
      op match {
        case AtomicOp.Is(sym) if wrappers.contains(sym.enumSym) =>
          val List(e) = es
          val freshVar = Symbol.freshVarSym("eopt", BoundBy.Let, loc)
          val ef = visitEff(e.eff, isGround = true)
          Expr.Let(freshVar, e, Expr.Cst(Constant.Bool(true), Type.mkBool(loc), loc), Type.mkBool(loc), ef, Occur.Unknown, loc)
        case AtomicOp.Tag(sym) if wrappers.contains(sym.enumSym) =>
          val List(e) = es
          e
        case AtomicOp.Untag(sym, _) if wrappers.contains(sym.enumSym) =>
          val List(e) = es
          e
        case _ =>
          val t = visitType(tpe, isGround = true)
          val ef = eff
          Expr.ApplyAtomic(op, es, t, ef, loc)
      }
    case Expr.ApplyClo(exp1, exp2, tpe, eff, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      val t = visitType(tpe, isGround = true)
      val ef = visitEff(eff, isGround = true)
      Expr.ApplyClo(e1, e2, t, ef, loc)
    case Expr.ApplyDef(sym, exps, itpe, tpe, eff, loc) =>
      val es = exps.map(visitExp)
      val it = visitType(itpe, isGround = true)
      val t = visitType(tpe, isGround = true)
      val ef = visitEff(eff, isGround = true)
      Expr.ApplyDef(sym, es, it, t, ef, loc)
    case Expr.ApplyLocalDef(sym, exps, tpe, eff, loc) =>
      val es = exps.map(visitExp)
      val t = visitType(tpe, isGround = true)
      val ef = visitEff(eff, isGround = true)
      Expr.ApplyLocalDef(sym, es, t, ef, loc)
    case Expr.Let(sym, exp1, exp2, tpe, eff, occur, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      val t = visitType(tpe, isGround = true)
      val ef = visitEff(eff, isGround = true)
      Expr.Let(sym, e1, e2, t, ef, occur, loc)
    case Expr.LocalDef(sym, fparams, exp1, exp2, tpe, eff, occur, loc) =>
      val fps = fparams.map(visitFparam(_, isGround = true))
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      val t = visitType(tpe, isGround = true)
      val ef = visitEff(eff, isGround = true)
      Expr.LocalDef(sym, fps, e1, e2, t, ef, occur, loc)
    case Expr.Scope(sym, regSym, exp, tpe, eff, loc) =>
      val e1 = visitExp(exp)
      val t = visitType(tpe, isGround = true)
      val ef = visitEff(eff, isGround = true)
      Expr.Scope(sym, regSym, e1, t, ef, loc)
    case Expr.IfThenElse(exp1, exp2, exp3, tpe, eff, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      val e3 = visitExp(exp3)
      val t = visitType(tpe, isGround = true)
      val ef = visitEff(eff, isGround = true)
      Expr.IfThenElse(e1, e2, e3, t, ef, loc)
    case Expr.Stm(exp1, exp2, tpe, eff, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      val t = visitType(tpe, isGround = true)
      val ef = visitEff(eff, isGround = true)
      Expr.Stm(e1, e2, t, ef, loc)
    case Expr.Discard(exp, eff, loc) =>
      val e = visitExp(exp)
      val ef = visitEff(eff, isGround = true)
      Expr.Discard(e, ef, loc)
    case Expr.Match(exp, rules, tpe, eff, loc) =>
      val e = visitExp(exp)
      val rs = rules.map(visitMatchRule)
      val t = visitType(tpe, isGround = true)
      val ef = visitEff(eff, isGround = true)
      Expr.Match(e, rs, t, ef, loc)
    case Expr.VectorLit(exps, tpe, eff, loc) =>
      val es = exps.map(visitExp)
      val t = visitType(tpe, isGround = true)
      val ef = visitEff(eff, isGround = true)
      Expr.VectorLit(es, t, ef, loc)
    case Expr.VectorLoad(exp1, exp2, tpe, eff, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      val t = visitType(tpe, isGround = true)
      val ef = visitEff(eff, isGround = true)
      Expr.VectorLoad(e1, e2, t, ef, loc)
    case Expr.VectorLength(exp, loc) =>
      val e = visitExp(exp)
      Expr.VectorLength(e, loc)
    case Expr.Ascribe(exp, tpe, eff, loc) =>
      val e = visitExp(exp)
      val t = visitType(tpe, isGround = true)
      val ef = visitEff(eff, isGround = true)
      Expr.Ascribe(e, t, ef, loc)
    case Expr.Cast(exp, tpe, eff, loc) =>
      val e = visitExp(exp)
      val t = visitType(tpe, isGround = true)
      val ef = visitEff(eff, isGround = true)
      Expr.Cast(e, t, ef, loc)
    case Expr.TryCatch(exp, rules, tpe, eff, loc) =>
      val e = visitExp(exp)
      val rs = rules.map(visitCatchRule)
      val t = visitType(tpe, isGround = true)
      val ef = visitEff(eff, isGround = true)
      Expr.TryCatch(e, rs, t, ef, loc)
    case Expr.RunWith(exp, effUse, rules, tpe, eff, loc) =>
      val e = visitExp(exp)
      val rs = rules.map(visitHandlerRule)
      val t = visitType(tpe, isGround = true)
      val ef = visitEff(eff, isGround = true)
      Expr.RunWith(e, effUse, rs, t, ef, loc)
    case Expr.Do(op, exps, tpe, eff, loc) =>
      val es = exps.map(visitExp)
      val t = visitType(tpe, isGround = true)
      val ef = visitEff(eff, isGround = true)
      Expr.Do(op, es, t, ef, loc)
    case Expr.NewObject(name, clazz, tpe, eff, methods, loc) =>
      val t = visitType(tpe, isGround = true)
      val ef = visitEff(eff, isGround = true)
      val ms = methods.map(visitJvmMethod)
      Expr.NewObject(name, clazz, t, ef, ms, loc)
  }

  private def visitType(tpe: Type, isGround: Boolean)(implicit wrappers: Enums, flix: Flix): Type = {
    val base = tpe.baseType
    val targs = tpe.typeArguments
    base match {
      case v@Type.Var(_, _) => Type.mkApply(v, targs.map(visitType(_, isGround)), tpe.loc)
      case Type.Cst(TypeConstructor.Enum(sym, kind), _) if wrappers.contains(sym) =>
        if (Kind.kindArgs(kind).sizeIs != targs.size) throw unexpectedTypeError(tpe)
        val (args, res) = wrappers(sym)
        val substitutedType = Substitution(args.map(_.sym).zip(targs.map(visitType(_, isGround))).toMap).apply(res)
        Monomorpher.simplify(visitType(substitutedType, isGround), isGround)(LoweredAst.empty, flix)
      case cst@Type.Cst(_, _) => Type.mkApply(cst, targs.map(visitType(_, isGround)), tpe.loc)
      case Type.Alias(_, _, _, _) => throw unexpectedTypeError(tpe)
      case Type.AssocType(_, _, _, _) => throw unexpectedTypeError(tpe)
      case Type.JvmToType(_, _) => throw unexpectedTypeError(tpe)
      case Type.JvmToEff(_, _) => throw unexpectedTypeError(tpe)
      case Type.UnresolvedJvmType(_, _) => throw unexpectedTypeError(tpe)
    }
  }

  private def visitEff(eff: Type, isGround: Boolean)(implicit wrappers: Enums, flix: Flix): Type = {
    visitType(eff, isGround)
  }

  private def visitMatchRule(r: MatchRule)(implicit wrappers: Enums, flix: Flix): MatchRule = {
    val p = visitPat(r.pat)
    val g = r.guard.map(visitExp)
    val e = visitExp(r.exp)
    MatchRule(p, g, e)
  }

  private def visitCatchRule(r: CatchRule)(implicit wrappers: Enums, flix: Flix): CatchRule = {
    val e = visitExp(r.exp)
    CatchRule(r.sym, r.clazz, e)
  }

  private def visitHandlerRule(r: HandlerRule)(implicit wrappers: Enums, flix: Flix): HandlerRule = {
    val fps = r.fparams.map(visitFparam(_, isGround = true))
    val e = visitExp(r.exp)
    HandlerRule(r.op, fps, e)
  }

  private def visitJvmMethod(m: JvmMethod)(implicit wrappers: Enums, flix: Flix): JvmMethod = {
    val fps = m.fparams.map(visitFparam(_, isGround = true))
    val e = visitExp(m.exp)
    val rt = visitType(m.retTpe, isGround = true)
    val ef = visitEff(m.eff, isGround = true)
    JvmMethod(m.ident, fps, e, rt, ef, m.loc)
  }

  private def visitPat(pat0: Pattern)(implicit wrappers: Enums, flix: Flix): Pattern = pat0 match {
    case Pattern.Wild(tpe, loc) =>
      val t = visitType(tpe, isGround = true)
      Pattern.Wild(t, loc)
    case Pattern.Var(sym, tpe, occur, loc) =>
      val t = visitType(tpe, isGround = true)
      Pattern.Var(sym, t, occur, loc)
    case Pattern.Cst(cst, tpe, loc) =>
      val t = visitType(tpe, isGround = true)
      Pattern.Cst(cst, t, loc)
    case Pattern.Tag(sym, pats, _, _) if wrappers.contains(sym.sym.enumSym) => pats match {
      case List(pat) => visitPat(pat)
      case _ => throw InternalCompilerException(s"Unexpected pattern '$pat0'", pat0.loc)
    }
    case Pattern.Tag(sym, pats, tpe, loc) =>
      val ps = pats.map(visitPat)
      val t = visitType(tpe, isGround = true)
      Pattern.Tag(sym, ps, t, loc)
    case Pattern.Tuple(pats, tpe, loc) =>
      val ps = pats.map(visitPat)
      val t = visitType(tpe, isGround = true)
      Pattern.Tuple(ps, t, loc)
    case Pattern.Record(pats, pat, tpe, loc) =>
      val ps = pats.map(visitRecordLabelPattern)
      val p = visitPat(pat)
      val t = visitType(tpe, isGround = true)
      Pattern.Record(ps, p, t, loc)
  }

  private def visitRecordLabelPattern(rpat: Pattern.Record.RecordLabelPattern)(implicit wrappers: Enums, flix: Flix): Pattern.Record.RecordLabelPattern = {
    val p = visitPat(rpat.pat)
    val t = visitType(rpat.tpe, isGround = true)
    Pattern.Record.RecordLabelPattern(rpat.label, p, t, rpat.loc)
  }

  private def unexpectedTypeError(tpe: Type): InternalCompilerException =
    InternalCompilerException(s"Unexpected type '$tpe'", tpe.loc)

}

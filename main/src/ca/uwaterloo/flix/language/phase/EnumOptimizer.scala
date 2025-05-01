package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.MonoAst.{Case, CatchRule, Def, Effect, Enum, Expr, FormalParam, HandlerRule, JvmMethod, MatchRule, Op, Pattern, Root, Spec, Struct, StructField, TypeParam}
import ca.uwaterloo.flix.language.ast.shared.{BoundBy, Constant, Scope}
import ca.uwaterloo.flix.language.ast.{AtomicOp, Kind, LoweredAst, Symbol, Type, TypeConstructor}
import ca.uwaterloo.flix.language.phase.unification.Substitution
import ca.uwaterloo.flix.util.{InternalCompilerException, ParOps}

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
  * It is assumed that enum types do not occur in effects.
  */
object EnumOptimizer {

  implicit val scope: Scope = Scope.Top

  type Enums = Map[Symbol.EnumSym, (List[TypeParam], Type)]

  def run(root: Root)(implicit flix: Flix): Root = {
    implicit val wrapperEnums: Enums = getWrapperEnums(root)

    val enums = root.enums.filterNot {
      case (sym, _) => wrapperEnums.contains(sym)
    }
    val defs = ParOps.parMapValues(root.defs)(visitDef)
    val structs = ParOps.parMapValues(root.structs)(visitStruct)
    val effects = ParOps.parMapValues(root.effects)(visitEffect)

    Root(defs, enums, structs, effects, root.mainEntryPoint, root.entryPoints, root.sources)
  }

  private def getWrapperEnums(root: Root): Map[Symbol.EnumSym, (List[TypeParam], Type)] = {
    root.enums.values.flatMap{
      case Enum(_, _, _, sym, tparams, cases, _) =>
        cases.values.toList match {
          case List(Case(_, List(tpe), _)) if !containsEnum(sym, tpe) => Some((sym, (tparams, tpe)))
          case _ => None
        }
    }.toMap
  }

  private def containsEnum(enm: Symbol.EnumSym, tpe: Type): Boolean = tpe match {
    case Type.Var(_, _) => false
    case Type.Cst(TypeConstructor.Enum(sym, _), _) => enm == sym
    case Type.Cst(_, _) => false
    case Type.Apply(tpe1, tpe2, _) => containsEnum(enm, tpe1) || containsEnum(enm, tpe2)
    case Type.Alias(_, _, _, loc) => throw InternalCompilerException(s"Unexpected type '$tpe'", loc)
    case Type.AssocType(_, _, _, loc) => throw InternalCompilerException(s"Unexpected type '$tpe'", loc)
    case Type.JvmToType(_, loc) => throw InternalCompilerException(s"Unexpected type '$tpe'", loc)
    case Type.JvmToEff(_, loc) => throw InternalCompilerException(s"Unexpected type '$tpe'", loc)
    case Type.UnresolvedJvmType(_, loc) => throw InternalCompilerException(s"Unexpected type '$tpe'", loc)
  }

  private def visitDef(defn: Def)(implicit wrappers: Enums, flix: Flix): Def = {
    val spec = visitSpec(defn.spec)
    val exp = visitExp(defn.exp)
    Def(defn.sym, spec, exp, defn.loc)
  }

  private def visitSpec(spec: Spec)(implicit wrappers: Enums, flix: Flix): Spec = {
    val fparams = spec.fparams.map(visitFparam)
    val functionType = visitType(spec.functionType)
    val retTpe = visitType(spec.retTpe)
    Spec(spec.doc, spec.ann, spec.mod, fparams, functionType, retTpe, spec.eff)
  }

  private def visitStruct(struct: Struct)(implicit wrappers: Enums, flix: Flix): Struct = {
    val fields = struct.fields.map(visitStructField)
    Struct(struct.doc, struct.ann, struct.mod, struct.sym, struct.tparams, fields, struct.loc)
  }

  private def visitStructField(field: StructField)(implicit wrappers: Enums, flix: Flix): StructField = {
    val tpe = visitType(field.tpe)
    StructField(field.sym, tpe, field.loc)
  }

  private def visitEffect(effect: Effect)(implicit wrappers: Enums, flix: Flix): Effect = {
    val ops = effect.ops.map(visitOp)
    Effect(effect.doc, effect.ann, effect.mod, effect.sym, ops, effect.loc)
  }

  private def visitOp(op: Op)(implicit wrappers: Enums, flix: Flix): Op = {
    val spec = visitSpec(op.spec)
    Op(op.sym, spec, op.loc)
  }

  private def visitFparam(fp: FormalParam)(implicit wrappers: Enums, flix: Flix): FormalParam = {
    val tpe = visitType(fp.tpe)
    FormalParam(fp.sym, fp.mod, tpe, fp.src, fp.loc)
  }

  private def visitExp(expr: Expr)(implicit wrappers: Enums, flix: Flix): Expr = expr match {
    case Expr.Cst(cst, tpe, loc) => Expr.Cst(cst, visitType(tpe), loc)
    case Expr.Var(sym, tpe, loc) => Expr.Var(sym, visitType(tpe), loc)
    case Expr.Lambda(fparam, exp, tpe, loc) => Expr.Lambda(visitFparam(fparam), visitExp(exp), visitType(tpe), loc)
    case Expr.ApplyAtomic(op, exps0, tpe, eff, loc) =>
      val exps = exps0.map(visitExp)
      op match {
      case AtomicOp.Is(sym) if wrappers.contains(sym.enumSym) && exps.sizeIs == 1 =>
        val List(exp) = exps
        val binder = Symbol.freshVarSym("eopt", BoundBy.Let, loc)
        Expr.Let(binder, exp, Expr.Cst(Constant.Bool(true), Type.mkBool(loc), loc), Type.mkBool(loc), exp.eff, loc)
      case AtomicOp.Tag(sym) if wrappers.contains(sym.enumSym) =>
        val List(exp) = exps
        exp
      case AtomicOp.Untag(sym, _) if wrappers.contains(sym.enumSym) =>
        val List(exp) = exps
        exp
      case _ =>
        val t = visitType(tpe)
        Expr.ApplyAtomic(op, exps, t, eff, loc)
    }
    case Expr.ApplyClo(exp1, exp2, tpe, eff, loc) =>
      Expr.ApplyClo(visitExp(exp1), visitExp(exp2), visitType(tpe), eff, loc)
    case Expr.ApplyDef(sym, exps, itpe, tpe, eff, loc) =>
      Expr.ApplyDef(sym, exps.map(visitExp), visitType(itpe), visitType(tpe), eff, loc)
    case Expr.ApplyLocalDef(sym, exps, tpe, eff, loc) =>
      Expr.ApplyLocalDef(sym, exps.map(visitExp), visitType(tpe), eff, loc)
    case Expr.Let(sym, exp1, exp2, tpe, eff, loc) =>
      Expr.Let(sym, visitExp(exp1), visitExp(exp2), visitType(tpe), eff, loc)
    case Expr.LocalDef(sym, fparams, exp1, exp2, tpe, eff, loc) =>
      Expr.LocalDef(sym, fparams.map(visitFparam), visitExp(exp1), visitExp(exp2), visitType(tpe), eff, loc)
    case Expr.Scope(sym, regSym, exp, tpe, eff, loc) =>
      Expr.Scope(sym, regSym, visitExp(exp), visitType(tpe), eff, loc)
    case Expr.IfThenElse(exp1, exp2, exp3, tpe, eff, loc) =>
      Expr.IfThenElse(visitExp(exp1), visitExp(exp2), visitExp(exp3), visitType(tpe), eff, loc)
    case Expr.Stm(exp1, exp2, tpe, eff, loc) =>
      Expr.Stm(visitExp(exp1), visitExp(exp2), visitType(tpe), eff, loc)
    case Expr.Discard(exp, eff, loc) =>
      Expr.Discard(visitExp(exp), eff, loc)
    case Expr.Match(exp, rules, tpe, eff, loc) =>
      Expr.Match(visitExp(exp), rules.map(visitMatchRule), visitType(tpe), eff, loc)
    case Expr.VectorLit(exps, tpe, eff, loc) =>
      Expr.VectorLit(exps, visitType(tpe), eff, loc)
    case Expr.VectorLoad(exp1, exp2, tpe, eff, loc) =>
      Expr.VectorLoad(visitExp(exp1), visitExp(exp2), visitType(tpe), eff, loc)
    case Expr.VectorLength(exp, loc) =>
      Expr.VectorLength(visitExp(exp), loc)
    case Expr.Ascribe(exp, tpe, eff, loc) =>
      Expr.Ascribe(visitExp(exp), visitType(tpe), eff, loc)
    case Expr.Cast(exp, declaredType, declaredEff, tpe, eff, loc) =>
      Expr.Cast(visitExp(exp), declaredType.map(visitType), declaredEff, visitType(tpe), eff, loc)
    case Expr.TryCatch(exp, rules, tpe, eff, loc) =>
      Expr.TryCatch(visitExp(exp), rules.map(visitCatchRule), visitType(tpe), eff, loc)
    case Expr.RunWith(exp, effUse, rules, tpe, eff, loc) =>
      Expr.RunWith(visitExp(exp), effUse, rules.map(visitHandlerRule), visitType(tpe), eff, loc)
    case Expr.Do(op, exps, tpe, eff, loc) =>
      Expr.Do(op, exps.map(visitExp), visitType(tpe), eff, loc)
    case Expr.NewObject(name, clazz, tpe, eff, methods, loc) =>
      Expr.NewObject(name, clazz, visitType(tpe), eff, methods.map(visitJvmMethod), loc)
  }

  private def visitType(tpe: Type)(implicit wrappers: Enums, flix: Flix): Type = {
    val base = tpe.baseType
    val targs = tpe.typeArguments.map(visitType)
    val res = base match {
      case Type.Var(_, _) => Type.mkApply(base, targs, tpe.loc)
      case Type.Cst(TypeConstructor.Enum(sym, kind), loc) if wrappers.contains(sym) =>
        if (Kind.kindArgs(kind).sizeIs != targs.size) throw InternalCompilerException(s"Unexpected type '$tpe'", loc)
        val (args, res) = wrappers(sym)
        val substitutedType = Substitution(args.map(_.sym).zip(targs).toMap).apply(res)
        Monomorpher.simplify(visitType(substitutedType), isGround = false)(LoweredAst.empty, flix)
      case Type.Cst(_, _) => Type.mkApply(base, targs, tpe.loc)
      case Type.Alias(_, _, _, loc) => throw InternalCompilerException(s"Unexpected type '$tpe'", loc)
      case Type.AssocType(_, _, _, loc) => throw InternalCompilerException(s"Unexpected type '$tpe'", loc)
      case Type.JvmToType(_, loc) => throw InternalCompilerException(s"Unexpected type '$tpe'", loc)
      case Type.JvmToEff(_, loc) => throw InternalCompilerException(s"Unexpected type '$tpe'", loc)
      case Type.UnresolvedJvmType(_, loc) => throw InternalCompilerException(s"Unexpected type '$tpe'", loc)
    }
    if (containsEnum(Symbol.mkEnumSym("Map"), res)) println(res)
    res
  }

  private def visitMatchRule(r: MatchRule)(implicit wrappers: Enums, flix: Flix): MatchRule = {
    val pat = visitPat(r.pat)
    val guard = r.guard.map(visitExp)
    val exp = visitExp(r.exp)
    MatchRule(pat, guard, exp)
  }

  private def visitCatchRule(r: CatchRule)(implicit wrappers: Enums, flix: Flix): CatchRule = {
    val exp = visitExp(r.exp)
    CatchRule(r.sym, r.clazz, exp)
  }

  private def visitHandlerRule(r: HandlerRule)(implicit wrappers: Enums, flix: Flix): HandlerRule = {
    val fparams = r.fparams.map(visitFparam)
    val exo = visitExp(r.exp)
    HandlerRule(r.op, fparams, exo)
  }

  private def visitJvmMethod(m: JvmMethod)(implicit wrappers: Enums, flix: Flix): JvmMethod = {
    val fparams = m.fparams.map(visitFparam)
    val exp = visitExp(m.exp)
    val retTpe = visitType(m.retTpe)
    JvmMethod(m.ident, fparams, exp, retTpe, m.eff, m.loc)
  }

  private def visitPat(pat0: Pattern)(implicit wrappers: Enums, flix: Flix): Pattern = pat0 match {
    case Pattern.Wild(tpe, loc) =>
      val t = visitType(tpe)
      Pattern.Wild(t, loc)
    case Pattern.Var(sym, tpe, loc) =>
      val t = visitType(tpe)
      Pattern.Var(sym, t, loc)
    case Pattern.Cst(cst, tpe, loc) =>
      Pattern.Cst(cst, tpe, loc)
    case Pattern.Tag(sym, pats, _, loc) if wrappers.contains(sym.sym.enumSym) => pats match {
      case List(pat) => visitPat(pat)
      case _ => throw InternalCompilerException(s"Unexpected pattern '$pat0'", loc)
    }
    case Pattern.Tag(sym, pats, tpe, loc) =>
      val ps = pats.map(visitPat)
      val t = visitType(tpe)
      Pattern.Tag(sym, ps, t, loc)
    case Pattern.Tuple(pats, tpe, loc) =>
      val ps = pats.map(visitPat)
      val t = visitType(tpe)
      Pattern.Tuple(ps, t, loc)
    case Pattern.Record(pats, pat, tpe, loc) =>
      val ps = pats.map(visitRecordLabelPattern)
      val p = visitPat(pat)
      val t = visitType(tpe)
      Pattern.Record(ps, p, t, loc)
  }

  private def visitRecordLabelPattern(rpat: Pattern.Record.RecordLabelPattern)(implicit wrappers: Enums, flix: Flix): Pattern.Record.RecordLabelPattern = {
    val pat = visitPat(rpat.pat)
    val tpe = visitType(rpat.tpe)
    Pattern.Record.RecordLabelPattern(rpat.label, pat, tpe, rpat.loc)
  }

}

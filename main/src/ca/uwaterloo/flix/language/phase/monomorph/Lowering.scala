/*
 * Copyright 2021 Magnus Madsen
 *           2025 Casper Dalgaard Nielsen
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

package ca.uwaterloo.flix.language.phase.monomorph

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.MonoAst.{DefContext, Occur}
import ca.uwaterloo.flix.language.ast.Type.eraseAliases
import ca.uwaterloo.flix.language.ast.shared.{BoundBy, Constant, Scope}
import ca.uwaterloo.flix.language.ast.{AtomicOp, LoweredAst, MonoAst, SourceLocation, Symbol, Type, TypeConstructor}
import ca.uwaterloo.flix.language.phase.monomorph.Specialization.Context
import ca.uwaterloo.flix.language.phase.monomorph.Symbols.{Defs, Enums, Types}
import ca.uwaterloo.flix.util.InternalCompilerException
import ca.uwaterloo.flix.util.collection.{ListOps, Nel}

/**
  * This phase translates AST expressions related to the Datalog subset of the
  * language into `Fixpoint.Ast.Datalog` values (which are ordinary Flix values).
  * This allows the Datalog engine to be implemented as an ordinary Flix program.
  *
  * In addition to translating expressions, types must also be translated from
  * Schema types to enum types.
  *
  * Finally, values must be boxed using the Boxable.
  *
  * It also translates channels to the lowered types.
  */
object Lowering {

  /**
    * Lowers the type `t`. Currently implemented as a no-op.
    *
    * When implemented:
    * Replaces schema types with the Datalog enum type and channel-related types with the channel enum type.
    *
    * @param tpe0 the type to be lowered.
    * @return
    */
  private def lowerType(tpe0: Type): Type = tpe0 match {
    case Type.Cst(_, _) => tpe0 // Performance: Reuse tpe0.

    case Type.Var(_, _) => tpe0

    // Rewrite Sender[t] to Concurrent.Channel.Mpmc[t, IO]
    case Type.Apply(Type.Cst(TypeConstructor.Sender, loc), tpe, _) =>
      val t = lowerType(tpe)
      mkChannelTpe(t, loc)

    // Rewrite Receiver[t] to Concurrent.Channel.Mpmc[t, IO]
    case Type.Apply(Type.Cst(TypeConstructor.Receiver, loc), tpe, _) =>
      val t = lowerType(tpe)
      mkChannelTpe(t, loc)

    case Type.Apply(tpe1, tpe2, loc) =>
      val t1 = lowerType(tpe1)
      val t2 = lowerType(tpe2)
      // Performance: Reuse tpe0, if possible.
      if ((t1 eq tpe1) && (t2 eq tpe2)) {
        tpe0
      } else {
        Type.Apply(t1, t2, loc)
      }

    case Type.Alias(sym, args, t, loc) =>
      Type.Alias(sym, args.map(lowerType), lowerType(t), loc)

    case Type.AssocType(_, _, _, loc) => throw InternalCompilerException("unexpected associated type", loc)

    case Type.JvmToType(_, loc) => throw InternalCompilerException("unexpected JVM type", loc)

    case Type.JvmToEff(_, loc) => throw InternalCompilerException("unexpected JVM eff", loc)

    case Type.UnresolvedJvmType(_, loc) => throw InternalCompilerException("unexpected JVM type", loc)

  }

  /**
    * Lowers the given def `defn`.
    */
  protected[monomorph] def lowerDef(defn: LoweredAst.Def)(implicit ctx: Context, root: LoweredAst.Root, flix: Flix): MonoAst.Def = defn match {
    case LoweredAst.Def(sym, spec0, exp, loc) =>
      val spec = lowerSpec(spec0)
      val e = lowerExp(exp)
      MonoAst.Def(sym, spec, e, loc)
  }

  /**
    * Lowers the given enum `enum0`.
    */
  protected[monomorph] def lowerEnum(enum0: LoweredAst.Enum): MonoAst.Enum = enum0 match {
    case LoweredAst.Enum(doc, ann, mod, sym, tparams0, _, cases0, loc) =>
      val tparams = tparams0.map(lowerTypeParam)
      val cases = cases0.map {
        case (_, LoweredAst.Case(caseSym, tpes0, _, caseLoc)) =>
          val tpes = tpes0.map(lowerType)
          (caseSym, MonoAst.Case(caseSym, tpes, caseLoc))
      }
      MonoAst.Enum(doc, ann, mod, sym, tparams, cases, loc)
  }

  /**
    * Lowers the given `effect`.
    */
  protected[monomorph] def lowerEffect(effect: LoweredAst.Effect): MonoAst.Effect = effect match {
    case LoweredAst.Effect(doc, ann, mod, sym, ops0, loc) =>
      // TODO EFFECT-TPARAMS use tparams
      val ops = ops0.map(lowerOp)
      MonoAst.Effect(doc, ann, mod, sym, ops, loc)
  }

  /**
    * Lowers the given struct `struct0`.
    */
  protected[monomorph] def lowerStruct(struct0: LoweredAst.Struct): MonoAst.Struct = struct0 match {
    case LoweredAst.Struct(doc, ann, mod, sym, tparams0, fields0, loc0) =>
      val tparams = tparams0.map(lowerTypeParam)
      val fields = fields0.map {
        case LoweredAst.StructField(fieldSym, tpe, loc) => MonoAst.StructField(fieldSym, lowerType(tpe), loc)
      }
      MonoAst.Struct(doc, ann, mod, sym, tparams, fields, loc0)
  }

  /**
    * Lowers the given `op`.
    */
  private def lowerOp(op: LoweredAst.Op): MonoAst.Op = op match {
    case LoweredAst.Op(sym, spec0, loc) =>
      val spec = lowerSpec(spec0)
      MonoAst.Op(sym, spec, loc)
  }

  /**
    * Lowers the given `spec0`.
    */
  private def lowerSpec(spec0: LoweredAst.Spec): MonoAst.Spec = spec0 match {
    case LoweredAst.Spec(doc, ann, mod, _, fparams0, declaredScheme, retTpe, eff, _) =>
      val fs = fparams0.map(lowerFormalParam)
      val fType = lowerType(declaredScheme.base)
      val rType = lowerType(retTpe)
      MonoAst.Spec(doc, ann, mod, fs, fType, rType, eff, DefContext.Unknown)
  }

  /**
    * Lowers `exp0` replacing all types with the lowered types and lowering channels and fixpoint to the primitives.
    */
  private def lowerExp(exp0: LoweredAst.Expr)(implicit ctx: Context, root: LoweredAst.Root, flix: Flix): MonoAst.Expr = exp0 match {
    case LoweredAst.Expr.Cst(cst, tpe, loc) => MonoAst.Expr.Cst(cst, lowerType(tpe), loc)
    case LoweredAst.Expr.Var(sym, tpe, loc) => MonoAst.Expr.Var(sym, lowerType(tpe), loc)
    case LoweredAst.Expr.Lambda(fparam, exp, tpe, loc) =>
      val p = lowerFormalParam(fparam)
      val e = lowerExp(exp)
      val t = lowerType(tpe)
      MonoAst.Expr.Lambda(p, e, t, loc)

    case LoweredAst.Expr.ApplyAtomic(op, exps, tpe, eff, loc) =>
      val es = exps.map(lowerExp)
      val t = lowerType(tpe)
      MonoAst.Expr.ApplyAtomic(op, es, t, eff, loc)

    case LoweredAst.Expr.ApplyClo(exp1, exp2, tpe, eff, loc) =>
      val e1 = lowerExp(exp1)
      val e2 = lowerExp(exp2)
      val t = lowerType(tpe)
      MonoAst.Expr.ApplyClo(e1, e2, t, eff, loc)

    case LoweredAst.Expr.ApplyDef(sym, exps, _, itpe, tpe, eff, loc) =>
      val es = exps.map(lowerExp)
      val it = lowerType(itpe)
      val t = lowerType(tpe)
      MonoAst.Expr.ApplyDef(sym, es, it, t, eff, loc)

    case LoweredAst.Expr.ApplyLocalDef(sym, exps, tpe, eff, loc) =>
      val es = exps.map(lowerExp)
      val t = lowerType(tpe)
      MonoAst.Expr.ApplyLocalDef(sym, es, t, eff, loc)

    case LoweredAst.Expr.ApplyOp(sym, exps, tpe, eff, loc) =>
      val es = exps.map(lowerExp)
      val t = lowerType(tpe)
      MonoAst.Expr.ApplyOp(sym, es, t, eff, loc)

    case LoweredAst.Expr.Let(sym, exp1, exp2, tpe, eff, loc) =>
      val e1 = lowerExp(exp1)
      val e2 = lowerExp(exp2)
      val t = lowerType(tpe)
      MonoAst.Expr.Let(sym, e1, e2, t, eff, Occur.Unknown, loc)

    case LoweredAst.Expr.LocalDef(sym, fparams, exp1, exp2, tpe, eff, loc) =>
      val fps = fparams.map(lowerFormalParam)
      val e1 = lowerExp(exp1)
      val e2 = lowerExp(exp2)
      val t = lowerType(tpe)
      MonoAst.Expr.LocalDef(sym, fps, e1, e2, t, eff, Occur.Unknown, loc)

    case LoweredAst.Expr.Region(sym, regSym, exp, tpe, eff, loc) =>
      val e = lowerExp(exp)
      val t = lowerType(tpe)
      MonoAst.Expr.Region(sym, regSym, e, t, eff, loc)

    case LoweredAst.Expr.IfThenElse(exp1, exp2, exp3, tpe, eff, loc) =>
      val e1 = lowerExp(exp1)
      val e2 = lowerExp(exp2)
      val e3 = lowerExp(exp3)
      val t = lowerType(tpe)
      MonoAst.Expr.IfThenElse(e1, e2, e3, t, eff, loc)

    case LoweredAst.Expr.Stm(exp1, exp2, tpe, eff, loc) =>
      val e1 = lowerExp(exp1)
      val e2 = lowerExp(exp2)
      val t = lowerType(tpe)
      MonoAst.Expr.Stm(e1, e2, t, eff, loc)

    case LoweredAst.Expr.Discard(exp, eff, loc) =>
      val e = lowerExp(exp)
      MonoAst.Expr.Discard(e, eff, loc)

    case LoweredAst.Expr.Match(exp, rules, tpe, eff, loc) =>
      val e = lowerExp(exp)
      val t = lowerType(tpe)
      val rs = rules.map(visitMatchRule)
      MonoAst.Expr.Match(e, rs, t, eff, loc)

    case LoweredAst.Expr.ExtMatch(exp, rules, tpe, eff, loc) =>
      val e = lowerExp(exp)
      val rs = rules.map(lowerExtMatch)
      val t = lowerType(tpe)
      MonoAst.Expr.ExtMatch(e, rs, t, eff, loc)

    case LoweredAst.Expr.VectorLit(exps, tpe, eff, loc) =>
      val es = exps.map(lowerExp)
      val t = lowerType(tpe)
      MonoAst.Expr.VectorLit(es, t, eff, loc)

    case LoweredAst.Expr.VectorLoad(exp1, exp2, tpe, eff, loc) =>
      val e1 = lowerExp(exp1)
      val e2 = lowerExp(exp2)
      val t = lowerType(tpe)
      MonoAst.Expr.VectorLoad(e1, e2, t, eff, loc)

    case LoweredAst.Expr.VectorLength(exp, loc) => MonoAst.Expr.VectorLength(lowerExp(exp), loc)

    case LoweredAst.Expr.Cast(exp, _, _, tpe, eff, loc) =>
      // Drop the declaredType and declaredEff.
      val e = lowerExp(exp)
      val t = lowerType(tpe)
      MonoAst.Expr.Cast(e, t, eff, loc)

    case LoweredAst.Expr.TryCatch(exp, rules, tpe, eff, loc) =>
      val e = lowerExp(exp)
      val t = lowerType(tpe)
      val rs = rules.map(lowerCatchRule)
      MonoAst.Expr.TryCatch(e, rs, t, eff, loc)

    case LoweredAst.Expr.RunWith(exp, effUse, rules, tpe, eff, loc) =>
      val e = lowerExp(exp)
      val t = lowerType(tpe)
      val rs = rules.map(lowerHandlerRule)
      MonoAst.Expr.RunWith(e, effUse, rs, t, eff, loc)

    case LoweredAst.Expr.NewObject(name, clazz, tpe, eff, methods, loc) =>
      val ms = methods.map(lowerJvmMethod)
      val t = lowerType(tpe)
      MonoAst.Expr.NewObject(name, clazz, t, eff, ms, loc)

    case LoweredAst.Expr.NewChannel(exp, tpe, eff, loc) =>
      val e = lowerExp(exp)
      lowerNewChannel(e, tpe, eff, loc)

    case LoweredAst.Expr.GetChannel(innerExp, tpe, eff, loc) =>
      val e = lowerExp(innerExp)
      val t = lowerType(tpe)
      mkGetChannel(e, t, eff, loc)

    case LoweredAst.Expr.PutChannel(innerExp1, innerExp2, _, eff, loc) =>
      val exp1 = lowerExp(innerExp1)
      val exp2 = lowerExp(innerExp2)
      Lowering.mkPutChannel(exp1, exp2, eff, loc)

    case LoweredAst.Expr.SelectChannel(rules0, default0, tpe, eff, loc) =>
      val rules = rules0.map {
        case LoweredAst.SelectChannelRule(sym, chan, exp, _) =>
          (sym, lowerExp(chan), lowerExp(exp))
      }
      val default = default0.map(lowerExp)
      val t = lowerType(tpe)
      Lowering.mkSelectChannel(rules, default, t, eff, loc)

    case LoweredAst.Expr.ParYield(frags, exp, tpe, eff, loc) =>
      val fs = frags.map {
        case LoweredAst.ParYieldFragment(pat, fragExp, fragLoc) =>
          val p = lowerPat(pat)
          (p, lowerExp(fragExp), fragLoc)
      }
      val e = lowerExp(exp)
      val t = lowerType(tpe)
      Lowering.mkParYield(fs, e, t, eff, loc)

    case LoweredAst.Expr.ApplySig(_, _, _, _, _, _, _, _) =>
      throw InternalCompilerException(s"Unexpected ApplySig", exp0.loc)

    case LoweredAst.Expr.Ascribe(_, _, _, _) =>
      throw InternalCompilerException(s"Unexpected Ascribe", exp0.loc)

    case LoweredAst.Expr.TypeMatch(_, _, _, _, _) =>
      throw InternalCompilerException(s"Unexpected TypeMatch", exp0.loc)

  }

  /**
    * Lowers the given JvmMethod `method`.
    */
  private def lowerJvmMethod(method: LoweredAst.JvmMethod)(implicit ctx: Context, root: LoweredAst.Root, flix: Flix): MonoAst.JvmMethod = method match {
    case LoweredAst.JvmMethod(ident, fparams, exp, retTyp, eff, loc) =>
      val fs = fparams.map(lowerFormalParam)
      val e = lowerExp(exp)
      val t = lowerType(retTyp)
      MonoAst.JvmMethod(ident, fs, e, t, eff, loc)
  }

  /**
    * Lowers the given catch rule `rule0`.
    */
  private def lowerCatchRule(rule: LoweredAst.CatchRule)(implicit ctx: Context, root: LoweredAst.Root, flix: Flix): MonoAst.CatchRule = rule match {
    case LoweredAst.CatchRule(sym, clazz, exp) =>
      val e = lowerExp(exp)
      MonoAst.CatchRule(sym, clazz, e)
  }

  /**
    * Lowers the given handler rule `rule0`.
    */
  private def lowerHandlerRule(rule0: LoweredAst.HandlerRule)(implicit ctx: Context, root: LoweredAst.Root, flix: Flix): MonoAst.HandlerRule = rule0 match {
    case LoweredAst.HandlerRule(opSymUse, fparams0, body0) =>
      val fparams = fparams0.map(lowerFormalParam)
      val body = lowerExp(body0)
      MonoAst.HandlerRule(opSymUse, fparams, body)
  }

  /**
    * Lowers the given match rule `rule0`.
    */
  private def visitMatchRule(rule0: LoweredAst.MatchRule)(implicit ctx: Context, root: LoweredAst.Root, flix: Flix): MonoAst.MatchRule = rule0 match {
    case LoweredAst.MatchRule(pat, guard, body) =>
      val p = lowerPat(pat)
      val g = guard.map(lowerExp)
      val b = lowerExp(body)
      MonoAst.MatchRule(p, g, b)
  }

  /**
    * Lowers the given pattern `pat0`.
    */
  private def lowerPat(pat0: LoweredAst.Pattern): MonoAst.Pattern = pat0 match {
    case LoweredAst.Pattern.Wild(tpe, loc) =>
      val t = lowerType(tpe)
      MonoAst.Pattern.Wild(t, loc)

    case LoweredAst.Pattern.Var(sym, tpe, loc) =>
      val t = lowerType(tpe)
      MonoAst.Pattern.Var(sym, t, Occur.Unknown, loc)

    case LoweredAst.Pattern.Cst(cst, tpe, loc) =>
      val t = lowerType(tpe)
      MonoAst.Pattern.Cst(cst, t, loc)

    case LoweredAst.Pattern.Tag(symUse, pats, tpe, loc) =>
      val ps = pats.map(lowerPat)
      val t = lowerType(tpe)
      MonoAst.Pattern.Tag(symUse, ps, t, loc)

    case LoweredAst.Pattern.Tuple(elms, tpe, loc) =>
      val es = elms.map(lowerPat)
      val t = lowerType(tpe)
      MonoAst.Pattern.Tuple(es, t, loc)

    case LoweredAst.Pattern.Record(pats, pat, tpe, loc) =>
      val patsVal = pats.map {
        case LoweredAst.Pattern.Record.RecordLabelPattern(label, pat1, tpe1, loc1) =>
          val p1 = lowerPat(pat1)
          val t1 = lowerType(tpe1)
          MonoAst.Pattern.Record.RecordLabelPattern(label, p1, t1, loc1)
      }
      val patVal = lowerPat(pat)
      val t = lowerType(tpe)
      MonoAst.Pattern.Record(patsVal, patVal, t, loc)
  }

  private def lowerExtPat(pat0: LoweredAst.ExtPattern): MonoAst.ExtPattern = pat0 match {
    case LoweredAst.ExtPattern.Default(loc) =>
      MonoAst.ExtPattern.Default(loc)

    case LoweredAst.ExtPattern.Tag(label, pats, loc) =>
      val ps = pats.map(lowerExtTagPat)
      MonoAst.ExtPattern.Tag(label, ps, loc)

  }

  private def lowerExtTagPat(pat0: LoweredAst.ExtTagPattern): MonoAst.ExtTagPattern = pat0 match {
    case LoweredAst.ExtTagPattern.Wild(tpe, loc) =>
      val t = lowerType(tpe)
      MonoAst.ExtTagPattern.Wild(t, loc)
    case LoweredAst.ExtTagPattern.Var(sym, tpe, loc) =>
      val t = lowerType(tpe)
      MonoAst.ExtTagPattern.Var(sym, t, Occur.Unknown, loc)
    case LoweredAst.ExtTagPattern.Unit(tpe, loc) =>
      val t = lowerType(tpe)
      MonoAst.ExtTagPattern.Unit(t, loc)
  }

  private def lowerExtMatch(rule0: LoweredAst.ExtMatchRule)(implicit ctx: Context, root: LoweredAst.Root, flix: Flix): MonoAst.ExtMatchRule = rule0 match {
    case LoweredAst.ExtMatchRule(pat, exp, loc) =>
      val p = lowerExtPat(pat)
      val e = lowerExp(exp)
      MonoAst.ExtMatchRule(p, e, loc)
  }

  private def lowerFormalParam(fparam: LoweredAst.FormalParam): MonoAst.FormalParam = fparam match {
    case LoweredAst.FormalParam(sym0, tpe, loc0) => MonoAst.FormalParam(sym0, lowerType(tpe), Occur.Unknown, loc0)
  }

  private def lowerTypeParam(tparam: LoweredAst.TypeParam): MonoAst.TypeParam = tparam match {
    case LoweredAst.TypeParam(name, sym, loc) =>
      MonoAst.TypeParam(name, sym, loc)
  }

  /**
    * Returns the definition associated with the given symbol `sym`.
    *
    * @param tpe must be specialized. Can be visited, if the underlying function can handle that
    */
  private def lookup(sym: Symbol.DefnSym, tpe: Type)(implicit ctx: Context, root: LoweredAst.Root, flix: Flix): Symbol.DefnSym =
    Specialization.specializeDefnSym(sym, tpe)

  /**
    * Make a new channel tuple (sender, receiver) expression
    *
    * New channel expressions are rewritten as follows:
    * {{{ %%CHANNEL_NEW%%(m) }}}
    * becomes a call to the standard library function:
    * {{{ Concurrent/Channel.newChannel(10) }}}
    *
    * @param tpe The specialized type of the result
    */
  private def lowerNewChannel(exp: MonoAst.Expr, tpe: Type, eff: Type, loc: SourceLocation)(implicit ctx: Context, root: LoweredAst.Root, flix: Flix): MonoAst.Expr = {
    val itpe = lowerType(Type.mkIoArrow(exp.tpe, tpe, loc))
    val defnSym = lookup(Defs.ChannelNewTuple, itpe)
    MonoAst.Expr.ApplyDef(defnSym, exp :: Nil, itpe, lowerType(tpe), eff, loc)
  }

  /**
    * Make a channel get expression
    *
    * Channel get expressions are rewritten as follows:
    * {{{ <- c }}}
    * becomes a call to the standard library function:
    * {{{ Concurrent/Channel.get(c) }}}
    */
  private def mkGetChannel(exp: MonoAst.Expr, tpe: Type, eff: Type, loc: SourceLocation)(implicit ctx: Context, root: LoweredAst.Root, flix: Flix): MonoAst.Expr = {
    val itpe = lowerType(Type.mkIoArrow(exp.tpe, tpe, loc))
    val defnSym = lookup(Defs.ChannelGet, itpe)
    MonoAst.Expr.ApplyDef(defnSym, exp :: Nil, itpe, lowerType(tpe), eff, loc)
  }

  /**
    * Make a channel put expression
    *
    * Channel put expressions are rewritten as follows:
    * {{{ c <- 42 }}}
    * becomes a call to the standard library function:
    * {{{ Concurrent/Channel.put(42, c) }}}
    */
  private def mkPutChannel(exp1: MonoAst.Expr, exp2: MonoAst.Expr, eff: Type, loc: SourceLocation)(implicit ctx: Context, root: LoweredAst.Root, flix: Flix): MonoAst.Expr = {
    val itpe = lowerType(Type.mkIoUncurriedArrow(List(exp2.tpe, exp1.tpe), Type.Unit, loc))
    val defnSym = lookup(Defs.ChannelPut, itpe)
    MonoAst.Expr.ApplyDef(defnSym, List(exp2, exp1), itpe, Type.Unit, eff, loc)
  }

  /**
    * Make a channel select expression
    *
    * Channel select expressions are rewritten as follows:
    * {{{
    *  select {
    *    case x <- ?ch1 => ?handlech1
    *    case y <- ?ch2 => ?handlech2
    *    case _ => ?default
    *  }
    * }}}
    * becomes
    * {{{
    *   let ch1 = ?ch1;
    *   let ch2 = ?ch2;
    *   match selectFrom(mpmcAdmin(ch1) :: mpmcAdmin(ch2) :: Nil, false) {  // true if no default
    *     case (0, locks) =>
    *       let x = unsafeGetAndUnlock(ch1, locks);
    *       ?handlech1
    *     case (1, locks) =>
    *       let y = unsafeGetAndUnlock(ch2, locks);
    *       ?handlech2
    *     case (-1, _) =>                                                  // Omitted if no default
    *      ?default                                                   // Unlock is handled by selectFrom
    * }}}
    * Note: match is not exhaustive: we're relying on the simplifier to handle this for us
    */
  private def mkSelectChannel(rules: List[(Symbol.VarSym, MonoAst.Expr, MonoAst.Expr)], default: Option[MonoAst.Expr], tpe: Type, eff: Type, loc: SourceLocation)(implicit ctx: Context, root: LoweredAst.Root, flix: Flix): MonoAst.Expr = {
    val t = lowerType(tpe)

    val channels = rules.map { case (_, c, _) => (mkLetSym("chan", loc), c) }
    val admins = mkChannelAdminList(rules, channels, loc)
    val selectExp = mkChannelSelect(admins, default, loc)
    val cases = mkChannelCases(rules, channels, eff, loc)
    val defaultCase = mkSelectDefaultCase(default, loc)
    val matchExp = MonoAst.Expr.Match(selectExp, cases ++ defaultCase, t, eff, loc)

    channels.foldRight[MonoAst.Expr](matchExp) {
      case ((sym, c), e) => MonoAst.Expr.Let(sym, c, e, t, eff, Occur.Unknown, loc)
    }
  }


  /**
    * Make the list of MpmcAdmin objects which will be passed to `selectFrom`.
    *
    * For each case like
    * {{{ x <- ?ch1 => ?handlech1 }}}
    * we generate
    * {{{ mpmcAdmin(x) }}}
    */
  private def mkChannelAdminList(rs: List[(Symbol.VarSym, MonoAst.Expr, MonoAst.Expr)], channels: List[(Symbol.VarSym, MonoAst.Expr)], loc: SourceLocation)(implicit ctx: Context, root: LoweredAst.Root, flix: Flix): MonoAst.Expr = {
    val admins = ListOps.zip(rs, channels) map {
      case ((_, c, _), (chanSym, _)) =>
        val itpe = lowerType(Type.mkPureArrow(c.tpe, Types.ChannelMpmcAdmin, loc))
        val defnSym = lookup(Defs.ChannelMpmcAdmin, itpe)
        MonoAst.Expr.ApplyDef(defnSym, List(MonoAst.Expr.Var(chanSym, lowerType(c.tpe), loc)), itpe, Types.ChannelMpmcAdmin, Type.Pure, loc)
    }
    mkList(admins, Types.ChannelMpmcAdmin, loc)
  }

  /**
    * Construct a call to `selectFrom` given a list of MpmcAdmin objects and optional default.
    *
    * Transforms
    * {{{ mpmcAdmin(ch1), mpmcAdmin(ch1), ... }}}
    * Into
    * {{{ selectFrom(mpmcAdmin(ch1) :: mpmcAdmin(ch2) :: ... :: Nil, false) }}}
    *
    * When `default` is `Some` the second parameter will be `true`.
    */
  private def mkChannelSelect(admins: MonoAst.Expr, default: Option[MonoAst.Expr], loc: SourceLocation)(implicit ctx: Context, root: LoweredAst.Root, flix: Flix): MonoAst.Expr = {
    val locksType = Types.mkList(Types.ConcurrentReentrantLock, loc)

    val selectRetTpe = Type.mkTuple(List(Type.Int32, locksType), loc)
    val itpe = Type.mkIoUncurriedArrow(List(admins.tpe, Type.Bool), selectRetTpe, loc)
    val blocking = default match {
      case Some(_) => MonoAst.Expr.Cst(Constant.Bool(false), Type.Bool, loc)
      case None => MonoAst.Expr.Cst(Constant.Bool(true), Type.Bool, loc)
    }
    val defnSym = lookup(Defs.ChannelSelectFrom, itpe)
    MonoAst.Expr.ApplyDef(defnSym, List(admins, blocking), lowerType(itpe), selectRetTpe, Type.IO, loc)
  }

  /**
    * Construct a sequence of MatchRules corresponding to the given SelectChannelRules
    *
    * Transforms the `i`'th
    * {{{ case x <- ?ch1 => ?handlech1 }}}
    * into
    * {{{
    * case (i, locks) =>
    *   let x = unsafeGetAndUnlock(ch1, locks);
    *   ?handlech1
    * }}}
    */
  private def mkChannelCases(rs: List[(Symbol.VarSym, MonoAst.Expr, MonoAst.Expr)], channels: List[(Symbol.VarSym, MonoAst.Expr)], eff: Type, loc: SourceLocation)(implicit ctx: Context, root: LoweredAst.Root, flix: Flix): List[MonoAst.MatchRule] = {
    val locksType = Types.mkList(Types.ConcurrentReentrantLock, loc)

    ListOps.zip(rs, channels).zipWithIndex map {
      case (((sym, chan, exp), (chSym, _)), i) =>
        val locksSym = mkLetSym("locks", loc)
        val pat = mkTuplePattern(Nel(MonoAst.Pattern.Cst(Constant.Int32(i), Type.Int32, loc), List(MonoAst.Pattern.Var(locksSym, locksType, Occur.Unknown, loc))), loc)
        val getTpe = extractChannelTpe(chan.tpe)
        val itpe = lowerType(Type.mkIoUncurriedArrow(List(chan.tpe, locksType), getTpe, loc))
        val args = List(MonoAst.Expr.Var(chSym, lowerType(chan.tpe), loc), MonoAst.Expr.Var(locksSym, locksType, loc))
        val defnSym = lookup(Defs.ChannelUnsafeGetAndUnlock, itpe)
        val getExp = MonoAst.Expr.ApplyDef(defnSym, args, lowerType(itpe), lowerType(getTpe), eff, loc)
        val e = MonoAst.Expr.Let(sym, getExp, exp, lowerType(exp.tpe), eff, Occur.Unknown, loc)
        MonoAst.MatchRule(pat, None, e)
    }
  }

  /**
    * Construct additional MatchRule to handle the (optional) default case
    * NB: Does not need to unlock because that is handled inside Concurrent/Channel.selectFrom.
    *
    * If `default` is `None` returns an empty list. Otherwise produces
    * {{{ case (-1, _) => ?default }}}
    */
  private def mkSelectDefaultCase(default: Option[MonoAst.Expr], loc: SourceLocation): List[MonoAst.MatchRule] = {
    default match {
      case Some(defaultExp) =>
        val locksType = Types.mkList(Types.ConcurrentReentrantLock, loc)
        val pat = mkTuplePattern(Nel(MonoAst.Pattern.Cst(Constant.Int32(-1), Type.Int32, loc), List(MonoAst.Pattern.Wild(locksType, loc))), loc)
        val defaultMatch = MonoAst.MatchRule(pat, None, defaultExp)
        List(defaultMatch)
      case _ =>
        List()
    }
  }

  /**
    * Returns a desugared [[LoweredAst.Expr.ParYield]] expression as a nested match-expression.
    */
  private def mkParYield(frags: List[(MonoAst.Pattern, MonoAst.Expr, SourceLocation)], exp: MonoAst.Expr, tpe: Type, eff: Type, loc: SourceLocation)(implicit flix: Flix, ctx: Context, root: LoweredAst.Root): MonoAst.Expr = {
    // Only generate channels for n-1 fragments. We use the current thread for the last fragment.
    val fs = frags.init
    val last = frags.last

    // Generate symbols for each channel.
    val chanSymsWithPatAndExp = fs.map { case (p, e, l) => (p, mkLetSym("channel", l.asSynthetic), e) }

    // Make `GetChannel` exps for the spawnable exps.
    val waitExps = mkBoundParWaits(chanSymsWithPatAndExp, exp)

    // Evaluate the last expression in the current thread (so just make let-binding)
    val desugaredYieldExp = mkLetMatch(last._1, last._2, waitExps)

    // Generate channels and spawn exps.
    val chanSymsWithExp = chanSymsWithPatAndExp.map { case (_, s, e) => (s, e) }
    val blockExp = mkParChannels(desugaredYieldExp, chanSymsWithExp)

    // Wrap everything in a purity cast,
    MonoAst.Expr.Cast(blockExp, lowerType(tpe), eff, loc.asSynthetic)
  }

  /**
    * Returns a full `par yield` expression.
    */
  private def mkParChannels(exp: MonoAst.Expr, chanSymsWithExps: List[(Symbol.VarSym, MonoAst.Expr)])(implicit ctx: Context, root: LoweredAst.Root, flix: Flix): MonoAst.Expr = {
    // Make spawn expressions `spawn ch <- exp`.
    val spawns = chanSymsWithExps.foldRight(exp: MonoAst.Expr) {
      case ((sym, e), acc) =>
        val loc = e.loc.asSynthetic
        val e1 = mkChannelExp(sym, e.tpe, loc) // The channel `ch`
        val e2 = mkPutChannel(e1, e, Type.IO, loc) // The put exp: `ch <- exp0`.
        val e3 = MonoAst.Expr.Cst(Constant.Static, Type.mkRegionToStar(Type.IO, loc), loc)
        val e4 = MonoAst.Expr.ApplyAtomic(AtomicOp.Spawn, List(e2, e3), Type.Unit, Type.IO, loc) // Spawn the put expression from above i.e. `spawn ch <- exp0`.
        MonoAst.Expr.Stm(e4, acc, acc.tpe, Type.mkUnion(e4.eff, acc.eff, loc), loc) // Return a statement expression containing the other spawn expressions along with this one.
    }

    // Make let bindings `let ch = chan 1;`.
    chanSymsWithExps.foldRight(spawns: MonoAst.Expr) {
      case ((sym, e), acc) =>
        val loc = e.loc.asSynthetic
        val chan = mkNewChannel(MonoAst.Expr.Cst(Constant.Int32(1), Type.Int32, loc), mkChannelTpe(e.tpe, loc), Type.IO, loc) // The channel exp `chan 1`
        MonoAst.Expr.Let(sym, chan, acc, acc.tpe, Type.mkUnion(e.eff, acc.eff, loc), Occur.Unknown, loc) // The let-binding `let ch = chan 1`
    }
  }

  /**
    * Make a new channel expression
    */
  private def mkNewChannel(exp: MonoAst.Expr, tpe: Type, eff: Type, loc: SourceLocation)(implicit ctx: Context, root: LoweredAst.Root, flix: Flix): MonoAst.Expr = {
    val itpe = lowerType(Type.mkIoArrow(exp.tpe, tpe, loc))
    val defnSym = lookup(Defs.ChannelNew, itpe)
    MonoAst.Expr.ApplyDef(defnSym, exp :: Nil, itpe, tpe, eff, loc)
  }

  /**
    * Returns an expression where the pattern variables used in `exp` are
    * bound to [[LoweredAst.Expr.GetChannel]] expressions,
    * i.e.
    * {{{
    *   let pat1 = <- ch1;
    *   let pat2 = <- ch2;
    *   let pat3 = <- ch3;
    *   ...
    *   let patn = <- chn;
    *   exp
    * }}}
    */
  private def mkBoundParWaits(patSymExps: List[(MonoAst.Pattern, Symbol.VarSym, MonoAst.Expr)], exp: MonoAst.Expr)(implicit ctx: Context, root: LoweredAst.Root, flix: Flix): MonoAst.Expr =
    patSymExps.map {
      case (p, sym, e) =>
        val loc = e.loc.asSynthetic
        val chExp = mkChannelExp(sym, e.tpe, loc)
        (p, mkGetChannel(chExp, e.tpe, Type.IO, loc))
    }.foldRight(exp) {
      case ((pat, chan), e) => mkLetMatch(pat, chan, e)
    }

  /**
    * Returns a desugared let-match expression, i.e.
    * {{{
    *   let pattern = exp;
    *   body
    * }}}
    * is desugared to
    * {{{
    *   match exp {
    *     case pattern => body
    *   }
    * }}}
    */
  private def mkLetMatch(pat: MonoAst.Pattern, exp: MonoAst.Expr, body: MonoAst.Expr): MonoAst.Expr = {
    val loc = exp.loc.asSynthetic
    val rule = List(MonoAst.MatchRule(pat, None, body))
    val eff = Type.mkUnion(exp.eff, body.eff, loc)
    MonoAst.Expr.Match(exp, rule, body.tpe, eff, loc)
  }

  /**
    * An expression for a channel variable called `sym`
    */
  private def mkChannelExp(sym: Symbol.VarSym, tpe: Type, loc: SourceLocation): MonoAst.Expr = {
    MonoAst.Expr.Var(sym, mkChannelTpe(tpe, loc), loc)
  }

  /**
    * Returns a list expression constructed from the given `exps` with type list of `elmType`.
    *
    * @param elmType is assumed to be specialized and lowered.
    */
  private def mkList(exps: List[MonoAst.Expr], elmType: Type, loc: SourceLocation): MonoAst.Expr = {
    val nil = mkNil(elmType, loc)
    exps.foldRight(nil) {
      case (e, acc) => mkCons(e, acc, loc)
    }
  }

  /**
    * Returns a `Nil` expression with type list of `elmType`.
    *
    * @param elmType is assumed to be specialized and lowered.
    */
  private def mkNil(elmType: Type, loc: SourceLocation): MonoAst.Expr = {
    mkTag(Enums.FList, "Nil", Nil, Types.mkList(elmType, loc), loc)
  }

  /**
    * returns a `Cons(hd, tail)` expression with type `tail.tpe`.
    */
  private def mkCons(hd: MonoAst.Expr, tail: MonoAst.Expr, loc: SourceLocation): MonoAst.Expr = {
    mkTag(Enums.FList, "Cons", List(hd, tail), lowerType(tail.tpe), loc)
  }

  /**
    * Returns a pure tag expression for the given `sym` and given `tag` with the given inner expression `exp`.
    *
    * @param tpe is assumed to be specialized and lowered.
    */
  private def mkTag(sym: Symbol.EnumSym, tag: String, exps: List[MonoAst.Expr], tpe: Type, loc: SourceLocation): MonoAst.Expr = {
    val caseSym = new Symbol.CaseSym(sym, tag, loc.asSynthetic)
    MonoAst.Expr.ApplyAtomic(AtomicOp.Tag(caseSym), exps, tpe, Type.Pure, loc)
  }

  /**
    * Returns `(t1, t2)` where `tpe = Concurrent.Channel.Mpmc[t1, t2]`.
    *
    * @param tpe is assumed to be specialized, but not lowered.
    */
  private def extractChannelTpe(tpe: Type): Type = eraseAliases(tpe) match {
    case Type.Apply(Type.Apply(Types.ChannelMpmc, elmType, _), _, _) => elmType
    case _ => throw InternalCompilerException(s"Cannot interpret '$tpe' as a channel type", tpe.loc)
  }

  /**
    * Returns a TypedAst.Pattern representing a tuple of patterns.
    *
    * @param patterns are assumed to contain specialized and lowered types.
    */
  private def mkTuplePattern(patterns: Nel[MonoAst.Pattern], loc: SourceLocation): MonoAst.Pattern = {
    MonoAst.Pattern.Tuple(patterns, Type.mkTuple(patterns.map(_.tpe), loc), loc)
  }

  /**
    * Returns a new `VarSym` for use in a let-binding.
    *
    * This function is called `mkLetSym` to avoid confusion with [[mkVarSym]].
    */
  private def mkLetSym(prefix: String, loc: SourceLocation)(implicit flix: Flix): Symbol.VarSym = {
    val name = prefix + Flix.Delimiter + flix.genSym.freshId()
    Symbol.freshVarSym(name, BoundBy.Let, loc)(Scope.Top, flix)
  }

  /**
    * The type of a channel which can transmit variables of type `tpe`.
    */
  private def mkChannelTpe(tpe: Type, loc: SourceLocation): Type = {
    mkChannelTpe(tpe, Type.IO, loc)
  }

  /**
    * The type of a channel which can transmit variables of type `tpe1` in region `tpe2`.
    */
  private def mkChannelTpe(tpe1: Type, tpe2: Type, loc: SourceLocation): Type = {
    Type.Apply(Type.Apply(Types.ChannelMpmc, tpe1, loc), tpe2, loc)
  }

}

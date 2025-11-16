/*
 * Copyright 2021 Magnus Madsen
 *                Casper Dalgaard Nielsen
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
import ca.uwaterloo.flix.language.ast.MonoAst.Occur
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
   def lowerType(tpe0: Type): Type = tpe0 match {
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
  protected[monomorph] def visitNewChannel(exp: MonoAst.Expr, tpe: Type, eff: Type, loc: SourceLocation)(implicit ctx: Context, root: LoweredAst.Root, flix: Flix): MonoAst.Expr = {
    val itpe = Type.mkIoArrow(exp.tpe, tpe, loc)
    val defnSym = lookup(Defs.ChannelNewTuple, itpe)
    MonoAst.Expr.ApplyDef(defnSym, exp :: Nil, lowerType(itpe), lowerType(tpe), eff, loc)
  }

  /**
    * Make a channel get expression
    *
    * Channel get expressions are rewritten as follows:
    * {{{ <- c }}}
    * becomes a call to the standard library function:
    * {{{ Concurrent/Channel.get(c) }}}
    */
  protected[monomorph] def mkGetChannel(exp: MonoAst.Expr, tpe: Type, eff: Type, loc: SourceLocation)(implicit ctx: Context, root: LoweredAst.Root, flix: Flix): MonoAst.Expr = {
    val itpe = Type.mkIoArrow(exp.tpe, tpe, loc)
    val defnSym = lookup(Defs.ChannelGet, itpe)
    MonoAst.Expr.ApplyDef(defnSym, exp :: Nil, lowerType(itpe), lowerType(tpe), eff, loc)
  }

  /**
    * Make a channel put expression
    *
    * Channel put expressions are rewritten as follows:
    * {{{ c <- 42 }}}
    * becomes a call to the standard library function:
    * {{{ Concurrent/Channel.put(42, c) }}}
    */
  protected[monomorph] def mkPutChannel(exp1: MonoAst.Expr, exp2: MonoAst.Expr, eff: Type, loc: SourceLocation)(implicit ctx: Context, root: LoweredAst.Root, flix: Flix): MonoAst.Expr = {
    val itpe = Type.mkIoUncurriedArrow(List(exp2.tpe, exp1.tpe), Type.Unit, loc)
    val defnSym = lookup(Defs.ChannelPut, itpe)
    MonoAst.Expr.ApplyDef(defnSym, List(exp2, exp1), lowerType(itpe), Type.Unit, eff, loc)
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
  protected[monomorph] def mkSelectChannel(rules: List[(Symbol.VarSym, MonoAst.Expr, MonoAst.Expr)], default: Option[MonoAst.Expr], tpe: Type, eff: Type, loc: SourceLocation)(implicit ctx: Context, root: LoweredAst.Root, flix: Flix): MonoAst.Expr = {
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
        val itpe = Type.mkPureArrow(c.tpe, Types.ChannelMpmcAdmin, loc)
        val defnSym = lookup(Defs.ChannelMpmcAdmin, itpe)
        MonoAst.Expr.ApplyDef(defnSym, List(MonoAst.Expr.Var(chanSym, lowerType(c.tpe), loc)), lowerType(itpe), Types.ChannelMpmcAdmin, Type.Pure, loc)
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
        val itpe = Type.mkIoUncurriedArrow(List(chan.tpe, locksType), getTpe, loc)
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

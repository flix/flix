/*
 * Copyright 2020 Magnus Madsen
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
package ca.uwaterloo.flix.api.lsp

import ca.uwaterloo.flix.api.lsp.Index.traverse
import ca.uwaterloo.flix.language.ast.TypedAst.Predicate.{Body, Head}
import ca.uwaterloo.flix.language.ast.TypedAst._
import ca.uwaterloo.flix.language.ast._
import ca.uwaterloo.flix.util.InternalCompilerException

object Indexer {

  /**
    * Returns a reverse index for the given AST `root`.
    */
  def visitRoot(root: Root): Index = {
    val idx1 = traverse(root.defs.values)(visitDef)
    val idx2 = traverse(root.enums.values)(visitEnum)
    val idx3 = traverse(root.classes.values)(visitClass)
    val idx4 = traverse(root.instances.values) {
      instances => traverse(instances)(visitInstance)
    }
    val idx5 = traverse(root.sigs.values)(visitSig)
    val idx6 = traverse(root.effects.values)(visitEff)
    idx1 ++ idx2 ++ idx3 ++ idx4 ++ idx5 ++ idx6
  }

  /**
    * Returns a reverse index for the given definition `def0`.
    */
  private def visitDef(def0: Def): Index = def0 match {
    case Def(_, spec, impl) =>
      val idx0 = Index.occurrenceOf(def0)
      val idx1 = visitSpec(spec)
      val idx2 = visitImpl(impl)
      idx0 ++ idx1 ++ idx2
  }

  /**
    * Returns a reverse index for the given signature `sig0`.
    */
  private def visitSig(sig0: Sig): Index = sig0 match {
    case Sig(_, spec, impl) =>
      val idx0 = Index.occurrenceOf(sig0)
      val idx1 = visitSpec(spec)
      val idx2 = traverse(impl)(visitImpl)
      idx0 ++ idx1 ++ idx2
  }

  /**
    * Returns a reverse index for the given `impl`.
    */
  private def visitImpl(impl: Impl): Index = impl match {
    case Impl(exp, _) => visitExp(exp)
  }

  /**
    * Returns a reverse index for the given `spec`.
    */
  private def visitSpec(spec: Spec): Index = spec match {
    case Spec(_, _, _, tparams, fparams, declaredScheme, retTpe, pur, eff, _) =>
      val idx1 = traverse(tparams)(visitTypeParam)
      val idx2 = traverse(fparams)(visitFormalParam)
      val idx3 = traverse(declaredScheme.constraints)(visitTypeConstraint)
      val idx4 = visitType(retTpe)
      val idx5 = visitType(pur)
      val idx6 = visitType(eff)
      idx1 ++ idx2 ++ idx3 ++ idx4 ++ idx5 ++ idx6
  }

  /**
    * Returns a reverse index for the given enum `enum0`.
    */
  private def visitEnum(enum0: Enum): Index = enum0 match {
    case Enum(_, _, _, _, tparams, derives, cases, _, _) =>
      val idx0 = Index.occurrenceOf(enum0)
      val idx1 = traverse(tparams)(visitTypeParam)
      val idx2 = traverse(derives) {
        case Ast.Derivation(clazz, loc) => Index.useOf(clazz, loc)
      }
      val idx3 = traverse(cases) {
        case (_, caze) => Index.occurrenceOf(caze)
      }
      idx0 ++ idx1 ++ idx2 ++ idx3
  }

  /**
    * Returns a reverse index for the given class `class0`.
    */
  private def visitClass(class0: TypedAst.Class): Index = Index.occurrenceOf(class0)

  /**
    * Returns a reverse index for the given instance `instance0`.
    */
  private def visitInstance(instance0: Instance): Index = instance0 match {
    case Instance(_, _, _, sym, tpe, tconstrs, defs, _, _) =>
      val idx1 = Index.useOf(sym.clazz, sym.loc)
      val idx2 = visitType(tpe)
      val idx3 = traverse(tconstrs)(visitTypeConstraint)
      val idx4 = traverse(defs)(visitDef)
      idx1 ++ idx2 ++ idx3 ++ idx4
  }

  /**
    * Returns a reverse index for the given effect `eff0`
    */
  private def visitEff(eff0: Effect): Index = eff0 match {
    case Effect(_, _, _, _, ops, _) =>
      val idx1 = Index.occurrenceOf(eff0)
      val idx2 = traverse(ops)(visitOp)
      idx1 ++ idx2
  }

  /**
    * Returns a reverse index for the given effect operation `op0`
    */
  private def visitOp(op0: Op): Index = op0 match {
    case Op(_, spec) =>
      val idx1 = Index.occurrenceOf(op0)
      val idx2 = visitSpec(spec)
      idx1 ++ idx2
  }

  /**
    * Returns a reverse index for the given expression `exp0`.
    */
  private def visitExp(exp0: Expression): Index = exp0 match {
    case Expression.Unit(_) =>
      Index.occurrenceOf(exp0)

    case Expression.Null(_, _) =>
      Index.occurrenceOf(exp0)

    case Expression.True(_) =>
      Index.occurrenceOf(exp0)

    case Expression.False(_) =>
      Index.occurrenceOf(exp0)

    case Expression.Char(_, _) =>
      Index.occurrenceOf(exp0)

    case Expression.Float32(_, _) =>
      Index.occurrenceOf(exp0)

    case Expression.Float64(_, _) =>
      Index.occurrenceOf(exp0)

    case Expression.Int8(_, _) =>
      Index.occurrenceOf(exp0)

    case Expression.Int16(_, _) =>
      Index.occurrenceOf(exp0)

    case Expression.Int32(_, _) =>
      Index.occurrenceOf(exp0)

    case Expression.Int64(_, _) =>
      Index.occurrenceOf(exp0)

    case Expression.BigInt(_, _) =>
      Index.occurrenceOf(exp0)

    case Expression.Str(_, _) =>
      Index.occurrenceOf(exp0)

    case Expression.Default(_, _) =>
      Index.occurrenceOf(exp0)

    case Expression.Wild(_, _) =>
      Index.occurrenceOf(exp0)

    case Expression.Var(sym, _, loc) =>
      Index.occurrenceOf(exp0) ++ Index.useOf(sym, loc)

    case Expression.Def(sym, _, loc) =>
      Index.occurrenceOf(exp0) ++ Index.useOf(sym, loc)

    case Expression.Sig(sym, _, loc) =>
      Index.occurrenceOf(exp0) ++ Index.useOf(sym, loc) ++ Index.useOf(sym.clazz, loc)

    case Expression.Hole(_, _, _) =>
      Index.occurrenceOf(exp0)

    case Expression.Lambda(fparam, exp, _, _) =>
      visitFormalParam(fparam) ++ visitExp(exp) ++ Index.occurrenceOf(exp0)

    case Expression.Apply(exp, exps, _, _, _, _) =>
      visitExp(exp) ++ visitExps(exps) ++ Index.occurrenceOf(exp0)

    case Expression.Unary(_, exp, _, _, _, _) =>
      visitExp(exp) ++ Index.occurrenceOf(exp0)

    case Expression.Binary(_, exp1, exp2, _, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2) ++ Index.occurrenceOf(exp0)

    case Expression.Let(sym, _, exp1, exp2, _, _, _, _) =>
      Index.occurrenceOf(sym, exp1.tpe) ++ visitExp(exp1) ++ visitExp(exp2) ++ Index.occurrenceOf(exp0)

    case Expression.LetRec(sym, _, exp1, exp2, _, _, _, _) =>
      Index.occurrenceOf(sym, exp1.tpe) ++ visitExp(exp1) ++ visitExp(exp2) ++ Index.occurrenceOf(exp0)

    case Expression.Region(_, _) =>
      Index.occurrenceOf(exp0)

    case Expression.Scope(sym, _, exp, _, _, _, loc) =>
      val tpe = Type.mkRegion(sym.tvar.ascribedWith(Kind.Bool), loc)
      Index.occurrenceOf(sym, tpe) ++ visitExp(exp) ++ Index.occurrenceOf(exp0)

    case Expression.IfThenElse(exp1, exp2, exp3, _, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2) ++ visitExp(exp3) ++ Index.occurrenceOf(exp0)

    case Expression.Stm(exp1, exp2, _, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2)

    case Expression.Discard(exp, _, _, _) =>
      visitExp(exp) ++ Index.occurrenceOf(exp0)

    case Expression.Match(exp, rules, _, _, _, _) =>
      val i0 = visitExp(exp) ++ Index.occurrenceOf(exp0)
      val i1 = traverse(rules) {
        case MatchRule(pat, guard, exp) => visitPat(pat) ++ visitExp(guard) ++ visitExp(exp)
      }
      i0 ++ i1

    case Expression.Choose(exps, rules, _, _, _, _) =>
      visitExps(exps) ++ traverse(rules) {
        case ChoiceRule(_, exp) => visitExp(exp)
      }

    case Expression.Tag(sym, tag, exp, _, _, _, _) =>
      visitExp(exp) ++ Index.useOf(sym, tag) ++ Index.occurrenceOf(exp0)

    case Expression.Tuple(exps, _, _, _, _) =>
      visitExps(exps) ++ Index.occurrenceOf(exp0)

    case Expression.RecordEmpty(_, _) =>
      Index.occurrenceOf(exp0)

    case Expression.RecordSelect(exp, field, _, _, _, _) =>
      Index.occurrenceOf(field) ++ Index.useOf(field) ++ visitExp(exp) ++ Index.occurrenceOf(exp0)

    case Expression.RecordExtend(field, exp1, exp2, _, _, _, _) =>
      Index.occurrenceOf(field) ++ Index.defOf(field) ++ visitExp(exp1) ++ visitExp(exp2) ++ Index.occurrenceOf(exp0)

    case Expression.RecordRestrict(field, exp, _, _, _, _) =>
      Index.occurrenceOf(field) ++ Index.defOf(field) ++ visitExp(exp) ++ Index.occurrenceOf(exp0)

    case Expression.ArrayLit(exps, exp, _, _, _, _) =>
      visitExps(exps) ++ visitExp(exp) ++ Index.occurrenceOf(exp0)

    case Expression.ArrayNew(exp1, exp2, exp3, _, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2) ++ visitExp(exp3) ++ Index.occurrenceOf(exp0)

    case Expression.ArrayLoad(exp1, exp2, _, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2) ++ Index.occurrenceOf(exp0)

    case Expression.ArrayLength(exp, _, _, _) =>
      visitExp(exp) ++ Index.occurrenceOf(exp0)

    case Expression.ArrayStore(exp1, exp2, exp3, _, _) =>
      visitExp(exp1) ++ visitExp(exp2) ++ visitExp(exp3) ++ Index.occurrenceOf(exp0)

    case Expression.ArraySlice(exp1, exp2, exp3, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2) ++ visitExp(exp3) ++ Index.occurrenceOf(exp0)

    case Expression.Ref(exp1, exp2, _, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2) ++ Index.occurrenceOf(exp0)

    case Expression.Deref(exp1, _, _, _, _) =>
      visitExp(exp1) ++ Index.occurrenceOf(exp0)

    case Expression.Assign(exp1, exp2, _, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2) ++ Index.occurrenceOf(exp0)

    case Expression.Ascribe(exp, tpe, pur, _, _) =>
      visitExp(exp) ++ visitType(tpe) ++ visitType(pur) ++ Index.occurrenceOf(exp0)

    case Expression.Cast(exp, declaredType, declaredPur, declaredEff, _, _, _, loc) =>
      val dt = declaredType.map(visitType).getOrElse(Index.empty)
      val dp = declaredPur.map(visitType).getOrElse(Index.empty)
      val de = declaredEff.map(visitType).getOrElse(Index.empty)
      visitExp(exp) ++ dt ++ dp ++ de ++ Index.occurrenceOf(exp0)

    case Expression.Without(exp, effUse, _, _, _, _) =>
      visitExp(exp) ++ Index.occurrenceOf(exp0) ++ Index.useOf(effUse.sym, effUse.loc)

    case Expression.TryCatch(exp, rules, _, _, _, _) =>
      val i0 = visitExp(exp) ++ Index.occurrenceOf(exp0)
      val i1 = traverse(rules) {
        case CatchRule(_, _, exp) => visitExp(exp)
      }
      i0 ++ i1

    case Expression.TryWith(exp, effUse, rules, _, _, _, _) =>
      val i0 = visitExp(exp) ++ Index.occurrenceOf(exp0) ++ Index.useOf(effUse.sym, effUse.loc)
      val i1 = traverse(rules) {
        case HandlerRule(op, fparams, exp) =>
          Index.traverse(fparams)(visitFormalParam) ++ visitExp(exp) ++ Index.useOf(op.sym, op.loc)
      }
      i0 ++ i1

    case Expression.Do(op, exps, _, _, loc) =>
      traverse(exps)(visitExp) ++ Index.occurrenceOf(exp0) ++ Index.useOf(op.sym, op.loc)

    case Expression.Resume(exp, _, _) =>
      visitExp(exp) ++ Index.occurrenceOf(exp0)

    case Expression.InvokeConstructor(_, args, _, _, _, _) =>
      visitExps(args) ++ Index.occurrenceOf(exp0)

    case Expression.InvokeMethod(_, exp, args, _, _, _, _) =>
      visitExp(exp) ++ visitExps(args) ++ Index.occurrenceOf(exp0)

    case Expression.InvokeStaticMethod(_, args, _, _, _, _) =>
      visitExps(args) ++ Index.occurrenceOf(exp0)

    case Expression.GetField(_, exp, _, _, _, _) =>
      visitExp(exp) ++ Index.occurrenceOf(exp0)

    case Expression.PutField(_, exp1, exp2, _, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2) ++ Index.occurrenceOf(exp0)

    case Expression.GetStaticField(_, _, _, _, _) =>
      Index.occurrenceOf(exp0)

    case Expression.PutStaticField(_, exp, _, _, _, _) =>
      visitExp(exp) ++ Index.occurrenceOf(exp0)

    case Expression.NewObject(_, _, _, _, methods, _) =>
      Index.occurrenceOf(exp0) ++ traverse(methods) {
        case JvmMethod(_, fparams, exp, tpe, eff, pur, _) =>
          Index.traverse(fparams)(visitFormalParam) ++ visitExp(exp) ++ visitType(tpe) ++ visitType(eff) ++ visitType(pur)
      }

    case Expression.NewChannel(exp, _, _, _, _) =>
      visitExp(exp) ++ Index.occurrenceOf(exp0)

    case Expression.GetChannel(exp, _, _, _, _) =>
      visitExp(exp) ++ Index.occurrenceOf(exp0)

    case Expression.PutChannel(exp1, exp2, _, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2) ++ Index.occurrenceOf(exp0)

    case Expression.SelectChannel(rules, default, _, _, _, _) =>
      val i0 = default.map(visitExp).getOrElse(Index.empty)
      val i1 = traverse(rules) {
        case SelectChannelRule(sym, chan, body) =>
          Index.occurrenceOf(sym, sym.tvar.ascribedWith(Kind.Star)) ++ visitExp(chan) ++ visitExp(body)
      }
      i0 ++ i1 ++ Index.occurrenceOf(exp0)

    case Expression.Spawn(exp, _, _, _, _) =>
      visitExp(exp) ++ Index.occurrenceOf(exp0)

    case Expression.Lazy(exp, _, _) =>
      visitExp(exp) ++ Index.occurrenceOf(exp0)

    case Expression.Force(exp, _, _, _, _) =>
      visitExp(exp) ++ Index.occurrenceOf(exp0)

    case Expression.FixpointConstraintSet(cs, _, _, _) => traverse(cs)(visitConstraint)

    case Expression.FixpointLambda(pparams, exp, _, _, _, _, _) =>
      val i0 = traverse(pparams)(visitPredicateParam)
      i0 ++ visitExp(exp) ++ Index.occurrenceOf(exp0)

    case Expression.FixpointMerge(exp1, exp2, _, _, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2) ++ Index.occurrenceOf(exp0)

    case Expression.FixpointSolve(exp, _, _, _, _, _) =>
      visitExp(exp) ++ Index.occurrenceOf(exp0)

    case Expression.FixpointFilter(_, exp, _, _, _, _) =>
      visitExp(exp) ++ Index.occurrenceOf(exp0)

    case Expression.FixpointInject(exp, _, _, _, _, _) =>
      visitExp(exp) ++ Index.occurrenceOf(exp0)

    case Expression.FixpointProject(_, exp, _, _, _, _) =>
      visitExp(exp) ++ Index.occurrenceOf(exp0)

    case Expression.Reify(t, _, _, _, _) =>
      visitType(t) ++ Index.occurrenceOf(exp0)

    case Expression.ReifyType(t, _, _, _, _, _) =>
      visitType(t) ++ Index.occurrenceOf(exp0)

    case Expression.ReifyEff(sym, exp1, exp2, exp3, _, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2) ++ visitExp(exp3) ++ Index.occurrenceOf(sym, exp1.tpe) ++ Index.occurrenceOf(exp0)
  }

  /**
    * Returns a reverse index for the given expressions `exps0`.
    */
  private def visitExps(exps0: List[Expression]): Index = traverse(exps0)(visitExp)

  /**
    * Returns a reverse index for the given pattern `pat0`.
    */
  private def visitPat(pat0: Pattern): Index = pat0 match {
    case Pattern.Wild(_, _) => Index.occurrenceOf(pat0)
    case Pattern.Var(sym, _, loc) => Index.occurrenceOf(pat0) ++ Index.useOf(sym, loc)
    case Pattern.Unit(_) => Index.occurrenceOf(pat0)
    case Pattern.True(_) => Index.occurrenceOf(pat0)
    case Pattern.False(_) => Index.occurrenceOf(pat0)
    case Pattern.Char(_, _) => Index.occurrenceOf(pat0)
    case Pattern.Float32(_, _) => Index.occurrenceOf(pat0)
    case Pattern.Float64(_, _) => Index.occurrenceOf(pat0)
    case Pattern.Int8(_, _) => Index.occurrenceOf(pat0)
    case Pattern.Int16(_, _) => Index.occurrenceOf(pat0)
    case Pattern.Int32(_, _) => Index.occurrenceOf(pat0)
    case Pattern.Int64(_, _) => Index.occurrenceOf(pat0)
    case Pattern.BigInt(_, _) => Index.occurrenceOf(pat0)
    case Pattern.Str(_, _) => Index.occurrenceOf(pat0)
    case Pattern.Tag(sym, tag, pat, _, loc) => Index.occurrenceOf(pat0) ++ visitPat(pat) ++ Index.useOf(sym, tag)
    case Pattern.Tuple(elms, _, _) => Index.occurrenceOf(pat0) ++ visitPats(elms)
    case Pattern.Array(elms, _, _) => Index.occurrenceOf(pat0) ++ visitPats(elms)
    case Pattern.ArrayTailSpread(elms, _, _, _) => Index.occurrenceOf(pat0) ++ visitPats(elms)
    case Pattern.ArrayHeadSpread(_, elms, _, _) => Index.occurrenceOf(pat0) ++ visitPats(elms)
  }

  /**
    * Returns a reverse index for the given patterns `pats0`.
    */
  private def visitPats(pats0: List[Pattern]): Index = traverse(pats0)(visitPat)

  /**
    * Returns a reverse index for the given constraint `c0`.
    */
  private def visitConstraint(c0: Constraint): Index = c0 match {
    case Constraint(_, head, body, _) =>
      val idx1 = visitHead(head)
      val idx2 = traverse(body)(visitBody)
      idx1 ++ idx2
  }

  /**
    * Returns a reverse index for the given head predicate `h0`.
    */
  private def visitHead(h0: Predicate.Head): Index = h0 match {
    case Head.Atom(pred, _, terms, tpe, _) => Index.occurrenceOf(pred, tpe) ++ Index.defOf(pred) ++ visitExps(terms)
  }

  /**
    * Returns a reverse index for the given body predicate `b0`.
    */
  private def visitBody(b0: Predicate.Body): Index = b0 match {
    case Body.Atom(pred, _, _, _, terms, tpe, _) => Index.occurrenceOf(pred, tpe) ++ Index.useOf(pred) ++ visitPats(terms)
    case Body.Guard(exp, _) => visitExp(exp)
    case Body.Loop(_, exp, _) => visitExp(exp)
  }

  /**
    * Returns a reverse index for the given type parameter `tparam0`.
    */
  private def visitTypeParam(tparam0: TypeParam): Index = tparam0 match {
    case TypeParam(_, sym, _) => Index.occurrenceOf(sym)
  }

  /**
    * Returns a reverse index for the given formal parameter `fparam0`.
    */
  private def visitFormalParam(fparam0: FormalParam): Index = fparam0 match {
    case FormalParam(_, _, tpe, _, _) =>
      Index.occurrenceOf(fparam0) ++ visitType(tpe)
  }

  /**
    * Returns a reverse index for the given predicate parameter `pparam0`.
    */
  private def visitPredicateParam(pparam0: PredicateParam): Index = pparam0 match {
    case PredicateParam(pred, tpe, _) =>
      Index.occurrenceOf(pred, tpe) ++ Index.defOf(pred) ++ visitType(tpe)
  }

  /**
    * Returns a reverse index for the given type `tpe0`.
    */
  private def visitType(tpe0: Type): Index = tpe0 match {
    case Type.KindedVar(sym, loc) => Index.occurrenceOf(tpe0) ++ Index.useOf(sym, loc)
    case Type.Cst(tc, loc) => tc match {
      case TypeConstructor.Arrow(_) =>
        // We do not index arrow constructors.
        Index.empty
      case TypeConstructor.RecordRowExtend(field) => Index.occurrenceOf(tpe0) ++ Index.useOf(field)
      case TypeConstructor.SchemaRowExtend(pred) => Index.occurrenceOf(tpe0) ++ Index.useOf(pred)
      case TypeConstructor.KindedEnum(sym, _) => Index.occurrenceOf(tpe0) ++ Index.useOf(sym, loc)
      case TypeConstructor.Effect(sym) => Index.occurrenceOf(tpe0) ++ Index.useOf(sym, loc)
      case _ => Index.occurrenceOf(tpe0)
    }
    case Type.Apply(tpe1, tpe2, _) => visitType(tpe1) ++ visitType(tpe2)
    case Type.Alias(_, _, tpe, _) => visitType(tpe) // TODO index TypeAlias
    case _: Type.Ascribe => throw InternalCompilerException(s"Unexpected type: $tpe0.")
    case _: Type.UnkindedVar => throw InternalCompilerException(s"Unexpected type: $tpe0.")
    case _: Type.UnkindedArrow => throw InternalCompilerException(s"Unexpected type: $tpe0.")
    case _: Type.ReadWrite => throw InternalCompilerException(s"Unexpected type: $tpe0.")
  }

  /**
    * Returns a reverse index for the given type constraint `tconstr0`.
    */
  private def visitTypeConstraint(tconstr0: Ast.TypeConstraint): Index = tconstr0 match {
    case Ast.TypeConstraint(head, arg, _) => visitTypeConstraintHead(head) ++ visitType(arg)
  }

  /**
    * Returns a reverse index for the given type constraint `head`.
    */
  private def visitTypeConstraintHead(head0: Ast.TypeConstraint.Head): Index = head0 match {
    case Ast.TypeConstraint.Head(sym, loc) => Index.useOf(sym, loc)
  }

}

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

import ca.uwaterloo.flix.language.ast.TypedAst.Predicate.{Body, Head}
import ca.uwaterloo.flix.language.ast.TypedAst.{CatchRule, ChoiceRule, Constraint, Def, Enum, Expression, FormalParam, Instance, MatchRule, Pattern, Predicate, Root, SelectChannelRule, Sig, Spec}
import ca.uwaterloo.flix.language.ast._

object Indexer {

  /**
    * Returns a reverse index for the given AST `root`.
    */
  def visitRoot(root: Root): Index = {
    val idx1 = root.defs.foldLeft(Index.empty) {
      case (acc, (_, def0)) => acc ++ visitDef(def0)
    }
    val idx2 = root.enums.foldLeft(Index.empty) {
      case (acc, (_, enum0)) => acc ++ visitEnum(enum0)
    }
    val idx3 = root.classes.foldLeft(Index.empty) {
      case (acc, (_, class0)) => acc ++ visitClass(class0)
    }
    val idx4 = root.instances.foldLeft(Index.empty) {
      case (acc, (_, instances)) => acc ++ instances.foldLeft(Index.empty) {
        case (acc1, instance) => acc1 ++ visitInstance(instance)
      }
    }
    val idx5 = root.sigs.foldLeft(Index.empty) {
      case (acc, (_, sig)) => acc ++ visitSig(sig)
    }
    idx1 ++ idx2 ++ idx3 ++ idx4 ++ idx5
  }

  /**
    * Returns a reverse index for the given definition `def0`.
    */
  private def visitDef(def0: Def): Index = {
    val idx0 = Index.occurrenceOf(def0)
    val idx1 = visitExp(def0.impl.exp)
    val idx2 = def0.spec.fparams.foldLeft(Index.empty) {
      case (acc, fparam) => acc ++ visitFormalParam(fparam)
    }
    val idx3 = visitScheme(def0.spec.declaredScheme, def0.spec.loc)
    idx0 ++ idx1 ++ idx2 ++ idx3
  }

  /**
    * Returns a reverse index for the given signature `sig0`.
    */
  private def visitSig(sig0: Sig): Index = sig0 match {
    case Sig(_, Spec(_, _, _, _, fparams, _, _, _, _), _) =>
      val idx1 = Index.occurrenceOf(sig0)
      val idx2 = fparams.foldLeft(Index.empty) {
        case (acc, fparam) => acc ++ visitFormalParam(fparam)
      }
      idx1 ++ idx2
  }

  /**
    * Returns a reverse index for the given enum `enum0`.
    */
  private def visitEnum(enum0: Enum): Index = {
    val idx0 = Index.occurrenceOf(enum0)
    val idx1 = enum0.cases.foldLeft(Index.empty) {
      case (idx, (tag, caze)) => idx ++ Index.occurrenceOf(caze)
    }
    idx0 ++ idx1
  }

  /**
    * Returns a reverse index for the given class `class0`.
    */
  private def visitClass(class0: TypedAst.Class): Index = class0 match {
    case TypedAst.Class(doc, mod, sym, tparam, superClasses, signatures, laws, loc) =>
      Index.occurrenceOf(class0)
  }

  /**
    * Returns a reverse index for the given instance `instance0`.
    */
  private def visitInstance(instance0: Instance): Index = instance0 match {
    case Instance(_, _, sym, tpe, _, defs, _, loc) =>
      val idx1 = Index.useOf(sym, loc)
      val idx2 = visitType(tpe)
      val idx3 = defs.foldLeft(Index.empty) {
        case (acc, defn) => visitDef(defn)
      }
      idx1 ++ idx2 ++ idx3
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

    case Expression.Hole(_, _, _, _) =>
      Index.occurrenceOf(exp0)

    case Expression.Lambda(fparam, exp, _, _) =>
      visitFormalParam(fparam) ++ visitExp(exp) ++ Index.occurrenceOf(exp0)

    case Expression.Apply(exp, exps, _, _, _) =>
      visitExp(exp) ++ visitExps(exps) ++ Index.occurrenceOf(exp0)

    case Expression.Unary(_, exp, _, _, _) =>
      visitExp(exp) ++ Index.occurrenceOf(exp0)

    case Expression.Binary(_, exp1, exp2, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2) ++ Index.occurrenceOf(exp0)

    case Expression.Let(sym, _, exp1, exp2, _, _, _) =>
      Index.occurrenceOf(sym, exp1.tpe) ++ visitExp(exp1) ++ visitExp(exp2) ++ Index.occurrenceOf(exp0)

    case Expression.LetRegion(sym, exp, _, _, _) =>
      visitExp(exp) ++ Index.occurrenceOf(exp0)

    case Expression.IfThenElse(exp1, exp2, exp3, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2) ++ visitExp(exp3) ++ Index.occurrenceOf(exp0)

    case Expression.Stm(exp1, exp2, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2)

    case Expression.Match(exp, rules, _, _, _) =>
      val i0 = visitExp(exp) ++ Index.occurrenceOf(exp0)
      rules.foldLeft(i0) {
        case (index, MatchRule(pat, guard, exp)) => index ++ visitPat(pat) ++ visitExp(guard) ++ visitExp(exp)
      }

    case Expression.Choose(exps, rules, _, _, _) =>
      visitExps(exps) ++ rules.foldLeft(Index.empty) {
        case (acc, ChoiceRule(_, exp)) => acc ++ visitExp(exp)
      }

    case Expression.Tag(sym, tag, exp, _, _, _) =>
      visitExp(exp) ++ Index.useOf(sym, tag) ++ Index.occurrenceOf(exp0)

    case Expression.Tuple(exps, tpe, eff, loc) =>
      visitExps(exps) ++ Index.occurrenceOf(exp0)

    case Expression.RecordEmpty(tpe, loc) =>
      Index.occurrenceOf(exp0)

    case Expression.RecordSelect(exp, field, _, _, _) =>
      Index.occurrenceOf(field) ++ Index.useOf(field) ++ visitExp(exp) ++ Index.occurrenceOf(exp0)

    case Expression.RecordExtend(field, exp1, exp2, _, _, _) =>
      Index.occurrenceOf(field) ++ Index.defOf(field) ++ visitExp(exp1) ++ visitExp(exp2) ++ Index.occurrenceOf(exp0)

    case Expression.RecordRestrict(field, exp, _, _, _) =>
      Index.occurrenceOf(field) ++ Index.defOf(field) ++ visitExp(exp) ++ Index.occurrenceOf(exp0)

    case Expression.ArrayLit(exps, _, _, _) =>
      visitExps(exps) ++ Index.occurrenceOf(exp0)

    case Expression.ArrayNew(exp1, exp2, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2) ++ Index.occurrenceOf(exp0)

    case Expression.ArrayLoad(exp1, exp2, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2) ++ Index.occurrenceOf(exp0)

    case Expression.ArrayLength(exp, _, _) =>
      visitExp(exp) ++ Index.occurrenceOf(exp0)

    case Expression.ArrayStore(exp1, exp2, exp3, _) =>
      visitExp(exp1) ++ visitExp(exp2) ++ visitExp(exp3) ++ Index.occurrenceOf(exp0)

    case Expression.ArraySlice(exp1, exp2, exp3, _, _) =>
      visitExp(exp1) ++ visitExp(exp2) ++ visitExp(exp3) ++ Index.occurrenceOf(exp0)

    case Expression.Ref(exp, _, _, _) =>
      visitExp(exp) ++ Index.occurrenceOf(exp0)

    case Expression.Deref(exp1, _, _, _) =>
      visitExp(exp1) ++ Index.occurrenceOf(exp0)

    case Expression.Assign(exp1, exp2, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2) ++ Index.occurrenceOf(exp0)

    case Expression.Existential(fparam, exp, _) =>
      visitFormalParam(fparam) ++ visitExp(exp) ++ Index.occurrenceOf(exp0)

    case Expression.Universal(fparam, exp, _) =>
      visitFormalParam(fparam) ++ visitExp(exp) ++ Index.occurrenceOf(exp0)

    case Expression.Ascribe(exp, tpe, eff, loc) =>
      visitExp(exp) ++ visitType(tpe) ++ visitType(eff) ++ Index.occurrenceOf(exp0)

    case Expression.Cast(exp, tpe, eff, loc) =>
      visitExp(exp) ++ visitType(tpe) ++ visitType(eff) ++ Index.occurrenceOf(exp0)

    case Expression.TryCatch(exp, rules, _, _, _) =>
      val i0 = visitExp(exp) ++ Index.occurrenceOf(exp0)
      rules.foldLeft(i0) {
        case (index, CatchRule(_, _, exp)) => index ++ visitExp(exp)
      }

    case Expression.InvokeConstructor(_, args, _, _, _) =>
      visitExps(args) ++ Index.occurrenceOf(exp0)

    case Expression.InvokeMethod(_, exp, args, _, _, _) =>
      visitExp(exp) ++ visitExps(args) ++ Index.occurrenceOf(exp0)

    case Expression.InvokeStaticMethod(_, args, _, _, _) =>
      visitExps(args) ++ Index.occurrenceOf(exp0)

    case Expression.GetField(_, exp, _, _, _) =>
      visitExp(exp) ++ Index.occurrenceOf(exp0)

    case Expression.PutField(_, exp1, exp2, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2) ++ Index.occurrenceOf(exp0)

    case Expression.GetStaticField(_, _, _, _) =>
      Index.occurrenceOf(exp0)

    case Expression.PutStaticField(_, exp, _, _, _) =>
      visitExp(exp) ++ Index.occurrenceOf(exp0)

    case Expression.NewChannel(exp, _, _, _) =>
      visitExp(exp) ++ Index.occurrenceOf(exp0)

    case Expression.GetChannel(exp, _, _, _) =>
      visitExp(exp) ++ Index.occurrenceOf(exp0)

    case Expression.PutChannel(exp1, exp2, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2) ++ Index.occurrenceOf(exp0)

    case Expression.SelectChannel(rules, default, _, _, _) =>
      val i0 = default.map(visitExp).getOrElse(Index.empty)
      val i1 = rules.foldLeft(Index.empty) {
        case (index, SelectChannelRule(sym, chan, body)) =>
          index ++ Index.occurrenceOf(sym, sym.tvar.ascribedWith(Kind.Star)) ++ visitExp(chan) ++ visitExp(body)
      }
      i0 ++ i1 ++ Index.occurrenceOf(exp0)

    case Expression.Spawn(exp, _, _, _) =>
      visitExp(exp) ++ Index.occurrenceOf(exp0)

    case Expression.Lazy(exp, _, _) =>
      visitExp(exp) ++ Index.occurrenceOf(exp0)

    case Expression.Force(exp, _, _, _) =>
      visitExp(exp) ++ Index.occurrenceOf(exp0)

    case Expression.FixpointConstraintSet(cs, _, _, _) =>
      cs.foldLeft(Index.empty) {
        case (index, c) => index ++ visitConstraint(c)
      }

    case Expression.FixpointMerge(exp1, exp2, _, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2) ++ Index.occurrenceOf(exp0)

    case Expression.FixpointSolve(exp, _, _, _, _) =>
      visitExp(exp) ++ Index.occurrenceOf(exp0)

    case Expression.FixpointFilter(_, exp, _, _, _) =>
      visitExp(exp) ++ Index.occurrenceOf(exp0)

    case Expression.FixpointProjectIn(exp, _, _, _, _) =>
      visitExp(exp) ++ Index.occurrenceOf(exp0)

    case Expression.FixpointProjectOut(_, exp, _, _, _) =>
      visitExp(exp) ++ Index.occurrenceOf(exp0)

    case Expression.MatchEff(exp1, exp2, exp3, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2) ++ visitExp(exp3) ++ Index.occurrenceOf(exp0)

  }

  /**
    * Returns a reverse index for the given expressions `exps0`.
    */
  private def visitExps(exps0: List[Expression]): Index =
    exps0.foldLeft(Index.empty) {
      case (index, exp0) => index ++ visitExp(exp0)
    }

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
  private def visitPats(pats0: List[Pattern]): Index = pats0.foldLeft(Index.empty) {
    case (acc, pat0) => acc ++ visitPat(pat0)
  }

  /**
    * Returns a reverse index for the given constraint `c0`.
    */
  private def visitConstraint(c0: Constraint): Index = {
    val i = visitHead(c0.head)
    c0.body.foldLeft(i) {
      case (index, b0) => index ++ visitBody(b0)
    }
  }

  /**
    * Returns a reverse index for the given head predicate `h0`.
    */
  private def visitHead(h0: Predicate.Head): Index = h0 match {
    case Head.Atom(pred, _, terms, _, _) => Index.occurrenceOf(pred) ++ Index.defOf(pred) ++ visitExps(terms)
  }

  /**
    * Returns a reverse index for the given body predicate `b0`.
    */
  private def visitBody(b0: Predicate.Body): Index = b0 match {
    case Body.Atom(pred, _, _, terms, _, _) => Index.occurrenceOf(pred) ++ Index.useOf(pred) ++ visitPats(terms)
    case Body.Guard(exp, _) => visitExp(exp)
  }

  /**
    * Returns a reverse index for the given formal parameter `fparam0`.
    */
  private def visitFormalParam(fparam0: FormalParam): Index = fparam0 match {
    case FormalParam(_, _, tpe, _) =>
      Index.occurrenceOf(fparam0) ++ visitType(tpe)
  }

  /**
    * Returns a reverse index for the given type scheme `sc0` at the given source location `loc`.
    */
  private def visitScheme(sc0: Scheme, loc: SourceLocation): Index = visitType(sc0.base)

  /**
    * Returns a reverse index for the given type `tpe0`.
    */
  private def visitType(tpe0: Type): Index = tpe0 match {
    case Type.Var(_, _, _, _) => Index.empty
    case Type.Cst(tc, loc) => tc match {
      case TypeConstructor.RecordExtend(field) => Index.occurrenceOf(tc, loc) ++ Index.useOf(field)
      case TypeConstructor.SchemaExtend(pred) => Index.occurrenceOf(tc, loc) ++ Index.useOf(pred)
      case _ => Index.occurrenceOf(tc, loc)
    }
    case Type.Lambda(_, tpe) => visitType(tpe)
    case Type.Apply(tpe1, tpe2) => visitType(tpe1) ++ visitType(tpe2)
  }

}

/*
 * Copyright 2021 Jacob Harris Cryer Kragh, Magnus Madsen
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
package ca.uwaterloo.flix.api.lsp.provider

import ca.uwaterloo.flix.api.lsp._
import ca.uwaterloo.flix.language.ast.Ast.{BoundBy, TypeConstraint}
import ca.uwaterloo.flix.language.ast.TypedAst.Predicate.{Body, Head}
import ca.uwaterloo.flix.language.ast.TypedAst._
import ca.uwaterloo.flix.language.ast.{Ast, SourceLocation, Symbol, Type, TypeConstructor, TypedAst}
import ca.uwaterloo.flix.util.InternalCompilerException
import org.json4s.JsonAST.JObject
import org.json4s.JsonDSL._

import scala.collection.immutable.SortedSet
import scala.collection.mutable.ArrayBuffer

object SemanticTokensProvider {

  /**
    * Processes a request for (full) semantic tokens.
    */
  def provideSemanticTokens(uri: String)(implicit index: Index, root: Root): JObject = {
    if (root == null)
      throw new IllegalArgumentException("The argument 'root' must be non-null.")

    //
    // This class uses iterators over lists to ensure fast append (!)
    //

    //
    // Construct an iterator of the semantic tokens from classes.
    //
    val classTokens = root.classes.values.flatMap {
      case decl if include(uri, decl.sym.loc) => visitClass(decl)
      case _ => Nil
    }

    //
    // Construct an iterator of the semantic tokens from instances.
    //
    val instanceTokens = root.instances.values.flatMap {
      case instances => instances.flatMap {
        case instance if include(uri, instance.sym.loc) => visitInstance(instance)
        case _ => Nil
      }
    }

    //
    // Construct an iterator of the semantic tokens from defs.
    //
    val defnTokens = root.defs.values.flatMap {
      case decl if include(uri, decl.sym.loc) => visitDef(decl)
      case _ => Nil
    }

    //
    // Construct an iterator of the semantic tokens from sigs.
    //
    val sigsTokens = root.sigs.values.flatMap {
      case decl if include(uri, decl.sym.loc) => visitSig(decl)
      case _ => Nil
    }

    //
    // Construct an iterator of the semantic tokens from enums.
    //
    val enumTokens = root.enums.values.flatMap {
      case decl if include(uri, decl.loc) => visitEnum(decl)
      case _ => Nil
    }

    //
    // Construct an iterator of the semantic tokens from type aliases.
    //
    val typeAliasTokens = root.typeAliases.flatMap {
      case (_, decl) if include(uri, decl.loc) => visitTypeAlias(decl)
      case _ => Nil
    }

    //
    // Collect all tokens into one list.
    //
    val allTokens = (classTokens ++ instanceTokens ++ defnTokens ++ enumTokens ++ typeAliasTokens).toList

    //
    // We keep all tokens that are: (i) single-line tokens, (ii) have the same source as `uri`, and (iii) come from real source locations.
    //
    // Note that the last criteria (automatically) excludes:
    //   (a) tokens with unknown source locations,
    //   (b) tokens that come from entities inside `uri` but that originate from different uris, and
    //   (c) tokens that come from synthetic (generated) source code.
    //
    val filteredTokens = allTokens.filter(t => t.loc.isSingleLine && include(uri, t.loc) && !t.loc.isSynthetic)

    //
    // Encode the semantic tokens as a list of integers.
    //
    val encodedTokens = encodeSemanticTokens(filteredTokens)

    //
    // Construct the JSON result.
    //
    ("status" -> "success") ~ ("result" -> ("data" -> encodedTokens))
  }

  /**
    * Returns `true` if the given source location `loc` is associated with the given `uri`.
    */
  private def include(uri: String, loc: SourceLocation): Boolean = loc.source.name == uri

  /**
    * Returns all semantic tokens in the given class `classDecl`.
    */
  private def visitClass(classDecl: TypedAst.Class): Iterator[SemanticToken] = classDecl match {
    case TypedAst.Class(_, _, _, sym, tparam, superClasses, signatures, laws, _) =>
      val t = SemanticToken(SemanticTokenType.Interface, Nil, sym.loc)
      val st1 = Iterator(t)
      val st2 = superClasses.flatMap(visitTypeConstraint)
      val st3 = visitTypeParam(tparam)
      val st4 = signatures.flatMap(visitSig)
      val st5 = laws.flatMap(visitDef)
      st1 ++ st2 ++ st3 ++ st4 ++ st5
  }

  /**
    * Returns all semantic tokens in the given instance `inst0`.
    */
  private def visitInstance(inst0: TypedAst.Instance): Iterator[SemanticToken] = inst0 match {
    case TypedAst.Instance(_, _, sym, tpe, tconstrs, defs, _, _) =>
      // NB: we use SemanticTokenType.Class because the OOP "Class" most directly corresponds to the FP "Instance"
      val t = SemanticToken(SemanticTokenType.Class, Nil, sym.loc)
      val st1 = Iterator(t)
      val st2 = visitType(tpe)
      val st3 = tconstrs.flatMap(visitTypeConstraint)
      val st4 = defs.flatMap(visitDef)
      st1 ++ st2 ++ st3 ++ st4
  }

  /**
    * Returns all semantic tokens in the given enum `enum0`.
    *
    * Returns tokens for the symbol, the type parameters, the derivations, and the cases.
    */
  private def visitEnum(enum0: TypedAst.Enum): Iterator[SemanticToken] = enum0 match {
    case TypedAst.Enum(_, _, _, sym, tparams, derives, cases, _, _, _) =>
      val t = SemanticToken(SemanticTokenType.Enum, Nil, sym.loc)
      val st1 = Iterator(t)
      val st2 = visitTypeParams(tparams)
      val st3 = Iterator(derives: _*).map {
        case Ast.Derivation(_, loc) => SemanticToken(SemanticTokenType.Class, Nil, loc)
      }
      val st4 = cases.foldLeft(Iterator.empty[SemanticToken]) {
        case (acc, (_, caze)) => acc ++ visitCase(caze)
      }
      st1 ++ st2 ++ st3 ++ st4
  }

  /**
    * Returns all semantic tokens in the given case `case0`.
    */
  private def visitCase(case0: TypedAst.Case): Iterator[SemanticToken] = case0 match {
    case TypedAst.Case(_, tag, _, sc, _) =>
      val t = SemanticToken(SemanticTokenType.EnumMember, Nil, tag.loc)
      Iterator(t) ++ visitType(sc.base)
  }

  /**
    * Returns all semantic tokens in the given definition `defn0`.
    */
  private def visitDef(defn0: TypedAst.Def): Iterator[SemanticToken] = defn0 match {
    case Def(sym, spec, impl) =>
      val t = SemanticToken(SemanticTokenType.Function, Nil, sym.loc)
      val st1 = Iterator(t)
      val st2 = visitSpec(spec)
      val st3 = visitImpl(impl)
      st1 ++ st2 ++ st3
  }

  /**
    * Returns all semantic tokens in the given signature `sig0`.
    */
  private def visitSig(sig0: TypedAst.Sig): Iterator[SemanticToken] = sig0 match {
    case TypedAst.Sig(sym, spec, impl) =>
      val t = SemanticToken(SemanticTokenType.Function, Nil, sym.loc)
      val st1 = Iterator(t)
      val st2 = visitSpec(spec)
      val st3 = impl.iterator.flatMap(visitImpl)
      st1 ++ st2 ++ st3
  }

  /**
    * Returns all semantic tokens in the given `spec`.
    */
  private def visitSpec(spec: Spec): Iterator[SemanticToken] = spec match {
    case Spec(_, _, _, tparams, fparams, sc, retTpe, eff, _) =>
      val st1 = visitTypeParams(tparams)
      val st2 = visitFormalParams(fparams)
      val st3 = sc.constraints.iterator.flatMap(visitTypeConstraint)
      val st4 = visitType(retTpe)
      val st5 = visitType(eff)
      st1 ++ st2 ++ st3 ++ st4 ++ st5
  }

  /**
    * Returns all semantic tokens in the given `impl`.
    */
  private def visitImpl(impl: Impl): Iterator[SemanticToken] = impl match {
    case Impl(exp, _) => visitExp(exp)
  }

  /**
    * Returns all semantic tokens in the given type alias `typeAlias0`.
    */
  private def visitTypeAlias(typeAlias0: TypedAst.TypeAlias): Iterator[SemanticToken] = typeAlias0 match {
    case TypedAst.TypeAlias(_, _, sym, tparams, tpe, _) =>
      val t = SemanticToken(SemanticTokenType.Type, Nil, sym.loc)
      val st1 = Iterator(t)
      val st2 = visitTypeParams(tparams)
      val st3 = visitType(tpe)
      st1 ++ st2 ++ st3
  }

  /**
    * Returns all semantic tokens in the given expression `exp0`.
    */
  private def visitExp(exp0: Expression): Iterator[SemanticToken] = exp0 match {
    case Expression.Wild(_, _) => Iterator.empty

    case Expression.Var(sym, tpe, loc) =>
      val o = getSemanticTokenType(sym, tpe)
      val t = SemanticToken(o, Nil, loc)
      Iterator(t)

    case Expression.Def(sym, _, loc) =>
      val o = if (isOperatorName(sym.name)) SemanticTokenType.Operator else SemanticTokenType.Function
      val t = SemanticToken(o, Nil, loc)
      Iterator(t)

    case Expression.Sig(sym, _, loc) =>
      val o = if (isOperatorName(sym.name)) SemanticTokenType.Operator else SemanticTokenType.Method
      val t = SemanticToken(o, Nil, loc)
      Iterator(t)

    case Expression.Hole(_, _, _) => Iterator.empty

    case Expression.Unit(loc) =>
      val t = SemanticToken(SemanticTokenType.EnumMember, Nil, loc)
      Iterator(t)

    case Expression.Null(_, _) => Iterator.empty

    case Expression.True(_) => Iterator.empty

    case Expression.False(_) => Iterator.empty

    case Expression.Char(_, _) => Iterator.empty

    case Expression.Float32(_, _) => Iterator.empty

    case Expression.Float64(_, _) => Iterator.empty

    case Expression.Int8(_, _) => Iterator.empty

    case Expression.Int16(_, _) => Iterator.empty

    case Expression.Int32(_, _) => Iterator.empty

    case Expression.Int64(_, _) => Iterator.empty

    case Expression.BigInt(_, _) => Iterator.empty

    case Expression.Str(_, _) => Iterator.empty

    case Expression.Default(_, _) => Iterator.empty

    case Expression.Lambda(fparam, exp, _, _) =>
      visitFormalParam(fparam) ++ visitExp(exp)

    case Expression.Apply(exp, exps, _, _, _) =>
      exps.foldLeft(visitExp(exp)) {
        case (acc, exp) => acc ++ visitExp(exp)
      }

    case Expression.Unary(sop, exp, tpe, eff, loc) =>
      visitExp(exp)

    case Expression.Binary(sop, exp1, exp2, tpe, eff, loc) =>
      visitExp(exp1) ++ visitExp(exp2)

    case Expression.Let(sym, _, exp1, exp2, _, _, _) =>
      val o = getSemanticTokenType(sym, exp1.tpe)
      val t = SemanticToken(o, Nil, sym.loc)
      Iterator(t) ++ visitExp(exp1) ++ visitExp(exp2)

    case Expression.LetRec(sym, _, exp1, exp2, _, _, _) =>
      val o = getSemanticTokenType(sym, exp1.tpe)
      val t = SemanticToken(o, Nil, sym.loc)
      Iterator(t) ++ visitExp(exp1) ++ visitExp(exp2)

    case Expression.Scope(sym, exp, _, _, _) =>
      val t = SemanticToken(SemanticTokenType.Variable, Nil, sym.loc)
      Iterator(t) ++ visitExp(exp)

    case Expression.IfThenElse(exp1, exp2, exp3, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2) ++ visitExp(exp3)

    case Expression.Stm(exp1, exp2, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2)

    case Expression.Match(matchExp, rules, _, _, _) =>
      val m = visitExp(matchExp)
      rules.foldLeft(m) {
        case (acc, MatchRule(pat, guard, exp)) =>
          acc ++ visitPat(pat) ++ visitExp(guard) ++ visitExp(exp)
      }

    case Expression.Choose(exps, rules, tpe, eff, loc) =>
      Iterator.empty // TODO: Choose expression.

    case Expression.Tag(_, tag, exp, _, _, _) =>
      val t = SemanticToken(SemanticTokenType.EnumMember, Nil, tag.loc)
      Iterator(t) ++ visitExp(exp)

    case Expression.Tuple(exps, _, _, _) =>
      visitExps(exps)

    case Expression.RecordEmpty(_, _) => Iterator.empty

    case Expression.RecordSelect(exp, field, _, _, _) =>
      val t = SemanticToken(SemanticTokenType.Property, Nil, field.loc)
      Iterator(t) ++ visitExp(exp)

    case Expression.RecordExtend(field, exp1, exp2, _, _, _) =>
      val t = SemanticToken(SemanticTokenType.Property, Nil, field.loc)
      Iterator(t) ++ visitExp(exp2) ++ visitExp(exp1)

    case Expression.RecordRestrict(field, exp, _, _, _) =>
      val t = SemanticToken(SemanticTokenType.Property, Nil, field.loc)
      Iterator(t) ++ visitExp(exp)

    case Expression.ArrayLit(exps, exp, _, _, _) =>
      visitExps(exps) ++ visitExp(exp)

    case Expression.ArrayNew(exp1, exp2, exp3, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2) ++ visitExp(exp3)

    case Expression.ArrayLoad(exp1, exp2, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2)

    case Expression.ArrayStore(exp1, exp2, exp3, _) =>
      visitExp(exp1) ++ visitExp(exp2) ++ visitExp(exp3)

    case Expression.ArrayLength(exp, _, _) =>
      visitExp(exp)

    case Expression.ArraySlice(exp1, exp2, exp3, _, _) =>
      visitExp(exp1) ++ visitExp(exp2) ++ visitExp(exp3)

    case Expression.Ref(exp1, exp2, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2)

    case Expression.Deref(exp, _, _, _) =>
      visitExp(exp)

    case Expression.Assign(exp1, exp2, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2)

    case Expression.Ascribe(exp, tpe, _, _) =>
      visitExp(exp) ++ visitType(tpe)

    case Expression.Cast(exp, _, _, tpe, _, _) =>
      visitExp(exp) ++ visitType(tpe)

    case Expression.TryCatch(exp, rules, _, _, _) =>
      rules.foldLeft(visitExp(exp)) {
        case (acc, CatchRule(sym, _, exp)) =>
          val t = SemanticToken(SemanticTokenType.Variable, Nil, sym.loc)
          acc ++ Iterator(t) ++ visitExp(exp)
      }

    case Expression.InvokeConstructor(_, exps, _, _, _) =>
      exps.foldLeft(Iterator.empty[SemanticToken]) {
        case (acc, exp) => acc ++ visitExp(exp)
      }

    case Expression.InvokeMethod(method, exp, exps, _, _, _) =>
      exps.foldLeft(visitExp(exp)) {
        case (acc, e) => acc ++ visitExp(e)
      }

    case Expression.InvokeStaticMethod(_, exps, _, _, _) =>
      exps.foldLeft(Iterator.empty[SemanticToken]) {
        case (acc, e) => acc ++ visitExp(e)
      }

    case Expression.GetField(_, exp, _, _, _) =>
      visitExp(exp)

    case Expression.PutField(_, exp1, exp2, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2)

    case Expression.GetStaticField(_, _, _, _) =>
      Iterator.empty

    case Expression.PutStaticField(_, exp, _, _, _) =>
      visitExp(exp)

    case Expression.NewChannel(exp, _, _, _) => visitExp(exp)

    case Expression.GetChannel(exp, _, _, _) => visitExp(exp)

    case Expression.PutChannel(exp1, exp2, _, _, _) => visitExp(exp1) ++ visitExp(exp2)

    case Expression.SelectChannel(rules, default, _, _, _) =>
      val rs = rules.foldLeft(Iterator.empty[SemanticToken]) {
        case (acc, SelectChannelRule(sym, chan, exp)) =>
          val t = SemanticToken(SemanticTokenType.Variable, Nil, sym.loc)
          acc ++ Iterator(t) ++ visitExp(chan) ++ visitExp(exp)
      }
      val d = default.map(visitExp).getOrElse(Iterator.empty)
      rs ++ d

    case Expression.Spawn(exp, _, _, _) => visitExp(exp)

    case Expression.Lazy(exp, _, _) => visitExp(exp)

    case Expression.Force(exp, _, _, _) => visitExp(exp)

    case Expression.FixpointConstraintSet(cs, _, _, _) =>
      cs.foldLeft(Iterator.empty[SemanticToken]) {
        case (acc, c) => acc ++ visitConstraint(c)
      }

    case Expression.FixpointMerge(exp1, exp2, _, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2)

    case Expression.FixpointSolve(exp, _, _, _, _) =>
      visitExp(exp)

    case Expression.FixpointFilter(_, exp, _, _, _) =>
      visitExp(exp)

    case Expression.FixpointProjectIn(exp, _, _, _, _) =>
      visitExp(exp)

    case Expression.FixpointProjectOut(_, exp, _, _, _) =>
      visitExp(exp)

    case Expression.Reify(_, _, _, _) => Iterator.empty

    case Expression.ReifyType(t, _, _, _, _) => visitType(t)

    case Expression.ReifyEff(sym, exp1, exp2, exp3, tpe, eff, loc) =>
      val o = getSemanticTokenType(sym, exp1.tpe)
      val t = SemanticToken(o, Nil, sym.loc)
      Iterator(t) ++ visitExp(exp1) ++ visitExp(exp2) ++ visitExp(exp3)
  }

  /**
    * Returns all semantic tokens in the given expressions `exps0`.
    */
  private def visitExps(exps0: List[Expression]): Iterator[SemanticToken] =
    exps0.flatMap(visitExp).iterator

  /**
    * Returns all semantic tokens in the given pattern `pat0`.
    */
  private def visitPat(pat0: Pattern): Iterator[SemanticToken] = pat0 match {
    case Pattern.Wild(_, loc) =>
      val t = SemanticToken(SemanticTokenType.Variable, Nil, loc)
      Iterator(t)

    case Pattern.Var(sym, tpe, loc) =>
      val o = getSemanticTokenType(sym, tpe)
      val t = SemanticToken(o, Nil, loc)
      Iterator(t)

    case Pattern.Unit(loc) =>
      val t = SemanticToken(SemanticTokenType.EnumMember, Nil, loc)
      Iterator(t)

    case Pattern.True(_) => Iterator.empty

    case Pattern.False(_) => Iterator.empty

    case Pattern.Char(_, _) => Iterator.empty

    case Pattern.Float32(_, _) => Iterator.empty

    case Pattern.Float64(_, _) => Iterator.empty

    case Pattern.Int8(_, _) => Iterator.empty

    case Pattern.Int16(_, _) => Iterator.empty

    case Pattern.Int32(_, _) => Iterator.empty

    case Pattern.Int64(_, _) => Iterator.empty

    case Pattern.BigInt(_, _) => Iterator.empty

    case Pattern.Str(_, _) => Iterator.empty

    case Pattern.Tag(_, tag, pat, _, _) =>
      val t = SemanticToken(SemanticTokenType.EnumMember, Nil, tag.loc)
      Iterator(t) ++ visitPat(pat)

    case Pattern.Tuple(pats, _, _) => pats.flatMap(visitPat).iterator

    case Pattern.Array(pats, _, _) => pats.flatMap(visitPat).iterator

    case Pattern.ArrayTailSpread(pats, sym, _, _) =>
      val t = SemanticToken(SemanticTokenType.Variable, Nil, sym.loc)
      Iterator(t) ++ pats.flatMap(visitPat).iterator

    case Pattern.ArrayHeadSpread(sym, pats, _, _) =>
      val t = SemanticToken(SemanticTokenType.Variable, Nil, sym.loc)
      Iterator(t) ++ pats.flatMap(visitPat).iterator
  }

  /**
    * Returns all semantic tokens in the given type `tpe0`.
    */
  private def visitType(tpe0: Type): Iterator[SemanticToken] = tpe0 match {
    case Type.KindedVar(_, loc) =>
      val t = SemanticToken(SemanticTokenType.TypeParameter, Nil, loc)
      Iterator(t)

    case Type.Ascribe(tpe, _, _) =>
      visitType(tpe)

    case Type.Cst(cst, loc) =>
      if (isVisibleTypeConstructor(cst)) {
        val t = SemanticToken(SemanticTokenType.Type, Nil, loc)
        Iterator(t)
      } else {
        Iterator.empty
      }

    case Type.Apply(tpe1, tpe2, _) =>
      visitType(tpe1) ++ visitType(tpe2)

    case Type.Alias(cst, args, _, _) =>
      val t = SemanticToken(SemanticTokenType.Type, Nil, cst.loc)
      Iterator(t) ++ args.flatMap(visitType).iterator

    case Type.UnkindedVar(_, _) =>
      throw InternalCompilerException(s"Unexpected type: '$tpe0'.")
  }

  /**
    * Returns true if the type constructor should be highlighted.
    * This is restricted to type constructors whose that use the standard shape (X[Y, Z]).
    */
  private def isVisibleTypeConstructor(tycon: TypeConstructor): Boolean = tycon match {
    case TypeConstructor.Unit => true
    case TypeConstructor.Null => true
    case TypeConstructor.Bool => true
    case TypeConstructor.Char => true
    case TypeConstructor.Float32 => true
    case TypeConstructor.Float64 => true
    case TypeConstructor.Int8 => true
    case TypeConstructor.Int16 => true
    case TypeConstructor.Int32 => true
    case TypeConstructor.Int64 => true
    case TypeConstructor.BigInt => true
    case TypeConstructor.Str => true
    case TypeConstructor.Arrow(_) => false
    case TypeConstructor.RecordRowEmpty => false
    case TypeConstructor.RecordRowExtend(_) => false
    case TypeConstructor.Record => false
    case TypeConstructor.SchemaRowEmpty => false
    case TypeConstructor.SchemaRowExtend(_) => false
    case TypeConstructor.Schema => false
    case TypeConstructor.Channel => true
    case TypeConstructor.Lazy => true
    case TypeConstructor.Tag(_, _) => false
    case TypeConstructor.KindedEnum(_, _) => true
    case TypeConstructor.Native(_) => true
    case TypeConstructor.ScopedArray => true
    case TypeConstructor.ScopedRef => true
    case TypeConstructor.Tuple(_) => false
    case TypeConstructor.Relation => false
    case TypeConstructor.Lattice => false
    case TypeConstructor.True => true
    case TypeConstructor.False => true
    case TypeConstructor.Not => false
    case TypeConstructor.And => false
    case TypeConstructor.Or => false
    case TypeConstructor.Region => false

    case TypeConstructor.UnkindedEnum(_) => throw InternalCompilerException("Unexpected unkinded type.")
    case TypeConstructor.UnappliedAlias(_) => throw InternalCompilerException("Unexpected unkinded type.")
  }

  /**
    * Returns all semantic tokens in the given type constraint `tc0`.
    */
  private def visitTypeConstraint(tc0: TypeConstraint): Iterator[SemanticToken] = tc0 match {
    case TypeConstraint(sym, arg, _) =>
      val o = SemanticTokenType.Class
      val t = SemanticToken(o, Nil, sym.loc)
      Iterator(t) ++ visitType(arg)
  }

  /**
    * Returns all semantic tokens in the given formal parameters `fparams0`.
    */
  private def visitFormalParams(fparams0: List[FormalParam]): Iterator[SemanticToken] =
    fparams0.foldLeft(Iterator.empty[SemanticToken]) {
      case (acc, fparam0) => acc ++ visitFormalParam(fparam0)
    }

  /**
    * Returns all semantic tokens in the given formal parameter `fparam0`.
    */
  private def visitFormalParam(fparam0: FormalParam): Iterator[SemanticToken] = fparam0 match {
    case FormalParam(sym, _, tpe, _) =>
      val o = getSemanticTokenType(sym, tpe)
      val t = SemanticToken(o, Nil, sym.loc)
      Iterator(t) ++ visitType(tpe)
  }

  /**
    * Returns all semantic tokens in the given type parameter `tparam0`.
    */
  private def visitTypeParams(tparams0: List[TypedAst.TypeParam]): Iterator[SemanticToken] =
    tparams0.foldLeft(Iterator.empty[SemanticToken]) {
      case (acc, tparam0) => acc ++ visitTypeParam(tparam0)
    }

  /**
    * Returns all semantic tokens in the given type parameter `tparam0`.
    */
  private def visitTypeParam(tparam0: TypedAst.TypeParam): Iterator[SemanticToken] = tparam0 match {
    case TypeParam(_, sym, _) =>
      val t = SemanticToken(SemanticTokenType.TypeParameter, Nil, sym.loc)
      Iterator(t)
  }

  /**
    * Returns all semantic tokens in the given constraint `constraint0`.
    */
  private def visitConstraint(constraint0: Constraint): Iterator[SemanticToken] = constraint0 match {
    case Constraint(_, head, body, _) =>
      visitHeadPredicate(head) ++ body.flatMap(visitBodyPredicate)
  }

  /**
    * Returns all semantic tokens in the given head predicate `h0`.
    */
  private def visitHeadPredicate(h0: TypedAst.Predicate.Head): Iterator[SemanticToken] = h0 match {
    case Head.Atom(pred, _, terms, _, _) =>
      val t = SemanticToken(SemanticTokenType.EnumMember, Nil, pred.loc)
      Iterator(t) ++ terms.flatMap(visitExp).iterator
  }

  /**
    * Returns all semantic tokens in the given body predicate `b0`.
    */
  private def visitBodyPredicate(b0: TypedAst.Predicate.Body): Iterator[SemanticToken] = b0 match {
    case Body.Atom(pred, _, _, _, terms, _, _) =>
      val t = SemanticToken(SemanticTokenType.EnumMember, Nil, pred.loc)
      Iterator(t) ++ terms.flatMap(visitPat).iterator

    case Body.Guard(exp, _) =>
      visitExp(exp)

    case Body.Loop(varSyms, exp, loc) =>
      val ts = varSyms.map(varSym => SemanticToken(SemanticTokenType.Variable, Nil, varSym.loc))
      visitExp(exp) ++ ts
  }

  /**
    * Returns the semantic token type associated with the given variable `sym` of the given type `tpe`.
    */
  private def getSemanticTokenType(sym: Symbol.VarSym, tpe: Type): SemanticTokenType = {
    if (boundByFormalParam(sym))
      SemanticTokenType.Parameter
    else if (isOperatorName(sym.text))
      SemanticTokenType.Operator
    else if (isFunctionType(tpe))
      SemanticTokenType.Function
    else
      SemanticTokenType.Variable
  }

  /**
    * Returns `true` if the given string `s` contains non-letter symbols.
    */
  private def isOperatorName(s: String): Boolean = s.forall(c => !Character.isLetter(c))

  /**
    * Returns `true` if the given type `tpe` is a function type.
    */
  private def isFunctionType(tpe: Type): Boolean = tpe.typeConstructor match {
    case Some(TypeConstructor.Arrow(_)) => true
    case _ => false
  }

  /**
    * Returns `true` if the given symbol `sym` is bound as a formal parameter.
    */
  private def boundByFormalParam(sym: Symbol.VarSym): Boolean = sym.boundBy match {
    case BoundBy.FormalParam => true
    case _ => false
  }

  /**
    * Returns the given `tokens` as an encoded list of integers.
    *
    * Inspired by https://github.com/microsoft/vscode-languageserver-node/blob/f425af9de46a0187adb78ec8a46b9b2ce80c5412/server/src/sematicTokens.proposed.ts#L45
    */
  private def encodeSemanticTokens(tokens: List[SemanticToken]): List[Int] = {
    val encoding = new ArrayBuffer[Int](initialSize = 5 * tokens.size)

    var prevLine = 0
    var prevCol = 0

    implicit val tokenOrdering: Ordering[SemanticToken] = Ordering.by(_.loc)
    for (token <- SortedSet.empty.concat(tokens)) {
      var relLine = token.loc.beginLine - 1
      var relCol = token.loc.beginCol - 1

      if (encoding.nonEmpty) {
        relLine -= prevLine
        if (relLine == 0) {
          relCol -= prevCol
        }
      }

      encoding += relLine
      encoding += relCol
      encoding += token.loc.endCol - token.loc.beginCol
      encoding += token.tpe.toInt
      encoding += encodeModifiers(token.mod)

      prevLine = token.loc.beginLine - 1
      prevCol = token.loc.beginCol - 1
    }

    encoding.toList
  }

  /**
    * Encodes a list of modifiers as a bitset (as per the LSP spec).
    */
  private def encodeModifiers(modifiers: List[SemanticTokenModifier]): Int =
    modifiers.foldLeft(0)((bitset, modifier) => bitset | (1 << modifier.toInt))
}

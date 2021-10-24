/*
 * Copyright 2021 Jacob Harris Cryer Kragh
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
import ca.uwaterloo.flix.language.ast.Ast.TypeConstraint
import ca.uwaterloo.flix.language.ast.{SourceLocation, Type, TypedAst}
import ca.uwaterloo.flix.language.ast.TypedAst.{CatchRule, Constraint, Expression, FormalParam, MatchRule, Pattern, Root, SelectChannelRule, TypeParam}
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
    // TODO: Replace by a call to check?
    if (root == null) {
      return ("status" -> "success") ~ ("result" -> ("data" -> Nil))
    }

    //
    // Construct an iterator for semantic tokens from classes.
    //
    val classTokens = root.classes.values.flatMap {
      case decl if include(uri, decl.loc) => visitClass(decl)
      case _ => Nil
    }

    //
    // Construct an iterator for semantic tokens from instances.
    //
    val st4: List[SemanticToken] = root.instances.filter(_._1.loc.source.name == uri).flatMap {
      case (_, instances) => instances.flatMap(visitInstance)
    }.toList

    //
    // Construct an iterator for semantic tokens from defs.
    //
    val defnTokens = root.defs.values.flatMap {
      case decl if include(uri, decl.sym.loc) => visitDef(decl)
      case _ => Nil
    }

    //
    // Construct an iterator for semantic tokens from sigs.
    //
    val sigsTokens = root.sigs.values.flatMap {
      case decl if include(uri, decl.spec.loc) => visitSig(decl)
      case _ => Nil
    }

    //
    // Construct an iterator for semantic tokens from enums.
    //
    val enumTokens = root.enums.values.flatMap {
      case decl if include(uri, decl.loc) => visitEnum(decl)
      case _ => Nil
    }

    //
    // Construct an iterator for semantic tokens from type aliases.
    //
    val typeAliasTokens = root.typealiases.flatMap {
      case (_, decl) if include(uri, decl.loc) => visitTypeAlias(decl)
      case _ => Nil
    }

    //
    // Encode all the semantic tokens as a list of integers.
    //
    val encodedTokens = encodeSemanticTokens((classTokens ++ st4 ++ defnTokens ++ enumTokens ++ typeAliasTokens).toList)

    //
    // Build the JSON result.
    //
    ("status" -> "success") ~ ("result" -> ("data" -> encodedTokens))
  }

  // TODO: DOC
  private def include(uri: String, loc: SourceLocation): Boolean = loc.source.name == uri

  /**
    * Returns all semantic tokens in the given class `classDecl`.
    */
  private def visitClass(classDecl: TypedAst.Class): Iterator[SemanticToken] = classDecl match {
    case TypedAst.Class(_, _, sym, tparam, superClasses, signatures, laws, _) =>
      // TODO: Superclasses, signatures and laws.
      val t = SemanticToken(SemanticTokenType.Interface, Nil, sym.loc)
      Iterator(t) ++ visitTypeParam(tparam)
  }

  /**
    * Returns all semantic tokens in the given instance `inst0`.
    */
  private def visitInstance(inst0: TypedAst.Instance): Iterator[SemanticToken] = inst0 match {
    case TypedAst.Instance(_, _, _, tpe, tconstrs, defs, _, _) =>
      // TODO: We cannot create a semantic token for the *USE* of the class symbol because we do not have its location.
      // TODO: We have the location of the class symbol, but not the location of the use.
      // val t = SemanticToken(SemanticTokenType.Class, Nil, sym.loc)
      val tokens1 = visitType(tpe)
      val tokens2 = tconstrs.flatMap(visitTypeConstraint)
      val tokens3 = defs.flatMap(visitDef)
      tokens1 ++ tokens2 ++ tokens3
  }

  /**
    * Returns all semantic tokens in the given enum `enum0`.
    *
    * Returns tokens for the symbol, the type parameters, and the cases.
    */
  private def visitEnum(enum0: TypedAst.Enum): Iterator[SemanticToken] = enum0 match {
    case TypedAst.Enum(_, _, sym, tparams, cases, _, _, _) =>
      val t = SemanticToken(SemanticTokenType.Enum, Nil, sym.loc)
      val ts1 = tparams.flatMap(visitTypeParam).iterator
      val ts2 = enum0.cases.foldLeft(Iterator.empty[SemanticToken]) {
        case (acc, (_, caze)) => acc ++ visitCase(caze)
      }
      Iterator(t) ++ ts1 ++ ts2
  }

  /**
    * Returns all semantic tokens in the given case `case0`.
    */
  private def visitCase(case0: TypedAst.Case): Iterator[SemanticToken] = case0 match {
    case TypedAst.Case(_, tag, _, sc, _) =>
      val t = SemanticToken(SemanticTokenType.EnumMember, Nil, tag.loc)
      Iterator(t) /* TODO: There is a bug where the type contains source locations corresponding to the enum. ++ visitType(sc.base) */
  }

  // TODO: DOC
  private def visitDef(defn0: TypedAst.Def): Iterator[SemanticToken] = {
    val t = SemanticToken(SemanticTokenType.Function, Nil, defn0.sym.loc)

    Iterator(t) ++
      visitFormalParams(defn0.spec.fparams) ++
      visitType(defn0.spec.retTpe) ++
      visitExp(defn0.impl.exp)
  }

  /**
    * Returns all semantic tokens in the given signature `sig0`.
    */
  private def visitSig(sig0: TypedAst.Sig): Iterator[SemanticToken] = sig0 match {
    case TypedAst.Sig(sym, spec, impl) => impl match {
      case None =>
        // No implementation. Just signature.
        val t = SemanticToken(SemanticTokenType.Function, Nil, sig0.sym.loc)
        Iterator(t) ++ visitFormalParams(sig0.spec.fparams) ++ visitType(sig0.spec.retTpe)

      case Some(i) =>
        // With implementation.
        val t = SemanticToken(SemanticTokenType.Function, Nil, sig0.sym.loc)
        Iterator(t) ++ visitFormalParams(sig0.spec.fparams) ++ visitType(sig0.spec.retTpe) ++ visitExp(i.exp)
    }
  }

  /**
    * Returns all semantic tokens in the given type alias `typeAlias0`.
    */
  private def visitTypeAlias(typeAlias0: TypedAst.TypeAlias): Iterator[SemanticToken] = typeAlias0 match {
    case TypedAst.TypeAlias(_, _, sym, tparams, tpe, _) =>
      val t = SemanticToken(SemanticTokenType.Type, Nil, sym.loc)
      Iterator(t) ++ visitTypeParams(tparams) ++ visitType(tpe)
  }

  /**
    * Returns all semantic tokens in the given expression `exp0`.
    */
  private def visitExp(exp0: Expression): Iterator[SemanticToken] = exp0 match {
    case Expression.Wild(_, _) => Iterator.empty

    case Expression.Var(sym, _, loc) =>
      val o = if (isOperator(sym.text)) SemanticTokenType.Operator else SemanticTokenType.Method
      val t = SemanticToken(o, Nil, loc)
      Iterator(t)

    case Expression.Def(sym, _, loc) =>
      val o = if (isOperator(sym.name)) SemanticTokenType.Operator else SemanticTokenType.Method
      val t = SemanticToken(o, Nil, loc)
      Iterator(t)

    case Expression.Sig(sym, _, loc) =>
      val o = if (isOperator(sym.name)) SemanticTokenType.Operator else SemanticTokenType.Method
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

    // TODO
    case Expression.Lambda(fparam, exp, tpe, loc) =>
      val env1 = Map(fparam.sym -> fparam.tpe)
      visitExp(exp)

    // TODO
    case Expression.Apply(exp, exps, tpe, eff, loc) =>
      val init = visitExp(exp)
      exps.foldLeft(init) {
        case (acc, exp) => acc ++ visitExp(exp)
      }

    case Expression.Unary(sop, exp, tpe, eff, loc) =>
      visitExp(exp)

    case Expression.Binary(sop, exp1, exp2, tpe, eff, loc) =>
      visitExp(exp1) ++ visitExp(exp2)

    case Expression.Let(sym, _, exp1, exp2, _, _, _) =>
      val t = SemanticToken(SemanticTokenType.Variable, Nil, sym.loc)
      Iterator(t) ++ visitExp(exp1) ++ visitExp(exp2)

    case Expression.LetRegion(sym, exp, _, _, _) =>
      val t = SemanticToken(SemanticTokenType.Variable, Nil, sym.loc)
      Iterator(t) ++ visitExp(exp)

    case Expression.IfThenElse(exp1, exp2, exp3, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2) ++ visitExp(exp3)

    case Expression.Stm(exp1, exp2, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2)

    case Expression.Match(matchExp, rules, _, _, _) =>
      val m = visitExp(matchExp)
      rules.foldLeft(m) {
        case (macc, MatchRule(pat, guard, exp)) =>
          macc ++ visitPat(pat) ++ visitExp(guard) ++ visitExp(exp)
      }

    case Expression.Choose(exps, rules, tpe, eff, loc) =>
      Iterator.empty // TODO

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

    case Expression.ArrayLit(exps, _, _, _) =>
      visitExps(exps)

    case Expression.ArrayNew(exp1, exp2, _, _, _) =>
      visitExp(exp1) ++ visitExp(exp2)

    // TODO: From here

    case Expression.ArrayLoad(base, index, tpe, eff, loc) =>
      visitExp(base) ++ visitExp(index)

    case Expression.ArrayStore(base, index, elm, loc) =>
      visitExp(base) ++ visitExp(index) ++ visitExp(elm)

    case Expression.ArrayLength(base, eff, loc) =>
      visitExp(base)

    case Expression.ArraySlice(base, beginIndex, endIndex, tpe, loc) =>
      visitExp(base) ++ visitExp(beginIndex) ++ visitExp(endIndex)

    case Expression.Ref(exp, tpe, eff, loc) =>
      visitExp(exp)

    case Expression.Deref(exp, tpe, eff, loc) =>
      visitExp(exp)

    case Expression.Assign(exp1, exp2, tpe, eff, loc) =>
      visitExp(exp1) ++ visitExp(exp2)

    case Expression.Existential(fparam, exp, loc) =>
      visitExp(exp)

    case Expression.Universal(fparam, exp, loc) =>
      visitExp(exp)

    case Expression.Ascribe(exp, tpe, eff, loc) =>
      visitExp(exp)

    case Expression.Cast(exp, tpe, eff, loc) =>
      visitExp(exp)

    case Expression.TryCatch(exp, rules, tpe, eff, loc) =>
      rules.foldLeft(visitExp(exp)) {
        case (macc, CatchRule(sym, clazz, body)) => macc ++ visitExp(body)
      }

    case Expression.InvokeConstructor(constructor, args, tpe, eff, loc) =>
      args.foldLeft(Iterator.empty[SemanticToken]) {
        case (macc, arg) => macc ++ visitExp(arg)
      }

    case Expression.InvokeMethod(method, exp, args, tpe, eff, loc) =>
      args.foldLeft(visitExp(exp)) {
        case (macc, arg) => macc ++ visitExp(arg)
      }

    case Expression.InvokeStaticMethod(method, args, tpe, eff, loc) =>
      args.foldLeft(Iterator.empty[SemanticToken]) {
        case (macc, arg) => macc ++ visitExp(arg)
      }

    case Expression.GetField(field, exp, tpe, eff, loc) =>
      visitExp(exp)

    case Expression.PutField(field, exp1, exp2, tpe, eff, loc) =>
      visitExp(exp1) ++ visitExp(exp2)

    case Expression.GetStaticField(field, tpe, eff, loc) =>
      Iterator.empty

    case Expression.PutStaticField(field, exp, tpe, eff, loc) =>
      visitExp(exp)

    case Expression.NewChannel(exp, tpe, eff, loc) => visitExp(exp)

    case Expression.GetChannel(exp, tpe, eff, loc) => visitExp(exp)

    case Expression.PutChannel(exp1, exp2, tpe, eff, loc) => visitExp(exp1) ++ visitExp(exp2)

    case Expression.SelectChannel(rules, default, tpe, eff, loc) =>
      val rs = rules.foldLeft(Iterator.empty[SemanticToken]) {
        case (macc, SelectChannelRule(sym, chan, exp)) => macc ++ visitExp(chan) ++ visitExp(exp)
      }

      val d = default.map(visitExp).getOrElse(Iterator.empty)
      rs ++ d

    // TODO> to here

    case Expression.Spawn(exp, _, _, _) => visitExp(exp)

    case Expression.Lazy(exp, _, _) => visitExp(exp)

    case Expression.Force(exp, _, _, _) => visitExp(exp)

    case Expression.FixpointConstraintSet(cs, _, _, _) =>
      cs.foldLeft(Iterator.empty[SemanticToken]) {
        case (macc, c) => macc ++ visitConstraint(c)
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

    case Pattern.Var(_, _, loc) =>
      val t = SemanticToken(SemanticTokenType.Variable, Nil, loc)
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
    case Type.KindedVar(_, _, loc, _, _) =>
      val t = SemanticToken(SemanticTokenType.TypeParameter, Nil, loc)
      Iterator(t)

    case Type.Ascribe(tpe, _, _) =>
      visitType(tpe)

    case Type.Cst(_, loc) =>
      val t = SemanticToken(SemanticTokenType.Type, Nil, loc)
      Iterator(t)

    case Type.Apply(tpe1, tpe2, _) =>
      visitType(tpe1) ++ visitType(tpe2)

    case Type.Alias(_, args, _, _) =>
      args.flatMap(visitType).iterator

    case Type.UnkindedVar(_, _, _, _) =>
      throw InternalCompilerException(s"Unexpected type: '$tpe0'.")
  }

  /**
    * Returns all semantic tokens in the given type constraint `tc0`.
    */
  private def visitTypeConstraint(tc0: TypeConstraint): Iterator[SemanticToken] = tc0 match {
    case TypeConstraint(_, arg, _) => visitType(arg)
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
      val t = SemanticToken(SemanticTokenType.Parameter, Nil, sym.loc)
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
    case TypeParam(ident, _, _) =>
      val t = SemanticToken(SemanticTokenType.Type, Nil, ident.loc)
      Iterator(t)
  }

  // TODO: DOC
  private def visitConstraint(c: Constraint): Iterator[SemanticToken] = Iterator.empty // TODO

  /**
    * Returns `true` if the given string `s` contains non-letter symbols.
    */
  private def isOperator(s: String): Boolean = s.forall(c => !Character.isLetter(c))

  // TODO: DOC
  // Inspired by https://github.com/microsoft/vscode-languageserver-node/blob/f425af9de46a0187adb78ec8a46b9b2ce80c5412/server/src/sematicTokens.proposed.ts#L45
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
  def encodeModifiers(modifiers: List[SemanticTokenModifier]): Int =
    modifiers.foldLeft(0)((bitset, modifier) => bitset | (1 << modifier.toInt))
}

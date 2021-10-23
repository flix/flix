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
import ca.uwaterloo.flix.language.ast.{Symbol, Type}
import ca.uwaterloo.flix.language.ast.TypedAst.{CatchRule, ChoicePattern, ChoiceRule, Constraint, Def, Expression, MatchRule, Root, SelectChannelRule}
import org.json4s.JsonAST.JObject
import org.json4s.JsonDSL._

import scala.collection.immutable.SortedSet
import scala.collection.mutable.ArrayBuffer

object SemanticTokensProvider {

  /**
    * Processes a request for (full) semantic tokens.
    */
  def provideSemanticTokens(uri: String)(implicit index: Index, root: Root): JObject = {
    val entities = index.query(uri)
    //val semanticTokens = entities.flatMap(getSemanticTokens)
    val semanticTokens = root.defs.filter(_._1.loc.source.name == uri).flatMap {
      case (_, defn) => visitDef(defn)
    }.toList
    val encoding = encodeSemanticTokens(semanticTokens)
    val result = ("data" -> encoding)
    ("status" -> "success") ~ ("result" -> result)
  }

  /**
    * Returns the semantic tokens in the given Entity.
    */
  private def getSemanticTokens(entity: Entity): List[SemanticToken] = entity match {
    case Entity.Exp(e) => e match {
      case Expression.Tag(_, tag, _, _, _, _) => List(SemanticToken(tag.loc, SemanticTokenType.EnumMember, List()))
      case _ => List() // TODO: Handle other kinds of expressions
    }
    case _ => List() // TODO: Handle other kinds of entities
  }

  private def visitDef(defn0: Def): Iterator[SemanticToken] = {
    visitExp(defn0.impl.exp, Map.empty)
  }

  /**
    * Returns all semantic tokens in the given expression `exp0`.
    */
  private def visitExp(exp0: Expression, env0: Map[Symbol.VarSym, Type]): Iterator[SemanticToken] = exp0 match {
    case Expression.Wild(_, _) => Iterator.empty

    case Expression.Var(_, _, loc) => Iterator(SemanticToken(loc, SemanticTokenType.Var, List()))

    case Expression.Def(_, _, _) =>  Iterator.empty

    case Expression.Sig(_, _, _) => Iterator.empty

    case Expression.Hole(sym, tpe, loc) => Iterator.empty

    case Expression.Unit(loc) => Iterator.empty

    case Expression.Null(tpe, loc) => Iterator.empty

    case Expression.True(loc) => Iterator.empty

    case Expression.False(loc) => Iterator.empty

    case Expression.Char(lit, loc) => Iterator.empty

    case Expression.Float32(lit, loc) => Iterator.empty

    case Expression.Float64(lit, loc) => Iterator.empty

    case Expression.Int8(lit, loc) => Iterator.empty

    case Expression.Int16(lit, loc) => Iterator.empty

    case Expression.Int32(lit, loc) => Iterator.empty

    case Expression.Int64(lit, loc) => Iterator.empty

    case Expression.BigInt(lit, loc) => Iterator.empty

    case Expression.Str(lit, loc) => Iterator.empty

    case Expression.Default(tpe, loc) => Iterator.empty

    case Expression.Lambda(fparam, exp, tpe, loc) =>
      val env1 = Map(fparam.sym -> fparam.tpe)
      visitExp(exp, env0 ++ env1)

    case Expression.Apply(exp, exps, tpe, eff, loc) =>
      val init = visitExp(exp, env0)
      exps.foldLeft(init) {
        case (acc, exp) => acc ++ visitExp(exp, env0)
      }

    case Expression.Unary(sop, exp, tpe, eff, loc) =>
      visitExp(exp, env0)

    case Expression.Binary(sop, exp1, exp2, tpe, eff, loc) =>
      visitExp(exp1, env0) ++ visitExp(exp2, env0)

    case Expression.Let(sym, _, exp1, exp2, tpe, eff, loc) =>
      visitExp(exp1, env0) ++ visitExp(exp2, env0 + (sym -> exp1.tpe))

    case Expression.LetRegion(_, exp, _, _, _) =>
      visitExp(exp, env0)

    case Expression.IfThenElse(exp1, exp2, exp3, tpe, eff, loc) =>
      visitExp(exp1, env0) ++ visitExp(exp2, env0) ++ visitExp(exp3, env0)

    case Expression.Stm(exp1, exp2, tpe, eff, loc) =>
      visitExp(exp1, env0) ++ visitExp(exp2, env0)

    case Expression.Match(matchExp, rules, tpe, eff, loc) =>
      val m = visitExp(matchExp, env0)
      rules.foldLeft(m) {
        case (macc, MatchRule(pat, guard, exp)) =>
          macc ++ visitExp(guard, env0) ++ visitExp(exp, Map.empty)
      }

    case Expression.Choose(exps, rules, tpe, eff, loc) =>
      Iterator.empty // TODO

    case Expression.Tag(sym, tag, exp, tpe, eff, loc) =>
      visitExp(exp, env0)

    case Expression.Tuple(elms, tpe, eff, loc) =>
      elms.foldLeft(Iterator.empty[SemanticToken]) {
        case (macc, elm) => macc ++ visitExp(elm, env0)
      }

    case Expression.RecordEmpty(tpe, loc) => Iterator.empty

    case Expression.RecordSelect(base, _, tpe, eff, loc) =>
      visitExp(base, env0)

    case Expression.RecordExtend(_, value, rest, tpe, eff, loc) =>
      visitExp(rest, env0) ++ visitExp(value, env0)

    case Expression.RecordRestrict(_, rest, tpe, eff, loc) =>
      visitExp(rest, env0)

    case Expression.ArrayLit(elms, tpe, eff, loc) =>
      elms.foldLeft(Iterator.empty[SemanticToken]) {
        case (macc, elm) => macc ++ visitExp(elm, env0)
      }

    case Expression.ArrayNew(elm, len, tpe, eff, loc) =>
      visitExp(elm, env0)

    case Expression.ArrayLoad(base, index, tpe, eff, loc) =>
      visitExp(base, env0) ++ visitExp(index, env0)

    case Expression.ArrayStore(base, index, elm, loc) =>
      visitExp(base, env0) ++ visitExp(index, env0) ++ visitExp(elm, env0)

    case Expression.ArrayLength(base, eff, loc) =>
      visitExp(base, env0)

    case Expression.ArraySlice(base, beginIndex, endIndex, tpe, loc) =>
      visitExp(base, env0) ++ visitExp(beginIndex, env0) ++ visitExp(endIndex, env0)

    case Expression.Ref(exp, tpe, eff, loc) =>
      visitExp(exp, env0)

    case Expression.Deref(exp, tpe, eff, loc) =>
      visitExp(exp, env0)

    case Expression.Assign(exp1, exp2, tpe, eff, loc) =>
      visitExp(exp1, env0) ++ visitExp(exp2, env0)

    case Expression.Existential(fparam, exp, loc) =>
      visitExp(exp, env0 + (fparam.sym -> fparam.tpe))

    case Expression.Universal(fparam, exp, loc) =>
      visitExp(exp, env0 + (fparam.sym -> fparam.tpe))

    case Expression.Ascribe(exp, tpe, eff, loc) =>
      visitExp(exp, env0)

    case Expression.Cast(exp, tpe, eff, loc) =>
      visitExp(exp, env0)

    case Expression.TryCatch(exp, rules, tpe, eff, loc) =>
      rules.foldLeft(visitExp(exp, env0)) {
        case (macc, CatchRule(sym, clazz, body)) => macc ++ visitExp(body, env0 + (sym -> Type.mkNative(null, loc)))
      }

    case Expression.InvokeConstructor(constructor, args, tpe, eff, loc) =>
      args.foldLeft(Iterator.empty[SemanticToken]) {
        case (macc, arg) => macc ++ visitExp(arg, env0)
      }

    case Expression.InvokeMethod(method, exp, args, tpe, eff, loc) =>
      args.foldLeft(visitExp(exp, env0)) {
        case (macc, arg) => macc ++ visitExp(arg, env0)
      }

    case Expression.InvokeStaticMethod(method, args, tpe, eff, loc) =>
      args.foldLeft(Iterator.empty[SemanticToken]) {
        case (macc, arg) => macc ++ visitExp(arg, env0)
      }

    case Expression.GetField(field, exp, tpe, eff, loc) =>
      visitExp(exp, env0)

    case Expression.PutField(field, exp1, exp2, tpe, eff, loc) =>
      visitExp(exp1, env0) ++ visitExp(exp2, env0)

    case Expression.GetStaticField(field, tpe, eff, loc) =>
      Iterator.empty

    case Expression.PutStaticField(field, exp, tpe, eff, loc) =>
      visitExp(exp, env0)

    case Expression.NewChannel(exp, tpe, eff, loc) => visitExp(exp, env0)

    case Expression.GetChannel(exp, tpe, eff, loc) => visitExp(exp, env0)

    case Expression.PutChannel(exp1, exp2, tpe, eff, loc) => visitExp(exp1, env0) ++ visitExp(exp2, env0)

    case Expression.SelectChannel(rules, default, tpe, eff, loc) =>
      val rs = rules.foldLeft(Iterator.empty[SemanticToken]) {
        case (macc, SelectChannelRule(sym, chan, exp)) => macc ++ visitExp(chan, env0) ++ visitExp(exp, env0)
      }

      val d = default.map(visitExp(_, env0)).getOrElse(Iterator.empty)

      rs ++ d

    case Expression.Spawn(exp, tpe, eff, loc) => visitExp(exp, env0)

    case Expression.Lazy(exp, tpe, loc) => visitExp(exp, env0)

    case Expression.Force(exp, tpe, eff, loc) => visitExp(exp, env0)

    case Expression.FixpointConstraintSet(cs, stf, tpe, loc) =>
      cs.foldLeft(Iterator.empty[SemanticToken]) {
        case (macc, c) => macc ++ visitConstraint(c)
      }

    case Expression.FixpointMerge(exp1, exp2, stf, tpe, eff, loc) =>
      visitExp(exp1, env0) ++ visitExp(exp2, env0)

    case Expression.FixpointSolve(exp, stf, tpe, eff, loc) =>
      visitExp(exp, env0)

    case Expression.FixpointFilter(_, exp, tpe, eff, loc) =>
      visitExp(exp, env0)

    case Expression.FixpointProjectIn(exp, _, tpe, eff, loc) =>
      visitExp(exp, env0)

    case Expression.FixpointProjectOut(_, exp, tpe, eff, loc) =>
      visitExp(exp, env0)

    case Expression.Reify(_, _, _, _) =>  Iterator.empty

    case Expression.ReifyType(_, _, _, _, _) =>  Iterator.empty

  }

  private def visitConstraint(c: Constraint): Iterator[SemanticToken] = Iterator.empty // TODO

  // Inspired by https://github.com/microsoft/vscode-languageserver-node/blob/f425af9de46a0187adb78ec8a46b9b2ce80c5412/server/src/sematicTokens.proposed.ts#L45
  private def encodeSemanticTokens(tokens: Iterable[SemanticToken]): List[Int] = {
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
      encoding += token.tokenType.toInt
      encoding += encodeModifiers(token.tokenModifiers)

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

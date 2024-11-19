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
package ca.uwaterloo.flix.api.lsp.provider

import ca.uwaterloo.flix.api.lsp.*
import ca.uwaterloo.flix.language.ast.TypedAst.{Pattern, Root}
import ca.uwaterloo.flix.language.ast.shared.SymUse
import ca.uwaterloo.flix.language.ast.shared.SymUse.CaseSymUse
import ca.uwaterloo.flix.language.ast.{Ast, Symbol, Type, TypeConstructor, TypedAst}
import org.json4s.JsonAST.JObject
import org.json4s.JsonDSL.*

object GotoProvider {

  /**
   * Processes a goto request.
   */
  def processGoto(uri: String, pos: Position)(implicit index: Index, root: Root): JObject = {
    val consumer = StackConsumer();

    Visitor.visitRoot(root, consumer, Visitor.InsideAcceptor(uri, pos))

    consumer.getStack.filter(isReal).headOption match {
      case None => mkNotFound(uri, pos)
      case Some(x) => goto(x, mkNotFound(uri, pos))
    }
  }

  private def goto(x: AnyRef, default: JObject)(implicit root: Root): JObject = x match {
    case SymUse.DefSymUse(sym, loc) => mkGoto(LocationLink.fromDefSym(sym, loc))
    case SymUse.SigSymUse(sym, loc) => mkGoto(LocationLink.fromSigSym(sym, loc))
    case SymUse.OpSymUse(sym, loc) => mkGoto(LocationLink.fromOpSym(sym, loc))
    // Enums
    case Type.Cst(TypeConstructor.Enum(sym, _), loc) => mkGoto(LocationLink.fromEnumSym(sym, loc))
    case SymUse.CaseSymUse(sym, loc) => mkGoto(LocationLink.fromCaseSym(sym, loc))

    case SymUse.StructFieldSymUse(sym, loc) => mkGoto(LocationLink.fromStructFieldSym(sym, loc))
    // Effects
    case SymUse.EffectSymUse(sym, loc) => mkGoto(LocationLink.fromEffectSym(sym, loc))
    case Type.Cst(TypeConstructor.Effect(sym), loc) => mkGoto(LocationLink.fromEffectSym(sym, loc))
    case _ => default
  }

  private def isReal(x: AnyRef): Boolean = x match {
    case TypedAst.Trait(_, _, _, _, _, _, _, _, _, loc) =>  loc.isReal
    case TypedAst.Instance(_, _, _, _, _, _, _, _, _, loc) => loc.isReal
    case TypedAst.Sig(_, _, _, loc) => loc.isReal
    case TypedAst.Def(_, _, _, loc) => loc.isReal
    case TypedAst.Enum(_, _, _, _, _, _, _, loc) => loc.isReal
    case TypedAst.Struct(_, _, _, _, _, _, _, loc) => loc.isReal
    case TypedAst.RestrictableEnum(_, _, _, _, _, _, _, _, loc) => loc.isReal
    case TypedAst.TypeAlias(_, _, _, _, _, _, loc) => loc.isReal
    case TypedAst.AssocTypeSig(_, _, _, _, _, _, loc) => loc.isReal
    case TypedAst.AssocTypeDef(_, _, _, _, _, loc) => loc.isReal
    case TypedAst.Effect(_, _, _, _, _, loc) => loc.isReal
    case TypedAst.Op(_, _, loc) => loc.isReal
    case exp: TypedAst.Expr => exp.loc.isReal
    case pat: TypedAst.Pattern => pat.loc.isReal
    case TypedAst.RestrictableChoosePattern.Wild(_, loc) => loc.isReal
    case TypedAst.RestrictableChoosePattern.Var(_, _, loc) => loc.isReal
    case TypedAst.RestrictableChoosePattern.Tag(_, _, _, loc) => loc.isReal
    case TypedAst.RestrictableChoosePattern.Error(_, loc) => loc.isReal
    case p: TypedAst.Predicate => p.loc.isReal
    case TypedAst.Binder(sym, _) => sym.loc.isReal
    case TypedAst.Case(_, _, _, loc) => loc.isReal
    case TypedAst.StructField(_, _, loc) => loc.isReal
    case TypedAst.RestrictableCase(_, _, _, loc) => loc.isReal
    case TypedAst.Constraint(_, _, _, loc) => loc.isReal
    case TypedAst.ConstraintParam(_, _, loc) => loc.isReal
    case TypedAst.FormalParam(_, _, _, _, loc) => loc.isReal
    case TypedAst.PredicateParam(_, _, loc) => loc.isReal
    case TypedAst.JvmMethod(_, _, _, _, _, loc) => loc.isReal
    case TypedAst.CatchRule(_, _, _) => true
    case TypedAst.HandlerRule(_, _, _) => true
    case TypedAst.TypeMatchRule(_, _, _) => true
    case TypedAst.SelectChannelRule(_, _, _) => true
    case TypedAst.TypeParam(_, _, loc) => loc.isReal
    case TypedAst.ParYieldFragment(_, _, loc) => loc.isReal

    case SymUse.AssocTypeSymUse(_, loc) => loc.isReal
    case SymUse.CaseSymUse(_, loc) => loc.isReal
    case SymUse.DefSymUse(_, loc) => loc.isReal
    case SymUse.EffectSymUse(_, loc) => loc.isReal
    case SymUse.LocalDefSymUse(_, loc) => loc.isReal
    case SymUse.OpSymUse(_, loc) => loc.isReal
    case SymUse.RestrictableCaseSymUse(_, loc) => loc.isReal
    case SymUse.RestrictableEnumSymUse(_, loc) => loc.isReal
    case SymUse.SigSymUse(_, loc) => loc.isReal
    case SymUse.StructFieldSymUse(_, loc) => loc.isReal
    case SymUse.TraitSymUse(_, loc) => loc.isReal

    case _: Symbol => true
    case tpe: Type => tpe.loc.isReal
    case _ => false
  }

  /**
    * Returns a succesful Goto reply containing the given [[LocationLink]] `link`
    *
    * @param  link [[LocationLink]] that the reply should contain.
    * @return succesful Goto reply containing `link`.
    */
  private def mkGoto(link: LocationLink): JObject = {
    ("status" -> ResponseStatus.Success) ~ ("result" -> link.toJSON)
  }

  /**
   * Returns a reply indicating that nothing was found at the `uri` and `pos`.
   */
  private def mkNotFound(uri: String, pos: Position): JObject =
    ("status" -> ResponseStatus.InvalidRequest) ~ ("message" -> s"Nothing found in '$uri' at '$pos'.")

}

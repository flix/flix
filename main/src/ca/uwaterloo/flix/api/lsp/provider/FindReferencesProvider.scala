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

import ca.uwaterloo.flix.api.lsp.acceptors.{AllAcceptor, InsideAcceptor}
import ca.uwaterloo.flix.api.lsp.consumers.StackConsumer
import ca.uwaterloo.flix.api.lsp.*
import ca.uwaterloo.flix.language.ast.TypedAst.Root
import ca.uwaterloo.flix.language.ast.shared.{EqualityConstraint, SymUse, TraitConstraint}
import ca.uwaterloo.flix.language.ast.{Ast, SourceLocation, Symbol, Type, TypeConstructor, TypedAst}
import org.json4s.JsonAST.JObject
import org.json4s.JsonDSL.*

object FindReferencesProvider {

  def findRefs(uri: String, pos: Position)(implicit root: Root): JObject = {
    val left = searchLeftOfCursor(uri, pos)
    val right = searchRightOfCursor(uri, pos)

    right
      .orElse(left)
      .flatMap(getOccurs)
      .map(mkResponse)
      .getOrElse(mkNotFound(uri, pos))
  }

  private def searchLeftOfCursor(uri: String, pos: Position)(implicit root: Root): Option[AnyRef] = {
    if (pos.character >= 2) {
      val left = Position(pos.line, pos.character - 1)
      search(uri, left)
    } else {
      search(uri, pos)
    }
  }

  private def searchRightOfCursor(uri: String, pos: Position)(implicit root: Root): Option[AnyRef] = search(uri, pos)

  private def search(uri: String, pos: Position)(implicit root: Root): Option[AnyRef] = {
    val consumer = StackConsumer()
    Visitor.visitRoot(root, consumer, InsideAcceptor(uri, pos))
    consumer.getStack.filter(isReal).headOption
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

    case Ast.AssocTypeConstructor(_, loc) => loc.isReal
    case EqualityConstraint(_, _, _, loc) => loc.isReal
    case TraitConstraint(_, _, loc) => loc.isReal
    case TraitConstraint.Head(_, loc) => loc.isReal

    case tpe: Type => tpe.loc.isReal
    case _ => false
  }

  private def getOccurs(x: AnyRef)(implicit root: Root): Option[Set[SourceLocation]] = x match {
    // Defs
    case TypedAst.Def(sym, _, _, _) => Some(getDefnSymOccurs(sym))
    case SymUse.DefSymUse(sym, _) => Some(getDefnSymOccurs(sym))
    // Enums
    case TypedAst.Enum(_, _, _, sym, _, _, _, _) => Some(getEnumSymOccurs(sym))
    case Type.Cst(TypeConstructor.Enum(sym, _), _) => Some(getEnumSymOccurs(sym))
    // Cases
    case TypedAst.Case(sym, _, _, _) => Some(getCaseSymOccurs(sym))
    case SymUse.CaseSymUse(sym, _) => Some(getCaseSymOccurs(sym))
    // Traits
    case TypedAst.Trait(_, _, _, sym, _, _, _, _, _, _) => Some(getTraitSymOccurs(sym))
    case SymUse.TraitSymUse(sym, _) => Some(getTraitSymOccurs(sym))
    case TraitConstraint.Head(sym, _) => Some(getTraitSymOccurs(sym))
    // Sigs
    case TypedAst.Sig(sym, _, _, _) => Some(getSigSymOccurs(sym))
    case SymUse.SigSymUse(sym, _) => Some(getSigSymOccurs(sym))
    // Effect
    case TypedAst.Effect(_, _, _, sym, _, _) => Some(getEffectSymOccurs(sym))
    case SymUse.EffectSymUse(sym, _) => Some(getEffectSymOccurs(sym))
    case Type.Cst(TypeConstructor.Effect(sym), _) => Some(getEffectSymOccurs(sym))

    case _ => None
  }

  private def getEffectSymOccurs(sym: Symbol.EffectSym)(implicit root: Root): Set[SourceLocation] = {
    var occurs: Set[SourceLocation] = Set.empty

    def consider(s: Symbol.EffectSym, loc: SourceLocation): Unit = {
      if (s == sym) { occurs += loc }
    }

    object EffectSymConsumer extends Consumer {
      override def consumeEff(eff: TypedAst.Effect): Unit = consider(eff.sym, eff.sym.loc)
      override def consumeEffectSymUse(effUse: SymUse.EffectSymUse): Unit = consider(effUse.sym, effUse.loc)
      override def consumeType(tpe: Type): Unit = tpe match {
        case Type.Cst(TypeConstructor.Effect(sym), loc) => consider(sym, loc)
        case _ => ()
      }
    }

    Visitor.visitRoot(root, EffectSymConsumer, AllAcceptor)

    occurs
  }

  private def getSigSymOccurs(sym: Symbol.SigSym)(implicit root: Root): Set[SourceLocation] = {
    var occurs: Set[SourceLocation] = Set.empty

    def consider(s: Symbol.SigSym, loc: SourceLocation): Unit = {
      if (s == sym) { occurs += loc }
    }

    object SigSymConsumer extends Consumer {
      override def consumeSig(sig: TypedAst.Sig): Unit = consider(sig.sym, sig.sym.loc)
      override def consumeSigSymUse(symUse: SymUse.SigSymUse): Unit = consider(symUse.sym, symUse.loc)
    }

    Visitor.visitRoot(root, SigSymConsumer, AllAcceptor)

    val implOccurs = root.instances(sym.trt)
      .flatMap(_.defs)
      .filter(defn => defn.sym.text == sym.name)
      .map(_.sym.loc)

    occurs ++ implOccurs
  }

  private def getTraitSymOccurs(sym: Symbol.TraitSym)(implicit root: Root): Set[SourceLocation] = {
    var occurs: Set[SourceLocation] = Set.empty

    def consider(s: Symbol.TraitSym, loc: SourceLocation): Unit = {
      if (s == sym) { occurs += loc }
    }

    object TraitSymConsumer extends Consumer {
      override def consumeTrait(traitt: TypedAst.Trait): Unit = consider(traitt.sym, traitt.sym.loc)
      override def consumeTraitSymUse(symUse: SymUse.TraitSymUse): Unit = consider(symUse.sym, symUse.loc)
      override def consumeTraitConstraintHead(tcHead: TraitConstraint.Head): Unit = consider(tcHead.sym, tcHead.loc)
    }

    Visitor.visitRoot(root, TraitSymConsumer, AllAcceptor)

    occurs
  }

  private def getCaseSymOccurs(sym: Symbol.CaseSym)(implicit root: Root): Set[SourceLocation] = {
    var occurs: Set[SourceLocation] = Set.empty

    def consider(s: Symbol.CaseSym, loc: SourceLocation): Unit = {
      if (s == sym) { occurs += loc }
    }

    object CaseSymConsumer extends Consumer {
      override def consumeCase(cse: TypedAst.Case): Unit = consider(cse.sym, cse.sym.loc)
      override def consumeCaseSymUse(sym: SymUse.CaseSymUse): Unit = consider(sym.sym, sym.loc)
    }

    Visitor.visitRoot(root, CaseSymConsumer, AllAcceptor)

    occurs
  }

  private def getEnumSymOccurs(sym: Symbol.EnumSym)(implicit root: Root): Set[SourceLocation] = {
    var occurs: Set[SourceLocation] = Set.empty

    def consider(s: Symbol.EnumSym, loc: SourceLocation): Unit = {
      if (s == sym) { occurs += loc }
    }

    object EnumSymConsumer extends Consumer {
      override def consumeEnum(enm: TypedAst.Enum): Unit = consider(enm.sym, enm.sym.loc)
      override def consumeType(tpe: Type): Unit = tpe match {
        case Type.Cst(TypeConstructor.Enum(sym, _), loc) => consider(sym, loc)
        case _ => ()
      }
    }

    Visitor.visitRoot(root, EnumSymConsumer, AllAcceptor)

    occurs
  }

  private def getDefnSymOccurs(sym: Symbol.DefnSym)(implicit root: Root): Set[SourceLocation] = {
    var occurs: Set[SourceLocation] = Set.empty

    def consider(s: Symbol.DefnSym, loc: SourceLocation): Unit = {
      if (s == sym) { occurs += loc }
    }

    object DefnSymConsumer extends Consumer {
      override def consumeDef(defn: TypedAst.Def): Unit = consider(defn.sym, defn.sym.loc)
      override def consumeDefSymUse(sym: SymUse.DefSymUse): Unit = consider(sym.sym, sym.loc)
    }

    Visitor.visitRoot(root, DefnSymConsumer, AllAcceptor)

    occurs
  }

  private def mkResponse(refs: Set[SourceLocation]): JObject = {
    ("status" -> ResponseStatus.Success) ~ ("result" -> refs.map(Location.from).map(_.toJSON))
  }

  /**
    * Returns a reply indicating that nothing was found at the `uri` and `pos`.
    */
  private def mkNotFound(uri: String, pos: Position): JObject =
    ("status" -> ResponseStatus.InvalidRequest) ~ ("message" -> s"Nothing found in '$uri' at '$pos'.")

}

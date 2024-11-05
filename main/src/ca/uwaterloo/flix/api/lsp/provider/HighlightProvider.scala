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

import ca.uwaterloo.flix.api.lsp.Visitor.Consumer
import ca.uwaterloo.flix.api.lsp.{DocumentHighlight, DocumentHighlightKind, Position, Range, ResponseStatus, StackConsumer, Visitor}
import ca.uwaterloo.flix.language.ast.TypedAst.{Binder, Expr, Root}
import ca.uwaterloo.flix.language.ast.shared.SymUse.CaseSymUse
import ca.uwaterloo.flix.language.ast.shared.{SymUse, TraitConstraint}
import ca.uwaterloo.flix.language.ast.{Ast, SourceLocation, Symbol, Type, TypeConstructor, TypedAst}
import org.json4s.JsonAST.{JArray, JObject}
import org.json4s.JsonDSL.*

object HighlightProvider {

  def processHighlight(uri: String, pos: Position)(implicit root: Root): JObject = {
    val stackConsumer = StackConsumer()
    Visitor.visitRoot(root, stackConsumer, Visitor.InsideAcceptor(uri, pos))


    stackConsumer.getStack.headOption match {
      case None => mkNotFound(uri, pos)
      case Some(x) => println(x); highlightAny(x, uri, pos)
    }

  }

  private def highlightAny(x: AnyRef, uri: String, pos: Position)(implicit root: Root): JObject = {
    implicit val acceptor: Visitor.Acceptor = Visitor.FileAcceptor(uri)
    x match {
      // Defs
      case TypedAst.Def(sym, _, _, _) => highlightDefnSym(sym)
      case SymUse.DefSymUse(sym, _) => highlightDefnSym(sym)
      // Effects
      case TypedAst.Effect(_, _, _, sym, _, _) => highlightEffectSym(sym)
      case Type.Cst(TypeConstructor.Effect(sym), _) => highlightEffectSym(sym)
      case SymUse.EffectSymUse(sym, _) => highlightEffectSym(sym)
      // Enums & Cases
      case TypedAst.Enum(_, _, _, sym, _, _, _, _) => highlightEnumSym(sym)
      case Type.Cst(TypeConstructor.Enum(sym, _), _) => highlightEnumSym(sym)
      case TypedAst.Case(sym, _, _, _) => highlightCaseSym(sym)
      case SymUse.CaseSymUse(sym, _) => highlightCaseSym(sym)
      // Signatures
      case TypedAst.Sig(sym, _, _, _) => highlightSigSym(sym)
      case SymUse.SigSymUse(sym, _) => highlightSigSym(sym)
      // Structs
      case TypedAst.Struct(_, _, _, sym, _, _, _, _) => highlightStructSym(sym)
      case Type.Cst(TypeConstructor.Struct(sym, _), _) => highlightStructSym(sym)
      case TypedAst.StructField(sym, _, _) => highlightStructFieldSym(sym)
      case SymUse.StructFieldSymUse(sym, _) => highlightStructFieldSym(sym)
      // Traits
      case TypedAst.Trait(_, _, _, sym, _, _, _, _, _, _) => highlightTraitSym(sym)
      case SymUse.TraitSymUse(sym, _) => highlightTraitSym(sym)
      case TraitConstraint.Head(sym, _) => highlightTraitSym(sym)
      // Type Aliases
      case TypedAst.TypeAlias(_, _, _, sym, _, _, _) => highlightTypeAliasSym(sym)
      case Type.Alias(Ast.AliasConstructor(sym, _), _, _, _) => highlightTypeAliasSym(sym)
      // Variables
      case Binder(sym, _) => highlightVarSym(sym)
      case TypedAst.Expr.Var(varSym, _, _) => highlightVarSym(varSym)

      case _ => mkNotFound(uri, pos)
    }
  }

  private case class HighlightBuilder[T <: Symbol](sym: T) {
    private var writes: List[SourceLocation] = Nil
    private var reads: List[SourceLocation] = Nil

    def considerWrite(x: T, loc: SourceLocation): Unit = {
      if (x == sym) {
        writes = loc :: writes
      }
    }
    def considerRead(x: T, loc: SourceLocation): Unit = {
      if (x == sym) {
        reads = loc :: reads
      }
    }

    def build: JObject = {
      val writeHighlights = writes.map(loc => DocumentHighlight(Range.from(loc), DocumentHighlightKind.Write))
      val readHighlights = reads.map(loc => DocumentHighlight(Range.from(loc), DocumentHighlightKind.Read))

      val highlights = writeHighlights ++ readHighlights

      ("status" -> ResponseStatus.Success) ~ ("result" -> JArray(highlights.map(_.toJSON)))
    }
  }

  private def highlightCaseSym(sym: Symbol.CaseSym)(implicit root: Root, acceptor: Visitor.Acceptor): JObject = {
    val builder = HighlightBuilder(sym)

    object CaseSymConsumer extends Consumer {
      override def consumeCase(cse: TypedAst.Case): Unit = builder.considerWrite(cse.sym, cse.sym.loc)
      override def consumeCaseSymUse(sym: CaseSymUse): Unit = builder.considerRead(sym.sym, sym.loc)
    }

    Visitor.visitRoot(root, CaseSymConsumer, acceptor)

    builder.build
  }

  private def highlightDefnSym(sym: Symbol.DefnSym)(implicit root: Root, acceptor: Visitor.Acceptor): JObject = {
    val builder = HighlightBuilder(sym)

    object DefnSymConsumer extends Consumer {
      override def consumeDef(defn: TypedAst.Def): Unit = builder.considerWrite(defn.sym, defn.sym.loc)
      override def consumeDefSymUse(sym: SymUse.DefSymUse): Unit = builder.considerRead(sym.sym, sym.loc)
    }

    Visitor.visitRoot(root, DefnSymConsumer, acceptor)

    builder.build
  }

  private def highlightEffectSym(sym: Symbol.EffectSym)(implicit root: Root, acceptor: Visitor.Acceptor): JObject = {
    val builder = HighlightBuilder(sym)

    object EffectSymConsumer extends Consumer {
      override def consumeEff(eff: TypedAst.Effect): Unit = builder.considerWrite(eff.sym, eff.sym.loc)
      override def consumeEffectSymUse(effUse: SymUse.EffectSymUse): Unit = builder.considerRead(effUse.sym, effUse.loc)
      override def consumeType(tpe: Type): Unit = tpe match {
        case Type.Cst(TypeConstructor.Effect(sym), loc) => builder.considerRead(sym, loc)
        case _ => ()
      }
    }

    Visitor.visitRoot(root, EffectSymConsumer, acceptor)

    builder.build
  }

  private def highlightEnumSym(sym: Symbol.EnumSym)(implicit root: Root, acceptor: Visitor.Acceptor): JObject = {
    val builder = HighlightBuilder(sym)

    object EnumSymConsumer extends Consumer {
      override def consumeEnum(enm: TypedAst.Enum): Unit = builder.considerWrite(enm.sym, enm.sym.loc)
      override def consumeType(tpe: Type): Unit = tpe match {
        case Type.Cst(TypeConstructor.Enum(sym, _), loc) => builder.considerRead(sym, loc)
        case _ => ()
      }
    }

    Visitor.visitRoot(root, EnumSymConsumer, acceptor)

    builder.build
  }

  private def highlightSigSym(sym: Symbol.SigSym)(implicit root: Root, acceptor: Visitor.Acceptor): JObject = {
    val builder = HighlightBuilder(sym)

    object SigSymConsumer extends Consumer {
      override def consumeSig(sig: TypedAst.Sig): Unit = builder.considerWrite(sig.sym, sig.sym.loc)
      override def consumeSigSymUse(symUse: SymUse.SigSymUse): Unit = builder.considerRead(symUse.sym, symUse.loc)
    }

    Visitor.visitRoot(root, SigSymConsumer, acceptor)

    builder.build
  }

  private def highlightStructFieldSym(sym: Symbol.StructFieldSym)(implicit root: Root, acceptor: Visitor.Acceptor): JObject = {
    val builder = HighlightBuilder(sym)

    object StructFieldSymConsumer extends Consumer {
      override def consumeStructField(field: TypedAst.StructField): Unit = builder.considerWrite(field.sym, field.sym.loc)
      override def consumeStructFieldSymUse(symUse: SymUse.StructFieldSymUse): Unit = builder.considerRead(symUse.sym, symUse.loc)
    }

    Visitor.visitRoot(root, StructFieldSymConsumer, acceptor)

    builder.build
  }

  private def highlightStructSym(sym: Symbol.StructSym)(implicit root: Root, acceptor: Visitor.Acceptor): JObject = {
    val builder = HighlightBuilder(sym)

    object StructSymConsumer extends Consumer {
      override def consumeStruct(struct: TypedAst.Struct): Unit = builder.considerWrite(struct.sym, struct.sym.loc)
      override def consumeExpr(exp: Expr): Unit = exp match {
        case Expr.StructNew(sym, _, _, _, _, loc) => builder.considerRead(sym, loc)
        case _ => ()
      }
      override def consumeType(tpe: Type): Unit = tpe match {
        case Type.Cst(TypeConstructor.Struct(sym, _), loc) => builder.considerRead(sym, loc)
        case _ => ()
      }
    }

    Visitor.visitRoot(root, StructSymConsumer, acceptor)

    builder.build
  }

  private def highlightTypeAliasSym(sym: Symbol.TypeAliasSym)(implicit root: Root, acceptor: Visitor.Acceptor): JObject = {
    val builder = HighlightBuilder(sym)

    object TypeAliasSymConsumer extends Consumer {
      override def consumeTypeAlias(alias: TypedAst.TypeAlias): Unit = builder.considerWrite(alias.sym, alias.sym.loc)
      override def consumeType(tpe: Type): Unit = tpe match {
        case Type.Alias(Ast.AliasConstructor(sym, _), _, _, loc) => builder.considerRead(sym, loc)
        case _ => ()
      }
    }

    Visitor.visitRoot(root, TypeAliasSymConsumer, acceptor)

    builder.build
  }

  private def highlightTraitSym(sym: Symbol.TraitSym)(implicit root: Root, acceptor: Visitor.Acceptor): JObject = {
    val builder = HighlightBuilder(sym)

    object TraitSymConsumer extends Consumer {
      override def consumeTrait(traitt: TypedAst.Trait): Unit = builder.considerWrite(traitt.sym, traitt.sym.loc)
      override def consumeTraitSymUse(symUse: SymUse.TraitSymUse): Unit = builder.considerRead(symUse.sym, symUse.loc)
      override def consumeTraitConstraintHead(tcHead: TraitConstraint.Head): Unit = builder.considerRead(tcHead.sym, tcHead.loc)
    }

    Visitor.visitRoot(root, TraitSymConsumer, acceptor)

    builder.build
  }

  private def highlightVarSym(sym: Symbol.VarSym)(implicit root: Root, acceptor: Visitor.Acceptor): JObject = {
    val builder = HighlightBuilder(sym)

    object VarSymConsumer extends Consumer {
      override def consumeBinder(bnd: TypedAst.Binder): Unit = builder.considerWrite(bnd.sym, bnd.sym.loc)
      override def consumeLocalDefSym(symUse: SymUse.LocalDefSymUse): Unit = builder.considerRead(symUse.sym, symUse.loc)
      override def consumeExpr(exp: Expr): Unit = exp match {
        case Expr.Var(sym, _, loc) => builder.considerRead(sym, loc)
        case _ => ()
      }
    }

    Visitor.visitRoot(root, VarSymConsumer, acceptor)

    builder.build
  }

  /**
    * Returns a reply indicating that nothing was found at the `uri` and `pos`.
    */
  private def mkNotFound(uri: String, pos: Position): JObject =
    ("status" -> ResponseStatus.InvalidRequest) ~ ("message" -> s"Nothing found in '$uri' at '$pos'.")

}

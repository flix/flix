/*
 * Copyright 2020 Magnus Madsen,
 *           2024 Alexander Dybdahl Troelsen
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
import ca.uwaterloo.flix.language.ast.{Ast, Name, SourceLocation, Symbol, Type, TypeConstructor, TypedAst}
import org.json4s.JsonAST.{JArray, JObject}
import org.json4s.JsonDSL.*

/**
  * Provides the means to handle an LSP highlight request.
  */
object HighlightProvider {

  /**
    * Handles an LSP highlight request by constructing an LSP highlight response for
    * when the cursor is at `pos` in the file at `uri`.
    *
    * Note that we assume a thin cursor so `pos` is interpreted as the character position
    * to the immediate right of the cursor
    *
    * If there is a [[Symbol]] or [[Name.Label]] under the cursor, every occurrence of
    * it in the file at `uri` is collected and a [[DocumentHighlight]] is created for each.
    * These are then put in a [[JObject]] representing an LSP highlight response. It takes the form
    *
    * `{'status': 'success', 'message': [...]}` where `[...]` is a [[JArray]] containing each [[DocumentHighlight]].
    *
    * If there is no [[Symbol]] or [[Name.Label]] at `pos`, a [[JObject]] representing an LSP failure response is returned. It takes the form
    *
    * `{'status': 'failure', 'message': "Nothing found in <uri> at <pos>`.
    *
    * Since a thin cursor exists between character positions, it's associated with both
    * the [[Position]] to its immediate left and right. This means we need to consider what's under
    * both. If there are two different [[Symbol]]/[[Name.Label]]s (any combination of the two)
    * under the left and right [[Position]], we highlight occurrences of the one on the right.
    *
    * Note that the [[Position]] `pos` given is interpreted as the [[Position]] to the
    * immediate right of the thin cursor.
    *
    * @param uri  the URI of the file in question.
    * @param pos  the [[Position]] of the cursor.
    * @param root the [[Root]] AST node of the Flix project.
    * @return     A [[JObject]] representing an LSP highlight response. On success, contains [[DocumentHighlight]]
    *             for each occurrence of the symbol under the cursor.
    */
  def processHighlight(uri: String, pos: Position)(implicit root: Root): JObject = {
    val highlightRight = searchRightOfCursor(uri, pos).flatMap(x => highlightAny(x, uri))
    val highlightLeft = searchLeftOfCursor(uri, pos).flatMap(x => highlightAny(x, uri))

    highlightRight
      .orElse(highlightLeft)
      .getOrElse(mkNotFound(uri, pos))
  }

  /**
    * Returns the most precise AST node under the position immediately left of the cursor,
    * if there is one. Otherwise, returns `None`.
    *
    * If there is no [[Position]] to the left of the cursor, returns `None`.
    *
    * Note that the [[Position]] `pos` given is interpreted as the [[Position]] to the
    * immediate right of the cursor.
    *
    * @param uri  the URI of the file in which the cursor is.
    * @param pos  the [[Position]] immediately right of the cursor.
    * @param root the [[Root]] AST node of the Flix project.
    * @return     the most precise AST node under the [[Position]] immediately left of the cursor, if there is one. Otherwise, returns `None`.
    */
  private def searchLeftOfCursor(uri: String, pos: Position)(implicit root: Root): Option[AnyRef] = pos match {
      case Position(line, character) if character >= 2 =>
        val leftOfCursor = Position(line, character - 1)
        search(uri, leftOfCursor)
      case _ => None
    }

  /**
    * Returns the most precise AST node under the [[Position]] immediately right of the cursor,
    * if there is one. Otherwise, returns `None`.
    *
    * Note that the [[Position]] `pos` given is interpreted as the [[Position]] to the
    * immediate right of the cursor.
    *
    * @param uri  the URI of the file in which the cursor is.
    * @param pos  the [[Position]] immediatately right of the cursor.
    * @param root the [[Root]] AST node of the Flix project.
    * @return     the most precise AST node under the [[Position]] immediately right of the cursor, if there is one. Otherwise, returns `None`.
    */
  private def searchRightOfCursor(uri: String, pos: Position)(implicit root: Root): Option[AnyRef] = {
    search(uri, pos)
  }

  /**
    * Searches for the most precise AST node at the [[Position]] `pos`.
    *
    * Certain AST nodes are filtered out. We filter out [[TypedAst.Def]], [[TypedAst.Sig]],
    * [[TypedAst.Op]] and [[TypedAst.Enum]] nodes if the cursor is in them but _not_ in their [[Symbol]].
    * Additionally, we filter out [[TypedAst.Expr.RecordEmpty]] nodes, since they're uninteresting
    * for highlighting and their [[SourceLocation]] overshadows [[TypedAst.Expr.RecordExtend]]
    * and [[TypedAst.Expr.RecordRestrict]] nodes.
    *
    * We return the most precise AST node under the right [[Position]] (after filtering), unless there is none,
    * then we return the most precise AST node under the left [[Position]] (after filtering). If we still
    * find nothing, we return `None`.
    *
    * @param uri  the URI of the file the cursor is in.
    * @param pos  the [[Position]] immediately right of the thin cursor.
    * @param root the [[Root]] AST node of the Flix project.
    * @return     the most precise AST under the cursor if there is one. Otherwise, returns `None`.
    */
  private def search(uri: String, pos: Position)(implicit root: Root): Option[AnyRef] = {
    val stackConsumer = StackConsumer()
    Visitor.visitRoot(root, stackConsumer, Visitor.InsideAcceptor(uri, pos))
    stackConsumer
      .getStack
      .filter(isNotEmptyRecord)
      .filter(ifDefThenInSym(uri, pos))
      .filter(ifSigThenInSym(uri, pos))
      .filter(ifOpThenInSym(uri, pos))
      .filter(ifTraitThenInSym(uri, pos))
      .filter(ifEnumThenInSym(uri, pos))
      .headOption
  }

  private def isNotEmptyRecord(x: AnyRef): Boolean = x match {
    case TypedAst.Expr.RecordEmpty(_, _) => false
    case _ => true
  }

  private def ifEnumThenInSym(uri: String, pos: Position)(x: AnyRef): Boolean = x match {
    case TypedAst.Enum(_, _, _, sym, _, _, _, _) if !Visitor.inside(uri, pos)(sym.loc) => false
    case _ => true
  }

  private def ifDefThenInSym(uri: String, pos: Position)(x: AnyRef): Boolean = x match {
    case TypedAst.Def(sym, _, _, _) if !Visitor.inside(uri, pos)(sym.loc) => false
    case _ => true
  }

  private def ifSigThenInSym(uri: String, pos: Position)(x: AnyRef): Boolean = x match {
    case TypedAst.Sig(sym, _, _, _) if !Visitor.inside(uri, pos)(sym.loc) => false
    case _ => true
  }

  private def ifOpThenInSym(uri: String, pos: Position)(x: AnyRef): Boolean = x match {
    case TypedAst.Op(sym, _, _) if !Visitor.inside(uri, pos)(sym.loc) => false
    case _ => true
  }

  private def ifTraitThenInSym(uri: String, pos: Position)(x: AnyRef): Boolean = x match {
    case TypedAst.Trait(_, _, _, sym, _, _, _, _, _, _) if !Visitor.inside(uri, pos)(sym.loc) => false
    case _ => true
  }

  /**
    * Constructs the LSP highlight response for when the cursor is on an arbitrary [[AnyRef]] `x`.
    *
    * If `x` is a [[Symbol]] or [[Name.Label]], finds all occurrences of it in the file at `uri`, makes a
    * [[DocumentHighlight]] for each, collecting them all in a successful LSP highlight response.
    *
    * If `x` is __not__ a [[Symbol]] or [[Name.Label]], returns a failure LSP highlight response.
    *
    * @param x    the object under the cursor.
    * @param uri  the URI of the file in question.
    * @param root the [[Root]] AST node of the Flix project.
    * @return     A [[JObject]] representing an LSP highlight response. On success, contains [[DocumentHighlight]]
    *             for each occurrence of the symbol under the cursor.
    */
  private def highlightAny(x: AnyRef, uri: String)(implicit root: Root): Option[JObject] = {
    implicit val acceptor: Visitor.Acceptor = Visitor.FileAcceptor(uri)
    x match {
      // Assoc Types
      case TypedAst.AssocTypeSig(_, _, sym, _, _, _, _) => Some(highlightAssocTypeSym(sym))
      case SymUse.AssocTypeSymUse(sym, _) => Some(highlightAssocTypeSym(sym))
      case Type.AssocType(Ast.AssocTypeConstructor(sym, _), _, _, _) => Some(highlightAssocTypeSym(sym))
      // Defs
      case TypedAst.Def(sym, _, _, _) => Some(highlightDefnSym(sym))
      case SymUse.DefSymUse(sym, _) => Some(highlightDefnSym(sym))
      // Effects
      case TypedAst.Effect(_, _, _, sym, _, _) => Some(highlightEffectSym(sym))
      case Type.Cst(TypeConstructor.Effect(sym), _) => Some(highlightEffectSym(sym))
      case SymUse.EffectSymUse(sym, _) => Some(highlightEffectSym(sym))
      // Enums & Cases
      case TypedAst.Enum(_, _, _, sym, _, _, _, _) => Some(highlightEnumSym(sym))
      case Type.Cst(TypeConstructor.Enum(sym, _), _) => Some(highlightEnumSym(sym))
      case TypedAst.Case(sym, _, _, _) => Some(highlightCaseSym(sym))
      case SymUse.CaseSymUse(sym, _) => Some(highlightCaseSym(sym))
      // Ops
      case TypedAst.Op(sym, _, _) => Some(highlightOpSym(sym))
      case SymUse.OpSymUse(sym, _) => Some(highlightOpSym(sym))
      // Records
      case TypedAst.Expr.RecordExtend(label, _, _, _, _, _) => Some(highlightLabel(label))
      case TypedAst.Expr.RecordRestrict(label, _, _, _, _) => Some(highlightLabel(label))
      case TypedAst.Expr.RecordSelect(_, label, _, _, _) => Some(highlightLabel(label))
      // Signatures
      case TypedAst.Sig(sym, _, _, _) => Some(highlightSigSym(sym))
      case SymUse.SigSymUse(sym, _) => Some(highlightSigSym(sym))
      // Structs
      case TypedAst.Struct(_, _, _, sym, _, _, _, _) => Some(highlightStructSym(sym))
      case Type.Cst(TypeConstructor.Struct(sym, _), _) => Some(highlightStructSym(sym))
      case TypedAst.StructField(sym, _, _) => Some(highlightStructFieldSym(sym))
      case SymUse.StructFieldSymUse(sym, _) => Some(highlightStructFieldSym(sym))
      // Traits
      case TypedAst.Trait(_, _, _, sym, _, _, _, _, _, _) => Some(highlightTraitSym(sym))
      case SymUse.TraitSymUse(sym, _) => Some(highlightTraitSym(sym))
      case TraitConstraint.Head(sym, _) => Some(highlightTraitSym(sym))
      // Type Aliases
      case TypedAst.TypeAlias(_, _, _, sym, _, _, _) => Some(highlightTypeAliasSym(sym))
      case Type.Alias(Ast.AliasConstructor(sym, _), _, _, _) => Some(highlightTypeAliasSym(sym))
      // Type Variables
      case TypedAst.TypeParam(_, sym, _) => Some(highlightTypeVarSym(sym))
      case Type.Var(sym, _) => Some(highlightTypeVarSym(sym))
      // Variables
      case Binder(sym, _) => Some(highlightVarSym(sym))
      case TypedAst.Expr.Var(varSym, _, _) => Some(highlightVarSym(varSym))

      case _ => None
    }
  }

  private def highlightAssocTypeSym(sym: Symbol.AssocTypeSym)(implicit root: Root, acceptor: Visitor.Acceptor): JObject = {
    val builder = new HighlightBuilder(sym)

    object AssocTypeSymConsumer extends Consumer {
      override def consumeAssocTypeSig(tsig: TypedAst.AssocTypeSig): Unit = builder.considerWrite(tsig.sym, tsig.sym.loc)
      override def consumeAssocTypeSymUse(symUse: SymUse.AssocTypeSymUse): Unit = builder.considerRead(symUse.sym, symUse.loc)
    }

    Visitor.visitRoot(root, AssocTypeSymConsumer, acceptor)

    builder.build
  }

  private def highlightDefnSym(sym: Symbol.DefnSym)(implicit root: Root, acceptor: Visitor.Acceptor): JObject = {
    val builder = new HighlightBuilder(sym)

    object DefnSymConsumer extends Consumer {
      override def consumeDef(defn: TypedAst.Def): Unit = builder.considerWrite(defn.sym, defn.sym.loc)
      override def consumeDefSymUse(sym: SymUse.DefSymUse): Unit = builder.considerRead(sym.sym, sym.loc)
    }

    Visitor.visitRoot(root, DefnSymConsumer, acceptor)

    builder.build
  }

  private def highlightEffectSym(sym: Symbol.EffectSym)(implicit root: Root, acceptor: Visitor.Acceptor): JObject = {
    val builder = new HighlightBuilder(sym)

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
    val builder = new HighlightBuilder(sym)

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

  private def highlightCaseSym(sym: Symbol.CaseSym)(implicit root: Root, acceptor: Visitor.Acceptor): JObject = {
    val builder = new HighlightBuilder(sym)

    object CaseSymConsumer extends Consumer {
      override def consumeCase(cse: TypedAst.Case): Unit = builder.considerWrite(cse.sym, cse.sym.loc)
      override def consumeCaseSymUse(sym: CaseSymUse): Unit = builder.considerRead(sym.sym, sym.loc)
    }

    Visitor.visitRoot(root, CaseSymConsumer, acceptor)

    builder.build
  }

  private def highlightOpSym(sym: Symbol.OpSym)(implicit root: Root, acceptor: Visitor.Acceptor): JObject = {
    val builder = new HighlightBuilder(sym)

    object OpSymConsumer extends Consumer {
      override def consumeOp(op: TypedAst.Op): Unit = builder.considerWrite(op.sym, op.sym.loc)
      override def consumeOpSymUse(sym: SymUse.OpSymUse): Unit = builder.considerRead(sym.sym, sym.loc)
    }

    Visitor.visitRoot(root, OpSymConsumer, acceptor)

    builder.build
  }

  private def highlightLabel(label: Name.Label)(implicit root: Root, acceptor: Visitor.Acceptor): JObject = {
    val builder = new HighlightBuilder(label)

    object LabelConsumer extends Consumer {
      override def consumeExpr(exp: Expr): Unit = exp match {
        case Expr.RecordExtend(l, _, _, _, _, _) => builder.considerWrite(l, l.loc)
        case Expr.RecordSelect(_, l, _, _, _) => builder.considerRead(l, l.loc)
        case Expr.RecordRestrict(l, _, _, _, _) => builder.considerWrite(l, l.loc)
        case _ => ()
      }
    }

    Visitor.visitRoot(root, LabelConsumer, acceptor)

    builder.build
  }

  private def highlightSigSym(sym: Symbol.SigSym)(implicit root: Root, acceptor: Visitor.Acceptor): JObject = {
    val builder = new HighlightBuilder(sym)

    object SigSymConsumer extends Consumer {
      override def consumeSig(sig: TypedAst.Sig): Unit = builder.considerWrite(sig.sym, sig.sym.loc)
      override def consumeSigSymUse(symUse: SymUse.SigSymUse): Unit = builder.considerRead(symUse.sym, symUse.loc)
    }

    Visitor.visitRoot(root, SigSymConsumer, acceptor)

    builder.build
  }

  private def highlightStructSym(sym: Symbol.StructSym)(implicit root: Root, acceptor: Visitor.Acceptor): JObject = {
    val builder = new HighlightBuilder(sym)

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

  private def highlightStructFieldSym(sym: Symbol.StructFieldSym)(implicit root: Root, acceptor: Visitor.Acceptor): JObject = {
    val builder = new HighlightBuilder(sym)

    object StructFieldSymConsumer extends Consumer {
      override def consumeStructField(field: TypedAst.StructField): Unit = builder.considerWrite(field.sym, field.sym.loc)
      override def consumeStructFieldSymUse(symUse: SymUse.StructFieldSymUse): Unit = builder.considerRead(symUse.sym, symUse.loc)
    }

    Visitor.visitRoot(root, StructFieldSymConsumer, acceptor)

    builder.build
  }

  private def highlightTraitSym(sym: Symbol.TraitSym)(implicit root: Root, acceptor: Visitor.Acceptor): JObject = {
    val builder = new HighlightBuilder(sym)

    object TraitSymConsumer extends Consumer {
      override def consumeTrait(traitt: TypedAst.Trait): Unit = builder.considerWrite(traitt.sym, traitt.sym.loc)
      override def consumeTraitSymUse(symUse: SymUse.TraitSymUse): Unit = builder.considerRead(symUse.sym, symUse.loc)
      override def consumeTraitConstraintHead(tcHead: TraitConstraint.Head): Unit = builder.considerRead(tcHead.sym, tcHead.loc)
    }

    Visitor.visitRoot(root, TraitSymConsumer, acceptor)

    builder.build
  }

  private def highlightTypeAliasSym(sym: Symbol.TypeAliasSym)(implicit root: Root, acceptor: Visitor.Acceptor): JObject = {
    val builder = new HighlightBuilder(sym)

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

  private def highlightTypeVarSym(sym: Symbol.KindedTypeVarSym)(implicit root: Root, acceptor: Visitor.Acceptor): JObject = {
    val builder = new HighlightBuilder(sym)

    object TypeVarSymConsumer extends Consumer {
      override def consumeTypeParam(tparam: TypedAst.TypeParam): Unit = builder.considerWrite(tparam.sym, tparam.sym.loc)
      override def consumeType(tpe: Type): Unit = tpe match {
        case Type.Var(sym, loc) => builder.considerRead(sym, loc)
        case _ => ()
      }
    }

    Visitor.visitRoot(root, TypeVarSymConsumer, acceptor)

    builder.build
  }

  private def highlightVarSym(sym: Symbol.VarSym)(implicit root: Root, acceptor: Visitor.Acceptor): JObject = {
    val builder = new HighlightBuilder(sym)

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
  private def mkNotFound(uri: String, pos: Position): JObject = {
    ("status" -> ResponseStatus.InvalidRequest) ~ ("message" -> s"Nothing found in '$uri' at '$pos'.")
  }

  /**
    * A builder for creating an LSP highlight response containing a [[DocumentHighlight]] for each recorded
    * occurrence of a given token [[tok]].
    *
    * An occurrence of a token of type [[T]] is "considered" by invoking [[considerRead]] or [[considerWrite]] for resp.
    * "read" and "write" occurrences. By a "write" occurrence, we mean an occurrence where the token is being
    * defined or otherwise bound to something. By "read" we mean an occurrence where the token is merely read.
    * When we say "consider", we mean first checking if the occurrence is an occurrence of [[tok]] specifically.
    * If so, it's added to our list of either "read" or "write" occurrences, depending on the type.
    *
    * When we're done considering tokens, we can construct the LSP response by calling [[build]]
    *
    * @param tok  the token we're finding occurrences of.
    * @tparam T   the type of token that [[tok]] is.
    */
  private class HighlightBuilder[T](tok: T) {
    private var reads: List[SourceLocation] = Nil
    private var writes: List[SourceLocation] = Nil

    /**
      * If `x` is an occurrence of [[tok]], adds it to our list of "read" occurrences.
      *
      * @param x    the token we're considering.
      * @param loc  the [[SourceLocation]] of the occurrence.
      */
    def considerRead(x: T, loc: SourceLocation): Unit = {
      if (x == tok) {
        reads = loc :: reads
      }
    }

    /**
      * If `x` is an occurrence of [[tok]], adds it to our list of "write" occurrences.
      *
      * @param x    the token we're considering.
      * @param loc  the [[SourceLocation]] of the occurrence.
      */
    def considerWrite(x: T, loc: SourceLocation): Unit = {
      if (x == tok) {
        writes = loc :: writes
      }
    }

    /**
      * Builds a [[JObject]] representing a successful LSP highlight response containing a [[DocumentHighlight]] for each read and write
      * occurrence of [[tok]] recorded.
      *
      * @return A [[JObject]] representing a successful LSP highlight response containing a highlight of each recorded occurrence of [[tok]]
      */
    def build: JObject = {
      val writeHighlights = writes.map(loc => DocumentHighlight(Range.from(loc), DocumentHighlightKind.Write))
      val readHighlights = reads.map(loc => DocumentHighlight(Range.from(loc), DocumentHighlightKind.Read))

      val highlights = writeHighlights ++ readHighlights

      ("status" -> ResponseStatus.Success) ~ ("result" -> JArray(highlights.map(_.toJSON)))
    }
  }
}

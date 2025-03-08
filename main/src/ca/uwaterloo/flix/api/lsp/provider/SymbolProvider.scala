/*
 * Copyright 2021 Nicola Dardanis
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

import ca.uwaterloo.flix.api.lsp.{DocumentSymbol, Location, Range, SymbolKind, WorkspaceSymbol}
import ca.uwaterloo.flix.language.ast.TypedAst
import ca.uwaterloo.flix.language.ast.TypedAst.Root
import ca.uwaterloo.flix.language.fmt.FormatKind.formatKind

object SymbolProvider {

  /**
    * Returns all symbols in the workspace that match the given query string.
    *
    * As for now we just match the very beginning of the string with the query string,
    * but a more inclusive matching pattern can be implemented.
    */
  def processWorkspaceSymbols(query: String)(implicit root: Root): List[WorkspaceSymbol] = {
    val enums = root.enums.values.filter(_.sym.name.startsWith(query)).flatMap(mkEnumWorkspaceSymbol)
    val defs = root.defs.values.collect { case d if d.sym.name.startsWith(query) => mkDefWorkspaceSymbol(d) }
    val traits = root.traits.values.collect { case t if t.sym.name.startsWith(query) => mkTraitWorkSpaceSymbol(t) }
    val sigs = root.sigs.values.collect { case sig if sig.sym.name.startsWith(query) => mkSigWorkspaceSymbol(sig) }
    val effs = root.effects.values.filter(_.sym.name.startsWith(query)).flatMap(mkEffectWorkspaceSymbol)
    val structs = root.structs.values.filter(_.sym.name.startsWith(query)).flatMap(mkStructWorkspaceSymbol)
    (traits ++ defs ++ enums ++ sigs ++ effs ++ structs).toList.filter{
      case WorkspaceSymbol(_, _, _, _, loc) => loc.uri.startsWith("file://")
    }
  }

  /**
    * Returns all symbols that are inside the file pointed by uri.
    */
  def processDocumentSymbols(uri: String)(implicit root: Root): List[DocumentSymbol] = {
    val enums = root.enums.values.collect { case enum0 if enum0.loc.source.name == uri => mkEnumDocumentSymbol(enum0) }
    val defs = root.defs.values.collect { case d if d.sym.loc.source.name == uri => mkDefDocumentSymbol(d) }
    val traits = root.traits.values.collect { case t if t.sym.loc.source.name == uri => mkTraitDocumentSymbol(t) }
    val effs = root.effects.values.collect { case e if e.sym.loc.source.name == uri => mkEffectDocumentSymbol(e) }
    val structs = root.structs.values.collect { case s if s.sym.loc.source.name == uri => mkStructDocumentSymbol(s) }
    (traits ++ defs ++ enums ++ effs ++ structs).toList.filter(_.name.nonEmpty)
  }

  /**
    * Returns an Interface SymbolInformation from a Trait node.
    */
  private def mkTraitWorkSpaceSymbol(t: TypedAst.Trait) = t match {
    case TypedAst.Trait(_, _, _, sym, _, _, _, _, _, _) => WorkspaceSymbol(
      sym.name, SymbolKind.Interface, Nil, None, Location(sym.loc.source.name, Range.from(sym.loc)),
    )
  }

  /**
    * Returns an Interface DocumentSymbol from a Trait node.
    * It navigates the AST and adds Sig and TypeParam of t and as children DocumentSymbols.
    */
  private def mkTraitDocumentSymbol(t: TypedAst.Trait): DocumentSymbol = t match {
    case TypedAst.Trait(doc, _, _, sym, tparam, _, _, signatures, _, _) => DocumentSymbol( // TODO ASSOC-TYPES visit assocs
      sym.name,
      Some(doc.text),
      SymbolKind.Interface,
      Range.from(sym.loc),
      Range.from(sym.loc),
      Nil,
      (signatures.map(mkSigDocumentSymbol) :+ mkTypeParamDocumentSymbol(tparam)).filter(_.name.nonEmpty),
    )
  }

  /**
    * Returns a TypeParameter DocumentSymbol from a TypeParam node.
    */
  private def mkTypeParamDocumentSymbol(t: TypedAst.TypeParam) = t match {
    case TypedAst.TypeParam(name, tpe, loc) => DocumentSymbol(
      name.name, Some(formatKind(tpe.kind)), SymbolKind.TypeParameter, Range.from(loc), Range.from(loc), Nil, Nil,
    )
  }

  /**
    * Returns a Method DocumentSymbol from a Sig node.
    */
  private def mkSigDocumentSymbol(s: TypedAst.Sig): DocumentSymbol = s match {
    case TypedAst.Sig(sym, spec, _, _) => DocumentSymbol(
      sym.name, Some(spec.doc.text), SymbolKind.Method, Range.from(sym.loc), Range.from(sym.loc), Nil, Nil,
    )
  }

  /**
    * Returns a Method SymbolInformation from a Sig node.
    */
  private def mkSigWorkspaceSymbol(s: TypedAst.Sig): WorkspaceSymbol = s match {
    case TypedAst.Sig(sym, _, _, _) => WorkspaceSymbol(
      sym.name, SymbolKind.Method, Nil, None, Location(sym.loc.source.name, Range.from(sym.loc)),
    )
  }

  /**
   * Returns a Method SymbolInformation from a Struct node.
   */
  private def mkStructWorkspaceSymbol(s: TypedAst.Struct): List[WorkspaceSymbol] = s match {
    case TypedAst.Struct(_, _, _, sym, _, _, fields, _) =>
      fields.values.map(mkFieldWorkspaceSymbol).toList :+ WorkspaceSymbol(
      sym.name, SymbolKind.Struct, Nil, None, Location(sym.loc.source.name, Range.from(sym.loc)),
    )
  }

  private def mkFieldWorkspaceSymbol(f: TypedAst.StructField): WorkspaceSymbol = f match {
    case TypedAst.StructField(sym, _, loc) => WorkspaceSymbol(
      sym.name, SymbolKind.Field, Nil, None, Location(loc.source.name, Range.from(loc)),
    )
  }

  /**
   * Returns a Function DocumentSymbol from a Struct node.
   */
  private def mkStructDocumentSymbol(s: TypedAst.Struct): DocumentSymbol = s match {
    case TypedAst.Struct(doc, _, _, sym, tparams, _, fields, _) => DocumentSymbol(
      sym.name,
      Some(doc.text),
      SymbolKind.Struct,
      Range.from(sym.loc),
      Range.from(sym.loc),
      Nil,
      (fields.values.map(mkFieldDocumentSymbol).toList ++  tparams.map(mkTypeParamDocumentSymbol)).filter(_.name.nonEmpty),
    )
  }

  private def mkFieldDocumentSymbol(f: TypedAst.StructField): DocumentSymbol = f match {
    case TypedAst.StructField(sym, _, loc) => DocumentSymbol(
      sym.name, None, SymbolKind.Field, Range.from(loc), Range.from(loc), Nil, Nil,
    )
  }

  /**
    * Returns a Function DocumentSymbol from a Def node.
    */
  private def mkDefDocumentSymbol(d: TypedAst.Def): DocumentSymbol = d match {
    case TypedAst.Def(sym, spec, _, _) => DocumentSymbol(
      sym.name, Some(spec.doc.text), SymbolKind.Function, Range.from(sym.loc), Range.from(sym.loc), Nil, Nil,
    )
  }

  /**
    * Returns a Function SymbolInformation from a Def node.
    */
  private def mkDefWorkspaceSymbol(d: TypedAst.Def): WorkspaceSymbol = d match {
    case TypedAst.Def(sym, _, _, _) => WorkspaceSymbol(
      sym.name, SymbolKind.Function, Nil, None, Location(sym.loc.source.name, Range.from(sym.loc)),
    )
  }

  /**
    * Returns an Enum DocumentSymbol from an Enum node.
    * It navigates the AST and adds Cases of enum as children DocumentSymbols.
    */
  private def mkEnumDocumentSymbol(enum0: TypedAst.Enum): DocumentSymbol = enum0 match {
    case TypedAst.Enum(doc, _, _, sym, tparams, _, cases, loc) => DocumentSymbol(
      sym.name,
      Some(doc.text),
      SymbolKind.Enum,
      Range.from(loc),
      Range.from(loc),
      Nil,
      (cases.values.map(mkCaseDocumentSymbol).toList ++ tparams.map(mkTypeParamDocumentSymbol)).filter(_.name.nonEmpty)
    )
  }

  /**
    * Returns an EnumMember DocumentSymbol from a Case node.
    */
  private def mkCaseDocumentSymbol(c: TypedAst.Case): DocumentSymbol = c match {
    case TypedAst.Case(sym, _, _, loc) => DocumentSymbol(
      sym.name, None, SymbolKind.EnumMember, Range.from(loc), Range.from(loc), Nil, Nil,
    )
  }

  /**
    * Returns an Enum DocumentSymbol from an Enum node.
    * It navigates the AST and returns also the Cases of the enum to the returned List.
    */
  private def mkEnumWorkspaceSymbol(enum0: TypedAst.Enum): List[WorkspaceSymbol] = enum0 match {
    case TypedAst.Enum(_, _, _, sym, _, _, cases, loc) =>
      cases.values.map(mkCaseWorkspaecSymbol).toList :+ WorkspaceSymbol(
        sym.name, SymbolKind.Enum, Nil, None, Location(loc.source.name, Range.from(loc)),
      )
  }

  /**
    * Returns an EnumMember SymbolInformation from a Case node.
    */
  private def mkCaseWorkspaecSymbol(c: TypedAst.Case): WorkspaceSymbol = c match {
    case TypedAst.Case(sym, _, _, loc) => WorkspaceSymbol(
      sym.name, SymbolKind.EnumMember, Nil, None, Location(loc.source.name, Range.from(loc)))
  }

  /**
    * Returns an Interface SymbolInformation from an Effect node.
    */
  private def mkEffectWorkspaceSymbol(effect: TypedAst.Effect): List[WorkspaceSymbol] = effect match {
    case TypedAst.Effect(_, _, _, sym, ops, _) =>
      ops.map(mkOpWorkspaceSymbol) :+ WorkspaceSymbol(
        sym.name, SymbolKind.Interface, Nil, None, Location(sym.loc.source.name, Range.from(sym.loc)))
  }

  /**
    * Returns an Interface DocumentSymbol from a Effect node.
    * It navigates the AST and adds Sig and TypeParam of c and as children DocumentSymbols.
    */
  private def mkEffectDocumentSymbol(c: TypedAst.Effect): DocumentSymbol = c match {
    case TypedAst.Effect(doc, _, _, sym, ops, _) => DocumentSymbol(
      sym.name,
      Some(doc.text),
      SymbolKind.Interface,
      Range.from(sym.loc),
      Range.from(sym.loc),
      Nil,
      ops.map(mkOpDocumentSymbol).filter(_.name.nonEmpty),
    )
  }

  /**
    * Returns an Function SymbolInformation from an Op node.
    */
  private def mkOpWorkspaceSymbol(op: TypedAst.Op): WorkspaceSymbol = op match {
    case TypedAst.Op(sym, _, _) =>
      WorkspaceSymbol(sym.name, SymbolKind.Function, Nil, None, Location(sym.loc.source.name, Range.from(sym.loc)))
  }

  /**
    * Returns a Method DocumentSymbol from an Op node.
    */
  private def mkOpDocumentSymbol(s: TypedAst.Op): DocumentSymbol = s match {
    case TypedAst.Op(sym, spec, _) => DocumentSymbol(
      sym.name, Some(spec.doc.text), SymbolKind.Method, Range.from(sym.loc), Range.from(sym.loc), Nil, Nil,
    )
  }

}

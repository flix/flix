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

import ca.uwaterloo.flix.api.lsp.{DocumentSymbol, Range, SymbolKind}
import ca.uwaterloo.flix.language.ast.TypedAst
import ca.uwaterloo.flix.language.ast.TypedAst.Root
import ca.uwaterloo.flix.language.debug.FormatKind.formatKind

object SymbolProvider {

  def processDocumentSymbols(uri: String)(implicit root: Root): List[DocumentSymbol] = {
    if (root == null) {
      // No AST available.
      return Nil
    }

    val enums = root.enums.values.collect { case enum if enum.loc.source.name == uri => mkEnumDocumentSymbol(enum) }
    val defs = root.defs.values.collect { case d if d.sym.loc.source.name == uri => mkDefDocumentSymbol(d) }
    val classes = root.classes.values.collect { case c if c.sym.loc.source.name == uri => mkClassDocumentSymbol(c) }
    (classes ++ defs ++ enums).toList
  }

  /**
    * Returns an Interface DocumentSymbol from a Class node.
    * It navigates the AST and adds Sig and TypeParam of c and as children DocumentSymbols.
    */
  private def mkClassDocumentSymbol(c: TypedAst.Class): DocumentSymbol = c match {
    case TypedAst.Class(doc, _, sym, tparam, _, signatures, _, loc) => DocumentSymbol(
      sym.name,
      Some(doc.text),
      SymbolKind.Interface,
      Range.from(loc),
      Range.from(loc),
      Nil,
      signatures.map(mkSigDocumentSymbol) :+ mkTypeParamDocumentSymbol(tparam),
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
    case TypedAst.Sig(sym, spec, _) => DocumentSymbol(
      sym.name, Some(spec.doc.text), SymbolKind.Method, Range.from(sym.loc), Range.from(sym.loc), Nil, Nil,
    )
  }

  /**
    * Returns a Function DocumentSymbol from a Def node.
    */
  private def mkDefDocumentSymbol(d: TypedAst.Def): DocumentSymbol = d match {
    case TypedAst.Def(sym, spec, _) => DocumentSymbol(
      sym.name, Some(spec.doc.text), SymbolKind.Function, Range.from(sym.loc), Range.from(sym.loc), Nil, Nil,
    )
  }

  /**
    * Returns an Enum DocumentSymbol from an Enum node.
    * It navigates the AST and adds Cases of enum as children DocumentSymbols.
    */
  private def mkEnumDocumentSymbol(enum: TypedAst.Enum): DocumentSymbol = enum match {
    case TypedAst.Enum(doc, _, sym, tparams, cases, _, _, loc) => DocumentSymbol(
      sym.name,
      Some(doc.text),
      SymbolKind.Enum,
      Range.from(loc),
      Range.from(loc),
      Nil,
      cases.values.map(mkCaseDocumentSymbol).toList ++ tparams.map(mkTypeParamDocumentSymbol),
    )
  }

  /**
    * Returns an EnumMember DocumentSymbol from a Case node.
    */
  private def mkCaseDocumentSymbol(c: TypedAst.Case): DocumentSymbol = c match {
    case TypedAst.Case(_, tag, _, _, loc) => DocumentSymbol(
      tag.name, None, SymbolKind.EnumMember, Range.from(loc), Range.from(loc), Nil, Nil,
    )
  }
}

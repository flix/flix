/*
 * Copyright 2025 Jonathan Lindegaard Starup
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */

package ca.uwaterloo.flix.language.dbg.printer

import ca.uwaterloo.flix.language.ast.Token
import ca.uwaterloo.flix.language.ast.shared.Source
import ca.uwaterloo.flix.language.dbg.DocAst

object TokenPrinter {

  def print(root: Map[Source, Array[Token]]): DocAst.Program = {
    val files = root.map{case (src, tokens) => (src.name, print(tokens))}.toList
    DocAst.Program(Nil, Nil, files)
  }

  private def print(tokens: Array[Token]): DocAst.Expr = {
    val printedTokens = tokens.iterator.map(print).toList
    DocAst.Expr.App(DocAst.Expr.AsIs("Tokens"), printedTokens)
  }

  private def print(token: Token): DocAst.Expr = {
    val kindString = token.kind.getClass.getSimpleName.replace("$", "")
    val contentString = printContent(token)
    DocAst.Expr.AsIs(s"$kindString($contentString)")
  }

  private def printContent(token: Token): String = {
    if (token.startIndex >= 0 && token.endIndex <= token.src.data.length && token.startIndex <= token.endIndex) {
      token.text.replace("\r\n", "\\n").replace("\n", "\\n")
    } else "!bad offset!"
  }

}

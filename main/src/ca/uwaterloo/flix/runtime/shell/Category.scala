/*
 * Copyright 2022 Magnus Madsen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.runtime.shell

import ca.uwaterloo.flix.api.CompilerConstants
import ca.uwaterloo.flix.language.ast.shared.{Input, SecurityContext, Source}
import ca.uwaterloo.flix.language.phase.Lexer

/**
  * A common super-type for the syntactic category of a source code fragment.
  */
sealed trait Category

object Category {
  /**
    * Represents source code that is a declaration.
    */
  case object Decl extends Category

  /**
    * Represents source code that is an expression.
    */
  case object Expr extends Category

  /**
    * Represents source code whose category cannot be determined.
    */
  case object Unknown extends Category

  /**
    * Returns the syntactic category of the given source code string `s`.
    */
  def categoryOf(s: String): Category = {
    val input = Input.VirtualFile(CompilerConstants.VirtualShellFile, s, SecurityContext.Unrestricted)
    val source = Source.fromString(input, s)

    // Tokenize the input and check if the first token looks like the start of a declaration or an expression.
    val (tokens, errors) = Lexer.lex(source)
    if (errors.isEmpty) {
      val start = tokens(0).kind
      (start.isFirstInDecl, start.isFirstInExp) match {
        case (true, _) => Category.Decl
        case (_, true) => Category.Expr
        case _ => Category.Unknown
      }
    } else {
      Category.Unknown
    }
  }
}

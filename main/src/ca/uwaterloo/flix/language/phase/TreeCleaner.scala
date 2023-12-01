/*
 * Copyright 2023 Herluf Baggesen
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
package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.CompilationMessage
import ca.uwaterloo.flix.language.ast.UnstructuredTree.{Child, Tree, TreeKind}
import ca.uwaterloo.flix.language.ast.{Ast, Name, ReadAst, SourceKind, SourceLocation, Symbol, WeededAst}
import ca.uwaterloo.flix.language.errors.Parse2Error
import ca.uwaterloo.flix.util.Validation._
import ca.uwaterloo.flix.util.{ParOps, Validation}
import org.parboiled2.ParserInput
import scala.annotation.tailrec

// TODO: Add change set support

// TODO: Add a way to transform Tree into a weededAst

object TreeCleaner {

  import WeededAst._

  private class State(val src: Ast.Source) {
    // Compute a `ParserInput` when initializing a state for lexing a source.
    // This is necessary to display source code in error messages.
    // See `sourceLocationAtStart` for usage and `SourceLocation` for more information.
    val parserInput: ParserInput = ParserInput.apply(src.data)
  }

  def run(readRoot: ReadAst.Root, entryPoint: Option[Symbol.DefnSym], trees: Map[Ast.Source, Tree])(implicit flix: Flix): Validation[WeededAst.Root, CompilationMessage] = {
    if (flix.options.xparser) {
      // New lexer and parser disabled. Return immediately.
      return Validation.Failure(LazyList.empty)
    }

    flix.phase("TreeCleaner") {
      // Parse each source file in parallel and join them into a WeededAst.Root
      val results = ParOps.parMap(trees) {
        case (src, tree) => mapN(transform(src, tree))(tree => src -> tree)
      }

      mapN(sequence(results))(_.toMap).map(m => WeededAst.Root(m, entryPoint, readRoot.names))
    }
  }

  private def transform(src: Ast.Source, tree: Tree ): Validation[CompilationUnit, CompilationMessage] = {
    implicit val s: State = new State(src)
    mapN(location(tree), visitUsesAndImports(tree), visitDeclarations(tree)) {
      case (loc, usesAndImports, declarations) => CompilationUnit(usesAndImports, declarations, loc)
    }
  }

  private def visitUsesAndImports(tree: Tree)(implicit s: State): Validation[List[UseOrImport], CompilationMessage] = {
    assert(tree.kind == TreeKind.Source)
    List.empty.toSuccess
  }

  private def visitDeclarations(tree: Tree)(implicit s: State): Validation[List[Declaration], CompilationMessage] = {
    assert(tree.kind == TreeKind.Source)
    List.empty.toSuccess
  }

  private def visitDefinition(tree: Tree)(implicit s: State): Validation[Declaration, CompilationMessage] = {
    assert(tree.kind == TreeKind.Def)
    mapN(
      location(tree),
      visitDocumentation(tree),
      visitAnnotations(tree),
      visitModifiers(tree),
      visitNameIdent(tree.children(1)),
      visitTypeParameters(tree),
      visitParameters(tree),
      visitExpression(tree),
      visitType(tree),
      visitTypeConstraints(tree),
      visitConstraints(tree),
      visitEffect(tree)
    ) {
      case (loc, doc, annotations, modifiers, ident, tparams, fparams, exp, ttype, tconstrs, constrs, eff) =>
        Declaration.Def(doc, annotations, modifiers, ident, tparams, fparams, exp, ttype, eff, tconstrs, constrs, loc)
    }
  }

  private def visitNameIdent(c: Child)(implicit s: State): Validation[Name.Ident, CompilationMessage] = {
    Validation.Failure(LazyList.empty)
  }

  private def visitDocumentation(tree: Tree)(implicit s: State): Validation[Ast.Doc, CompilationMessage] = {
    Ast.Doc(List.empty, SourceLocation.Unknown).toSuccess
  }

  private def visitAnnotations(tree: Tree)(implicit s: State): Validation[Ast.Annotations, CompilationMessage] = {
    Ast.Annotations(List.empty).toSuccess
  }

  private def visitModifiers(tree: Tree)(implicit s: State): Validation[Ast.Modifiers, CompilationMessage] = {
    Ast.Modifiers(List.empty).toSuccess
  }

  private def visitTypeParameters(tree: Tree)(implicit s: State): Validation[KindedTypeParams, CompilationMessage] = {
    TypeParams.Elided.toSuccess
  }

  private def visitParameters(tree: Tree)(implicit s: State): Validation[List[FormalParam], CompilationMessage] = {
    List.empty.toSuccess
  }

  private def visitTypeConstraints(tree: Tree)(implicit s: State): Validation[List[TypeConstraint], CompilationMessage] = {
    List.empty.toSuccess
  }

  private def visitConstraints(tree: Tree)(implicit s: State): Validation[List[EqualityConstraint], CompilationMessage] = {
    List.empty.toSuccess
  }

  private def visitExpression(tree: Tree)(implicit s: State): Validation[Expr, CompilationMessage] = {
    mapN(location(tree))(loc => Expr.Error(Parse2Error.DevErr(loc, "expected expression")))
  }

  private def visitType(tree: Tree)(implicit s: State): Validation[Type, CompilationMessage] = {
    mapN(location(tree))(Type.Unit)
  }

  private def visitEffect(tree: Tree)(implicit s: State): Validation[Option[Type], CompilationMessage] = {
    None.toSuccess
  }

  private def location(tree: Tree)(implicit s: State): Validation[SourceLocation, CompilationMessage] = {
    mapN(treeBegin(tree), treeEnd(tree)) {
      case ((b_line, b_col), (e_line, e_col)) =>
        SourceLocation(Some(s.parserInput), s.src, SourceKind.Real, b_line, b_col, e_line, e_col)
    }
  }

  @tailrec
  private def treeBegin(tree: Tree): Validation[(Int, Int), CompilationMessage] = {
    tree.children.headOption match {
      case Some(Child.Token(token)) => (token.line, token.col).toSuccess
      case Some(Child.Tree(tree)) => treeBegin(tree)
      case _ => Validation.Failure(LazyList.empty)
    }
  }

  @tailrec
  private def treeEnd(tree: Tree): Validation[(Int, Int), CompilationMessage] = {
    tree.children.lastOption match {
      case Some(Child.Token(token)) => (token.line, token.col).toSuccess
      case Some(Child.Tree(tree)) => treeEnd(tree)
      case _ => Validation.Failure(LazyList.empty)
    }
  }
}

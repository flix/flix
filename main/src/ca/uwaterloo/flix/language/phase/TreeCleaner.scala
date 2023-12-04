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
import ca.uwaterloo.flix.language.ast.{Ast, Name, ReadAst, SourceLocation, SourcePosition, Symbol, Token, WeededAst}
import ca.uwaterloo.flix.language.errors.Parse2Error
import ca.uwaterloo.flix.util.Validation._
import ca.uwaterloo.flix.util.{ParOps, Validation}
import org.parboiled2.ParserInput

// TODO: Add change set support

// TODO: Remove this

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

  private def transform(src: Ast.Source, tree: Tree): Validation[CompilationUnit, CompilationMessage] = {
    implicit val s: State = new State(src)

    mapN(visitUsesAndImports(tree), visitDeclarations(tree)) {
      case (usesAndImports, declarations) =>
        CompilationUnit(usesAndImports, declarations, tree.loc)
    }
  }

  private def visitUsesAndImports(tree: Tree): Validation[List[UseOrImport], CompilationMessage] = {
    assert(tree.kind == TreeKind.Source)
    List.empty.toSuccess
  }

  private def visitDeclarations(tree: Tree)(implicit s: State): Validation[List[Declaration], CompilationMessage] = {
    assert(tree.kind == TreeKind.Source)
    sequence(pickAll(TreeKind.Def)(tree.children).map(visitDefinition))
  }

  private def visitDefinition(tree: Tree)(implicit s: State): Validation[Declaration, CompilationMessage] = {
    assert(tree.kind == TreeKind.Def)
    mapN(
      visitDocumentation(tree),
      visitAnnotations(tree),
      visitModifiers(tree),
      visitNameIdent(tree),
      visitTypeParameters(tree),
      visitParameters(tree),
      visitExpression(tree),
      visitType(tree),
      visitTypeConstraints(tree),
      visitConstraints(tree),
      visitEffect(tree)
    ) {
      case (doc, annotations, modifiers, ident, tparams, fparams, exp, ttype, tconstrs, constrs, eff) =>
        Declaration.Def(doc, annotations, modifiers, ident, tparams, fparams, exp, ttype, eff, tconstrs, constrs, tree.loc)
    }
  }

  private def visitDocumentation(tree: Tree): Validation[Ast.Doc, CompilationMessage] = {
    val docTree = tryPick(TreeKind.Doc, tree.children)
    val loc = docTree.map(_.loc).getOrElse(SourceLocation.Unknown)
    val comments = docTree.map(text).map(
      _.map(_.stripPrefix("///").trim())
        .filter(_ != "")
    ).getOrElse(List.empty)
    Ast.Doc(comments, loc).toSuccess
  }

  private def visitAnnotations(tree: Tree): Validation[Ast.Annotations, CompilationMessage] = {
    // TODO
    Ast.Annotations(List.empty).toSuccess
  }

  private def visitModifiers(tree: Tree): Validation[Ast.Modifiers, CompilationMessage] = {
    // TODO
    Ast.Modifiers(List.empty).toSuccess
  }

  private def visitTypeParameters(tree: Tree): Validation[KindedTypeParams, CompilationMessage] = {
    // TODO
    TypeParams.Elided.toSuccess
  }

  private def visitParameters(tree: Tree): Validation[List[FormalParam], CompilationMessage] = {
    // TODO
    List.empty.toSuccess
  }

  private def visitTypeConstraints(tree: Tree): Validation[List[TypeConstraint], CompilationMessage] = {
    // TODO
    List.empty.toSuccess
  }

  private def visitConstraints(tree: Tree): Validation[List[EqualityConstraint], CompilationMessage] = {
    // TODO
    List.empty.toSuccess
  }

  private def visitExpression(tree: Tree): Validation[Expr, CompilationMessage] = {
    // TODO
    Expr.Error(Parse2Error.DevErr(tree.loc, "expected expression")).toSuccess
  }

  private def visitType(tree: Tree): Validation[Type, CompilationMessage] = {
    // TODO
    Type.Unit(tree.loc).toSuccess
  }

  private def visitEffect(tree: Tree): Validation[Option[Type], CompilationMessage] = {
    // TODO
    None.toSuccess
  }

  private def visitNameIdent(tree: Tree)(implicit s: State): Validation[Name.Ident, CompilationMessage] = {
    flatMapN(pick(TreeKind.Ident, tree.children))(t => t.children(0) match {
      case Child.Token(token) => Name.Ident(
        SourcePosition(s.src, token.line + 1, token.col + 1, Some(s.parserInput)),
        token.text,
        SourcePosition(s.src, token.line + 1, token.col + 1 + (token.end - token.start), Some(s.parserInput))
      ).toSuccess
      case _ => Validation.Failure(LazyList.empty)
    })
  }

  // A helper that collects the text in token children
  private def text(tree: Tree): List[String] = {
    tree.children.foldLeft[List[String]](List.empty)((acc, c) => c match {
      case Child.Token(token) => acc :+ token.text
      case Child.Tree(_) => acc
    })
  }

  // A helper that picks out the first tree of a specific kind from a list of children
  private def pick(kind: TreeKind, children: Array[Child]): Validation[Tree, CompilationMessage] = {
    tryPick(kind, children)
      .map(_.toSuccess)
      .getOrElse(Validation.Failure(LazyList.empty)) // TODO: Error here?
  }

  private def tryPick(kind: TreeKind, children: Array[Child]): Option[Tree] = {
    children.find {
      case Child.Tree(tree) if tree.kind == kind => true
      case _ => false
    } match {
      case Some(Child.Tree(tree)) => Some(tree)
      case _ => None
    }
  }

  // A helper that picks out trees of a specific kind from a list of children
  private def pickAll(kind: TreeKind)(children: Array[Child]): List[Tree] = {
    children.foldLeft[List[Tree]](List.empty)((acc, child) => child match {
      case Child.Tree(tree) if tree.kind == kind => acc.appended(tree)
      case _ => acc
    })
  }
}

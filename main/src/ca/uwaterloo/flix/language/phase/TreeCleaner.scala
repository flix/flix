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

object TreeCleaner {

  import WeededAst._

  // A helper that picks out trees of a specific kind from a list of children
  private def pick(kind: TreeKind)(children: Array[Child]): List[Tree] = {
    children.foldLeft[List[Tree]](List.empty)((acc, child) => child match {
      case Child.Tree(tree) if tree.kind == kind => acc.appended(tree)
      case _ => acc
    })
  }

  def run(readRoot: ReadAst.Root, entryPoint: Option[Symbol.DefnSym], trees: Map[Ast.Source, Tree])(implicit flix: Flix): Validation[WeededAst.Root, CompilationMessage] = {
    if (flix.options.xparser) {
      // New lexer and parser disabled. Return immediately.
      return Validation.Failure(LazyList.empty)
    }

    flix.phase("TreeCleaner") {
      // Parse each source file in parallel and join them into a WeededAst.Root
      val results = ParOps.parMap(trees) {
        case (src, tree) => mapN(transform(tree))(tree => src -> tree)
      }

      mapN(sequence(results))(_.toMap).map(m => WeededAst.Root(m, entryPoint, readRoot.names))
    }
  }

  private def transform(tree: Tree): Validation[CompilationUnit, CompilationMessage] = {
    mapN(visitUsesAndImports(tree), visitDeclarations(tree)) {
      case (usesAndImports, declarations) => CompilationUnit(usesAndImports, declarations, tree.loc)
    }
  }

  private def visitUsesAndImports(tree: Tree): Validation[List[UseOrImport], CompilationMessage] = {
    assert(tree.kind == TreeKind.Source)
    List.empty.toSuccess
  }

  private def visitDeclarations(tree: Tree): Validation[List[Declaration], CompilationMessage] = {
    assert(tree.kind == TreeKind.Source)
    sequence(pick(TreeKind.Def)(tree.children).map(visitDefinition))
  }

  private def visitDefinition(tree: Tree): Validation[Declaration, CompilationMessage] = {
    assert(tree.kind == TreeKind.Def)
    mapN(
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
      case (doc, annotations, modifiers, ident, tparams, fparams, exp, ttype, tconstrs, constrs, eff) =>
        Declaration.Def(doc, annotations, modifiers, ident, tparams, fparams, exp, ttype, eff, tconstrs, constrs, tree.loc)
    }
  }

  private def visitNameIdent(c: Child): Validation[Name.Ident, CompilationMessage] = {
    // TODO
    Validation.Failure(LazyList.empty)
  }

  private def visitDocumentation(tree: Tree): Validation[Ast.Doc, CompilationMessage] = {
    // TODO
    Ast.Doc(List.empty, SourceLocation.Unknown).toSuccess
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
}

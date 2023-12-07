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
import ca.uwaterloo.flix.language.ast.{Ast, Name, ReadAst, SourceLocation, SourcePosition, Symbol, Token, TokenKind, WeededAst}
import ca.uwaterloo.flix.language.errors.Parse2Error
import ca.uwaterloo.flix.util.Validation._
import ca.uwaterloo.flix.util.{ParOps, Validation}
import org.parboiled2.ParserInput

// TODO: Add change set support

object Weeder2 {

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

    flix.phase("Weeder2") {
      // Parse each source file in parallel and join them into a WeededAst.Root
      val results = ParOps.parMap(trees) {
        case (src, tree) => mapN(weed(src, tree))(tree => src -> tree)
      }

      mapN(sequence(results))(_.toMap).map(m => WeededAst.Root(m, entryPoint, readRoot.names))
    }
  }

  def weed(src: Ast.Source, tree: Tree): Validation[CompilationUnit, CompilationMessage] = {
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
    // strip prefixing `///` and remove empty lines after that
    val comments = docTree.map(text).map(
      _.map(_.stripPrefix("///").trim())
        .filter(_ != "")
    ).getOrElse(List.empty)
    Ast.Doc(comments, loc).toSuccess
  }

  private def visitAnnotations(tree: Tree)(implicit s: State): Validation[Ast.Annotations, CompilationMessage] = {
    val annotationTree = tryPick(TreeKind.Annotations, tree.children)
    val annotations = annotationTree.map(
      t => sequence(t.children.map(visitAnnotation))
    ).getOrElse(List.empty.toSuccess)
    annotations.map(annotations => Ast.Annotations(annotations))
  }

  private def visitAnnotation(c: Child)(implicit s: State): Validation[Ast.Annotation, CompilationMessage] = {
    c match {
      case Child.Token(token) =>
        val loc = token.mkSourceLocation(s.src, Some(s.parserInput))
        import Ast.Annotation._
        // TODO: annotation.Error is a soft failure
        token.text match {
          case "@benchmark" | "@Benchmark" => Benchmark(loc).toSuccess
          case "@Deprecated" => Deprecated(loc).toSuccess
          case "@Experimental" => Experimental(loc).toSuccess
          case "@Internal" => Internal(loc).toSuccess
          case "@Parallel" => Parallel(loc).toSuccess
          case "@ParallelWhenPure" => ParallelWhenPure(loc).toSuccess
          case "@Lazy" => Lazy(loc).toSuccess
          case "@LazyWhenPure" => LazyWhenPure(loc).toSuccess
          case "@MustUse" => MustUse(loc).toSuccess
          case "@Skip" => Skip(loc).toSuccess
          case "@Test" | "@test" => Test(loc).toSuccess
          case "@TailRec" => TailRecursive(loc).toSuccess
          case other => softFailWith(Ast.Annotation.Error(other.stripPrefix("@"), loc))
        }
      case Child.Tree(tree) => failWith(s"expected annotation but found subtree of kind ${tree.kind}")
    }
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
    val paramTree = tryPick(TreeKind.Parameters, tree.children)
    val params = paramTree.map(
      t => {
        val parameters = if (t.children.length == 2) {
          // only ParenL and ParenR are in parameters, so produce synthetic unit parameter
          List(
            FormalParam(
              Name.Ident(SourcePosition.Unknown, "_unit", SourcePosition.Unknown),
              Ast.Modifiers(List.empty),
              Some(Type.Unit(SourceLocation.Unknown)),
              SourceLocation.Unknown
            )
          )
        } else {
          List.empty // TODO: Map parameters
        }
        parameters.toSuccess
      }
    ).getOrElse(failWith("expected formal parameters"))

    params
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
    val exprTree = tryPick(TreeKind.Expr.Expr, tree.children)
    val loc = exprTree.map(_.loc).getOrElse(SourceLocation.Unknown)
    val notFoundError = softFailWith(Expr.Error(Parse2Error.DevErr(loc, "expected expression")))

    exprTree.map(_.children(0) match {
      case Child.Tree(tree) => tree.kind match {
        case TreeKind.Expr.Literal => visitLiteral(tree)
        case _ => notFoundError
      }
      case _ => notFoundError
    }).getOrElse(notFoundError)
  }

  private def visitLiteral(tree: Tree): Validation[Expr, CompilationMessage] = {
    val literal = tree.kind match {
      case TreeKind.Expr.Literal => tree.children(0) match {
        case Child.Token(token) => token.kind match {
          case TokenKind.ParenL => Expr.Tuple(List.empty, tree.loc)
          case _ => Expr.Error(Parse2Error.DevErr(tree.loc, "expected literal"))
        }
        case _ => Expr.Error(Parse2Error.DevErr(tree.loc, "expected literal"))
      }
      case _ => Expr.Error(Parse2Error.DevErr(tree.loc, "expected literal"))
    }

    literal.toSuccess
  }

  private def visitType(tree: Tree)(implicit s: State): Validation[Type, CompilationMessage] = {
    val typeTree = tryPick(TreeKind.Type.Type, tree.children)
    val loc = typeTree.map(_.loc).getOrElse(SourceLocation.Unknown)
    val ident = typeTree.map(visitNameIdent(_)).getOrElse(failWith("TODO: types are more that idents"))
    mapN(ident) {
      ident =>
        Type.Ambiguous(
          Name.QName(ident.sp1, Name.NName(ident.sp1, List.empty, ident.sp2), ident, ident.sp2),
          loc)
    }
  }

  private def visitEffect(tree: Tree): Validation[Option[Type], CompilationMessage] = {
    // TODO
    None.toSuccess
  }

  private def visitNameIdent(tree: Tree)(implicit s: State): Validation[Name.Ident, CompilationMessage] = {
    flatMapN(pick(TreeKind.Ident, tree.children))(t => t.children(0) match {
      case Child.Token(token) => Name.Ident(
        token.mkSourcePosition(s.src, Some(s.parserInput)),
        token.text,
        token.mkSourcePositionEnd(s.src, Some(s.parserInput))
      ).toSuccess
      case _ => failWith("expected TreeKind.Ident to contain a Child.Token")
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
      .getOrElse(failWith(s"expected to find at least one $kind"))
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

  private def failWith[T](message: String): Validation[T, CompilationMessage] = {
    Validation.Failure(LazyList(Parse2Error.DevErr(SourceLocation.Unknown, message)))
  }

  private def softFailWith[T](result: T): Validation[T, CompilationMessage] = {
    Validation.SoftFailure(result, LazyList())
  }
}

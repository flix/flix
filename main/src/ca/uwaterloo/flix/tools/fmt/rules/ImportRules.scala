package ca.uwaterloo.flix.tools.fmt.rules

import ca.uwaterloo.flix.language.ast.SyntaxTree
import ca.uwaterloo.flix.language.ast.SyntaxTree.TreeKind
import ca.uwaterloo.flix.tools.fmt.Doc
import ca.uwaterloo.flix.tools.fmt.Doc.{empty, lineBreak}

object ImportRules extends FormatterModule {

  val rules: PartialFunction[SyntaxTree.Tree, Doc] = {
    case tree if tree.kind == TreeKind.UsesOrImports.UseOrImportList =>
      formatUseOrImportList(tree)
  }

  private def formatUseOrImportList(tree: SyntaxTree.Tree): Doc = {
    val importTrees = tree.children.toList.collect {
      case t: SyntaxTree.Tree
        if t.kind == TreeKind.UsesOrImports.Use ||
          t.kind == TreeKind.UsesOrImports.UseMany ||
          t.kind == TreeKind.UsesOrImports.Import => t
    }

    val (uses, imports) = importTrees.partition(t =>
      t.kind == TreeKind.UsesOrImports.Use ||
        t.kind == TreeKind.UsesOrImports.UseMany
    )

    val sortedUses    = uses.sortBy(importSortKey)
    val sortedImports = imports.sortBy(importSortKey)

    val allSorted = sortedUses ++ sortedImports

    allSorted.foldLeft(empty) { (acc, t) =>
      acc <> treeToDoc(t) <> lineBreak("\n")
    }
  }

  private def importSortKey(tree: SyntaxTree.Tree): String = {
    val tokens = collectTokens(tree)
    tokens.map(_.text).mkString("")
  }
}


package ca.uwaterloo.flix.tools

import ca.uwaterloo.flix.language.CompilationMessage
import ca.uwaterloo.flix.language.ast.Ast
import ca.uwaterloo.flix.language.ast.TypedAst.Root
import ca.uwaterloo.flix.util.Validation
import ca.uwaterloo.flix.util.Validation._

object Summarizer {

  def printSummary(v: Validation[Root, CompilationMessage]): Unit = mapN(v) {
    case root =>

      for ((source, loc) <- root.sources.toList.sortBy(_._1.name)) {
        val module = source.name
        val numberOfLines = loc.endLine
        val numberOfDefs = countDefs(root, source)
        val numberOfClasses= countClasses(root, source)
        val numberOfInstances= countInstances(root, source)
        if (include(module, numberOfLines)) {
          println(s"$module & ${thousands(numberOfLines)} & $numberOfDefs & $numberOfClasses & $numberOfInstances \\\\") // TODO: global constants.
        }
      }
  }

  private def include(mod: String, lines: Int): Boolean =
    !mod.contains("/") && lines > 100

  private def thousands(n: Int): String = f"$n%,d".replace(".", ",")

  private def countDefs(root: Root, source: Ast.Source): Int =
    root.defs.count {
      case (sym, _) => sym.loc.source == source
    }

  private def countClasses(root: Root, source: Ast.Source): Int =
    root.classes.count {
      case (sym, _) => sym.loc.source == source
    }

  private def countInstances(root: Root, source: Ast.Source): Int =
    root.instances.count {
      case (sym, _) => sym.loc.source == source
    }

}

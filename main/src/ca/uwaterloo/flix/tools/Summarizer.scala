package ca.uwaterloo.flix.tools

import ca.uwaterloo.flix.language.CompilationMessage
import ca.uwaterloo.flix.language.ast.{Ast, Type}
import ca.uwaterloo.flix.language.ast.TypedAst.{Def, Root}
import ca.uwaterloo.flix.util.Validation
import ca.uwaterloo.flix.util.Validation._

object Summarizer {

  private val Separator = " & "

  private val EndOfLine = "\\\\"

  def printSummary(v: Validation[Root, CompilationMessage]): Unit = mapN(v) {
    case root =>

      print(padRight("Module", 30))
      print(Separator)
      print(padLeft("Functions", 20))

      for ((source, loc) <- root.sources.toList.sortBy(_._1.name)) {
        val module = source.name
        val numberOfLines = loc.endLine
        val defs = getDefs(root, source)
        val numberOfDefs = defs.size
        val pureDefs = defs.count(isPure)
        val numberOfClasses = countClasses(root, source)
        val numberOfInstances = countInstances(root, source)
        if (include(module, numberOfLines)) {
          print(padRight(module, 30))
          print(Separator)
          print(padLeft(thousands(numberOfLines), 10))
          println(EndOfLine)
        }
      }
  }

  private def include(mod: String, lines: Int): Boolean =
    !mod.contains("/") && lines > 100

  private def thousands(n: Int): String = f"$n%,d".replace(".", ",")

  private def getDefs(root: Root, source: Ast.Source): Iterable[Def] =
    root.defs.collect {
      case (sym, defn) if sym.loc.source == source => defn
    }

  private def countDefs(root: Root, source: Ast.Source): Int =
    root.defs.count {
      case (sym, _) => sym.loc.source == source
    }

  private def countHigherOrderDefs(root: Root, source: Ast.Source): Int =
    root.defs.count {
      case (sym, defn) =>
        isHigherOrder(defn.spec.declaredScheme.base) &&
          sym.loc.source == source
    }


  private def countClasses(root: Root, source: Ast.Source): Int =
    root.classes.count {
      case (sym, _) => sym.loc.source == source
    }

  private def countInstances(root: Root, source: Ast.Source): Int =
    root.instances.count {
      case (sym, _) => sym.loc.source == source
    }

  private def isPure(defn: Def): Boolean = defn.spec.pur == Type.Pure

  private def isHigherOrder(tpe: Type): Boolean = true

  private def padRight(s: String, l: Int): String = s.padTo(l, ' ')

  def padLeft(s: String, l: Int): String = {
    if (s.length >= l) return s
    val sb = new StringBuilder
    while (sb.length < l - s.length) sb.append(' ')
    sb.append(s)
    sb.toString
  }

}

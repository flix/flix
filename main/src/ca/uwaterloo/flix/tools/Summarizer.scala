package ca.uwaterloo.flix.tools

import ca.uwaterloo.flix.language.CompilationMessage
import ca.uwaterloo.flix.language.ast.{Ast, Type}
import ca.uwaterloo.flix.language.ast.TypedAst.{Def, Root}
import ca.uwaterloo.flix.util.Validation
import ca.uwaterloo.flix.util.Validation._

object Summarizer {

  private val Separator = " & "

  private val EndOfLine = " \\\\"

  private val ModWidth = 30

  private val ColWidth = 12

  private def include(mod: String, lines: Int): Boolean =
    !mod.contains("/") && lines > 700

  def printSummary(v: Validation[Root, CompilationMessage]): Unit = mapN(v) {
    case root =>

      print(padR("Module", ModWidth))
      print(Separator)
      print(padL("Lines", ColWidth))
      print(Separator)
      print(padL("Functions", ColWidth))
      print(Separator)
      print(padL("Pure", ColWidth))
      print(Separator)
      print(padL("Impure", ColWidth))
      print(Separator)
      print(padL("Polymorphic", ColWidth))
      println(EndOfLine)

      var totalLines = 0
      var totalFunctions = 0
      var totalPureFunctions = 0
      var totalImpureFunctions = 0
      var totalEffPolymorphicFunctions = 0

      for ((source, loc) <- root.sources.toList.sortBy(_._1.name)) {
        val module = source.name
        val lines = loc.endLine
        val defs = getDefs(root, source)

        val numberOfLines = format(lines)
        val numberOfFunctions = defs.size
        val numberOfPureFunctions = defs.count(isPure)
        val numberOfImpureFunctions = defs.count(isImpure)
        val numberOfEffectPolymorphicFunctions = defs.count(isEffectPolymorphic)

        totalLines = totalLines + lines
        totalFunctions = totalFunctions + numberOfFunctions
        totalPureFunctions = totalPureFunctions + numberOfPureFunctions
        totalImpureFunctions = totalImpureFunctions + numberOfImpureFunctions
        totalEffPolymorphicFunctions = totalEffPolymorphicFunctions + numberOfEffectPolymorphicFunctions

        if (include(module, lines)) {
          print(padR(module, ModWidth))
          print(Separator)
          print(padL(numberOfLines, ColWidth))
          print(Separator)
          print(padL(format(numberOfFunctions), ColWidth))
          print(Separator)
          print(padL(format(numberOfPureFunctions), ColWidth))
          print(Separator)
          print(padL(format(numberOfImpureFunctions), ColWidth))
          print(Separator)
          print(padL(format(numberOfEffectPolymorphicFunctions), ColWidth))
          println(EndOfLine)
        }
      }

      println("---")
      print(padR("Totals", ModWidth))
      print(Separator)
      print(padL(format(totalLines), ColWidth))
      print(Separator)
      print(padL(format(totalFunctions), ColWidth))
  }

  private def getDefs(root: Root, source: Ast.Source): Iterable[Def] =
    root.defs.collect {
      case (sym, defn) if sym.loc.source == source => defn
    }

  private def format(n: Int): String = f"$n%,d".replace(".", ",")

  private def isPure(defn: Def): Boolean = defn.spec.pur == Type.Pure

  private def isImpure(defn: Def): Boolean = defn.spec.pur == Type.Impure

  private def isEffectPolymorphic(defn: Def): Boolean = !isPure(defn) && !isImpure(defn)

  /**
    * Right-pads the given string `s` to length `l`.
    */
  private def padR(s: String, l: Int): String = s.padTo(l, ' ')

  /**
    * Left-pads the given string `s` to length `l`.
    */
  private def padL(s: String, l: Int): String = {
    if (s.length >= l) {
      return s
    }
    val sb = new StringBuilder
    while (sb.length < l - s.length) {
      sb.append(' ')
    }
    sb.append(s)
    sb.toString
  }

}

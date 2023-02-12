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

      for ((source, loc) <- root.sources.toList.sortBy(_._1.name)) {
        val module = source.name
        val lines = loc.endLine
        val numberOfLines = number(lines)
        val defs = getDefs(root, source)

        val numberOfDefs = number(defs.size)
        val numberOfPureDefs = number(defs.count(isPure))
        val numberOfImpureDefs = number(defs.count(isImpure))
        val numberOfEffectPolymorphicDefs = number(defs.count(isEffectPolymorphic))

        if (include(module, lines)) {
          print(padR(module, ModWidth))
          print(Separator)
          print(padL(numberOfLines, ColWidth))
          print(Separator)
          print(padL(numberOfDefs, ColWidth))
          print(Separator)
          print(padL(numberOfPureDefs, ColWidth))
          print(Separator)
          print(padL(numberOfImpureDefs, ColWidth))
          print(Separator)
          print(padL(numberOfEffectPolymorphicDefs, ColWidth))
          println(EndOfLine)
        }
      }
  }

  private def include(mod: String, lines: Int): Boolean =
    !mod.contains("/") && lines > 700

  private def number(n: Int): String = f"$n%,d".replace(".", ",")

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

  private def isImpure(defn: Def): Boolean = defn.spec.pur == Type.Impure

  private def isEffectPolymorphic(defn: Def): Boolean = !isPure(defn) && !isImpure(defn)

  private def isHigherOrder(tpe: Type): Boolean = true

  private def padR(s: String, l: Int): String = s.padTo(l, ' ')

  def padL(s: String, l: Int): String = {
    if (s.length >= l) return s
    val sb = new StringBuilder
    while (sb.length < l - s.length) sb.append(' ')
    sb.append(s)
    sb.toString
  }

}

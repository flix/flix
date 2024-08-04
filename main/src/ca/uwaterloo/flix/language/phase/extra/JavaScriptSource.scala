package ca.uwaterloo.flix.language.phase.extra

import ca.uwaterloo.flix.language.dbg.Doc
import ca.uwaterloo.flix.language.dbg.Doc._

object JavaScriptSource {

  def function(name: Doc, args: List[Doc], body: Doc)(implicit indent: Indent): Doc = {
    group(text("function") +: name :: tuple(args) +: curlyOpen(body))
  }

  def namespacedName(ns: List[String], name: String): Doc = text(ns match {
    case Nil => mkSafe(name)
    case _ => ns.map(mkSafe).mkString("", "γ", s"γ${mkSafe(name)}")
  })

  def mkSafe(s: String): String = s // TODO

}

package ca.uwaterloo.flix.language.dbg.prettierPrettyPrinting

import ca.uwaterloo.flix.language.dbg.prettierPrettyPrinting.Doc._

import java.lang.reflect.Method
import scala.collection.immutable

object DocUtil {

  def bracket(l: String, x: Doc, r: String)(implicit i: Indent): Doc = {
    text(l) :: nest(breakWith("") :: x) \: text(r)
  }

  /**
    * A space/line following the sep
    */
  def groupVSep(sep: String, d: List[Doc]): Doc =
    group(fold(_ :: text(sep) +\: _, d))

  def commaSep(d: List[Doc]): Doc = groupVSep(",", d)

  object Language {

    def tuplef(t: List[Doc])(implicit i: Indent): Doc = t match {
      case Nil => text("()") // this omits the possible line breaks
      case _ => group(bracket("(", sep(text(",") :: breakWith(" "), t), ")"))
    }

    def selectiveTuplef(t: List[Doc])(implicit i: Indent): Doc = t match {
      case Nil => text("()")
      case single :: Nil => single
      case _ => tuplef(t)
    }

    def parens(d: Doc)(implicit i: Indent): Doc = group(bracket("(", d, ")"))

    def applyf(fun: Doc, args: List[Doc])(implicit i: Indent): Doc = {
      fun :: tuplef(args)
    }

    /**
      * def f(x: t, x: t): t = e
      *
      * def f(x: t, x: t): t =
      * e
      *
      * def f(
      * x: t,
      * x: t
      * ): t =
      * e
      */
    def defnf(name: String, args: List[Doc], resType: Doc, body: Doc)(implicit i: Indent): Doc = {
      group(
        group(
          text("def") +:
            text(name) :: bracket("(", sep(text(",") :: breakWith(" "), args), ")") ::
            text(":") +: resType +: text("=")
        ) +: bracket("{", body, "}")
      )
    }
  }
}

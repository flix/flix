package ca.uwaterloo.flix.language.dbg.prettierPrettyPrinting.`lazy`

import Doc._

import scala.collection.immutable

object DocUtil {

  def fold(f: (Doc, Doc) => Doc, d: List[Doc]): Doc = d match {
    case immutable.Nil => nil
    case x :: immutable.Nil => x
    case x :: xs => f(x, fold(f, xs))
  }

  def spaces(d: List[Doc]): Doc = fold(_ <+> _, d)

  def lines(d: List[Doc]): Doc = fold(_ <\> _, d)

  def bracket(l: String, x: Doc, r: String)(implicit indent: Int): Doc = {
    group("")(text(l) <> nest(line <> x) <\> text(r))
  }

  def sep(sep: Doc, d: List[Doc]): Doc = fold(_ <> sep <> _, d)

  /**
    * A space/line following the sep
    */
  def groupSep(sep: String, d: List[Doc]): Doc =
    group(" ")(fold(_ <> text(sep) <\> _, d))

  def parens(d: Doc)(implicit indent: Int): Doc = bracket("(", d, ")")

  def defnf(name: String, args: List[Doc], resType: Doc, body: Doc)(implicit indent: Int): Doc = {
    text("def") <+>
      text(name) <> tuplef(args) <>
      text(":") <+> resType <+> text("=") <+\\>
      body
  }

  def arrowf(args: List[Doc], res: Doc)(implicit indent: Int): Doc = {
    selectiveTuplef(args) <+> text("->") <+\\> res
  }

  def eqf(d1: Doc, d2: Doc)(implicit indent: Int): Doc =
    d1 <+> text("=") <+\\> d2

  def recordExtendf(fields: List[(Doc, Doc)], rest: Doc)(implicit indent: Int): Doc = {
    val f = fields.map{case (x, y) => eqf(x, y)}
    bracket("{", group(" ")(sep(text(",") <> line, f) <+> text("|") <+\\> rest), "}")
  }

  def schemaExtendf(fields: List[(Doc, Doc)], rest: Doc)(implicit indent: Int): Doc =
    text("#") <> recordExtendf(fields, rest)

  def typeAppf(tpe: Doc, args: List[Doc])(implicit indent: Int): Doc = {
    tpe <> bracket("[", commaSep(args),"]")
  }

  def ascf(exp: Doc, tpe: Doc)(implicit indent: Int): Doc =
    exp <> text(":") <+\\> tpe

  def commaSep(d: List[Doc]): Doc = groupSep(",", d)

  def tuplef(t: List[Doc])(implicit indent: Int): Doc =
    parens(commaSep(t))

  def selectiveTuplef(t: List[Doc])(implicit indent: Int): Doc = t match {
    case Nil => text("()")
    case single :: Nil => single
    case _ => parens(commaSep(t))
  }

  def main(args: Array[String]): Unit = {
    implicit val indent: Int = 4
    val doc = defnf(
      "f",
      List(text("x: String"), text("y: Int32"), text("z: Bool")),
      text("String"),
      text("x + y + z ......................")
    )
    println(pretty(0, doc))
    println(pretty(40, doc))
    println(pretty(50, doc))
    println(pretty(100, doc))
  }



}

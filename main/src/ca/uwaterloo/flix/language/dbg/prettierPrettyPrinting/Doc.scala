package ca.uwaterloo.flix.language.dbg.prettierPrettyPrinting

import scala.annotation.tailrec
import scala.collection.{immutable, mutable}

sealed trait Doc

object Doc {

  implicit class DocOps(d: Doc) {
    def ::(d: Doc): Doc = Doc.::(d, this.d)

    def +:(d: Doc): Doc = Doc.+:(d, this.d)

    def +\:(d: Doc): Doc = Doc.+\:(d, this.d)

    def \:(d: Doc): Doc = Doc.\:(d, this.d)
  }

  private case object Nil extends Doc

  private case class Cons(d1: Doc, d2: Doc) extends Doc

  private case class Text(s: String) extends Doc

  private case class Nest(i: Int, d: Doc) extends Doc

  private case class Break(s: String) extends Doc

  private case class Group(d: Doc) extends Doc

  sealed trait Indent

  private case class Indentation(i: Int) extends Indent

  private def indentI(i: Indent): Int = i match {
    case Indentation(i) => i
  }

  def indentationLevel(i: Int): Indent = Indentation(i)

  def deconstruct(d: Doc)(implicit indent: Indent): Doc = d match {
    case Nil => Nil
    case Cons(d1, d2) => Cons(deconstruct(d1), deconstruct(d2))
    case Text(s) => Text(s)
    case Nest(i, d) => Nest(i, deconstruct(d))
    case Break(s) => Break(s)
    case Group(d) => Group(deconstruct(d))
  }

  /**
    * Concatenates two docs.
    */
  def ::(d1: Doc, d2: Doc): Doc = Cons(d1, d2)

  /**
    * The empty document.
    */
  def empty: Doc = Nil

  /**
    * The document of the string `s`.
    * This string must not contain newlines!
    */
  def text(s: String): Doc = Text(s)

  /**
    * _If_ newlines are printed in `d` they will have another level of
    * indentation.
    */
  def nest(d: Doc)(implicit i: Indent): Doc = Nest(indentI(i), d)

  /**
    * Inserts the string `s` is space is available, otherwise a newline is used.
    */
  def breakWith(s: String): Doc = Break(s)

  /**
    * Groups all newlines in `d`. Inner groups can only use newlines if outer
    * groups have, and all newlines in this group will be triggered together.
    */
  def group(d: Doc): Doc = Group(d)

  /**
    * This data exists as a simpler format that [[Doc]] is translated into
    * before computing a string.
    */
  private sealed trait SDoc

  private case object SNil extends SDoc

  private case class SText(s: String, d: SDoc) extends SDoc

  private case class SLine(i: Int, d: SDoc) extends SDoc


  /**
    * Returns the string representation of `d`.
    */
  private def sdocToString(d: SDoc): String = {
    val sb = new mutable.StringBuilder()

    @tailrec
    def aux(d: SDoc): Unit = d match {
      case SNil => ()
      case SText(s, x) => sb.append(s); aux(x)
      case SLine(i, x) => sb.append("\n"); sb.append(" " * i); aux(x)
    }

    aux(d)
    sb.toString
  }

  /**
    * [[Mode]] is used to keep track of the context while computing the layout
    * of a [[Doc]].
    */
  private sealed trait Mode

  private case object MFlat extends Mode

  private case object MBreak extends Mode

  /**
    * Returns true if the next element in `l` fits the available space `w`.
    *
    * @param w available width
    */
  @tailrec
  private def fits(w: Int, l: List[(Int, Mode, Doc)]): Boolean = l match {
    case _ if w < 0 => false
    case immutable.Nil => true
    case (_, _, Nil) :: z => fits(w, z)
    case (i, m, Cons(x, y)) :: z =>
      fits(w, (i, m, x) :: (i, m, y) :: z)
    case (i, m, Nest(j, x)) :: z => fits(w, (i + j, m, x) :: z)
    case (_, _, Text(s)) :: z => fits(w - s.length, z)
    case (_, MFlat, Break(s)) :: z => fits(w - s.length, z)
    case (_, MBreak, Break(_)) :: _ => true // impossible
    case (i, _, Group(x)) :: z => fits(w, (i, MFlat, x) :: z)
  }

  /**
    * Returns the [[SDoc]] layout of `l` with the maximum width `w` and used
    * space `k`.
    *
    * @param w maximum width
    * @param k used width
    */
  @tailrec
  private def format(w: Int, k: Int, l: List[(Int, Mode, Doc)], cont: SDoc => SDoc): SDoc = l match {
    case immutable.Nil =>
      cont(SNil)
    case (_, _, Nil) :: z =>
      format(w, k, z, cont)
    case (i, m, Cons(x, y)) :: z =>
      format(w, k, (i, m, x) :: (i, m, y) :: z, cont)
    case (i, m, Nest(j, x)) :: z =>
      format(w, k, (i + j, m, x) :: z, cont)
    case (_, _, Text(s)) :: z =>
      format(w, k + s.length, z, v1 => cont(SText(s, v1)))
    case (_, MFlat, Break(s)) :: z =>
      format(w, k + s.length, z, v1 => cont(SText(s, v1)))
    case (i, MBreak, Break(_)) :: z =>
      format(w, i, z, v1 => cont(SLine(i, v1)))
    case (i, _, Group(x)) :: z =>
      if (fits(w - k, (i, MFlat, x) :: z)) {
        format(w, k, (i, MFlat, x) :: z, cont)
      } else {
        format(w, k, (i, MBreak, x) :: z, cont)
      }
  }

  /**
    * Prints the [[Doc]] `d` with maximum width `w` given an implicit
    * indentation level `i`.
    *
    * @param w maximum width
    * @param d the document to print
    * @param i the width of each indentation level
    */
  def pretty(w: Int, d: Doc)(implicit i: Indent): String =
    sdocToString(format(w, 0, List((0, MBreak, deconstruct(d))), x => x))

  // aux

  /**
    * Concatenates two docs with a space.
    */
  def +:(d1: Doc, d2: Doc): Doc =
    d1 :: text(" ") :: d2

  /**
    * Concatenates two docs with a space _or_ an ungrouped newline.
    */
  def +\:(d1: Doc, d2: Doc): Doc =
    d1 :: breakWith(" ") :: d2

  /** Concatenates two docs with an ungrouped optional newline. */
  def \:(d1: Doc, d2: Doc): Doc =
    d1 :: breakWith("") :: d2

  /**
    * Concatenates two docs with a space _or_ a grouped optional newline with
    * indentation.
    * `d2` is included in the group.
    */
  def groupBreakIndent(d1: Doc, d2: Doc)(implicit i: Indent): Doc =
    d1 :: group(nest(breakWith(" ") :: d2))

  /**
    * Concatenates two docs with a space _or_ an ungrouped optional newline with
    * indentation.
    * `d2` is included in the indentation
    */
  def breakIndent(d1: Doc, d2: Doc)(implicit i: Indent): Doc =
    d1 :: nest(breakWith(" ") :: d2)

  /**
    * Right fold of `d` with `f`.
    */
  def fold(f: (Doc, Doc) => Doc, d: List[Doc]): Doc = d match {
    case immutable.Nil => empty
    case x :: immutable.Nil => x
    case x :: xs => f(x, fold(f, xs))
  }

  /**
    * Inserts the separator between elements of `d`.
    */
  def sep(sep: Doc, d: List[Doc]): Doc =
    fold(_ :: sep :: _, d)

}


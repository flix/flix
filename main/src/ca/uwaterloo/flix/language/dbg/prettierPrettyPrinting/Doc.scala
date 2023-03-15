package ca.uwaterloo.flix.language.dbg.prettierPrettyPrinting

import scala.annotation.tailrec
import scala.collection.{immutable, mutable}

sealed trait Doc

object Doc {

  implicit class DocOps(d: Doc) {
    def <>(d: Doc): Doc = Doc.<>(this.d, d)

    def <+>(d: Doc): Doc = Doc.<+>(this.d, d)

    def <+\>(d: Doc): Doc = Doc.<+\>(this.d, d)

    def <\>(d: Doc): Doc = Doc.<\>(this.d, d)

    def <+\?>(d: Doc): Doc = Doc.<+\?>(this.d, d)

    def <\?>(d: Doc): Doc = Doc.<\?>(this.d, d)

    def <\?>>(d: Doc)(implicit i: Indent): Doc = Doc.<\?>>(this.d, d)

    def <+\?>>(d: Doc)(implicit i: Indent): Doc = Doc.<+\?>>(this.d, d)

    def <+\>>(d: Doc)(implicit i: Indent): Doc = Doc.<+\>>(this.d, d)
  }

  private case object Nil extends Doc

  private case class Cons(d1: Doc, d2: Doc) extends Doc

  private case class Text(s: String) extends Doc

  private case class Nest(i: Int, d: Doc) extends Doc

  private case class Break(s: String) extends Doc

  private case class Group(d: Doc) extends Doc

  // fancy constructors

  private case class Let(d1: Doc, d2: Doc) extends Doc {
    def collect(acc: List[Doc]): List[Doc] = {
      d2 match {
        case l: Let => l.collect(d1 :: acc)
        case other => (other :: d1 :: acc).reverse
      }
    }
  }

  sealed trait Indent

  private case class Indentation(i: Int) extends Indent

  private def indentI(i: Indent): Int = i match {
    case Indentation(i) => i
  }

  def indentationLevel(i: Int): Indent = Indentation(i)

  def deconstruct(d: Doc): Doc = d match {
    case Nil => Nil
    case Cons(d1, d2) => Cons(deconstruct(d1), deconstruct(d2))
    case Text(s) => Text(s)
    case Nest(i, d) => Nest(i, deconstruct(d))
    case Break(s) => Break(s)
    case Group(d) => Group(deconstruct(d))
    // fancy
    case l: Let => DocUtil.groupVSep(";", l.collect(immutable.Nil))
  }

  def concat(d1: Doc, d2: Doc): Doc = Cons(d1, d2)

  def <>(d1: Doc, d2: Doc): Doc = concat(d1, d2)

  def empty: Doc = Nil

  def text(s: String): Doc = Text(s)

  def nest(x: Doc)(implicit i: Indent): Doc = Nest(indentI(i), x)

  def break: Doc = Break(" ")

  def breakWith(s: String): Doc = Break(s)

  def group(d: Doc): Doc = Group(d)

  def let(d1: Doc, d2: Doc): Doc = Let(d1, d2)

  // SDoc

  private sealed trait SDoc

  private case object SNil extends SDoc

  private case class SText(s: String, d: SDoc) extends SDoc

  private case class SLine(i: Int, d: SDoc) extends SDoc


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

  private sealed trait Mode

  private case object MFlat extends Mode

  private case object MBreak extends Mode

  @tailrec
  private def fits(w: Int, l: List[(Int, Mode, Doc)]): Boolean = l match {
    case _ if w < 0 => false
    case immutable.Nil => true
    case (i, m, Nil) :: z => fits(w, z)
    case (i, m, Cons(x, y)) :: z =>
      fits(w, (i, m, x) :: (i, m, y) :: z)
    case (i, m, Nest(j, x)) :: z => fits(w, (i + j, m, x) :: z)
    case (i, m, Text(s)) :: z => fits(w - s.length, z)
    case (i, MFlat, Break(s)) :: z => fits(w - s.length, z)
    case (i, MBreak, Break(_)) :: z => true // impossible
    case (i, m, Group(x)) :: z => fits(w, (i, MFlat, x) :: z)
    case (_, _, _: Let) :: _ => ???
  }

  @tailrec
  private def format(w: Int, k: Int, l: List[(Int, Mode, Doc)], cont: SDoc => SDoc): SDoc = l match {
    case immutable.Nil =>
      cont(SNil)
    case (i, m, Nil) :: z =>
      format(w, k, z, cont)
    case (i, m, Cons(x, y)) :: z =>
      format(w, k, (i, m, x) :: (i, m, y) :: z, cont)
    case (i, m, Nest(j, x)) :: z =>
      format(w, k, (i + j, m, x) :: z, cont)
    case (i, m, Text(s)) :: z =>
      format(w, k + s.length, z, v1 => cont(SText(s, v1)))
    case (i, MFlat, Break(s)) :: z =>
      format(w, k + s.length, z, v1 => cont(SText(s, v1)))
    case (i, MBreak, Break(s)) :: z =>
      format(w, i, z, v1 => cont(SLine(i, v1)))
    case (i, m, Group(x)) :: z =>
      if (fits(w - k, (i, MFlat, x) :: z)) {
        format(w, k, (i, MFlat, x) :: z, cont)
      } else {
        format(w, k, (i, MBreak, x) :: z, cont)
      }
    case (_, _, _: Let) :: _ => ???
  }

  def pretty(w: Int, d: Doc): String = sdocToString(format(w, 0, List((0, MBreak, d)), x => x))

  // aux

  def <+>(d1: Doc, d2: Doc): Doc = d1 <> text(" ") <> d2

  def <+\>(d1: Doc, d2: Doc): Doc = d1 <> breakWith(" ") <> d2

  def <\>(d1: Doc, d2: Doc): Doc = d1 <> breakWith("") <> d2

  def <+\?>(d1: Doc, d2: Doc): Doc = d1 <> group(breakWith(" ")) <> d2

  def <\?>(d1: Doc, d2: Doc): Doc = d1 <> group(breakWith("")) <> d2

  def <\?>>(d1: Doc, d2: Doc)(implicit i: Indent): Doc = d1 <> group(nest(breakWith("") <> d2))

  def <+\?>>(d1: Doc, d2: Doc)(implicit i: Indent): Doc = d1 <> group(nest(breakWith(" ") <> d2))

  def <+\>>(d1: Doc, d2: Doc)(implicit i: Indent): Doc = d1 <> nest(breakWith(" ") <> d2)

}


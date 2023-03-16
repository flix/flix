package ca.uwaterloo.flix.language.dbg.prettierPrettyPrinting

import ca.uwaterloo.flix.language.ast.Symbol

sealed trait DocAst

object DocAst {

  /** A [[DocAst]] atom that doesn't need parenthesis */
  sealed trait Atom extends DocAst

  /** `<sym>` */
  case class Var(sym: Symbol.VarSym) extends Atom

  /** inserted string printed as-is (assumed not to require parenthesis) */
  case class AsIs(s: String) extends Atom

  /** inserted string printed as-is, enclosed with special meta symbols */
  case class Meta(s: String) extends Atom

  case object RecordEmpty extends Atom

  case class HoleError(sym: Symbol.HoleSym) extends Atom

  /** `<sym>%<offset>` */
  case class VarWithOffset(sym: Symbol.VarSym) extends Atom

  /** `(<op><d>)` */
  case class Unary(op: String, d: DocAst) extends DocAst

  /** `(<d1> <op> <d2>)` */
  case class Binary(d1: DocAst, op: String, d2: DocAst) extends DocAst

  /** `if (<cond>) <thn> else <els>` */
  case class IfThenElse(cond: DocAst, thn: DocAst, els: DocAst) extends DocAst

  /**
    * `let <v>: <tpe> = <bind>; <body>`
    *
    * or
    *
    * `let <v> = <bind>; <body>`
    */
  case class Let(v: Symbol.VarSym, tpe: Option[DocAst], bind: DocAst, body: DocAst) extends DocAst

  /** `<v>: <tpe>` */
  case class Ascription(v: DocAst, tpe: DocAst) extends DocAst

  // constants

  val Region: DocAst = Meta("region")

  val MatchError: DocAst = Meta("match error")

}



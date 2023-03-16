package ca.uwaterloo.flix.language.dbg.prettierPrettyPrinting

import ca.uwaterloo.flix.language.ast.{Ast, Symbol}

import java.lang.reflect.{Constructor, Field, Method}

sealed trait DocAst

object DocAst {

  /** A [[DocAst]] atom that doesn't need parenthesis */
  sealed trait Atom extends DocAst

  /** A [[DocAst]] that sometimes need parenthesis */
  sealed trait Composite extends DocAst

  sealed trait LetBinder extends Atom

  /** inserted string printed as-is (assumed not to require parenthesis) */
  case class AsIs(s: String) extends Atom

  /** inserted string printed as-is, enclosed with special meta symbols */
  case class Meta(s: String) extends Atom

  case object RecordEmpty extends Atom

  /** `(<word> <d>)` */
  case class Keyword(word: String, d: DocAst) extends Composite

  /** `(<op><d>)` */
  case class Unary(op: String, d: DocAst) extends Composite

  /** `(<d1> <op> <d2>)` */
  case class Binary(d1: DocAst, op: String, d2: DocAst) extends Composite

  /** `if (<cond>) <thn> else <els>` */
  case class IfThenElse(cond: DocAst, thn: DocAst, els: DocAst) extends Composite

  /** `<d1>.<d2>` */
  case class Dot(d1: DocAst, d2: DocAst) extends Atom

  /** `<d1>.,<d2>` */
  case class DoubleDot(d1: DocAst, d2: DocAst) extends Atom

  /**
    * `let <v>: <tpe> = <bind>; <body>`
    *
    * or
    *
    * `let <v> = <bind>; <body>`
    */
  case class Let(v: DocAst, tpe: Option[DocAst], bind: DocAst, body: DocAst) extends LetBinder

  case class LetRec(v: DocAst, tpe: Option[DocAst], bind: DocAst, body: DocAst) extends LetBinder

  case class Scope(v: DocAst, d: DocAst) extends Atom

  case class App(f: DocAst, args: List[DocAst]) extends Atom

  /** `<v>: <tpe>` */
  case class Ascription(v: DocAst, tpe: DocAst) extends Composite

  case class Cast(d: DocAst, tpe: DocAst) extends Composite

  case class ArrayLit(ds: List[DocAst]) extends Atom

  // constants
  /** `<sym>` */
  def Var(sym: Symbol.VarSym): DocAst =
    AsIs(sym.toString)

  /** `<sym>_<offset>` */
  def VarWithOffset(sym: Symbol.VarSym): DocAst =
    AsIs(sym.toString + "_" + sym.getStackOffset.toString)

  /** `?<sym>` */
  def HoleError(sym: Symbol.HoleSym): DocAst =
    AsIs(sym.toString)

  /** `region` */
  val Region: DocAst = Meta("region")

  /** `Meta(match error)` */
  val MatchError: DocAst = Meta("match error")

  /** `<sym>(<arg0>, <arg1>, ..., <argn>)` */
  def Tag(sym: Symbol.CaseSym, args: List[DocAst]): DocAst = {
    val tag = AsIs(sym.toString)
    if (args.isEmpty) tag else App(tag, args)
  }

  def Ref(d: DocAst): DocAst =
    Keyword("ref", d)

  def Deref(d: DocAst): DocAst =
    Keyword("deref", d)

  def Lazy(d: DocAst): DocAst =
    Keyword("lazy", d)

  def Force(d: DocAst): DocAst =
    Keyword("force", d)

  def Box(d: DocAst): DocAst =
    Keyword("box", d)

  def Unbox(d: DocAst): DocAst =
    Keyword("unbox", d)

  def Tuple(ds: List[DocAst]): DocAst =
    App(AsIs(""), ds)

  def ClosureLifted(sym: Symbol.DefnSym, ds: List[DocAst]): DocAst = {
    val defName = AsIs(sym.toString)
    if (ds.isEmpty) defName else App(defName, ds)
  }

  def Cst(cst: Ast.Constant): DocAst =
    AsIs(Printers.ConstantPrinter.print(cst))

  def AppClo(d: DocAst, ds: List[DocAst]): DocAst =
    App(d, ds)

  def AppCloTail(d: DocAst, ds: List[DocAst]): DocAst =
    App(d, ds)

  def AppDefTail(sym: Symbol.DefnSym, ds: List[DocAst]): DocAst =
    App(AsIs(sym.toString), ds)

  def AppSelfTail(sym: Symbol.DefnSym, ds: List[DocAst]): DocAst =
    App(AsIs(sym.toString), ds)

  def App(sym: Symbol.DefnSym, ds: List[DocAst]): DocAst =
    App(AsIs(sym.toString), ds)

  def JavaInvokeMethod(m: Method, d: DocAst, ds: List[DocAst]): DocAst =
    App(DoubleDot(d, AsIs(m.getName)), ds)

  def JavaInvokeStaticMethod(m: Method, ds: List[DocAst]): DocAst = {
    val className = "##" + m.getDeclaringClass.getName
    App(Dot(AsIs(className), AsIs(m.getName)), ds)
  }

  def JavaGetStaticField(f: Field): DocAst = {
    val className = "##" + f.getDeclaringClass.getName
    Dot(AsIs(className), AsIs(f.getName))
  }

  def JavaInvokeConstructor(c: Constructor[_], ds: List[DocAst]): DocAst = {
    val className = "##" + c.getDeclaringClass.getName
    App(AsIs(className), ds)
  }

  def JavaGetField(f: Field, d: DocAst): DocAst =
    DoubleDot(d, AsIs(f.getName))

}



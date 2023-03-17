package ca.uwaterloo.flix.language.dbg.prettierPrettyPrinting

import ca.uwaterloo.flix.language.ast.{Ast, Name, Symbol}

import java.lang.reflect.{Constructor, Field, Method}

sealed trait DocAst

object DocAst {

  case class Def(ann: Ast.Annotations, mod: Ast.Modifiers, sym: Symbol.DefnSym, parameters: List[Ascription], resType: Type, body: DocAst)

  case class Program(defs: List[Def])

  // expressions

  /** A [[DocAst]] atom that doesn't need parenthesis */
  sealed trait Atom extends DocAst

  /** A [[DocAst]] that sometimes need parenthesis */
  sealed trait Composite extends DocAst

  sealed trait LetBinder extends Atom

  case class InRegion(d1: DocAst, d2: DocAst) extends Composite

  case object Unit extends Atom

  case class Tuple(elms: List[DocAst]) extends Atom

  case class Tag(sym: Symbol.CaseSym, args: List[DocAst]) extends Atom

  /** inserted string printed as-is (assumed not to require parenthesis) */
  case class AsIs(s: String) extends Atom

  /** inserted string printed as-is, enclosed with special meta symbols */
  case class Meta(s: String) extends Atom

  case object RecordEmpty extends Atom

  case class RecordExtend(field: Name.Field, value: DocAst, rest: DocAst) extends Atom

  /** `(<word> <d>)` */
  case class Keyword(word: String, d: DocAst) extends Composite

  /** `(<op><d>)` */
  case class Unary(op: String, d: DocAst) extends Composite

  /** `(<d1> <op> <d2>)` */
  case class Binary(d1: DocAst, op: String, d2: DocAst) extends Composite

  /** `if (<cond>) <thn> else <els>` */
  case class IfThenElse(cond: DocAst, thn: DocAst, els: DocAst) extends Composite

  case class Branch(d: DocAst, branches: Map[Symbol.LabelSym, DocAst]) extends Atom

  /** `<d1>.<d2>` */
  case class Dot(d1: DocAst, d2: DocAst) extends Atom

  /** `<d1>.,<d2>` */
  case class DoubleDot(d1: DocAst, d2: DocAst) extends Atom

  case class TryCatch(d: DocAst, rules: List[(Symbol.VarSym, Class[_], DocAst)]) extends Atom

  /**
    * `let <v>: <tpe> = <bind>; <body>`
    *
    * or
    *
    * `let <v> = <bind>; <body>`
    */
  case class Let(v: DocAst, tpe: Option[Type], bind: DocAst, body: DocAst) extends LetBinder

  case class LetRec(v: DocAst, tpe: Option[Type], bind: DocAst, body: DocAst) extends LetBinder

  case class Scope(v: DocAst, d: DocAst) extends Atom

  case class App(f: DocAst, args: List[DocAst]) extends Atom

  case class SquareApp(f: DocAst, args: List[DocAst]) extends Atom

  case class Assign(d1: DocAst, d2: DocAst) extends Composite

  /** `<v>: <tpe>` */
  case class Ascription(v: DocAst, tpe: Type) extends Composite

  case class Cast(d: DocAst, tpe: Type) extends Composite

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
  val Region: DocAst =
    Meta("region")

  /** `Meta(match error)` */
  val MatchError: DocAst =
    AsIs("?matchError")

  def Untag(sym: Symbol.CaseSym, d: DocAst): DocAst =
    Keyword("untag", d)

  def IsTag(sym: Symbol.CaseSym, d: DocAst): DocAst =
    Binary(d, "is", AsIs(sym.toString))

  def Ref(d: DocAst): DocAst =
    Keyword("ref", d)

  def Deref(d: DocAst): DocAst =
    Keyword("deref", d)

  def ArrayNew(d1: DocAst, d2: DocAst): DocAst =
    SquareApp(AsIs(""), List(Binary(d1, ";", d2)))

  def ArrayLit(ds: List[DocAst]): DocAst =
    SquareApp(AsIs(""), ds)

  def ArrayLength(d: DocAst): DocAst =
    DoubleDot(d, AsIs("length"))

  def ArrayLoad(d1: DocAst, d2: DocAst): DocAst =
    SquareApp(d1, List(d2))

  def ArrayStore(d1: DocAst, d2: DocAst, d3: DocAst): DocAst =
    Assign(SquareApp(d1, List(d2)), d3)

  def Lazy(d: DocAst): DocAst =
    Keyword("lazy", d)

  def Force(d: DocAst): DocAst =
    Keyword("force", d)

  def Box(d: DocAst): DocAst =
    Keyword("box", d)

  def Unbox(d: DocAst): DocAst =
    Keyword("unbox", d)

  def Index(idx: Int, d: DocAst): DocAst =
    Dot(d, AsIs(s"_$idx"))

  def ClosureLifted(sym: Symbol.DefnSym, ds: List[DocAst]): DocAst = {
    val defName = AsIs(sym.toString)
    if (ds.isEmpty) defName else App(defName, ds)
  }

  def Spawn(d1: DocAst, d2: DocAst): DocAst =
    InRegion(Keyword("spawn", d1), d2)

  def Cst(cst: Ast.Constant): DocAst =
    Printers.ConstantPrinter.print(cst)

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

  def JavaPutField(f: Field, d1: DocAst, d2: DocAst): DocAst =
    Assign(DoubleDot(d1, AsIs(f.getName)), d2)

  def JavaPutStaticField(f: Field, d: DocAst): DocAst =
    Assign(Dot(AsIs("##" + f.getDeclaringClass), AsIs(f.getName)), d)

  def JumpTo(sym: Symbol.LabelSym): DocAst =
    Keyword("goto", AsIs(sym.toString))

  def RecordSelect(field: Name.Field, d: DocAst): DocAst =
    Dot(d, AsIs(field.name))

  sealed trait Type

  object Type {

    sealed trait Atom extends Type

    sealed trait Composite extends Type

    case class AsIs(s: String) extends Atom

    case class App(obj: String, args: List[Type]) extends Atom

    case class Tuple(elms: List[Type]) extends Atom

    case class Arrow(args: List[Type], res: Type) extends Composite

    case object RecordEmpty extends Atom

    case class RecordExtend(field: String, value: Type, rest: Type) extends Atom

    case object SchemaEmpty extends Atom

    case class SchemaExtend(name: String, tpe: Type, rest: Type) extends Atom

    val Unit: Type = AsIs("Unit")

    val Bool: Type = AsIs("Bool")

    val Char: Type = AsIs("Char")

    val Float32: Type = AsIs("Float32")

    val Float64: Type = AsIs("Float64")

    val BigDecimal: Type = AsIs("BigDecimal")

    val Int8: Type = AsIs("Int8")

    val Int16: Type = AsIs("Int16")

    val Int32: Type = AsIs("Int32")

    val Int64: Type = AsIs("Int64")

    val BigInt: Type = AsIs("BigInt")

    val Str: Type = AsIs("String")

    val Region: Type = AsIs("Region")

    def Array(t: Type): Type = App("Array", List(t))

    def Lazy(t: Type): Type = App("Lazy", List(t))

    def Ref(t: Type): Type = App("Ref", List(t))

    def Enum(sym: Symbol.EnumSym, args: List[Type]): Type = App(sym.toString, args)

    def Native(clazz: Class[_]): Type = AsIs("##" + clazz.getName)

    def Var(id: Int): Type = AsIs(s"var$id")
  }

}



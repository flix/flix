/*
 * Copyright 2023 Jonathan Lindegaard Starup
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package ca.uwaterloo.flix.language.dbg

import ca.uwaterloo.flix.language.ast.shared.*
import ca.uwaterloo.flix.language.ast.{Name, Symbol}

import java.lang.reflect.{Constructor, Field, Method}
import scala.collection.immutable.SortedSet

sealed trait DocAst

object DocAst {

  case class Def(ann: Annotations, mod: Modifiers, sym: Symbol.DefnSym, parameters: List[Exp.AscriptionTpe], resType: Type, effect: Type, body: Exp)

  case class Enum(ann: Annotations, mod: Modifiers, sym: Symbol.EnumSym, tparams: List[TypeParam], cases: List[Case])

  case class Case(sym: Symbol.CaseSym, tpes: List[Type])

  case class TypeParam(sym: Symbol.KindedTypeVarSym)

  /** `misc` is used for printing non-structured asts like [[ca.uwaterloo.flix.language.ast.SyntaxTree]] */
  case class Program(enums: List[Enum], defs: List[Def], misc: List[(String, Exp)])

  case class JvmMethod(ident: Name.Ident, fparams: List[Exp.AscriptionTpe], clo: Exp, tpe: Type)


  sealed trait Exp

  object Exp {

    /** A [[Exp]] atom that doesn't need parenthesis */
    sealed trait Atom extends Exp

    /** A [[Exp]] that sometimes needs parenthesis */
    sealed trait Composite extends Exp

    sealed trait LetBinder extends Atom

    sealed trait RecordOp extends Atom

    case object Unit extends Atom

    case class Tuple(elms: List[Exp]) extends Atom

    case class Tag(sym: Sym, args: List[Exp]) extends Atom

    /** inserted string printed as-is (assumed not to require parenthesis) */
    case class AsIs(s: String) extends Atom

    /** inserted string printed as-is, enclosed with special meta symbols */
    case class Meta(s: String) extends Atom

    case object RecordEmpty extends Atom

    case class RecordExtend(label: Name.Label, value: Exp, rest: Exp) extends RecordOp

    case class RecordRestrict(label: Name.Label, value: Exp) extends RecordOp

    case class Keyword(word: String, d: Exp) extends Composite

    case class DoubleKeyword(word1: String, d1: Exp, word2: String, d2: Either[Exp, Type]) extends Composite

    case class DoubleKeywordPost(d1: Exp, word1: String, d2: Type, word3: String, d3: Type) extends Composite

    case class InfixKeyword(d1: Exp, word: String, d2: Type) extends Composite

    case class Unary(op: String, d: Exp) extends Composite

    /** e.g. `arr?` */
    case class UnaryRightAfter(d: Exp, op: String) extends Atom

    case class Binary(d1: Exp, op: String, d2: Exp) extends Composite

    case class IfThenElse(cond: Exp, thn: Exp, els: Exp) extends Composite

    case class Branch(d: Exp, branches: Map[Symbol.LabelSym, Exp]) extends Atom

    case class Match(d: Exp, branches: List[(Exp, Option[Exp], Exp)]) extends Atom

    case class ExtMatch(d: Exp, branches: List[(Exp, Exp)]) extends Atom

    case class TypeMatch(d: Exp, branches: List[(Exp, Type, Exp)]) extends Atom

    /** e.g. `r.x` */
    case class Dot(d1: Exp, d2: Exp) extends Atom

    /** e.g. `r..toString()`. It is used for java "dots" */
    case class DoubleDot(d1: Exp, d2: Exp) extends Atom

    /**
      * e.g. `r#x` for [[RecordSelect]].
      */
    case class Hash(d1: Exp, d2: Exp) extends Atom

    case class TryCatch(d: Exp, rules: List[(Symbol.VarSym, Class[?], Exp)]) extends Atom

    case class Handler(eff: Symbol.EffSym, rules: List[(Symbol.OpSym, List[AscriptionTpe], Exp)]) extends Composite

    case class Stm(d1: Exp, d2: Exp) extends LetBinder

    case class Let(v: Exp, tpe: Option[Type], bind: Exp, body: Exp) extends LetBinder

    case class LocalDef(sym: Exp, parameters: List[Exp.AscriptionTpe], resType: Option[Type], effect: Option[Type], body: Exp, next: Exp) extends LetBinder

    case class Region(v: Exp, d: Exp) extends Atom

    case class AppWithTail(f: Exp, args: List[Exp], ct: Option[ExpPosition]) extends Atom

    case class SquareApp(f: Exp, args: List[Exp]) extends Atom

    case class DoubleSquareApp(f: Exp, args: List[Exp]) extends Atom

    case class Assign(d1: Exp, d2: Exp) extends Composite

    case class AscriptionTpe(v: Exp, tpe: Type) extends Composite

    case class AscriptionEff(v: Exp, tpe: Option[Type], eff: Option[Type]) extends Composite

    case class Unsafe(d: Exp, tpe: Type) extends Composite

    case class NewObject(name: String, clazz: Class[?], tpe: Type, methods: List[JvmMethod]) extends Composite

    case class Lambda(fparams: List[Exp.AscriptionTpe], body: Exp) extends Composite

    case class Native(clazz: Class[?]) extends Atom

    val Unknown: Exp =
      Meta("unknown exp")

    def Var(sym: Symbol.VarSym): Exp =
      AsIs(sym.toString)

    val Wild: Exp =
      AsIs("_")

    def Hole(sym: Symbol.HoleSym): Exp =
      AsIs("?" + sym.toString)

    def HoleWithExp(exp: Exp): Exp =
      UnaryRightAfter(exp, "?")

    def HoleError(sym: Symbol.HoleSym): Exp =
      AsIs(sym.toString)

    val MatchError: Exp =
      AsIs("?matchError")

    val CastError: Exp =
      AsIs("?castError")

    /** represents the error ast node when compiling partial programs */
    val Error: Exp =
      AsIs("?astError")

    def Tag(sym: Symbol.CaseSym, exprs: List[Exp]): Exp =
      Tag(Sym(sym), exprs)

    def ExtTag(label: Name.Label, exprs: List[Exp]): Exp =
      Keyword("xvar", Tag(Sym(label), exprs))

    def Untag(d: Exp, idx: Int): Exp =
      Keyword("untag_" + idx, d)

    def Is(sym: Symbol.CaseSym, d: Exp): Exp =
      Binary(d, "is", AsIs(sym.toString))

    def Discard(d: Exp): Exp =
      Keyword("discard", d)

    def Def(sym: Symbol.DefnSym): Exp =
      AsIs(sym.toString)

    def Sig(sym: Symbol.SigSym): Exp =
      AsIs(sym.toString)

    /** e.g. `something @ rc` */
    def InRegion(d1: Exp, d2: Exp): Exp =
      Binary(d1, "@", d2)

    def ArrayNew(d1: Exp, d2: Exp): Exp =
      SquareApp(AsIs(""), List(Binary(d1, ";", d2)))

    def ArrayLit(ds: List[Exp]): Exp =
      SquareApp(AsIs(""), ds)

    def ArrayLength(d: Exp): Exp =
      DoubleDot(d, AsIs("length"))

    def ArrayLoad(d1: Exp, index: Exp): Exp =
      SquareApp(d1, List(index))

    def ArrayStore(d1: Exp, index: Exp, d2: Exp): Exp =
      Assign(SquareApp(d1, List(index)), d2)

    def StructNew(sym: Symbol.StructSym, exps: List[(Symbol.StructFieldSym, Exp)], d2: Exp): Exp = {
      val beforeRecord = "new " + sym.toString
      val name = Name.Label(sym.name, sym.loc)
      val record = exps.foldRight(RecordEmpty: Exp) { case (cur, acc) => RecordExtend(name, cur._2, acc) }
      DoubleKeyword(beforeRecord, record, "@", Left(d2))
    }

    def StructGet(d1: Exp, field: Symbol.StructFieldSym): Exp =
      Dot(d1, AsIs(field.name))

    def StructPut(d1: Exp, field: Symbol.StructFieldSym, d2: Exp): Exp =
      Assign(Dot(d1, AsIs(field.name)), d2)

    def VectorLit(ds: List[Exp]): Exp =
      DoubleSquareApp(AsIs(""), ds)

    def VectorLoad(d1: Exp, index: Exp): Exp =
      DoubleSquareApp(d1, List(index))

    def VectorLength(d: Exp): Exp =
      DoubleDot(d, AsIs("length"))

    def Lazy(d: Exp): Exp =
      Keyword("lazy", d)

    def Force(d: Exp): Exp =
      Keyword("force", d)

    def Throw(d: Exp): Exp =
      Keyword("throw", d)

    def Index(idx: Int, d: Exp): Exp =
      Dot(d, AsIs(s"_$idx"))

    def InstanceOf(d: Exp, clazz: Class[?]): Exp =
      Binary(d, "instanceof", Native(clazz))

    def ClosureLifted(sym: Symbol.DefnSym, ds: List[Exp]): Exp = {
      val defName = AsIs(sym.toString)
      if (ds.isEmpty) defName else App(defName, ds)
    }

    def RunWith(d1: Exp, d2: Exp): Exp =
      DoubleKeyword("run", d1, "with", Left(d2))

    def RunWithHandler(d: Exp, eff: Symbol.EffSym, rules: List[(Symbol.OpSym, List[AscriptionTpe], Exp)]): Exp =
      RunWith(d, Handler(eff, rules))

    def Spawn(d1: Exp, d2: Exp): Exp =
      InRegion(Keyword("spawn", d1), d2)

    def Cast(d: Exp, tpe: Type): Exp =
      DoubleKeyword("cast", d, "as", Right(tpe))

    def UncheckedCast(d: Exp, tpe0: Option[Type], eff0: Option[Type]): Exp = (tpe0, eff0) match {
      case (Some(tpe), Some(eff)) => App(AsIs("unchecked_cast"), List(DoubleKeywordPost(d, "as", tpe, "\\", eff)))
      case (Some(tpe), None) => App(AsIs("unchecked_cast"), List(InfixKeyword(d, "as", tpe)))
      case (None, Some(eff)) => App(AsIs("unchecked_cast"), List(DoubleKeywordPost(d, "as", Type.Wild, "\\", eff)))
      case (None, None) => d
    }

    def CheckedCast(cast: CheckedCastType, d: Exp): Exp = cast match {
      case CheckedCastType.TypeCast => Keyword("checked_cast", d)
      case CheckedCastType.EffectCast => Keyword("checked_ecast", d)
    }

    def Unbox(d: Exp, tpe: Type): Exp =
      DoubleKeyword("unbox", d, "as", Right(tpe))

    def Box(d: Exp): Exp =
      Keyword("box", d)

    def Without(d: Exp, sym: Symbol.EffSym): Exp =
      Binary(d, "without", AsIs(sym.toString))

    def Cst(cst: Constant): Exp =
      printer.ConstantPrinter.print(cst)

    def App(f: Exp, args: List[Exp]): Exp =
      AppWithTail(f, args, None)

    def ApplyClo(d: Exp, ds: List[Exp]): Exp =
      App(d, ds)

    def ApplyCloWithTail(d: Exp, ds: List[Exp], ct: ExpPosition): Exp =
      AppWithTail(d, ds, Some(ct))

    def ApplySelfTail(sym: Symbol.DefnSym, ds: List[Exp]): Exp =
      AppWithTail(AsIs(sym.toString), ds, Some(ExpPosition.Tail))

    def ApplyDef(sym: Symbol.DefnSym, ds: List[Exp]): Exp =
      App(AsIs(sym.toString), ds)

    def ApplyLocalDef(sym: Symbol.VarSym, ds: List[Exp]): Exp =
      App(AsIs(sym.toString), ds)

    def ApplyDefWithTail(sym: Symbol.DefnSym, ds: List[Exp], ct: ExpPosition): Exp =
      AppWithTail(AsIs(sym.toString), ds, Some(ct))

    def ApplyOp(sym: Symbol.OpSym, ds: List[Exp]): Exp =
      Keyword("do", App(AsIs(sym.toString), ds))

    def JavaInvokeMethod(d: Exp, methodName: Name.Ident, ds: List[Exp]): Exp =
      App(DoubleDot(d, AsIs(methodName.name)), ds)

    def JavaInvokeMethod(m: Method, d: Exp, ds: List[Exp]): Exp =
      App(DoubleDot(d, AsIs(m.getName)), ds)

    def JavaInvokeStaticMethod(m: Method, ds: List[Exp]): Exp = {
      App(Dot(Native(m.getDeclaringClass), AsIs(m.getName)), ds)
    }

    def JavaGetStaticField(f: Field): Exp = {
      Dot(Native(f.getDeclaringClass), AsIs(f.getName))
    }

    def JavaInvokeConstructor(c: Constructor[?], ds: List[Exp]): Exp = {
      App(Native(c.getDeclaringClass), ds)
    }

    def JavaGetField(f: Field, d: Exp): Exp =
      DoubleDot(d, AsIs(f.getName))

    def JavaPutField(f: Field, d1: Exp, d2: Exp): Exp =
      Assign(DoubleDot(d1, AsIs(f.getName)), d2)

    def JavaPutStaticField(f: Field, d: Exp): Exp =
      Assign(Dot(Native(f.getDeclaringClass), AsIs(f.getName)), d)

    def JumpTo(sym: Symbol.LabelSym): Exp =
      Keyword("goto", AsIs(sym.toString))

    def RecordSelect(label: Name.Label, d: Exp): Exp =
      Hash(d, AsIs(label.name))

    def Regex(p: java.util.regex.Pattern): Exp =
      App(AsIs("Regex"), List(AsIs(s""""${p.toString}"""")))

    val Absent: Exp =
      AsIs("Absent")

  }

  sealed trait Type

  object Type {

    sealed trait Atom extends Type

    sealed trait Composite extends Type

    case object Unit extends Atom

    case class AsIs(s: String) extends Atom

    case class Ascribe(tpe: Type, kind: Type) extends Composite

    case class App(obj: Type, args: List[Type]) extends Atom

    case class Tuple(elms: List[Type]) extends Atom

    case class ArrowEff(args: List[Type], res: Type, eff: Type) extends Composite

    case object RecordRowEmpty extends Atom

    case class RecordRowExtend(label: String, value: Type, rest: Type) extends Atom

    case class RecordOf(tpe: Type) extends Atom

    case object RecordEmpty extends Atom

    case class RecordExtend(label: String, value: Type, rest: Type) extends Atom

    case object ExtensibleEmpty extends Atom

    case class ExtensibleExtend(cons: String, tpes: List[Type], rest: Type) extends Atom

    case object SchemaRowEmpty extends Atom

    case class SchemaRowExtend(label: String, tpe: Type, rest: Type) extends Atom

    case class SchemaOf(tpe: Type) extends Atom

    case object SchemaEmpty extends Atom

    case class SchemaExtend(name: String, tpe: Type, rest: Type) extends Atom

    case class Native(clazz: Class[?]) extends Atom

    case class JvmConstructor(constructor: Constructor[?]) extends Atom

    case class JvmMethod(method: Method) extends Atom

    case class JvmField(field: Field) extends Atom


    case class Not(tpe: Type) extends Composite

    case class And(tpe1: Type, tpe2: Type) extends Composite

    case class Or(tpe1: Type, tpe2: Type) extends Composite

    case class Complement(tpe: Type) extends Composite

    case class Union(tpe1: Type, tpe2: Type) extends Composite

    case class Intersection(tpe1: Type, tpe2: Type) extends Composite

    case class Difference(tpe1: Type, tpe2: Type) extends Composite

    case class SymmetricDiff(tpe1: Type, tpe2: Type) extends Composite

    case class CaseSet(syms: SortedSet[Symbol.RestrictableCaseSym]) extends Atom

    case class CaseComplement(tpe: Type) extends Composite

    case class CaseUnion(tpe1: Type, tpe2: Type) extends Composite

    case class CaseIntersection(tpe1: Type, tpe2: Type) extends Composite

    case object Pure extends Atom

    /** Represents the union of IO and all regions. */
    case object Impure extends Atom

    /** Represents Impure and all algebraic effect. */
    case object ControlImpure extends Atom

    /** Inserted string printed as-is (assumed not to require parenthesis) */
    case class Meta(s: String) extends Atom

    val Void: Type = AsIs("Void")

    val AnyType: Type = AsIs("AnyType")

    val Unknown: Type = Meta("unknown type")

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

    val Regex: Type = AsIs("Regex")

    val Region: Type = AsIs("Region")

    val Null: Type = AsIs("Null")

    val Schema: Type = AsIs("Schema")

    val Error: Type = AsIs("Error")

    val Univ: Type = AsIs("Univ")

    val Wild: Type = AsIs("_")

    def Arrow(args: List[Type], res: Type): Type = ArrowEff(args, res, Type.Pure)

    def Alias(sym: Symbol.TypeAliasSym, args: List[Type]): Type = App(AsIs(sym.toString), args)

    def AssocType(sym: Symbol.AssocTypeSym, arg: Type): Type = App(AsIs(sym.toString), List(arg))

    def Array(t: Type): Type = App(AsIs("Array"), List(t))

    def Lazy(t: Type): Type = App(AsIs("Lazy"), List(t))

    def Enum(sym: Symbol.EnumSym, args: List[Type]): Type = App(AsIs(sym.toString), args)

    def Struct(sym: Symbol.StructSym, args: List[Type]): Type = App(AsIs(sym.toString), args)

    def Var(sym: Symbol.KindedTypeVarSym): Type = AsIs(sym.toString)

    def Var(sym: Symbol.UnkindedTypeVarSym): Type = AsIs(sym.toString)
  }

  object Pattern {

    def ExtTag(label: Name.Label, exprs: List[Exp]): Exp =
      Exp.Tag(Sym(label), exprs)

    def Default: Exp =
      Exp.Wild

  }

  case class Sym(private val symbol: String) {
    override def toString: String = symbol
  }

  def Sym(label: Name.Label): Sym = Sym(label.name)

  def Sym(sym: Symbol.CaseSym): Sym = Sym(sym.toString)
}



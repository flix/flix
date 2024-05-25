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

import ca.uwaterloo.flix.language.ast.{Ast, Name, Symbol}

import java.lang.reflect.{Constructor, Field, Method}

sealed trait DocAst

object DocAst {

  case class Def(ann: Ast.Annotations, mod: Ast.Modifiers, sym: Symbol.DefnSym, parameters: List[Expression.Ascription], resType: Type, effect: Eff, body: Expression)

  case class Enum(ann: Ast.Annotations, mod: Ast.Modifiers, sym: Symbol.EnumSym, tparams: List[TypeParam], cases: List[Case])

  case class Case(sym: Symbol.CaseSym, tpe: Type)

  case class TypeParam(sym: Symbol.KindedTypeVarSym)

  case class Program(enums: List[Enum], defs: List[Def])

  case class JvmMethod(ident: Name.Ident, fparams: List[Expression.Ascription], clo: Expression, tpe: Type)


  sealed trait Expression

  object Expression {

    /** A [[Expression]] atom that doesn't need parenthesis */
    sealed trait Atom extends Expression

    /** A [[Expression]] that sometimes needs parenthesis */
    sealed trait Composite extends Expression

    sealed trait LetBinder extends Atom

    sealed trait RecordOp extends Atom

    case object Unit extends Atom

    case class Tuple(elms: List[Expression]) extends Atom

    case class Tag(sym: Symbol.CaseSym, args: List[Expression]) extends Atom

    /** inserted string printed as-is (assumed not to require parenthesis) */
    case class AsIs(s: String) extends Atom

    /** inserted string printed as-is, enclosed with special meta symbols */
    case class Meta(s: String) extends Atom

    case object RecordEmpty extends Atom

    case class RecordExtend(label: Name.Label, value: Expression, rest: Expression) extends RecordOp

    case class RecordRestrict(label: Name.Label, value: Expression) extends RecordOp

    case class Keyword(word: String, d: Expression) extends Composite

    case class DoubleKeyword(word1: String, d1: Expression, word2: String, d2: Either[Expression, Type]) extends Composite

    case class Unary(op: String, d: Expression) extends Composite

    /** e.g. `arr?` */
    case class UnaryRightAfter(d: Expression, op: String) extends Atom

    case class Binary(d1: Expression, op: String, d2: Expression) extends Composite

    case class IfThenElse(cond: Expression, thn: Expression, els: Expression) extends Composite

    case class Branch(d: Expression, branches: Map[Symbol.LabelSym, Expression]) extends Atom

    case class Match(d: Expression, branches: List[(Expression, Option[Expression], Expression)]) extends Atom

    case class TypeMatch(d: Expression, branches: List[(Expression, Type, Expression)]) extends Atom

    /** e.g. `r.x` */
    case class Dot(d1: Expression, d2: Expression) extends Atom

    /** e.g. `r..toString()`. It is used for java "dots" */
    case class DoubleDot(d1: Expression, d2: Expression) extends Atom

    case class TryCatch(d: Expression, rules: List[(Symbol.VarSym, Class[_], Expression)]) extends Atom

    case class TryWith(d1: Expression, eff: Symbol.EffectSym, rules: List[(Symbol.OpSym, List[Ascription], Expression)]) extends Atom

    case class Stm(d1: Expression, d2: Expression) extends LetBinder

    case class Let(v: Expression, tpe: Option[Type], bind: Expression, body: Expression) extends LetBinder

    case class LetRec(v: Expression, tpe: Option[Type], bind: Expression, body: Expression) extends LetBinder

    case class Scope(v: Expression, d: Expression) extends Atom

    case class App(f: Expression, args: List[Expression]) extends Atom

    case class SquareApp(f: Expression, args: List[Expression]) extends Atom

    case class DoubleSquareApp(f: Expression, args: List[Expression]) extends Atom

    case class Assign(d1: Expression, d2: Expression) extends Composite

    case class Ascription(v: Expression, tpe: Type) extends Composite

    case class NewObject(name: String, clazz: Class[_], tpe: Type, methods: List[JvmMethod]) extends Composite

    case class Lambda(fparams: List[Expression.Ascription], body: Expression) extends Composite

    case class Native(clazz: Class[_]) extends Atom

    val Unknown: Expression =
      Meta("unknown exp")

    def Var(sym: Symbol.VarSym): Expression =
      AsIs(sym.toString)

    val Wild: Expression =
      AsIs("_")

    /** e.g. `x_2` */
    def VarWithOffset(sym: Symbol.VarSym): Expression =
      AsIs(sym.toString + "_" + sym.getStackOffset(0).toString)

    def Hole(sym: Symbol.HoleSym): Expression =
      AsIs("?" + sym.toString)

    def HoleWithExp(exp: Expression): Expression =
      UnaryRightAfter(exp, "?")

    def HoleError(sym: Symbol.HoleSym): Expression =
      AsIs(sym.toString)

    /** the region value */
    val Region: Expression =
      Meta("region")

    val MatchError: Expression =
      AsIs("?matchError")

    /** represents the error ast node when compiling partial programs */
    val Error: Expression =
      AsIs("?astError")

    def Untag(sym: Symbol.CaseSym, d: Expression): Expression =
      Keyword("untag", d)

    def Is(sym: Symbol.CaseSym, d: Expression): Expression =
      Binary(d, "is", AsIs(sym.toString))

    /** The control separated return statement */
    def Ret(d: Expression): Expression =
      Keyword("ret", d)

    def Ref(d: Expression): Expression =
      Keyword("ref", d)

    def Deref(d: Expression): Expression =
      Keyword("deref", d)

    def Discard(d: Expression): Expression =
      Keyword("discard", d)

    def Def(sym: Symbol.DefnSym): Expression =
      AsIs(sym.toString)

    def Sig(sym: Symbol.SigSym): Expression =
      AsIs(sym.toString)

    /** e.g. `something @ rc` */
    def InRegion(d1: Expression, d2: Expression): Expression =
      Binary(d1, "@", d2)

    def ArrayNew(d1: Expression, d2: Expression): Expression =
      SquareApp(AsIs(""), List(Binary(d1, ";", d2)))

    def ArrayLit(ds: List[Expression]): Expression =
      SquareApp(AsIs(""), ds)

    def ArrayLength(d: Expression): Expression =
      DoubleDot(d, AsIs("length"))

    def ArrayLoad(d1: Expression, index: Expression): Expression =
      SquareApp(d1, List(index))

    def ArrayStore(d1: Expression, index: Expression, d2: Expression): Expression =
      Assign(SquareApp(d1, List(index)), d2)

    def VectorLit(ds: List[Expression]): Expression =
      DoubleSquareApp(AsIs(""), ds)

    def VectorLoad(d1: Expression, index: Expression): Expression =
      DoubleSquareApp(d1, List(index))

    def VectorLength(d: Expression): Expression =
      DoubleDot(d, AsIs("length"))

    def Lazy(d: Expression): Expression =
      Keyword("lazy", d)

    def Force(d: Expression): Expression =
      Keyword("force", d)

    def Index(idx: Int, d: Expression): Expression =
      Dot(d, AsIs(s"_$idx"))

    def InstanceOf(d: Expression, clazz: Class[_]): Expression =
      Binary(d, "instanceof", Native(clazz))

    def ClosureLifted(sym: Symbol.DefnSym, ds: List[Expression]): Expression = {
      val defName = AsIs(sym.toString)
      if (ds.isEmpty) defName else App(defName, ds)
    }

    def Spawn(d1: Expression, d2: Expression): Expression =
      InRegion(Keyword("spawn", d1), d2)

    def Cast(d: Expression, tpe: Type): Expression =
      DoubleKeyword("cast", d, "as", Right(tpe))

    def Unbox(d: Expression, tpe: Type): Expression =
      DoubleKeyword("unbox", d, "as", Right(tpe))

    def Box(d: Expression): Expression =
      Keyword("box", d)

    def Without(d: Expression, sym: Symbol.EffectSym): Expression =
      Binary(d, "without", AsIs(sym.toString))

    def Cst(cst: Ast.Constant): Expression =
      printer.ConstantPrinter.print(cst)

    def ApplyClo(d: Expression, ds: List[Expression], ct: Option[Ast.ExpPosition]): Expression =
      App(d, ds)

    def ApplySelfTail(sym: Symbol.DefnSym, ds: List[Expression]): Expression =
      App(AsIs(sym.toString), ds)

    def ApplyDef(sym: Symbol.DefnSym, ds: List[Expression], ct: Option[Ast.ExpPosition]): Expression =
      App(AsIs(sym.toString), ds)

    def Do(sym: Symbol.OpSym, ds: List[Expression]): Expression =
      Keyword("do", App(AsIs(sym.toString), ds))

    def JavaInvokeMethod2(d: Expression, name: Name.Ident, ds: List[Expression]): Expression =
      App(DoubleDot(d, AsIs(name.name)), ds)

    def JavaInvokeMethod(m: Method, d: Expression, ds: List[Expression]): Expression =
      App(DoubleDot(d, AsIs(m.getName)), ds)

    def JavaInvokeStaticMethod(m: Method, ds: List[Expression]): Expression = {
      App(Dot(Native(m.getDeclaringClass), AsIs(m.getName)), ds)
    }

    def JavaGetStaticField(f: Field): Expression = {
      Dot(Native(f.getDeclaringClass), AsIs(f.getName))
    }

    def JavaInvokeConstructor(c: Constructor[_], ds: List[Expression]): Expression = {
      App(Native(c.getDeclaringClass), ds)
    }

    def JavaGetField(f: Field, d: Expression): Expression =
      DoubleDot(d, AsIs(f.getName))

    def JavaPutField(f: Field, d1: Expression, d2: Expression): Expression =
      Assign(DoubleDot(d1, AsIs(f.getName)), d2)

    def JavaPutStaticField(f: Field, d: Expression): Expression =
      Assign(Dot(Native(f.getDeclaringClass), AsIs(f.getName)), d)

    def JumpTo(sym: Symbol.LabelSym): Expression =
      Keyword("goto", AsIs(sym.toString))

    def RecordSelect(label: Name.Label, d: Expression): Expression =
      Dot(d, AsIs(label.name))

    def Regex(p: java.util.regex.Pattern): Expression =
      App(AsIs("Regex"), List(AsIs(s""""${p.toString}"""")))

    val Absent: Expression =
      AsIs("Absent")

    def Present(d: Expression): Expression =
      App(AsIs("Present"), List(d))

  }

  sealed trait Type

  object Type {

    sealed trait Atom extends Type

    sealed trait Composite extends Type

    case object Unit extends Atom

    case class AsIs(s: String) extends Atom

    case class App(obj: String, args: List[Type]) extends Atom

    case class Tuple(elms: List[Type]) extends Atom

    case class Arrow(args: List[Type], res: Type) extends Composite

    case object RecordEmpty extends Atom

    case class RecordExtend(label: String, value: Type, rest: Type) extends Atom

    case object SchemaEmpty extends Atom

    case class SchemaExtend(name: String, tpe: Type, rest: Type) extends Atom

    case class Native(clazz: Class[_]) extends Atom

    /** inserted string printed as-is (assumed not to require parenthesis) */
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

    def Array(t: Type): Type = App("Array", List(t))

    def Lazy(t: Type): Type = App("Lazy", List(t))

    def Ref(t: Type): Type = App("Ref", List(t))

    def Enum(sym: Symbol.EnumSym, args: List[Type]): Type = App(sym.toString, args)

    def Var(id: Int): Type = AsIs(s"var$id")
  }

  sealed trait Eff

  object Eff {

    case object Pure extends Eff

    /** Represents the union of IO and all regions. */
    case object Impure extends Eff

    /** Represents Impure and all algebraic effect. */
    case object ControlImpure extends Eff

    case class AsIs(s: String) extends Eff

    /** Represents the top effect. */
    def Univ: Eff = AsIs("Univ")

  }

}



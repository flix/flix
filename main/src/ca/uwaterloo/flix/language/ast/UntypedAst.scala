/*
 * Copyright 2021 Matthew Lutze
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

package ca.uwaterloo.flix.language.ast

import ca.uwaterloo.flix.language.ast.Ast.{Denotation, Source}

import java.lang.reflect.{Constructor, Field, Method}
import scala.collection.immutable.List

abstract class UntypedAst[Tpe, TVar <: Tpe] {

  val ThisAst: UntypedAst[Tpe, TVar] = this


  case class Root(classes: Map[Symbol.ClassSym, ThisAst.Class],
                  instances: Map[Symbol.ClassSym, List[ThisAst.Instance]],
                  defs: Map[Symbol.DefnSym, ThisAst.Def],
                  enums: Map[Symbol.EnumSym, ThisAst.Enum],
                  typealiases: Map[Symbol.TypeAliasSym, ThisAst.TypeAlias],
                  reachable: Set[Symbol.DefnSym],
                  sources: Map[Source, SourceLocation])

  // TODO use ResolvedAst.Law for laws
  case class Class(doc: Ast.Doc, mod: Ast.Modifiers, sym: Symbol.ClassSym, tparam: ThisAst.TypeParam, superClasses: List[ThisAst.TypeConstraint], sigs: Map[Symbol.SigSym, ThisAst.Sig], laws: List[ThisAst.Def], loc: SourceLocation)

  case class Instance(doc: Ast.Doc, mod: Ast.Modifiers, sym: Symbol.ClassSym, tpe: Tpe, tconstrs: List[ThisAst.TypeConstraint], defs: List[ThisAst.Def], ns: Name.NName, loc: SourceLocation)

  case class Sig(sym: Symbol.SigSym, spec: ThisAst.Spec, exp: Option[ThisAst.Expression])

  case class Def(sym: Symbol.DefnSym, spec: ThisAst.Spec, exp: ThisAst.Expression)

  case class Spec(doc: Ast.Doc, ann: List[ThisAst.Annotation], mod: Ast.Modifiers, tparams: ThisAst.TypeParams, fparams: List[ThisAst.FormalParam], sc: ThisAst.Scheme, tpe: Tpe, eff: Tpe, loc: SourceLocation)

  case class Enum(doc: Ast.Doc, mod: Ast.Modifiers, sym: Symbol.EnumSym, tparams: ThisAst.TypeParams, cases: Map[Name.Tag, ThisAst.Case], tpeDeprecated: Tpe, sc: ThisAst.Scheme, loc: SourceLocation)

  case class TypeAlias(doc: Ast.Doc, mod: Ast.Modifiers, sym: Symbol.TypeAliasSym, tparams: ThisAst.TypeParams, tpe: Tpe, loc: SourceLocation)

  sealed trait Expression {
    def loc: SourceLocation
  }

  object Expression {

    case class Wild(loc: SourceLocation) extends ThisAst.Expression

    case class Var(sym: Symbol.VarSym, tpe: Tpe, loc: SourceLocation) extends ThisAst.Expression

    case class Def(sym: Symbol.DefnSym, loc: SourceLocation) extends ThisAst.Expression

    case class Sig(sym: Symbol.SigSym, loc: SourceLocation) extends ThisAst.Expression

    case class Hole(sym: Symbol.HoleSym, loc: SourceLocation) extends ThisAst.Expression

    case class Unit(loc: SourceLocation) extends ThisAst.Expression

    case class Null(loc: SourceLocation) extends ThisAst.Expression

    case class True(loc: SourceLocation) extends ThisAst.Expression

    case class False(loc: SourceLocation) extends ThisAst.Expression

    case class Char(lit: scala.Char, loc: SourceLocation) extends ThisAst.Expression

    case class Float32(lit: scala.Float, loc: SourceLocation) extends ThisAst.Expression

    case class Float64(lit: scala.Double, loc: SourceLocation) extends ThisAst.Expression

    case class Int8(lit: scala.Byte, loc: SourceLocation) extends ThisAst.Expression

    case class Int16(lit: scala.Short, loc: SourceLocation) extends ThisAst.Expression

    case class Int32(lit: scala.Int, loc: SourceLocation) extends ThisAst.Expression

    case class Int64(lit: scala.Long, loc: SourceLocation) extends ThisAst.Expression

    case class BigInt(lit: java.math.BigInteger, loc: SourceLocation) extends ThisAst.Expression

    case class Str(lit: java.lang.String, loc: SourceLocation) extends ThisAst.Expression

    case class Default(loc: SourceLocation) extends ThisAst.Expression

    case class Apply(exp: ThisAst.Expression, exps: List[ThisAst.Expression], loc: SourceLocation) extends ThisAst.Expression

    case class Lambda(fparam: ThisAst.FormalParam, exp: ThisAst.Expression, loc: SourceLocation) extends ThisAst.Expression

    case class Unary(sop: SemanticOperator, exp: ThisAst.Expression, loc: SourceLocation) extends ThisAst.Expression

    case class Binary(sop: SemanticOperator, exp1: ThisAst.Expression, exp2: ThisAst.Expression, loc: SourceLocation) extends ThisAst.Expression

    case class IfThenElse(exp1: ThisAst.Expression, exp2: ThisAst.Expression, exp3: ThisAst.Expression, loc: SourceLocation) extends ThisAst.Expression

    case class Stm(exp1: ThisAst.Expression, exp2: ThisAst.Expression, loc: SourceLocation) extends ThisAst.Expression

    case class Let(sym: Symbol.VarSym, mod: Ast.Modifiers, exp1: ThisAst.Expression, exp2: ThisAst.Expression, loc: SourceLocation) extends ThisAst.Expression

    case class LetRegion(sym: Symbol.VarSym, exp: ThisAst.Expression, loc: SourceLocation) extends ThisAst.Expression

    case class Match(exp: ThisAst.Expression, rules: List[ThisAst.MatchRule], loc: SourceLocation) extends ThisAst.Expression

    case class Choose(star: Boolean, exps: List[ThisAst.Expression], rules: List[ThisAst.ChoiceRule], loc: SourceLocation) extends ThisAst.Expression

    case class Tag(sym: Symbol.EnumSym, tag: Name.Tag, exp: ThisAst.Expression, loc: SourceLocation) extends ThisAst.Expression

    case class Tuple(elms: List[ThisAst.Expression], loc: SourceLocation) extends ThisAst.Expression

    case class RecordEmpty(loc: SourceLocation) extends ThisAst.Expression

    case class RecordSelect(exp: ThisAst.Expression, field: Name.Field, loc: SourceLocation) extends ThisAst.Expression

    case class RecordExtend(field: Name.Field, value: ThisAst.Expression, rest: ThisAst.Expression, loc: SourceLocation) extends ThisAst.Expression

    case class RecordRestrict(field: Name.Field, rest: ThisAst.Expression, loc: SourceLocation) extends ThisAst.Expression

    case class ArrayLit(elms: List[ThisAst.Expression], loc: SourceLocation) extends ThisAst.Expression

    case class ArrayNew(elm: ThisAst.Expression, len: ThisAst.Expression, loc: SourceLocation) extends ThisAst.Expression

    case class ArrayLoad(base: ThisAst.Expression, index: ThisAst.Expression, loc: SourceLocation) extends ThisAst.Expression

    case class ArrayStore(base: ThisAst.Expression, index: ThisAst.Expression, elm: ThisAst.Expression, loc: SourceLocation) extends ThisAst.Expression

    case class ArrayLength(base: ThisAst.Expression, loc: SourceLocation) extends ThisAst.Expression

    case class ArraySlice(base: ThisAst.Expression, beginIndex: ThisAst.Expression, endIndex: ThisAst.Expression, loc: SourceLocation) extends ThisAst.Expression

    case class Ref(exp: ThisAst.Expression, loc: SourceLocation) extends ThisAst.Expression

    case class RefWithRegion(exp1: ThisAst.Expression, exp2: ThisAst.Expression, loc: SourceLocation) extends ThisAst.Expression

    case class Deref(exp: ThisAst.Expression, loc: SourceLocation) extends ThisAst.Expression

    case class Assign(exp1: ThisAst.Expression, exp2: ThisAst.Expression, loc: SourceLocation) extends ThisAst.Expression

    case class Existential(fparam: ThisAst.FormalParam, exp: ThisAst.Expression, loc: SourceLocation) extends ThisAst.Expression

    case class Universal(fparam: ThisAst.FormalParam, exp: ThisAst.Expression, loc: SourceLocation) extends ThisAst.Expression

    case class Ascribe(exp: ThisAst.Expression, expectedType: Option[Tpe], expectedEff: Option[Tpe], loc: SourceLocation) extends ThisAst.Expression

    case class Cast(exp: ThisAst.Expression, declaredType: Option[Tpe], declaredEff: Option[Tpe], loc: SourceLocation) extends ThisAst.Expression

    case class TryCatch(exp: ThisAst.Expression, rules: List[ThisAst.CatchRule], loc: SourceLocation) extends ThisAst.Expression

    case class InvokeConstructor(constructor: Constructor[_], args: List[ThisAst.Expression], loc: SourceLocation) extends ThisAst.Expression

    case class InvokeMethod(method: Method, exp: ThisAst.Expression, args: List[ThisAst.Expression], loc: SourceLocation) extends ThisAst.Expression

    case class InvokeStaticMethod(method: Method, args: List[ThisAst.Expression], loc: SourceLocation) extends ThisAst.Expression

    case class GetField(field: Field, exp: ThisAst.Expression, loc: SourceLocation) extends ThisAst.Expression

    case class PutField(field: Field, exp1: ThisAst.Expression, exp2: ThisAst.Expression, loc: SourceLocation) extends ThisAst.Expression

    case class GetStaticField(field: Field, loc: SourceLocation) extends ThisAst.Expression

    case class PutStaticField(field: Field, exp: ThisAst.Expression, loc: SourceLocation) extends ThisAst.Expression

    case class NewChannel(exp: ThisAst.Expression, tpe: Tpe, loc: SourceLocation) extends ThisAst.Expression

    case class GetChannel(exp: ThisAst.Expression, loc: SourceLocation) extends ThisAst.Expression

    case class PutChannel(exp1: ThisAst.Expression, exp2: ThisAst.Expression, loc: SourceLocation) extends ThisAst.Expression

    case class SelectChannel(rules: List[ThisAst.SelectChannelRule], default: Option[ThisAst.Expression], loc: SourceLocation) extends ThisAst.Expression

    case class Spawn(exp: ThisAst.Expression, loc: SourceLocation) extends ThisAst.Expression

    case class Lazy(exp: ThisAst.Expression, loc: SourceLocation) extends ThisAst.Expression

    case class Force(exp: ThisAst.Expression, loc: SourceLocation) extends ThisAst.Expression

    case class FixpointConstraintSet(cs: List[ThisAst.Constraint], loc: SourceLocation) extends ThisAst.Expression

    case class FixpointMerge(exp1: ThisAst.Expression, exp2: ThisAst.Expression, loc: SourceLocation) extends ThisAst.Expression

    case class FixpointSolve(exp: ThisAst.Expression, loc: SourceLocation) extends ThisAst.Expression

    case class FixpointFilter(pred: Name.Pred, exp: ThisAst.Expression, loc: SourceLocation) extends ThisAst.Expression

    case class FixpointProjectIn(exp: ThisAst.Expression, pred: Name.Pred, loc: SourceLocation) extends ThisAst.Expression

    case class FixpointProjectOut(pred: Name.Pred, exp1: ThisAst.Expression, exp2: ThisAst.Expression, loc: SourceLocation) extends ThisAst.Expression

    case class MatchEff(exp1: ThisAst.Expression, exp2: ThisAst.Expression, exp3: ThisAst.Expression, loc: SourceLocation) extends ThisAst.Expression

  }

  sealed trait Pattern {
    def loc: SourceLocation
  }

  object Pattern {

    case class Wild(loc: SourceLocation) extends ThisAst.Pattern

    case class Var(sym: Symbol.VarSym, loc: SourceLocation) extends ThisAst.Pattern

    case class Unit(loc: SourceLocation) extends ThisAst.Pattern

    case class True(loc: SourceLocation) extends ThisAst.Pattern

    case class False(loc: SourceLocation) extends ThisAst.Pattern

    case class Char(lit: scala.Char, loc: SourceLocation) extends ThisAst.Pattern

    case class Float32(lit: scala.Float, loc: SourceLocation) extends ThisAst.Pattern

    case class Float64(lit: scala.Double, loc: SourceLocation) extends ThisAst.Pattern

    case class Int8(lit: scala.Byte, loc: SourceLocation) extends ThisAst.Pattern

    case class Int16(lit: scala.Short, loc: SourceLocation) extends ThisAst.Pattern

    case class Int32(lit: scala.Int, loc: SourceLocation) extends ThisAst.Pattern

    case class Int64(lit: scala.Long, loc: SourceLocation) extends ThisAst.Pattern

    case class BigInt(lit: java.math.BigInteger, loc: SourceLocation) extends ThisAst.Pattern

    case class Str(lit: java.lang.String, loc: SourceLocation) extends ThisAst.Pattern

    case class Tag(sym: Symbol.EnumSym, tag: Name.Tag, pat: ThisAst.Pattern, loc: SourceLocation) extends ThisAst.Pattern

    case class Tuple(elms: List[ThisAst.Pattern], loc: SourceLocation) extends ThisAst.Pattern

    case class Array(elms: List[ThisAst.Pattern], loc: SourceLocation) extends ThisAst.Pattern

    case class ArrayTailSpread(elms: scala.List[ThisAst.Pattern], sym: Symbol.VarSym, loc: SourceLocation) extends ThisAst.Pattern

    case class ArrayHeadSpread(sym: Symbol.VarSym, elms: scala.List[ThisAst.Pattern], loc: SourceLocation) extends ThisAst.Pattern

  }

  sealed trait ChoicePattern {
    def loc: SourceLocation
  }

  object ChoicePattern {

    case class Wild(loc: SourceLocation) extends ChoicePattern

    case class Absent(loc: SourceLocation) extends ChoicePattern

    case class Present(sym: Symbol.VarSym, loc: SourceLocation) extends ChoicePattern

  }

  sealed trait Predicate

  object Predicate {

    sealed trait Head extends ThisAst.Predicate

    object Head {

      case class Atom(pred: Name.Pred, den: Denotation, terms: List[ThisAst.Expression], loc: SourceLocation) extends ThisAst.Predicate.Head

    }

    sealed trait Body extends ThisAst.Predicate

    object Body {

      case class Atom(pred: Name.Pred, den: Denotation, polarity: Ast.Polarity, terms: List[ThisAst.Pattern], loc: SourceLocation) extends ThisAst.Predicate.Body

      case class Guard(exp: ThisAst.Expression, loc: SourceLocation) extends ThisAst.Predicate.Body

    }

  }

  case class Scheme(quantifiers: List[TVar], constraints: List[ThisAst.TypeConstraint], base: Tpe)

  sealed trait TypeParams {
    val tparams: List[ThisAst.TypeParam]
  }

  object TypeParams {

    case class Kinded(tparams: List[ThisAst.TypeParam.Kinded]) extends ThisAst.TypeParams

    case class Unkinded(tparams: List[ThisAst.TypeParam.Unkinded]) extends ThisAst.TypeParams

  }

  case class Annotation(name: Ast.Annotation, exps: List[ThisAst.Expression], loc: SourceLocation)

  case class Attribute(ident: Name.Ident, tpe: Tpe, loc: SourceLocation)

  case class Case(enum: Name.Ident, tag: Name.Tag, tpeDeprecated: Tpe, sc: Scheme)

  case class Constraint(cparams: List[ThisAst.ConstraintParam], head: ThisAst.Predicate.Head, body: List[ThisAst.Predicate.Body], loc: SourceLocation)

  sealed trait ConstraintParam

  object ConstraintParam {

    case class HeadParam(sym: Symbol.VarSym, tpe: TVar, loc: SourceLocation) extends ThisAst.ConstraintParam

    case class RuleParam(sym: Symbol.VarSym, tpe: TVar, loc: SourceLocation) extends ThisAst.ConstraintParam

  }

  case class FormalParam(sym: Symbol.VarSym, mod: Ast.Modifiers, tpe: Tpe, loc: SourceLocation)

  case class CatchRule(sym: Symbol.VarSym, clazz: java.lang.Class[_], exp: ThisAst.Expression)

  case class ChoiceRule(pat: List[ThisAst.ChoicePattern], exp: ThisAst.Expression)

  case class MatchRule(pat: ThisAst.Pattern, guard: ThisAst.Expression, exp: ThisAst.Expression)

  case class SelectChannelRule(sym: Symbol.VarSym, chan: ThisAst.Expression, exp: ThisAst.Expression)

  sealed trait TypeParam {
    val tpe: TVar
  }

  object TypeParam {

    case class Kinded(name: Name.Ident, tpe: TVar, kind: Kind, loc: SourceLocation) extends ThisAst.TypeParam

    case class Unkinded(name: Name.Ident, tpe: TVar, loc: SourceLocation) extends ThisAst.TypeParam

  }

  case class TypeConstraint(clazz: Symbol.ClassSym, tpe: Tpe, loc: SourceLocation)

}

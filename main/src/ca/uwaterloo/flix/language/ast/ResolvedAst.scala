/*
 *  Copyright 2017 Magnus Madsen
 *  
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *  
 *  http://www.apache.org/licenses/LICENSE-2.0
 *  
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

package ca.uwaterloo.flix.language.ast

import ca.uwaterloo.flix.language.ast.Ast.{Denotation, Source}

import java.lang.reflect.{Constructor, Field, Method}
import scala.collection.immutable.List

object ResolvedAst {

  case class Root(classes: Map[Symbol.ClassSym, ResolvedAst.Class],
                  instances: Map[Symbol.ClassSym, List[ResolvedAst.Instance]],
                  defs: Map[Symbol.DefnSym, ResolvedAst.Def],
                  enums: Map[Symbol.EnumSym, ResolvedAst.Enum],
                  typealiases: Map[Symbol.TypeAliasSym, ResolvedAst.TypeAlias],
                  reachable: Set[Symbol.DefnSym],
                  sources: Map[Source, SourceLocation])

  // TODO use ResolvedAst.Law for laws
  case class Class(doc: Ast.Doc, mod: Ast.Modifiers, sym: Symbol.ClassSym, tparam: ResolvedAst.TypeParam, superClasses: List[ResolvedAst.TypeConstraint], sigs: Map[Symbol.SigSym, ResolvedAst.Sig], laws: List[ResolvedAst.Def], loc: SourceLocation)

  case class Instance(doc: Ast.Doc, mod: Ast.Modifiers, sym: Symbol.ClassSym, tpe: UnkindedType, tconstrs: List[ResolvedAst.TypeConstraint], defs: List[ResolvedAst.Def], ns: Name.NName, loc: SourceLocation)

  case class Sig(sym: Symbol.SigSym, spec: ResolvedAst.Spec, exp: Option[ResolvedAst.Expression])

  case class Def(sym: Symbol.DefnSym, spec: ResolvedAst.Spec, exp: ResolvedAst.Expression)

  case class Spec(doc: Ast.Doc, ann: List[ResolvedAst.Annotation], mod: Ast.Modifiers, tparams: ResolvedAst.TypeParams, fparams: List[ResolvedAst.FormalParam], sc: ResolvedAst.Scheme, tpe: UnkindedType, eff: UnkindedType, loc: SourceLocation)

  case class Enum(doc: Ast.Doc, mod: Ast.Modifiers, sym: Symbol.EnumSym, tparams: ResolvedAst.TypeParams, cases: Map[Name.Tag, ResolvedAst.Case], tpeDeprecated: UnkindedType, sc: ResolvedAst.Scheme, loc: SourceLocation)

  case class TypeAlias(doc: Ast.Doc, mod: Ast.Modifiers, sym: Symbol.TypeAliasSym, tparams: ResolvedAst.TypeParams, tpe: UnkindedType, loc: SourceLocation)

  sealed trait Expression {
    def loc: SourceLocation
  }

  object Expression {

    case class Wild(tpe: UnkindedType.Var, loc: SourceLocation) extends ResolvedAst.Expression

    case class Var(sym: Symbol.VarSym, tpe: UnkindedType, loc: SourceLocation) extends ResolvedAst.Expression

    case class Def(sym: Symbol.DefnSym, tpe: UnkindedType.Var, loc: SourceLocation) extends ResolvedAst.Expression

    case class Sig(sym: Symbol.SigSym, tpe: UnkindedType.Var, loc: SourceLocation) extends ResolvedAst.Expression

    case class Hole(sym: Symbol.HoleSym, tpe: UnkindedType.Var, eff: UnkindedType.Var, loc: SourceLocation) extends ResolvedAst.Expression

    case class Unit(loc: SourceLocation) extends ResolvedAst.Expression

    case class Null(loc: SourceLocation) extends ResolvedAst.Expression

    case class True(loc: SourceLocation) extends ResolvedAst.Expression

    case class False(loc: SourceLocation) extends ResolvedAst.Expression

    case class Char(lit: scala.Char, loc: SourceLocation) extends ResolvedAst.Expression

    case class Float32(lit: scala.Float, loc: SourceLocation) extends ResolvedAst.Expression

    case class Float64(lit: scala.Double, loc: SourceLocation) extends ResolvedAst.Expression

    case class Int8(lit: scala.Byte, loc: SourceLocation) extends ResolvedAst.Expression

    case class Int16(lit: scala.Short, loc: SourceLocation) extends ResolvedAst.Expression

    case class Int32(lit: scala.Int, loc: SourceLocation) extends ResolvedAst.Expression

    case class Int64(lit: scala.Long, loc: SourceLocation) extends ResolvedAst.Expression

    case class BigInt(lit: java.math.BigInteger, loc: SourceLocation) extends ResolvedAst.Expression

    case class Str(lit: java.lang.String, loc: SourceLocation) extends ResolvedAst.Expression

    case class Default(tpe: UnkindedType.Var, loc: SourceLocation) extends ResolvedAst.Expression

    case class Apply(exp: ResolvedAst.Expression, exps: List[ResolvedAst.Expression], tpe: UnkindedType.Var, eff: UnkindedType.Var, loc: SourceLocation) extends ResolvedAst.Expression

    case class Lambda(fparam: ResolvedAst.FormalParam, exp: ResolvedAst.Expression, tpe: UnkindedType.Var, loc: SourceLocation) extends ResolvedAst.Expression

    case class Unary(sop: SemanticOperator, exp: ResolvedAst.Expression, tpe: UnkindedType.Var, loc: SourceLocation) extends ResolvedAst.Expression

    case class Binary(sop: SemanticOperator, exp1: ResolvedAst.Expression, exp2: ResolvedAst.Expression, tpe: UnkindedType.Var, loc: SourceLocation) extends ResolvedAst.Expression

    case class IfThenElse(exp1: ResolvedAst.Expression, exp2: ResolvedAst.Expression, exp3: ResolvedAst.Expression, loc: SourceLocation) extends ResolvedAst.Expression

    case class Stm(exp1: ResolvedAst.Expression, exp2: ResolvedAst.Expression, loc: SourceLocation) extends ResolvedAst.Expression

    case class Let(sym: Symbol.VarSym, mod: Ast.Modifiers, exp1: ResolvedAst.Expression, exp2: ResolvedAst.Expression, loc: SourceLocation) extends ResolvedAst.Expression

    case class LetRegion(sym: Symbol.VarSym, exp: ResolvedAst.Expression, evar: UnkindedType.Var, loc: SourceLocation) extends ResolvedAst.Expression

    case class Match(exp: ResolvedAst.Expression, rules: List[ResolvedAst.MatchRule], loc: SourceLocation) extends ResolvedAst.Expression

    case class Choose(star: Boolean, exps: List[ResolvedAst.Expression], rules: List[ResolvedAst.ChoiceRule], tpe: UnkindedType.Var, loc: SourceLocation) extends ResolvedAst.Expression

    case class Tag(sym: Symbol.EnumSym, tag: Name.Tag, exp: ResolvedAst.Expression, tpe: UnkindedType.Var, loc: SourceLocation) extends ResolvedAst.Expression

    case class Tuple(elms: List[ResolvedAst.Expression], loc: SourceLocation) extends ResolvedAst.Expression

    case class RecordEmpty(tpe: UnkindedType.Var, loc: SourceLocation) extends ResolvedAst.Expression

    case class RecordSelect(exp: ResolvedAst.Expression, field: Name.Field, tpe: UnkindedType.Var, loc: SourceLocation) extends ResolvedAst.Expression

    case class RecordExtend(field: Name.Field, value: ResolvedAst.Expression, rest: ResolvedAst.Expression, tpe: UnkindedType.Var, loc: SourceLocation) extends ResolvedAst.Expression

    case class RecordRestrict(field: Name.Field, rest: ResolvedAst.Expression, tpe: UnkindedType.Var, loc: SourceLocation) extends ResolvedAst.Expression

    case class ArrayLit(elms: List[ResolvedAst.Expression], tpe: UnkindedType.Var, loc: SourceLocation) extends ResolvedAst.Expression

    case class ArrayNew(elm: ResolvedAst.Expression, len: ResolvedAst.Expression, tpe: UnkindedType.Var, loc: SourceLocation) extends ResolvedAst.Expression

    case class ArrayLoad(base: ResolvedAst.Expression, index: ResolvedAst.Expression, tpe: UnkindedType.Var, loc: SourceLocation) extends ResolvedAst.Expression

    case class ArrayStore(base: ResolvedAst.Expression, index: ResolvedAst.Expression, elm: ResolvedAst.Expression, loc: SourceLocation) extends ResolvedAst.Expression

    case class ArrayLength(base: ResolvedAst.Expression, loc: SourceLocation) extends ResolvedAst.Expression

    case class ArraySlice(base: ResolvedAst.Expression, beginIndex: ResolvedAst.Expression, endIndex: ResolvedAst.Expression, loc: SourceLocation) extends ResolvedAst.Expression

    case class Ref(exp: ResolvedAst.Expression, tvar: UnkindedType.Var, loc: SourceLocation) extends ResolvedAst.Expression

    case class RefWithRegion(exp1: ResolvedAst.Expression, exp2: ResolvedAst.Expression, tpe: UnkindedType.Var, evar: UnkindedType.Var, loc: SourceLocation) extends ResolvedAst.Expression

    case class Deref(exp: ResolvedAst.Expression, tvar: UnkindedType.Var, evar: UnkindedType.Var, loc: SourceLocation) extends ResolvedAst.Expression

    case class Assign(exp1: ResolvedAst.Expression, exp2: ResolvedAst.Expression, evar: UnkindedType.Var, loc: SourceLocation) extends ResolvedAst.Expression

    case class Existential(fparam: ResolvedAst.FormalParam, exp: ResolvedAst.Expression, loc: SourceLocation) extends ResolvedAst.Expression

    case class Universal(fparam: ResolvedAst.FormalParam, exp: ResolvedAst.Expression, loc: SourceLocation) extends ResolvedAst.Expression

    case class Ascribe(exp: ResolvedAst.Expression, expectedType: Option[UnkindedType], expectedEff: Option[UnkindedType], tpe: UnkindedType.Var, loc: SourceLocation) extends ResolvedAst.Expression

    case class Cast(exp: ResolvedAst.Expression, declaredType: Option[UnkindedType], declaredEff: Option[UnkindedType], tpe: UnkindedType.Var, loc: SourceLocation) extends ResolvedAst.Expression

    case class TryCatch(exp: ResolvedAst.Expression, rules: List[ResolvedAst.CatchRule], loc: SourceLocation) extends ResolvedAst.Expression

    case class InvokeConstructor(constructor: Constructor[_], args: List[ResolvedAst.Expression], loc: SourceLocation) extends ResolvedAst.Expression

    case class InvokeMethod(method: Method, exp: ResolvedAst.Expression, args: List[ResolvedAst.Expression], loc: SourceLocation) extends ResolvedAst.Expression

    case class InvokeStaticMethod(method: Method, args: List[ResolvedAst.Expression], loc: SourceLocation) extends ResolvedAst.Expression

    case class GetField(field: Field, exp: ResolvedAst.Expression, loc: SourceLocation) extends ResolvedAst.Expression

    case class PutField(field: Field, exp1: ResolvedAst.Expression, exp2: ResolvedAst.Expression, loc: SourceLocation) extends ResolvedAst.Expression

    case class GetStaticField(field: Field, loc: SourceLocation) extends ResolvedAst.Expression

    case class PutStaticField(field: Field, exp: ResolvedAst.Expression, loc: SourceLocation) extends ResolvedAst.Expression

    case class NewChannel(exp: ResolvedAst.Expression, tpe: UnkindedType, loc: SourceLocation) extends ResolvedAst.Expression

    case class GetChannel(exp: ResolvedAst.Expression, tpe: UnkindedType.Var, loc: SourceLocation) extends ResolvedAst.Expression

    case class PutChannel(exp1: ResolvedAst.Expression, exp2: ResolvedAst.Expression, tpe: UnkindedType.Var, loc: SourceLocation) extends ResolvedAst.Expression

    case class SelectChannel(rules: List[ResolvedAst.SelectChannelRule], default: Option[ResolvedAst.Expression], tpe: UnkindedType.Var, loc: SourceLocation) extends ResolvedAst.Expression

    case class Spawn(exp: ResolvedAst.Expression, loc: SourceLocation) extends ResolvedAst.Expression

    case class Lazy(exp: ResolvedAst.Expression, loc: SourceLocation) extends ResolvedAst.Expression

    case class Force(exp: ResolvedAst.Expression, tpe: UnkindedType.Var, loc: SourceLocation) extends ResolvedAst.Expression

    case class FixpointConstraintSet(cs: List[ResolvedAst.Constraint], tpe: UnkindedType.Var, loc: SourceLocation) extends ResolvedAst.Expression

    case class FixpointMerge(exp1: ResolvedAst.Expression, exp2: ResolvedAst.Expression, loc: SourceLocation) extends ResolvedAst.Expression

    case class FixpointSolve(exp: ResolvedAst.Expression, loc: SourceLocation) extends ResolvedAst.Expression

    case class FixpointFilter(pred: Name.Pred, exp: ResolvedAst.Expression, tpe: UnkindedType.Var, loc: SourceLocation) extends ResolvedAst.Expression

    case class FixpointProjectIn(exp: ResolvedAst.Expression, pred: Name.Pred, tpe: UnkindedType.Var, loc: SourceLocation) extends ResolvedAst.Expression

    case class FixpointProjectOut(pred: Name.Pred, exp1: ResolvedAst.Expression, exp2: ResolvedAst.Expression, tpe: UnkindedType.Var, loc: SourceLocation) extends ResolvedAst.Expression

    case class MatchEff(exp1: ResolvedAst.Expression, exp2: ResolvedAst.Expression, exp3: ResolvedAst.Expression, loc: SourceLocation) extends ResolvedAst.Expression

  }

  sealed trait Pattern {
    def loc: SourceLocation
  }

  object Pattern {

    case class Wild(tvar: UnkindedType.Var, loc: SourceLocation) extends ResolvedAst.Pattern

    case class Var(sym: Symbol.VarSym, tvar: UnkindedType.Var, loc: SourceLocation) extends ResolvedAst.Pattern

    case class Unit(loc: SourceLocation) extends ResolvedAst.Pattern

    case class True(loc: SourceLocation) extends ResolvedAst.Pattern

    case class False(loc: SourceLocation) extends ResolvedAst.Pattern

    case class Char(lit: scala.Char, loc: SourceLocation) extends ResolvedAst.Pattern

    case class Float32(lit: scala.Float, loc: SourceLocation) extends ResolvedAst.Pattern

    case class Float64(lit: scala.Double, loc: SourceLocation) extends ResolvedAst.Pattern

    case class Int8(lit: scala.Byte, loc: SourceLocation) extends ResolvedAst.Pattern

    case class Int16(lit: scala.Short, loc: SourceLocation) extends ResolvedAst.Pattern

    case class Int32(lit: scala.Int, loc: SourceLocation) extends ResolvedAst.Pattern

    case class Int64(lit: scala.Long, loc: SourceLocation) extends ResolvedAst.Pattern

    case class BigInt(lit: java.math.BigInteger, loc: SourceLocation) extends ResolvedAst.Pattern

    case class Str(lit: java.lang.String, loc: SourceLocation) extends ResolvedAst.Pattern

    case class Tag(sym: Symbol.EnumSym, tag: Name.Tag, pat: ResolvedAst.Pattern, tvar: UnkindedType.Var, loc: SourceLocation) extends ResolvedAst.Pattern

    case class Tuple(elms: List[ResolvedAst.Pattern], loc: SourceLocation) extends ResolvedAst.Pattern

    case class Array(elms: List[ResolvedAst.Pattern], tvar: UnkindedType.Var, loc: SourceLocation) extends ResolvedAst.Pattern

    case class ArrayTailSpread(elms: scala.List[ResolvedAst.Pattern], sym: Symbol.VarSym, tvar: UnkindedType.Var, loc: SourceLocation) extends ResolvedAst.Pattern

    case class ArrayHeadSpread(sym: Symbol.VarSym, elms: scala.List[ResolvedAst.Pattern], tvar: UnkindedType.Var, loc: SourceLocation) extends ResolvedAst.Pattern

  }

  sealed trait ChoicePattern {
    def loc: SourceLocation
  }

  object ChoicePattern {

    case class Wild(loc: SourceLocation) extends ChoicePattern

    case class Absent(loc: SourceLocation) extends ChoicePattern

    case class Present(sym: Symbol.VarSym, tvar: UnkindedType.Var, loc: SourceLocation) extends ChoicePattern

  }

  sealed trait Predicate

  object Predicate {

    sealed trait Head extends ResolvedAst.Predicate

    object Head {

      case class Atom(pred: Name.Pred, den: Denotation, terms: List[ResolvedAst.Expression], tvar: UnkindedType.Var, loc: SourceLocation) extends ResolvedAst.Predicate.Head

    }

    sealed trait Body extends ResolvedAst.Predicate

    object Body {

      case class Atom(pred: Name.Pred, den: Denotation, polarity: Ast.Polarity, terms: List[ResolvedAst.Pattern], tvar: UnkindedType.Var, loc: SourceLocation) extends ResolvedAst.Predicate.Body

      case class Guard(exp: ResolvedAst.Expression, loc: SourceLocation) extends ResolvedAst.Predicate.Body

    }

  }

  case class Scheme(quantifiers: List[UnkindedType.Var], constraints: List[ResolvedAst.TypeConstraint], base: UnkindedType)

  sealed trait TypeParams {
    val tparams: List[ResolvedAst.TypeParam]
  }

  object TypeParams {

    case class Kinded(tparams: List[ResolvedAst.TypeParam.Kinded]) extends TypeParams

    case class Unkinded(tparams: List[ResolvedAst.TypeParam.Unkinded]) extends TypeParams

  }

  case class Annotation(name: Ast.Annotation, exps: List[ResolvedAst.Expression], loc: SourceLocation)

  case class Attribute(ident: Name.Ident, tpe: UnkindedType, loc: SourceLocation)

  case class Case(enum: Name.Ident, tag: Name.Tag, tpeDeprecated: UnkindedType, sc: Scheme)

  case class Constraint(cparams: List[ResolvedAst.ConstraintParam], head: ResolvedAst.Predicate.Head, body: List[ResolvedAst.Predicate.Body], loc: SourceLocation)

  sealed trait ConstraintParam

  object ConstraintParam {

    case class HeadParam(sym: Symbol.VarSym, tpe: UnkindedType.Var, loc: SourceLocation) extends ResolvedAst.ConstraintParam

    case class RuleParam(sym: Symbol.VarSym, tpe: UnkindedType.Var, loc: SourceLocation) extends ResolvedAst.ConstraintParam

  }

  case class FormalParam(sym: Symbol.VarSym, mod: Ast.Modifiers, tpe: UnkindedType, loc: SourceLocation)

  case class CatchRule(sym: Symbol.VarSym, clazz: java.lang.Class[_], exp: ResolvedAst.Expression)

  case class ChoiceRule(pat: List[ResolvedAst.ChoicePattern], exp: ResolvedAst.Expression)

  case class MatchRule(pat: ResolvedAst.Pattern, guard: ResolvedAst.Expression, exp: ResolvedAst.Expression)

  case class SelectChannelRule(sym: Symbol.VarSym, chan: ResolvedAst.Expression, exp: ResolvedAst.Expression)

  sealed trait TypeParam {
    val tpe: UnkindedType.Var
  }

  object TypeParam {

    case class Kinded(name: Name.Ident, tpe: UnkindedType.Var, kind: Kind, loc: SourceLocation) extends TypeParam

    case class Unkinded(name: Name.Ident, tpe: UnkindedType.Var, loc: SourceLocation) extends TypeParam

  }

  case class TypeConstraint(clazz: Symbol.ClassSym, tpe: UnkindedType, loc: SourceLocation)

}

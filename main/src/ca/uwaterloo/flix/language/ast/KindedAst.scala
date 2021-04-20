/*
 *  Copyright 2021 Matthew Lutze
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

import ca.uwaterloo.flix.language.ast
import ca.uwaterloo.flix.language.ast.Ast.{Denotation, Source}

import java.lang.reflect.{Constructor, Field, Method}
import scala.collection.immutable.List

object KindedAst {

  case class Root(classes: Map[Symbol.ClassSym, KindedAst.Class],
                  instances: Map[Symbol.ClassSym, List[KindedAst.Instance]],
                  defs: Map[Symbol.DefnSym, KindedAst.Def],
                  enums: Map[Symbol.EnumSym, KindedAst.Enum],
                  properties: List[KindedAst.Property],
                  reachable: Set[Symbol.DefnSym],
                  sources: Map[Source, SourceLocation])

  // TODO use ResolvedAst.Law for laws
  case class Class(doc: Ast.Doc, mod: Ast.Modifiers, sym: Symbol.ClassSym, tparam: KindedAst.TypeParam, superClasses: List[Ast.TypeConstraint], sigs: Map[Symbol.SigSym, KindedAst.Sig], laws: List[KindedAst.Def], loc: SourceLocation)

  case class Instance(doc: Ast.Doc, mod: Ast.Modifiers, sym: Symbol.ClassSym, tpe: Type, tconstrs: List[Ast.TypeConstraint], defs: List[KindedAst.Def], ns: Name.NName, loc: SourceLocation)

  case class Sig(sym: Symbol.SigSym, spec: KindedAst.Spec, exp: Option[KindedAst.Expression])

  case class Def(sym: Symbol.DefnSym, spec: KindedAst.Spec, exp: KindedAst.Expression)

  case class Spec(doc: Ast.Doc, ann: List[KindedAst.Annotation], mod: Ast.Modifiers, tparams: List[KindedAst.TypeParam], fparams: List[KindedAst.FormalParam], sc: Scheme, eff: Type, loc: SourceLocation)

  case class Enum(doc: Ast.Doc, mod: Ast.Modifiers, sym: Symbol.EnumSym, tparams: List[KindedAst.TypeParam], cases: Map[Name.Tag, KindedAst.Case], tpeDeprecated: Type, sc: Scheme, loc: SourceLocation)

  case class Property(law: Symbol.DefnSym, defn: Symbol.DefnSym, exp: KindedAst.Expression, loc: SourceLocation)

  sealed trait Expression {
    def loc: SourceLocation
  }

  object Expression {

    case class Wild(tpe: Type.Var, loc: SourceLocation) extends KindedAst.Expression

    case class Var(sym: Symbol.VarSym, tpe: Type, loc: SourceLocation) extends KindedAst.Expression

    case class Def(sym: Symbol.DefnSym, tpe: Type.Var, loc: SourceLocation) extends KindedAst.Expression

    case class Sig(sym: Symbol.SigSym, tpe: Type.Var, loc: SourceLocation) extends KindedAst.Expression

    case class Hole(sym: Symbol.HoleSym, tpe: Type.Var, eff: Type.Var, loc: SourceLocation) extends KindedAst.Expression

    case class Unit(loc: SourceLocation) extends KindedAst.Expression

    case class Null(loc: SourceLocation) extends KindedAst.Expression

    case class True(loc: SourceLocation) extends KindedAst.Expression

    case class False(loc: SourceLocation) extends KindedAst.Expression

    case class Char(lit: scala.Char, loc: SourceLocation) extends KindedAst.Expression

    case class Float32(lit: scala.Float, loc: SourceLocation) extends KindedAst.Expression

    case class Float64(lit: scala.Double, loc: SourceLocation) extends KindedAst.Expression

    case class Int8(lit: scala.Byte, loc: SourceLocation) extends KindedAst.Expression

    case class Int16(lit: scala.Short, loc: SourceLocation) extends KindedAst.Expression

    case class Int32(lit: scala.Int, loc: SourceLocation) extends KindedAst.Expression

    case class Int64(lit: scala.Long, loc: SourceLocation) extends KindedAst.Expression

    case class BigInt(lit: java.math.BigInteger, loc: SourceLocation) extends KindedAst.Expression

    case class Str(lit: java.lang.String, loc: SourceLocation) extends KindedAst.Expression

    case class Default(tpe: Type.Var, loc: SourceLocation) extends KindedAst.Expression

    case class Apply(exp: KindedAst.Expression, exps: List[KindedAst.Expression], tpe: Type.Var, eff: Type.Var, loc: SourceLocation) extends KindedAst.Expression

    case class Lambda(fparam: KindedAst.FormalParam, exp: KindedAst.Expression, tpe: Type.Var, loc: SourceLocation) extends KindedAst.Expression

    case class Unary(sop: SemanticOperator, exp: KindedAst.Expression, tpe: Type.Var, loc: SourceLocation) extends KindedAst.Expression

    case class Binary(sop: SemanticOperator, exp1: KindedAst.Expression, exp2: KindedAst.Expression, tpe: Type.Var, loc: SourceLocation) extends KindedAst.Expression

    case class IfThenElse(exp1: KindedAst.Expression, exp2: KindedAst.Expression, exp3: KindedAst.Expression, loc: SourceLocation) extends KindedAst.Expression

    case class Stm(exp1: KindedAst.Expression, exp2: KindedAst.Expression, loc: SourceLocation) extends KindedAst.Expression

    case class Let(sym: Symbol.VarSym, exp1: KindedAst.Expression, exp2: KindedAst.Expression, loc: SourceLocation) extends KindedAst.Expression

    case class Match(exp: KindedAst.Expression, rules: List[KindedAst.MatchRule], loc: SourceLocation) extends KindedAst.Expression

    case class Choose(star: Boolean, exps: List[KindedAst.Expression], rules: List[KindedAst.ChoiceRule], tpe: Type.Var, loc: SourceLocation) extends KindedAst.Expression

    case class Tag(sym: Symbol.EnumSym, tag: Name.Tag, exp: KindedAst.Expression, tpe: Type.Var, loc: SourceLocation) extends KindedAst.Expression

    case class Tuple(elms: List[KindedAst.Expression], loc: SourceLocation) extends KindedAst.Expression

    case class RecordEmpty(tpe: Type.Var, loc: SourceLocation) extends KindedAst.Expression

    case class RecordSelect(exp: KindedAst.Expression, field: Name.Field, tpe: Type.Var, loc: SourceLocation) extends KindedAst.Expression

    case class RecordExtend(field: Name.Field, value: KindedAst.Expression, rest: KindedAst.Expression, tpe: Type.Var, loc: SourceLocation) extends KindedAst.Expression

    case class RecordRestrict(field: Name.Field, rest: KindedAst.Expression, tpe: Type.Var, loc: SourceLocation) extends KindedAst.Expression

    case class ArrayLit(elms: List[KindedAst.Expression], tpe: Type.Var, loc: SourceLocation) extends KindedAst.Expression

    case class ArrayNew(elm: KindedAst.Expression, len: KindedAst.Expression, tpe: Type.Var, loc: SourceLocation) extends KindedAst.Expression

    case class ArrayLoad(base: KindedAst.Expression, index: KindedAst.Expression, tpe: Type.Var, loc: SourceLocation) extends KindedAst.Expression

    case class ArrayStore(base: KindedAst.Expression, index: KindedAst.Expression, elm: KindedAst.Expression, loc: SourceLocation) extends KindedAst.Expression

    case class ArrayLength(base: KindedAst.Expression, loc: SourceLocation) extends KindedAst.Expression

    case class ArraySlice(base: KindedAst.Expression, beginIndex: KindedAst.Expression, endIndex: KindedAst.Expression, loc: SourceLocation) extends KindedAst.Expression

    case class Ref(exp: KindedAst.Expression, loc: SourceLocation) extends KindedAst.Expression

    case class Deref(exp: KindedAst.Expression, tpe: Type.Var, loc: SourceLocation) extends KindedAst.Expression

    case class Assign(exp1: KindedAst.Expression, exp2: KindedAst.Expression, loc: SourceLocation) extends KindedAst.Expression

    case class Existential(fparam: KindedAst.FormalParam, exp: KindedAst.Expression, loc: SourceLocation) extends KindedAst.Expression

    case class Universal(fparam: KindedAst.FormalParam, exp: KindedAst.Expression, loc: SourceLocation) extends KindedAst.Expression

    case class Ascribe(exp: KindedAst.Expression, expectedType: Option[Type], expectedEff: Option[Type], tpe: Type.Var, loc: SourceLocation) extends KindedAst.Expression

    case class Cast(exp: KindedAst.Expression, declaredType: Option[Type], declaredEff: Option[Type], tpe: Type.Var, loc: SourceLocation) extends KindedAst.Expression

    case class TryCatch(exp: KindedAst.Expression, rules: List[KindedAst.CatchRule], loc: SourceLocation) extends KindedAst.Expression

    case class InvokeConstructor(constructor: Constructor[_], args: List[KindedAst.Expression], loc: SourceLocation) extends KindedAst.Expression

    case class InvokeMethod(method: Method, exp: KindedAst.Expression, args: List[KindedAst.Expression], loc: SourceLocation) extends KindedAst.Expression

    case class InvokeStaticMethod(method: Method, args: List[KindedAst.Expression], loc: SourceLocation) extends KindedAst.Expression

    case class GetField(field: Field, exp: KindedAst.Expression, loc: SourceLocation) extends KindedAst.Expression

    case class PutField(field: Field, exp1: KindedAst.Expression, exp2: KindedAst.Expression, loc: SourceLocation) extends KindedAst.Expression

    case class GetStaticField(field: Field, loc: SourceLocation) extends KindedAst.Expression

    case class PutStaticField(field: Field, exp: KindedAst.Expression, loc: SourceLocation) extends KindedAst.Expression

    case class NewChannel(exp: KindedAst.Expression, tpe: Type, loc: SourceLocation) extends KindedAst.Expression

    case class GetChannel(exp: KindedAst.Expression, tpe: Type.Var, loc: SourceLocation) extends KindedAst.Expression

    case class PutChannel(exp1: KindedAst.Expression, exp2: KindedAst.Expression, tpe: Type.Var, loc: SourceLocation) extends KindedAst.Expression

    case class SelectChannel(rules: List[KindedAst.SelectChannelRule], default: Option[KindedAst.Expression], tpe: Type.Var, loc: SourceLocation) extends KindedAst.Expression

    case class Spawn(exp: KindedAst.Expression, loc: SourceLocation) extends KindedAst.Expression

    case class Lazy(exp: KindedAst.Expression, loc: SourceLocation) extends KindedAst.Expression

    case class Force(exp: KindedAst.Expression, tpe: Type.Var, loc: SourceLocation) extends KindedAst.Expression

    case class FixpointConstraintSet(cs: List[KindedAst.Constraint], tpe: Type.Var, loc: SourceLocation) extends KindedAst.Expression

    case class FixpointCompose(exp1: KindedAst.Expression, exp2: KindedAst.Expression, loc: SourceLocation) extends KindedAst.Expression

    case class FixpointSolve(exp: KindedAst.Expression, loc: SourceLocation) extends KindedAst.Expression

    case class FixpointProject(pred: Name.Pred, exp: KindedAst.Expression, tpe: Type.Var, loc: SourceLocation) extends KindedAst.Expression

    case class FixpointEntails(exp1: KindedAst.Expression, exp2: KindedAst.Expression, loc: SourceLocation) extends KindedAst.Expression

    case class FixpointFold(pred: Name.Pred, init: KindedAst.Expression, f: KindedAst.Expression, constraints: KindedAst.Expression, tpe: Type.Var, loc: SourceLocation) extends KindedAst.Expression

  }

  sealed trait Pattern {
    def loc: SourceLocation
  }

  object Pattern {

    case class Wild(tvar: ast.Type.Var, loc: SourceLocation) extends KindedAst.Pattern

    case class Var(sym: Symbol.VarSym, tvar: ast.Type.Var, loc: SourceLocation) extends KindedAst.Pattern

    case class Unit(loc: SourceLocation) extends KindedAst.Pattern

    case class True(loc: SourceLocation) extends KindedAst.Pattern

    case class False(loc: SourceLocation) extends KindedAst.Pattern

    case class Char(lit: scala.Char, loc: SourceLocation) extends KindedAst.Pattern

    case class Float32(lit: scala.Float, loc: SourceLocation) extends KindedAst.Pattern

    case class Float64(lit: scala.Double, loc: SourceLocation) extends KindedAst.Pattern

    case class Int8(lit: scala.Byte, loc: SourceLocation) extends KindedAst.Pattern

    case class Int16(lit: scala.Short, loc: SourceLocation) extends KindedAst.Pattern

    case class Int32(lit: scala.Int, loc: SourceLocation) extends KindedAst.Pattern

    case class Int64(lit: scala.Long, loc: SourceLocation) extends KindedAst.Pattern

    case class BigInt(lit: java.math.BigInteger, loc: SourceLocation) extends KindedAst.Pattern

    case class Str(lit: java.lang.String, loc: SourceLocation) extends KindedAst.Pattern

    case class Tag(sym: Symbol.EnumSym, tag: Name.Tag, pat: KindedAst.Pattern, tvar: ast.Type.Var, loc: SourceLocation) extends KindedAst.Pattern

    case class Tuple(elms: List[KindedAst.Pattern], loc: SourceLocation) extends KindedAst.Pattern

    case class Array(elms: List[KindedAst.Pattern], tvar: ast.Type.Var, loc: SourceLocation) extends KindedAst.Pattern

    case class ArrayTailSpread(elms: scala.List[KindedAst.Pattern], sym: Symbol.VarSym, tvar: ast.Type.Var, loc: SourceLocation) extends KindedAst.Pattern

    case class ArrayHeadSpread(sym: Symbol.VarSym, elms: scala.List[KindedAst.Pattern], tvar: ast.Type.Var, loc: SourceLocation) extends KindedAst.Pattern

  }

  sealed trait ChoicePattern {
    def loc: SourceLocation
  }

  object ChoicePattern {

    case class Wild(loc: SourceLocation) extends ChoicePattern

    case class Absent(loc: SourceLocation) extends ChoicePattern

    case class Present(sym: Symbol.VarSym, tvar: ast.Type.Var, loc: SourceLocation) extends ChoicePattern

  }

  sealed trait Predicate

  object Predicate {

    sealed trait Head extends KindedAst.Predicate

    object Head {

      case class Atom(pred: Name.Pred, den: Denotation, terms: List[KindedAst.Expression], tvar: ast.Type.Var, loc: SourceLocation) extends KindedAst.Predicate.Head

      case class Union(exp: KindedAst.Expression, tvar: ast.Type.Var, loc: SourceLocation) extends KindedAst.Predicate.Head

    }

    sealed trait Body extends KindedAst.Predicate

    object Body {

      case class Atom(pred: Name.Pred, den: Denotation, polarity: Ast.Polarity, terms: List[KindedAst.Pattern], tvar: ast.Type.Var, loc: SourceLocation) extends KindedAst.Predicate.Body

      case class Guard(exp: KindedAst.Expression, loc: SourceLocation) extends KindedAst.Predicate.Body

    }

  }

  case class Annotation(name: Ast.Annotation, exps: List[KindedAst.Expression], loc: SourceLocation)

  case class Attribute(ident: Name.Ident, tpe: Type, loc: SourceLocation)

  case class Case(enum: Name.Ident, tag: Name.Tag, tpeDeprecated: Type, sc: Scheme)

  case class Constraint(cparams: List[KindedAst.ConstraintParam], head: KindedAst.Predicate.Head, body: List[KindedAst.Predicate.Body], loc: SourceLocation)

  sealed trait ConstraintParam

  object ConstraintParam {

    case class HeadParam(sym: Symbol.VarSym, tpe: Type.Var, loc: SourceLocation) extends KindedAst.ConstraintParam

    case class RuleParam(sym: Symbol.VarSym, tpe: Type.Var, loc: SourceLocation) extends KindedAst.ConstraintParam

  }

  case class FormalParam(sym: Symbol.VarSym, mod: Ast.Modifiers, tpe: Type, loc: SourceLocation)

  case class CatchRule(sym: Symbol.VarSym, clazz: java.lang.Class[_], exp: KindedAst.Expression)

  case class ChoiceRule(pat: List[KindedAst.ChoicePattern], exp: KindedAst.Expression)

  case class MatchRule(pat: KindedAst.Pattern, guard: KindedAst.Expression, exp: KindedAst.Expression)

  case class SelectChannelRule(sym: Symbol.VarSym, chan: KindedAst.Expression, exp: KindedAst.Expression)

  case class TypeParam(name: Name.Ident, tpe: Type.Var, kind: Kind, loc: SourceLocation)
}

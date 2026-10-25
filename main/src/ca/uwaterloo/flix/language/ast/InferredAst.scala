/*
 * Copyright 2026 Magnus Madsen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */

package ca.uwaterloo.flix.language.ast

import ca.uwaterloo.flix.language.CompilationMessage
import ca.uwaterloo.flix.language.ast.jvm.JavaField
import ca.uwaterloo.flix.language.ast.shared.*
import ca.uwaterloo.flix.language.ast.shared.SymUse.*
import ca.uwaterloo.flix.util.collection.Nel

import java.lang.constant.ClassDesc

/**
  * The abstract syntax tree between constraint generation and type reconstruction.
  *
  * Every expression carries its inferred type and effect. These may contain type variables that
  * are only resolved once the type constraints of the enclosing definition have been solved.
  *
  * The tree only contains expressions, i.e. it has no declarations.
  */
object InferredAst {

  sealed trait Expr {
    def tpe: Type

    def eff: Type

    def loc: SourceLocation
  }

  object Expr {

    case class Cst(cst: Constant, tpe: Type, loc: SourceLocation) extends Expr {
      def eff: Type = Type.Pure
    }

    case class Var(sym: Symbol.VarSym, tpe: Type, loc: SourceLocation) extends Expr {
      def eff: Type = Type.Pure
    }

    case class Hole(sym: Symbol.HoleSym, scp: LocalScope, tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class HoleWithExp(exp: Expr, scp: LocalScope, tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class OpenAs(symUse: RestrictableEnumSymUse, exp: Expr, tpe: Type, loc: SourceLocation) extends Expr {
      def eff: Type = exp.eff
    }

    case class Use(sym: Symbol, alias: Name.Ident, exp: Expr, loc: SourceLocation) extends Expr {
      def tpe: Type = exp.tpe

      def eff: Type = exp.eff
    }

    case class Lambda(fparam: FormalParam, exp: Expr, tpe: Type, loc: SourceLocation) extends Expr {
      def eff: Type = Type.Pure
    }

    case class ApplyClo(exp1: Expr, exp2: Expr, tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class ApplyDef(symUse: DefSymUse, exps: List[Expr], targs: List[Type], itpe: Type, tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class ApplyLocalDef(symUse: LocalDefSymUse, exps: List[Expr], arrowTpe: Type, tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class ApplyOp(symUse: OpSymUse, exps: List[Expr], tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class ApplySig(symUse: SigSymUse, exps: List[Expr], targ: Type, targs: List[Type], itpe: Type, tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class Unary(sop: SemanticOp.UnaryOp, exp: Expr, tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class Binary(sop: SemanticOp.BinaryOp, exp1: Expr, exp2: Expr, tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class Let(bnd: Binder, exp1: Expr, exp2: Expr, tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class LocalDef(ann: Annotations, bnd: Binder, fparams: Nel[FormalParam], exp1: Expr, exp2: Expr, tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class Region(bnd: Binder, regSym: Symbol.RegionSym, exp: Expr, tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class IfThenElse(exp1: Expr, exp2: Expr, exp3: Expr, tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class Stm(exps: List[Expr], exp: Expr, tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class Discard(exp: Expr, eff: Type, loc: SourceLocation) extends Expr {
      def tpe: Type = Type.mkUnit(loc)
    }

    case class Match(exp: Expr, rules: List[MatchRule], tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class RestrictableChoose(star: Boolean, exp: Expr, rules: List[RestrictableChooseRule], tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class ExtMatch(exp: Expr, rules: List[ExtMatchRule], tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class Tag(symUse: CaseSymUse, exps: List[Expr], tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class RestrictableTag(symUse: RestrictableCaseSymUse, exps: List[Expr], tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class ExtTag(label: Name.Label, exps: List[Expr], tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class Tuple(exps: List[Expr], tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class RecordSelect(exp: Expr, label: Name.Label, tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class RecordExtend(label: Name.Label, exp1: Expr, exp2: Expr, tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class RecordRestrict(label: Name.Label, exp: Expr, tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class ArrayLit(exps: List[Expr], exp: Expr, tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class ArrayNew(exp1: Expr, exp2: Expr, exp3: Expr, tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class ArrayLoad(exp1: Expr, exp2: Expr, tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class ArrayLength(exp: Expr, eff: Type, loc: SourceLocation) extends Expr {
      def tpe: Type = Type.Int32
    }

    case class ArrayStore(exp1: Expr, exp2: Expr, exp3: Expr, eff: Type, loc: SourceLocation) extends Expr {
      def tpe: Type = Type.Unit
    }

    case class StructNew(sym: Symbol.StructSym, fields: List[(StructFieldSymUse, Expr)], region: Option[Expr], tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class StructGet(exp: Expr, symUse: StructFieldSymUse, tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class StructPut(exp1: Expr, symUse: StructFieldSymUse, exp2: Expr, tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class VectorLit(exps: List[Expr], tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class VectorLoad(exp1: Expr, exp2: Expr, tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class VectorLength(exp: Expr, loc: SourceLocation) extends Expr {
      def eff: Type = exp.eff

      def tpe: Type = Type.Int32
    }

    case class Ascribe(exp: Expr, expectedType: Option[Type], expectedEff: Option[Type], tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class InstanceOf(exp: Expr, clazz: ClassDesc, loc: SourceLocation) extends Expr {
      def eff: Type = exp.eff

      def tpe: Type = Type.Bool
    }

    case class CheckedCast(cast: CheckedCastType, exp: Expr, tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class UncheckedCast(exp: Expr, declaredType: Option[Type], declaredEff: Option[Type], tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class Unsafe(exp: Expr, runEff: Type, asEff: Option[Type], tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class TryCatch(exp: Expr, rules: List[CatchRule], tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class Throw(exp: Expr, tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class Handler(symUse: EffSymUse, rules: List[HandlerRule], bodyType: Type, bodyEff: Type, handledEff: Type, tpe: Type, loc: SourceLocation) extends Expr {
      def eff: Type = Type.Pure
    }

    case class RunWith(exp1: Expr, exp2: Expr, tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    /**
      * A Java constructor call where `jvar` resolves to the constructor once the type constraints have been solved.
      */
    case class InvokeConstructor(exps: List[Expr], jvar: Type.Var, tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    /**
      * A Java super constructor call where `jvar` resolves to the constructor once the type constraints have been solved.
      */
    case class InvokeSuperConstructor(exps: List[Expr], jvar: Type.Var, tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    /**
      * A Java method call where `jvar` resolves to the method once the type constraints have been solved.
      */
    case class InvokeMethod(exp: Expr, exps: List[Expr], jvar: Type.Var, tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    /**
      * A Java super method call where `jvar` resolves to the method once the type constraints have been solved.
      */
    case class InvokeSuperMethod(exps: List[Expr], jvar: Type.Var, tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    /**
      * A Java static method call where `jvar` resolves to the method once the type constraints have been solved.
      */
    case class InvokeStaticMethod(exps: List[Expr], jvar: Type.Var, tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    /**
      * A Java field read where `jvar` resolves to the field once the type constraints have been solved.
      */
    case class GetField(exp: Expr, jvar: Type.Var, tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class PutField(field: JavaField, exp1: Expr, exp2: Expr, tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class GetStaticField(field: JavaField, tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class PutStaticField(field: JavaField, exp: Expr, tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class NewObject(sym: Symbol.AnonClassSym, clazz: JClass, tpe: Type, eff: Type, constructors: List[JvmConstructor], methods: List[JvmMethod], loc: SourceLocation) extends Expr

    case class NewChannel(exp: Expr, tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class GetChannel(exp: Expr, tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class PutChannel(exp1: Expr, exp2: Expr, tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class SelectChannel(rules: List[SelectChannelRule], default: Option[Expr], tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class Spawn(exp1: Expr, exp2: Expr, tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class ParYield(frags: List[ParYieldFragment], exp: Expr, tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class Lazy(exp: Expr, tpe: Type, loc: SourceLocation) extends Expr {
      def eff: Type = Type.Pure
    }

    case class Force(exp: Expr, tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class FixpointConstraintSet(cs: List[Constraint], tpe: Type, loc: SourceLocation) extends Expr {
      def eff: Type = Type.Pure
    }

    case class FixpointLambda(pparams: List[PredicateParam], exp: Expr, tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class FixpointMerge(exp1: Expr, exp2: Expr, tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class FixpointQueryWithProvenance(exps: List[Expr], select: Predicate.Head, withh: List[Name.Pred], tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class FixpointQueryWithSelect(exps: List[Expr], queryExp: Expr, selects: List[Expr], from: List[Predicate.Body], where: List[Expr], pred: Name.Pred, tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class FixpointSolveWithProject(exps: List[Expr], optPreds: Option[List[Name.Pred]], mode: SolveMode, tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class FixpointInjectInto(exps: List[Expr], predsAndArities: List[PredicateAndArity], tpe: Type, eff: Type, loc: SourceLocation) extends Expr

    case class Error(m: CompilationMessage, tpe: Type, eff: Type) extends Expr {
      override def loc: SourceLocation = m.loc
    }

  }

  sealed trait Pattern {
    def tpe: Type

    def loc: SourceLocation
  }

  object Pattern {

    case class Wild(tpe: Type, loc: SourceLocation) extends Pattern

    case class Var(bnd: Binder, tpe: Type, loc: SourceLocation) extends Pattern

    case class Cst(cst: Constant, tpe: Type, loc: SourceLocation) extends Pattern

    case class Tag(symUse: CaseSymUse, pats: List[Pattern], tpe: Type, loc: SourceLocation) extends Pattern

    case class Tuple(pats: Nel[Pattern], tpe: Type, loc: SourceLocation) extends Pattern

    case class Record(pats: List[Record.RecordLabelPattern], pat: Pattern, tpe: Type, loc: SourceLocation) extends Pattern

    object Record {
      case class RecordLabelPattern(label: Name.Label, pat: Pattern, tpe: Type, loc: SourceLocation)
    }

    case class Error(tpe: Type, loc: SourceLocation) extends Pattern
  }

  sealed trait RestrictableChoosePattern {
    def tpe: Type

    def loc: SourceLocation
  }

  object RestrictableChoosePattern {

    sealed trait VarOrWild {
      def tpe: Type
    }

    case class Wild(tpe: Type, loc: SourceLocation) extends VarOrWild

    case class Var(bnd: Binder, tpe: Type, loc: SourceLocation) extends VarOrWild

    case class Tag(symUse: RestrictableCaseSymUse, pat: List[VarOrWild], tpe: Type, loc: SourceLocation) extends RestrictableChoosePattern

    case class Error(tpe: Type, loc: SourceLocation) extends VarOrWild with RestrictableChoosePattern

  }

  sealed trait ExtPattern {
    def loc: SourceLocation
  }

  object ExtPattern {

    case class Default(loc: SourceLocation) extends ExtPattern

    case class Tag(label: Name.Label, pats: List[ExtTagPattern], loc: SourceLocation) extends ExtPattern

    case class Error(loc: SourceLocation) extends ExtPattern

  }

  sealed trait ExtTagPattern {
    def tpe: Type

    def loc: SourceLocation
  }

  object ExtTagPattern {

    case class Wild(tpe: Type, loc: SourceLocation) extends ExtTagPattern

    case class Var(bnd: Binder, tpe: Type, loc: SourceLocation) extends ExtTagPattern

    case class Unit(tpe: Type, loc: SourceLocation) extends ExtTagPattern

    case class Error(tpe: Type, loc: SourceLocation) extends ExtTagPattern

  }

  sealed trait Predicate {
    def loc: SourceLocation
  }

  object Predicate {

    sealed trait Head extends Predicate

    object Head {

      case class Atom(pred: Name.Pred, den: Denotation, terms: List[Expr], tpe: Type, loc: SourceLocation) extends Predicate.Head

    }

    sealed trait Body extends Predicate

    object Body {

      case class Atom(pred: Name.Pred, den: Denotation, polarity: Polarity, fixity: Fixity, terms: List[Pattern], tpe: Type, loc: SourceLocation) extends Predicate.Body

      case class Functional(outBnds: List[Binder], exp: Expr, loc: SourceLocation) extends Predicate.Body

      case class Guard(exp: Expr, loc: SourceLocation) extends Predicate.Body

    }

  }

  case class Binder(sym: Symbol.VarSym, tpe: Type)

  case class Constraint(cparams: List[ConstraintParam], head: Predicate.Head, body: List[Predicate.Body], loc: SourceLocation)

  case class ConstraintParam(bnd: Binder, tpe: Type, loc: SourceLocation)

  case class FormalParam(bnd: Binder, tpe: Type, src: TypeSource, loc: SourceLocation)

  case class PredicateParam(pred: Name.Pred, tpe: Type, loc: SourceLocation)

  case class JvmConstructor(exp: Expr, retTpe: Type, eff: Type, loc: SourceLocation)

  case class JvmMethod(ann: List[JvmAnnotation], ident: Name.Ident, fparams: Nel[FormalParam], exp: Expr, retTpe: Type, eff: Type, loc: SourceLocation)

  case class CatchRule(bnd: Binder, clazz: ClassDesc, exp: Expr, loc: SourceLocation)

  case class HandlerRule(symUse: OpSymUse, fparams: Nel[FormalParam], exp: Expr, loc: SourceLocation)

  case class RestrictableChooseRule(pat: RestrictableChoosePattern, exp: Expr)

  case class ExtMatchRule(pat: ExtPattern, exp: Expr, loc: SourceLocation)

  case class MatchRule(pat: Pattern, guard: Option[Expr], exp: Expr, loc: SourceLocation)

  case class SelectChannelRule(bnd: Binder, chan: Expr, exp: Expr, loc: SourceLocation)

  case class ParYieldFragment(pat: Pattern, exp: Expr, loc: SourceLocation)

}

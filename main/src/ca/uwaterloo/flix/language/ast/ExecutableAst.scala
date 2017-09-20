/*
 * Copyright 2015-2016 Magnus Madsen
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

import java.lang.reflect.{Constructor, Field, Method}
import java.util.concurrent.atomic.{AtomicInteger, AtomicLong}
import ca.uwaterloo.flix.language.phase.CodegenHelper._
import ca.uwaterloo.flix.language.ast.Symbol.EnumSym
import ca.uwaterloo.flix.language.phase.CodegenHelper.{EnumTypeInterfaceName, FlixClassName, QualName, TupleClassName}
import ca.uwaterloo.flix.runtime.InvocationTarget

sealed trait ExecutableAst

object ExecutableAst {

  // TODO: Get rid of most uses of array.

  case class Root(defs: Map[Symbol.DefnSym, ExecutableAst.Def],
                  enums: Map[Symbol.EnumSym, ExecutableAst.Enum],
                  lattices: Map[Type, ExecutableAst.Lattice],
                  tables: Map[Symbol.TableSym, ExecutableAst.Table],
                  indexes: Map[Symbol.TableSym, ExecutableAst.Index],
                  constraints: List[ExecutableAst.Constraint],
                  properties: List[ExecutableAst.Property],
                  specialOps: Map[SpecialOperator, Map[Type, Symbol.DefnSym]],
                  reachable: Set[Symbol.DefnSym],
                  byteCodes: ByteCodes,
                  time: Time,
                  dependenciesOf: Map[Symbol.TableSym, Set[(Constraint, ExecutableAst.Predicate.Body.Positive)]]) extends ExecutableAst

  case class ByteCodes(enumInterfaceByteCodes:  Map[EnumSym, (EnumTypeInterfaceName, Array[Byte])],
                       enumClassByteCodes: Map[EnumSym, (Map[(String, Type), Array[Byte]], Map[String, Array[Byte]])],
                       functionalInterfaceByteCodes: Map[Type, (FlixClassName, Array[Byte])],
                       classByteCodes: Map[FlixClassName, Array[Byte]],
                       tupleByteCode: Map[List[WrappedType], (Array[Byte], Array[Byte])],
                       ETFusionByteCode: Map[EnumSym, Map[String, Map[List[WrappedType], Array[Byte]]]])


  case class Constraint(cparams: List[ConstraintParam], head: Predicate.Head, body: List[Predicate.Body]) extends ExecutableAst {

    /**
      * Returns the arity of the constraint.
      *
      * The arity of a constraint is the number of constraint parameters (i.e. variables in the constraint).
      * Not to be confused with the number of predicates or terms.
      */
    val arity: Int = cparams.length

    /**
      * Returns `true` if the constraint is a fact.
      */
    val isFact: Boolean = body.isEmpty

    /**
      * Returns `true` if the constraint is a rule.
      */
    val isRule: Boolean = body.nonEmpty

    /**
      * Returns the tables referenced by the body predicates of the constraint.
      */
    val tables: List[ExecutableAst.Predicate.Body] = body.collect {
      case p: ExecutableAst.Predicate.Body.Positive => p
      case p: ExecutableAst.Predicate.Body.Negative => p
    }

    /**
      * Returns the filter predicates in the body of the constraint.
      */
    val filters: Array[ExecutableAst.Predicate.Body.Filter] = body.collect {
      case p: ExecutableAst.Predicate.Body.Filter => p
    }.toArray

    /**
      * Returns the loop predicates in the body of the constraint.
      */
    val loops: List[ExecutableAst.Predicate.Body.Loop] = body.collect {
      case p: ExecutableAst.Predicate.Body.Loop => p
    }

    /**
      * Records the number of times this rule has been evaluated.
      */
    val hits = new AtomicInteger()

    /**
      * Records the amount of time spent evaluating this rule.
      */
    val time = new AtomicLong()

  }

  case class Def(ann: Ast.Annotations, sym: Symbol.DefnSym, formals: Array[ExecutableAst.FormalParam], exp: ExecutableAst.Expression, isSynthetic: Boolean, tpe: Type, loc: SourceLocation) extends ExecutableAst {
    /**
      * Pointer to generated code.
      */
    var method: Method = null
  }

  case class Enum(sym: Symbol.EnumSym, cases: Map[String, ExecutableAst.Case], loc: SourceLocation) extends ExecutableAst

  case class Lattice(tpe: Type, bot: Symbol.DefnSym, top: Symbol.DefnSym, equ: Symbol.DefnSym, leq: Symbol.DefnSym, lub: Symbol.DefnSym, glb: Symbol.DefnSym, loc: SourceLocation) extends ExecutableAst

  case class Index(name: Symbol.TableSym, indexes: Seq[Seq[Name.Ident]], loc: SourceLocation) extends ExecutableAst

  case class Property(law: Symbol.DefnSym, defn: Symbol.DefnSym, exp: ExecutableAst.Expression) extends ExecutableAst {
    def loc: SourceLocation = defn.loc
  }

  sealed trait Table extends ExecutableAst

  object Table {

    case class Relation(sym: Symbol.TableSym, attributes: Array[ExecutableAst.Attribute], loc: SourceLocation) extends ExecutableAst.Table

    case class Lattice(sym: Symbol.TableSym, keys: Array[ExecutableAst.Attribute], value: ExecutableAst.Attribute, loc: SourceLocation) extends ExecutableAst.Table

  }

  sealed trait Expression extends ExecutableAst {
    def tpe: Type

    def loc: SourceLocation
  }

  object Expression {

    case object Unit extends ExecutableAst.Expression {
      final val tpe = Type.Unit
      final val loc = SourceLocation.Unknown
    }

    case object True extends ExecutableAst.Expression {
      final val tpe = Type.Bool
      final val loc = SourceLocation.Unknown
    }

    case object False extends ExecutableAst.Expression {
      final val tpe = Type.Bool
      final val loc = SourceLocation.Unknown
    }

    case class Char(lit: scala.Char) extends ExecutableAst.Expression {
      final val tpe = Type.Char
      final val loc = SourceLocation.Unknown
    }

    case class Float32(lit: scala.Float) extends ExecutableAst.Expression {
      final val tpe = Type.Float32
      final val loc = SourceLocation.Unknown
    }

    case class Float64(lit: scala.Double) extends ExecutableAst.Expression {
      final val tpe = Type.Float64
      final val loc = SourceLocation.Unknown
    }

    case class Int8(lit: scala.Byte) extends ExecutableAst.Expression {
      final val tpe = Type.Int8
      final val loc = SourceLocation.Unknown
    }

    case class Int16(lit: scala.Short) extends ExecutableAst.Expression {
      final val tpe = Type.Int16
      final val loc = SourceLocation.Unknown
    }

    case class Int32(lit: scala.Int) extends ExecutableAst.Expression {
      final val tpe = Type.Int32
      final val loc = SourceLocation.Unknown
    }

    case class Int64(lit: scala.Long) extends ExecutableAst.Expression {
      final val tpe = Type.Int64
      final val loc = SourceLocation.Unknown
    }

    case class BigInt(lit: java.math.BigInteger) extends ExecutableAst.Expression {
      final val tpe = Type.BigInt
      final val loc = SourceLocation.Unknown
    }

    case class Str(lit: java.lang.String) extends ExecutableAst.Expression {
      final val tpe = Type.Str
      final val loc = SourceLocation.Unknown
    }

    case class Var(sym: Symbol.VarSym, tpe: Type, loc: SourceLocation) extends ExecutableAst.Expression

    // TODO: Get rid of the fnType here.
    case class Closure(sym: Symbol.DefnSym, freeVars: List[FreeVar], fnType: Type, tpe: Type, loc: SourceLocation) extends ExecutableAst.Expression

    case class ApplyClo(exp: ExecutableAst.Expression, args: List[ExecutableAst.Expression], tpe: Type, loc: SourceLocation) extends ExecutableAst.Expression

    case class ApplyDef(sym: Symbol.DefnSym, args: List[ExecutableAst.Expression], tpe: Type, loc: SourceLocation) extends ExecutableAst.Expression

    case class ApplyCloTail(exp: ExecutableAst.Expression, args: List[ExecutableAst.Expression], tpe: Type, loc: SourceLocation) extends ExecutableAst.Expression

    case class ApplyDefTail(sym: Symbol.DefnSym, args: List[ExecutableAst.Expression], tpe: Type, loc: SourceLocation) extends ExecutableAst.Expression

    case class ApplySelfTail(name: Symbol.DefnSym, formals: List[ExecutableAst.FormalParam], actuals: List[ExecutableAst.Expression], tpe: Type, loc: SourceLocation) extends ExecutableAst.Expression

    case class ApplyHook(hook: Ast.Hook, args: List[ExecutableAst.Expression], tpe: Type, loc: SourceLocation) extends ExecutableAst.Expression

    case class Unary(sop: SemanticOperator, op: UnaryOperator, exp: ExecutableAst.Expression, tpe: Type, loc: SourceLocation) extends ExecutableAst.Expression

    case class Binary(sop: SemanticOperator, op: BinaryOperator, exp1: ExecutableAst.Expression, exp2: ExecutableAst.Expression, tpe: Type, loc: SourceLocation) extends ExecutableAst.Expression

    case class IfThenElse(exp1: ExecutableAst.Expression, exp2: ExecutableAst.Expression, exp3: ExecutableAst.Expression, tpe: Type, loc: SourceLocation) extends ExecutableAst.Expression

    case class Branch(exp: ExecutableAst.Expression, branches: Map[Symbol.LabelSym, ExecutableAst.Expression], tpe: Type, loc: SourceLocation) extends ExecutableAst.Expression

    case class JumpTo(sym: Symbol.LabelSym, tpe: Type, loc: SourceLocation) extends ExecutableAst.Expression

    case class Let(sym: Symbol.VarSym, exp1: ExecutableAst.Expression, exp2: ExecutableAst.Expression, tpe: Type, loc: SourceLocation) extends ExecutableAst.Expression

    // NB: After lambda lifting and closure conversion `exp1` is guaranteed to be a MkClosureDef.
    case class LetRec(sym: Symbol.VarSym, exp1: ExecutableAst.Expression, exp2: ExecutableAst.Expression, tpe: Type, loc: SourceLocation) extends ExecutableAst.Expression

    case class Is(sym: Symbol.EnumSym, tag: String, exp: ExecutableAst.Expression, loc: SourceLocation) extends ExecutableAst.Expression {
      final val tpe: Type = Type.Bool
    }

    case class Tag(sym: Symbol.EnumSym, tag: String, exp: ExecutableAst.Expression, tpe: Type, loc: SourceLocation) extends ExecutableAst.Expression

    case class Untag(sym: Symbol.EnumSym, tag: String, exp: ExecutableAst.Expression, tpe: Type, loc: SourceLocation) extends ExecutableAst.Expression

    case class Index(base: ExecutableAst.Expression, offset: scala.Int, tpe: Type, loc: SourceLocation) extends ExecutableAst.Expression

    case class Tuple(elms: Array[ExecutableAst.Expression], tpe: Type, loc: SourceLocation) extends ExecutableAst.Expression

    case class Ref(exp: ExecutableAst.Expression, tpe: Type, loc: SourceLocation) extends ExecutableAst.Expression

    case class Deref(exp: ExecutableAst.Expression, tpe: Type, loc: SourceLocation) extends ExecutableAst.Expression

    case class Assign(exp1: ExecutableAst.Expression, exp2: ExecutableAst.Expression, tpe: Type, loc: SourceLocation) extends ExecutableAst.Expression

    case class Existential(fparam: ExecutableAst.FormalParam, exp: ExecutableAst.Expression, loc: SourceLocation) extends ExecutableAst.Expression {
      def tpe: Type = Type.Bool
    }

    case class Universal(fparam: ExecutableAst.FormalParam, exp: ExecutableAst.Expression, loc: SourceLocation) extends ExecutableAst.Expression {
      def tpe: Type = Type.Bool
    }

    case class NativeConstructor(constructor: Constructor[_], args: List[ExecutableAst.Expression], tpe: Type, loc: SourceLocation) extends ExecutableAst.Expression

    case class NativeField(field: Field, tpe: Type, loc: SourceLocation) extends ExecutableAst.Expression

    case class NativeMethod(method: Method, args: List[ExecutableAst.Expression], tpe: Type, loc: SourceLocation) extends ExecutableAst.Expression

    case class UserError(tpe: Type, loc: SourceLocation) extends ExecutableAst.Expression

    case class MatchError(tpe: Type, loc: SourceLocation) extends ExecutableAst.Expression

    case class SwitchError(tpe: Type, loc: SourceLocation) extends ExecutableAst.Expression

  }

  sealed trait Pattern extends ExecutableAst

  object Pattern {

    case class Wild(tpe: Type, loc: SourceLocation) extends ExecutableAst.Pattern

    case class Var(sym: Symbol.VarSym, tpe: Type, loc: SourceLocation) extends ExecutableAst.Pattern

    case class Unit(loc: SourceLocation) extends ExecutableAst.Pattern

    case class True(loc: SourceLocation) extends ExecutableAst.Pattern

    case class False(loc: SourceLocation) extends ExecutableAst.Pattern

    case class Char(lit: scala.Char, loc: SourceLocation) extends ExecutableAst.Pattern

    case class Float32(lit: scala.Float, loc: SourceLocation) extends ExecutableAst.Pattern

    case class Float64(lit: scala.Double, loc: SourceLocation) extends ExecutableAst.Pattern

    case class Int8(lit: scala.Byte, loc: SourceLocation) extends ExecutableAst.Pattern

    case class Int16(lit: scala.Short, loc: SourceLocation) extends ExecutableAst.Pattern

    case class Int32(lit: scala.Int, loc: SourceLocation) extends ExecutableAst.Pattern

    case class Int64(lit: scala.Long, loc: SourceLocation) extends ExecutableAst.Pattern

    case class BigInt(lit: java.math.BigInteger, loc: SourceLocation) extends ExecutableAst.Pattern

    case class Str(lit: java.lang.String, loc: SourceLocation) extends ExecutableAst.Pattern

    case class Tag(sym: Symbol.EnumSym, tag: String, pat: ExecutableAst.Pattern, tpe: Type, loc: SourceLocation) extends ExecutableAst.Pattern

    case class Tuple(elms: List[ExecutableAst.Pattern], tpe: Type, loc: SourceLocation) extends ExecutableAst.Pattern

  }

  sealed trait Predicate extends ExecutableAst {
    def loc: SourceLocation
  }

  object Predicate {

    sealed trait Head extends ExecutableAst.Predicate

    object Head {

      case class True(loc: SourceLocation) extends ExecutableAst.Predicate.Head

      case class False(loc: SourceLocation) extends ExecutableAst.Predicate.Head

      case class Positive(sym: Symbol.TableSym, terms: Array[ExecutableAst.Term.Head], loc: SourceLocation) extends ExecutableAst.Predicate.Head {
        val arity: Int = terms.length
      }

      case class Negative(sym: Symbol.TableSym, terms: Array[ExecutableAst.Term.Head], loc: SourceLocation) extends ExecutableAst.Predicate.Head {
        val arity: Int = terms.length
      }

    }

    sealed trait Body extends ExecutableAst.Predicate {
      val freeVars: Set[String]
    }

    object Body {

      // TODO: Remove freeVars

      case class Positive(sym: Symbol.TableSym, terms: Array[ExecutableAst.Term.Body], index2sym: Array[Symbol.VarSym], freeVars: Set[String], loc: SourceLocation) extends ExecutableAst.Predicate.Body {
        val arity: Int = terms.length
      }

      case class Negative(sym: Symbol.TableSym, terms: Array[ExecutableAst.Term.Body], index2sym: Array[Symbol.VarSym], freeVars: Set[String], loc: SourceLocation) extends ExecutableAst.Predicate.Body {
        val arity: Int = terms.length
      }

      case class Filter(sym: Symbol.DefnSym, terms: Array[ExecutableAst.Term.Body], freeVars: Set[String], loc: SourceLocation) extends ExecutableAst.Predicate.Body {

        /**
          * A reference to the invocation target of this filter function. Initially `null`.
          */
        var target: InvocationTarget = _

      }

      case class Loop(sym: Symbol.VarSym, term: ExecutableAst.Term.Head, freeVars: Set[String], loc: SourceLocation) extends ExecutableAst.Predicate.Body

    }

  }

  object Term {

    sealed trait Head extends ExecutableAst

    object Head {

      case class Var(sym: Symbol.VarSym, tpe: Type, loc: SourceLocation) extends ExecutableAst.Term.Head

      case class Lit(lit: AnyRef, tpe: Type, loc: SourceLocation) extends ExecutableAst.Term.Head

      case class Cst(ref: Symbol.DefnSym, tpe: Type, loc: SourceLocation) extends ExecutableAst.Term.Head

      case class App(sym: Symbol.DefnSym, args: Array[Symbol.VarSym], tpe: Type, loc: SourceLocation) extends ExecutableAst.Term.Head

    }

    sealed trait Body extends ExecutableAst

    object Body {

      case class Wild(tpe: Type, loc: SourceLocation) extends ExecutableAst.Term.Body

      case class Var(sym: Symbol.VarSym, tpe: Type, loc: SourceLocation) extends ExecutableAst.Term.Body

      case class Lit(lit: AnyRef, tpe: Type, loc: SourceLocation) extends ExecutableAst.Term.Body

      case class Cst(ref: Symbol.DefnSym, tpe: Type, loc: SourceLocation) extends ExecutableAst.Term.Body

      case class Pat(pat: ExecutableAst.Pattern, tpe: Type, loc: SourceLocation) extends ExecutableAst.Term.Body

    }

  }

  case class Attribute(name: String, tpe: Type) extends ExecutableAst

  case class Case(enum: Name.Ident, tag: Name.Ident, tpe: Type) extends ExecutableAst

  sealed trait ConstraintParam

  object ConstraintParam {

    case class HeadParam(sym: Symbol.VarSym, tpe: Type, loc: SourceLocation) extends ExecutableAst.ConstraintParam

    case class RuleParam(sym: Symbol.VarSym, tpe: Type, loc: SourceLocation) extends ExecutableAst.ConstraintParam

  }

  case class FormalParam(sym: Symbol.VarSym, tpe: Type) extends ExecutableAst

  case class FreeVar(sym: Symbol.VarSym, tpe: Type) extends ExecutableAst

}

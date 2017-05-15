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

import ca.uwaterloo.flix.runtime.InvocationTarget

sealed trait ExecutableAst

object ExecutableAst {

  case class Root(definitions: Map[Symbol.DefnSym, ExecutableAst.Definition.Constant],
                  enums: Map[Symbol.EnumSym, ExecutableAst.Definition.Enum],
                  lattices: Map[Type, ExecutableAst.Definition.Lattice],
                  tables: Map[Symbol.TableSym, ExecutableAst.Table],
                  indexes: Map[Symbol.TableSym, ExecutableAst.Definition.Index],
                  constraints: List[ExecutableAst.Constraint],
                  properties: List[ExecutableAst.Property],
                  reachable: Set[Symbol.DefnSym],
                  time: Time,
                  dependenciesOf: Map[Symbol.TableSym, Set[(Constraint, ExecutableAst.Predicate.Body.Positive)]]) extends ExecutableAst


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

  sealed trait Definition

  object Definition {

    case class Constant(ann: Ast.Annotations, sym: Symbol.DefnSym, formals: Array[ExecutableAst.FormalParam], exp: ExecutableAst.Expression, isSynthetic: Boolean, tpe: Type, loc: SourceLocation) extends ExecutableAst.Definition {
      /**
        * Pointer to generated code.
        */
      var method: Method = null
    }

    case class Enum(sym: Symbol.EnumSym, cases: Map[String, ExecutableAst.Case], loc: SourceLocation) extends ExecutableAst.Definition

    case class Lattice(tpe: Type,
                       bot: Symbol.DefnSym,
                       top: Symbol.DefnSym,
                       leq: Symbol.DefnSym,
                       lub: Symbol.DefnSym,
                       glb: Symbol.DefnSym,
                       loc: SourceLocation) extends ExecutableAst.Definition

    case class Index(name: Symbol.TableSym,
                     indexes: Seq[Seq[Name.Ident]],
                     loc: SourceLocation) extends ExecutableAst.Definition

  }

  sealed trait Table extends ExecutableAst

  object Table {

    case class Relation(sym: Symbol.TableSym,
                        attributes: Array[ExecutableAst.Attribute],
                        loc: SourceLocation) extends ExecutableAst.Table

    case class Lattice(sym: Symbol.TableSym,
                       keys: Array[ExecutableAst.Attribute],
                       value: ExecutableAst.Attribute,
                       loc: SourceLocation) extends ExecutableAst.Table

  }

  sealed trait Expression extends ExecutableAst {
    def tpe: Type

    def loc: SourceLocation
  }

  sealed trait LoadExpression extends Expression {
    val e: ExecutableAst.Expression
    val offset: scala.Int
    val mask: scala.Int
    final val loc = SourceLocation.Unknown
  }

  sealed trait StoreExpression extends Expression {
    val e: ExecutableAst.Expression
    val offset: scala.Int
    val v: ExecutableAst.Expression
    val mask: Long
    final val targetMask = ~(mask << offset)
    final val tpe = Type.Int64
    final val loc = SourceLocation.Unknown
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

    /**
      * An AST node representing a value (of type Bool) loaded from an Int64.
      *
      * @param e      the expression, returning an Int64, that the value is loaded from.
      * @param offset the offset (in bits) from the least significant bit that the value is loaded from.
      */
    case class LoadBool(e: ExecutableAst.Expression, offset: scala.Int) extends ExecutableAst.LoadExpression {
      val mask = 1
      val tpe = Type.Bool
    }

    /**
      * An AST node representing a value (of type Int8) loaded from an Int64.
      *
      * @param e      the expression, returning an Int64, that the value is loaded from.
      * @param offset the offset (in bits) from the least significant bit that the value is loaded from.
      */
    case class LoadInt8(e: ExecutableAst.Expression, offset: scala.Int) extends ExecutableAst.LoadExpression {
      val mask = 0xFF
      val tpe = Type.Int8
    }

    /**
      * An AST node representing a value (of type Int16) loaded from an Int64.
      *
      * @param e      the expression, returning an Int64, that the value is loaded from.
      * @param offset the offset (in bits) from the least significant bit that the value is loaded from.
      */
    case class LoadInt16(e: ExecutableAst.Expression, offset: scala.Int) extends ExecutableAst.LoadExpression {
      val mask = 0xFFFF
      val tpe = Type.Int16
    }

    /**
      * An AST node representing a value (of type Int32) loaded from an Int64.
      *
      * @param e      the expression, returning an Int64, that the value is loaded from.
      * @param offset the offset (in bits) from the least significant bit that the value is loaded from.
      */
    case class LoadInt32(e: ExecutableAst.Expression, offset: scala.Int) extends ExecutableAst.LoadExpression {
      // If we had unsigned ints, would be 0xFFFFFFFF
      val mask = -1
      val tpe = Type.Int32
    }

    /**
      * An AST node representing a value (of type Bool) to be stored into an Int64.
      *
      * @param e      the expression, returning an Int64, that the value is stored into.
      * @param offset the offset (in bits) from the least significant bit that the value is stored into.
      * @param v      the value to be stored.
      */
    case class StoreBool(e: ExecutableAst.Expression,
                         offset: scala.Int,
                         v: ExecutableAst.Expression) extends ExecutableAst.StoreExpression {
      val mask = 0x1L
    }

    /**
      * An AST node representing a value (of type Int8) to be stored into an Int64.
      *
      * @param e      the expression, returning an Int64, that the value is stored into.
      * @param offset the offset (in bits) from the least significant bit that the value is stored into.
      * @param v      the value to be stored.
      */
    case class StoreInt8(e: ExecutableAst.Expression,
                         offset: scala.Int,
                         v: ExecutableAst.Expression) extends ExecutableAst.StoreExpression {
      val mask = 0xFFL
    }

    /**
      * An AST node representing a value (of type Int16) to be stored into an Int64.
      *
      * @param e      the expression, returning an Int64, that the value is stored into.
      * @param offset the offset (in bits) from the least significant bit that the value is stored into.
      * @param v      the value to be stored.
      */
    case class StoreInt16(e: ExecutableAst.Expression,
                          offset: scala.Int,
                          v: ExecutableAst.Expression) extends ExecutableAst.StoreExpression {
      val mask = 0xFFFFL
    }

    /**
      * An AST node representing a value (of type Int32) to be stored into an Int64.
      *
      * @param e      the expression, returning an Int64, that the value is stored into.
      * @param offset the offset (in bits) from the least significant bit that the value is stored into.
      * @param v      the value to be stored.
      */
    case class StoreInt32(e: ExecutableAst.Expression,
                          offset: scala.Int,
                          v: ExecutableAst.Expression) extends ExecutableAst.StoreExpression {
      val mask = 0xFFFFFFFFL
    }

    /**
      * A typed AST node representing a local variable expression (i.e. a parameter or let-bound variable).
      *
      * @param sym the name of the variable.
      * @param tpe the type of the variable.
      * @param loc the source location of the variable.
      */
    case class Var(sym: Symbol.VarSym,
                   tpe: Type,
                   loc: SourceLocation) extends ExecutableAst.Expression

    /**
      * A typed AST node representing a reference to a top-level definition.
      *
      * @param sym the name of the reference.
      * @param tpe the type of the reference.
      * @param loc the source location of the reference.
      */
    case class Ref(sym: Symbol.DefnSym, tpe: Type, loc: SourceLocation) extends ExecutableAst.Expression

    /**
      * A typed AST node representing the creation of a closure. Free variables are computed at compile time and bound
      * at run time.
      *
      * @param ref      the reference to the lambda associated with the closure.
      * @param freeVars the cached set of free variables occurring within the lambda expression.
      * @param tpe      the type of the closure.
      * @param loc      the source location of the lambda.
      */
    case class MkClosureRef(ref: ExecutableAst.Expression.Ref,
                            freeVars: Array[FreeVar],
                            tpe: Type,
                            loc: SourceLocation) extends ExecutableAst.Expression

    /**
      * A typed AST node representing a function call.
      *
      * @param sym  the name of the function being called.
      * @param args the function arguments.
      * @param tpe  the return type of the function.
      * @param loc  the source location of the expression.
      */
    case class ApplyRef(sym: Symbol.DefnSym, args: List[ExecutableAst.Expression], tpe: Type, loc: SourceLocation) extends ExecutableAst.Expression

    /**
      * A typed AST node representing a tail recursive call.
      *
      * @param name    the name of the function being called.
      * @param formals the formal parameters.
      * @param actuals the actual parameters.
      * @param tpe     the return type of the function.
      * @param loc     the source location of the expression.
      */
    case class ApplyTail(name: Symbol.DefnSym, formals: List[ExecutableAst.FormalParam], actuals: List[ExecutableAst.Expression], tpe: Type, loc: SourceLocation) extends ExecutableAst.Expression

    /**
      * A typed AST node representing a function call.
      *
      * @param hook the hook being called
      * @param args the function arguments.
      * @param tpe  the return type of the function.
      * @param loc  the source location of the expression.
      */
    case class ApplyHook(hook: Ast.Hook, args: List[ExecutableAst.Expression], tpe: Type, loc: SourceLocation) extends ExecutableAst.Expression

    /**
      * A typed AST node representing a function call.
      *
      * @param exp  the function being called.
      * @param args the function arguments.
      * @param tpe  the return type of the function.
      * @param loc  the source location of the expression.
      */
    case class ApplyClosure(exp: ExecutableAst.Expression, args: List[ExecutableAst.Expression], tpe: Type, loc: SourceLocation) extends ExecutableAst.Expression

    /**
      * A typed AST node representing a unary expression.
      *
      * @param op  the unary operator.
      * @param exp the expression.
      * @param tpe the type of the expression.
      * @param loc the source location of the expression.
      */
    case class Unary(op: UnaryOperator,
                     exp: ExecutableAst.Expression,
                     tpe: Type,
                     loc: SourceLocation) extends ExecutableAst.Expression {
      override def toString: String = "Unary(" + op + ", " + exp + ")"
    }

    /**
      * A typed AST node representing a binary expression.
      *
      * @param op   the binary operator.
      * @param exp1 the left expression.
      * @param exp2 the right expression.
      * @param tpe  the type of the expression.
      * @param loc  the source location of the expression.
      */
    case class Binary(op: BinaryOperator,
                      exp1: ExecutableAst.Expression,
                      exp2: ExecutableAst.Expression,
                      tpe: Type,
                      loc: SourceLocation) extends ExecutableAst.Expression {
      override def toString: String = "Binary(" + op + ", " + exp1 + ", " + exp2 + ")"
    }

    /**
      * A typed AST node representing an if-then-else expression.
      *
      * @param exp1 the conditional expression.
      * @param exp2 the consequent expression.
      * @param exp3 the alternative expression.
      * @param tpe  the type of the consequent and alternative expression.
      * @param loc  the source location of the expression.
      */
    case class IfThenElse(exp1: ExecutableAst.Expression,
                          exp2: ExecutableAst.Expression,
                          exp3: ExecutableAst.Expression,
                          tpe: Type,
                          loc: SourceLocation) extends ExecutableAst.Expression {
      override def toString: String = "IfThenElse(" + exp1 + ", " + exp2 + ", " + exp3 + ")"
    }

    case class Let(sym: Symbol.VarSym, exp1: ExecutableAst.Expression, exp2: ExecutableAst.Expression, tpe: Type, loc: SourceLocation) extends ExecutableAst.Expression

    // TODO: Might consider changing to the type of exp1 to ref.
    case class LetRec(sym: Symbol.VarSym, exp1: ExecutableAst.Expression, exp2: ExecutableAst.Expression, tpe: Type, loc: SourceLocation) extends ExecutableAst.Expression

    case class Is(sym: Symbol.EnumSym, tag: String, exp: ExecutableAst.Expression, loc: SourceLocation) extends ExecutableAst.Expression {
      final val tpe: Type = Type.Bool
    }

    case class Tag(sym: Symbol.EnumSym, tag: String, exp: ExecutableAst.Expression, tpe: Type, loc: SourceLocation) extends ExecutableAst.Expression

    case class Untag(sym: Symbol.EnumSym, tag: String, exp: ExecutableAst.Expression, tpe: Type, loc: SourceLocation) extends ExecutableAst.Expression

    case class Index(base: ExecutableAst.Expression, offset: scala.Int, tpe: Type, loc: SourceLocation) extends ExecutableAst.Expression

    case class Tuple(elms: Array[ExecutableAst.Expression], tpe: Type, loc: SourceLocation) extends ExecutableAst.Expression

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

      case class Lit(ref: AnyRef, tpe: Type, loc: SourceLocation) extends ExecutableAst.Term.Head

      case class App(sym: Symbol.DefnSym, args: Array[Symbol.VarSym], tpe: Type, loc: SourceLocation) extends ExecutableAst.Term.Head

    }

    sealed trait Body extends ExecutableAst

    object Body {

      case class Wild(tpe: Type, loc: SourceLocation) extends ExecutableAst.Term.Body

      case class Var(sym: Symbol.VarSym, tpe: Type, loc: SourceLocation) extends ExecutableAst.Term.Body

      case class Lit(ref: AnyRef, tpe: Type, loc: SourceLocation) extends ExecutableAst.Term.Body

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

  case class Property(law: Symbol.DefnSym, defn: Symbol.DefnSym, exp: ExecutableAst.Expression) extends ExecutableAst {
    def loc: SourceLocation = defn.loc
  }

}

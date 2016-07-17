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

import java.lang.reflect.Method
import java.util.concurrent.atomic.{AtomicInteger, AtomicLong}

sealed trait ExecutableAst

object ExecutableAst {

  case class Root(constants: Map[Symbol.Resolved, ExecutableAst.Definition.Constant],
                  lattices: Map[Type, ExecutableAst.Definition.Lattice],
                  tables: Map[Symbol.TableSym, ExecutableAst.Table],
                  indexes: Map[Symbol.TableSym, ExecutableAst.Definition.Index],
                  facts: Array[ExecutableAst.Constraint.Fact],
                  rules: Array[ExecutableAst.Constraint.Rule],
                  properties: List[ExecutableAst.Property],
                  time: Time,
                  dependenciesOf: Map[Symbol.TableSym, Set[(Constraint.Rule, ExecutableAst.Predicate.Body.Table)]]) extends ExecutableAst

  sealed trait Definition

  object Definition {

    case class Constant(name: Symbol.Resolved,
                        formals: Array[ExecutableAst.FormalArg],
                        exp: ExecutableAst.Expression,
                        isSynthetic: Boolean,
                        tpe: Type,
                        loc: SourceLocation) extends ExecutableAst.Definition {
      var method: Method = null
    }

    case class Lattice(tpe: Type,
                       bot: Symbol.Resolved,
                       top: Symbol.Resolved,
                       leq: Symbol.Resolved,
                       lub: Symbol.Resolved,
                       glb: Symbol.Resolved,
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

  sealed trait Constraint extends ExecutableAst

  object Constraint {

    case class Fact(head: ExecutableAst.Predicate.Head) extends ExecutableAst.Constraint

    // TODO(magnus): Change lists to arrays
    case class Rule(head: ExecutableAst.Predicate.Head,
                    body: List[ExecutableAst.Predicate.Body],
                    tables: List[ExecutableAst.Predicate.Body.Table],
                    filters: List[ExecutableAst.Predicate.Body.ApplyFilter],
                    filterHooks: List[ExecutableAst.Predicate.Body.ApplyHookFilter],
                    disjoint: List[ExecutableAst.Predicate.Body.NotEqual],
                    loops: List[ExecutableAst.Predicate.Body.Loop]) extends ExecutableAst.Constraint {


      /**
        * Records the number of times this rule has been evaluated.
        */
      val hits = new AtomicInteger()

      /**
        * Records the amount of time spent evaluating this rule.
        */
      val time = new AtomicLong()

    }

  }

  sealed trait Expression extends ExecutableAst {

    /**
      * Returns a list of all the universally quantified variables in this expression.
      */
    def getQuantifiers: List[Expression.Var] = this match {
      case Expression.Universal(params, _, _) => params.map {
        case Ast.FormalParam(ident, tpe) => Expression.Var(ident, -1, tpe, SourceLocation.Unknown)
      }
      case _ => Nil
    }

    /**
      * Returns this expression with all universal quantifiers stripped.
      */
    def peelQuantifiers: Expression = this match {
      case Expression.Existential(params, exp, loc) => exp.peelQuantifiers
      case Expression.Universal(params, exp, loc) => exp.peelQuantifiers
      case _ => this
    }

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

      override def toString: String = "#U"
    }

    case object True extends ExecutableAst.Expression {
      final val tpe = Type.Bool
      final val loc = SourceLocation.Unknown

      override def toString: String = "#t"
    }

    case object False extends ExecutableAst.Expression {
      final val tpe = Type.Bool
      final val loc = SourceLocation.Unknown

      override def toString: String = "#f"
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
      * @param ident  the name of the variable.
      * @param offset the (0-based) index of the variable.
      * @param tpe    the type of the variable.
      * @param loc    the source location of the variable.
      */
    // TODO: Rename to LocalVar
    case class Var(ident: Name.Ident,
                   offset: scala.Int,
                   tpe: Type,
                   loc: SourceLocation) extends ExecutableAst.Expression

    /**
      * A typed AST node representing a reference to a top-level definition.
      *
      * @param name the name of the reference.
      * @param tpe  the type of the reference.
      * @param loc  the source location of the reference.
      */
    case class Ref(name: Symbol.Resolved, tpe: Type, loc: SourceLocation) extends ExecutableAst.Expression {
      override def toString: String = "Ref(" + name.fqn + ")"
    }

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
                            tpe: Type.Lambda,
                            loc: SourceLocation) extends ExecutableAst.Expression

    /**
      * A typed AST node representing a function call.
      *
      * @param name the name of the function being called.
      * @param args the function arguments.
      * @param tpe  the return type of the function.
      * @param loc  the source location of the expression.
      */
    case class ApplyRef(name: Symbol.Resolved,
                        args: Array[ExecutableAst.Expression],
                        tpe: Type,
                        loc: SourceLocation) extends ExecutableAst.Expression

    /**
      * A typed AST node representing a function call.
      *
      * @param hook the hook being called
      * @param args the function arguments.
      * @param tpe  the return type of the function.
      * @param loc  the source location of the expression.
      */
    case class ApplyHook(hook: Ast.Hook,
                         args: Array[ExecutableAst.Expression],
                         tpe: Type,
                         loc: SourceLocation) extends ExecutableAst.Expression

    /**
      * A typed AST node representing a function call.
      *
      * @param exp  the function being called.
      * @param args the function arguments.
      * @param tpe  the return type of the function.
      * @param loc  the source location of the expression.
      */
    case class ApplyClosure(exp: ExecutableAst.Expression,
                            args: Array[ExecutableAst.Expression],
                            tpe: Type,
                            loc: SourceLocation) extends ExecutableAst.Expression

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

    /**
      * A typed AST node representing a let expression.
      *
      * @param ident  the name of the bound variable.
      * @param offset the (0-based) index of the bound variable.
      * @param exp1   the value of the bound variable.
      * @param exp2   the body expression in which the bound variable is visible.
      * @param tpe    the type of the expression (which is equivalent to the type of the body expression).
      * @param loc    the source location of the expression.
      */
    case class Let(ident: Name.Ident,
                   offset: scala.Int,
                   exp1: ExecutableAst.Expression,
                   exp2: ExecutableAst.Expression,
                   tpe: Type,
                   loc: SourceLocation) extends ExecutableAst.Expression {
      override def toString: String = "Let(" + ident.name + " = " + exp1 + " in " + exp2 + ")"
    }

    /**
      * A typed AST node representing a check-tag expression, i.e. check if the tag expression matches the given tag
      * identifier.
      *
      * @param tag the tag identifier.
      * @param exp the tag expression to check.
      * @param loc the source location of the expression.
      */
    case class CheckTag(tag: Name.Ident,
                        exp: ExecutableAst.Expression,
                        loc: SourceLocation) extends ExecutableAst.Expression {
      final val tpe: Type = Type.Bool

      override def toString: String = "CheckTag(" + tag.name + ", " + exp + ")"
    }

    /**
      * A typed AST node representing a dereference of the inner value of a tag, i.e. destruct a tag.
      *
      * @param tag the tag identifier.
      * @param exp the tag expression to destruct.
      * @param tpe the type of the inner tag value.
      * @param loc the source location of the expression.
      */
    case class GetTagValue(tag: Name.Ident,
                           exp: ExecutableAst.Expression,
                           tpe: Type,
                           loc: SourceLocation) extends ExecutableAst.Expression {
      override def toString: String = "GetTagValue(" + exp + ")"
    }

    /**
      * A typed AST node representing a tagged expression.
      *
      * @param enum the name of the enum.
      * @param tag  the name of the tag.
      * @param exp  the expression.
      * @param tpe  the type of the expression.
      * @param loc  The source location of the tag.
      */
    case class Tag(enum: Symbol.Resolved,
                   tag: Name.Ident,
                   exp: ExecutableAst.Expression,
                   tpe: Type.Enum,
                   loc: SourceLocation) extends ExecutableAst.Expression {
      override def toString: String = {
        val inner = exp match {
          case Expression.Unit => ""
          case _ => s"($exp)"
        }
        tag.name + inner
      }
    }

    /**
      * A typed AST node representing an index into a tuple, i.e. destruct a tuple.
      *
      * @param base   the tuple expression to index into.
      * @param offset the (0-based) offset of the tuple.
      * @param tpe    the type of the expression.
      * @param loc    the source location of the tuple.
      */
    case class GetTupleIndex(base: ExecutableAst.Expression,
                             offset: scala.Int,
                             tpe: Type,
                             loc: SourceLocation) extends ExecutableAst.Expression {
      override def toString: String = base + "[" + offset + "]"
    }

    /**
      * A typed AST node representing a tuple expression.
      *
      * @param elms the elements of the tuple.
      * @param tpe  the type of the tuple.
      * @param loc  the source location of the tuple.
      */
    case class Tuple(elms: Array[ExecutableAst.Expression],
                     tpe: Type,
                     loc: SourceLocation) extends ExecutableAst.Expression {
      override def toString: String = "(" + elms.mkString(", ") + ")"
    }

    case class CheckNil(exp: ExecutableAst.Expression, loc: SourceLocation) extends ExecutableAst.Expression {
      final val tpe: Type = Type.Bool
    }

    case class CheckCons(exp: ExecutableAst.Expression, loc: SourceLocation) extends ExecutableAst.Expression {
      final val tpe: Type = Type.Bool
    }

    case class FSet(elms: Array[ExecutableAst.Expression],
                    tpe: Type.FSet,
                    loc: SourceLocation) extends ExecutableAst.Expression

    case class Existential(params: List[Ast.FormalParam], exp: ExecutableAst.Expression, loc: SourceLocation) extends ExecutableAst.Expression {
      def tpe: Type = Type.Bool
    }

    case class Universal(params: List[Ast.FormalParam], exp: ExecutableAst.Expression, loc: SourceLocation) extends ExecutableAst.Expression {
      def tpe: Type = Type.Bool
    }

    case class UserError(tpe: Type, loc: SourceLocation) extends ExecutableAst.Expression

    case class MatchError(tpe: Type, loc: SourceLocation) extends ExecutableAst.Expression

    case class SwitchError(tpe: Type, loc: SourceLocation) extends ExecutableAst.Expression

  }

  sealed trait Predicate extends ExecutableAst {
    def tpe: Type

    def loc: SourceLocation
  }

  object Predicate {

    sealed trait Head extends ExecutableAst.Predicate

    object Head {

      case class True(loc: SourceLocation) extends ExecutableAst.Predicate.Head {
        def tpe: Type = Type.Predicate(Nil)
      }

      case class False(loc: SourceLocation) extends ExecutableAst.Predicate.Head {
        def tpe: Type = Type.Predicate(Nil)
      }

      case class Table(sym: Symbol.TableSym,
                       terms: Array[ExecutableAst.Term.Head],
                       tpe: Type.Predicate,
                       loc: SourceLocation) extends ExecutableAst.Predicate.Head {
        /**
          * Returns the arity of the predicate.
          */
        val arity: Int = terms.length
      }

    }

    sealed trait Body extends ExecutableAst.Predicate {
      /**
        * Returns the set of free variables in the term.
        */
      val freeVars: Set[String]
    }

    object Body {

      case class Table(sym: Symbol.TableSym,
                       terms: Array[ExecutableAst.Term.Body],
                       index2var: Array[String],
                       freeVars: Set[String],
                       tpe: Type.Predicate,
                       loc: SourceLocation) extends ExecutableAst.Predicate.Body {
        /**
          * Returns the arity of this table predicate.
          */
        val arity: Int = terms.length
      }

      case class ApplyFilter(name: Symbol.Resolved,
                             terms: Array[ExecutableAst.Term.Body],
                             freeVars: Set[String],
                             tpe: Type.Lambda,
                             loc: SourceLocation) extends ExecutableAst.Predicate.Body

      case class ApplyHookFilter(hook: Ast.Hook,
                                 terms: Array[ExecutableAst.Term.Body],
                                 freeVars: Set[String],
                                 tpe: Type.Lambda,
                                 loc: SourceLocation) extends ExecutableAst.Predicate.Body

      case class NotEqual(ident1: Name.Ident,
                          ident2: Name.Ident,
                          freeVars: Set[String],
                          tpe: Type,
                          loc: SourceLocation) extends ExecutableAst.Predicate.Body

      case class Loop(ident: Name.Ident,
                      term: ExecutableAst.Term.Head,
                      freeVars: Set[String],
                      tpe: Type,
                      loc: SourceLocation) extends ExecutableAst.Predicate.Body

    }

  }

  object Term {

    sealed trait Head extends ExecutableAst {
      def tpe: Type

      def loc: SourceLocation
    }

    object Head {

      case class Var(ident: Name.Ident, tpe: Type, loc: SourceLocation) extends ExecutableAst.Term.Head

      case class Exp(e: ExecutableAst.Expression, tpe: Type, loc: SourceLocation) extends ExecutableAst.Term.Head

      case class Apply(name: Symbol.Resolved,
                       args: Array[ExecutableAst.Term.Head],
                       tpe: Type,
                       loc: SourceLocation) extends ExecutableAst.Term.Head

      case class ApplyHook(hook: Ast.Hook,
                           args: Array[ExecutableAst.Term.Head],
                           tpe: Type,
                           loc: SourceLocation) extends ExecutableAst.Term.Head

    }

    sealed trait Body extends ExecutableAst {
      def tpe: Type

      def loc: SourceLocation
    }

    object Body {

      case class Wildcard(tpe: Type, loc: SourceLocation) extends ExecutableAst.Term.Body

      case class Var(ident: Name.Ident, v: scala.Int, tpe: Type, loc: SourceLocation) extends ExecutableAst.Term.Body

      case class Exp(e: ExecutableAst.Expression, tpe: Type, loc: SourceLocation) extends ExecutableAst.Term.Body

    }

  }

  case class Attribute(ident: Name.Ident, tpe: Type) extends ExecutableAst

  case class FormalArg(ident: Name.Ident, tpe: Type) extends ExecutableAst

  case class FreeVar(ident: Name.Ident, offset: Int, tpe: Type) extends ExecutableAst

  case class Property(law: Law, exp: ExecutableAst.Expression, loc: SourceLocation) extends ExecutableAst

}

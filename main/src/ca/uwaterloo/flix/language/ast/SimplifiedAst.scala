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

sealed trait SimplifiedAst

object SimplifiedAst {

  case class Root(definitions: Map[Symbol.DefnSym, SimplifiedAst.Definition.Constant],
                  enums: Map[Symbol.EnumSym, SimplifiedAst.Definition.Enum],
                  lattices: Map[Type, SimplifiedAst.Definition.Lattice],
                  tables: Map[Symbol.TableSym, SimplifiedAst.Table],
                  indexes: Map[Symbol.TableSym, SimplifiedAst.Definition.Index],
                  facts: List[SimplifiedAst.Constraint.Fact],
                  rules: List[SimplifiedAst.Constraint.Rule],
                  properties: List[SimplifiedAst.Property],
                  time: Time) extends SimplifiedAst

  sealed trait Definition

  object Definition {

    case class Constant(ann: Ast.Annotations,
                        sym: Symbol.DefnSym,
                        formals: List[SimplifiedAst.FormalParam],
                        exp: SimplifiedAst.Expression,
                        isSynthetic: Boolean,
                        tpe: Type,
                        loc: SourceLocation) extends SimplifiedAst.Definition

    case class Enum(sym: Symbol.EnumSym, cases: Map[String, SimplifiedAst.Case], loc: SourceLocation) extends SimplifiedAst.Definition

    case class Lattice(tpe: Type,
                       bot: SimplifiedAst.Expression,
                       top: SimplifiedAst.Expression,
                       leq: SimplifiedAst.Expression,
                       lub: SimplifiedAst.Expression,
                       glb: SimplifiedAst.Expression,
                       loc: SourceLocation) extends SimplifiedAst.Definition

    case class Index(sym: Symbol.TableSym,
                     indexes: Seq[Seq[Name.Ident]],
                     loc: SourceLocation) extends SimplifiedAst.Definition

  }

  sealed trait Table extends SimplifiedAst

  object Table {

    case class Relation(sym: Symbol.TableSym, attributes: List[SimplifiedAst.Attribute], loc: SourceLocation) extends SimplifiedAst.Table

    case class Lattice(sym: Symbol.TableSym, keys: List[SimplifiedAst.Attribute], value: SimplifiedAst.Attribute, loc: SourceLocation) extends SimplifiedAst.Table

  }

  sealed trait Constraint extends SimplifiedAst

  object Constraint {

    case class Fact(head: SimplifiedAst.Predicate.Head) extends SimplifiedAst.Constraint

    case class Rule(head: SimplifiedAst.Predicate.Head, body: List[SimplifiedAst.Predicate.Body]) extends SimplifiedAst.Constraint

  }

  sealed trait Expression extends SimplifiedAst {
    def tpe: Type

    def loc: SourceLocation
  }

  sealed trait LoadExpression extends Expression {
    val e: SimplifiedAst.Expression
    val offset: scala.Int
    val mask: scala.Int
    final val loc = SourceLocation.Unknown
  }

  sealed trait StoreExpression extends Expression {
    val e: SimplifiedAst.Expression
    val offset: scala.Int
    val v: SimplifiedAst.Expression
    val mask: Long
    final val targetMask = ~(mask << offset)
    final val tpe = Type.Int64
    final val loc = SourceLocation.Unknown
  }

  object Expression {

    case object Unit extends SimplifiedAst.Expression {
      final val tpe = Type.Unit
      final val loc = SourceLocation.Unknown
    }

    case object True extends SimplifiedAst.Expression {
      final val tpe = Type.Bool
      final val loc = SourceLocation.Unknown
    }

    case object False extends SimplifiedAst.Expression {
      final val tpe = Type.Bool
      final val loc = SourceLocation.Unknown
    }

    case class Char(lit: scala.Char) extends SimplifiedAst.Expression {
      final val tpe = Type.Char
      final val loc = SourceLocation.Unknown
    }

    case class Float32(lit: scala.Float) extends SimplifiedAst.Expression {
      final val tpe = Type.Float32
      final val loc = SourceLocation.Unknown
    }

    case class Float64(list: scala.Double) extends SimplifiedAst.Expression {
      final val tpe = Type.Float64
      final val loc = SourceLocation.Unknown
    }

    case class Int8(lit: scala.Byte) extends SimplifiedAst.Expression {
      final val tpe = Type.Int8
      final val loc = SourceLocation.Unknown
    }

    case class Int16(lit: scala.Short) extends SimplifiedAst.Expression {
      final val tpe = Type.Int16
      final val loc = SourceLocation.Unknown
    }

    case class Int32(lit: scala.Int) extends SimplifiedAst.Expression {
      final val tpe = Type.Int32
      final val loc = SourceLocation.Unknown
    }

    case class Int64(lit: scala.Long) extends SimplifiedAst.Expression {
      final val tpe = Type.Int64
      final val loc = SourceLocation.Unknown
    }

    case class BigInt(lit: java.math.BigInteger) extends SimplifiedAst.Expression {
      final val tpe = Type.BigInt
      final val loc = SourceLocation.Unknown
    }

    case class Str(lit: java.lang.String) extends SimplifiedAst.Expression {
      final val tpe = Type.Str
      final val loc = SourceLocation.Unknown
    }

    /**
      * An AST node representing a value (of type Bool) loaded from an Int64.
      *
      * @param e      the expression, returning an Int64, that the value is loaded from.
      * @param offset the offset (in bits) from the least significant bit that the value is loaded from.
      */
    case class LoadBool(e: SimplifiedAst.Expression, offset: scala.Int) extends SimplifiedAst.LoadExpression {
      val mask = 1
      val tpe = Type.Bool
    }

    /**
      * An AST node representing a value (of type Int8) loaded from an Int64.
      *
      * @param e      the expression, returning an Int64, that the value is loaded from.
      * @param offset the offset (in bits) from the least significant bit that the value is loaded from.
      */
    case class LoadInt8(e: SimplifiedAst.Expression, offset: scala.Int) extends SimplifiedAst.LoadExpression {
      val mask = 0xFF
      val tpe = Type.Int8
    }

    /**
      * An AST node representing a value (of type Int16) loaded from an Int64.
      *
      * @param e      the expression, returning an Int64, that the value is loaded from.
      * @param offset the offset (in bits) from the least significant bit that the value is loaded from.
      */
    case class LoadInt16(e: SimplifiedAst.Expression, offset: scala.Int) extends SimplifiedAst.LoadExpression {
      val mask = 0xFFFF
      val tpe = Type.Int16
    }

    /**
      * An AST node representing a value (of type Int32) loaded from an Int64.
      *
      * @param e      the expression, returning an Int64, that the value is loaded from.
      * @param offset the offset (in bits) from the least significant bit that the value is loaded from.
      */
    case class LoadInt32(e: SimplifiedAst.Expression, offset: scala.Int) extends SimplifiedAst.LoadExpression {
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
    case class StoreBool(e: SimplifiedAst.Expression,
                         offset: scala.Int,
                         v: SimplifiedAst.Expression) extends SimplifiedAst.StoreExpression {
      val mask = 0x1L
    }

    /**
      * An AST node representing a value (of type Int8) to be stored into an Int64.
      *
      * @param e      the expression, returning an Int64, that the value is stored into.
      * @param offset the offset (in bits) from the least significant bit that the value is stored into.
      * @param v      the value to be stored.
      */
    case class StoreInt8(e: SimplifiedAst.Expression,
                         offset: scala.Int,
                         v: SimplifiedAst.Expression) extends SimplifiedAst.StoreExpression {
      val mask = 0xFFL
    }

    /**
      * An AST node representing a value (of type Int16) to be stored into an Int64.
      *
      * @param e      the expression, returning an Int64, that the value is stored into.
      * @param offset the offset (in bits) from the least significant bit that the value is stored into.
      * @param v      the value to be stored.
      */
    case class StoreInt16(e: SimplifiedAst.Expression,
                          offset: scala.Int,
                          v: SimplifiedAst.Expression) extends SimplifiedAst.StoreExpression {
      val mask = 0xFFFFL
    }

    /**
      * An AST node representing a value (of type Int32) to be stored into an Int64.
      *
      * @param e      the expression, returning an Int64, that the value is stored into.
      * @param offset the offset (in bits) from the least significant bit that the value is stored into.
      * @param v      the value to be stored.
      */
    case class StoreInt32(e: SimplifiedAst.Expression,
                          offset: scala.Int,
                          v: SimplifiedAst.Expression) extends SimplifiedAst.StoreExpression {
      val mask = 0xFFFFFFFFL
    }

    /**
      * A typed AST node representing a local variable expression (i.e. a parameter or let-bound variable).
      *
      * @param sym    the name of the variable.
      * @param tpe    the type of the variable.
      * @param loc    the source location of the variable.
      */
    case class Var(sym: Symbol.VarSym, tpe: Type, loc: SourceLocation) extends SimplifiedAst.Expression

    /**
      * A typed AST node representing a reference to a top-level definition.
      *
      * @param sym the name of the reference.
      * @param tpe the type of the reference.
      * @param loc the source location of the reference.
      */
    case class Ref(sym: Symbol.DefnSym, tpe: Type, loc: SourceLocation) extends SimplifiedAst.Expression

    /**
      * A typed AST node representing a lambda function.
      *
      * A later phase/pass lifts these lambda functions to top-level definitions,
      * thus they no longer exist after lambda lifting.
      *
      * @param args the formal arguments to the lambda.
      * @param body the body expression of the lambda.
      * @param tpe  the type of the lambda.
      * @param loc  the source location of the lambda.
      */
    case class Lambda(args: List[SimplifiedAst.FormalParam], body: SimplifiedAst.Expression, tpe: Type, loc: SourceLocation) extends SimplifiedAst.Expression

    /**
      * A typed AST node representing a hook (native function).
      *
      * @param hook the hook representing the native function.
      * @param tpe  the type of the hook.
      * @param loc  the source location of the hook.
      */
    case class Hook(hook: Ast.Hook, tpe: Type, loc: SourceLocation) extends SimplifiedAst.Expression

    /**
      * A typed AST node representing the creation of a closure.
      *
      * MkClosure nodes are created during closure conversion, replacing Lambda nodes. Then, during lambda lifting,
      * MkClosure is replaced with MkClosureRef.
      *
      * @param lambda   the lambda associated with the closure.
      * @param freeVars the cached set of free variables occurring within the lambda expression.
      * @param tpe      the type of the closure.
      * @param loc      the source location of the lambda.
      */
    case class MkClosure(lambda: SimplifiedAst.Expression.Lambda, freeVars: List[FreeVar], tpe: Type, loc: SourceLocation) extends SimplifiedAst.Expression

    /**
      * A typed AST node representing the creation of a closure, with the lambda lifted and replaced by a ref.
      *
      * Free variables are bound at run time. MkClosureRef nodes may be created during closure conversion, but most of
      * them are created during lambda lifting, to replace MkClosure nodes.
      *
      * @param ref      the reference to the lambda associated with the closure.
      * @param freeVars the cached set of free variables occurring within the lambda expression.
      * @param tpe      the type of the closure.
      * @param loc      the source location of the lambda.
      */
    case class MkClosureRef(ref: SimplifiedAst.Expression.Ref, freeVars: List[FreeVar], tpe: Type, loc: SourceLocation) extends SimplifiedAst.Expression

    /**
      * A typed AST node representing a function call.
      *
      * @param sym  the name of the function being called.
      * @param args the function arguments.
      * @param tpe  the return type of the function.
      * @param loc  the source location of the expression.
      */
    case class ApplyRef(sym: Symbol.DefnSym, args: List[SimplifiedAst.Expression], tpe: Type, loc: SourceLocation) extends SimplifiedAst.Expression

    /**
      * A typed AST node representing a tail recursive call.
      *
      * @param sym     the name of the function being called.
      * @param formals the formal parameters.
      * @param actuals the actual parameters.
      * @param tpe     the return type of the function.
      * @param loc     the source location of the expression.
      */
    case class ApplyTail(sym: Symbol.DefnSym, formals: List[SimplifiedAst.FormalParam], actuals: List[SimplifiedAst.Expression], tpe: Type, loc: SourceLocation) extends SimplifiedAst.Expression

    /**
      * A typed AST node representing a function call.
      *
      * @param hook the hook being called
      * @param args the function arguments.
      * @param tpe  the return type of the function.
      * @param loc  the source location of the expression.
      */
    case class ApplyHook(hook: Ast.Hook, args: List[SimplifiedAst.Expression], tpe: Type, loc: SourceLocation) extends SimplifiedAst.Expression

    /**
      * A typed AST node representing a function call.
      *
      * @param exp  the function being called.
      * @param args the function arguments.
      * @param tpe  the return type of the function.
      * @param loc  the source location of the expression.
      */
    case class Apply(exp: SimplifiedAst.Expression, args: List[SimplifiedAst.Expression], tpe: Type, loc: SourceLocation) extends SimplifiedAst.Expression

    /**
      * A typed AST node representing a unary expression.
      *
      * @param op  the unary operator.
      * @param exp the expression.
      * @param tpe the type of the expression.
      * @param loc the source location of the expression.
      */
    case class Unary(op: UnaryOperator, exp: SimplifiedAst.Expression, tpe: Type, loc: SourceLocation) extends SimplifiedAst.Expression

    /**
      * A typed AST node representing a binary expression.
      *
      * @param op   the binary operator.
      * @param exp1 the left expression.
      * @param exp2 the right expression.
      * @param tpe  the type of the expression.
      * @param loc  the source location of the expression.
      */
    case class Binary(op: BinaryOperator, exp1: SimplifiedAst.Expression, exp2: SimplifiedAst.Expression, tpe: Type, loc: SourceLocation) extends SimplifiedAst.Expression

    /**
      * A typed AST node representing an if-then-else expression.
      *
      * @param exp1 the conditional expression.
      * @param exp2 the consequent expression.
      * @param exp3 the alternative expression.
      * @param tpe  the type of the consequent and alternative expression.
      * @param loc  the source location of the expression.
      */
    case class IfThenElse(exp1: SimplifiedAst.Expression, exp2: SimplifiedAst.Expression, exp3: SimplifiedAst.Expression, tpe: Type, loc: SourceLocation) extends SimplifiedAst.Expression

    /**
      * A typed AST node representing a let expression.
      *
      * @param sym    the name of the bound variable.
      * @param exp1   the value of the bound variable.
      * @param exp2   the body expression in which the bound variable is visible.
      * @param tpe    the type of the expression (which is equivalent to the type of the body expression).
      * @param loc    the source location of the expression.
      */
    case class Let(sym: Symbol.VarSym, exp1: SimplifiedAst.Expression, exp2: SimplifiedAst.Expression, tpe: Type, loc: SourceLocation) extends SimplifiedAst.Expression

    /**
      * A typed AST node representing a check-tag expression, i.e. check if the tag expression matches the given tag
      * identifier.
      *
      * @param tag the tag identifier.
      * @param exp the tag expression to check.
      * @param loc the source location of the expression.
      */
    case class CheckTag(tag: String, exp: SimplifiedAst.Expression, loc: SourceLocation) extends SimplifiedAst.Expression {
      final val tpe: Type = Type.Bool
    }

    /**
      * A typed AST node representing a dereference of the inner value of a tag, i.e. destruct a tag.
      *
      * @param tag the tag identifier.
      * @param exp the tag expression to destruct.
      * @param tpe the type of the inner tag value.
      * @param loc the source location of the expression.
      */
    case class GetTagValue(tag: String, exp: SimplifiedAst.Expression, tpe: Type, loc: SourceLocation) extends SimplifiedAst.Expression

    /**
      * A typed AST node representing a tagged expression.
      *
      * @param sym the name of the enum.
      * @param tag the name of the tag.
      * @param exp the expression.
      * @param tpe the type of the expression.
      * @param loc The source location of the tag.
      */
    case class Tag(sym: Symbol.EnumSym, tag: String, exp: SimplifiedAst.Expression, tpe: Type, loc: SourceLocation) extends SimplifiedAst.Expression

    /**
      * A typed AST node representing an index into a tuple, i.e. destruct a tuple.
      *
      * @param base   the tuple expression to index into.
      * @param offset the (0-based) offset of the tuple.
      * @param tpe    the type of the expression.
      * @param loc    the source location of the tuple.
      */
    case class GetTupleIndex(base: SimplifiedAst.Expression, offset: scala.Int, tpe: Type, loc: SourceLocation) extends SimplifiedAst.Expression

    case class Tuple(elms: List[SimplifiedAst.Expression], tpe: Type, loc: SourceLocation) extends SimplifiedAst.Expression

    case class Existential(params: List[SimplifiedAst.FormalParam], exp: SimplifiedAst.Expression, loc: SourceLocation) extends SimplifiedAst.Expression {
      def tpe: Type = Type.Bool
    }

    case class Universal(params: List[SimplifiedAst.FormalParam], exp: SimplifiedAst.Expression, loc: SourceLocation) extends SimplifiedAst.Expression {
      def tpe: Type = Type.Bool
    }

    case class UserError(tpe: Type, loc: SourceLocation) extends SimplifiedAst.Expression

    case class MatchError(tpe: Type, loc: SourceLocation) extends SimplifiedAst.Expression

    case class SwitchError(tpe: Type, loc: SourceLocation) extends SimplifiedAst.Expression

  }

  sealed trait Predicate extends SimplifiedAst {
    def loc: SourceLocation
  }

  object Predicate {

    sealed trait Head extends SimplifiedAst.Predicate

    object Head {

      case class True(loc: SourceLocation) extends SimplifiedAst.Predicate.Head

      case class False(loc: SourceLocation) extends SimplifiedAst.Predicate.Head

      case class Table(sym: Symbol.TableSym, terms: List[SimplifiedAst.Term.Head], loc: SourceLocation) extends SimplifiedAst.Predicate.Head

    }

    sealed trait Body extends SimplifiedAst.Predicate

    object Body {

      case class Table(sym: Symbol.TableSym, terms: List[SimplifiedAst.Term.Body], loc: SourceLocation) extends SimplifiedAst.Predicate.Body

      case class ApplyFilter(sym: Symbol.DefnSym, terms: List[SimplifiedAst.Term.Body], loc: SourceLocation) extends SimplifiedAst.Predicate.Body

      case class ApplyHookFilter(hook: Ast.Hook, terms: List[SimplifiedAst.Term.Body], loc: SourceLocation) extends SimplifiedAst.Predicate.Body

      case class NotEqual(sym1: Symbol.VarSym, sym2: Symbol.VarSym, loc: SourceLocation) extends SimplifiedAst.Predicate.Body

      case class Loop(sym: Symbol.VarSym, term: SimplifiedAst.Term.Head, loc: SourceLocation) extends SimplifiedAst.Predicate.Body

    }

  }

  object Term {

    sealed trait Head extends SimplifiedAst {
      def tpe: Type

      def loc: SourceLocation
    }

    object Head {

      case class Var(sym: Symbol.VarSym, tpe: Type, loc: SourceLocation) extends SimplifiedAst.Term.Head

      // TODO: Lambda lift?
      case class Exp(literal: SimplifiedAst.Expression, tpe: Type, loc: SourceLocation) extends SimplifiedAst.Term.Head

      // TODO: Can we get rid of this?
      case class Apply(sym: Symbol.DefnSym, args: List[SimplifiedAst.Term.Head], tpe: Type, loc: SourceLocation) extends SimplifiedAst.Term.Head {

      }

      // TODO: To be replaced.
      case class ApplyHook(hook: Ast.Hook, args: List[SimplifiedAst.Term.Head], tpe: Type, loc: SourceLocation) extends SimplifiedAst.Term.Head

    }

    sealed trait Body extends SimplifiedAst {
      def tpe: Type

      def loc: SourceLocation
    }

    object Body {

      case class Wildcard(tpe: Type, loc: SourceLocation) extends SimplifiedAst.Term.Body

      case class Var(sym: Symbol.VarSym, v: scala.Int, tpe: Type, loc: SourceLocation) extends SimplifiedAst.Term.Body

      // TODO: Lambda lift?
      case class Exp(e: SimplifiedAst.Expression, tpe: Type, loc: SourceLocation) extends SimplifiedAst.Term.Body

    }

  }

  case class Attribute(name: String, tpe: Type) extends SimplifiedAst

  case class Case(enum: Name.Ident, tag: Name.Ident, tpe: Type) extends SimplifiedAst

  case class FormalParam(sym: Symbol.VarSym, tpe: Type) extends SimplifiedAst

  case class FreeVar(sym: Symbol.VarSym, tpe: Type) extends SimplifiedAst

  case class Property(law: Law, exp: SimplifiedAst.Expression, loc: SourceLocation) extends SimplifiedAst

}

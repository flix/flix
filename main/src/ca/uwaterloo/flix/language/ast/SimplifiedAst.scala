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

  // TODO: Order of elements.

  case class Root(constants: Map[Symbol.Resolved, SimplifiedAst.Definition.Constant],
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
                        name: Symbol.Resolved,
                        formals: List[SimplifiedAst.FormalArg],
                        exp: SimplifiedAst.Expression,
                        isSynthetic: Boolean,
                        tpe: Type,
                        loc: SourceLocation) extends SimplifiedAst.Definition

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

    case class Relation(sym: Symbol.TableSym,
                        attributes: List[SimplifiedAst.Attribute],
                        loc: SourceLocation) extends SimplifiedAst.Table

    case class Lattice(sym: Symbol.TableSym,
                       keys: List[SimplifiedAst.Attribute],
                       value: SimplifiedAst.Attribute,
                       loc: SourceLocation) extends SimplifiedAst.Table

  }

  sealed trait Constraint extends SimplifiedAst

  object Constraint {

    case class Fact(head: SimplifiedAst.Predicate.Head) extends SimplifiedAst.Constraint

    case class Rule(head: SimplifiedAst.Predicate.Head,
                    body: List[SimplifiedAst.Predicate.Body]) extends SimplifiedAst.Constraint

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

      override def toString: String = "#U"
    }

    case object True extends SimplifiedAst.Expression {
      final val tpe = Type.Bool
      final val loc = SourceLocation.Unknown

      override def toString: String = "#t"
    }

    case object False extends SimplifiedAst.Expression {
      final val tpe = Type.Bool
      final val loc = SourceLocation.Unknown

      override def toString: String = "#f"
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
      * @param ident  the name of the variable.
      * @param offset the (0-based) index of the variable.
      * @param tpe    the type of the variable.
      * @param loc    the source location of the variable.
      */
    case class Var(ident: Name.Ident,
                   offset: scala.Int,
                   tpe: Type,
                   loc: SourceLocation) extends SimplifiedAst.Expression {
      override def toString: String = ident.name
    }

    /**
      * A typed AST node representing a reference to a top-level definition.
      *
      * @param name the name of the reference.
      * @param tpe  the type of the reference.
      * @param loc  the source location of the reference.
      */
    case class Ref(name: Symbol.Resolved, tpe: Type, loc: SourceLocation) extends SimplifiedAst.Expression {
      override def toString: String = "Ref(" + name.fqn + ")"
    }

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
    case class Lambda(args: List[SimplifiedAst.FormalArg],
                      body: SimplifiedAst.Expression,
                      tpe: Type.Lambda,
                      loc: SourceLocation) extends SimplifiedAst.Expression {
      override def toString: String = "λ(" + args.map(_.tpe).mkString(", ") + ") " + body
    }

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
    case class MkClosure(lambda: SimplifiedAst.Expression.Lambda,
                         freeVars: List[FreeVar],
                         tpe: Type.Lambda,
                         loc: SourceLocation) extends SimplifiedAst.Expression

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
    case class MkClosureRef(ref: SimplifiedAst.Expression.Ref,
                            freeVars: List[FreeVar],
                            tpe: Type.Lambda,
                            loc: SourceLocation) extends SimplifiedAst.Expression

    /**
      * A typed AST node representing a function call.
      *
      * @param name the name of the function being called.
      * @param args the function arguments.
      * @param tpe  the return type of the function.
      * @param loc  the source location of the expression.
      */
    case class ApplyRef(name: Symbol.Resolved,
                        args: List[SimplifiedAst.Expression],
                        tpe: Type,
                        loc: SourceLocation) extends SimplifiedAst.Expression

    /**
      * A typed AST node representing a function call.
      *
      * @param hook the hook being called
      * @param args the function arguments.
      * @param tpe  the return type of the function.
      * @param loc  the source location of the expression.
      */
    case class ApplyHook(hook: Ast.Hook,
                         args: List[SimplifiedAst.Expression],
                         tpe: Type,
                         loc: SourceLocation) extends SimplifiedAst.Expression

    /**
      * A typed AST node representing a function call.
      *
      * @param exp  the function being called.
      * @param args the function arguments.
      * @param tpe  the return type of the function.
      * @param loc  the source location of the expression.
      */
    case class Apply(exp: SimplifiedAst.Expression,
                     args: List[SimplifiedAst.Expression],
                     tpe: Type,
                     loc: SourceLocation) extends SimplifiedAst.Expression {
      override def toString: String = "Apply(" + exp + ", [" + args.mkString(",") + "])"
    }

    /**
      * A typed AST node representing a unary expression.
      *
      * @param op  the unary operator.
      * @param exp the expression.
      * @param tpe the type of the expression.
      * @param loc the source location of the expression.
      */
    case class Unary(op: UnaryOperator,
                     exp: SimplifiedAst.Expression,
                     tpe: Type,
                     loc: SourceLocation) extends SimplifiedAst.Expression {
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
                      exp1: SimplifiedAst.Expression,
                      exp2: SimplifiedAst.Expression,
                      tpe: Type,
                      loc: SourceLocation) extends SimplifiedAst.Expression {
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
    case class IfThenElse(exp1: SimplifiedAst.Expression,
                          exp2: SimplifiedAst.Expression,
                          exp3: SimplifiedAst.Expression,
                          tpe: Type,
                          loc: SourceLocation) extends SimplifiedAst.Expression {
      // TODO: Built infrastructure for this.
      // type invariants.
      assert(exp1.tpe == Type.Bool, s"Expected Type.Bool but got '${exp1.tpe}'.")
      assert(exp2.tpe == exp3.tpe, s"Expected equal types, but got '${exp1.tpe}' and '${exp2.tpe}'.")

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
                   exp1: SimplifiedAst.Expression,
                   exp2: SimplifiedAst.Expression,
                   tpe: Type,
                   loc: SourceLocation) extends SimplifiedAst.Expression {
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
                        exp: SimplifiedAst.Expression,
                        loc: SourceLocation) extends SimplifiedAst.Expression {
      final val tpe: Type = Type.Bool

      assert(exp.tpe.isInstanceOf[Type.Enum], s"CheckTag expects an expression if Type.Enum, but got '${exp.tpe}'.")

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
                           exp: SimplifiedAst.Expression,
                           tpe: Type,
                           loc: SourceLocation) extends SimplifiedAst.Expression {

      assert(exp.tpe.isInstanceOf[Type.Enum], s"GetTagValue expects an expression of Type.Enum, but got '${exp.tpe}'.")
      assert(exp.tpe.asInstanceOf[Type.Enum].cases(tag.name).tpe == tpe, s"GetTagValue type mismatch.")

      // TODO: Remove once bug is fixed.
      exp match {
        case Tag(_, tag1, exp1, _, _) =>
          assert(tag.name == tag1.name)
          assert(tpe == exp1.tpe)
        case _ => // nop
      }

      override def toString: String = "GetTagValue(" + tag.name + ", " + exp + ")"
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
                   exp: SimplifiedAst.Expression,
                   tpe: Type.Enum,
                   loc: SourceLocation) extends SimplifiedAst.Expression {
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
    case class GetTupleIndex(base: SimplifiedAst.Expression,
                             offset: scala.Int,
                             tpe: Type,
                             loc: SourceLocation) extends SimplifiedAst.Expression {

      assert(base.tpe.isInstanceOf[Type.Tuple], s"GetTupleIndex expects an expression of Type.Tuple, but got '${base.tpe}'.")

      override def toString: String = base + "[" + offset + "]"
    }

    /**
      * A typed AST node representing a tuple expression.
      *
      * @param elms the elements of the tuple.
      * @param tpe  the type of the tuple.
      * @param loc  the source location of the tuple.
      */
    case class Tuple(elms: List[SimplifiedAst.Expression],
                     tpe: Type.Tuple,
                     loc: SourceLocation) extends SimplifiedAst.Expression {
      override def toString: String = "(" + elms.mkString(", ") + ")"
    }

    case class CheckNil(exp: SimplifiedAst.Expression, loc: SourceLocation) extends SimplifiedAst.Expression {
      final val tpe: Type = Type.Bool
    }

    case class CheckCons(exp: SimplifiedAst.Expression, loc: SourceLocation) extends SimplifiedAst.Expression {
      final val tpe: Type = Type.Bool
    }

    case class FSet(elms: List[SimplifiedAst.Expression],
                    tpe: Type.FSet,
                    loc: SourceLocation) extends SimplifiedAst.Expression

    case class Existential(params: List[Ast.FormalParam], exp: SimplifiedAst.Expression, loc: SourceLocation) extends SimplifiedAst.Expression {
      def tpe: Type = Type.Bool
    }

    case class Universal(params: List[Ast.FormalParam], exp: SimplifiedAst.Expression, loc: SourceLocation) extends SimplifiedAst.Expression {
      def tpe: Type = Type.Bool
    }

    /**
      * A typed AST node representing an error.
      *
      * @param tpe the type of the error.
      * @param loc the source location of the error.
      */
    case class UserError(tpe: Type, loc: SourceLocation) extends SimplifiedAst.Expression {
      override def toString: String = "Error"
    }

    /**
      * A typed AST node representing a match error.
      *
      * @param tpe the type of the error.
      * @param loc the source location of the error.
      */
    case class MatchError(tpe: Type, loc: SourceLocation) extends SimplifiedAst.Expression {
      override def toString: String = "MatchError"
    }

    /**
      * A typed AST node representing a switch error.
      *
      * @param tpe the type of the error.
      * @param loc the source location of the error.
      */
    case class SwitchError(tpe: Type, loc: SourceLocation) extends SimplifiedAst.Expression {
      override def toString: String = "SwitchError"
    }

  }

  sealed trait Predicate extends SimplifiedAst {
    def tpe: Type

    def loc: SourceLocation
  }

  object Predicate {

    sealed trait Head extends SimplifiedAst.Predicate

    object Head {

      case class True(loc: SourceLocation) extends SimplifiedAst.Predicate.Head {
        def tpe: Type = Type.Predicate(Nil)
      }

      case class False(loc: SourceLocation) extends SimplifiedAst.Predicate.Head {
        def tpe: Type = Type.Predicate(Nil)
      }

      case class Table(sym: Symbol.TableSym,
                       terms: List[SimplifiedAst.Term.Head],
                       tpe: Type.Predicate,
                       loc: SourceLocation) extends SimplifiedAst.Predicate.Head

    }

    sealed trait Body extends SimplifiedAst.Predicate

    object Body {

      case class Table(sym: Symbol.TableSym,
                       terms: List[SimplifiedAst.Term.Body],
                       tpe: Type.Predicate,
                       loc: SourceLocation) extends SimplifiedAst.Predicate.Body

      case class ApplyFilter(name: Symbol.Resolved,
                             terms: List[SimplifiedAst.Term.Body],
                             tpe: Type.Lambda,
                             loc: SourceLocation) extends SimplifiedAst.Predicate.Body

      case class ApplyHookFilter(hook: Ast.Hook,
                                 terms: List[SimplifiedAst.Term.Body],
                                 tpe: Type.Lambda,
                                 loc: SourceLocation) extends SimplifiedAst.Predicate.Body

      case class NotEqual(ident1: Name.Ident,
                          ident2: Name.Ident,
                          tpe: Type,
                          loc: SourceLocation) extends SimplifiedAst.Predicate.Body

      case class Loop(ident: Name.Ident,
                      term: SimplifiedAst.Term.Head,
                      tpe: Type,
                      loc: SourceLocation) extends SimplifiedAst.Predicate.Body

    }

  }

  object Term {

    sealed trait Head extends SimplifiedAst {
      def tpe: Type

      def loc: SourceLocation
    }

    object Head {

      case class Var(ident: Name.Ident, tpe: Type, loc: SourceLocation) extends SimplifiedAst.Term.Head

      // TODO: Lambda lift?
      case class Exp(literal: SimplifiedAst.Expression, tpe: Type, loc: SourceLocation) extends SimplifiedAst.Term.Head

      // TODO: Can we get rid of this?
      case class Apply(name: Symbol.Resolved,
                       args: List[SimplifiedAst.Term.Head],
                       tpe: Type,
                       loc: SourceLocation) extends SimplifiedAst.Term.Head {

      }

      // TODO: To be replaced.
      case class ApplyHook(hook: Ast.Hook,
                           args: List[SimplifiedAst.Term.Head],
                           tpe: Type,
                           loc: SourceLocation) extends SimplifiedAst.Term.Head

    }

    sealed trait Body extends SimplifiedAst {
      def tpe: Type

      def loc: SourceLocation
    }

    object Body {

      case class Wildcard(tpe: Type, loc: SourceLocation) extends SimplifiedAst.Term.Body

      case class Var(ident: Name.Ident, v: scala.Int, tpe: Type, loc: SourceLocation) extends SimplifiedAst.Term.Body

      // TODO: Lambda lift?
      case class Exp(e: SimplifiedAst.Expression, tpe: Type, loc: SourceLocation) extends SimplifiedAst.Term.Body

    }

  }

  case class Attribute(ident: Name.Ident, tpe: Type) extends SimplifiedAst

  case class FormalArg(ident: Name.Ident, tpe: Type) extends SimplifiedAst

  case class FreeVar(ident: Name.Ident, offset: Int, tpe: Type) extends SimplifiedAst

  case class Property(law: Law, exp: SimplifiedAst.Expression, loc: SourceLocation) extends SimplifiedAst


  /**
   * A trait to consume a SimplifiedAst.Expression
   * and produce a new Expression that's modified
   * in some way. Typical usage is to extend the
   * trait and override which methods corresponding
   * to whatever case of Expression you're interested
   * in and then call the `foldExpression` method
   * to begin modifying a given expression.
   */
  trait ExpressionFolder {

    /**
     * This is the entry point for the folder. It recursively visits every Expression
     * calling out to the overridable fold* methods.
     *
     * @param expr The expression to fold over and potentially modify.
     * @return The potentially modified Expression.
     */
    final def foldExpression(expr: Expression): Expression = expr match {
      case Expression.Unit => foldUnit()

      case Expression.True => foldTrue()
      case Expression.False => foldFalse()

      case Expression.Char(lit) => foldChar(lit)

      case Expression.Float32(lit) => foldFloat32(lit)
      case Expression.Float64(lit) => foldFloat64(lit)

      case Expression.Int8(lit) => foldInt8(lit)
      case Expression.Int16(lit) => foldInt16(lit)
      case Expression.Int32(lit) => foldInt32(lit)
      case Expression.Int64(lit) => foldInt64(lit)
      case Expression.BigInt(lit) => foldBigInt(lit)

      case Expression.Str(lit) => foldStr(lit)

      case Expression.LoadBool(e, offset) => foldLoadBool(e, offset)
      case Expression.LoadInt8(e, offset) => foldLoadInt8(e, offset)
      case Expression.LoadInt16(e, offset) => foldLoadInt16(e, offset)
      case Expression.LoadInt32(e, offset) => foldLoadInt32(e, offset)

      case Expression.StoreBool(e, offset, v) => foldStoreBool(e, offset, v)
      case Expression.StoreInt8(e, offset, v) => foldStoreInt8(e, offset, v)
      case Expression.StoreInt16(e, offset, v) => foldStoreInt16(e, offset, v)
      case Expression.StoreInt32(e, offset, v) => foldStoreInt32(e, offset, v)

      case Expression.Var(ident, offset, tpe, loc) => foldVar(ident, offset, tpe, loc)

      case Expression.Ref(name, tpe, loc) => foldRef(name, tpe, loc)

      case Expression.Lambda(args, body, tpe, loc) => foldLambda(args, body, tpe, loc)

      case Expression.Hook(hook, tpe, loc) => foldHook(hook, tpe, loc)

      case Expression.MkClosure(lambda, freeVars, tpe, loc) => foldMkClosure(lambda, freeVars, tpe, loc)
      case Expression.MkClosureRef(ref, freeVars, tpe, loc) => foldMkClosureRef(ref, freeVars, tpe, loc)

      case Expression.ApplyRef(name, args, tpe, loc) => foldApplyRef(name, args, tpe, loc)
      case Expression.ApplyHook(hook, args, tpe, loc) => foldApplyHook(hook, args, tpe, loc)
      case Expression.Apply(exp, args, tpe, loc) => foldApply(exp, args, tpe, loc)

      case Expression.Unary(op, exp, tpe, loc) => foldUnary(op, exp, tpe, loc)
      case Expression.Binary(op, exp1, exp2, tpe, loc) => foldBinary(op, exp1, exp2, tpe, loc)

      case Expression.IfThenElse(exp1, exp2, exp3, tpe, loc) => foldIfThenElse(exp1, exp2, exp3, tpe, loc)

      case Expression.Let(ident, offset, exp1, exp2, tpe, loc) => foldLet(ident, offset, exp1, exp2, tpe, loc)

      case Expression.CheckTag(tag, exp, loc) => foldCheckTag(tag, exp, loc)
      case Expression.GetTagValue(tag, exp, tpe, loc) => foldGetTagValue(tag, exp, tpe, loc)
      case Expression.Tag(enum, tag, exp, tpe, loc) => foldTag(enum, tag, exp, tpe, loc)

      case Expression.GetTupleIndex(base, offset, tpe, loc) => foldGetTupleIndex(base, offset, tpe, loc)
      case Expression.Tuple(elms, tpe, loc) => foldTuple(elms, tpe, loc)

      case Expression.CheckNil(exp, loc) => foldCheckNil(exp, loc)
      case Expression.CheckCons(exp, loc) => foldCheckCons(exp, loc)

      case Expression.FSet(elms, tpe, loc) => foldFSet(elms, tpe, loc)

      case Expression.Existential(params, exp, loc) => foldExistential(params, exp, loc)
      case Expression.Universal(params, exp, loc) => foldUniversal(params, exp, loc)

      case Expression.UserError(tpe, loc) => foldUserError(tpe, loc)
      case Expression.MatchError(tpe, loc) => foldMatchError(tpe, loc)
      case Expression.SwitchError(tpe, loc) => foldSwitchError(tpe, loc)
    }

    def foldUnit(): Expression = Expression.Unit

    def foldTrue(): Expression = Expression.True
    def foldFalse(): Expression = Expression.False

    def foldChar(lit: scala.Char): Expression = Expression.Char(lit)

    def foldFloat32(lit: scala.Float): Expression = Expression.Float32(lit)
    def foldFloat64(lit: scala.Double): Expression = Expression.Float64(lit)

    def foldInt8(lit: scala.Byte): Expression = Expression.Int8(lit)
    def foldInt16(lit: scala.Short): Expression = Expression.Int16(lit)
    def foldInt32(lit: scala.Int): Expression = Expression.Int32(lit)
    def foldInt64(lit: scala.Long): Expression = Expression.Int64(lit)
    def foldBigInt(lit: java.math.BigInteger): Expression = Expression.BigInt(lit)

    def foldStr(lit: java.lang.String): Expression = Expression.Str(lit)

    def foldLoadBool(e: Expression, offset: scala.Int): Expression = {
      val fe = foldExpression(e)
      Expression.LoadBool(fe, offset)
    }

    def foldLoadInt8(e: Expression, offset: scala.Int): Expression = {
      val fe = foldExpression(e)
      Expression.LoadInt8(fe, offset)
    }

    def foldLoadInt16(e: Expression, offset: scala.Int): Expression = {
      val fe = foldExpression(e)
      Expression.LoadInt16(fe, offset)
    }

    def foldLoadInt32(e: Expression, offset: scala.Int): Expression = {
      val fe = foldExpression(e)
      Expression.LoadInt32(fe, offset)
    }

    def foldStoreBool(e: Expression,
                      offset: scala.Int,
                      v: Expression): Expression = {
      val fe = foldExpression(e)
      val fv = foldExpression(v)
      Expression.StoreBool(fe, offset, fv)
    }

    def foldStoreInt8(e: Expression,
                      offset: scala.Int,
                      v: Expression): Expression = {
      val fe = foldExpression(e)
      val fv = foldExpression(v)
      Expression.StoreInt8(fe, offset, fv)
    }

    def foldStoreInt16(e: Expression,
                      offset: scala.Int,
                      v: Expression): Expression = {
      val fe = foldExpression(e)
      val fv = foldExpression(v)
      Expression.StoreInt16(fe, offset, fv)
    }

    def foldStoreInt32(e: Expression,
                      offset: scala.Int,
                      v: Expression): Expression = {
      val fe = foldExpression(e)
      val fv = foldExpression(v)
      Expression.StoreInt32(fe, offset, fv)
    }

    def foldVar(ident: Name.Ident,
                offset: scala.Int,
                tpe: Type,
                loc: SourceLocation): Expression = Expression.Var(ident, offset, tpe, loc)

    def foldRef(name: Symbol.Resolved,
                tpe: Type,
                loc: SourceLocation): Expression = Expression.Ref(name, tpe, loc)

    def foldLambda(args: List[SimplifiedAst.FormalArg],
                   body: SimplifiedAst.Expression,
                   tpe: Type.Lambda,
                   loc: SourceLocation): Expression = {
      val fbody = foldExpression(body)
      Expression.Lambda(args, fbody, tpe, loc)
    }

    def foldHook(hook: Ast.Hook,
                 tpe: Type,
                 loc: SourceLocation): Expression = Expression.Hook(hook, tpe, loc)

    def foldMkClosure(lambda: SimplifiedAst.Expression.Lambda,
                      freeVars: List[FreeVar],
                      tpe: Type.Lambda,
                      loc: SourceLocation): Expression = {
      val flambda = lambda.copy(body = foldExpression(lambda.body))
      Expression.MkClosure(flambda, freeVars, tpe, loc)
    }

    def foldMkClosureRef(ref: SimplifiedAst.Expression.Ref,
                         freeVars: List[FreeVar],
                         tpe: Type.Lambda,
                         loc: SourceLocation): Expression = Expression.MkClosureRef(ref, freeVars, tpe, loc)

    def foldApplyRef(name: Symbol.Resolved,
                     args: List[SimplifiedAst.Expression],
                     tpe: Type,
                     loc: SourceLocation): Expression = {
      val fargs = args.map(exp => foldExpression(exp))
      Expression.ApplyRef(name, fargs, tpe, loc)
    }

    def foldApplyHook(hook: Ast.Hook,
                      args: List[SimplifiedAst.Expression],
                      tpe: Type,
                      loc: SourceLocation): Expression = {
      val fargs = args.map(exp => foldExpression(exp))
      Expression.ApplyHook(hook, fargs, tpe, loc)
    }

    def foldApply(exp: SimplifiedAst.Expression,
                  args: List[SimplifiedAst.Expression],
                  tpe: Type,
                  loc: SourceLocation): Expression = {
      val fexp = foldExpression(exp)
      val fargs = args.map(exp => foldExpression(exp))
      Expression.Apply(fexp, fargs, tpe, loc)
    }

    def foldUnary(op: UnaryOperator,
                  exp: SimplifiedAst.Expression,
                  tpe: Type,
                  loc: SourceLocation): Expression = {
      val fexp = foldExpression(exp)
      Expression.Unary(op, fexp, tpe, loc)
    }

    def foldBinary(op: BinaryOperator,
                   exp1: SimplifiedAst.Expression,
                   exp2: SimplifiedAst.Expression,
                   tpe: Type,
                   loc: SourceLocation): Expression = {
      val fexp1 = foldExpression(exp1)
      val fexp2 = foldExpression(exp2)
      Expression.Binary(op, fexp1, fexp2, tpe, loc)
    }

    def foldIfThenElse(exp1: SimplifiedAst.Expression,
                       exp2: SimplifiedAst.Expression,
                       exp3: SimplifiedAst.Expression,
                       tpe: Type,
                       loc: SourceLocation): Expression = {
      val fexp1 = foldExpression(exp1)
      val fexp2 = foldExpression(exp2)
      val fexp3 = foldExpression(exp3)
      Expression.IfThenElse(fexp1, fexp2, fexp3, tpe, loc)
    }

    def foldLet(ident: Name.Ident,
                offset: scala.Int,
                exp1: SimplifiedAst.Expression,
                exp2: SimplifiedAst.Expression,
                tpe: Type,
                loc: SourceLocation): Expression = {
      val fexp1 = foldExpression(exp1)
      val fexp2 = foldExpression(exp2)
      Expression.Let(ident, offset, fexp1, fexp2, tpe, loc)
    }

    def foldCheckTag(tag: Name.Ident,
                     exp: SimplifiedAst.Expression,
                     loc: SourceLocation): Expression = {
      val fexp = foldExpression(exp)
      Expression.CheckTag(tag, fexp, loc)
    }

    def foldGetTagValue(tag: Name.Ident,
                        exp: SimplifiedAst.Expression,
                        tpe: Type,
                        loc: SourceLocation): Expression = {
      val fexp = foldExpression(exp)
      Expression.GetTagValue(tag, fexp, tpe, loc)
    }

    def foldTag(enum: Symbol.Resolved,
                tag: Name.Ident,
                exp: SimplifiedAst.Expression,
                tpe: Type.Enum,
                loc: SourceLocation): Expression = {
      val fexp = foldExpression(exp)
      Expression.Tag(enum, tag, fexp, tpe, loc)
    }

    def foldGetTupleIndex(base: SimplifiedAst.Expression,
                          offset: scala.Int,
                          tpe: Type,
                          loc: SourceLocation): Expression = {
      val fbase = foldExpression(base)
      Expression.GetTupleIndex(fbase, offset, tpe, loc)
    }

    def foldTuple(elms: List[SimplifiedAst.Expression],
                  tpe: Type.Tuple,
                  loc: SourceLocation): Expression = {
      val felms = elms.map(exp => foldExpression(exp))
      Expression.Tuple(felms, tpe, loc)
    }

    def foldCheckNil(exp: SimplifiedAst.Expression,
                     loc: SourceLocation): Expression = {
      val fexp = foldExpression(exp)
      Expression.CheckNil(fexp, loc)
    }

    def foldCheckCons(exp: SimplifiedAst.Expression,
                     loc: SourceLocation): Expression = {
      val fexp = foldExpression(exp)
      Expression.CheckCons(fexp, loc)
    }

    def foldFSet(elms: List[SimplifiedAst.Expression],
                 tpe: Type.FSet,
                 loc: SourceLocation): Expression = {
      val felms = elms.map(exp => foldExpression(exp))
      Expression.FSet(felms, tpe, loc)
    }

    def foldExistential(params: List[Ast.FormalParam],
                        exp: SimplifiedAst.Expression,
                        loc: SourceLocation): Expression = {
      val fexp = foldExpression(exp)
      Expression.Existential(params, fexp, loc)
    }

    def foldUniversal(params: List[Ast.FormalParam],
                      exp: SimplifiedAst.Expression,
                      loc: SourceLocation): Expression = {
      val fexp = foldExpression(exp)
      Expression.Universal(params, fexp, loc)
    }

    def foldUserError(tpe: Type, loc: SourceLocation): Expression = Expression.UserError(tpe, loc)
    def foldMatchError(tpe: Type, loc: SourceLocation): Expression = Expression.MatchError(tpe, loc)
    def foldSwitchError(tpe: Type, loc: SourceLocation): Expression = Expression.SwitchError(tpe, loc)

  }
}

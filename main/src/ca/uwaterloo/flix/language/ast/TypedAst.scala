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

/**
  * A common super-type for typed AST nodes.
  */
sealed trait TypedAst

object TypedAst {

  /**
    * A typed AST node representing the root of the entire AST.
    *
    * @param constants a map from names to constant definitions.
    * @param lattices  a map from types to user-specified bounded lattice definitions.
    * @param tables    a map from names to lattice or relation definitions.
    * @param indexes   a map from table symbols to indexes.
    * @param facts     a list of facts.
    * @param rules     a list of rules.
    * @param hooks     a map from names to hooks.
    * @param time      the time spent in each compiler phase.
    */
  case class Root(constants: Map[Symbol.Resolved, TypedAst.Definition.Constant],
                  lattices: Map[Type, TypedAst.Definition.BoundedLattice],
                  tables: Map[Symbol.TableSym, TypedAst.Table],
                  indexes: Map[Symbol.TableSym, TypedAst.Definition.Index],
                  facts: List[TypedAst.Constraint.Fact],
                  rules: List[TypedAst.Constraint.Rule],
                  hooks: Map[Symbol.Resolved, Ast.Hook],
                  properties: List[TypedAst.Property],
                  time: Time) extends TypedAst

  /**
    * A common super-type for typed definitions.
    */
  sealed trait Definition

  object Definition {

    /**
      * A typed AST node representing a constant definition.
      *
      * @param name the name of the constant.
      * @param exp  the constant expression.
      * @param tpe  the type of the constant.
      * @param loc  the source location.
      */
    case class Constant(ann: Ast.Annotations, name: Symbol.Resolved, formals: List[TypedAst.FormalArg], exp: TypedAst.Expression, tpe: Type, loc: SourceLocation) extends TypedAst.Definition

    /**
      * A typed AST node representing a bounded lattice definition.
      *
      * @param tpe the type of the lattice elements.
      * @param bot the bot element.
      * @param top the top element.
      * @param leq the partial order.
      * @param lub the least upper bound.
      * @param glb the greatest lower bound.
      * @param loc the source location.
      */
    case class BoundedLattice(tpe: Type,
                              bot: TypedAst.Expression,
                              top: TypedAst.Expression,
                              leq: TypedAst.Expression,
                              lub: TypedAst.Expression,
                              glb: TypedAst.Expression,
                              loc: SourceLocation) extends TypedAst.Definition

    /**
      * A typed AST node representing an index definition.
      *
      * @param sym     the symbol of the collection.
      * @param indexes the selected indexes.
      * @param loc     the source location.
      */
    case class Index(sym: Symbol.TableSym, indexes: Seq[Seq[Name.Ident]], loc: SourceLocation) extends TypedAst.Definition

  }

  /**
    * A common super-type for tables that are either relations or lattices.
    */
  sealed trait Table

  object Table {

    /**
      * A typed AST node representing a relation definition.
      *
      * @param sym        the symbol of the relation.
      * @param attributes the attributes of the relation.
      * @param loc        the source location.
      */
    case class Relation(sym: Symbol.TableSym, attributes: List[TypedAst.Attribute], loc: SourceLocation) extends TypedAst.Table

    /**
      * A typed AST node representing a lattice definition.
      *
      * @param sym   the symbol of the relation.
      * @param keys  the keys of the lattice.
      * @param value the value of the lattice.
      * @param loc   the source location.
      */
    case class Lattice(sym: Symbol.TableSym, keys: List[TypedAst.Attribute], value: TypedAst.Attribute, loc: SourceLocation) extends TypedAst.Table

  }


  /**
    * A common super-type for typed facts and rules.
    */
  sealed trait Constraint extends TypedAst

  object Constraint {

    /**
      * A typed AST node representing a fact declaration.
      *
      * @param head the head predicate.
      */
    case class Fact(head: TypedAst.Predicate.Head) extends TypedAst.Constraint

    /**
      * A typed AST node representing a rule declaration.
      *
      * @param head the head predicate.
      * @param body the body predicates.
      */
    case class Rule(head: TypedAst.Predicate.Head, body: List[TypedAst.Predicate.Body]) extends TypedAst.Constraint

  }

  /**
    * A common super-type for typed literals.
    */
  sealed trait Literal extends TypedAst {
    /**
      * The type of `this` literal.
      */
    def tpe: Type

    /**
      * The source location of `this` literal.
      */
    def loc: SourceLocation
  }

  object Literal {

    /**
      * A typed AST node representing the unit literal.
      *
      * @param loc the source location.
      */
    case class Unit(loc: SourceLocation) extends TypedAst.Literal {
      final val tpe = Type.Unit
    }

    /**
      * A typed AST node representing a boolean literal.
      *
      * @param lit the boolean literal.
      * @param loc the source location.
      */
    case class Bool(lit: scala.Boolean, loc: SourceLocation) extends TypedAst.Literal {
      final val tpe = Type.Bool
    }

    /**
      * A typed AST node that represent a char literal.
      *
      * @param lit the char literal.
      * @param loc the source location.
      */
    case class Char(lit: scala.Char, loc: SourceLocation) extends TypedAst.Literal {
      final val tpe = Type.Char
    }

    /**
      * A typed AST node that represent a float32 literal.
      *
      * @param lit the float32 literal.
      * @param loc the source location.
      */
    case class Float32(lit: scala.Float, loc: SourceLocation) extends TypedAst.Literal {
      final val tpe = Type.Float32
    }

    /**
      * A typed AST node that represent a float64 literal.
      *
      * @param lit the float64 literal.
      * @param loc the source location.
      */
    case class Float64(lit: scala.Double, loc: SourceLocation) extends TypedAst.Literal {
      final val tpe = Type.Float64
    }

    /**
      * A typed AST node that represent an int8 literal.
      *
      * @param lit the int8 literal.
      * @param loc the source location.
      */
    case class Int8(lit: scala.Byte, loc: SourceLocation) extends TypedAst.Literal {
      final val tpe = Type.Int8
    }

    /**
      * A typed AST node that represent an int16 literal.
      *
      * @param lit the int16 literal.
      * @param loc the source location.
      */
    case class Int16(lit: scala.Short, loc: SourceLocation) extends TypedAst.Literal {
      final val tpe = Type.Int16
    }

    /**
      * A typed AST node that represent an int32 literal.
      *
      * @param lit the int32 literal.
      * @param loc the source location.
      */
    case class Int32(lit: scala.Int, loc: SourceLocation) extends TypedAst.Literal {
      final val tpe = Type.Int32
    }

    /**
      * A typed AST node that represent an int64 literal.
      *
      * @param lit the int64 literal.
      * @param loc the source location.
      */
    case class Int64(lit: scala.Long, loc: SourceLocation) extends TypedAst.Literal {
      final val tpe = Type.Int64
    }

    /**
      * A typed AST node that represent a big int literal.
      *
      * @param lit the integer literal.
      * @param loc the source location.
      */
    case class BigInt(lit: java.math.BigInteger, loc: SourceLocation) extends TypedAst.Literal {
      final val tpe = Type.BigInt
    }

    /**
      * A typed AST node representing a string literal.
      *
      * @param lit the string literal.
      * @param loc the source location.
      */
    case class Str(lit: java.lang.String, loc: SourceLocation) extends TypedAst.Literal {
      final val tpe = Type.Str
    }

  }

  sealed trait Expression extends TypedAst {
    /**
      * The type of `this` expression.
      */
    def tpe: Type

    /**
      * The source location of `this` expression.
      */
    def loc: SourceLocation
  }

  object Expression {

    case class Unit(loc: SourceLocation) extends TypedAst.Expression {
      final def tpe: Type = Type.Unit
    }

    case class True(loc: SourceLocation) extends TypedAst.Expression {
      final def tpe: Type = Type.Bool
    }

    case class False(loc: SourceLocation) extends TypedAst.Expression {
      final def tpe: Type = Type.Bool
    }

    case class Char(lit: scala.Char, loc: SourceLocation) extends TypedAst.Expression {
      final def tpe: Type = Type.Char
    }

    case class Float32(lit: scala.Float, loc: SourceLocation) extends TypedAst.Expression {
      final def tpe: Type = Type.Float32
    }

    case class Float64(lit: scala.Double, loc: SourceLocation) extends TypedAst.Expression {
      final def tpe: Type = Type.Float64
    }

    case class Int8(lit: scala.Byte, loc: SourceLocation) extends TypedAst.Expression {
      final def tpe: Type = Type.Int8
    }

    case class Int16(lit: scala.Short, loc: SourceLocation) extends TypedAst.Expression {
      final def tpe: Type = Type.Int16
    }

    case class Int32(lit: scala.Int, loc: SourceLocation) extends TypedAst.Expression {
      final def tpe: Type = Type.Int32
    }

    case class Int64(lit: scala.Long, loc: SourceLocation) extends TypedAst.Expression {
      final def tpe: Type = Type.Int64
    }

    case class BigInt(lit: java.math.BigInteger, loc: SourceLocation) extends TypedAst.Expression {
      final def tpe: Type = Type.BigInt
    }

    case class Str(lit: java.lang.String, loc: SourceLocation) extends TypedAst.Expression {
      final def tpe: Type = Type.Str
    }

    /**
      * A typed AST node representing a literal expression.
      *
      * @param literal the literal.
      * @param tpe     the type of the literal.
      * @param loc     the source location.
      */
    case class Lit(literal: TypedAst.Literal, tpe: Type, loc: SourceLocation) extends TypedAst.Expression

    /**
      * A typed AST node representing a local variable expression (i.e. a parameter or let-bound variable).
      *
      * @param ident the name of the variable.
      * @param tpe   the type of the variable.
      */
    case class Var(ident: Name.Ident, tpe: Type, loc: SourceLocation) extends TypedAst.Expression

    /**
      * A typed AST node representing a reference to a definition (i.e. a value or function).
      *
      * @param name the name of the definition.
      * @param tpe  the type of the definition.
      * @param loc  the source location.
      */
    case class Ref(name: Symbol.Resolved, tpe: Type, loc: SourceLocation) extends TypedAst.Expression

    /**
      * A typed AST node representing a reference to a native JVM function.
      *
      * @param hook the native hook.
      * @param tpe  the type of native function.
      * @param loc  the source location.
      */
    case class Hook(hook: Ast.Hook, tpe: Type, loc: SourceLocation) extends TypedAst.Expression

    /**
      * A typed AST node representing a lambda abstraction.
      *
      * @param args the formal arguments.
      * @param body the body expression of the lambda.
      * @param tpe  the type of the entire function.
      * @param loc  the source location.
      */
    case class Lambda(args: List[TypedAst.FormalArg], body: TypedAst.Expression, tpe: Type.Lambda, loc: SourceLocation) extends TypedAst.Expression

    /**
      * A typed AST node representing a function call.
      *
      * @param exp  the lambda/function expression.
      * @param args the function arguments.
      * @param tpe  the return type of the function.
      * @param loc  the source location.
      */
    case class Apply(exp: TypedAst.Expression, args: List[TypedAst.Expression], tpe: Type, loc: SourceLocation) extends TypedAst.Expression

    /**
      * A typed AST node representing a unary expression.
      *
      * @param op  the unary operator.
      * @param exp the expression.
      * @param tpe the type
      * @param loc the source location.
      */
    case class Unary(op: UnaryOperator, exp: TypedAst.Expression, tpe: Type, loc: SourceLocation) extends TypedAst.Expression

    /**
      * A typed AST node representing a binary expression.
      *
      * @param op   the binary operator.
      * @param exp1 the lhs expression.
      * @param exp2 the rhs expression.
      * @param tpe  the type of the expression.
      * @param loc  the source location.
      */
    case class Binary(op: BinaryOperator, exp1: TypedAst.Expression, exp2: TypedAst.Expression, tpe: Type, loc: SourceLocation) extends TypedAst.Expression

    /**
      * A typed AST node representing a let expression.
      *
      * @param ident the name of the bound variable.
      * @param exp1  the value of the bound variable.
      * @param exp2  the body expression in which the bound variable is visible.
      * @param tpe   the type of the expression (which is equivalent to the type of the body expression).
      * @param loc   the source location.
      */
    case class Let(ident: Name.Ident, exp1: TypedAst.Expression, exp2: TypedAst.Expression, tpe: Type, loc: SourceLocation) extends TypedAst.Expression

    /**
      * A typed AST node representing an if-then-else expression.
      *
      * @param exp1 the conditional expression.
      * @param exp2 the consequent expression.
      * @param exp3 the alternative expression.
      * @param tpe  the type of the consequent and alternative expressions.
      * @param loc  the source location.
      */
    case class IfThenElse(exp1: TypedAst.Expression, exp2: TypedAst.Expression, exp3: TypedAst.Expression, tpe: Type, loc: SourceLocation) extends TypedAst.Expression

    /**
      * A typed AST node representing a match expression.
      *
      * @param exp   the match expression.
      * @param rules the match rules.
      * @param tpe   the type of the match expression (which is equivalent to the type of each rule).
      * @param loc   the source location.
      */
    case class Match(exp: TypedAst.Expression, rules: List[(TypedAst.Pattern, TypedAst.Expression)], tpe: Type, loc: SourceLocation) extends TypedAst.Expression

    /**
      * A typed AST node representing a switch expression.
      *
      * @param rules the rules of the switch.
      * @param tpe   the type of the rule bodies.
      * @param loc   the source location.
      */
    case class Switch(rules: List[(TypedAst.Expression, TypedAst.Expression)], tpe: Type, loc: SourceLocation) extends TypedAst.Expression

    /**
      * A typed AST node representing a tagged expression.
      *
      * @param name  the name of the enum.
      * @param ident the name of the tag.
      * @param exp   the expression.
      * @param tpe   the type of the expression.
      * @param loc   the source location.
      */
    case class Tag(name: Symbol.Resolved, ident: Name.Ident, exp: TypedAst.Expression, tpe: Type.Enum, loc: SourceLocation) extends TypedAst.Expression

    /**
      * A typed AST node representing a tuple expression.
      *
      * @param elms the elements of the tuple.
      * @param tpe  the type of the tuple.
      * @param loc  the source location.
      */
    case class Tuple(elms: List[TypedAst.Expression], tpe: Type.Tuple, loc: SourceLocation) extends TypedAst.Expression

    case class FNone(tpe: Type.FOpt, loc: SourceLocation) extends TypedAst.Expression

    case class FSome(exp: TypedAst.Expression, tpe: Type.FOpt, loc: SourceLocation) extends TypedAst.Expression

    case class FNil(tpe: Type.FList, loc: SourceLocation) extends TypedAst.Expression

    case class FList(hd: TypedAst.Expression, tl: TypedAst.Expression, tpe: Type.FList, loc: SourceLocation) extends TypedAst.Expression

    case class FVec(elms: List[TypedAst.Expression], tpe: Type.FVec, loc: SourceLocation) extends TypedAst.Expression

    case class FSet(elms: List[TypedAst.Expression], tpe: Type.FSet, loc: SourceLocation) extends TypedAst.Expression

    case class FMap(elms: List[(TypedAst.Expression, TypedAst.Expression)], tpe: Type.FMap, loc: SourceLocation) extends TypedAst.Expression

    case class GetIndex(exp1: TypedAst.Expression, exp2: TypedAst.Expression, tpe: Type, loc: SourceLocation) extends TypedAst.Expression

    case class PutIndex(exp1: TypedAst.Expression, exp2: TypedAst.Expression, exp3: TypedAst.Expression, tpe: Type, loc: SourceLocation) extends TypedAst.Expression


    case class Existential(params: List[Ast.FormalParam], exp: TypedAst.Expression, loc: SourceLocation) extends TypedAst.Expression {
      def tpe: Type = Type.Bool
    }

    case class Universal(params: List[Ast.FormalParam], exp: TypedAst.Expression, loc: SourceLocation) extends TypedAst.Expression {
      def tpe: Type = Type.Bool
    }

    /**
      * A typed AST node representing an error expression.
      *
      * @param tpe the type of the error expression.
      * @param loc the source location.
      */
    case class Error(tpe: Type, loc: SourceLocation) extends TypedAst.Expression

  }

  /**
    * A common-super type for typed patterns.
    */
  sealed trait Pattern extends TypedAst {
    /**
      * The type of `this` pattern.
      */
    def tpe: Type

    /**
      * The source location of `this` pattern.
      */
    def loc: SourceLocation

    /**
      * Returns the free variables (along with their types) in `this` pattern.
      */
    def freeVars: Map[String, Type] = {
      def visit(pat: TypedAst.Pattern, m: Map[String, Type]): Map[String, Type] =
        pat match {
          case TypedAst.Pattern.Wildcard(_, _) => m
          case TypedAst.Pattern.Var(ident, tpe, _) => m + (ident.name -> tpe)
          case TypedAst.Pattern.Lit(_, _, _) => m
          case TypedAst.Pattern.Tag(_, _, pat2, _, _) => visit(pat2, m)
          case TypedAst.Pattern.Tuple(elms, _, _) => elms.foldLeft(m) {
            case (macc, elm) => visit(elm, macc)
          }
        }

      visit(this, Map.empty)
    }
  }

  object Pattern {

    /**
      * A typed AST node representing a wildcard pattern.
      *
      * @param tpe the type of the wildcard variable.
      * @param loc the source location.
      */
    case class Wildcard(tpe: Type, loc: SourceLocation) extends TypedAst.Pattern

    /**
      * A typed AST node representing a variable pattern.
      *
      * @param ident the name of the variable.
      * @param tpe   the type of the variable.
      * @param loc   the source location.
      */
    case class Var(ident: Name.Ident, tpe: Type, loc: SourceLocation) extends TypedAst.Pattern

    /**
      * A typed AST node representing a literal pattern.
      *
      * @param lit the literal.
      * @param tpe the type of the literal.
      * @param loc the source location.
      */
    case class Lit(lit: TypedAst.Literal, tpe: Type, loc: SourceLocation) extends TypedAst.Pattern

    /**
      * A typed AST node representing a tagged pattern.
      *
      * @param name  the namespace of the tag.
      * @param ident the tag name.
      * @param pat   the nested pattern.
      * @param tpe   the type of the tag.
      * @param loc   the source location.
      */
    case class Tag(name: Symbol.Resolved, ident: Name.Ident, pat: TypedAst.Pattern, tpe: Type.Enum, loc: SourceLocation) extends TypedAst.Pattern

    /**
      * A typed AST node representing a tuple pattern.
      *
      * @param elms the elements of the tuple.
      * @param tpe  the type of the tuple.
      * @param loc  the source location.
      */
    case class Tuple(elms: List[TypedAst.Pattern], tpe: Type.Tuple, loc: SourceLocation) extends TypedAst.Pattern

    // TODO: Add for opt, list, map, ???

  }

  /**
    * A common super-type for typed predicates.
    */
  sealed trait Predicate extends TypedAst {
    /**
      * The type of the predicate.
      */
    def tpe: Type

    /**
      * The source location of the predicate.
      */
    def loc: SourceLocation
  }

  object Predicate {

    // TODO Add maps from String to Type for vars?

    /**
      * A common super-type for head predicates.
      */
    sealed trait Head extends TypedAst.Predicate

    object Head {

      case class True(loc: SourceLocation) extends TypedAst.Predicate.Head {
        def tpe: Type = Type.Predicate(Nil)
      }

      case class False(loc: SourceLocation) extends TypedAst.Predicate.Head {
        def tpe: Type = Type.Predicate(Nil)
      }

      /**
        * A typed relational predicate that occurs in the head of a fact/rule.
        *
        * @param sym   the symbol of the predicate.
        * @param terms the terms of the predicate.
        * @param tpe   the type of the predicate.
        * @param loc   the source location.
        */
      case class Table(sym: Symbol.TableSym, terms: List[TypedAst.Term.Head], tpe: Type.Predicate, loc: SourceLocation) extends TypedAst.Predicate.Head

    }

    /**
      * A common super-type for body predicates.
      */
    sealed trait Body extends TypedAst.Predicate

    object Body {

      /**
        * A typed table predicate that occurs in the body of a rule.
        *
        * @param sym   the symbol of the table.
        * @param terms the terms of the predicate.
        * @param tpe   the type of the predicate.
        * @param loc   the source location.
        */
      case class Table(sym: Symbol.TableSym, terms: List[TypedAst.Term.Body], tpe: Type.Predicate, loc: SourceLocation) extends TypedAst.Predicate.Body

      /**
        * A filter predicate that occurs in the body of a rule.
        *
        * @param name  the name of the function.
        * @param terms the terms of the predicate.
        * @param tpe   the type of the predicate.
        * @param loc   the source location.
        */
      case class ApplyFilter(name: Symbol.Resolved, terms: List[TypedAst.Term.Body], tpe: Type.Lambda, loc: SourceLocation) extends TypedAst.Predicate.Body

      /**
        * A hook filter predicate that occurs in the body of a rule.
        *
        * @param hook  the name hook.
        * @param terms the terms of the predicate.
        * @param tpe   the type of the predicate.
        * @param loc   the source location.
        */
      case class ApplyHookFilter(hook: Ast.Hook, terms: List[TypedAst.Term.Body], tpe: Type.Lambda, loc: SourceLocation) extends TypedAst.Predicate.Body

      /**
        * A typed not equal predicate that occurs in the body of a rule.
        *
        * @param ident1 the name of the first variable.
        * @param ident2 the name of the second variable.
        * @param tpe    the type of the predicate.
        * @param loc    the source location.
        */
      case class NotEqual(ident1: Name.Ident, ident2: Name.Ident, tpe: Type, loc: SourceLocation) extends TypedAst.Predicate.Body

      /**
        * An AST node that represents the special loop predicate.
        *
        * @param ident the loop variable.
        * @param term  the set term.
        * @param tpe   the type of the predicate.
        * @param loc   the source location.
        */
      case class Loop(ident: Name.Ident, term: TypedAst.Term.Head, tpe: Type, loc: SourceLocation) extends TypedAst.Predicate.Body

    }

  }

  object Term {

    /**
      * A common super-type for terms that are allowed appear in a head predicate.
      */
    sealed trait Head extends TypedAst {
      /**
        * The type of `this` term.
        */
      def tpe: Type

      /**
        * The source location of `this` term.
        */
      def loc: SourceLocation
    }

    object Head {

      /**
        * A typed AST node representing a variable term.
        *
        * @param ident the variable name.
        * @param tpe   the type of the term.
        * @param loc   the source location.
        */
      case class Var(ident: Name.Ident, tpe: Type, loc: SourceLocation) extends TypedAst.Term.Head

      /**
        * A typed AST node representing a literal term.
        *
        * @param literal the literal.
        * @param tpe     the type of the term.
        * @param loc     the source location.
        */
      case class Lit(literal: TypedAst.Literal, tpe: Type, loc: SourceLocation) extends TypedAst.Term.Head

      case class Tag(enumName: Symbol.Resolved, tagName: Name.Ident, t: TypedAst.Term.Head, tpe: Type.Enum, loc: SourceLocation) extends TypedAst.Term.Head

      case class Tuple(elms: List[TypedAst.Term.Head], tpe: Type.Tuple, loc: SourceLocation) extends TypedAst.Term.Head

      /**
        * A typed AST node representing a function call term.
        *
        * @param name the name of the called function.
        * @param args the arguments to the function.
        * @param tpe  the type of the term.
        * @param loc  the source location.
        */
      case class Apply(name: Symbol.Resolved, args: List[TypedAst.Term.Head], tpe: Type, loc: SourceLocation) extends TypedAst.Term.Head

      /**
        * A typed AST node representing a hook function call term.
        *
        * @param hook the hook.
        * @param args the arguments to the function.
        * @param tpe  the type of the term.
        * @param loc  the source location.
        */
      case class ApplyHook(hook: Ast.Hook, args: List[TypedAst.Term.Head], tpe: Type, loc: SourceLocation) extends TypedAst.Term.Head

    }

    /**
      * A common super-type for terms that are allowed to appear in a body predicate.
      */
    sealed trait Body extends TypedAst {
      /**
        * The type of `this` term.
        */
      def tpe: Type

      /**
        * The source location of `this` term.
        */
      def loc: SourceLocation
    }

    object Body {

      /**
        * A typed AST node representing a wildcard term.
        *
        * @param tpe the type of the term.
        * @param loc the source location.
        */
      case class Wildcard(tpe: Type, loc: SourceLocation) extends TypedAst.Term.Body

      /**
        * A typed AST node representing a variable term.
        *
        * @param ident the variable name.
        * @param tpe   the type of the term.
        * @param loc   the source location.
        */
      case class Var(ident: Name.Ident, tpe: Type, loc: SourceLocation) extends TypedAst.Term.Body

      /**
        * A typed AST node representing a literal term.
        *
        * @param lit the literal.
        * @param tpe the type of the term.
        * @param loc the source location.
        */
      case class Lit(lit: TypedAst.Literal, tpe: Type, loc: SourceLocation) extends TypedAst.Term.Body

    }

  }

  /**
    * A typed AST node representing an attribute in a relation.
    *
    * @param ident the name of the attribute.
    * @param tpe   the type of the attribute.
    */
  case class Attribute(ident: Name.Ident, tpe: Type) extends TypedAst

  /**
    * A typed AST node representing a formal argument of a function.
    *
    * @param ident the name of the argument.
    * @param tpe   the type of the argument.
    */
  case class FormalArg(ident: Name.Ident, tpe: Type) extends TypedAst

  case class Property(law: Law, exp: TypedAst.Expression, loc: SourceLocation) extends TypedAst

}

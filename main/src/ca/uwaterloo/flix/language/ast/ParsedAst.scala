package ca.uwaterloo.flix.language.ast

import scala.collection.immutable.Seq

// TODO: New syntax:
//
// 1. namespace replaced by a.b.c/Something
//
// 2. lambda-fn
// fn sum(..): = ...
//   let f = (x, y) => x + y in
//   List/foldLeft(f, 0, xs)
//
// 3. list pattern match ::
// set/map pattern matching #{}

// TODO: Allow syntax for: variableName.length() --> length(variableName). "postfix call"
// aSet.has(athing) --> has(aSet, athing).
// aSet.getOrElse(xyz) --> getOrElse(aSet, xyz).
//
// foo::bar::baz(qux) <--> quux.foo::bar::baz.
// aSet.Set::has(athing) --> Set::has(aSet, athing).
// TODO: Allow postfix calls without ()?
//    // TODO: Pattern matching can simplify this, e.g.:
//
//    match xs with {
//      case #Set{} => // empty set
//      case #Set{x} => // singleton with variable x
//      case #Set{42} => singleton with literao 42
//      case #Set{x, 42, y, rest...} => // set with two elements x and y, and 42, and rest...
//    }
// TODO: Introduce type classes.
// TODO: Introduce type bounds.
// TODO: Introduce extern.
// TODO: Why have literals?

//index SUAfter(
//Index({location, object}, BTREE) with FilterF(f),
//
//)
//
//CREATE INDEX ON films ((lower(title)));
//
//index SUBefore({location, object}, Tree, location == "foo")


// TODO: Why split into declaration and definition?


/**
  * A common-super type for parsed AST nodes.
  */
sealed trait ParsedAst

object ParsedAst {

  /**
    * The AST root node.
    *
    * @param declarations the declarations in the AST.
    * @param time         the time spent in each compiler phase.
    */
  case class Root(declarations: Seq[ParsedAst.Declaration], time: Time) extends ParsedAst

  /**
    * A common super-type for AST nodes that represent declarations.
    */
  sealed trait Declaration extends ParsedAst {
    /**
      * Returns the source location of `this` declaration.
      */
    def loc: SourceLocation
  }

  object Declaration {

    /**
      * An AST node that represents a namespace declaration.
      *
      * @param sp1  the position of the first character in the namespace.
      * @param name the name of the namespace.
      * @param body the nested declarations.
      * @param sp2  the position of the last character in the namespace.
      */
    case class Namespace(sp1: SourcePosition, name: Name.Unresolved, body: Seq[ParsedAst.Declaration], sp2: SourcePosition) extends ParsedAst.Declaration {
      def loc: SourceLocation = SourceLocation.mk(sp1, sp2)
    }

    // TODO: Probably merge fact and rule.

    /**
      * An AST node that represents a fact declaration.
      *
      * @param sp1  the position of the first character in the fact.
      * @param head the head predicate.
      * @param sp2  the position of the last character in the fact.
      */
    case class Fact(sp1: SourcePosition, head: ParsedAst.Predicate, sp2: SourcePosition) extends ParsedAst.Declaration {
      def loc: SourceLocation = SourceLocation.mk(sp1, sp2)
    }

    /**
      * An AST node that represent a rule declaration.
      *
      * @param sp1  the position of the first character in the rule.
      * @param head the head predicate.
      * @param body the body predicates.
      * @param sp2  the position of the last character in the rule.
      */
    case class Rule(sp1: SourcePosition, head: ParsedAst.Predicate, body: Seq[ParsedAst.Predicate], sp2: SourcePosition) extends ParsedAst.Declaration {
      /**
        * Returns the list of alias predicates occurring the body of the rule.
        */
      val aliases: Seq[ParsedAst.Predicate.Alias] = body collect {
        case p: ParsedAst.Predicate.Alias => p
      }

      def loc: SourceLocation = SourceLocation.mk(sp1, sp2)
    }

  }

  /**
    * A common super-type for AST nodes that represent definitions.
    */
  sealed trait Definition extends Declaration

  object Definition {

    /**
      * An AST node that represents a function definition.
      *
      * @param sp1         the position of the first character in the definition.
      * @param annotations the annotations associated with the function.
      * @param ident       the name of the function.
      * @param formals     the formals (i.e. parameters and their types).
      * @param tpe         the return type.
      * @param body        the body expression of the function.
      * @param sp2         the position of the last character in the definition.
      */
    case class Function(sp1: SourcePosition, annotations: Seq[ParsedAst.Annotation], ident: Name.Ident, formals: Seq[FormalArg], tpe: Type, body: ParsedAst.Expression, sp2: SourcePosition) extends ParsedAst.Definition {
      def loc: SourceLocation = SourceLocation.mk(sp1, sp2)
    }

    /**
      * An AST node that represents a law definition.
      *
      * @param sp1     the position of the first character in the definition.
      * @param ident   the name of the function.
      * @param tparams the type parameters.
      * @param params  the value parameters.
      * @param tpe     the return type.
      * @param body    the body expression of the function.
      * @param sp2     the position of the last character in the definition.
      */
    case class Law(sp1: SourcePosition, ident: Name.Ident, tparams: Seq[ParsedAst.ContextBound], params: Seq[FormalArg], tpe: Type, body: ParsedAst.Expression, sp2: SourcePosition) extends ParsedAst.Definition {
      def loc: SourceLocation = SourceLocation.mk(sp1, sp2)
    }

    /**
      * An AST node that represents a function signature.
      *
      * @param sp1     the position of the first character in the definition.
      * @param ident   the name of the function.
      * @param formals the formals (i.e. parameters and their types).
      * @param tpe     the return type.
      * @param sp2     the position of the last character in the definition.
      */
    case class Signature(sp1: SourcePosition, ident: Name.Ident, formals: Seq[FormalArg], tpe: Type, sp2: SourcePosition) extends ParsedAst.Definition {
      def loc: SourceLocation = SourceLocation.mk(sp1, sp2)
    }

    /**
      * An AST node that represents a enum definition.
      *
      * @param sp1   the position of the first character in the definition.
      * @param ident the name of the enum.
      * @param cases the variants of the enum.
      * @param sp2   the position of the last character in the definition.
      */
    case class Enum(sp1: SourcePosition, ident: Name.Ident, cases: Seq[ParsedAst.Case], sp2: SourcePosition) extends ParsedAst.Definition {
      def loc: SourceLocation = SourceLocation.mk(sp1, sp2)
    }

    /**
      * An AST node that represents bounded lattice definition.
      *
      * @param sp1  the position of the first character in the definition.
      * @param tpe  the type of the lattice elements.
      * @param elms the components of the lattice.
      * @param sp2  the position of the last character in the definition.
      */
    case class BoundedLattice(sp1: SourcePosition, tpe: Type, elms: Seq[ParsedAst.Expression], sp2: SourcePosition) extends ParsedAst.Definition {
      def loc: SourceLocation = SourceLocation.mk(sp1, sp2)
    }

    /**
      * An AST node that represent a relation definition.
      *
      * @param sp1        the position of the first character in the definition.
      * @param ident      the name of the relation.
      * @param attributes the name and type of the attributes.
      * @param sp2        the position of the last character in the definition.
      */
    case class Relation(sp1: SourcePosition, ident: Name.Ident, attributes: Seq[ParsedAst.Attribute], sp2: SourcePosition) extends ParsedAst.Definition {
      /**
        * Returns the source location of `this` definition.
        */
      def loc: SourceLocation = SourceLocation.mk(sp1, sp2)
    }

    /**
      * An AST node that represents a lattice definition.
      *
      * @param sp1        the position of the first character in the definition.
      * @param ident      the name of the relation.
      * @param attributes the name and type of the attributes.
      * @param sp2        the position of the last character in the definition.
      */
    case class Lattice(sp1: SourcePosition, ident: Name.Ident, attributes: Seq[ParsedAst.Attribute], sp2: SourcePosition) extends ParsedAst.Definition {
      /**
        * Returns the source location of `this` definition.
        */
      def loc: SourceLocation = SourceLocation.mk(sp1, sp2)
    }

    /**
      * An AST node that represents an index definition.
      *
      * @param sp1     the position of the first character in the definition.
      * @param ident   the name of the relation or lattice.
      * @param indexes the sequence of indexes.
      * @param sp2     the position of the last character in the definition.
      */
    case class Index(sp1: SourcePosition, ident: Name.Ident, indexes: Seq[Seq[Name.Ident]], sp2: SourcePosition) extends ParsedAst.Definition {
      def loc: SourceLocation = SourceLocation.mk(sp1, sp2)
    }

    /**
      * An AST node that represents a type class definition.
      *
      * @param sp1     the position of the first character in the definition.
      * @param ident   the name of the type class.
      * @param tparams the type parameters of the type class.
      * @param bounds  the context bounds (i.e. type parameter constraints).
      * @param sp2     the position of the last character in the definition.
      */
    case class Class(sp1: SourcePosition, ident: Name.Ident, tparams: Seq[Type], bounds: Seq[ContextBound], body: Seq[ParsedAst.Definition], sp2: SourcePosition) extends ParsedAst.Definition {
      def loc: SourceLocation = SourceLocation.mk(sp1, sp2)
    }

    /**
      * An AST node that represents a type class implementation.
      *
      * @param sp1     the position of the first character in the definition.
      * @param ident   the name of the type class.
      * @param tparams the type parameters of the type class.
      * @param bounds  the context bounds (i.e. type parameter constraints).
      * @param sp2     the position of the last character in the definition.
      */
    case class Impl(sp1: SourcePosition, ident: Name.Ident, tparams: Seq[Type], bounds: Seq[ContextBound], body: Seq[ParsedAst.Definition.Function], sp2: SourcePosition) extends ParsedAst.Definition {
      def loc: SourceLocation = SourceLocation.mk(sp1, sp2)
    }


  }

  /**
    * A common super-type for AST node that represents literals.
    */
  sealed trait Literal {
    /**
      * Returns the source location of `this` literal.
      */
    def loc: SourceLocation
  }

  // TODO: Eliminate literals.
  object Literal {

    /**
      * An AST node that represents the Unit literal.
      *
      * @param sp1 the position of the first character in the literal.
      * @param sp2 the position of the last character in the literal.
      */
    case class Unit(sp1: SourcePosition, sp2: SourcePosition) extends ParsedAst.Literal {
      def loc: SourceLocation = SourceLocation.mk(sp1, sp2)
    }

    /**
      * An AST node that represents a boolean literal.
      *
      * @param sp1 the position of the first character in the literal.
      * @param lit the boolean literal.
      * @param sp2 the position of the last character in the literal.
      */
    case class Bool(sp1: SourcePosition, lit: String, sp2: SourcePosition) extends ParsedAst.Literal {
      def loc: SourceLocation = SourceLocation.mk(sp1, sp2)
    }

    /**
      * An AST node that represents a char literal.
      *
      * @param sp1 the position of the first character in the literal.
      * @param lit the char literal.
      * @param sp2 the position of the last character in the literal.
      */
    case class Char(sp1: SourcePosition, lit: String, sp2: SourcePosition) extends ParsedAst.Literal {
      def loc: SourceLocation = SourceLocation.mk(sp1, sp2)
    }

    /**
      * An AST node that represents an int8 literal.
      *
      * @param sp1 the position of the first character in the literal.
      * @param lit the int8 literal.
      * @param sp2 the position of the last character in the literal.
      */
    case class Int8(sp1: SourcePosition, lit: String, sp2: SourcePosition) extends ParsedAst.Literal {
      def loc: SourceLocation = SourceLocation.mk(sp1, sp2)
    }

    /**
      * An AST node that represents an int16 literal.
      *
      * @param sp1 the position of the first character in the literal.
      * @param lit the int16 literal.
      * @param sp2 the position of the last character in the literal.
      */
    case class Int16(sp1: SourcePosition, lit: String, sp2: SourcePosition) extends ParsedAst.Literal {
      def loc: SourceLocation = SourceLocation.mk(sp1, sp2)
    }

    /**
      * An AST node that represents an int32 literal.
      *
      * @param sp1 the position of the first character in the literal.
      * @param lit the int32 literal.
      * @param sp2 the position of the last character in the literal.
      */
    case class Int32(sp1: SourcePosition, lit: String, sp2: SourcePosition) extends ParsedAst.Literal {
      def loc: SourceLocation = SourceLocation.mk(sp1, sp2)
    }

    /**
      * An AST node that represents an int64 literal.
      *
      * @param sp1 the position of the first character in the literal.
      * @param lit the int64 literal.
      * @param sp2 the position of the last character in the literal.
      */
    case class Int64(sp1: SourcePosition, lit: String, sp2: SourcePosition) extends ParsedAst.Literal {
      def loc: SourceLocation = SourceLocation.mk(sp1, sp2)
    }

    /**
      * An AST node that represents a string literal.
      *
      * @param sp1 the position of the first character in the literal.
      * @param lit the string literal.
      * @param sp2 the position of the last character in the literal.
      */
    case class Str(sp1: SourcePosition, lit: String, sp2: SourcePosition) extends ParsedAst.Literal {
      def loc: SourceLocation = SourceLocation.mk(sp1, sp2)
    }

    /**
      * An AST node that represents a tagged literal.
      *
      * @param sp1  the position of the first character in the literal.
      * @param enum the name of the enum.
      * @param tag  the name of the tag.
      * @param lit  the nested literal.
      * @param sp2  the position of the last character in the literal.
      */
    case class Tag(sp1: SourcePosition, enum: Name.Unresolved, tag: Name.Ident, lit: ParsedAst.Literal, sp2: SourcePosition) extends ParsedAst.Literal {
      def loc: SourceLocation = SourceLocation.mk(sp1, sp2)
    }

    /**
      * An AST node that represents a tuple literal.
      *
      * @param sp1  the position of the first character in the literal.
      * @param elms the elements of the tuple.
      * @param sp2  the position of the last character in the literal.
      */
    case class Tuple(sp1: SourcePosition, elms: Seq[ParsedAst.Literal], sp2: SourcePosition) extends ParsedAst.Literal {
      def loc: SourceLocation = SourceLocation.mk(sp1, sp2)
    }

    /**
      * An AST node that represents a set literal.
      *
      * @param sp1  the position of the first character in the literal.
      * @param elms the elements of the set.
      * @param sp2  the position of the last character in the literal.
      */
    case class Set(sp1: SourcePosition, elms: Seq[ParsedAst.Literal], sp2: SourcePosition) extends ParsedAst.Literal {
      def loc: SourceLocation = SourceLocation.mk(sp1, sp2)
    }

  }

  /**
    * AST nodes for expressions.
    */
  sealed trait Expression extends ParsedAst {
    /**
      * Returns the source location of `this` expression.
      */
    def loc: SourceLocation
  }

  object Expression {

    /**
      * An AST node that represents a literal.
      *
      * @param sp1 the position of the first character in the expression.
      * @param lit the literal.
      * @param sp2 the position of the last character in the expression.
      */
    case class Lit(sp1: SourcePosition, lit: ParsedAst.Literal, sp2: SourcePosition) extends ParsedAst.Expression {
      def loc: SourceLocation = SourceLocation.mk(sp1, sp2)
    }

    /**
      * An AST node that represents an unresolved variable.
      *
      * @param sp1  the position of the first character in the expression.
      * @param name the ambiguous name.
      * @param sp2  the position of the last character in the expression.
      */
    case class Var(sp1: SourcePosition, name: Name.Unresolved, sp2: SourcePosition) extends ParsedAst.Expression {
      def loc: SourceLocation = SourceLocation.mk(sp1, sp2)
    }

    /**
      * An AST node that represents a function application.
      *
      * @param sp1     the position of the first character in the expression.
      * @param lambda  the lambda expression.
      * @param actuals the arguments.
      * @param sp2     the position of the last character in the expression.
      */
    case class Apply(sp1: SourcePosition, lambda: ParsedAst.Expression, actuals: Seq[ParsedAst.Expression], sp2: SourcePosition) extends ParsedAst.Expression {
      def loc: SourceLocation = SourceLocation.mk(sp1, sp2)
    }

    /**
      * An AST node that represents a lambda expression.
      *
      * @param sp1     the position of the first character in the expression.
      * @param formals the formals (i.e. parameters and their types).
      * @param tpe     the return type.
      * @param body    the body expression of the lambda.
      * @param sp2     the position of the last character in the expression.
      */
    case class Lambda(sp1: SourcePosition, formals: Seq[FormalArg], tpe: Type, body: ParsedAst.Expression, sp2: SourcePosition) extends ParsedAst.Expression {
      def loc: SourceLocation = SourceLocation.mk(sp1, sp2)
    }

    /**
      * An AST node that represents unary expressions.
      *
      * @param sp1 the position of the first character in the expression.
      * @param op  the unary operator.
      * @param e   the expression.
      * @param sp2 the position of the last character in the expression.
      */
    case class Unary(sp1: SourcePosition, op: UnaryOperator, e: ParsedAst.Expression, sp2: SourcePosition) extends ParsedAst.Expression {
      def loc: SourceLocation = SourceLocation.mk(sp1, sp2)
    }

    /**
      * An AST node that represents binary expressions.
      *
      * @param e1  the left expression.
      * @param sp1 the position of the first character in the expression.
      * @param op  the binary operator.
      * @param e2  the right expression.
      * @param sp2 the position of the last character in the expression.
      */
    case class Binary(e1: ParsedAst.Expression, sp1: SourcePosition, op: BinaryOperator, e2: ParsedAst.Expression, sp2: SourcePosition) extends ParsedAst.Expression {
      def loc: SourceLocation = SourceLocation.mk(sp1, sp2)
    }

    /**
      * An AST node that represents extended binary operator expressions.
      *
      * @param e1  the left expression.
      * @param sp1 the position of the first character in the expression.
      * @param op  the extended binary operator.
      * @param e2  the right expression.
      * @param sp2 the position of the last character in the expression.
      */
    case class ExtendedBinary(e1: ParsedAst.Expression, sp1: SourcePosition, op: ExtendedBinaryOperator, e2: ParsedAst.Expression, sp2: SourcePosition) extends ParsedAst.Expression {
      def loc: SourceLocation = SourceLocation.mk(sp1, sp2)
    }

    /**
      * An AST node that represents an if-then-else expression.
      *
      * @param sp1 the position of the first character in the expression.
      * @param e1  the conditional expression.
      * @param e2  the consequence expression.
      * @param e3  the alternative expression.
      * @param sp2 the position of the last character in the expression.
      */
    case class IfThenElse(sp1: SourcePosition, e1: ParsedAst.Expression, e2: ParsedAst.Expression, e3: ParsedAst.Expression, sp2: SourcePosition) extends ParsedAst.Expression {
      def loc: SourceLocation = SourceLocation.mk(sp1, sp2)
    }

    /**
      * An AST node that represents a let-binding with pattern matching.
      *
      * @param sp1   the position of the first character in the expression.
      * @param pat   the match pattern.
      * @param value the expression whose value the identifier should be bound to.
      * @param body  the expression in which the bound variable is visible.
      * @param sp2   the position of the last character in the expression.
      */
    case class LetMatch(sp1: SourcePosition, pat: ParsedAst.Pattern, value: ParsedAst.Expression, body: ParsedAst.Expression, sp2: SourcePosition) extends ParsedAst.Expression {
      def loc: SourceLocation = SourceLocation.mk(sp1, sp2)
    }

    /**
      * An AST node that represents a match expression.
      *
      * @param sp1   the position of the first character in the expression.
      * @param e     the match expression.
      * @param rules the match rules and their bodies.
      * @param sp2   the position of the last character in the expression.
      */
    case class Match(sp1: SourcePosition, e: ParsedAst.Expression, rules: Seq[(ParsedAst.Pattern, ParsedAst.Expression)], sp2: SourcePosition) extends ParsedAst.Expression {
      def loc: SourceLocation = SourceLocation.mk(sp1, sp2)
    }

    /**
      * An AST node that represents a switch expression.
      *
      * @param sp1   the position of the first character in the expression.
      * @param rules the rules of the switch.
      * @param sp2   the position of the last character in the expression.
      */
    case class Switch(sp1: SourcePosition, rules: Seq[(ParsedAst.Expression, ParsedAst.Expression)], sp2: SourcePosition) extends ParsedAst.Expression {
      def loc: SourceLocation = SourceLocation.mk(sp1, sp2)
    }


    /**
      * An AST node that represents an infix function call.
      *
      * @param e1   the first argument expression.
      * @param sp1  the position of the first character in the expression.
      * @param name the ambiguous name of the function.
      * @param e2   the second argument expression.
      * @param sp2  the position of the last character in the expression.
      */
    case class Infix(e1: ParsedAst.Expression, sp1: SourcePosition, name: Name.Unresolved, e2: ParsedAst.Expression, sp2: SourcePosition) extends ParsedAst.Expression {
      def loc: SourceLocation = SourceLocation.mk(sp1, sp2)
    }

    /**
      * An AST node that represents a tagged expression.
      *
      * @param sp1      the position of the first character in the expression.
      * @param enumName the namespace of the enum.
      * @param tagName  the tag name.
      * @param e        the nested expression.
      * @param sp2      the position of the last character in the expression.
      */
    case class Tag(sp1: SourcePosition, enumName: Name.Unresolved, tagName: Name.Ident, e: ParsedAst.Expression, sp2: SourcePosition) extends ParsedAst.Expression {
      def loc: SourceLocation = SourceLocation.mk(sp1, sp2)
    }

    /**
      * An AST node that represents a tuple expression.
      *
      * @param sp1  the position of the first character in the expression.
      * @param elms the elements of the tuple.
      * @param sp2  the position of the last character in the expression.
      */
    case class Tuple(sp1: SourcePosition, elms: Seq[ParsedAst.Expression], sp2: SourcePosition) extends ParsedAst.Expression {
      def loc: SourceLocation = SourceLocation.mk(sp1, sp2)
    }

    /**
      * An AST node that represents a set expression.
      *
      * @param sp1  the position of the first character in the expression.
      * @param elms the elements of the set.
      * @param sp2  the position of the last character in the expression.
      */
    case class Set(sp1: SourcePosition, elms: Seq[ParsedAst.Expression], sp2: SourcePosition) extends ParsedAst.Expression {
      def loc: SourceLocation = SourceLocation.mk(sp1, sp2)
    }

    /**
      * An AST node that ascribes a type to an expression.
      *
      * @param sp1 the position of the first character in the expression.
      * @param e   the expression.
      * @param tpe the ascribed type.
      * @param sp2 the position of the last character in the expression.
      */
    case class Ascribe(sp1: SourcePosition, e: ParsedAst.Expression, tpe: Type, sp2: SourcePosition) extends ParsedAst.Expression {
      def loc: SourceLocation = SourceLocation.mk(sp1, sp2)
    }

    /**
      * An AST node that represents an error expression.
      *
      * @param sp1 the position of the first character in the expression.
      * @param tpe the type of the error expression.
      * @param sp2 the position of the last character in the expression.
      */
    case class Error(sp1: SourcePosition, tpe: Type, sp2: SourcePosition) extends ParsedAst.Expression {
      def loc: SourceLocation = SourceLocation.mk(sp1, sp2)
    }

    /**
      * An AST node that represents a bot expression.
      *
      * @param sp1 the position of the first character in the expression.
      * @param sp2 the position of the last character in the expression.
      */
    case class Bot(sp1: SourcePosition, sp2: SourcePosition) extends ParsedAst.Expression {
      def loc: SourceLocation = SourceLocation.mk(sp1, sp2)
    }

    /**
      * An AST node that represents a top expression.
      *
      * @param sp1 the position of the first character in the expression.
      * @param sp2 the position of the last character in the expression.
      */
    case class Top(sp1: SourcePosition, sp2: SourcePosition) extends ParsedAst.Expression {
      def loc: SourceLocation = SourceLocation.mk(sp1, sp2)
    }

    /**
      * An AST node that represents an existentially quantified expression.
      *
      * @param sp1    the position of the first character in the expression.
      * @param params the existentially quantified variables.
      * @param body   the existentially quantified expression.
      * @param sp2    the position of the last character in the expression.
      */
    case class Existential(sp1: SourcePosition, params: Seq[FormalArg], body: ParsedAst.Expression, sp2: SourcePosition) extends ParsedAst.Expression {
      def loc: SourceLocation = SourceLocation.mk(sp1, sp2)
    }

    /**
      * An AST node that represents a universally quantified expression.
      *
      * @param sp1    the position of the first character in the expression.
      * @param params the universally quantified variables.
      * @param body   the universally quantified expression.
      * @param sp2    the position of the last character in the expression.
      */
    case class Universal(sp1: SourcePosition, params: Seq[FormalArg], body: ParsedAst.Expression, sp2: SourcePosition) extends ParsedAst.Expression {
      def loc: SourceLocation = SourceLocation.mk(sp1, sp2)
    }

  }

  /**
    * AST nodes for Patterns.
    *
    * A pattern is like a literal except it may contain variables and wildcards.
    */
  sealed trait Pattern extends ParsedAst {
    /**
      * The source location of `this` pattern.
      */
    def loc: SourceLocation
  }

  object Pattern {

    /**
      * An AST node that represents a wildcard pattern.
      *
      * @param sp1 the position of the first character in the pattern.
      * @param sp2 the position of the last character in the pattern.
      */
    case class Wildcard(sp1: SourcePosition, sp2: SourcePosition) extends ParsedAst.Pattern {
      def loc: SourceLocation = SourceLocation.mk(sp1, sp2)
    }

    /**
      * An AST node that represents a variable pattern.
      *
      * @param sp1   the position of the first character in the pattern.
      * @param ident the variable identifier.
      * @param sp2   the position of the last character in the pattern.
      */
    case class Var(sp1: SourcePosition, ident: Name.Ident, sp2: SourcePosition) extends ParsedAst.Pattern {
      def loc: SourceLocation = SourceLocation.mk(sp1, sp2)
    }

    /**
      * An AST node that represents a literal pattern.
      *
      * @param sp1 the position of the first character in the pattern.
      * @param lit the literal.
      * @param sp2 the position of the last character in the pattern.
      */
    case class Lit(sp1: SourcePosition, lit: ParsedAst.Literal, sp2: SourcePosition) extends ParsedAst.Pattern {
      def loc: SourceLocation = SourceLocation.mk(sp1, sp2)
    }

    /**
      * An AST node that represents a tagged pattern.
      *
      * @param sp1      the position of the first character in the pattern.
      * @param enumName the enum name.
      * @param tagName  the tag name.
      * @param p        the nested pattern.
      * @param sp2      the position of the last character in the pattern.
      */
    case class Tag(sp1: SourcePosition, enumName: Name.Unresolved, tagName: Name.Ident, p: ParsedAst.Pattern, sp2: SourcePosition) extends ParsedAst.Pattern {
      def loc: SourceLocation = SourceLocation.mk(sp1, sp2)
    }

    /**
      * An AST node that represents a tuple pattern.
      *
      * @param sp1  the position of the first character in the pattern.
      * @param elms the elements of the tuple.
      * @param sp2  the position of the last character in the pattern.
      */
    case class Tuple(sp1: SourcePosition, elms: Seq[ParsedAst.Pattern], sp2: SourcePosition) extends ParsedAst.Pattern {
      def loc: SourceLocation = SourceLocation.mk(sp1, sp2)
    }

  }

  /**
    * A common super-type for predicates.
    */
  sealed trait Predicate extends ParsedAst {
    /**
      * Returns the source location of `this` predicate.
      */
    def loc: SourceLocation
  }

  object Predicate {

    /**
      * An AST node that represent an ambiguous predicate.
      *
      * @param sp1   the position of the first character in the predicate.
      * @param name  the unresolved name of the predicate.
      * @param terms the terms of the predicate.
      * @param sp2   the position of the last character in the predicate.
      */
    case class Ambiguous(sp1: SourcePosition, name: Name.Unresolved, terms: Seq[ParsedAst.Term], sp2: SourcePosition) extends ParsedAst.Predicate {
      def loc: SourceLocation = SourceLocation.mk(sp1, sp2)
    }

    /**
      * An AST node that represents the special not equal predicate.
      */
    case class NotEqual(sp1: SourcePosition, ident1: Name.Ident, ident2: Name.Ident, sp2: SourcePosition) extends ParsedAst.Predicate {
      def loc: SourceLocation = SourceLocation.mk(sp1, sp2)
    }

    /**
      * An AST node that represents the special alias predicate.
      *
      * @param sp1   the position of the first character in the predicate.
      * @param ident the name of the variable.
      * @param term  the term.
      * @param sp2   the position of the last character in the predicate.
      */
    case class Alias(sp1: SourcePosition, ident: Name.Ident, term: ParsedAst.Term, sp2: SourcePosition) extends ParsedAst.Predicate {
      def loc: SourceLocation = SourceLocation.mk(sp1, sp2)
    }

    /**
      * An AST node that represents the special loop predicate.
      *
      * @param sp1   the position of the first character in the predicate.
      * @param ident the loop variable.
      * @param term  the set term.
      * @param sp2   the position of the last character in the predicate.
      */
    case class Loop(sp1: SourcePosition, ident: Name.Ident, term: ParsedAst.Term, sp2: SourcePosition) extends ParsedAst.Predicate {
      def loc: SourceLocation = SourceLocation.mk(sp1, sp2)
    }

  }

  /**
    * AST nodes for Terms.
    */
  sealed trait Term extends ParsedAst {
    /**
      * Returns the source location of `this` term.
      */
    def loc: SourceLocation
  }

  object Term {

    /**
      * An AST node that represent a wildcard variable term.
      *
      * @param sp1 the position of the first character in the term.
      * @param sp2 the position of the last character in the term.
      */
    case class Wildcard(sp1: SourcePosition, sp2: SourcePosition) extends ParsedAst.Term {
      def loc: SourceLocation = SourceLocation.mk(sp1, sp2)
    }

    /**
      * An AST node that represent a variable term.
      *
      * @param sp1   the position of the first character in the term.
      * @param ident the variable identifier.
      * @param sp2   the position of the last character in the term.
      */
    case class Var(sp1: SourcePosition, ident: Name.Ident, sp2: SourcePosition) extends ParsedAst.Term {
      def loc: SourceLocation = SourceLocation.mk(sp1, sp2)
    }

    /**
      * An AST node that represent a literal term.
      *
      * @param sp1 the position of the first character in the term.
      * @param lit the literal.
      * @param sp2 the position of the last character in the term.
      */
    case class Lit(sp1: SourcePosition, lit: ParsedAst.Literal, sp2: SourcePosition) extends ParsedAst.Term {
      def loc: SourceLocation = SourceLocation.mk(sp1, sp2)
    }

    /**
      * An AST node that represent a function application term
      *
      * @param sp1  the position of the first character in the term.
      * @param name the unresolved name of the function.
      * @param args the arguments to the function.
      * @param sp2  the position of the last character in the term.
      */
    case class Apply(sp1: SourcePosition, name: Name.Unresolved, args: Seq[ParsedAst.Term], sp2: SourcePosition) extends ParsedAst.Term {
      def loc: SourceLocation = SourceLocation.mk(sp1, sp2)
    }

    /**
      * An AST node that represents an infix function application term.
      *
      * @param sp1  the position of the first character in the term.
      * @param t1   the left argument.
      * @param name the unresolved name of the function.
      * @param t2   the right argument.
      * @param sp2  the position of the last character in the term.
      */
    case class Infix(sp1: SourcePosition, t1: ParsedAst.Term, name: Name.Unresolved, t2: ParsedAst.Term, sp2: SourcePosition) extends ParsedAst.Term {
      def loc: SourceLocation = SourceLocation.mk(sp1, sp2)
    }

    /**
      * An AST node that represents an ascribed term.
      *
      * @param sp1  the position of the first character in the term.
      * @param term the term.
      * @param tpe  the type.
      * @param sp2  the position of the last character in the term.
      */
    case class Ascribe(sp1: SourcePosition, term: ParsedAst.Term, tpe: Type, sp2: SourcePosition) extends ParsedAst.Term {
      def loc: SourceLocation = SourceLocation.mk(sp1, sp2)
    }

  }

  /**
    * An AST node that represents an attribute.
    *
    * @param ident  the name of the attribute.
    * @param interp the interpretation of the attribute.
    */
  case class Attribute(ident: Name.Ident, interp: Interpretation) extends ParsedAst

  /**
    * A common super-type for attribute interpretations.
    */
  sealed trait Interpretation {
    /**
      * The type of elements in `this` interpretation.
      */
    def tpe: Type
  }

  object Interpretation {

    /**
      * An AST node representing the standard set-based interpretation of an attribute in a relation.
      *
      * @param tpe the type of the attribute.
      */
    case class Set(tpe: Type) extends ParsedAst.Interpretation

    /**
      * An AST node representing a lattice-based interpretation of an attribute in a relation.
      *
      * @param tpe the type of the attribute.
      */
    case class Lattice(tpe: Type) extends ParsedAst.Interpretation

  }

  /**
    * An AST node representing an annotation.
    *
    * @param sp1  the position of the first character in the annotation.
    * @param name the name of the annotation.
    * @param sp2  the position of the last character in the annotation.
    */
  case class Annotation(sp1: SourcePosition, name: String, sp2: SourcePosition) extends ParsedAst {
    def loc: SourceLocation = SourceLocation.mk(sp1, sp2)
  }

  /**
    * An AST node representing a case declaration.
    *
    * @param sp1   the position of the first character in the case declaration.
    * @param ident the name of the declared tag.
    * @param tpe   the type of the declared tag
    * @param sp2   the position of the last character in the case declaration.
    */
  case class Case(sp1: SourcePosition, ident: Name.Ident, tpe: Type, sp2: SourcePosition) extends ParsedAst {
    def loc: SourceLocation = SourceLocation.mk(sp1, sp2)
  }

  /**
    * An AST node representing a formal argument of a function.
    *
    * @param ident       the name of the argument.
    * @param annotations a sequence of annotations associated with the formal argument.
    * @param tpe         the type of the argument.
    */
  case class FormalArg(ident: Name.Ident, annotations: Seq[ParsedAst.Annotation], tpe: Type) extends ParsedAst

  /**
    * A context bound is a type class constraint on one more type parameters.
    *
    * @param sp1     the position of the first character in the context bound.
    * @param ident   the name of the type class.
    * @param tparams the type params of the class.
    * @param sp2     the position of the last character in the context bound.
    */
  case class ContextBound(sp1: SourcePosition, ident: Name.Ident, tparams: Seq[Type], sp2: SourcePosition) extends ParsedAst

}

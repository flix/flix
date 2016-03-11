package ca.uwaterloo.flix.language.ast

import scala.collection.immutable.Seq

// TODO: Tasks

// 1. Introduce imports:
// import foo.bar.baz/myFunction
// import foo.bar._ (imports everything in bar)
// import foo.bar (now you can write bar/myFunc)
// Imports are only allowed at the top of a compilation unit, before any namespaces. Does this even make sense?
// Decide: How should the components of a namespace be named: list or List?

// 2. Fat arrows
//   let f = (x, y) => x + y in
//   list/foldLeft(f, 0, xs)
//
//   list/foldLeft((x, y) => x + y, 0, 1 :: 2 :: Nil)
//

// 3. Post fix calls:
// length(xs) or xs.length() or even xs.length
// toList(xs) or xs.toList() or even xs.toList
// or even better:
// map(x => x + 1, xs) or (x => x + 1) map xs (?) or (x => x + 1) `map` xs is already possible?
// NB: Does not work with namespaces, e.g. foo/length(xs) cannot be written as xs.foo/length
// more examples:
//
// null(xs) or xs.null (maybe bad?)
// head(xs) or xs.head
// tail(xs) or xs.tail
// at(24, xs) or 24.at(xs) (woot?) (alternatively 42 `at` xs`)
// indexOf(42, xs) or 42 `indexOf` xs.

// 4. Cleanup declarations and definitions.

// 5. Cleanup relations/lattices.

// 6. Indexes:
// index SUAfter(
//  Index({location, object}, BTREE) with FilterF(f),
//
//  CREATE INDEX ON films ((lower(title)));
//
//  index SUBefore({location, object}, Tree, location == "foo")

// 7. Remove .loc from ast nodes.

// 8. Improve UTF8 support.

// 9. Improve parsing of characters (and escapes!)

// 10. Re-order all items in here.

/**
  * A common-super type for parsed AST nodes.
  */
sealed trait ParsedAst

object ParsedAst {

  /**
    * Program. A collection of abstract syntax trees.
    *
    * @param roots the roots of the abstract syntax trees in the program.
    * @param time  the time spent in each compiler phase.
    */
  case class Program(roots: List[ParsedAst.Root], time: Time) extends ParsedAst

  /**
    * Root. A collection of declarations.
    *
    * @param declarations the declarations in the compilation unit.
    */
  case class Root(declarations: Seq[ParsedAst.Declaration]) extends ParsedAst

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
      * @param name the the namespace.
      * @param body the nested declarations.
      * @param sp2  the position of the last character in the namespace.
      */
    case class Namespace(sp1: SourcePosition, name: Name.NName, body: Seq[ParsedAst.Declaration], sp2: SourcePosition) extends ParsedAst.Declaration {
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
      * An AST node that represents a signature of a function.
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
      * An AST node that represents an external function.
      *
      * @param sp1     the position of the first character in the definition.
      * @param ident   the name of the function.
      * @param formals the formals (i.e. parameters and their types).
      * @param tpe     the return type.
      * @param sp2     the position of the last character in the definition.
      */
    case class External(sp1: SourcePosition, ident: Name.Ident, formals: Seq[FormalArg], tpe: Type, sp2: SourcePosition) extends ParsedAst.Definition {
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
    * A common super-type for AST nodes that represent imports.
    */
  sealed trait Import extends ParsedAst

  object Import {

    /**
      * An AST node that imports every definition from a namespace (import foo.bar.baz._).
      *
      * @param sp1 the position of the first character in the import.
      * @param ns  the namespace.
      * @param sp2 the position of the last character in the import.
      */
    case class Wildcard(sp1: SourcePosition, ns: Name.NName, sp2: SourcePosition) extends ParsedAst.Import

    /**
      * An AST node that imports a definition from a namespace (import foo.bar.baz/qux).
      *
      * @param sp1  the position of the first character in the import.
      * @param name the qualified name.
      * @param sp2  the position of the last character in the import.
      */
    case class Definition(sp1: SourcePosition, name: Name.QName, sp2: SourcePosition) extends ParsedAst.Import

    /**
      * An AST node that imports a namespace (import foo.bar.baz).
      *
      * @param sp1 the position of the first character in the import.
      * @param ns  the namespace.
      * @param sp2 the position of the last character in the import.
      */
    case class Namespace(sp1: SourcePosition, ns: Name.NName, sp2: SourcePosition) extends ParsedAst.Import

  }

  /**
    * A common super-type for AST nodes that represents literals.
    */
  sealed trait Literal {
    /**
      * Returns the source location of `this` literal.
      */
    def loc: SourceLocation
  }

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
      * An AST node that represents a float32 literal.
      *
      * @param sp1    the position of the first character in the literal.
      * @param sign   the sign (true if signed).
      * @param before the digits before the decimal point.
      * @param after  the digits after the decimal point.
      * @param sp2    the position of the last character in the literal.
      */
    case class Float32(sp1: SourcePosition, sign: Boolean, before: String, after: String, sp2: SourcePosition) extends ParsedAst.Literal {
      def loc: SourceLocation = SourceLocation.mk(sp1, sp2)
    }

    /**
      * An AST node that represents a float64 literal.
      *
      * @param sp1    the position of the first character in the literal.
      * @param sign   the sign (true if signed).
      * @param before the digits before the decimal point.
      * @param after  the digits after the decimal point.
      * @param sp2    the position of the last character in the literal.
      */
    case class Float64(sp1: SourcePosition, sign: Boolean, before: String, after: String, sp2: SourcePosition) extends ParsedAst.Literal {
      def loc: SourceLocation = SourceLocation.mk(sp1, sp2)
    }

    /**
      * An AST node that represents an int8 literal.
      *
      * @param sp1  the position of the first character in the literal.
      * @param sign the sign (true if signed).
      * @param lit  the int8 literal.
      * @param sp2  the position of the last character in the literal.
      */
    case class Int8(sp1: SourcePosition, sign: Boolean, lit: String, sp2: SourcePosition) extends ParsedAst.Literal {
      def loc: SourceLocation = SourceLocation.mk(sp1, sp2)
    }

    /**
      * An AST node that represents an int16 literal.
      *
      * @param sp1  the position of the first character in the literal.
      * @param sign the sign (true if signed).
      * @param lit  the int16 literal.
      * @param sp2  the position of the last character in the literal.
      */
    case class Int16(sp1: SourcePosition, sign: Boolean, lit: String, sp2: SourcePosition) extends ParsedAst.Literal {
      def loc: SourceLocation = SourceLocation.mk(sp1, sp2)
    }

    /**
      * An AST node that represents an int32 literal.
      *
      * @param sp1  the position of the first character in the literal.
      * @param sign the sign (true if signed).
      * @param lit  the int32 literal.
      * @param sp2  the position of the last character in the literal.
      */
    case class Int32(sp1: SourcePosition, sign: Boolean, lit: String, sp2: SourcePosition) extends ParsedAst.Literal {
      def loc: SourceLocation = SourceLocation.mk(sp1, sp2)
    }

    /**
      * An AST node that represents an int64 literal.
      *
      * @param sp1  the position of the first character in the literal.
      * @param sign the sign (true if signed).
      * @param lit  the int64 literal.
      * @param sp2  the position of the last character in the literal.
      */
    case class Int64(sp1: SourcePosition, sign: Boolean, lit: String, sp2: SourcePosition) extends ParsedAst.Literal {
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

  }

  /**
    * AST nodes for expressions.
    */
  sealed trait Expression extends ParsedAst {

    /**
      * Returns the left most source position in the sub-tree of `this` expression.
      */
    def leftMostSourcePosition: SourcePosition = this match {
      case Expression.Lit(sp1, _, _) => sp1
      case Expression.Var(sp1, _, _) => sp1
      case Expression.Apply(sp1, _, _, _) => sp1
      case Expression.Lambda(sp1, _, _, _, _) => sp1
      case Expression.Unary(sp1, _, _, _) => sp1
      case Expression.Binary(e1, _, _, _) => e1.leftMostSourcePosition
      case Expression.ExtendedBinary(e1, _, _, _) => e1.leftMostSourcePosition
      case Expression.IfThenElse(sp1, _, _, _, _) => sp1
      case Expression.LetMatch(sp1, _, _, _, _) => sp1
      case Expression.Match(sp1, _, _, _) => sp1
      case Expression.Switch(sp1, _, _) => sp1
      case Expression.Infix(e1, _, _, _) => e1.leftMostSourcePosition
      case Expression.Tag(sp1, _, _, _, _) => sp1
      case Expression.Tuple(sp1, _, _) => sp1
      case Expression.FNil(sp1, _) => sp1
      case Expression.FList(hd, _, _) => hd.leftMostSourcePosition
      case Expression.FNone(sp1, _) => sp1
      case Expression.FSome(sp1, _, _) => sp1
      case Expression.FSet(sp1, _, _) => sp1
      case Expression.FMap(sp1, _, _) => sp1
      case Expression.Ascribe(sp1, _, _, _) => sp1
      case Expression.UserError(sp1, _, _) => sp1
      case Expression.Bot(sp1, sp2) => sp1
      case Expression.Top(sp1, sp2) => sp1
      case Expression.Existential(sp1, _, _, _) => sp1
      case Expression.Universal(sp1, _, _, _) => sp1
    }

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
    case class Var(sp1: SourcePosition, name: Name.QName, sp2: SourcePosition) extends ParsedAst.Expression {
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
      * @param op  the binary operator.
      * @param e2  the right expression.
      * @param sp2 the position of the last character in the expression.
      */
    case class Binary(e1: ParsedAst.Expression, op: BinaryOperator, e2: ParsedAst.Expression, sp2: SourcePosition) extends ParsedAst.Expression {
      def loc: SourceLocation = SourceLocation.mk(e1.leftMostSourcePosition, sp2)
    }

    /**
      * An AST node that represents extended binary operator expressions.
      *
      * @param e1  the left expression.
      * @param op  the extended binary operator.
      * @param e2  the right expression.
      * @param sp2 the position of the last character in the expression.
      */
    case class ExtendedBinary(e1: ParsedAst.Expression, op: ExtendedBinaryOperator, e2: ParsedAst.Expression, sp2: SourcePosition) extends ParsedAst.Expression {
      def loc: SourceLocation = SourceLocation.mk(e1.leftMostSourcePosition, sp2)
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
      * @param name the ambiguous name of the function.
      * @param e2   the second argument expression.
      * @param sp2  the position of the last character in the expression.
      */
    case class Infix(e1: ParsedAst.Expression, name: Name.QName, e2: ParsedAst.Expression, sp2: SourcePosition) extends ParsedAst.Expression {
      def loc: SourceLocation = SourceLocation.mk(e1.leftMostSourcePosition, sp2)
    }

    /**
      * An AST node that represents a tagged expression.
      *
      * @param sp1  the position of the first character in the expression.
      * @param enum the namespace of the enum.
      * @param tag  the tag name.
      * @param e    the nested expression.
      * @param sp2  the position of the last character in the expression.
      */
    case class Tag(sp1: SourcePosition, enum: Name.QName, tag: Name.Ident, e: ParsedAst.Expression, sp2: SourcePosition) extends ParsedAst.Expression {
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
      * An AST node that represents the Nil expression.
      *
      * @param sp1 the position of the first character in the expression.
      * @param sp2 the position of the last character in the expression.
      */
    case class FNil(sp1: SourcePosition, sp2: SourcePosition) extends ParsedAst.Expression {
      def loc: SourceLocation = SourceLocation.mk(sp1, sp2)
    }

    /**
      * An AST node that represents a list expression.
      *
      * @param hd  the head of the list.
      * @param tl  the tail of the list.
      * @param sp2 the position of the last character in the expression.
      */
    case class FList(hd: ParsedAst.Expression, tl: ParsedAst.Expression, sp2: SourcePosition) extends ParsedAst.Expression {
      def loc: SourceLocation = SourceLocation.mk(hd.leftMostSourcePosition, sp2)
    }

    /**
      * An AST node that represents the None expression.
      *
      * @param sp1 the position of the first character in the expression.
      * @param sp2 the position of the last character in the expression.
      */
    case class FNone(sp1: SourcePosition, sp2: SourcePosition) extends ParsedAst.Expression {
      def loc: SourceLocation = SourceLocation.mk(sp1, sp2)
    }

    /**
      * An AST node that represents the Some expression.
      *
      * @param sp1 the position of the first character in the expression.
      * @param elm the nested expression.
      * @param sp2 the position of the last character in the expression.
      */
    case class FSome(sp1: SourcePosition, elm: ParsedAst.Expression, sp2: SourcePosition) extends ParsedAst.Expression {
      def loc: SourceLocation = SourceLocation.mk(sp1, sp2)
    }

    /**
      * An AST node that represents a set expression.
      *
      * @param sp1  the position of the first character in the expression.
      * @param elms the elements of the set.
      * @param sp2  the position of the last character in the expression.
      */
    case class FSet(sp1: SourcePosition, elms: Seq[ParsedAst.Expression], sp2: SourcePosition) extends ParsedAst.Expression {
      def loc: SourceLocation = SourceLocation.mk(sp1, sp2)
    }

    /**
      * An AST node that represents a map expression.
      *
      * @param sp1  the position of the first character in the expression.
      * @param elms the (key, values) of the map.
      * @param sp2  the position of the last character in the expression.
      */
    case class FMap(sp1: SourcePosition, elms: Seq[(ParsedAst.Expression, ParsedAst.Expression)], sp2: SourcePosition) extends ParsedAst.Expression {
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
    case class UserError(sp1: SourcePosition, tpe: Type, sp2: SourcePosition) extends ParsedAst.Expression {
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
      * Returns the left most source position in sub-tree of `this` pattern.
      */
    def leftMostSourcePosition: SourcePosition = this match {
      case Pattern.Wildcard(sp1, _) => sp1
      case Pattern.Var(sp1, _, _) => sp1
      case Pattern.Lit(sp1, _, _) => sp1
      case Pattern.Tag(sp1, _, _, _, _) => sp1
      case Pattern.Tuple(sp1, _, _) => sp1
      case Pattern.FNone(sp1, _) => sp1
      case Pattern.FSome(sp1, _, _) => sp1
      case Pattern.FNil(sp1, sp2) => sp1
      case Pattern.FList(hd, _, _) => hd.leftMostSourcePosition
      case Pattern.FSet(sp1, _, _, _) => sp1
      case Pattern.FMap(sp1, _, _, _) => sp1
    }

  }

  object Pattern {

    /**
      * An AST node that represents a wildcard pattern.
      *
      * @param sp1 the position of the first character in the pattern.
      * @param sp2 the position of the last character in the pattern.
      */
    case class Wildcard(sp1: SourcePosition, sp2: SourcePosition) extends ParsedAst.Pattern

    /**
      * An AST node that represents a variable pattern.
      *
      * @param sp1   the position of the first character in the pattern.
      * @param ident the variable identifier.
      * @param sp2   the position of the last character in the pattern.
      */
    case class Var(sp1: SourcePosition, ident: Name.Ident, sp2: SourcePosition) extends ParsedAst.Pattern

    /**
      * An AST node that represents a literal pattern.
      *
      * @param sp1 the position of the first character in the pattern.
      * @param lit the literal.
      * @param sp2 the position of the last character in the pattern.
      */
    case class Lit(sp1: SourcePosition, lit: ParsedAst.Literal, sp2: SourcePosition) extends ParsedAst.Pattern

    /**
      * An AST node that represents a tagged pattern.
      *
      * @param sp1  the position of the first character in the pattern.
      * @param enum the enum name.
      * @param tag  the tag name.
      * @param p    the nested pattern.
      * @param sp2  the position of the last character in the pattern.
      */
    case class Tag(sp1: SourcePosition, enum: Name.QName, tag: Name.Ident, p: ParsedAst.Pattern, sp2: SourcePosition) extends ParsedAst.Pattern

    /**
      * An AST node that represents a tuple pattern.
      *
      * @param sp1  the position of the first character in the pattern.
      * @param pats the nested patterns of the tuple.
      * @param sp2  the position of the last character in the pattern.
      */
    case class Tuple(sp1: SourcePosition, pats: Seq[ParsedAst.Pattern], sp2: SourcePosition) extends ParsedAst.Pattern

    /**
      * An AST node that represents the Nil pattern.
      *
      * @param sp1 the position of the first character in the pattern.
      * @param sp2 the position of the last character in the pattern.
      */
    case class FNil(sp1: SourcePosition, sp2: SourcePosition) extends ParsedAst.Pattern

    /**
      * An AST node that represents a list pattern.
      *
      * @param hd  the head pattern.
      * @param tl  the tail pattern.
      * @param sp2 the position of the last character in the pattern.
      */
    case class FList(hd: ParsedAst.Pattern, tl: ParsedAst.Pattern, sp2: SourcePosition) extends ParsedAst.Pattern

    /**
      * An AST node that represents the None pattern.
      *
      * @param sp1 the position of the first character in the pattern.
      * @param sp2 the position of the last character in the pattern.
      */
    case class FNone(sp1: SourcePosition, sp2: SourcePosition) extends ParsedAst.Pattern

    /**
      * An AST node that represents the Some pattern.
      *
      * @param sp1 the position of the first character in the pattern.
      * @param pat the nested pattern.
      * @param sp2 the position of the last character in the pattern.
      */
    case class FSome(sp1: SourcePosition, pat: ParsedAst.Pattern, sp2: SourcePosition) extends ParsedAst.Pattern

    /**
      * An AST node that represents a set pattern.
      *
      * @param sp1  the position of the first character in the pattern.
      * @param elms the element patterns.
      * @param rest the optional rest pattern.
      * @param sp2  the position of the last character in the pattern.
      */
    case class FSet(sp1: SourcePosition, elms: Seq[ParsedAst.Pattern], rest: Option[ParsedAst.Pattern], sp2: SourcePosition) extends ParsedAst.Pattern

    /**
      * An AST node that represents a map pattern.
      *
      * @param sp1  the position of the first character in the pattern.
      * @param elms the element patterns.
      * @param rest the optional rest pattern.
      * @param sp2  the position of the last character in the pattern.
      */
    case class FMap(sp1: SourcePosition, elms: Seq[(ParsedAst.Pattern, ParsedAst.Pattern)], rest: Option[ParsedAst.Pattern], sp2: SourcePosition) extends ParsedAst.Pattern

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
    case class Ambiguous(sp1: SourcePosition, name: Name.QName, terms: Seq[ParsedAst.Term], sp2: SourcePosition) extends ParsedAst.Predicate {
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
      * An AST node that represents a tag term.
      *
      * @param sp1      the position of the first character in the term.
      * @param enumName the namespace of the enum.
      * @param tagName  the tag name.
      * @param t        the (optional) nested term.
      * @param sp2      the position of the last character in the term.
      */
    case class Tag(sp1: SourcePosition, enumName: Name.QName, tagName: Name.Ident, t: Option[ParsedAst.Term], sp2: SourcePosition) extends ParsedAst.Term {
      def loc: SourceLocation = SourceLocation.mk(sp1, sp2)
    }

    /**
      * An AST node that represents a tuple term.
      *
      * @param sp1  the position of the first character in the term.
      * @param elms the elements of the tuple.
      * @param sp2  the position of the last character in the term.
      */
    case class Tuple(sp1: SourcePosition, elms: Seq[ParsedAst.Term], sp2: SourcePosition) extends ParsedAst.Term {
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
    case class Apply(sp1: SourcePosition, name: Name.QName, args: Seq[ParsedAst.Term], sp2: SourcePosition) extends ParsedAst.Term {
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

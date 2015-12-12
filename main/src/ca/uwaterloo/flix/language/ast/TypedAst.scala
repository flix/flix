package ca.uwaterloo.flix.language.ast

import java.lang.reflect.{Method, Field}

import scala.collection.mutable

// TODO: The documentation is not fully consistent with when something is an AST node. "that represents" vs "representing"...

/**
  * A common super-type for typed AST nodes.
  */
sealed trait TypedAst

object TypedAst {

  /**
    * A typed AST node representing the root of the entire AST.
    *
    * @param constants a map from names to constant definitions.
    * @param directives a list of directives.
    * @param lattices a map from types to user-specified bounded lattice definitions.
    * @param collections a map from names to lattice or relation definitions.
    * @param indexes a map from collection names to indexes.
    * @param facts a list of facts.
    * @param rules a list of rules.
    * @param time the time spent in each compiler phase.
    */
  case class Root(constants: Map[Name.Resolved, TypedAst.Definition.Constant],
                  directives: TypedAst.Directives,
                  lattices: Map[TypedAst.Type, TypedAst.Definition.BoundedLattice],
                  collections: Map[Name.Resolved, TypedAst.Collection],
                  indexes: Map[Name.Resolved, TypedAst.Definition.Index],
                  facts: List[TypedAst.Constraint.Fact],
                  rules: List[TypedAst.Constraint.Rule],
                  time: Time) extends TypedAst {

    /**
      * Computes map of the dependencies between collection predicates.
      */
    val dependenciesOf: Map[Name.Resolved, mutable.Set[(Constraint.Rule, TypedAst.Predicate.Body.Collection)]] = {
      val result = mutable.Map.empty[Name.Resolved, mutable.Set[(Constraint.Rule, TypedAst.Predicate.Body.Collection)]]

      for (rule <- rules) {
        rule.head match {
          case TypedAst.Predicate.Head.Relation(name, _, _, _) => result.update(name, mutable.Set.empty)
          case _ => // nop
        }
      }

      for (outerRule <- rules) {
        for (innerRule <- rules) {
          for (body <- innerRule.body) {
            (outerRule.head, body) match {
              case (outer: TypedAst.Predicate.Head.Relation, inner: TypedAst.Predicate.Body.Collection) =>
                if (outer.name == inner.name) {
                  val deps = result(outer.name)
                  deps += ((innerRule, inner))
                }
              case _ => // nop
            }
          }
        }
      }
      result.toMap
    }

  }

  /**
    * A common super-type for typed definitions.
    */
  sealed trait Definition

  object Definition {

    /**
      * A typed AST node representing a constant definition.
      *
      * @param name the name of the constant.
      * @param exp the constant expression.
      * @param tpe the type of the constant.
      * @param loc the source location.
      */
    case class Constant(name: Name.Resolved, exp: TypedAst.Expression, tpe: TypedAst.Type, loc: SourceLocation) extends TypedAst.Definition

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
    case class BoundedLattice(tpe: TypedAst.Type, bot: TypedAst.Expression, top: TypedAst.Expression, leq: TypedAst.Expression,
                              lub: TypedAst.Expression, glb: TypedAst.Expression, loc: SourceLocation) extends TypedAst.Definition

    /**
      * A typed AST node representing an index definition.
      *
      * @param name the name of the collection.
      * @param indexes the selected indexes.
      * @param loc the source location.
      */
    case class Index(name: Name.Resolved, indexes: Seq[Seq[Name.Ident]], loc: SourceLocation) extends TypedAst.Definition

  }

  /**
    * A common super-type for collections that are either relations or lattices.
    */
  sealed trait Collection

  object Collection {

    /**
      * A typed AST node representing a relation definition.
      *
      * @param name the name of the relation.
      * @param attributes the attributes of the relation.
      * @param loc the source location.
      */
    case class Relation(name: Name.Resolved, attributes: List[TypedAst.Attribute], loc: SourceLocation) extends TypedAst.Collection

    /**
      * A typed AST node representing a lattice definition.
      *
      * @param name the name of the relation.
      * @param keys the keys of the lattice.
      * @param values the keys of the lattice.
      * @param loc the source location.
      */
    case class Lattice(name: Name.Resolved, keys: List[TypedAst.Attribute], values: List[TypedAst.Attribute], loc: SourceLocation) extends TypedAst.Collection

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
    case class Rule(head: TypedAst.Predicate.Head, body: List[TypedAst.Predicate.Body]) extends TypedAst.Constraint {

      val collections: List[TypedAst.Predicate.Body.Collection] = body collect {
        case p: TypedAst.Predicate.Body.Collection => p
      }

      val loops: List[TypedAst.Predicate.Body.Loop] = body collect {
        case p: TypedAst.Predicate.Body.Loop => p
      }

      val filters: List[TypedAst.Predicate.Body.Function] = body collect {
        case p: TypedAst.Predicate.Body.Function => p
      }

      val disjoint: List[TypedAst.Predicate.Body.NotEqual] = body collect {
        case p: TypedAst.Predicate.Body.NotEqual => p
      }

      // TODO: Refactor so this is not part of the typed ast.
      var elapsedTime: Long = 0
      var hitcount: Int = 0

    }

  }

  /**
    * A container class for all typed directives.
    */
  case class Directives(directives: List[TypedAst.Directive]) extends TypedAst {
    /**
      * A collection fact assertions in the program.
      */
    val assertedFacts: List[TypedAst.Directive.AssertFact] = directives collect {
      case d: TypedAst.Directive.AssertFact => d
    }

    /**
      * A collection of rule assertions in the program.
      */
    val assertedRules: List[TypedAst.Directive.AssertRule] = directives collect {
      case d: TypedAst.Directive.AssertRule => d
    }

    /**
      * A collection print directives in the program.
      */
    val prints: List[TypedAst.Directive.Print] = directives collect {
      case d: TypedAst.Directive.Print => d
    }
  }

  /**
    * A common super-type for typed directives.
    */
  sealed trait Directive

  object Directive {

    /**
      * A typed directive asserting a fact.
      *
      * @param fact the asserted fact.
      * @param loc the source location of the directive.
      */
    case class AssertFact(fact: TypedAst.Constraint.Fact, loc: SourceLocation) extends TypedAst.Directive

    /**
      * A typed directive asserting a rule.
      *
      * @param rule the asserted rule.
      * @param loc the source location of the directive.
      */
    case class AssertRule(rule: TypedAst.Constraint.Rule, loc: SourceLocation) extends TypedAst.Directive

    /**
      * A typed directive asserting that relation should be printed.
      *
      * @param name the name of the relation.
      * @param loc the source location of the directive.
      */
    case class Print(name: Name.Resolved, loc: SourceLocation) extends TypedAst.Directive

  }

  /**
    * A common super-type for typed literals.
    */
  sealed trait Literal extends TypedAst {
    /**
      * The type of `this` literal.
      */
    def tpe: TypedAst.Type

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
      final val tpe = TypedAst.Type.Unit
    }

    /**
      * A typed AST node representing a boolean literal.
      *
      * @param lit the boolean literal.
      * @param loc the source location.
      */
    case class Bool(lit: scala.Boolean, loc: SourceLocation) extends TypedAst.Literal {
      final val tpe = TypedAst.Type.Bool
    }

    /**
      * A typed AST node representing an integer literal.
      *
      * @param lit the integer literal.
      * @param loc the source location.
      */
    case class Int(lit: scala.Int, loc: SourceLocation) extends TypedAst.Literal {
      final val tpe = TypedAst.Type.Int
    }

    /**
      * A typed AST node representing a string literal.
      *
      * @param lit the string literal.
      * @param loc the source location.
      */
    case class Str(lit: java.lang.String, loc: SourceLocation) extends TypedAst.Literal {
      final val tpe = TypedAst.Type.Str
    }

    /**
      * A typed AST node representing a tagged literal.
      *
      * @param enum the enum name.
      * @param tag the tag name.
      * @param lit the nested literal.
      * @param tpe the type of the tag.
      * @param loc the source location.
      */
    case class Tag(enum: Name.Resolved, tag: Name.Ident, lit: TypedAst.Literal, tpe: TypedAst.Type.Enum, loc: SourceLocation) extends TypedAst.Literal

    /**
      * A typed AST node representing a tuple literal.
      *
      * @param elms the elements of the tuple.
      * @param tpe the type of the tuple.
      * @param loc the source location.
      */
    case class Tuple(elms: List[TypedAst.Literal], tpe: TypedAst.Type.Tuple, loc: SourceLocation) extends TypedAst.Literal {
      // TODO: Move
      val asArray: Array[TypedAst.Literal] = elms.toArray
    }

    /**
      * A typed AST node representing a Set literal.
      *
      * @param elms the elements of the set.
      * @param tpe the type of the set.
      * @param loc the source location.
      */
    case class Set(elms: List[TypedAst.Literal], tpe: TypedAst.Type.Set, loc: SourceLocation) extends TypedAst.Literal

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

    /**
      * A typed AST node representing a literal expression.
      *
      * @param literal the literal.
      * @param tpe the type of the literal.
      * @param loc the source location.
      */
    case class Lit(literal: TypedAst.Literal, tpe: TypedAst.Type, loc: SourceLocation) extends TypedAst.Expression

    /**
      * A typed AST node representing a local variable expression (i.e. a parameter or let-bound variable).
      *
      * @param ident the name of the variable.
      * @param tpe the type of the variable.
      */
    case class Var(ident: Name.Ident, tpe: TypedAst.Type, loc: SourceLocation) extends TypedAst.Expression

    /**
      * A typed AST node representing a reference to a definition (i.e. a value or function).
      *
      * @param name the name of the definition.
      * @param tpe the type of the definition.
      * @param loc the source location.
      */
    case class Ref(name: Name.Resolved, tpe: TypedAst.Type, loc: SourceLocation) extends TypedAst.Expression

    /**
      * A typed AST node representing a lambda abstraction.
      *
      * @param annotations the annotations.
      * @param args the formal arguments.
      * @param body the body expression of the lambda.
      * @param tpe the type of the entire function.
      * @param loc the source location.
      */
    case class Lambda(annotations: Ast.Annotations, args: List[TypedAst.FormalArg], body: TypedAst.Expression, tpe: TypedAst.Type.Lambda, loc: SourceLocation) extends TypedAst.Expression {
      // TODO: Move
      val argsAsArray: Array[TypedAst.FormalArg] = args.toArray
    }

    /**
      * A typed AST node representing a function call.
      *
      * @param exp the lambda/function expression.
      * @param args the function arguments.
      * @param tpe the return type of the function.
      * @param loc the source location.
      */
    case class Apply(exp: TypedAst.Expression, args: List[TypedAst.Expression], tpe: TypedAst.Type, loc: SourceLocation) extends TypedAst.Expression {
      // TODO: Move
      val argsAsArray: Array[TypedAst.Expression] = args.toArray
    }

    /**
      * A typed AST node representing a unary expression.
      *
      * @param op the unary operator.
      * @param exp the expression.
      * @param tpe the type
      * @param loc the source location.
      */
    case class Unary(op: UnaryOperator, exp: TypedAst.Expression, tpe: TypedAst.Type, loc: SourceLocation) extends TypedAst.Expression

    /**
      * A typed AST node representing a binary expression.
      *
      * @param op the binary operator.
      * @param exp1 the lhs expression.
      * @param exp2 the rhs expression.
      * @param tpe the type of the expression.
      * @param loc the source location.
      */
    case class Binary(op: BinaryOperator, exp1: TypedAst.Expression, exp2: TypedAst.Expression, tpe: TypedAst.Type, loc: SourceLocation) extends TypedAst.Expression

    /**
      * A typed AST node representing an if-then-else expression.
      *
      * @param exp1 the conditional expression.
      * @param exp2 the consequent expression.
      * @param exp3 the alternative expression.
      * @param tpe the type of the consequent and alternative expressions.
      * @param loc the source location.
      */
    case class IfThenElse(exp1: TypedAst.Expression, exp2: TypedAst.Expression, exp3: TypedAst.Expression, tpe: TypedAst.Type, loc: SourceLocation) extends TypedAst.Expression

    /**
      * A typed AST node representing a let expression.
      *
      * @param ident the name of the bound variable.
      * @param exp1 the value of the bound variable.
      * @param exp2 the body expression in which the bound variable is visible.
      * @param tpe the type of the expression (which is equivalent to the type of the body expression).
      * @param loc the source location.
      */
    case class Let(ident: Name.Ident, exp1: TypedAst.Expression, exp2: TypedAst.Expression, tpe: TypedAst.Type, loc: SourceLocation) extends TypedAst.Expression

    /**
      * A typed AST node representing a match expression.
      *
      * @param exp the match expression.
      * @param rules the match rules.
      * @param tpe the type of the match expression (which is equivalent to the type of each rule).
      * @param loc the source location.
      */
    case class Match(exp: TypedAst.Expression, rules: List[(TypedAst.Pattern, TypedAst.Expression)], tpe: TypedAst.Type, loc: SourceLocation) extends TypedAst.Expression {
      // TODO: Move
      val rulesAsArray: Array[(TypedAst.Pattern, TypedAst.Expression)] = rules.toArray
    }

    /**
      * A typed AST node representing a tagged expression.
      *
      * @param name the name of the enum.
      * @param ident the name of the tag.
      * @param exp the expression.
      * @param tpe the type of the expression.
      * @param loc the source location.
      */
    case class Tag(name: Name.Resolved, ident: Name.Ident, exp: TypedAst.Expression, tpe: TypedAst.Type.Enum, loc: SourceLocation) extends TypedAst.Expression

    /**
      * A typed AST node representing a tuple expression.
      *
      * @param elms the elements of the tuple.
      * @param tpe the type of the tuple.
      * @param loc the source location.
      */
    case class Tuple(elms: List[TypedAst.Expression], tpe: TypedAst.Type, loc: SourceLocation) extends TypedAst.Expression {
      // TODO: Move
      val asArray: Array[TypedAst.Expression] = elms.toArray
    }

    /**
      * A typed AST node representing a set expression.
      *
      * @param elms the elements of the set.
      * @param tpe the type of the set.
      * @param loc the source location.
      */
    case class Set(elms: List[TypedAst.Expression], tpe: TypedAst.Type.Set, loc: SourceLocation) extends TypedAst.Expression

    /**
      * A typed AST node representing an error expression.
      *
      * @param tpe the type of the error expression.
      * @param loc the source location.
      */
    case class Error(tpe: TypedAst.Type, loc: SourceLocation) extends TypedAst.Expression

    /**
      * A typed AST node representing a native field access expression.
      *
      * @param field the field itself
      * @param tpe the type of the field.
      * @param loc the source location.
      */
    case class NativeField(field: Field, tpe: TypedAst.Type, loc: SourceLocation) extends TypedAst.Expression

    /**
      * A typed AST node representing a native method expression.
      *
      * @param method the field itself
      * @param tpe the type of the method.
      * @param loc the source location.
      */
    case class NativeMethod(method: Method, tpe: TypedAst.Type, loc: SourceLocation) extends TypedAst.Expression

  }

  /**
    * A common-super type for typed patterns.
    */
  sealed trait Pattern extends TypedAst {
    /**
      * The type of `this` pattern.
      */
    def tpe: TypedAst.Type

    /**
      * The source location of `this` pattern.
      */
    def loc: SourceLocation

    /**
      * Returns the free variables (along with their types) in `this` pattern.
      */
    def freeVars: Map[String, TypedAst.Type] = {
      def visit(pat: TypedAst.Pattern, m: Map[String, TypedAst.Type]): Map[String, TypedAst.Type] =
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
    case class Wildcard(tpe: TypedAst.Type, loc: SourceLocation) extends TypedAst.Pattern

    /**
      * A typed AST node representing a variable pattern.
      *
      * @param ident the name of the variable.
      * @param tpe the type of the variable.
      * @param loc the source location.
      */
    case class Var(ident: Name.Ident, tpe: TypedAst.Type, loc: SourceLocation) extends TypedAst.Pattern

    /**
      * A typed AST node representing a literal pattern.
      *
      * @param lit the literal.
      * @param tpe the type of the literal.
      * @param loc the source location.
      */
    case class Lit(lit: TypedAst.Literal, tpe: TypedAst.Type, loc: SourceLocation) extends TypedAst.Pattern

    /**
      * A typed AST node representing a tagged pattern.
      *
      * @param name the namespace of the tag.
      * @param ident the tag name.
      * @param pat the nested pattern.
      * @param tpe the type of the tag.
      * @param loc the source location.
      */
    case class Tag(name: Name.Resolved, ident: Name.Ident, pat: TypedAst.Pattern, tpe: TypedAst.Type.Tag, loc: SourceLocation) extends TypedAst.Pattern

    /**
      * A typed AST node representing a tuple pattern.
      *
      * @param elms the elements of the tuple.
      * @param tpe the type of the tuple.
      * @param loc the source location.
      */
    case class Tuple(elms: List[TypedAst.Pattern], tpe: TypedAst.Type.Tuple, loc: SourceLocation) extends TypedAst.Pattern {
      // TODO: Move
      val asArray: Array[TypedAst.Pattern] = elms.toArray
    }

  }

  /**
    * A common super-type for typed predicates.
    */
  sealed trait Predicate extends TypedAst {
    /**
      * The type of the predicate.
      */
    def tpe: TypedAst.Type

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

      /**
        * A typed relational predicate that occurs in the head of a fact/rule.
        *
        * @param name the name of the predicate.
        * @param terms the terms of the predicate.
        * @param tpe the type of the predicate.
        * @param loc the source location.
        */
      // TODO: Need better name....could also be a lattice...
      case class Relation(name: Name.Resolved, terms: List[TypedAst.Term.Head], tpe: TypedAst.Type.Predicate, loc: SourceLocation) extends TypedAst.Predicate.Head {
        /**
          * Returns the arity of the predicate.
          */
        val arity: Int = terms.length
        /**
          * Returns the terms as an array.
          */
        // TODO: Move this into a more appropiate IR.
        val termsArray: Array[TypedAst.Term.Head] = terms.toArray
      }

      /**
        * A typed trace predicate that occurs in the head of a rule.
        *
        * @param terms the terms of the predicate.
        * @param tpe the type of the predicate.
        * @param loc the source location.
        */
      case class Trace(terms: List[TypedAst.Term.Head], tpe: TypedAst.Type, loc: SourceLocation) extends TypedAst.Predicate.Head

      /**
        * A typed write predicate that occurs in the head of a rule.
        *
        * @param terms the terms of the predicate.
        * @param path the path to write to.
        * @param tpe the type of the predicate.
        * @param loc the source location.
        */
      case class Write(terms: List[TypedAst.Term.Head], path: TypedAst.Term.Head, tpe: TypedAst.Type, loc: SourceLocation) extends TypedAst.Predicate.Head

      /**
        * A typed error predicate that occurs in the head of a rule.
        *
        * @param terms the terms of the predicate.
        * @param tpe the type of the predicate.
        * @param loc the source location.
        */
      case class Error(terms: List[TypedAst.Term.Head], tpe: TypedAst.Type, loc: SourceLocation) extends TypedAst.Predicate.Head

    }

    /**
      * A common super-type for body predicates.
      */
    sealed trait Body extends TypedAst.Predicate {
      /**
        * Returns the set of free variables in the term.
        */
      def freeVars: Set[String] = this match {
        case TypedAst.Predicate.Body.Collection(_, terms, _, _) => terms.foldLeft(Set.empty[String]) {
          case (xs, t: TypedAst.Term.Body.Wildcard) => xs
          case (xs, t: TypedAst.Term.Body.Var) => xs + t.ident.name
          case (xs, t: TypedAst.Term.Body.Lit) => xs
        }
        case TypedAst.Predicate.Body.Function(_, terms, _, _) => terms.foldLeft(Set.empty[String]) {
          case (xs, t: TypedAst.Term.Body.Wildcard) => xs
          case (xs, t: TypedAst.Term.Body.Var) => xs + t.ident.name
          case (xs, t: TypedAst.Term.Body.Lit) => xs
        }
        case TypedAst.Predicate.Body.NotEqual(x, y, _, _) => Set(x.name, y.name)
        case TypedAst.Predicate.Body.Read(terms, body, _, _) => ???
        case TypedAst.Predicate.Body.Loop(_, _, _, _) => ???
      }
    }

    object Body {

      /**
        * A typed collection predicate that occurs in the body of a rule.
        *
        * @param name the name of the relation.
        * @param terms the terms of the predicate.
        * @param tpe the type of the predicate.
        * @param loc the source location.
        */
      case class Collection(name: Name.Resolved, terms: List[TypedAst.Term.Body], tpe: TypedAst.Type.Predicate, loc: SourceLocation) extends TypedAst.Predicate.Body {
        /**
          * Returns the arity of this collection predicate.
          */
        val arity: Int = terms.length

        /**
          * Returns the terms as an array.
          */
        // TODO: Move this into a more appropiate IR.
        val termsArray: Array[TypedAst.Term.Body] = terms.toArray

        // TODO: Move this into a more appropiate IR.
        val index2var: Array[String] = {
          val r = new Array[String](terms.length)
          var i = 0
          while (i < r.length) {
            terms(i) match {
              case TypedAst.Term.Body.Var(ident, _, _) =>
                r(i) = ident.name
              case _ => // nop
            }
            i = i + 1
          }
          r
        }
      }

      /**
        * A typed functional predicate that occurs in the body of a rule.
        *
        * @param name the name of the function.
        * @param terms the terms of the predicate.
        * @param tpe the type of the predicate.
        * @param loc the source location.
        */
      case class Function(name: Name.Resolved, terms: List[TypedAst.Term.Body], tpe: TypedAst.Type.Lambda, loc: SourceLocation) extends TypedAst.Predicate.Body {
        // TODO: Move
        val termsAsArray: Array[TypedAst.Term.Body] = terms.toArray
      }

      /**
        * A typed not equal predicate that occurs in the body of a rule.
        *
        * @param ident1 the name of the first variable.
        * @param ident2 the name of the second variable.
        * @param tpe the type of the predicate.
        * @param loc the source location.
        */
      case class NotEqual(ident1: Name.Ident, ident2: Name.Ident, tpe: TypedAst.Type, loc: SourceLocation) extends TypedAst.Predicate.Body

      /**
        * An AST node that represents the special loop predicate.
        *
        * @param ident the loop variable.
        * @param term the set term.
        * @param tpe the type of the predicate.
        * @param loc the source location.
        */
      case class Loop(ident: Name.Ident, term: TypedAst.Term.Head, tpe: TypedAst.Type, loc: SourceLocation) extends TypedAst.Predicate.Body

      /**
        * A typed read predicate that occurs in the head of a rule.
        *
        * @param terms the terms of the predicate.
        * @param path the path to read from.
        * @param tpe the type of the predicate.
        * @param loc the source location.
        */
      case class Read(terms: List[TypedAst.Term.Body], path: TypedAst.Term.Body, tpe: TypedAst.Type, loc: SourceLocation) extends TypedAst.Predicate.Body


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
      def tpe: TypedAst.Type

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
        * @param tpe the type of the term.
        * @param loc the source location.
        */
      case class Var(ident: Name.Ident, tpe: TypedAst.Type, loc: SourceLocation) extends TypedAst.Term.Head

      /**
        * A typed AST node representing a literal term.
        *
        * @param literal the literal.
        * @param tpe the type of the term.
        * @param loc the source location.
        */
      case class Lit(literal: TypedAst.Literal, tpe: TypedAst.Type, loc: SourceLocation) extends TypedAst.Term.Head

      /**
        * A typed AST node representing a function call term.
        *
        * @param name the name of the called function.
        * @param args the arguments to the function.
        * @param tpe the type of the term.
        * @param loc the source location.
        */
      case class Apply(name: Name.Resolved, args: List[TypedAst.Term.Head], tpe: TypedAst.Type, loc: SourceLocation) extends TypedAst.Term.Head {
        // TODO: Move
        val argsAsArray: Array[TypedAst.Term.Head] = args.toArray
      }

      /**
        * A typed AST node representing a reference to a native JVM static field.
        *
        * @param field the field.
        * @param tpe the type of the field.
        * @param loc the source location.
        */
      case class NativeField(field: Field, tpe: TypedAst.Type, loc: SourceLocation) extends TypedAst.Term.Head

    }

    /**
      * A common super-type for terms that are allowed to appear in a body predicate.
      */
    sealed trait Body extends TypedAst {
      /**
        * The type of `this` term.
        */
      def tpe: TypedAst.Type

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
      case class Wildcard(tpe: TypedAst.Type, loc: SourceLocation) extends TypedAst.Term.Body

      /**
        * A typed AST node representing a variable term.
        *
        * @param ident the variable name.
        * @param tpe the type of the term.
        * @param loc the source location.
        */
      case class Var(ident: Name.Ident, tpe: TypedAst.Type, loc: SourceLocation) extends TypedAst.Term.Body

      /**
        * A typed AST node representing a literal term.
        *
        * @param lit the literal.
        * @param tpe the type of the term.
        * @param loc the source location.
        */
      case class Lit(lit: TypedAst.Literal, tpe: TypedAst.Type, loc: SourceLocation) extends TypedAst.Term.Body

    }

  }

  /**
    * A common super-type for types.
    */
  sealed trait Type extends TypedAst

  object Type {

    case object Any extends TypedAst.Type

    /**
      * A type variable.
      */
    case class Var(x: String) extends TypedAst.Type

    /**
      * An AST node representing the Unit type.
      */
    case object Unit extends TypedAst.Type

    /**
      * An AST node representing the Boolean type.
      */
    case object Bool extends TypedAst.Type

    /**
      * An AST node representing the Integer type.
      */
    case object Int extends TypedAst.Type

    /**
      * An AST node representing the String type.
      */
    case object Str extends TypedAst.Type

    /**
      * An AST node representing the type of a tag.
      *
      * @param name the namespace of the tag.
      * @param ident the name of the tag.
      * @param tpe the type of the nested value.
      */
    case class Tag(name: Name.Resolved, ident: Name.Ident, tpe: TypedAst.Type) extends TypedAst.Type

    /**
      * An AST node representing an enum type (a set of tags).
      *
      * @param cases a map from tag names to tag types.
      */
    case class Enum(cases: Map[String, TypedAst.Type.Tag]) extends TypedAst.Type

    /**
      * An AST node representing a tuple type.
      *
      * @param elms the types of the elements.
      */
    case class Tuple(elms: List[TypedAst.Type]) extends TypedAst.Type {
      // TODO: Move
      val asArray: Array[TypedAst.Type] = elms.toArray
    }

    /**
      * An AST node representing a set type.
      *
      * @param elmType the types of the elements.
      */
    case class Set(elmType: TypedAst.Type) extends TypedAst.Type

    case class Lst(elmType: TypedAst.Type) extends TypedAst.Type

    /**
      * An AST node representing a function type.
      *
      * @param args the type of the arguments.
      * @param retTpe the type of the return type.
      */
    case class Lambda(args: List[TypedAst.Type], retTpe: TypedAst.Type) extends TypedAst.Type

    /**
      * An AST node representing a predicate type.
      *
      * @param terms the terms of the predicate.
      */
    case class Predicate(terms: List[TypedAst.Type]) extends TypedAst.Type

    /**
      * An AST node that represents a native type.
      *
      * @param name the fully qualified name of the type.
      */
    case class Native(name: String) extends TypedAst.Type

  }

  /**
    * A typed AST node representing an attribute in a relation.
    *
    * @param ident the name of the attribute.
    * @param tpe  the type of the attribute.
    */
  case class Attribute(ident: Name.Ident, tpe: TypedAst.Type) extends TypedAst

  /**
    * A typed AST node representing a formal argument of a function.
    *
    * @param ident the name of the argument.
    * @param tpe the type of the argument.
    */
  case class FormalArg(ident: Name.Ident, tpe: TypedAst.Type) extends TypedAst

}

package ca.uwaterloo.flix.language.ast

sealed trait SimplifiedAst

object SimplifiedAst {

  case class Root(constants: Map[Name.Resolved, SimplifiedAst.Definition.Constant],
                  directives: SimplifiedAst.Directives,
                  lattices: Map[Type, SimplifiedAst.Definition.Lattice],
                  collections: Map[Name.Resolved, SimplifiedAst.Collection],
                  indexes: Map[Name.Resolved, SimplifiedAst.Definition.Index],
                  facts: List[SimplifiedAst.Constraint.Fact],
                  rules: List[SimplifiedAst.Constraint.Rule],
                  time: Time) extends SimplifiedAst

  sealed trait Definition

  object Definition {

    case class Constant(name: Name.Resolved,
                        exp: SimplifiedAst.Expression,
                        tpe: Type,
                        loc: SourceLocation) extends SimplifiedAst.Definition

    case class Lattice(tpe: Type,
                       bot: SimplifiedAst.Expression,
                       top: SimplifiedAst.Expression,
                       leq: SimplifiedAst.Expression,
                       lub: SimplifiedAst.Expression,
                       glb: SimplifiedAst.Expression,
                       loc: SourceLocation) extends SimplifiedAst.Definition

    case class Index(name: Name.Resolved,
                     indexes: Seq[Seq[Name.Ident]],
                     loc: SourceLocation) extends SimplifiedAst.Definition

    /**
      * An AST node that represents the definition of a function.
      *
      * @param name the resolved name of the function.
      * @param args the arguments of the function, for debugging purposes.
      * @param body the expression body of the function.
      * @param tpe the (lambda) type of the function.
      * @param loc the source location of the function definition.
      */
    case class Function(name: Name.Resolved,
                        args: List[String],
                        body: SimplifiedAst.Expression,
                        tpe: Type.Lambda,
                        loc: SourceLocation) extends SimplifiedAst.Definition

  }

  sealed trait Collection

  object Collection {

    case class Relation(name: Name.Resolved,
                        attributes: List[SimplifiedAst.Attribute],
                        loc: SourceLocation) extends SimplifiedAst.Collection

    case class Lattice(name: Name.Resolved,
                       keys: List[SimplifiedAst.Attribute],
                       values: List[SimplifiedAst.Attribute],
                       loc: SourceLocation) extends SimplifiedAst.Collection

  }

  sealed trait Constraint extends SimplifiedAst

  object Constraint {

    case class Fact(head: SimplifiedAst.Predicate.Head) extends SimplifiedAst.Constraint

    case class Rule(head: SimplifiedAst.Predicate.Head,
                    body: List[SimplifiedAst.Predicate.Body]) extends SimplifiedAst.Constraint

  }

  case class Directives(directives: List[SimplifiedAst.Directive]) extends SimplifiedAst

  sealed trait Directive

  object Directive {

    case class AssertFact(fact: SimplifiedAst.Constraint.Fact, loc: SourceLocation) extends SimplifiedAst.Directive

    case class AssertRule(rule: SimplifiedAst.Constraint.Rule, loc: SourceLocation) extends SimplifiedAst.Directive

    case class Print(name: Name.Resolved, loc: SourceLocation) extends SimplifiedAst.Directive

  }

  sealed trait Expression extends SimplifiedAst {
    def tpe: Type
  }

  sealed trait LoadExpression extends Expression {
    val e: SimplifiedAst.Expression
    val offset: Int
    val mask: Int
  }

  sealed trait StoreExpression extends Expression {
    val e: SimplifiedAst.Expression
    val offset: Int
    val v: SimplifiedAst.Expression
    val mask: Long
    final val targetMask = ~(mask << offset)
    final val tpe = Type.Int64
  }

  object Expression {

    case object Unit extends SimplifiedAst.Expression {
      final val tpe = Type.Unit
    }

    case object True extends SimplifiedAst.Expression {
      final val tpe = Type.Bool
    }

    case object False extends SimplifiedAst.Expression {
      final val tpe = Type.Bool
    }

    case class Int(lit: scala.Int) extends SimplifiedAst.Expression {
      final val tpe = Type.Int
    }

    case class Str(lit: java.lang.String, loc: SourceLocation) extends SimplifiedAst.Expression {
      final val tpe = Type.Str
    }

    /**
      * A typed AST node representing a constant integer literal.
      * Note that calling this case class "Int" conflicts with scala.Int.
      *
      * @param value the integer value.
      * @param tpe the type of the integer.
      * @param loc the source location of the integer.
      */
    // TODO(mhyee): Remove this
    @deprecated("placeholder until it can be merged into SimplifiedAst.Int", "0.1")
    case class Const(value: Long, tpe: Type, loc: SourceLocation) extends SimplifiedAst.Expression

    /**
      * An AST node representing a value (of type Bool) loaded from an Int64.
      *
      * @param e the expression, returning an Int64, that the value is loaded from.
      * @param offset the offset (in bits) from the least significant bit that the value is loaded from.
      */
    case class LoadBool(e: SimplifiedAst.Expression, offset: scala.Int) extends SimplifiedAst.LoadExpression {
      val mask = 1
      val tpe = Type.Bool
    }

    /**
      * An AST node representing a value (of type Int8) loaded from an Int64.
      *
      * @param e the expression, returning an Int64, that the value is loaded from.
      * @param offset the offset (in bits) from the least significant bit that the value is loaded from.
      */
    case class LoadInt8(e: SimplifiedAst.Expression, offset: scala.Int) extends SimplifiedAst.LoadExpression {
      val mask = 0xFF
      val tpe = Type.Int8
    }

    /**
      * An AST node representing a value (of type Int16) loaded from an Int64.
      *
      * @param e the expression, returning an Int64, that the value is loaded from.
      * @param offset the offset (in bits) from the least significant bit that the value is loaded from.
      */
    case class LoadInt16(e: SimplifiedAst.Expression, offset: scala.Int) extends SimplifiedAst.LoadExpression {
      val mask = 0xFFFF
      val tpe = Type.Int16
    }

    /**
      * An AST node representing a value (of type Int32) loaded from an Int64.
      *
      * @param e the expression, returning an Int64, that the value is loaded from.
      * @param offset the offset (in bits) from the least significant bit that the value is loaded from.
      */
    case class LoadInt32(e: SimplifiedAst.Expression, offset: scala.Int) extends SimplifiedAst.LoadExpression {
      val mask = -1 // if we had unsigned ints, would be 0xFFFFFFFF
      val tpe = Type.Int32
    }

    /**
      * An AST node representing a value (of type Bool) to be stored into an Int64.
      *
      * @param e the expression, returning an Int64, that the value is stored into.
      * @param offset the offset (in bits) from the least significant bit that the value is stored into.
      * @param v the value to be stored.
      */
    case class StoreBool(e: SimplifiedAst.Expression,
                         offset: scala.Int,
                         v: SimplifiedAst.Expression) extends SimplifiedAst.StoreExpression {
      val mask = 0x1L
    }

    /**
      * An AST node representing a value (of type Int8) to be stored into an Int64.
      *
      * @param e the expression, returning an Int64, that the value is stored into.
      * @param offset the offset (in bits) from the least significant bit that the value is stored into.
      * @param v the value to be stored.
      */
    case class StoreInt8(e: SimplifiedAst.Expression,
                         offset: scala.Int, v:
                         SimplifiedAst.Expression) extends SimplifiedAst.StoreExpression {
      val mask = 0xFFL
    }

    /**
      * An AST node representing a value (of type Int16) to be stored into an Int64.
      *
      * @param e the expression, returning an Int64, that the value is stored into.
      * @param offset the offset (in bits) from the least significant bit that the value is stored into.
      * @param v the value to be stored.
      */
    case class StoreInt16(e: SimplifiedAst.Expression,
                          offset: scala.Int, v:
                          SimplifiedAst.Expression) extends SimplifiedAst.StoreExpression {
      val mask = 0xFFFFL
    }

    /**
      * An AST node representing a value (of type Int32) to be stored into an Int64.
      *
      * @param e the expression, returning an Int64, that the value is stored into.
      * @param offset the offset (in bits) from the least significant bit that the value is stored into.
      * @param v the value to be stored.
      */
    case class StoreInt32(e: SimplifiedAst.Expression,
                          offset: scala.Int, v:
                          SimplifiedAst.Expression) extends SimplifiedAst.StoreExpression {
      val mask = 0xFFFFFFFFL
    }

    /**
      * A typed AST node representing a local variable expression (i.e. a parameter or let-bound variable).
      *
      * @param localVar the local variable being referenced.
      * @param tpe the type of the variable.
      * @param loc the source location of the variable.
      */
    // TODO(mhyee): Remove this
    @deprecated("placeholder until it can be merged with SimplifiedAst", "0.1")
    case class ReducedIrVar(localVar: LocalVar,
                            tpe: Type,
                            loc: SourceLocation) extends SimplifiedAst.Expression

    case class Var(ident: Name.Ident, tpe: Type, loc: SourceLocation) extends SimplifiedAst.Expression

    case class Ref(name: Name.Resolved, tpe: Type, loc: SourceLocation) extends SimplifiedAst.Expression

    // TODO: Lambda lift?
    case class Lambda(annotations: Ast.Annotations,
                      args: List[SimplifiedAst.FormalArg],
                      body: SimplifiedAst.Expression,
                      tpe: Type.Lambda,
                      loc: SourceLocation) extends SimplifiedAst.Expression

    /**
      * A typed AST node representing a function call.
      *
      * @param name the name of the function being called.
      * @param args the function arguments.
      * @param tpe the return type of the function.
      * @param loc the source location.
      */
    // TODO(mhyee): Remove this
    @deprecated("placeholder until it can be merged with SimplifiedAst", "0.1")
    case class ReducedIrApply(name: Name.Resolved,
                              args: List[SimplifiedAst.Expression],
                              tpe: Type,
                              loc: SourceLocation) extends SimplifiedAst.Expression

    case class Apply(exp: SimplifiedAst.Expression,
                     args: List[SimplifiedAst.Expression],
                     tpe: Type,
                     loc: SourceLocation) extends SimplifiedAst.Expression

    /**
      * A typed AST node representing a unary expression.
      *
      * @param op the unary operator.
      * @param exp the expression.
      * @param tpe the type of the expression.
      * @param loc the source location of the expression.
      */
    case class Unary(op: UnaryOperator,
                     exp: SimplifiedAst.Expression,
                     tpe: Type, loc: SourceLocation) extends SimplifiedAst.Expression

    /**
      * A typed AST node representing a binary expression.
      *
      * @param op the binary operator.
      * @param exp1 the left expression.
      * @param exp2 the right expression.
      * @param tpe the type of the expression.
      * @param loc the source location of the expression.
      */
    case class Binary(op: BinaryOperator,
                      exp1: SimplifiedAst.Expression,
                      exp2: SimplifiedAst.Expression,
                      tpe: Type,
                      loc: SourceLocation) extends SimplifiedAst.Expression

    /**
      * A typed AST node representing an if-then-else expression.
      *
      * @param exp1 the conditional expression.
      * @param exp2 the consequent expression.
      * @param exp3 the alternative expression.
      * @param tpe the type of the consequent and alternative expression.
      * @param loc the source location of the expression.
      */
    case class IfThenElse(exp1: SimplifiedAst.Expression,
                          exp2: SimplifiedAst.Expression,
                          exp3: SimplifiedAst.Expression,
                          tpe: Type,
                          loc: SourceLocation) extends SimplifiedAst.Expression

    /**
      * A typed AST node representing a let expression.
      *
      * @param localVar the bound variable.
      * @param exp1 the value of the bound variable.
      * @param exp2 the body expression in which the bound variable is visible.
      * @param tpe the type of the expression (which is equivalent to the type of the body expression).
      * @param loc the source location.
      */
    // TODO(mhyee): Remove this
    @deprecated("placeholder until it can be merged with SimplifiedAst", "0.1")
    case class ReducedIrLet(localVar: LocalVar,
                            exp1: SimplifiedAst.Expression,
                            exp2: SimplifiedAst.Expression,
                            tpe: Type,
                            loc: SourceLocation) extends SimplifiedAst.Expression

    case class Let(ident: Name.Ident,
                   exp1: SimplifiedAst.Expression,
                   exp2: SimplifiedAst.Expression,
                   tpe: Type,
                   loc: SourceLocation) extends SimplifiedAst.Expression

    // TODO(mhyee): Remove this
    @deprecated("placeholder until it can be merged with SimplifiedAst", "0.1")
    case class ReducedIrTag(name: Name.Resolved,
                            tag: String,
                            exp: SimplifiedAst.Expression,
                            tpe: Type,
                            loc: SourceLocation) extends SimplifiedAst.Expression

    // Destructs a (ReducedIr)Tag
    // TODO(mhyee): Remove this
    @deprecated("placeholder until it can be merged with SimplifiedAst", "0.1")
    case class TagOf(exp: SimplifiedAst.Expression,
                     name: Name.Resolved,
                     tag: String,
                     tpe: Type,
                     loc: SourceLocation) extends SimplifiedAst.Expression

    case class Tag(enum: Name.Resolved,
                   tag: Name.Ident,
                   exp: SimplifiedAst.Expression,
                   tpe: Type.Enum,
                   loc: SourceLocation) extends SimplifiedAst.Expression

    // Destructs a Tuple
    // TODO(mhyee): Remove this
    @deprecated("placeholder until it can be merged with SimplifiedAst", "0.1")
    case class TupleAt(base: SimplifiedAst.Expression,
                       offset: scala.Int,
                       tpe: Type,
                       loc: SourceLocation) extends SimplifiedAst.Expression

    case class Tuple(elms: List[SimplifiedAst.Expression],
                     tpe: Type,
                     loc: SourceLocation) extends SimplifiedAst.Expression

    case class Set(elms: List[SimplifiedAst.Expression],
                   tpe: Type.Set,
                   loc: SourceLocation) extends SimplifiedAst.Expression

    case class Error(tpe: Type, loc: SourceLocation) extends SimplifiedAst.Expression

    case class MatchError(tpe: Type, loc: SourceLocation) extends SimplifiedAst.Expression

  }

  sealed trait Predicate extends SimplifiedAst {
    def tpe: Type
    def loc: SourceLocation
  }

  object Predicate {

    sealed trait Head extends SimplifiedAst.Predicate

    object Head {

      case class Relation(name: Name.Resolved,
                          terms: List[SimplifiedAst.Term.Head],
                          tpe: Type.Predicate,
                          loc: SourceLocation) extends SimplifiedAst.Predicate.Head

    }

    sealed trait Body extends SimplifiedAst.Predicate

    object Body {

      case class Collection(name: Name.Resolved,
                            terms: List[SimplifiedAst.Term.Body],
                            tpe: Type.Predicate,
                            loc: SourceLocation) extends SimplifiedAst.Predicate.Body

      case class Function(name: Name.Resolved,
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
      case class Apply(name: Name.Resolved,
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

      case class Var(v: Int, tpe: Type, loc: SourceLocation) extends SimplifiedAst.Term.Body

      // TODO: Lambda lift?
      case class Exp(e: SimplifiedAst.Expression, tpe: Type, loc: SourceLocation) extends SimplifiedAst.Term.Body

    }

  }

  case class Attribute(ident: Name.Ident, tpe: Type) extends SimplifiedAst

  case class FormalArg(ident: Name.Ident, tpe: Type) extends SimplifiedAst

  /**
    * A local variable that is being referenced.
    *
    * @param offset the (0-based) local variable slot in the JVM method.
    * @param name the name of the variable, for debugging purposes.
    */
  case class LocalVar(offset: scala.Int, name: String) extends SimplifiedAst

}

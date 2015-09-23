package ca.uwaterloo.flix.lang.phase

import ca.uwaterloo.flix.lang.ast.{SourceLocation, WeededAst, TypedAst, ResolvedAst}

import util.Validation
import util.Validation._

object Typer {

  import TypeError._

  sealed trait TypeError {
    def format: String
  }

  object TypeError {

    case class ExpectedType(expected: TypedAst.Type, actual: TypedAst.Type, location: SourceLocation) extends TypeError {
      val format = ???
    }

    case class ExpectedEqualTypes(tpe1: TypedAst.Type, tpe2: TypedAst.Type, location: SourceLocation) extends TypeError {
      val format = ???
    }

  }

  /**
   * Runs the typer on the entire given AST `rast`.
   */
  def typecheck(root: ResolvedAst.Root): Validation[TypedAst.Root, TypeError] = {

    // TODO: Need to implement Validation.fold over maps.

    val factsVal = @@(root.facts.map(fact => Constraint.typer(fact, root)))
    val rulesVal = @@(root.rules.map(rule => Constraint.typer(rule, root)))

    @@(factsVal, rulesVal) map {
      case (facts, rules) => TypedAst.Root(Map.empty, Map.empty, Map.empty, facts, rules)
    }
  }


  object Definition {

    def typer(rast: ResolvedAst.Definition.Constant, root: ResolvedAst.Root): Validation[TypedAst.Definition.Constant, TypeError] = {
      ???
    }

    def typer(rast: ResolvedAst.Definition.Lattice, root: ResolvedAst.Root): Validation[TypedAst.Definition.Lattice, TypeError] = {
      ???
    }

    def typer(rast: ResolvedAst.Definition.Relation, root: ResolvedAst.Root): Validation[TypedAst.Definition.Relation, TypeError] = {
      ???
    }

  }

  object Constraint {

    def typer(rast: ResolvedAst.Constraint.Fact, root: ResolvedAst.Root): Validation[TypedAst.Constraint.Fact, TypeError] = {
      ???
    }

    def typer(rast: ResolvedAst.Constraint.Rule, root: ResolvedAst.Root): Validation[TypedAst.Constraint.Rule, TypeError] = {
      ???
    }
  }

  object Literal {

    /**
     * Types the given resolved literal `rast`.
     */
    def typer(rast: ResolvedAst.Literal, root: ResolvedAst.Root): TypedAst.Literal = {
      def visit(rast: ResolvedAst.Literal): TypedAst.Literal = rast match {
        case ResolvedAst.Literal.Unit => TypedAst.Literal.Unit
        case ResolvedAst.Literal.Bool(b) => TypedAst.Literal.Bool(b)
        case ResolvedAst.Literal.Int(i) => TypedAst.Literal.Int(i)
        case ResolvedAst.Literal.Str(s) => TypedAst.Literal.Str(s)
        case ResolvedAst.Literal.Tag(name, ident, rlit) =>
          val defn = root.enums(name)
          val cases = defn.cases.map {
            case (tag, tpe) => tag -> Type.typer(tpe).asInstanceOf[TypedAst.Type.Tag]
          }
          TypedAst.Literal.Tag(name, ident, visit(rlit), TypedAst.Type.Enum(cases))
        case ResolvedAst.Literal.Tuple(relms) =>
          val elms = relms map visit
          TypedAst.Literal.Tuple(elms, TypedAst.Type.Tuple(elms map (_.tpe)))
      }

      visit(rast)
    }
  }


  object Expression {
    // TODO: Visit
    def typer(rast: ResolvedAst.Expression, root: ResolvedAst.Root, env: Map[String, TypedAst.Type] = Map.empty): Validation[TypedAst.Expression, TypeError] = rast match {
      case ResolvedAst.Expression.Var(ident) => ??? // pull type out of map.

      case ResolvedAst.Expression.Ref(name) => ??? // TODO

      case ResolvedAst.Expression.Let(ident, rvalue, rbody) =>
        typer(rvalue, root, env) flatMap {
          case value => typer(rbody, root, env + (ident.name -> value.tpe)) map {
            case body => TypedAst.Expression.Let(ident, value, body, body.tpe)
          }
        }

      case ResolvedAst.Expression.IfThenElse(re1, re2, re3) =>
        @@(typer(re1, root, env), typer(re2, root, env), typer(re3, root, env)) flatMap {
          case (e1, e2, e3) =>
            val conditionType = expect(TypedAst.Type.Bool)(e1.tpe)
            val expressionType = expectEqual(e2.tpe, e3.tpe)
            #@(conditionType, expressionType) map {
              case tpe => TypedAst.Expression.IfThenElse(e1, e2, e3, tpe)
            }
        }

    }

  }

  object Pattern {
    def typer(rast: ResolvedAst.Pattern, tpe: TypedAst.Type): Validation[TypedAst.Pattern, TypeError] = ???
  }

  object Type {

    /**
     * Compiles the given resolved AST node type to a typed AST node.
     */
    def typer(rast: ResolvedAst.Type): TypedAst.Type = rast match {
      case ResolvedAst.Type.Unit => TypedAst.Type.Unit
    }

  }


  //  def unify(pattern: TypedAst.Pattern, tpe: TypedAst.Type): Validation[Map[String, TypedAst.Type], TypeError] =
  //    (pattern, tpe) match {
  //      case (TypedAst.Pattern.Wildcard(_), _) => Map.empty[String, TypedAst.Type].toSuccess
  //      case (TypedAst.Pattern.Var(ident), t) => Map(ident.name -> t).toSuccess
  //      case (TypedAst.Pattern.Lit(literal), t) => ??? // TODO: Get type of the literal and then we are good?
  //        // TODO: remaining cases.
  //    }

  def expect(tpe1: TypedAst.Type)(tpe2: TypedAst.Type): Validation[TypedAst.Type, TypeError] = ???

  def expectEqual(tpe1: TypedAst.Type, tpe2: TypedAst.Type): Validation[TypedAst.Type, TypeError] = ???

}

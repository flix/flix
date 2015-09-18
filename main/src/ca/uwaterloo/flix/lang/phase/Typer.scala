package ca.uwaterloo.flix.lang.phase

import ca.uwaterloo.flix.lang.ast.{WeededAst, TypedAst, ResolvedAst}
import org.scalatest.events.Location

import util.Validation
import util.Validation._

object Typer {

  import TypeError._

  sealed trait TypeError {
    def format: String
  }

  object TypeError {

    // TODO

    case class ExpectedType(expected: TypedAst.Type, actual: TypedAst.Type, location: Location) extends TypeError {
      val format = ???
    }

    case class ExpectedEqualTypes(tpe1: TypedAst.Type, tpe2: TypedAst.Type, location: Location) extends TypeError {
      val format = ???
    }

  }


  def typecheck(rast: ResolvedAst.Root): Validation[TypedAst.Root, TypeError] = ???

  object Literal {

    def typer(rast: ResolvedAst.Literal): Validation[TypedAst.Literal, TypeError] = rast match {
      case ResolvedAst.Literal.Unit => TypedAst.Literal.Unit.toSuccess
      case ResolvedAst.Literal.Bool(b) => TypedAst.Literal.Bool(b).toSuccess
      case ResolvedAst.Literal.Int(i) => TypedAst.Literal.Int(i).toSuccess
      case ResolvedAst.Literal.Str(s) => TypedAst.Literal.Str(s).toSuccess
      case ResolvedAst.Literal.Tag(name, ident, literal, defn) => ??? // TODO
      case ResolvedAst.Literal.Tuple(relms) => @@(relms map typer) map {
        case elms => TypedAst.Literal.Tuple(elms, TypedAst.Type.Tuple(elms map (_.tpe)))
      }
    }

  }

  object Expression {

    def typer(rast: ResolvedAst.Expression, env0: Map[String, TypedAst.Type]): Validation[TypedAst.Expression, TypeError] = rast match {
      case ResolvedAst.Expression.Var(ident) => ??? // pull type out of map.

      case ResolvedAst.Expression.Ref(name, decl) => ??? // TODO

      case ResolvedAst.Expression.Let(ident, rvalue, rbody) =>
        typer(rvalue, env0) flatMap {
          case value => typer(rbody, env0 + (ident.name -> value.tpe)) map {
            case body => TypedAst.Expression.Let(ident, value, body, body.tpe)
          }
        }

      case ResolvedAst.Expression.IfThenElse(re1, re2, re3) =>
        @@(typer(re1, env0), typer(re2, env0), typer(re3, env0)) flatMap {
          case (e1, e2, e3) =>
            val conditionType = expect(TypedAst.Type.Bool)(e1.tpe)
            val expressionType = expectEqual(e2.tpe, e3.tpe)
            #@(conditionType, expressionType) map {
              case tpe => TypedAst.Expression.IfThenElse(e1, e2, e3, tpe)
            }
        }

    }

  }

  object Type {

    def translate(rast: WeededAst.Type): ResolvedAst.Type = ???

  }


  def unify(pattern: TypedAst.Pattern, tpe: TypedAst.Type): Validation[Map[String, TypedAst.Type], TypeError] =
    (pattern, tpe) match {
      case (TypedAst.Pattern.Wildcard(_), _) => Map.empty[String, TypedAst.Type].toSuccess
      case (TypedAst.Pattern.Var(ident), t) => Map(ident.name -> t).toSuccess
      case (TypedAst.Pattern.Lit(literal), t) => ??? // TODO: Get type of the literal and then we are good?
        // TODO: remaining cases.
    }

  def expect(tpe1: TypedAst.Type)(tpe2: TypedAst.Type): Validation[TypedAst.Type, TypeError] = ???

  def expectEqual(tpe1: TypedAst.Type, tpe2: TypedAst.Type): Validation[TypedAst.Type, TypeError] = ???

}

package ca.uwaterloo.flix.lang.phase

import ca.uwaterloo.flix.lang.ast.{WeededAst, SourceLocation, ParsedAst}
import ca.uwaterloo.flix.lang.phase.Weeder.WeederError.DuplicateTag

import util.Validation
import util.Validation._

/**
 * The Weeder phase is responsible for:
 *
 * (1) reporting basic syntactic problems, and
 * (2) performing basic syntactic rewritings.
 */
object Weeder {

  /**
   * A common super-type for weeding errors.
   */
  sealed trait WeederError {
    /**
     * Returns human readable error message.
     */
    def format: String
  }

  object WeederError {

    /**
     * An error raised to indicate that the tag `name` was declared multiple times.
     *
     * @param name the name of the tag.
     * @param location1 the location of the first declaration.
     * @param location2 the location of the second declaration.
     */
    case class DuplicateTag(name: String, location1: SourceLocation, location2: SourceLocation) extends WeederError {
      val format =
        s"Error: Duplicate  tag name $name.\n" +
          s"  First declaration was here: ${location1.format}. This one will be used.\n" +
          s"  Second declaration was here: ${location2.format}. This one will be ignored.\n"
    }


    case class DuplicateVariableInPattern()

    case class DuplicatedFormalArgument()

    case class DuplicatedAttributeInRelation()

    case class IllegalApplyInTerm() extends WeederError {
      val format = ???
    }

  }


  // TODO: JoinSemiLattice vs. CompleteLattice.
  // TODO: valid "Traits"
  // TODO: Allow nested lattice types? <<foo>> ?

  // rewrite all functions to lambdas of one argument?

  // TODO: The weeder could be responsible for dealing with Single tuple expressions and literal/expression conversion.

  def weed(ast: ParsedAst.Root): Unit = {
    ast.declarations.map(compile)
  }

  def compile(d: ParsedAst.Declaration): Unit = d match {
    case ParsedAst.Declaration.Namespace(name, body) => body map compile
    case d: ParsedAst.Declaration.Enum =>
      val r = compile(d)
      println(r)
    case _ =>
  }

  /**
   * Compiles the given enum declaration `d`.
   */
  def compile(d: ParsedAst.Declaration.Enum): Validation[WeededAst.Declaration.Enum, WeederError] =
    Validation.fold[ParsedAst.Type.Tag, Map[String, ParsedAst.Type.Tag], WeederError](d.body, Map.empty) {
      // loop through each tag declaration
      case (macc, tag@ParsedAst.Type.Tag(ParsedAst.Ident(name, location2), _)) => macc.get(name) match {
        // check if the tag was already declared
        case None => (macc + (name -> tag)).toSuccess
        case Some(otherTag) => DuplicateTag(name, otherTag.ident.location, location2).toFailure
      }
    } map {
      case m => WeededAst.Declaration.Enum(d.ident, m)
    }

  /**
   * Compiles the given predicate `p`.
   */
  def compile(p: ParsedAst.AmbiguousPredicate) = {
    ???
  }

  /**
   * Compiles the given type `t`.
   */
  def compile(t: ParsedAst.Type): Validation[WeededAst.Type, WeederError] = t match {
    case ParsedAst.Type.Unit => WeededAst.Type.Unit.toSuccess
    case ParsedAst.Type.Ambiguous(name) => WeededAst.Type.Ambiguous(name).toSuccess
    case ParsedAst.Type.Function(t1, t2) =>
      @@(compile(t1), compile(t2)) map {
        case (tpe1, tpe2) => WeededAst.Type.Function(tpe1, tpe2)
      }
    case ParsedAst.Type.Tag(ident, tpe) => compile(tpe) map {
      case t1 => WeededAst.Type.Tag(ident, t1)
    }
    case ParsedAst.Type.Tuple(elms) => flatten(elms map compile) map {
      case ts => WeededAst.Type.Tuple(ts)
    }
    case ParsedAst.Type.Parametric(name, elms) => flatten(elms map compile) map {
      case ts => WeededAst.Type.Parametric(name, ts)
    }
    case ParsedAst.Type.Lattice(tpe) => compile(tpe) map {
      case t1 => WeededAst.Type.Lattice(t1)
    }
  }

}

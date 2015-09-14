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

    // TODO:
    // - Duplicate variable in pattern.
    // - Duplicate variable in function argument. lambda/def
    // - Duplicate tag in enum
    // - duplicate name in relation

    // TODO: JoinSemiLattice vs. CompleteLattice.

    // TODO: valid "Traits"
    // TODO: Allow nested lattice types? <<foo>> ?

    // rewrite all functions to lambdas of one argument?

    // - Disallow Ast.Term.Apply in body of rules.

  }

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

}

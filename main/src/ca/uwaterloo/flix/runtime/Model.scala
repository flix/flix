package ca.uwaterloo.flix.runtime

import ca.uwaterloo.flix.language.ast.{TypedAst, Name}
import ca.uwaterloo.flix.language.ast.TypedAst.Directive
import ca.uwaterloo.flix.util.AsciiTable

/**
 * A case class representing the minimal model.
 *
 * @param root the Flix program.
 * @param constants the constant functions in the model.
 * @param relations the relational facts in the model.
 * @param lattices the lattice facts in the model.
 */
case class Model(root: TypedAst.Root, // TODO: remove
                 constants: Map[Name.Resolved, Value],
                 relations: Map[Name.Resolved, Iterable[List[Value]]],
                 lattices: Map[Name.Resolved, Iterable[(List[Value], List[Value])]]) {

  /**
   * Evaluates all print directives in the program.
   */
  // TODO: Move somewhere else.
  def print(): Unit = {
    for (directive <- root.directives.prints) {
      print(directive)
    }
  }

  /**
   * Evaluates the given print `directive`.
   */
  // TODO: Move somewhere else.
  private def print(directive: Directive.Print): Unit = {
    val collection = root.collections(directive.name)

    collection match {
      case r: TypedAst.Collection.Relation =>
        val table = relations(directive.name)
        val cols = r.attributes.map(_.ident.name)
        val ascii = new AsciiTable().withCols(cols: _*)
        for (row <- table.toSeq.sortBy(_.head.toString)) {
          ascii.mkRow(row.toList map Value.pretty)
        }

        Console.println(r.name)
        ascii.write(System.out)
        Console.println()
        Console.println()

      case l: TypedAst.Collection.Lattice =>
        val table = lattices(directive.name)
        val cols = l.keys.map(_.ident.name) ::: l.values.map(_.ident.name + "<>")
        val ascii = new AsciiTable().withCols(cols: _*)
        for ((keys, elms) <- table.toSeq.sortBy(_._1.head.toString)) {
          ascii.mkRow((keys map Value.pretty) ++ (elms map Value.pretty))
        }

        Console.println(l.name)
        ascii.write(System.out)
        Console.println()
        Console.println()
    }

  }

}
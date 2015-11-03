package ca.uwaterloo.flix.runtime

import ca.uwaterloo.flix.language.ast.{TypedAst, Name}
import ca.uwaterloo.flix.language.ast.TypedAst.Directive
import ca.uwaterloo.flix.util.AsciiTable

/**
 * A case class representing the minimal model.
 *
 * @param root the Flix program.
 * @param relations the relational facts in the model.
 * @param lattices the lattice facts in the model.
 */
case class Model(root: TypedAst.Root, relations: Map[Name.Resolved, List[List[Value]]], lattices: Map[Name.Resolved, Map[List[Value], List[Value]]]) {

  /**
   * Evaluates all print directives in the program.
   */
  def print(): Unit = {
    for (directive <- root.directives.prints) {
      print(directive)
    }
  }

  /**
   * Evaluates the given print `directive`.
   */
  private def print(directive: Directive.Print): Unit = {
    val collection = root.collections(directive.name)

    collection match {
      case r: TypedAst.Collection.Relation =>
        val table = relations(directive.name)
        val cols = r.attributes.map(_.ident.name)
        val ascii = new AsciiTable().withCols(cols: _*)
        for (row <- table.toSeq.sortBy(_.head.toString)) {
          ascii.mkRow(row.toList map (_.pretty))
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
          ascii.mkRow((keys map (_.pretty)) ++ (elms map (_.pretty)))
        }

        Console.println(l.name)
        ascii.write(System.out)
        Console.println()
        Console.println()
    }

  }

}
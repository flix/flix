package ca.uwaterloo.flix

import java.nio.file.{Path, InvalidPathException, Files, Paths}

import ca.uwaterloo.flix.language.Compiler
import ca.uwaterloo.flix.language.ast.TypedAst
import ca.uwaterloo.flix.language.ast.TypedAst.Directive
import ca.uwaterloo.flix.runtime.{Model, Solver}
import ca.uwaterloo.flix.util.{AsciiTable, Validation}
import ca.uwaterloo.flix.util.Validation._

object Flix {

  trait FlixError {

  }

  def solve(s: String): Unit = {
    if (isValidPath(s))
      Compiler.compile(Paths.get(s))
    else
      Compiler.compile(s)
  }

  def solve(paths: Traversable[Path]): Validation[Model, FlixError] = {
    val ast = Compiler.compile(paths)

    if (ast.isEmpty) {
      System.exit(1) // TODO
    }

    implicit val sCtx = Solver.SolverContext(ast.get)

    val solver = new Solver()
    val model = solver.solve()

    for (directive <- sCtx.root.directives.prints) {
      print(model, directive)
    }

    model.toSuccess
  }

  private def isValidPath(s: String): Boolean = try {
    val path = Paths.get(s)
    Files.exists(path) && Files.isRegularFile(path)
  } catch {
    case e: InvalidPathException => false
  }


  /**
   * Evaluates the given print `directive`.
   */
  def print(model: Model, directive: Directive.Print)(implicit sCtx: Solver.SolverContext): Unit = {
    val collection = sCtx.root.collections(directive.name)

    collection match {
      case r: TypedAst.Collection.Relation =>
        val table = model.relations(directive.name)
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
        val table = model.lattices(directive.name)
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

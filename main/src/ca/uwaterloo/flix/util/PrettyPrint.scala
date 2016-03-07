package ca.uwaterloo.flix.util

import ca.uwaterloo.flix.language.ast.{TypedAst, Symbol}
import ca.uwaterloo.flix.runtime.{Value, Model}


object PrettyPrint {

  def print(name: String, model: Model): Unit = {
    val rname = Symbol.Resolved.mk(name)

    var found = false

    model.constants.get(rname) match {
      case None => // nop
      case Some(v) =>
        found = true
        Value.pretty(v)
    }

    model.relations.get(rname) match {
      case None => // nop
      case Some(xs) =>
        val r = model.root.collections(rname).asInstanceOf[TypedAst.Table.Relation]
        val cols = r.attributes.map(_.ident.name)
        val ascii = new AsciiTable().withCols(cols: _*)
        for (row <- xs.toSeq.sortBy(_.head.toString)) {
          ascii.mkRow(row.toList map Value.pretty)
        }

        Console.println(r.name)
        ascii.write(System.out)
        Console.println()
        Console.println()
        found = true
    }

    model.lattices.get(rname) match {
      case None => // nop
      case Some(xs) =>
        val l = model.root.collections(rname).asInstanceOf[TypedAst.Table.Lattice]
        val cols = l.keys.map(_.ident.name) ::: l.values.map(_.ident.name + "<>")
        val ascii = new AsciiTable().withCols(cols: _*)
        for ((keys, elms) <- xs.toSeq.sortBy(_._1.head.toString)) {
          ascii.mkRow((keys map Value.pretty) ++ (elms map Value.pretty))
        }

        Console.println(l.name)
        ascii.write(System.out)
        Console.println()
        Console.println()
        found = true
    }

    if (!found)
      Console.println("No such name: " + name)
  }

}

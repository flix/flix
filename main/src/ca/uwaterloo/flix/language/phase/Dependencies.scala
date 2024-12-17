package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.api.lsp.{Consumer, Visitor}
import ca.uwaterloo.flix.api.lsp.acceptors.AllAcceptor
import ca.uwaterloo.flix.language.ast.SourceLocation
import ca.uwaterloo.flix.language.ast.Symbol.DefnSym
import ca.uwaterloo.flix.language.ast.TypedAst.Root
import ca.uwaterloo.flix.language.ast.shared.{DependencyGraph, Input, SymUse}
import ca.uwaterloo.flix.util.collection.MultiMap

import scala.collection.mutable

object Dependencies {

  /** Checks the safety and well-formedness of `root`. */
  def run(root: Root)(implicit flix: Flix): Root = {

    object consumer extends Consumer {
      var defs: MultiMap[DefnSym, SourceLocation] = MultiMap.empty[DefnSym, SourceLocation]

      override def consumeDefSymUse(symUse: SymUse.DefSymUse): Unit = {
        defs = defs + (symUse.sym -> symUse.loc)
      }
    }

    Visitor.visitRoot(root, consumer, AllAcceptor)


    val dg = DependencyGraph(
      consumer.defs
    )

    println(dg)

    root
  }

}

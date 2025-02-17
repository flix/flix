/*
 * Copyright 2024 Matthew Lutze
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package ca.uwaterloo.flix.language.phase.typer

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.{SourceLocation, Type}
import ca.uwaterloo.flix.language.phase.unification.Substitution

import java.nio.file.{Files, Path}
import java.util.concurrent.{Executors, TimeUnit, TimeoutException}

/**
  * Debugging utilities for typing constraints.
  * Not thread-safe.
  */
object Debug {

  /**
    * The directory in which to store the constraint graphs.
    */
  private var graphDir: Path = _

  /**
    * Indicates whether we are currently recording constraint resolution.
    */
  private var record: Boolean = false

  /**
    * The number of the next graph to record.
    */
  private var index = 0


  /**
    * Activates recording of constraint resolution.
    */
  def startRecording()(implicit flix: Flix): Unit = {
    graphDir = flix.options.output.getOrElse(Path.of("./build/")).resolve("constraint-graphs")
    Files.createDirectories(graphDir)
    record = true
  }

  /**
    * Deactivates recording of constraint resolution.
    */
  def stopRecording(): Unit = record = false

  /**
    * Records the given typing constraints and substitution as a dot graph.
    */
  def recordGraph(tconstrs: List[TypeConstraint], subst: SubstitutionTree, label: String): Unit = {
    if (record) {
      val dot = toDotWithSubst(tconstrs, subst, label)
      val fileName = s"${index.toString.reverse.padTo(4, '0').reverse}.dot"
      val path = graphDir.resolve(fileName)
      Files.writeString(path, dot)
      index += 1
    }
  }

  /**
    * Generates a GraphViz (dot) string representing the given constraints.
    */
  def toDot(constrs: List[TypeConstraint]): String = {
    val contents = constrs.map(toSubDot)
    val edges = constrs.map { constr => s"root -> ${constraintId(constr)};" }
    format((
      "digraph Constraints {" ::
        "rankdir = \"LR\";" ::
        "node [shape=\"box\"];" ::
        contents :::
        edges :::
        List("}")
      ).mkString("\n"))
  }

  /**
    * Generates a GraphViz (dot) string representing the given constraints and substitution.
    */
  def toDotWithSubst(constrs: List[TypeConstraint], tree: SubstitutionTree, label: String): String = {
    val contents = constrs.map(toSubDot)
    val edges = constrs.map { constr => s"root -> ${constraintId(constr)};" }
    val substSubgraph = toTreeSubDot(tree)

    val constrsSubgraph =
      "subgraph Constraints {" ::
        "root" ::
        contents :::
        edges :::
        List("}")

    val labelLine = s"label=\"$label\"";

    format((
      "digraph Constraints {" ::
        labelLine ::
        "labelloc=t;" ::
        "compound=true;" ::
        "rankdir = \"LR\";" ::
        "node [shape=\"box\"];" ::
        substSubgraph :::
        constrsSubgraph :::
        List("}")
      ).mkString("\n"))
  }

  /**
    * Executes the function `f`, timing out after `limitMs` milliseconds.
    */
  def runWithTimeout[A](limitMs: Int)(f: () => A): Option[A] = {
    val executor = Executors.newSingleThreadExecutor();
    val future = executor.submit(() => f())
    try {
      Some(future.get(limitMs, TimeUnit.MILLISECONDS))
    } catch {
      case _: TimeoutException => None
    } finally {
      executor.shutdown()
    }
  }

  /**
    * Generates a GraphViz (dot) string representing a fragment of the constraint graph.
    */
  private def toSubDot(constr: TypeConstraint): String = constr match {
    case TypeConstraint.Equality(tpe1, tpe2, _) => s"""${constraintId(constr)} [label = "$tpe1 ~ $tpe2"];"""
    case TypeConstraint.Trait(sym, tpe, _) => s"""${constraintId(constr)} [label = "$sym[$tpe]"];"""
    case TypeConstraint.Purification(sym, eff1, eff2, _, nested) =>
      val header = s"""${constraintId(constr)} [label = "$eff1 ~ ($eff2)[$sym ↦ Pure]"];"""
      val children = nested.map(toSubDot)
      val edges = nested.map { child => s"${constraintId(constr)} -> ${constraintId(child)};" }
      (header :: children ::: edges).mkString("\n")
  }

  /**
    * Generates a GraphViz (dot) string representing a fragment of the substitution tree graph.
    */
  private def toTreeSubDot(tree: SubstitutionTree): List[String] = tree match {
    case SubstitutionTree(subst, branches) =>

      val header = List(
        s"subgraph cluster_${treeId{tree}} {",
        "label=\"\";",
        s"handle_${treeId{tree}} [shape=point, style=invis]" // create a dummy node for connecting clusters
      )

      val contents = subst.m.toList.sortBy(_._1).flatMap {
        case (sym, tpe) =>
          val tvar = Type.Var(sym, SourceLocation.Unknown)
          val from = System.nanoTime()
          val to = System.nanoTime()
          List(
            s"""$from [label = "$tvar"];""",
            s"""$to [label = "$tpe"];""",
            s"$from -> $to;"
          )
      }
      val footer = List("}")

      val children = branches.toList.sortBy(_._1).flatMap {
        case (sym, child) =>
          // visit the child
          val childLines = toTreeSubDot(child)

          // connect the child to the parent
          val connection = List(
            s"handle_${treeId(tree)} -> handle_${treeId(child)} [ltail=cluster_${treeId(tree)}, lhead=cluster_${treeId(child)}, label=\"$sym\"]"
          )

          childLines ::: connection
      }

      header ::: contents ::: footer ::: children
  }

  /**
    * Returns a probably-unique ID for the constraint.
    */
  private def constraintId(constr: TypeConstraint): Int = System.identityHashCode(constr)

  /**
    * Returns a probably-unique ID for the substitution tree.
    */
  private def treeId(tree: SubstitutionTree): Int = System.identityHashCode(tree)


  /**
    * Escapes slashes in the string for use with GraphViz.
    */
  private def format(s: String): String = s.replace("\\", "\\\\")
}

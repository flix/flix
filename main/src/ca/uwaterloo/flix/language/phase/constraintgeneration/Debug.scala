package ca.uwaterloo.flix.language.phase.constraintgeneration

import ca.uwaterloo.flix.language.ast.{SourceLocation, Type}
import ca.uwaterloo.flix.language.phase.unification.Substitution

/**
  * Debugging utilities for typing constraints.
  */
class Debug {

  /**
    * Generates a GraphViz (dot) string representing the given constraints.
    */
  def toDot(constrs: List[TypingConstraint]): String = {
    val contents = constrs.map(toSubDot)
    val edges = constrs.map { constr => s"root -> ${dotId(constr)};" }
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
  def toDotWithSubst(constrs: List[TypingConstraint], subst: Substitution): String = {
    val contents = constrs.map(toSubDot)
    val edges = constrs.map { constr => s"root -> ${dotId(constr)};" }
    val substPart = subst.m.toList.sortBy(_._1).flatMap {
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
    val substSubgraph =
      "subgraph cluster_Subst {" ::
        "label = \"Substitution\";" ::
        "style = \"filled\";" ::
        "invisL [style=\"invis\"];" :: // these invisible boxes help align the substitution
        "invisR [style=\"invis\"];" ::
        "invisL -> invisR [style=\"invis\"];" ::
        substPart :::
        List("}")

    val constrsSubgraph =
      "subgraph Constraints {" ::
        "root" ::
        contents :::
        edges :::
        List("}")

    format((
      "digraph Constraints {" ::
        "rankdir = \"LR\";" ::
        "node [shape=\"box\"];" ::
        substSubgraph :::
        constrsSubgraph :::
        "invisR -> root [style=\"invis\"];" ::
        List("}")
      ).mkString("\n"))
  }

  /**
    * Generates a GraphViz (dot) string representing a fragment of the constraint graph.
    */
  private def toSubDot(constr: TypingConstraint): String = constr match {
    case TypingConstraint.Equality(tpe1, tpe2, _) => s"""${dotId(constr)} [label = "$tpe1 ~ $tpe2"];"""
    case TypingConstraint.Class(sym, tpe, _) => s"""${dotId(constr)} [label = "$sym[$tpe]"];"""
    case TypingConstraint.Purification(sym, eff1, eff2, _, _, nested) =>
      val header = s"""${dotId(constr)} [label = "$eff1 ~ ($eff2)[$sym ↦ Pure]"];"""
      val children = nested.map(toSubDot)
      val edges = nested.map { child => s"${dotId(constr)} -> ${dotId(child)};" }
      (header :: children ::: edges).mkString("\n")
  }

  /**
    * Returns a probably-unique ID for the constraint.
    */
  private def dotId(constr: TypingConstraint): Int = System.identityHashCode(constr: TypingConstraint)


  /**
    * Escapes slashes in the string for use with GraphViz.
    */
  private def format(s: String): String = s.replace("\\", "\\\\")
}

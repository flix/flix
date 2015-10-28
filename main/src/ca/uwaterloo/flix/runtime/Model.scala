package ca.uwaterloo.flix.runtime

import ca.uwaterloo.flix.language.ast.Name

// TODO: Solver should return this.
case class Model(relations: Map[Name.Resolved, List[List[Value]]], lattices: Map[Name.Resolved, Map[List[Value], List[Value]]])
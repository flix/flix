package ca.uwaterloo.flix.language.ast.shared

import ca.uwaterloo.flix.language.ast.Symbol

object DatalogDefs {
  val version: String = "3"
  lazy val box: Symbol.DefnSym = Symbol.mkDefnSym(s"Fixpoint${DatalogDefs.version}.Ast.Shared.box")
  lazy val lattice: Symbol.DefnSym =  Symbol.mkDefnSym(s"Fixpoint${DatalogDefs.version}.Ast.Shared.lattice")

  lazy val Box: Symbol.DefnSym = Symbol.mkDefnSym(s"Fixpoint${version}.Boxable.box")
  lazy val Unbox: Symbol.DefnSym = Symbol.mkDefnSym(s"Fixpoint${version}.Boxable.unbox")
  lazy val Solve: Symbol.DefnSym = Symbol.mkDefnSym(s"Fixpoint${version}.Solver.runSolver")
  lazy val SolveWithProvenance: Symbol.DefnSym = Symbol.mkDefnSym(s"Fixpoint${version}.Solver.runSolverWithProvenance")
  lazy val Merge: Symbol.DefnSym = Symbol.mkDefnSym(s"Fixpoint${version}.Solver.union")
  lazy val Filter: Symbol.DefnSym = Symbol.mkDefnSym(s"Fixpoint${version}.Solver.projectSym")
  lazy val Rename: Symbol.DefnSym = Symbol.mkDefnSym(s"Fixpoint${version}.Solver.rename")
  lazy val ProvenanceOf: Symbol.DefnSym = Symbol.mkDefnSym(s"Fixpoint3.Solver.provenanceOf")

  def ProjectInto(arity: Int): Symbol.DefnSym = Symbol.mkDefnSym(s"Fixpoint${version}.Solver.injectInto$arity")

  def Facts(arity: Int): Symbol.DefnSym = Symbol.mkDefnSym(s"Fixpoint${version}.Solver.facts$arity")

  def liftX(arity: Int): Symbol.DefnSym = {
    Symbol.mkDefnSym(s"Fixpoint${DatalogDefs.version}.Boxable.lift${arity}")
  }

  def liftXb(arity: Int): Symbol.DefnSym = {
    Symbol.mkDefnSym(s"Fixpoint${DatalogDefs.version}.Boxable.lift${arity}b")
  }

  def liftXY(arity1: Int, arity2: Int): Symbol.DefnSym = {
    Symbol.mkDefnSym(s"Fixpoint${DatalogDefs.version}.Boxable.lift${arity1}X$arity2")
  }


  lazy val allDatalogDefs: List[Symbol.DefnSym] = {
    val simpleDefs =
      List(
        DatalogDefs.box,
        DatalogDefs.lattice,
        DatalogDefs.Box,
        DatalogDefs.Unbox,
        DatalogDefs.Solve,
        DatalogDefs.SolveWithProvenance,
        DatalogDefs.Merge,
        DatalogDefs.Filter,
        DatalogDefs.Rename,
        DatalogDefs.ProvenanceOf
      )
    val maxSize = 15
    val factDefs = List.range(0, maxSize + 1).map(DatalogDefs.Facts)
    val projectDefs = List.range(1, maxSize + 1).map(DatalogDefs.ProjectInto)
    val maxLiftSize = 5
    val simpleLifts = List.range(1, maxLiftSize + 1).map(DatalogDefs.liftX)
    val boolLifts = List.range(1, maxLiftSize + 1).map(DatalogDefs.liftXb)
    val functionLifts = List.range(0, maxLiftSize + 1).flatMap(x => List.range(1, maxLiftSize + 1).map(y => liftXY(x, y)))
    simpleDefs ++ factDefs ++ projectDefs ++ simpleLifts ++ boolLifts ++ functionLifts
  }

}

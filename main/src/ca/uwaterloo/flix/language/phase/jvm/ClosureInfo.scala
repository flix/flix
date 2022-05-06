package ca.uwaterloo.flix.language.phase.jvm

import ca.uwaterloo.flix.language.ast.{MonoType, Symbol}

/**
  * Meta information about a closure.
  */
case class ClosureInfo(sym: Symbol.DefnSym, closureArgTypes: List[MonoType], tpe: MonoType)

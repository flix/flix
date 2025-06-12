package ca.uwaterloo.flix.language.ast

import ca.uwaterloo.flix.language.ast.shared.Source
import ca.uwaterloo.flix.language.phase.jvm.{JvmClass, JvmName}


object BytecodeAst {

  case class Root(
                   classes: Map[JvmName, JvmClass],
                   tests: Map[Symbol.DefnSym, Test],
                   main: Option[Def],
                   sources: Map[Source, SourceLocation]
                 )

  case class Def(className: JvmName, methodName: String)

  case class Test(className: JvmName, methodName: String, isSkip: Boolean)

}

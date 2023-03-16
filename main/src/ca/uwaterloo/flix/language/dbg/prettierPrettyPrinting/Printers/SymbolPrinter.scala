package ca.uwaterloo.flix.language.dbg.prettierPrettyPrinting.Printers

import ca.uwaterloo.flix.language.ast.Symbol
import ca.uwaterloo.flix.language.dbg.prettierPrettyPrinting.DocAst

object SymbolPrinter {

  def print(sym: Symbol.VarSym): DocAst =
    DocAst.Var(sym)

  def printWithOffset(sym: Symbol.VarSym): DocAst =
    DocAst.VarWithOffset(sym)
}

package ca.uwaterloo.flix.language.dbg

import ca.uwaterloo.flix.language.ast.{Ast, TypedAst}

/**
  * Formatting Patterns.
  */
object FormatPattern {

  /**
    * TypedAst.
    */
  def format(p0: TypedAst.Pattern): String = p0 match {
    case TypedAst.Pattern.Wild(_, _) => "_"
    case TypedAst.Pattern.Var(sym, _, _) => sym.text
    case TypedAst.Pattern.Cst(cst, _, _) => FormatConstant.format(cst)
    case TypedAst.Pattern.Tag(Ast.CaseSymUse(sym, _), pat, _, _) => pat match {
      case TypedAst.Pattern.Cst(Ast.Constant.Unit, _, _) => sym.name
      case _ => sym.name + "(" + format(pat) + ")"
    }
    case TypedAst.Pattern.Tuple(elms, _, _) => "(" + elms.map(format).mkString(", ") + ")"
    case TypedAst.Pattern.Array(elms, _, _) => "[" + elms.map(format).mkString(", ") + "]"
    case TypedAst.Pattern.ArrayTailSpread(elms, sym, _, _) =>
      "[" + elms.map(format).mkString(", ") + ", .." + sym.text + "]"

    case TypedAst.Pattern.ArrayHeadSpread(sym, elms, _, _) =>
      "[" + sym.text + ".., " + elms.map(format).mkString(", ") + "]"

  }

}

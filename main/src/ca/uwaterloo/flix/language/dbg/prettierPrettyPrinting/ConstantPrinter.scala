package ca.uwaterloo.flix.language.dbg.prettierPrettyPrinting

import ca.uwaterloo.flix.language.ast.Ast
import ca.uwaterloo.flix.language.ast.Ast.Constant
import ca.uwaterloo.flix.language.dbg.prettierPrettyPrinting.Doc.{Indent, text}
import ca.uwaterloo.flix.language.dbg.prettierPrettyPrinting.DocUtil.Language.stringf

object ConstantPrinter {

  def doc(cst: Ast.Constant): Doc = cst match {
    case Constant.Unit => text("()")
    case Constant.Null => text("null")
    case Constant.Bool(lit) => text(lit.toString)
    case Constant.Char(lit) => text("'") :: text(lit.toString) :: text("'")
    case Constant.Float32(lit) => text(lit.toString) :: text("f32")
    case Constant.Float64(lit) => text(lit.toString) :: text("f64")
    case Constant.BigDecimal(lit) => text(lit.toString) :: text("ff")
    case Constant.Int8(lit) => text(lit.toString) :: text("i8")
    case Constant.Int16(lit) => text(lit.toString) :: text("i16")
    case Constant.Int32(lit) => text(lit.toString) :: text("i32")
    case Constant.Int64(lit) => text(lit.toString) :: text("i64")
    case Constant.BigInt(lit) => text(lit.toString) :: text("ii")
    case Constant.Str(lit) => stringf(lit)
  }

}

package ca.uwaterloo.flix.language.dbg.prettierPrettyPrinting.Printers

import ca.uwaterloo.flix.language.ast.Ast.Constant
import ca.uwaterloo.flix.language.dbg.prettierPrettyPrinting.DocAst

object ConstantPrinter {

  def print(cst: Constant): DocAst = cst match {
    case Constant.Unit => DocAst.Unit
    case Constant.Null => DocAst.AsIs("null")
    case Constant.Bool(lit) => DocAst.AsIs(lit.toString)
    case Constant.Char(lit) => DocAst.AsIs(lit.toString)
    case Constant.Float32(lit) => DocAst.AsIs(s"${lit}f32")
    case Constant.Float64(lit) => DocAst.AsIs(s"${lit}f64")
    case Constant.BigDecimal(lit) => DocAst.AsIs(s"${lit}ff")
    case Constant.Int8(lit) => DocAst.AsIs(s"${lit}i8")
    case Constant.Int16(lit) => DocAst.AsIs(s"${lit}i16")
    case Constant.Int32(lit) => DocAst.AsIs(s"${lit}i32")
    case Constant.Int64(lit) => DocAst.AsIs(s"${lit}i64")
    case Constant.BigInt(lit) => DocAst.AsIs(s"${lit}ii")
    case Constant.Str(lit) => DocAst.AsIs("\"\"\"" + lit + "\"\"\"")
  }

}

package ca.uwaterloo.flix.language.debug

import ca.uwaterloo.flix.language.ast.TypedAst

/**
  * Implementations of `toString` for patterns of all the different ASTs.
  */
object FormatPattern {

  def format(p0: TypedAst.Pattern): String = p0 match {
    case TypedAst.Pattern.Wild(_, _) => "_"
    case TypedAst.Pattern.Var(sym, _, _) => sym.toString
    case TypedAst.Pattern.Unit(_) => "()"
    case TypedAst.Pattern.True(_) => "true"
    case TypedAst.Pattern.False(_) => "false"
    case TypedAst.Pattern.Char(lit, _) => s"'$lit'"
    case TypedAst.Pattern.Float32(lit, _) => s"${lit}f32"
    case TypedAst.Pattern.Float64(lit, _) => s"${lit}f64"
    case TypedAst.Pattern.Int8(lit, _) => s"${lit}i8"
    case TypedAst.Pattern.Int16(lit, _) => s"${lit}i16"
    case TypedAst.Pattern.Int32(lit, _) => s"${lit}i32"
    case TypedAst.Pattern.Int64(lit, _) => s"${lit}i64"
    case TypedAst.Pattern.BigInt(lit, _) => s"${lit}ii"
    case TypedAst.Pattern.Str(lit, _) => "\"" + lit + "\""
    case TypedAst.Pattern.Tag(_, tag, pat, _, _) => pat match {
      case TypedAst.Pattern.Unit(_) => tag
      case _ => tag + "(" + format(pat) + ")"
    }
    case TypedAst.Pattern.Tuple(elms, _, _) => "(" + elms.map(format).mkString(", ") + ")"
  }

}

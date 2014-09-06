package syntax

import impl.logic.Type

/**
 * Embedded DSL syntax for types.
 */
object Types {

  /**
   * Rich Type.
   */
  implicit class RichType(typ: Type) {
    def fmt: String = typ match {
      case Type.Var(x) => x.s

      case Type.Unit => "Unit"
      case Type.Bool => "Bool"
      case Type.Int => "Int"
      case Type.Str => "Str"

      case Type.Set(typ1) => s"Set {${typ1.fmt}}"
      case Type.Sum(ts) => "[" + ts.map(_.fmt).mkString(" + ") + "]"
      case Type.Function(typ1, typ2) => s"${typ1.fmt} -> (${typ2.fmt})"

      case Type.Tag(s, Type.Unit) => s"${s.s}"
      case Type.Tag(s, typ1) => s"${s.s} ${typ1.fmt}"
      case Type.Tuple2(typ1, typ2) => s"(${typ1.fmt}, ${typ2.fmt})"
      case Type.Tuple3(typ1, typ2, typ3) => s"(${typ1.fmt}, ${typ2.fmt}, ${typ3.fmt})"
      case Type.Tuple4(typ1, typ2, typ3, typ4) => s"(${typ1.fmt}, ${typ2.fmt}, ${typ3.fmt}, ${typ4.fmt})"
      case Type.Tuple5(typ1, typ2, typ3, typ4, typ5) => s"(${typ1.fmt}, ${typ2.fmt}, ${typ3.fmt}, ${typ4.fmt}, ${typ5.fmt})"
    }
  }

}

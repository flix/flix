package ca.uwaterloo.flix.language.dbg.prettierPrettyPrinting.Printers

import ca.uwaterloo.flix.language.ast.MonoType
import ca.uwaterloo.flix.language.dbg.prettierPrettyPrinting.DocAst.Type

object MonoTypePrinter {

  /**
    * Returns the [[Type]] representation of `tpe`.
    */
  def print(tpe: MonoType): Type = tpe match {
    case MonoType.Unit => Type.Unit
    case MonoType.Bool => Type.Bool
    case MonoType.Char => Type.Char
    case MonoType.Float32 => Type.Float32
    case MonoType.Float64 => Type.Float64
    case MonoType.BigDecimal => Type.BigDecimal
    case MonoType.Int8 => Type.Int8
    case MonoType.Int16 => Type.Int16
    case MonoType.Int32 => Type.Int32
    case MonoType.Int64 => Type.Int64
    case MonoType.BigInt => Type.BigInt
    case MonoType.Str => Type.Str
    case MonoType.Region => Type.Region
    case MonoType.Array(tpe) => Type.Array(print(tpe))
    case MonoType.Lazy(tpe) => Type.Lazy(print(tpe))
    case MonoType.Ref(tpe) => Type.Ref(print(tpe))
    case MonoType.Tuple(elms) => Type.Tuple(elms.map(print))
    case MonoType.Enum(sym, args) => Type.Enum(sym, args.map(print))
    case MonoType.Arrow(args, result) => Type.Arrow(args.map(print), print(result))
    case MonoType.RecordEmpty() => Type.RecordEmpty
    case MonoType.RecordExtend(field, value, rest) => Type.RecordExtend(field, print(value), print(rest))
    case MonoType.SchemaEmpty() => Type.SchemaEmpty
    case MonoType.SchemaExtend(name, tpe, rest) => Type.SchemaExtend(name, print(tpe), print(rest))
    case MonoType.Native(clazz) => Type.Native(clazz)
    case MonoType.Var(id) => Type.Var(id)
  }

}

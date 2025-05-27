package ca.uwaterloo.flix.language.phase.jvm2

import ca.uwaterloo.flix.language.ast.ReducedAst.{AnonClass, Def, Effect, Enum, Struct}
import ca.uwaterloo.flix.language.ast.shared.Source
import ca.uwaterloo.flix.language.ast.{JvmAst, MonoType, ReducedAst, SourceLocation, Symbol}

import java.lang.constant.{ClassDesc, ConstantDescs}

object ToJvmAst {

  /**
    *
    */
  def run(root: ReducedAst.Root): JvmAst.Root = {

    // visit defs...
    // essence
    JvmAst.Root(root.defs, // need to convert values of the map see 'Eraser.scala'
      root.enums,
      root.structs,
      root.effects,
      root.types,
      root.anonClasses,
      root.mainEntryPoint,
      root.entryPoints,
      root.sources)
  }

  def toClassDesc(tpe: MonoType): ClassDesc = tpe match {
    case MonoType.Void => ConstantDescs.CD_void
    case MonoType.AnyType => ConstantDescs.CD_Object
    case MonoType.Unit =>
    case MonoType.Bool =>
    case MonoType.Char =>
    case MonoType.Float32 =>
    case MonoType.Float64 => ConstantDescs.CD_double // primitive type
    case MonoType.BigDecimal =>
    case MonoType.Int8 =>
    case MonoType.Int16 =>
    case MonoType.Int32 =>
    case MonoType.Int64 =>
    case MonoType.BigInt =>
    case MonoType.String =>
    case MonoType.Regex =>
    case MonoType.Region =>
    case MonoType.Null =>
    case MonoType.Array(tpe) =>
    // layout needed
    case MonoType.Lazy(tpe) =>
    case MonoType.Tuple(tpes) =>
    case MonoType.Enum(sym, targs) =>
    case MonoType.Struct(sym, targs) =>
    case MonoType.Arrow(args, result) =>
    case MonoType.RecordEmpty =>
    case MonoType.RecordExtend(label, value, rest) =>
    case MonoType.ExtensibleEmpty =>
    case MonoType.ExtensibleExtend(cons, tpes, rest) =>
    case MonoType.Native(clazz) =>
  }
}

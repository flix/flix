package ca.uwaterloo.flix.language.debug

object FormatSimpleType {
  def format(tpe: SimpleType): String = tpe match {
    case SimpleType.Unit => "Unit"
    case SimpleType.Null =>"Null"
    case SimpleType.Bool => "Bool"
    case SimpleType.Char => "Char"
    case SimpleType.Float32 => "Float32"
    case SimpleType.Float64 => "Float64"
    case SimpleType.Int8 => "Int8"
    case SimpleType.Int16 => "Int16"
    case SimpleType.Int32 => "Int32"
    case SimpleType.Int64 => "Int64"
    case SimpleType.BigInt => "BigInt"
    case SimpleType.Str => "Str"
    case SimpleType.Array => "Array"
    case SimpleType.ScopedRef => "ScopedRef"
    case SimpleType.Channel => "Channel"
    case SimpleType.Lazy => "Lazy"
    case SimpleType.True => "True"
    case SimpleType.False => "False"
    case SimpleType.Region => "Region"
    case SimpleType.Record(fields, rest) => ??? // MATT probably easier if we separate into extended an unextended records
    case SimpleType.RecordRow(fields, rest) => ??? // MATT see above
    case SimpleType.RecordRowEmpty => "()"
    case SimpleType.RecordEmpty => "{}"
    case SimpleType.RecordConstructor => "{ ? }"
    case SimpleType.RecordRowConstructor(field) => s"( $field :: ? | ? )"
    case SimpleType.RecordRowHead(name, tpe) => s"( $name :: ${format(tpe)} | ? )"
    case SimpleType.Schema(fields, rest) => ??? // MATT see above
    case SimpleType.SchemaRow(fields, rest) => ??? // MATT see above
    case SimpleType.SchemaRowEmpty => "#()"
    case SimpleType.SchemaEmpty => "#{}"
    case SimpleType.SchemaConstructor => "#{ ? }"
    case SimpleType.SchemaRowConstructor(field) => s"#( $field :: ? | ? )"
    case SimpleType.SchemaRowHead(name, tpe) => s"#( $name :: ${format(tpe)} | ? )"
    case SimpleType.Not(tpe) => s"not ${tpe.map(format).getOrElse("?")}" // MATT handle parens
    case SimpleType.And(tpes) => ???
    case SimpleType.Or(tpes) => ???
    case SimpleType.RelationConstructor => ???
    case SimpleType.Relation(tpes) => ???
    case SimpleType.LatticeConstructor => ???
    case SimpleType.Lattice(tpes) => ???
    case SimpleType.ArrowConstructor(arity) => ???
    case SimpleType.PartialPureArrow(arity, tpes) => ???
    case SimpleType.PartialImpureArrow(arity, tpes) => ???
    case SimpleType.PartialPolyArrow(arity, tpes, eff) => ???
    case SimpleType.PureArrow(args, ret) => ???
    case SimpleType.ImpureArrow(args, ret) => ???
    case SimpleType.PolyArrow(args, ret, eff) => ???
    case SimpleType.TagConstructor(name) => ???
    case SimpleType.PartialTag(name, args) => ???
    case SimpleType.Tag(name, args, ret) => ???
    case SimpleType.Name(name) => ???
    case SimpleType.Apply(tpe, tpes) => ???
    case SimpleType.Var(id, text) => ???
    case SimpleType.Tuple(length, fields) => ???
  }

}

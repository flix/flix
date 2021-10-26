package ca.uwaterloo.flix.language.debug

object FormatSimpleType {
  def format(tpe: SimpleType, nameContext: Map[Int, String]): String = tpe match {
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
    case SimpleType.Record(fields, rest) =>
      val fieldString = fields.map {
        case SimpleType.FieldType(name, tpe) => s"$name :: ${format(tpe)}"
      }.mkString(", ")
      val restString = rest.map(r => s" | ${format(r)}").getOrElse("")
      s"{$fieldString$restString}"
    case SimpleType.RecordRow(fields, rest) =>
      val fieldString = fields.map {
        case SimpleType.FieldType(name, tpe) => s"$name :: ${format(tpe)}"
      }.mkString(", ")
      val restString = rest.map(r => s" | ${format(r)}").getOrElse("")
      s"($fieldString$restString)"
    case SimpleType.RecordRowEmpty => "()"
    case SimpleType.RecordEmpty => "{}"
    case SimpleType.RecordConstructor => "{ ? }"
    case SimpleType.RecordRowConstructor(field) => s"( $field :: ? | ? )"
    case SimpleType.RecordRowHead(name, tpe) => s"( $name :: ${format(tpe)} | ? )"
    case SimpleType.Schema(fields, rest) =>
    case SimpleType.SchemaRow(fields, rest) => ??? // MATT see above
    case SimpleType.SchemaRowEmpty => "#()"
    case SimpleType.SchemaEmpty => "#{}"
    case SimpleType.SchemaConstructor => "#{ ? }"
    case SimpleType.SchemaRowConstructor(field) => s"#( $field :: ? | ? )"
    case SimpleType.SchemaRowHead(name, tpe) => s"#( $name :: ${format(tpe)} | ? )"
    case SimpleType.Not(tpe) => s"not ${tpe.map(format).getOrElse("?")}" // MATT handle parens
    case SimpleType.And(tpes) =>
      val strings = tpes.map(format).padTo(2, "?")
      strings.mkString(" and ")
    case SimpleType.Or(tpes) =>
      val strings = tpes.map(format).padTo(2, "?")
      strings.mkString(" or ")
    case SimpleType.RelationConstructor => "Relation" // MATT ?
    case SimpleType.Relation(tpes) =>
      val terms = tpes.map(format).mkString(", ")
      s"Relation($terms)"
    case SimpleType.LatticeConstructor => "Lattice" // MATT ?
    case SimpleType.Lattice(tpes) =>
      val lat = format(tpes.last)
      val terms = tpes.init.map(format).mkString(", ")
      s"Lattice($terms; $lat)"
    case SimpleType.ArrowConstructor(arity) =>
      val params = Iterable.fill(arity - 1)("?").mkString("(", ", ", ")")
      s"$params -> ? & ?"
    case SimpleType.PartialPureArrow(arity, tpes) =>
      val params = tpes.map(format).padTo(arity - 1, "?").mkString("(", ", ", ")")
      s"$params -> ?"
    case SimpleType.PartialImpureArrow(arity, tpes) =>
      val params = tpes.map(format).padTo(arity - 1, "?").mkString("(", ", ", ")")
      s"$params ~> ?"
    case SimpleType.PartialPolyArrow(arity, tpes, eff) =>
      val params = tpes.map(format).padTo(arity - 1, "?").mkString("(", ", ", ")")
      val effString = format(eff)
      s"$params -> ? & $effString"
    case SimpleType.PureArrow(args, ret) =>
      val params = args.mkString("(", ", ", ")")
      val retString = format(ret)
      s"$params -> $retString"
    case SimpleType.ImpureArrow(args, ret) =>
      val params = args.mkString("(", ", ", ")")
      val retString = format(ret)
      s"$params ~> $retString"
    case SimpleType.PolyArrow(args, ret, eff) =>
      val params = args.mkString("(", ", ", ")")
      val retString = format(ret)
      val effString = format(eff)
      s"$params ~> $retString & $effString"
    case SimpleType.TagConstructor(name) => ???
    case SimpleType.PartialTag(name, args) => ???
    case SimpleType.Tag(name, args, ret) => ???
    case SimpleType.Name(name) => name
    case SimpleType.Apply(tpe, tpes) =>
      val string = format(tpe)
      val strings = tpes.map(format)
      string + strings.mkString("[", ", ", "]")
    case SimpleType.Var(id) => nameContext(id) // MATT have to handle bad lookups
    case SimpleType.Tuple(length, fields) =>
      val strings = fields.map(format).padTo(length, "?")
      strings.mkString("(", ", ", ")")
  }

}

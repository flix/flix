package ca.uwaterloo.flix.language.debug

import ca.uwaterloo.flix.language.ast.Kind.Effect
import ca.uwaterloo.flix.language.ast.{Type, TypeConstructor}

object FormatType2 { // MATT rename and replace FormatType

  // MATT this may be a very silly idea
  sealed trait Readability
  object Readability {
    case object Internal extends Readability
    case object External extends Readability
  }

  def format(tpe: Type)(implicit readability: Readability): String = tpe match {
    case tvar@Type.Var(id, kind, _) => readability match {
      case Readability.Internal => kind match {
        case Effect => s"''$id"
        case _ => s"'$id" // MATT probably want the text here too
      }
      case Readability.External => tvar.getText.getOrElse("<internal variable>") // MATT need a better representation?
    }

    case Type.Cst(TypeConstructor.Unit) => "Unit"
    case Type.Cst(TypeConstructor.Bool) => "Bool"
    case Type.Cst(TypeConstructor.Char) => "Char"
    case Type.Cst(TypeConstructor.Float32) => "Float32"
    case Type.Cst(TypeConstructor.Float64) => "Float64"
    case Type.Cst(TypeConstructor.Int8) => "Int8"
    case Type.Cst(TypeConstructor.Int16) => "Int16"
    case Type.Cst(TypeConstructor.Int32) => "Int32"
    case Type.Cst(TypeConstructor.Int64) => "Int64"
    case Type.Cst(TypeConstructor.BigInt) => "BigInt"
    case Type.Cst(TypeConstructor.Str) => "Str"
    case Type.Cst(TypeConstructor.RecordEmpty) => "{ }"
    case Type.Cst(TypeConstructor.SchemaEmpty) => "#{ }"
    case Type.Cst(TypeConstructor.Pure) => "Pure"
    case Type.Cst(TypeConstructor.Impure) => "Impure"

    case record@Type.Apply(Type.Apply(Type.Cst(TypeConstructor.RecordExtend(_)), _), _) => flattenRecord(record) match {
      case FlatThing(fields, Type.Cst(TypeConstructor.RecordEmpty)) =>
        fields.map { case (label, tpe) => s"$label: ${format(tpe)}" }.mkString("{ ", ", ", " }")
      case FlatThing(fields, rest) =>
        val fieldString = fields.map { case (label, tpe) => s"$label: ${format(tpe)}" }.mkString(", ")
        s"{ $fieldString | ${format(rest)} }"
    }

      // MATT this one is wrong: needs to be like #{ A(Int32, Str), B(Str) }
    case schema@Type.Apply(Type.Apply(Type.Cst(TypeConstructor.SchemaExtend(_)), _), _) => flattenSchema(schema) match {
      case FlatThing(fields, Type.Cst(TypeConstructor.SchemaEmpty)) =>
        fields.map { case (label, tpe) => s"$label: ${format(tpe)}" }.mkString("#{ ", ", ", " }")
      case FlatThing(fields, rest) =>
        val fieldString = fields.map { case (label, tpe) => s"$label: ${format(tpe)}" }.mkString(", ")
        s"#{ $fieldString | ${format(rest)} }"
    }

    case _ => ???

  }

  // MATT rename if we keep this
  case class FlatThing(fields: List[(String, Type)], rest: Type) {
    def ::(head: (String, Type)): FlatThing = {
      copy(fields = head :: fields)
    }
  }

  private def flattenRecord(record: Type): FlatThing = record match {
    case Type.Apply(Type.Apply(Type.Cst(TypeConstructor.RecordExtend(label)), tpe), rest) =>
      (label, tpe) :: flattenRecord(rest)
    case _ => FlatThing(Nil, record)
  }


  private def flattenSchema(schema: Type): FlatThing = schema match {
    case Type.Apply(Type.Apply(Type.Cst(TypeConstructor.SchemaExtend(label)), tpe), rest) =>
      (label, tpe) :: flattenSchema(rest)
    case _ => FlatThing(Nil, schema)
  }
}

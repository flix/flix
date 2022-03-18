/*
 * Copyright 2022 Matthew Lutze
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package ca.uwaterloo.flix.language.debug

import ca.uwaterloo.flix.language.ast.{Kind, Rigidity, Type}

object FormatSimpleType {
  /**
    * Transforms the given well-kinded type into a string.
    */
  def formatWellKindedType(tpe: Type)(implicit audience: Audience): String = {
    format(SimpleType.fromWellKindedType(tpe))
  }

  /**
    * Transforms the given type into a string.
    */
  private def format(tpe00: SimpleType)(implicit audience: Audience): String = {

    /**
      * Wraps the given type with parentheses.
      */
    def parenthesize(s: String): String = "(" + s + ")"

    /**
      * Transforms the given record `fieldType` pair into a string.
      */
    def visitRecordFieldType(fieldType: SimpleType.RecordFieldType): String = fieldType match {
      case SimpleType.RecordFieldType(field, tpe) => s"$field :: ${visit(tpe, Mode.Type)}"
    }

    /**
      * Transforms the given schema `fieldType` pair into a string.
      */
    def visitSchemaFieldType(fieldType: SimpleType.PredicateFieldType): String = fieldType match {
      case SimpleType.RelationFieldType(field, tpes) =>
        val tpeString = tpes.map(visit(_, Mode.Type)).mkString(", ")
        s"$field($tpeString)"
      case SimpleType.LatticeFieldType(field, tpes, lat) =>
        val tpeString = tpes.map(visit(_, Mode.Type)).mkString(", ")
        val latString = visit(lat, Mode.Type)
        s"$field($tpeString; $latString)"
    }

    /**
      * Transforms the given type into a string,
      * delimiting it as appropriate for display as a function argument.
      */
    def delimitFunctionArg(arg: SimpleType): String = arg match {
      // Tuples get an extra set of parentheses
      case tuple: SimpleType.Tuple => parenthesize(visit(tuple, Mode.Type))
      case tpe => delimit(tpe, Mode.Type)
    }

    /**
      * Returns `true` iff the given `tpe` is innately delimited,
      * meaning that it never needs parenthesization.
      */
    def isDelimited(tpe: SimpleType): Boolean = tpe match {
      // non-delimited types
      case SimpleType.Not(_) => false
      case SimpleType.And(_) => false
      case SimpleType.Or(_) => false
      case SimpleType.PureArrow(_, _) => false
      case SimpleType.PolyArrow(_, _, _) => false

      // delimited types
      case SimpleType.Hole => true
      case SimpleType.Unit => true
      case SimpleType.Null => true
      case SimpleType.Bool => true
      case SimpleType.Char => true
      case SimpleType.Float32 => true
      case SimpleType.Float64 => true
      case SimpleType.Int8 => true
      case SimpleType.Int16 => true
      case SimpleType.Int32 => true
      case SimpleType.Int64 => true
      case SimpleType.BigInt => true
      case SimpleType.Str => true
      case SimpleType.ScopedArray => true
      case SimpleType.ScopedRef => true
      case SimpleType.Channel => true
      case SimpleType.Lazy => true
      case SimpleType.True => true
      case SimpleType.False => true
      case SimpleType.Region => true
      case SimpleType.RecordConstructor(_) => true
      case SimpleType.Record(_) => true
      case SimpleType.RecordExtend(_, _) => true
      case SimpleType.RecordRow(_) => true
      case SimpleType.RecordRowExtend(_, _) => true
      case SimpleType.SchemaConstructor(_) => true
      case SimpleType.Schema(_) => true
      case SimpleType.SchemaExtend(_, _) => true
      case SimpleType.SchemaRow(_) => true
      case SimpleType.SchemaRowExtend(_, _) => true
      case SimpleType.RelationConstructor => true
      case SimpleType.Relation(_) => true
      case SimpleType.LatticeConstructor => true
      case SimpleType.Lattice(_, _) => true
      case SimpleType.TagConstructor(_) => true
      case SimpleType.Tag(_, _, _) => true
      case SimpleType.Name(_) => true
      case SimpleType.Apply(_, _) => true
      case SimpleType.Var(_, _, _, _) => true
      case SimpleType.Tuple(_) => true
    }

    /**
      * Delimits the given `tpe`, parenthesizing it if needed.
      */
    def delimit(tpe: SimpleType, mode: Mode): String = {
      if (isDelimited(tpe)) {
        visit(tpe, mode)
      } else {
        parenthesize(visit(tpe, mode))
      }
    }

    /**
      * Converts the given `tpe0` to a string.
      */
    def visit(tpe0: SimpleType, mode: Mode): String = tpe0 match {
      case SimpleType.Hole => "?"
      case SimpleType.Unit => "Unit"
      case SimpleType.Null => "Null"
      case SimpleType.Bool => "Bool"
      case SimpleType.Char => "Char"
      case SimpleType.Float32 => "Float32"
      case SimpleType.Float64 => "Float64"
      case SimpleType.Int8 => "Int8"
      case SimpleType.Int16 => "Int16"
      case SimpleType.Int32 => "Int32"
      case SimpleType.Int64 => "Int64"
      case SimpleType.BigInt => "BigInt"
      case SimpleType.Str => "String"
      case SimpleType.ScopedArray => "ScopedArray"
      case SimpleType.ScopedRef => "ScopedRef"
      case SimpleType.Channel => "Channel"
      case SimpleType.Lazy => "Lazy"
      case SimpleType.True => mode match {
        case Mode.Type => "true"
        case Mode.Effect => "Pure"
      }
      case SimpleType.False => mode match {
        case Mode.Type => "false"
        case Mode.Effect => "Impure"
      }
      case SimpleType.Region => "Region"
      case SimpleType.Record(fields) =>
        val fieldString = fields.map(visitRecordFieldType).mkString(", ")
        s"{ $fieldString }"
      case SimpleType.RecordExtend(fields, rest) =>
        val fieldString = fields.map(visitRecordFieldType).mkString(", ")
        val restString = visit(rest, mode)
        s"{ $fieldString | $restString }"
      case SimpleType.RecordRow(fields) =>
        val fieldString = fields.map(visitRecordFieldType).mkString(", ")
        s"( $fieldString )"
      case SimpleType.RecordRowExtend(fields, rest) =>
        val fieldString = fields.map(visitRecordFieldType).mkString(", ")
        val restString = visit(rest, Mode.Type)
        s"( $fieldString | $restString )"
      case SimpleType.RecordConstructor(arg) => s"{ $arg }"
      case SimpleType.Schema(fields) =>
        val fieldString = fields.map(visitSchemaFieldType).mkString(", ")
        s"#{ $fieldString }"
      case SimpleType.SchemaExtend(fields, rest) =>
        val fieldString = fields.map(visitSchemaFieldType).mkString(", ")
        val restString = visit(rest, Mode.Type)
        s"#{ $fieldString | $restString }"
      case SimpleType.SchemaRow(fields) =>
        val fieldString = fields.map(visitSchemaFieldType).mkString(", ")
        s"#( $fieldString )"
      case SimpleType.SchemaRowExtend(fields, rest) =>
        val fieldString = fields.map(visitSchemaFieldType).mkString(", ")
        val restString = visit(rest, Mode.Type)
        s"#( $fieldString | $restString )"
      case SimpleType.SchemaConstructor(arg) => s"#{ $arg }"
      case SimpleType.Not(tpe) => s"not ${delimit(tpe, mode)}"
      case SimpleType.And(tpes) =>
        val strings = tpes.map(delimit(_, mode))
        strings.mkString(" and ")
      case SimpleType.Or(tpes) =>
        val strings = tpes.map(delimit(_, mode))
        strings.mkString(" or ")
      case SimpleType.RelationConstructor => "Relation"
      case SimpleType.Relation(tpes) =>
        val terms = tpes.map(visit(_, Mode.Type)).mkString(", ")
        s"Relation($terms)"
      case SimpleType.LatticeConstructor => "Lattice"
      case SimpleType.Lattice(tpes0, lat0) =>
        val lat = visit(lat0, Mode.Type)
        val tpes = tpes0.map(visit(_, Mode.Type)).mkString(", ")
        s"Lattice($tpes; $lat)"
      case SimpleType.PureArrow(arg, ret) =>
        val argString = delimitFunctionArg(arg)
        val retString = delimit(ret, Mode.Type)
        s"$argString -> $retString"
      case SimpleType.PolyArrow(arg, eff, ret) =>
        val argString = delimitFunctionArg(arg)
        val effString = visit(eff, Mode.Effect)
        val retString = delimit(ret, Mode.Type)
        s"$argString ->{$effString} $retString"
      case SimpleType.TagConstructor(name) => ???
      case SimpleType.Tag(name, args, ret) => ???
      case SimpleType.Name(name) => name
      case SimpleType.Apply(tpe, tpes) =>
        val string = visit(tpe, Mode.Type)
        val strings = tpes.map(visit(_, Mode.Type))
        string + strings.mkString("[", ", ", "]")
      case SimpleType.Var(id, kind, rigidity, text) =>
        val prefix: String = kind match {
          case Kind.Wild => "_" + id.toString
          case Kind.Star => "t" + id
          case Kind.Bool => "b" + id
          case Kind.RecordRow => "r" + id
          case Kind.SchemaRow => "s" + id
          case Kind.Predicate => "'" + id.toString
          case Kind.Arrow(_, _) => "'" + id.toString
        }
        val suffix = rigidity match {
          case Rigidity.Flexible => ""
          case Rigidity.Rigid => "!"
        }
        val s = prefix + suffix
        audience match {
          case Audience.Internal => s
          case Audience.External => text.getOrElse(s)
        }

      case SimpleType.Tuple(fields) =>
        fields.map(visit(_, Mode.Type)).mkString("(", ", ", ")")
    }

    visit(tpe00, Mode.Type)
  }

  /**
    * Flag indicating whether a type should be formatted as an effect or as a regular type.
    */
  private sealed trait Mode
  private object Mode {
    case object Effect extends Mode
    case object Type extends Mode
  }

}

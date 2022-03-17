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

import ca.uwaterloo.flix.language.ast.Type

import scala.collection.mutable

object FormatSimpleType {
  // MATT docs
  // MATT decide on how API should look (exposing to all types here)
  def formatWellKindedType(tpe: Type, nameContext: Map[Int, String]): String = {
    format(SimpleType.fromWellKindedType(tpe), nameContext)
  }

  // MATT docs
  // MATT private?
  def format(tpe00: SimpleType, nc0: Map[Int, String]): String = {

    // generates the names a, b, ..., z, a1, b1, ...., z1, a2, b2, ...
    val nameGenerator = Iterator.iterate(('a', 0)) {
      case ('z', n) => ('a', n + 1)
      case (c, n) => ((c + 1).toChar, n)
    } map {
      case (c, 0) => s"$c"
      case (c, n) => s"$c$n"
    }

    val nc = mutable.Map.from(nc0)
    val names = mutable.Set.from(nc0.values)

    def nextAvailableName(): String = {
      nameGenerator.find(!names.contains(_)).get // safe to get since nameGenerator is infinite
    }

    /**
      * Wrap the given type with parentheses if it isn't already wrapped.
      */
    def withParens(tpe: String): String = {
      if (tpe.startsWith("(") && tpe.endsWith(")")) {
        tpe
      } else {
        s"($tpe)"
      }
    }

    def formatRecordFieldType(fieldType: SimpleType.FieldType): String = fieldType match {
      case SimpleType.FieldType(field, tpe) => s"$field :: ${visit(tpe)}"
    }

    def formatSchemaFieldType(fieldType: SimpleType.FieldType): String = fieldType match {
      case SimpleType.FieldType(field, tpe) => s"$field${withParens(visit(tpe))}"
    }

    def visit(tpe0: SimpleType): String = tpe0 match {
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
      case SimpleType.Str => "Str"
      case SimpleType.ScopedArray => "ScopedArray"
      case SimpleType.ScopedRef => "ScopedRef"
      case SimpleType.Channel => "Channel"
      case SimpleType.Lazy => "Lazy"
      case SimpleType.True => "True"
      case SimpleType.False => "False"
      case SimpleType.Region => "Region"
      case SimpleType.Record(fields) =>
        val fieldString = fields.map(formatRecordFieldType).mkString(", ")
        s"{ $fieldString }"
      case SimpleType.RecordExtend(fields, rest) =>
        val fieldString = fields.map(formatRecordFieldType).mkString(", ")
        val restString = visit(rest)
        s"{ $fieldString | $restString }"
      case SimpleType.RecordRow(fields) =>
        val fieldString = fields.map(formatRecordFieldType).mkString(", ")
        s"( $fieldString )"
      case SimpleType.RecordRowExtend(fields, rest) =>
        val fieldString = fields.map(formatRecordFieldType).mkString(", ")
        val restString = visit(rest)
        s"( $fieldString | $restString )"
      case SimpleType.RecordConstructor(arg) => s"{ $arg }"
      case SimpleType.Schema(fields) =>
        val fieldString = fields.map(formatSchemaFieldType).mkString(", ")
        s"#{ $fieldString }"
      case SimpleType.SchemaExtend(fields, rest) =>
        val fieldString = fields.map(formatSchemaFieldType).mkString(", ")
        val restString = visit(rest)
        s"#{ $fieldString | $restString}"
      case SimpleType.SchemaRow(fields) =>
        val fieldString = fields.map(formatSchemaFieldType).mkString(", ")
        s"#( $fieldString )"
      case SimpleType.SchemaRowExtend(fields, rest) =>
        val fieldString = fields.map(formatSchemaFieldType).mkString(", ")
        val restString = visit(rest)
        s"#( $fieldString | $restString)"
      case SimpleType.SchemaConstructor(arg) => s"#{ $arg }"
      case SimpleType.Not(tpe) => s"not ${visit(tpe)}" // MATT handle parens
      case SimpleType.And(tpes) =>
        val strings = tpes.map(visit)
        strings.mkString(" and ")
      case SimpleType.Or(tpes) =>
        val strings = tpes.map(visit)
        strings.mkString(" or ")
      case SimpleType.RelationConstructor => "Relation" // MATT ?
      case SimpleType.Relation(tpes) =>
        val terms = tpes.map(visit).mkString(", ")
        s"Relation($terms)"
      case SimpleType.LatticeConstructor => "Lattice" // MATT ?
      case SimpleType.Lattice(tpes) =>
        val lat = visit(tpes.last)
        val terms = tpes.init.map(visit).mkString(", ")
        s"Lattice($terms; $lat)"
      case SimpleType.PureArrow(arg, ret) => s"$arg -> $ret"
      case SimpleType.PolyArrow(arg, eff, ret) => s"$arg ->{$eff} $ret"
      case SimpleType.TagConstructor(name) => ???
      case SimpleType.Tag(name, args, ret) => ???
      case SimpleType.Name(name) => name
      case SimpleType.Apply(tpe, tpes) =>
        val string = visit(tpe)
        val strings = tpes.map(visit)
        string + strings.mkString("[", ", ", "]")
      case SimpleType.Var(id) =>
        val name = nc.getOrElseUpdate(id, nextAvailableName())
        names.add(name)
        name
      case SimpleType.Tuple(fields) =>
        fields.map(visit).mkString("(", ", ", ")")
    }

    visit(tpe00)
  }

}

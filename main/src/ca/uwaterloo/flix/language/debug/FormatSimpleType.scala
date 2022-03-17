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
  def formatWellKindedType(tpe: Type): String = {
    format(SimpleType.fromWellKindedType(tpe))
  }

  // MATT docs
  // MATT private?
  def format(tpe00: SimpleType): String = {

    /**
      * Wrap the given type with parentheses
      */
    def parenthesize(s: String): String = "(" + s + ")"

    /**
      * Wrap the given type with parentheses if it isn't already wrapped.
      */
    def ensureParens(s: String): String = {
      if (s.startsWith("(")) {
        s
      } else {
        parenthesize(s)
      }
    }

    def visitRecordFieldType(fieldType: SimpleType.FieldType): String = fieldType match {
      case SimpleType.FieldType(field, tpe) => s"$field :: ${visit(tpe)}"
    }

    def visitSchemaFieldType(fieldType: SimpleType.FieldType): String = fieldType match {
      case SimpleType.FieldType(field, tpe) => s"$field${ensureParens(visit(tpe))}"
    }

    def delimitFunctionArg(arg: SimpleType): String = arg match {
      // Tuples get an extra set of parentheses
      case tuple: SimpleType.Tuple => parenthesize(visit(tuple))
      case tpe => delimit(tpe)
    }

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
      case SimpleType.Lattice(_) => true
      case SimpleType.TagConstructor(_) => true
      case SimpleType.Tag(_, _, _) => true
      case SimpleType.Name(_) => true
      case SimpleType.Apply(_, _) => true
      case SimpleType.Var(_) => true
      case SimpleType.Tuple(_) => true
    }

    def delimit(tpe: SimpleType): String = {
      if (isDelimited(tpe)) {
        visit(tpe)
      } else {
        parenthesize(visit(tpe))
      }
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
      case SimpleType.Str => "String"
      case SimpleType.ScopedArray => "ScopedArray"
      case SimpleType.ScopedRef => "ScopedRef"
      case SimpleType.Channel => "Channel"
      case SimpleType.Lazy => "Lazy"
      case SimpleType.True => "True"
      case SimpleType.False => "False"
      case SimpleType.Region => "Region"
      case SimpleType.Record(fields) =>
        val fieldString = fields.map(visitRecordFieldType).mkString(", ")
        s"{ $fieldString }"
      case SimpleType.RecordExtend(fields, rest) =>
        val fieldString = fields.map(visitRecordFieldType).mkString(", ")
        val restString = visit(rest)
        s"{ $fieldString | $restString }"
      case SimpleType.RecordRow(fields) =>
        val fieldString = fields.map(visitRecordFieldType).mkString(", ")
        s"( $fieldString )"
      case SimpleType.RecordRowExtend(fields, rest) =>
        val fieldString = fields.map(visitRecordFieldType).mkString(", ")
        val restString = visit(rest)
        s"( $fieldString | $restString )"
      case SimpleType.RecordConstructor(arg) => s"{ $arg }"
      case SimpleType.Schema(fields) =>
        val fieldString = fields.map(visitSchemaFieldType).mkString(", ")
        s"#{ $fieldString }"
      case SimpleType.SchemaExtend(fields, rest) =>
        val fieldString = fields.map(visitSchemaFieldType).mkString(", ")
        val restString = visit(rest)
        s"#{ $fieldString | $restString}"
      case SimpleType.SchemaRow(fields) =>
        val fieldString = fields.map(visitSchemaFieldType).mkString(", ")
        s"#( $fieldString )"
      case SimpleType.SchemaRowExtend(fields, rest) =>
        val fieldString = fields.map(visitSchemaFieldType).mkString(", ")
        val restString = visit(rest)
        s"#( $fieldString | $restString)"
      case SimpleType.SchemaConstructor(arg) => s"#{ $arg }"
      case SimpleType.Not(tpe) => s"not ${delimit(tpe)}"
      case SimpleType.And(tpes) =>
        val strings = tpes.map(delimit)
        strings.mkString(" and ")
      case SimpleType.Or(tpes) =>
        val strings = tpes.map(delimit)
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
      case SimpleType.PureArrow(arg, ret) =>
        val argString = delimitFunctionArg(arg)
        val retString = delimit(ret)
        s"$argString -> $retString"
      case SimpleType.PolyArrow(arg, eff, ret) =>
        val argString = delimitFunctionArg(arg)
        val effString = visit(eff)
        val retString = delimit(ret)
        s"$argString ->{$effString} $retString"
      case SimpleType.TagConstructor(name) => ???
      case SimpleType.Tag(name, args, ret) => ???
      case SimpleType.Name(name) => name
      case SimpleType.Apply(tpe, tpes) =>
        val string = visit(tpe)
        val strings = tpes.map(visit)
        string + strings.mkString("[", ", ", "]")
      case SimpleType.Var(id) => ???
      case SimpleType.Tuple(fields) =>
        fields.map(visit).mkString("(", ", ", ")")
    }

    visit(tpe00)
  }

}

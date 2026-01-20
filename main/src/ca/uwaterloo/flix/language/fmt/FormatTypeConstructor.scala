/*
 * Copyright 2025 Flix Contributors
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
package ca.uwaterloo.flix.language.fmt

import ca.uwaterloo.flix.language.ast.TypeConstructor

object FormatTypeConstructor {

  /**
    * Returns a human-readable string representation of the given type constructor.
    */
  def formatTypeConstructor(tc: TypeConstructor): String = tc match {
    // Primitives
    case TypeConstructor.Void => "Void"
    case TypeConstructor.AnyType => "AnyType"
    case TypeConstructor.Unit => "Unit"
    case TypeConstructor.Null => "Null"
    case TypeConstructor.Bool => "Bool"
    case TypeConstructor.Char => "Char"
    case TypeConstructor.Float32 => "Float32"
    case TypeConstructor.Float64 => "Float64"
    case TypeConstructor.BigDecimal => "BigDecimal"
    case TypeConstructor.Int8 => "Int8"
    case TypeConstructor.Int16 => "Int16"
    case TypeConstructor.Int32 => "Int32"
    case TypeConstructor.Int64 => "Int64"
    case TypeConstructor.BigInt => "BigInt"
    case TypeConstructor.Str => "String"
    case TypeConstructor.Regex => "Regex"

    // Arrows
    case TypeConstructor.Arrow(arity) => s"Arrow$arity"
    case TypeConstructor.ArrowWithoutEffect(arity) => s"Arrow$arity"

    // Records
    case TypeConstructor.RecordRowEmpty => "RecordRowEmpty"
    case TypeConstructor.RecordRowExtend(label) => s"RecordRowExtend(${label.name})"
    case TypeConstructor.Record => "Record"

    // Schemas
    case TypeConstructor.Extensible => "Extensible"
    case TypeConstructor.SchemaRowEmpty => "SchemaRowEmpty"
    case TypeConstructor.SchemaRowExtend(pred) => s"SchemaRowExtend(${pred.name})"
    case TypeConstructor.Schema => "Schema"

    // Collections
    case TypeConstructor.Array => "Array"
    case TypeConstructor.ArrayWithoutRegion => "Array"
    case TypeConstructor.Vector => "Vector"
    case TypeConstructor.Sender => "Sender"
    case TypeConstructor.Receiver => "Receiver"
    case TypeConstructor.Lazy => "Lazy"

    // User-defined types
    case TypeConstructor.Enum(sym, _) => sym.name
    case TypeConstructor.Struct(sym, _) => sym.name
    case TypeConstructor.RestrictableEnum(sym, _) => sym.name

    // JVM types
    case TypeConstructor.Native(clazz) => clazz.getSimpleName
    case TypeConstructor.JvmConstructor(constructor) => s"Constructor(${constructor.getDeclaringClass.getSimpleName})"
    case TypeConstructor.JvmMethod(method) => s"Method(${method.getName})"
    case TypeConstructor.JvmField(field) => s"Field(${field.getName})"

    // Tuples and relations
    case TypeConstructor.Tuple(arity) => s"Tuple$arity"
    case TypeConstructor.Relation(arity) => s"Relation$arity"
    case TypeConstructor.Lattice(arity) => s"Lattice$arity"

    // Effects
    case TypeConstructor.Pure => "Pure"
    case TypeConstructor.Univ => "Univ"
    case TypeConstructor.Effect(sym, _) => sym.name

    // Boolean formulas
    case TypeConstructor.True => "True"
    case TypeConstructor.False => "False"
    case TypeConstructor.And => "And"
    case TypeConstructor.Or => "Or"
    case TypeConstructor.Not => "Not"

    // Set operations
    case TypeConstructor.Complement => "Complement"
    case TypeConstructor.Union => "Union"
    case TypeConstructor.Intersection => "Intersection"
    case TypeConstructor.Difference => "Difference"
    case TypeConstructor.SymmetricDiff => "SymmetricDiff"

    // Case set operations
    case TypeConstructor.CaseSet(syms, _) => s"CaseSet(${syms.map(_.name).mkString(", ")})"
    case TypeConstructor.CaseComplement(_) => "CaseComplement"
    case TypeConstructor.CaseIntersection(_) => "CaseIntersection"
    case TypeConstructor.CaseUnion(_) => "CaseUnion"
    case TypeConstructor.CaseSymmetricDiff(_) => "CaseSymmetricDiff"

    // Regions
    case TypeConstructor.Region(sym) => s"Region(${sym.text})"
    case TypeConstructor.RegionToStar => "RegionToStar"
    case TypeConstructor.RegionWithoutRegion => "Region"

    // Error
    case TypeConstructor.Error(_, _) => "Error"
  }
}

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
package ca.uwaterloo.flix.language.fmt

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.shared.{Scope, SymbolSet, VarText}
import ca.uwaterloo.flix.language.ast.{Kind, RigidityEnv, SourceLocation, Symbol, Type}
import ca.uwaterloo.flix.language.phase.TypeSimplifier
import ca.uwaterloo.flix.language.phase.unification.Substitution

object FormatType {
  /**
    * Transforms the given well-kinded type into a string.
    *
    * Minimizes the given type.
    *
    * Performs alpha renaming if the rigidity environment is present.
    */
  def formatType(tpe: Type, renv: Option[RigidityEnv] = None, minimizeEffs: Boolean = false, amb: SymbolSet = SymbolSet.empty)(implicit flix: Flix): String = {
    val renamed = renv match {
      case None => tpe
      case Some(env) => alphaRename(tpe, env)
    }
    val minimized = if (minimizeEffs) {
      TypeSimplifier.simplify(renamed)
    } else {
      renamed
    }
    formatTypeWithOptions(minimized, flix.getFormatOptions, amb = amb)
  }

  /**
    * Renames all flexible variables in the given `tpe` to fresh consecutively numbered variables.
    *
    * NOTE: This function should *ONLY* be used for pretty printing.
    */
  private def alphaRename(tpe: Type, renv: RigidityEnv): Type = {
    // Compute the free type variables.
    val freeVars = tpe.typeVars.toList.sortBy(_.sym.id)

    // Compute the flexible variables (i.e. the free variables that are not rigid).
    val flexibleVars = renv.getFlexibleVarsOf(freeVars)(Scope.Top) // TODO LEVELS ideally we should have a proper scope here

    // Compute a substitution that maps the first flexible variable to id 1 and so forth.
    val m = flexibleVars.zipWithIndex.map {
      case (Type.Var(sym, loc), index) =>
        sym -> (Type.Var(new Symbol.KindedTypeVarSym(index, sym.text, sym.kind, sym.isSlack, sym.scope, loc), loc): Type)
    }
    val s = Substitution(m.toMap)

    // Apply the substitution to the type.
    s(tpe)
  }

  /**
    * Transforms the given well-kinded type into a string, using the given format options.
    */
  def formatTypeWithOptions(tpe: Type, fmt: FormatOptions, amb: SymbolSet = SymbolSet.empty): String = {
    try {
      format(DisplayType.fromWellKindedType(tpe, amb = amb))(fmt)
    } catch {
      case _: Throwable => "ERR_UNABLE_TO_FORMAT_TYPE"
    }
  }

  /**
    * Transforms the given kinded type variable symbol into a string.
    */
  def formatTypeVarSymWithOptions(sym: Symbol.KindedTypeVarSym, fmt: FormatOptions): String = {
    val tpe = Type.Var(sym, SourceLocation.Unknown)
    formatTypeWithOptions(tpe, fmt)
  }

  /**
    * Transforms the given type view into a string.
    */
  def formatDisplayType(tpe: DisplayType)(implicit flix: Flix): String =
    formatDisplayTypeWithOptions(tpe, flix.getFormatOptions)

  /**
    * Transforms the given type view into a string, using the given format options.
    */
  def formatDisplayTypeWithOptions(tpe: DisplayType, fmt: FormatOptions): String =
    format(tpe)(fmt)

  /**
    * Transforms the given type into a string.
    */
  private def format(tpe00: DisplayType)(implicit fmt: FormatOptions): String = {

    /**
      * Wraps the given type with parentheses.
      */
    def parenthesize(s: String): String = "(" + s + ")"

    /**
      * Transforms the given record `labelType` pair into a string.
      */
    def visitRecordLabelType(labelType: DisplayType.RecordLabelType): String = labelType match {
      case DisplayType.RecordLabelType(label, tpe) => s"$label = ${visit(tpe, Mode.Type)}"
    }

    /**
      * Transforms the given schema `fieldType` pair into a string.
      */
    def visitSchemaFieldType(fieldType: DisplayType.PredicateFieldType): String = fieldType match {
      case DisplayType.RelationFieldType(field, tpes) =>
        val tpeString = tpes.map(visit(_, Mode.Type)).mkString(", ")
        s"$field($tpeString)"
      case DisplayType.LatticeFieldType(field, tpes, lat) =>
        val tpeString = tpes.map(visit(_, Mode.Type)).mkString(", ")
        val latString = visit(lat, Mode.Type)
        s"$field($tpeString; $latString)"
      case DisplayType.NonPredFieldType(field, tpe) =>
        val tpeString = visit(tpe, Mode.Type)
        s"$field(<$tpeString>)"
    }

    /**
      * Transforms the given type into a string,
      * delimiting it as appropriate for display as a function argument.
      */
    def delimitFunctionArg(arg: DisplayType): String = arg match {
      // Tuples get an extra set of parentheses
      case tuple: DisplayType.Tuple => parenthesize(visit(tuple, Mode.Type))
      case tpe => delimit(tpe, Mode.Type)
    }

    /**
      * Returns `true` iff the given `tpe` is innately delimited,
      * meaning that it never needs parenthesization.
      */
    def isDelimited(tpe: DisplayType): Boolean = tpe match {
      // non-delimited types
      case DisplayType.Not(_) => false
      case DisplayType.And(_) => false
      case DisplayType.Or(_) => false
      case DisplayType.Complement(_) => false
      case DisplayType.Intersection(_) => false
      case DisplayType.SymmetricDiff(_) => false
      case DisplayType.Difference(_) => false
      case DisplayType.Plus(_) => false
      case DisplayType.PureArrow(_, _) => false
      case DisplayType.PolyArrow(_, _, _) => false
      case DisplayType.ArrowWithoutEffect(_, _) => false

      // delimited types
      case DisplayType.Hole => true
      case DisplayType.Void => true
      case DisplayType.AnyType => true
      case DisplayType.Unit => true
      case DisplayType.Null => true
      case DisplayType.Bool => true
      case DisplayType.Char => true
      case DisplayType.Float32 => true
      case DisplayType.Float64 => true
      case DisplayType.BigDecimal => true
      case DisplayType.Int8 => true
      case DisplayType.Int16 => true
      case DisplayType.Int32 => true
      case DisplayType.Int64 => true
      case DisplayType.BigInt => true
      case DisplayType.Str => true
      case DisplayType.Regex => true
      case DisplayType.Array => true
      case DisplayType.ArrayWithoutRegion => true
      case DisplayType.Vector => true
      case DisplayType.Sender => true
      case DisplayType.Receiver => true
      case DisplayType.Lazy => true
      case DisplayType.True => true
      case DisplayType.False => true
      case DisplayType.Pure => true
      case DisplayType.Univ => true
      case DisplayType.Region(_) => true
      case DisplayType.RegionToStar => true
      case DisplayType.RegionWithoutRegion => true
      case DisplayType.RecordConstructor(_) => true
      case DisplayType.Record(_) => true
      case DisplayType.RecordExtend(_, _) => true
      case DisplayType.RecordRow(_) => true
      case DisplayType.RecordRowExtend(_, _) => true
      case DisplayType.SchemaConstructor(_) => true
      case DisplayType.Schema(_) => true
      case DisplayType.SchemaExtend(_, _) => true
      case DisplayType.SchemaRow(_) => true
      case DisplayType.SchemaRowExtend(_, _) => true
      case DisplayType.ExtensibleUnknown(_) => true
      case DisplayType.Extensible(_) => true
      case DisplayType.ExtensibleExtend(_, _) => true
      case DisplayType.RelationConstructor => true
      case DisplayType.Relation(_) => true
      case DisplayType.LatticeConstructor => true
      case DisplayType.Lattice(_, _) => true
      case DisplayType.TagConstructor(_) => true
      case DisplayType.Name(_) => true
      case DisplayType.Apply(_, _) => true
      case DisplayType.Var(_, _, _) => true
      case DisplayType.Tuple(_) => true
      case DisplayType.JvmToType(_) => true
      case DisplayType.JvmToEff(_) => true
      case DisplayType.JvmUnresolvedConstructor(_, _) => true
      case DisplayType.JvmUnresolvedField(_, _) => true
      case DisplayType.JvmUnresolvedMethod(_, _, _) => true
      case DisplayType.JvmUnresolvedStaticMethod(_, _, _) => true
      case DisplayType.JvmConstructor(_) => true
      case DisplayType.JvmField(_) => true
      case DisplayType.JvmMethod(_) => true
      case DisplayType.Union(_) => true
      case DisplayType.Error => true
    }

    /**
      * Delimits the given `tpe`, parenthesizing it if needed.
      */
    def delimit(tpe: DisplayType, mode: Mode): String = {
      if (isDelimited(tpe)) {
        visit(tpe, mode)
      } else {
        parenthesize(visit(tpe, mode))
      }
    }

    /**
      * Converts the given `tpe0` to a string.
      */
    def visit(tpe0: DisplayType, mode: Mode): String = tpe0 match {
      case DisplayType.Hole => "?"
      case DisplayType.Void => "Void"
      case DisplayType.AnyType => "AnyType"
      case DisplayType.Unit => "Unit"
      case DisplayType.Null => "Null"
      case DisplayType.Bool => "Bool"
      case DisplayType.Char => "Char"
      case DisplayType.Float32 => "Float32"
      case DisplayType.Float64 => "Float64"
      case DisplayType.BigDecimal => "BigDecimal"
      case DisplayType.Int8 => "Int8"
      case DisplayType.Int16 => "Int16"
      case DisplayType.Int32 => "Int32"
      case DisplayType.Int64 => "Int64"
      case DisplayType.BigInt => "BigInt"
      case DisplayType.Str => "String"
      case DisplayType.Regex => "Regex"
      case DisplayType.Array => "Array"
      case DisplayType.ArrayWithoutRegion => "ArrayWithoutRegion"
      case DisplayType.Vector => "Vector"
      case DisplayType.Sender => "Sender"
      case DisplayType.Receiver => "Receiver"
      case DisplayType.Lazy => "Lazy"
      case DisplayType.False => "false"
      case DisplayType.True => "true"
      case DisplayType.Pure => mode match {
        case Mode.Type => "Pure"
        case Mode.Purity => "{}"
      }
      case DisplayType.Univ => "Univ"
      case DisplayType.Region(name) => name
      case DisplayType.RegionToStar => "Region"
      case DisplayType.RegionWithoutRegion => "RegionWithoutRegion"
      case DisplayType.Record(labels) =>
        val labelString = labels.map(visitRecordLabelType).mkString(", ")
        s"{ $labelString }"
      case DisplayType.RecordExtend(labels, rest) =>
        val labelString = labels.map(visitRecordLabelType).mkString(", ")
        val restString = visit(rest, mode)
        s"{ $labelString | $restString }"
      case DisplayType.RecordRow(labels) =>
        val labelString = labels.map(visitRecordLabelType).mkString(", ")
        s"( $labelString )"
      case DisplayType.RecordRowExtend(labels, rest) =>
        val labelString = labels.map(visitRecordLabelType).mkString(", ")
        val restString = visit(rest, Mode.Type)
        s"( $labelString | $restString )"
      case DisplayType.RecordConstructor(arg) => s"{ ${visit(arg, Mode.Type)} }"
      case DisplayType.Schema(fields) =>
        val fieldString = fields.map(visitSchemaFieldType).mkString(", ")
        s"#{ $fieldString }"
      case DisplayType.SchemaExtend(fields, rest) =>
        val fieldString = fields.map(visitSchemaFieldType).mkString(", ")
        val restString = visit(rest, Mode.Type)
        s"#{ $fieldString | $restString }"
      case DisplayType.SchemaRow(fields) =>
        val fieldString = fields.map(visitSchemaFieldType).mkString(", ")
        s"#( $fieldString )"
      case DisplayType.SchemaRowExtend(fields, rest) =>
        val fieldString = fields.map(visitSchemaFieldType).mkString(", ")
        val restString = visit(rest, Mode.Type)
        s"#( $fieldString | $restString )"
      case DisplayType.SchemaConstructor(arg) => s"#{ ${visit(arg, Mode.Type)} }"
      case DisplayType.Extensible(fields) =>
        val fieldString = fields.map(visitSchemaFieldType).mkString(", ")
        s"#| $fieldString |#"
      case DisplayType.ExtensibleExtend(fields, rest) =>
        val fieldString = fields.map(visitSchemaFieldType).mkString(", ")
        val restString = visit(rest, Mode.Type)
        s"#| $fieldString | $restString |#"
      case DisplayType.ExtensibleUnknown(arg) => s"#| ${visit(arg, Mode.Type)} |#"
      case DisplayType.Not(tpe) => s"not ${delimit(tpe, mode)}"
      case DisplayType.And(tpes) =>
        val strings = tpes.map(delimit(_, mode))
        strings.mkString(" and ")
      case DisplayType.Or(tpes) =>
        val strings = tpes.map(delimit(_, mode))
        strings.mkString(" or ")
      case DisplayType.Complement(tpe) => s"~${delimit(tpe, mode)}"
      case DisplayType.Union(tpes) =>
        val strings = tpes.map(visit(_, mode))
        strings.mkString("{", ", ", "}")
      case DisplayType.Plus(tpes) =>
        val strings = tpes.map(delimit(_, mode))
        strings.mkString(" + ")
      case DisplayType.Intersection(tpes) =>
        val strings = tpes.map(delimit(_, mode))
        strings.mkString(" & ")
      case DisplayType.SymmetricDiff(tpes) =>
        val strings = tpes.map(delimit(_, mode))
        strings.mkString(" ⊕ ")
      case DisplayType.Difference(tpes) =>
        val strings = tpes.map(delimit(_, mode))
        strings.mkString(" - ")
      case DisplayType.RelationConstructor => "Relation"
      case DisplayType.Relation(tpes) =>
        val terms = tpes.map(visit(_, Mode.Type)).mkString(", ")
        s"Relation($terms)"
      case DisplayType.LatticeConstructor => "Lattice"
      case DisplayType.Lattice(tpes0, lat0) =>
        val lat = visit(lat0, Mode.Type)
        val tpes = tpes0.map(visit(_, Mode.Type)).mkString(", ")
        s"Lattice($tpes; $lat)"
      case DisplayType.PureArrow(arg, ret) =>
        val argString = delimitFunctionArg(arg)
        val retString = delimit(ret, Mode.Type)
        s"$argString -> $retString"
      case DisplayType.PolyArrow(arg, eff, ret) =>
        val argString = delimitFunctionArg(arg)
        val effString = visit(eff, Mode.Purity)
        val retString = delimit(ret, Mode.Type)
        s"$argString -> $retString \\ $effString"
      case DisplayType.ArrowWithoutEffect(arg, ret) =>
        val argString = delimitFunctionArg(arg)
        val retString = delimit(ret, Mode.Type)
        s"$argString --> $retString"
      case DisplayType.TagConstructor(name) => name
      case DisplayType.Name(name) => name
      case DisplayType.Apply(tpe, tpes) =>
        val string = visit(tpe, Mode.Type)
        val strings = tpes.map(visit(_, Mode.Type))
        string + strings.mkString("[", ", ", "]")
      case DisplayType.Var(id, kind, text) =>
        val string: String = kind match {
          case Kind.Wild => "_" + id.toString
          case Kind.WildCaseSet => "_c" + id.toString
          case Kind.Star => "t" + id
          case Kind.Eff => "e" + id
          case Kind.Bool => "b" + id
          case Kind.RecordRow => "r" + id
          case Kind.SchemaRow => "s" + id
          case Kind.Predicate => "'" + id.toString
          case Kind.Jvm => "j" + id.toString
          case Kind.CaseSet(_) => "c" + id.toString
          case Kind.Arrow(_, _) => "'" + id.toString
          case Kind.Error => "err" + id.toString
        }
        fmt.varNames match {
          case FormatOptions.VarName.IdBased => string
          case FormatOptions.VarName.NameBased => text match {
            case VarText.Absent => string
            case VarText.SourceText(s) => s
          }
        }

      case DisplayType.Tuple(elms) =>
        elms.map(visit(_, Mode.Type)).mkString("(", ", ", ")")

      case DisplayType.JvmToType(tpe) =>
        val arg = visit(tpe, Mode.Type)
        "JvmToType(" + arg + ")"

      case DisplayType.JvmToEff(tpe) =>
        val arg = visit(tpe, Mode.Type)
        "JvmToEff(" + arg + ")"

      case DisplayType.JvmUnresolvedConstructor(name, tpes0) =>
        val tpes = tpes0.map(visit(_, Mode.Type))
        "JvmUnresolvedConstructor(" + name + ", " + tpes.mkString(", ") + ")"

      case DisplayType.JvmUnresolvedField(t0, name) =>
        val t = visit(t0, Mode.Type)
        "JvmUnresolvedField(" + t + ", " + name + ")"

      case DisplayType.JvmUnresolvedMethod(t0, name, ts0) =>
        val t = visit(t0, Mode.Type)
        val ts = ts0.map(visit(_, Mode.Type))
        "JvmUnresolvedMethod(" + t + ", " + name + ", " + ts.mkString(", ") + ")"

      case DisplayType.JvmUnresolvedStaticMethod(clazz, name, ts0) =>
        val ts = ts0.map(visit(_, Mode.Type))
        "JvmUnresolvedStaticMethod(" + clazz + ", " + name + ", " + ts.mkString(", ") + ")"

      case DisplayType.JvmConstructor(constructor) =>
        s"JvmConstructor($constructor)"

      case DisplayType.JvmField(field) =>
        s"JvmField($field)"

      case DisplayType.JvmMethod(method) =>
        s"JvmMethod($method)"

      case DisplayType.Error => "Error"

    }

    visit(tpe00, Mode.Type)
  }

  /**
    * Flag indicating whether a type should be formatted as an effect or as a regular type.
    */
  private sealed trait Mode

  private object Mode {
    case object Purity extends Mode

    case object Type extends Mode
  }

}

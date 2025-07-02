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
import ca.uwaterloo.flix.language.ast.shared.{Scope, VarText}
import ca.uwaterloo.flix.language.ast.{Kind, RigidityEnv, SourceLocation, Symbol, Type}
import ca.uwaterloo.flix.language.phase.unification.Substitution

object FormatType {
  /**
    * Transforms the given well-kinded type into a string.
    *
    * Minimizes the given type.
    *
    * Performs alpha renaming if the rigidity environment is present.
    */
  def formatType(tpe: Type, renv: Option[RigidityEnv] = None, minimizeEffs: Boolean = false)(implicit flix: Flix): String = {
    val renamed = renv match {
      case None => tpe
      case Some(env) => alphaRename(tpe, env)
    }
    val minimized = if (minimizeEffs) {
      Type.simplifyEffects(renamed)
    } else {
      renamed
    }
    formatTypeWithOptions(minimized, flix.getFormatOptions)
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
  def formatTypeWithOptions(tpe: Type, fmt: FormatOptions): String = {
    try {
      format(TypeView.fromWellKindedType(tpe))(fmt)
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
  def formatTypeView(tpe: TypeView)(implicit flix: Flix): String =
    formatTypeViewWithOptions(tpe, flix.getFormatOptions)

  /**
    * Transforms the given type view into a string, using the given format options.
    */
  def formatTypeViewWithOptions(tpe: TypeView, fmt: FormatOptions): String =
    format(tpe)(fmt)

  /**
    * Transforms the given type into a string.
    */
  private def format(tpe00: TypeView)(implicit fmt: FormatOptions): String = {

    /**
      * Wraps the given type with parentheses.
      */
    def parenthesize(s: String): String = "(" + s + ")"

    /**
      * Transforms the given record `labelType` pair into a string.
      */
    def visitRecordLabelType(labelType: TypeView.RecordLabelType): String = labelType match {
      case TypeView.RecordLabelType(label, tpe) => s"$label = ${visit(tpe, Mode.Type)}"
    }

    /**
      * Transforms the given schema `fieldType` pair into a string.
      */
    def visitSchemaFieldType(fieldType: TypeView.PredicateFieldType): String = fieldType match {
      case TypeView.RelationFieldType(field, tpes) =>
        val tpeString = tpes.map(visit(_, Mode.Type)).mkString(", ")
        s"$field($tpeString)"
      case TypeView.LatticeFieldType(field, tpes, lat) =>
        val tpeString = tpes.map(visit(_, Mode.Type)).mkString(", ")
        val latString = visit(lat, Mode.Type)
        s"$field($tpeString; $latString)"
      case TypeView.NonPredFieldType(field, tpe) =>
        val tpeString = visit(tpe, Mode.Type)
        s"$field(<$tpeString>)"
    }

    /**
      * Transforms the given type into a string,
      * delimiting it as appropriate for display as a function argument.
      */
    def delimitFunctionArg(arg: TypeView): String = arg match {
      // Tuples get an extra set of parentheses
      case tuple: TypeView.Tuple => parenthesize(visit(tuple, Mode.Type))
      case tpe => delimit(tpe, Mode.Type)
    }

    /**
      * Returns `true` iff the given `tpe` is innately delimited,
      * meaning that it never needs parenthesization.
      */
    def isDelimited(tpe: TypeView): Boolean = tpe match {
      // non-delimited types
      case TypeView.Not(_) => false
      case TypeView.And(_) => false
      case TypeView.Or(_) => false
      case TypeView.Complement(_) => false
      case TypeView.Intersection(_) => false
      case TypeView.SymmetricDiff(_) => false
      case TypeView.Difference(_) => false
      case TypeView.Plus(_) => false
      case TypeView.PureArrow(_, _) => false
      case TypeView.PolyArrow(_, _, _) => false
      case TypeView.ArrowWithoutEffect(_, _) => false

      // delimited types
      case TypeView.Hole => true
      case TypeView.Void => true
      case TypeView.AnyType => true
      case TypeView.Unit => true
      case TypeView.Null => true
      case TypeView.Bool => true
      case TypeView.Char => true
      case TypeView.Float32 => true
      case TypeView.Float64 => true
      case TypeView.BigDecimal => true
      case TypeView.Int8 => true
      case TypeView.Int16 => true
      case TypeView.Int32 => true
      case TypeView.Int64 => true
      case TypeView.BigInt => true
      case TypeView.Str => true
      case TypeView.Regex => true
      case TypeView.Array => true
      case TypeView.ArrayWithoutRegion => true
      case TypeView.Vector => true
      case TypeView.Sender => true
      case TypeView.Receiver => true
      case TypeView.Lazy => true
      case TypeView.True => true
      case TypeView.False => true
      case TypeView.Pure => true
      case TypeView.Univ => true
      case TypeView.Region(_) => true
      case TypeView.RegionToStar => true
      case TypeView.RegionWithoutRegion => true
      case TypeView.RecordConstructor(_) => true
      case TypeView.Record(_) => true
      case TypeView.RecordExtend(_, _) => true
      case TypeView.RecordRow(_) => true
      case TypeView.RecordRowExtend(_, _) => true
      case TypeView.SchemaConstructor(_) => true
      case TypeView.Schema(_) => true
      case TypeView.SchemaExtend(_, _) => true
      case TypeView.SchemaRow(_) => true
      case TypeView.SchemaRowExtend(_, _) => true
      case TypeView.RelationConstructor => true
      case TypeView.Relation(_) => true
      case TypeView.LatticeConstructor => true
      case TypeView.Lattice(_, _) => true
      case TypeView.TagConstructor(_) => true
      case TypeView.Name(_) => true
      case TypeView.Apply(_, _) => true
      case TypeView.Var(_, _, _) => true
      case TypeView.Tuple(_) => true
      case TypeView.JvmToType(_) => true
      case TypeView.JvmToEff(_) => true
      case TypeView.JvmUnresolvedConstructor(_, _) => true
      case TypeView.JvmUnresolvedField(_, _) => true
      case TypeView.JvmUnresolvedMethod(_, _, _) => true
      case TypeView.JvmUnresolvedStaticMethod(_, _, _) => true
      case TypeView.JvmConstructor(_) => true
      case TypeView.JvmField(_) => true
      case TypeView.JvmMethod(_) => true
      case TypeView.Union(_) => true
      case TypeView.Error => true
    }

    /**
      * Delimits the given `tpe`, parenthesizing it if needed.
      */
    def delimit(tpe: TypeView, mode: Mode): String = {
      if (isDelimited(tpe)) {
        visit(tpe, mode)
      } else {
        parenthesize(visit(tpe, mode))
      }
    }

    /**
      * Converts the given `tpe0` to a string.
      */
    def visit(tpe0: TypeView, mode: Mode): String = tpe0 match {
      case TypeView.Hole => "?"
      case TypeView.Void => "Void"
      case TypeView.AnyType => "AnyType"
      case TypeView.Unit => "Unit"
      case TypeView.Null => "Null"
      case TypeView.Bool => "Bool"
      case TypeView.Char => "Char"
      case TypeView.Float32 => "Float32"
      case TypeView.Float64 => "Float64"
      case TypeView.BigDecimal => "BigDecimal"
      case TypeView.Int8 => "Int8"
      case TypeView.Int16 => "Int16"
      case TypeView.Int32 => "Int32"
      case TypeView.Int64 => "Int64"
      case TypeView.BigInt => "BigInt"
      case TypeView.Str => "String"
      case TypeView.Regex => "Regex"
      case TypeView.Array => "Array"
      case TypeView.ArrayWithoutRegion => "ArrayWithoutRegion"
      case TypeView.Vector => "Vector"
      case TypeView.Sender => "Sender"
      case TypeView.Receiver => "Receiver"
      case TypeView.Lazy => "Lazy"
      case TypeView.False => "false"
      case TypeView.True => "true"
      case TypeView.Pure => mode match {
        case Mode.Type => "Pure"
        case Mode.Purity => "{}"
      }
      case TypeView.Univ => "Univ"
      case TypeView.Region(name) => name
      case TypeView.RegionToStar => "Region"
      case TypeView.RegionWithoutRegion => "RegionWithoutRegion"
      case TypeView.Record(labels) =>
        val labelString = labels.map(visitRecordLabelType).mkString(", ")
        s"{ $labelString }"
      case TypeView.RecordExtend(labels, rest) =>
        val labelString = labels.map(visitRecordLabelType).mkString(", ")
        val restString = visit(rest, mode)
        s"{ $labelString | $restString }"
      case TypeView.RecordRow(labels) =>
        val labelString = labels.map(visitRecordLabelType).mkString(", ")
        s"( $labelString )"
      case TypeView.RecordRowExtend(labels, rest) =>
        val labelString = labels.map(visitRecordLabelType).mkString(", ")
        val restString = visit(rest, Mode.Type)
        s"( $labelString | $restString )"
      case TypeView.RecordConstructor(arg) => s"{ ${visit(arg, Mode.Type)} }"
      case TypeView.Schema(fields) =>
        val fieldString = fields.map(visitSchemaFieldType).mkString(", ")
        s"#{ $fieldString }"
      case TypeView.SchemaExtend(fields, rest) =>
        val fieldString = fields.map(visitSchemaFieldType).mkString(", ")
        val restString = visit(rest, Mode.Type)
        s"#{ $fieldString | $restString }"
      case TypeView.SchemaRow(fields) =>
        val fieldString = fields.map(visitSchemaFieldType).mkString(", ")
        s"#( $fieldString )"
      case TypeView.SchemaRowExtend(fields, rest) =>
        val fieldString = fields.map(visitSchemaFieldType).mkString(", ")
        val restString = visit(rest, Mode.Type)
        s"#( $fieldString | $restString )"
      case TypeView.SchemaConstructor(arg) => s"#{ ${visit(arg, Mode.Type)} }"
      case TypeView.Not(tpe) => s"not ${delimit(tpe, mode)}"
      case TypeView.And(tpes) =>
        val strings = tpes.map(delimit(_, mode))
        strings.mkString(" and ")
      case TypeView.Or(tpes) =>
        val strings = tpes.map(delimit(_, mode))
        strings.mkString(" or ")
      case TypeView.Complement(tpe) => s"~${delimit(tpe, mode)}"
      case TypeView.Union(tpes) =>
        val strings = tpes.map(visit(_, mode))
        strings.mkString("{", ", ", "}")
      case TypeView.Plus(tpes) =>
        val strings = tpes.map(delimit(_, mode))
        strings.mkString(" + ")
      case TypeView.Intersection(tpes) =>
        val strings = tpes.map(delimit(_, mode))
        strings.mkString(" & ")
      case TypeView.SymmetricDiff(tpes) =>
        val strings = tpes.map(delimit(_, mode))
        strings.mkString(" ⊕ ")
      case TypeView.Difference(tpes) =>
        val strings = tpes.map(delimit(_, mode))
        strings.mkString(" - ")
      case TypeView.RelationConstructor => "Relation"
      case TypeView.Relation(tpes) =>
        val terms = tpes.map(visit(_, Mode.Type)).mkString(", ")
        s"Relation($terms)"
      case TypeView.LatticeConstructor => "Lattice"
      case TypeView.Lattice(tpes0, lat0) =>
        val lat = visit(lat0, Mode.Type)
        val tpes = tpes0.map(visit(_, Mode.Type)).mkString(", ")
        s"Lattice($tpes; $lat)"
      case TypeView.PureArrow(arg, ret) =>
        val argString = delimitFunctionArg(arg)
        val retString = delimit(ret, Mode.Type)
        s"$argString -> $retString"
      case TypeView.PolyArrow(arg, eff, ret) =>
        val argString = delimitFunctionArg(arg)
        val effString = visit(eff, Mode.Purity)
        val retString = delimit(ret, Mode.Type)
        s"$argString -> $retString \\ $effString"
      case TypeView.ArrowWithoutEffect(arg, ret) =>
        val argString = delimitFunctionArg(arg)
        val retString = delimit(ret, Mode.Type)
        s"$argString --> $retString"
      case TypeView.TagConstructor(name) => name
      case TypeView.Name(name) => name
      case TypeView.Apply(tpe, tpes) =>
        val string = visit(tpe, Mode.Type)
        val strings = tpes.map(visit(_, Mode.Type))
        string + strings.mkString("[", ", ", "]")
      case TypeView.Var(id, kind, text) =>
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

      case TypeView.Tuple(elms) =>
        elms.map(visit(_, Mode.Type)).mkString("(", ", ", ")")

      case TypeView.JvmToType(tpe) =>
        val arg = visit(tpe, Mode.Type)
        "JvmToType(" + arg + ")"

      case TypeView.JvmToEff(tpe) =>
        val arg = visit(tpe, Mode.Type)
        "JvmToEff(" + arg + ")"

      case TypeView.JvmUnresolvedConstructor(name, tpes0) =>
        val tpes = tpes0.map(visit(_, Mode.Type))
        "JvmUnresolvedConstructor(" + name + ", " + tpes.mkString(", ") + ")"

      case TypeView.JvmUnresolvedField(t0, name) =>
        val t = visit(t0, Mode.Type)
        "JvmUnresolvedField(" + t + ", " + name + ")"

      case TypeView.JvmUnresolvedMethod(t0, name, ts0) =>
        val t = visit(t0, Mode.Type)
        val ts = ts0.map(visit(_, Mode.Type))
        "JvmUnresolvedMethod(" + t + ", " + name + ", " + ts.mkString(", ") + ")"

      case TypeView.JvmUnresolvedStaticMethod(clazz, name, ts0) =>
        val ts = ts0.map(visit(_, Mode.Type))
        "JvmUnresolvedStaticMethod(" + clazz + ", " + name + ", " + ts.mkString(", ") + ")"

      case TypeView.JvmConstructor(constructor) =>
        s"JvmConstructor($constructor)"

      case TypeView.JvmField(field) =>
        s"JvmField($field)"

      case TypeView.JvmMethod(method) =>
        s"JvmMethod($method)"

      case TypeView.Error => "Error"

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

/*
 * Copyright 2026 Magnus Madsen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.language.fmt

import ca.uwaterloo.flix.api.lsp.SemanticTokenType
import ca.uwaterloo.flix.language.ast.shared.{QualifiedSym, RegionScope, VarText}
import ca.uwaterloo.flix.language.ast.{Kind, RigidityEnv, Symbol, Type}
import ca.uwaterloo.flix.language.fmt.Fmt.{SymUse, ef, kw, op, parens, prop, sep, text, tparam, ty}
import ca.uwaterloo.flix.language.jvm.ClassDescs
import ca.uwaterloo.flix.language.phase.TypeSimplifier
import ca.uwaterloo.flix.language.phase.unification.Substitution

/**
  * The printer: builds a [[Fmt]] tree from a type.
  *
  * A type is normalized by [[DisplayType]] first, which curries arrows, collects record and schema
  * rows, flattens operator chains and fills partial applications with holes. The walk from the view
  * to the tree puts a node where a string would be: `Int32` is a type token, `List` is a use of the
  * symbol of its enum, and ` -> ` is an operator token with spacing around it.
  *
  * Every entry point takes the [[FormatOptions]] implicitly.
  */
object Pretty {

  /**
    * Returns the given well-kinded type `t` as a tree.
    *
    * Alpha-renames the flexible variables of `t` if the rigidity environment `renv` is present, and
    * simplifies its effect formulas if `simplify` is set. Both are passes over the type that run
    * before the view is built.
    */
  def tpe(t: Type, renv: Option[RigidityEnv] = None, simplify: Boolean = false)(implicit o: FormatOptions): Fmt = {
    val renamed = renv match {
      case None => t
      case Some(env) => alphaRename(t, env)
    }
    val simplified = if (simplify) TypeSimplifier.simplify(renamed) else renamed
    displayType(DisplayType.fromWellKindedType(simplified))
  }

  /**
    * Returns the given effect `t` as a tree, as it is written in effect position: the pure effect
    * prints as `{}`.
    */
  def eff(t: Type)(implicit o: FormatOptions): Fmt =
    visit(DisplayType.fromWellKindedType(t), effPos = true)

  /**
    * Returns ` \ ef` for the given effect `t`, or [[Fmt.Empty]] if it is pure.
    */
  def effSuffix(t: Type)(implicit o: FormatOptions): Fmt = DisplayType.fromWellKindedType(t) match {
    case DisplayType.Pure => Fmt.Empty
    case d => text(" ") ++ op("\\") ++ text(" ") ++ visit(d, effPos = true)
  }

  /**
    * Returns `t \ e` for the given type `t` and effect `e`, or just `t` if `e` is pure.
    *
    * Simplifies the effect formulas of both if `simplify` is set.
    */
  def typeAndEff(t: Type, e: Type, simplify: Boolean = false)(implicit o: FormatOptions): Fmt = {
    val e1 = if (simplify) TypeSimplifier.simplify(e) else e
    tpe(t, simplify = simplify) ++ effSuffix(e1)
  }

  /**
    * Returns the given type variable `sym` as a tree.
    */
  def typeVar(sym: Symbol.KindedTypeVarSym)(implicit o: FormatOptions): Fmt = tparam(varName(sym))

  /**
    * Returns the given type view `d` as a tree.
    */
  def displayType(d: DisplayType)(implicit o: FormatOptions): Fmt = visit(d, effPos = false)

  /**
    * Renames all flexible variables in the given `tpe` to fresh consecutively numbered variables.
    *
    * NOTE: This function should *ONLY* be used for pretty printing.
    */
  private def alphaRename(tpe: Type, renv: RigidityEnv): Type = {
    // Compute the free type variables.
    val freeVars = tpe.typeVars.toList.sortBy(_.sym.id)

    // Compute the flexible variables (i.e. the free variables that are not rigid).
    val flexibleVars = renv.getFlexibleVarsOf(freeVars)(RegionScope.Top) // TODO LEVELS ideally we should have a proper scope here

    // Compute a substitution that maps the first flexible variable to id 1 and so forth.
    val m = flexibleVars.zipWithIndex.map {
      case (Type.Var(sym, loc), index) =>
        sym -> (Type.Var(new Symbol.KindedTypeVarSym(index, sym.text, sym.kind, sym.scope, loc), loc): Type)
    }
    val s = Substitution(m.toMap)

    // Apply the substitution to the type.
    s(tpe)
  }

  /**
    * Returns the given type view `d` as a tree.
    *
    * `effPos` is set in effect position, where the pure effect prints as `{}` rather than `Pure`.
    */
  private def visit(d: DisplayType, effPos: Boolean)(implicit o: FormatOptions): Fmt = d match {
    case DisplayType.Hole => text("?")
    case DisplayType.Void => ty("Void")
    case DisplayType.AnyType => ty("AnyType")
    case DisplayType.Unit => ty("Unit")
    case DisplayType.Null => ty("Null")
    case DisplayType.Bool => ty("Bool")
    case DisplayType.Char => ty("Char")
    case DisplayType.Float32 => ty("Float32")
    case DisplayType.Float64 => ty("Float64")
    case DisplayType.BigDecimal => ty("BigDecimal")
    case DisplayType.Int8 => ty("Int8")
    case DisplayType.Int16 => ty("Int16")
    case DisplayType.Int32 => ty("Int32")
    case DisplayType.Int64 => ty("Int64")
    case DisplayType.BigInt => ty("BigInt")
    case DisplayType.Str => ty("String")
    case DisplayType.Regex => ty("Regex")
    case DisplayType.Array => ty("Array")
    case DisplayType.ArrayWithoutRegion => ty("ArrayWithoutRegion")
    case DisplayType.Vector => ty("Vector")
    case DisplayType.Sender => ty("Sender")
    case DisplayType.Receiver => ty("Receiver")
    case DisplayType.Lazy => ty("Lazy")
    case DisplayType.False => ty("false")
    case DisplayType.True => ty("true")
    case DisplayType.Pure => if (effPos) ef("{}") else ef("Pure")
    case DisplayType.Univ => ef("Univ")
    case DisplayType.Region(sym) => tparam(sym.text)
    case DisplayType.RegionToStar => ty("Region")
    case DisplayType.RegionWithoutRegion => ty("RegionWithoutRegion")
    case DisplayType.Record(labels) =>
      text("{ ") ++ sep(", ", labels.map(label)) ++ text(" }")
    case DisplayType.RecordExtend(labels, rest) =>
      text("{ ") ++ sep(", ", labels.map(label)) ++ text(" | ") ++ visit(rest, effPos) ++ text(" }")
    case DisplayType.RecordRow(labels) =>
      text("( ") ++ sep(", ", labels.map(label)) ++ text(" )")
    case DisplayType.RecordRowExtend(labels, rest) =>
      text("( ") ++ sep(", ", labels.map(label)) ++ text(" | ") ++ visit(rest, effPos = false) ++ text(" )")
    case DisplayType.RecordConstructor(arg) =>
      text("{ ") ++ visit(arg, effPos = false) ++ text(" }")
    case DisplayType.Schema(fields) =>
      text("#{ ") ++ sep(", ", fields.map(field)) ++ text(" }")
    case DisplayType.SchemaExtend(fields, rest) =>
      text("#{ ") ++ sep(", ", fields.map(field)) ++ text(" | ") ++ visit(rest, effPos = false) ++ text(" }")
    case DisplayType.SchemaRow(fields) =>
      text("#( ") ++ sep(", ", fields.map(field)) ++ text(" )")
    case DisplayType.SchemaRowExtend(fields, rest) =>
      text("#( ") ++ sep(", ", fields.map(field)) ++ text(" | ") ++ visit(rest, effPos = false) ++ text(" )")
    case DisplayType.SchemaConstructor(arg) =>
      text("#{ ") ++ visit(arg, effPos = false) ++ text(" }")
    case DisplayType.Extensible(fields) =>
      text("#| ") ++ sep(", ", fields.map(field)) ++ text(" |#")
    case DisplayType.ExtensibleExtend(fields, rest) =>
      text("#| ") ++ sep(", ", fields.map(field)) ++ text(" | ") ++ visit(rest, effPos = false) ++ text(" |#")
    case DisplayType.ExtensibleUnknown(arg) =>
      text("#| ") ++ visit(arg, effPos = false) ++ text(" |#")
    case DisplayType.Not(tpe) => kw("not") ++ text(" ") ++ delimit(tpe, effPos)
    case DisplayType.And(tpes) => sep(text(" ") ++ kw("and") ++ text(" "), tpes.map(delimit(_, effPos)))
    case DisplayType.Or(tpes) => sep(text(" ") ++ kw("or") ++ text(" "), tpes.map(delimit(_, effPos)))
    case DisplayType.Complement(tpe) => op("~") ++ delimit(tpe, effPos)
    case DisplayType.Union(tpes) => text("{") ++ sep(", ", tpes.map(visit(_, effPos))) ++ text("}")
    case DisplayType.Plus(tpes) => sep(text(" ") ++ op("+") ++ text(" "), tpes.map(delimit(_, effPos)))
    case DisplayType.Intersection(tpes) => sep(text(" ") ++ op("&") ++ text(" "), tpes.map(delimit(_, effPos)))
    case DisplayType.SymmetricDiff(tpes) => sep(text(" ") ++ op("⊕") ++ text(" "), tpes.map(delimit(_, effPos)))
    case DisplayType.Difference(tpes) => sep(text(" ") ++ op("-") ++ text(" "), tpes.map(delimit(_, effPos)))
    case DisplayType.RelationConstructor => ty("Relation")
    case DisplayType.Relation(tpes) =>
      ty("Relation") ++ parens(sep(", ", tpes.map(visit(_, effPos = false))))
    case DisplayType.LatticeConstructor => ty("Lattice")
    case DisplayType.Lattice(tpes, lat) =>
      ty("Lattice") ++ parens(sep(", ", tpes.map(visit(_, effPos = false))) ++ text("; ") ++ visit(lat, effPos = false))
    case DisplayType.PureArrow(arg, ret) =>
      delimitFunctionArg(arg) ++ text(" ") ++ op("->") ++ text(" ") ++ delimit(ret, effPos = false)
    case DisplayType.PolyArrow(arg, eff, ret) =>
      delimitFunctionArg(arg) ++ text(" ") ++ op("->") ++ text(" ") ++ delimit(ret, effPos = false) ++ text(" ") ++ op("\\") ++ text(" ") ++ visit(eff, effPos = true)
    case DisplayType.ArrowWithoutEffect(arg, ret) =>
      delimitFunctionArg(arg) ++ text(" ") ++ op("-->") ++ text(" ") ++ delimit(ret, effPos = false)
    case DisplayType.Enum(sym) => SymUse(sym, SemanticTokenType.Enum, qualify(sym))
    case DisplayType.Struct(sym) => SymUse(sym, SemanticTokenType.Struct, qualify(sym))
    case DisplayType.RestrictableEnum(sym) => SymUse(sym, SemanticTokenType.Enum, qualify(sym))
    case DisplayType.Case(sym) => SymUse(sym, SemanticTokenType.EnumMember, sym.name)
    case DisplayType.Alias(sym) => SymUse(sym, SemanticTokenType.Type, qualify(sym))
    case DisplayType.Assoc(sym) => SymUse(sym, SemanticTokenType.Type, sym.name)
    case DisplayType.Effect(sym) => SymUse(sym, SemanticTokenType.Effect, sym.name)
    case DisplayType.Native(desc) => ty(ClassDescs.canonicalNameOf(desc))
    case DisplayType.Apply(tpe, tpes) =>
      delimit(tpe, effPos = false) ++ text("[") ++ sep(", ", tpes.map(visit(_, effPos = false))) ++ text("]")
    case DisplayType.Var(sym) => typeVar(sym)
    case DisplayType.Tuple(elms) =>
      parens(sep(", ", elms.map(visit(_, effPos = false))))
    case DisplayType.JvmToType(tpe) =>
      ty("JvmToType") ++ parens(visit(tpe, effPos = false))
    case DisplayType.JvmToEff(tpe) =>
      ty("JvmToEff") ++ parens(visit(tpe, effPos = false))
    case DisplayType.JvmUnresolvedConstructor(name, tpes) =>
      ty("JvmUnresolvedConstructor") ++ parens(ty(name) ++ text(", ") ++ sep(", ", tpes.map(visit(_, effPos = false))))
    case DisplayType.JvmUnresolvedField(tpe, name) =>
      ty("JvmUnresolvedField") ++ parens(visit(tpe, effPos = false) ++ text(", ") ++ text(name))
    case DisplayType.JvmUnresolvedMethod(tpe, name, tpes) =>
      ty("JvmUnresolvedMethod") ++ parens(visit(tpe, effPos = false) ++ text(", ") ++ text(name) ++ text(", ") ++ sep(", ", tpes.map(visit(_, effPos = false))))
    case DisplayType.JvmUnresolvedStaticMethod(clazz, name, tpes) =>
      ty("JvmUnresolvedStaticMethod") ++ parens(ty(clazz) ++ text(", ") ++ text(name) ++ text(", ") ++ sep(", ", tpes.map(visit(_, effPos = false))))
    case DisplayType.JvmConstructor(constructor) =>
      ty("JvmConstructor") ++ parens(text(constructor.toString))
    case DisplayType.JvmField(fld) =>
      ty("JvmField") ++ parens(text(fld.toString))
    case DisplayType.JvmMethod(method) =>
      ty("JvmMethod") ++ parens(text(method.toString))
    case DisplayType.Error => ty("Error")
  }

  /**
    * Returns the given record `labelType` pair as a tree, e.g. `x = Int32`.
    */
  private def label(labelType: DisplayType.RecordLabelType)(implicit o: FormatOptions): Fmt = labelType match {
    case DisplayType.RecordLabelType(name, tpe) => prop(name) ++ text(" = ") ++ visit(tpe, effPos = false)
  }

  /**
    * Returns the given schema `fieldType` pair as a tree, e.g. `P(Int32, String)`.
    */
  private def field(fieldType: DisplayType.PredicateFieldType)(implicit o: FormatOptions): Fmt = fieldType match {
    case DisplayType.RelationFieldType(name, tpes) =>
      prop(name) ++ parens(sep(", ", tpes.map(visit(_, effPos = false))))
    case DisplayType.LatticeFieldType(name, tpes, lat) =>
      prop(name) ++ parens(sep(", ", tpes.map(visit(_, effPos = false))) ++ text("; ") ++ visit(lat, effPos = false))
    case DisplayType.NonPredFieldType(name, tpe) =>
      prop(name) ++ parens(text("<") ++ visit(tpe, effPos = false) ++ text(">"))
  }

  /**
    * Returns the given type view `d` as a tree, parenthesized if it is an operator, so that it can
    * stand as an operand or in an argument position.
    */
  private def delimit(d: DisplayType, effPos: Boolean)(implicit o: FormatOptions): Fmt = d match {
    case _: DisplayType.Operator => parens(visit(d, effPos))
    case _ => visit(d, effPos)
  }

  /**
    * Returns the given type view `arg` as a tree, delimited as a function argument: a tuple gets an
    * extra set of parentheses, so that `((a, b)) -> c` is told from `(a, b) -> c`.
    */
  private def delimitFunctionArg(arg: DisplayType)(implicit o: FormatOptions): Fmt = arg match {
    case tuple: DisplayType.Tuple => parens(visit(tuple, effPos = false))
    case d => delimit(d, effPos = false)
  }

  /**
    * Returns the name of the given type variable `sym`: its source text under
    * [[FormatOptions.VarName.NameBased]] when it has one, and otherwise its id with a prefix for its
    * kind, e.g. `t12`, `e3`, `r7`.
    */
  private def varName(sym: Symbol.KindedTypeVarSym)(implicit o: FormatOptions): String = {
    val idBased: String = sym.kind match {
      case Kind.Wild => "_" + sym.id.toString
      case Kind.WildCaseSet => "_c" + sym.id.toString
      case Kind.Star => "t" + sym.id
      case Kind.Eff => "e" + sym.id
      case Kind.Bool => "b" + sym.id
      case Kind.RecordRow => "r" + sym.id
      case Kind.SchemaRow => "s" + sym.id
      case Kind.Predicate => "'" + sym.id.toString
      case Kind.Jvm => "j" + sym.id.toString
      case Kind.CaseSet(_) => "c" + sym.id.toString
      case Kind.Arrow(_, _) => "'" + sym.id.toString
      case Kind.Error => "err" + sym.id.toString
    }
    o.varNames match {
      case FormatOptions.VarName.IdBased => idBased
      case FormatOptions.VarName.NameBased => sym.text match {
        case VarText.Absent => idBased
        case VarText.SourceText(s) => s
      }
    }
  }

  /**
    * Returns the name of the given symbol `sym`, qualified by its namespace if the options say so.
    */
  private def qualify(sym: QualifiedSym)(implicit o: FormatOptions): String = o.qualify.qualify(sym)

}

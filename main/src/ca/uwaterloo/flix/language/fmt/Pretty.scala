/*
 * Copyright 2026 Magnus Madsen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.language.fmt

import ca.uwaterloo.flix.api.lsp.SemanticTokenType
import ca.uwaterloo.flix.language.ast.shared.{Constant, EqualityConstraint, QualifiedSym, RegionScope, TraitConstraint, VarText}
import ca.uwaterloo.flix.language.ast.{Kind, RigidityEnv, Scheme, Symbol, Type, TypeConstructor, TypedAst}
import ca.uwaterloo.flix.language.fmt.Fmt.{SymUse, Token, ef, kw, op, parens, prop, sep, text, tparam, ty}
import ca.uwaterloo.flix.language.jvm.ClassDescs
import ca.uwaterloo.flix.language.phase.TypeSimplifier
import ca.uwaterloo.flix.language.phase.unification.Substitution
import ca.uwaterloo.flix.util.collection.Nel

/**
  * The printer: builds a [[Fmt]] tree from a type, a type constructor, a scheme, a constraint, a
  * signature, the head of a declaration, a kind or a constant.
  *
  * A type is normalized by [[DisplayType]] first, which curries arrows, collects record and schema
  * rows, flattens operator chains and fills partial applications with holes. The walk from the view
  * to the tree puts a node where a string would be: `Int32` is a type token, `List` is a use of the
  * symbol of its enum, and ` -> ` is an operator token with spacing around it.
  *
  * Every entry point that prints a type takes the [[FormatOptions]] implicitly.
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
    * Returns `tpe \ eff` for the given type `tpe` and effect `eff`, or just `tpe` if `eff` is pure.
    *
    * Simplifies the effect formulas of both if `simplify` is set.
    */
  def typeAndEff(tpe: Type, eff: Type, simplify: Boolean = false)(implicit o: FormatOptions): Fmt = {
    val simplifiedEff = if (simplify) TypeSimplifier.simplify(eff) else eff
    val effFmt = DisplayType.fromWellKindedType(simplifiedEff) match {
      case DisplayType.Pure => Fmt.Empty
      case d => text(" ") ++ op("\\") ++ text(" ") ++ visit(d)
    }
    Pretty.tpe(tpe, simplify = simplify) ++ effFmt
  }

  /**
    * Returns the given type variable `sym` as a tree.
    */
  def typeVar(sym: Symbol.KindedTypeVarSym)(implicit o: FormatOptions): Fmt = tparam(varName(sym))

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
    * Returns the given type view `d` as a tree.
    */
  def displayType(d: DisplayType)(implicit o: FormatOptions): Fmt = visit(d)

  /**
    * Returns the given type constructor `tc` as a tree, written by its name alone, e.g. `Arrow2`.
    *
    * A constructor that stands for a declaration is a use of its symbol.
    */
  def tycon(tc: TypeConstructor): Fmt = tc match {
    // Primitives
    case TypeConstructor.Void => ty("Void")
    case TypeConstructor.AnyType => ty("AnyType")
    case TypeConstructor.Unit => ty("Unit")
    case TypeConstructor.Null => ty("Null")
    case TypeConstructor.Bool => ty("Bool")
    case TypeConstructor.Char => ty("Char")
    case TypeConstructor.Float32 => ty("Float32")
    case TypeConstructor.Float64 => ty("Float64")
    case TypeConstructor.BigDecimal => ty("BigDecimal")
    case TypeConstructor.Int8 => ty("Int8")
    case TypeConstructor.Int16 => ty("Int16")
    case TypeConstructor.Int32 => ty("Int32")
    case TypeConstructor.Int64 => ty("Int64")
    case TypeConstructor.BigInt => ty("BigInt")
    case TypeConstructor.Str => ty("String")
    case TypeConstructor.Regex => ty("Regex")

    // Arrows
    case TypeConstructor.Arrow(arity) => ty(s"Arrow$arity")
    case TypeConstructor.ArrowWithoutEffect(arity) => ty(s"Arrow$arity")

    // Records
    case TypeConstructor.RecordRowEmpty => ty("RecordRowEmpty")
    case TypeConstructor.RecordRowExtend(label) => ty("RecordRowExtend") ++ parens(prop(label.name))
    case TypeConstructor.Record => ty("Record")

    // Schemas
    case TypeConstructor.Extensible => ty("Extensible")
    case TypeConstructor.SchemaRowEmpty => ty("SchemaRowEmpty")
    case TypeConstructor.SchemaRowExtend(pred) => ty("SchemaRowExtend") ++ parens(prop(pred.name))
    case TypeConstructor.Schema => ty("Schema")

    // Collections
    case TypeConstructor.Array => ty("Array")
    case TypeConstructor.ArrayWithoutRegion => ty("Array")
    case TypeConstructor.Vector => ty("Vector")
    case TypeConstructor.Sender => ty("Sender")
    case TypeConstructor.Receiver => ty("Receiver")
    case TypeConstructor.Lazy => ty("Lazy")

    // User-defined types
    case TypeConstructor.Enum(sym, _) => SymUse(sym, SemanticTokenType.Enum, sym.name)
    case TypeConstructor.Struct(sym, _) => SymUse(sym, SemanticTokenType.Struct, sym.name)
    case TypeConstructor.RestrictableEnum(sym, _) => SymUse(sym, SemanticTokenType.Enum, sym.name)

    // JVM types
    case TypeConstructor.Native(desc, _) => ty(ClassDescs.binaryNameOf(desc))
    case TypeConstructor.JvmConstructor(constructor) => ty("Constructor") ++ parens(ty(ClassDescs.binaryNameOf(constructor.ref.owner)))
    case TypeConstructor.JvmMethod(method, _) => ty("Method") ++ parens(text(method.ref.name))
    case TypeConstructor.JvmField(field) => ty("Field") ++ parens(text(field.ref.name))

    // Tuples and relations
    case TypeConstructor.Tuple(arity) => ty(s"Tuple$arity")
    case TypeConstructor.Relation(arity) => ty(s"Relation$arity")
    case TypeConstructor.Lattice(arity) => ty(s"Lattice$arity")

    // Effects
    case TypeConstructor.Pure => ef("Pure")
    case TypeConstructor.Univ => ef("Univ")
    case TypeConstructor.Effect(sym, _) => SymUse(sym, SemanticTokenType.Effect, sym.name)

    // Boolean formulas
    case TypeConstructor.True => ty("True")
    case TypeConstructor.False => ty("False")
    case TypeConstructor.And => ty("And")
    case TypeConstructor.Or => ty("Or")
    case TypeConstructor.Not => ty("Not")

    // Set operations
    case TypeConstructor.Complement => ty("Complement")
    case TypeConstructor.Union => ty("Union")
    case TypeConstructor.Intersection => ty("Intersection")
    case TypeConstructor.Difference => ty("Difference")
    case TypeConstructor.SymmetricDiff => ty("SymmetricDiff")

    // Case set operations
    case TypeConstructor.CaseSet(syms, _) => ty("CaseSet") ++ parens(sep(", ", syms.toList.map(sym => SymUse(sym, SemanticTokenType.EnumMember, sym.name))))
    case TypeConstructor.CaseComplement(_) => ty("CaseComplement")
    case TypeConstructor.CaseIntersection(_) => ty("CaseIntersection")
    case TypeConstructor.CaseUnion(_) => ty("CaseUnion")
    case TypeConstructor.CaseSymmetricDiff(_) => ty("CaseSymmetricDiff")

    // Regions
    case TypeConstructor.Region(sym) => ty("Region") ++ parens(tparam(sym.text))
    case TypeConstructor.RegionToStar => ty("RegionToStar")
    case TypeConstructor.RegionWithoutRegion => ty("Region")

    // Error
    case TypeConstructor.Error(_, _) => ty("Error")
  }

  /**
    * Returns the given scheme `sc` as a tree, e.g.
    * `∀(a, b). a -> b with Order[a] where Collection.Elm[a] ~ b`.
    */
  def scheme(sc: Scheme)(implicit o: FormatOptions): Fmt = {
    val quantifiers =
      if (sc.quantifiers.isEmpty) Fmt.Empty
      else text("∀(") ++ sep(", ", sc.quantifiers.map(typeVar)) ++ text("). ")
    quantifiers ++ tpe(sc.base) ++ constraints(sc.tconstrs, sc.econstrs)
  }

  /**
    * Returns the given trait constraint `tc` as a tree, e.g. `Order[a]`.
    */
  def traitConstraint(tc: TraitConstraint)(implicit o: FormatOptions): Fmt = tc match {
    case TraitConstraint(head, arg, _) =>
      SymUse(head.sym, SemanticTokenType.Interface, qualify(head.sym)) ++ text("[") ++ tpe(arg) ++ text("]")
  }

  /**
    * Returns the given equality constraint `ec` as a tree, e.g. `Collection.Elm[a] ~ Int32`.
    */
  def equalityConstraint(ec: EqualityConstraint)(implicit o: FormatOptions): Fmt = ec match {
    case EqualityConstraint(symUse, tpe1, tpe2, _) =>
      assocType(symUse.sym) ++ text("[") ++ tpe(tpe1) ++ text("] ") ++ op("~") ++ text(" ") ++ tpe(tpe2)
  }

  /**
    * Returns the given trait constraints `tcs` and equality constraints `ecs` as a tree, each list
    * after its keyword, e.g. ` with Order[a] where Collection.Elm[a] ~ Int32`, or [[Fmt.Empty]] if
    * both are empty.
    */
  def constraints(tcs: List[TraitConstraint], ecs: List[EqualityConstraint])(implicit o: FormatOptions): Fmt = {
    val tcPart =
      if (tcs.isEmpty) Fmt.Empty
      else text(" ") ++ kw("with") ++ text(" ") ++ sep(", ", tcs.map(traitConstraint))
    val ecPart =
      if (ecs.isEmpty) Fmt.Empty
      else text(" ") ++ kw("where") ++ text(" ") ++ sep(", ", ecs.map(equalityConstraint))
    tcPart ++ ecPart
  }

  /**
    * Returns the signature of the def, sig or op `sym` with the given `spec` as a tree, written as
    * the declaration would be, e.g. `def sort(l: List[a]): List[a] with Order[a]`.
    *
    * The name is a use of `sym`. A symbol of any other kind is written as a function name that
    * stands for no declaration.
    *
    * The constraints are taken from the declared scheme: `Spec` holds the equality constraints as
    * `TypedAst.EqualityConstraint`, which does not name the associated type, so there is not enough
    * in one to write it out.
    */
  def spec(sym: QualifiedSym, spec: TypedAst.Spec)(implicit o: FormatOptions): Fmt = {
    val name = sym match {
      case s: Symbol.DefnSym => SymUse(s, SemanticTokenType.Function, s.name)
      case s: Symbol.SigSym => SymUse(s, SemanticTokenType.Method, s.name)
      case s: Symbol.OpSym => SymUse(s, SemanticTokenType.Function, s.name)
      case s => Token(SemanticTokenType.Function, s.name)
    }
    val sc = spec.declaredScheme
    kw("def") ++ text(" ") ++ name ++ fparams(spec.fparams) ++ text(": ") ++ typeAndEff(spec.retTpe, spec.eff) ++ constraints(sc.tconstrs, sc.econstrs)
  }

  /**
    * Returns the given formal parameters `fparams0` as a tree, in parentheses, e.g.
    * `(x: Int32, y: a)`.
    *
    * A function declared with no parameters gets a single parameter of the unit type, which is
    * written as it was declared: `()`.
    */
  def fparams(fparams0: Nel[TypedAst.FormalParam])(implicit o: FormatOptions): Fmt = fparams0 match {
    case Nel(fparam, Nil) if fparam.tpe == Type.Unit => text("()")
    case _ =>
      val params = fparams0.toList.map {
        case TypedAst.FormalParam(bnd, tpe, _, _, _) =>
          Token(SemanticTokenType.Parameter, bnd.sym.text) ++ text(": ") ++ Pretty.tpe(tpe)
      }
      parens(sep(", ", params))
  }

  /**
    * Returns the given type parameters `tparams0` as a tree, in brackets and each with its kind,
    * e.g. `[a: Type, ef: Eff]`, or [[Fmt.Empty]] if there are none.
    */
  def tparams(tparams0: List[TypedAst.TypeParam])(implicit o: FormatOptions): Fmt = tparams0 match {
    case Nil => Fmt.Empty
    case _ =>
      val params = tparams0.map {
        case TypedAst.TypeParam(_, sym, _) => typeVar(sym) ++ text(": ") ++ kind(sym.kind)
      }
      text("[") ++ sep(", ", params) ++ text("]")
  }

  /**
    * Returns the given case `caze` of an enum as a tree, written as it is declared, e.g.
    * `case Some(a)`.
    */
  def caseDecl(caze: TypedAst.Case)(implicit o: FormatOptions): Fmt = {
    val args = caze.tpes match {
      case Nil => Fmt.Empty
      case tpes => parens(sep(", ", tpes.map(t => tpe(t))))
    }
    kw("case") ++ text(" ") ++ SymUse(caze.sym, SemanticTokenType.EnumMember, caze.sym.name) ++ args
  }

  /**
    * Returns the given field `field` of a struct as a tree, written as it is declared, e.g.
    * `mut x: Int32`.
    */
  def structField(field: TypedAst.StructField)(implicit o: FormatOptions): Fmt = {
    val mod = if (field.mod.isMutable) kw("mut") ++ text(" ") else Fmt.Empty
    mod ++ SymUse(field.sym, SemanticTokenType.Property, field.sym.name) ++ text(": ") ++ tpe(field.tpe)
  }

  /**
    * Returns the given type alias `alias` as a tree, written as it is declared, e.g.
    * `type alias Pair[a, b] = (a, b)`.
    */
  def typeAliasDecl(alias: TypedAst.TypeAlias)(implicit o: FormatOptions): Fmt =
    kw("type") ++ text(" ") ++ kw("alias") ++ text(" ") ++ SymUse(alias.sym, SemanticTokenType.Type, alias.sym.name) ++ tparams(alias.tparams) ++ text(" = ") ++ tpe(alias.tpe)

  /**
    * Returns the given kind `k` as a tree, e.g. `Type -> Eff`.
    */
  def kind(k: Kind): Fmt = k match {
    case Kind.Wild => text("???")
    case Kind.WildCaseSet => ty("CaseSet") ++ text("[???]")
    case Kind.Star => ty("Type")
    case Kind.Eff => ty("Eff")
    case Kind.Bool => ty("Bool")
    case Kind.RecordRow => ty("RecordRow")
    case Kind.SchemaRow => ty("SchemaRow")
    case Kind.Predicate => ty("Predicate")
    case Kind.Jvm => ty("Jvm")
    case Kind.CaseSet(sym) => ty("CaseSet") ++ text("[") ++ SymUse(sym, SemanticTokenType.Enum, sym.name) ++ text("]")
    // parenthesize the left because `->` is right-associative
    case Kind.Arrow(k1: Kind.Arrow, k2) => parens(kind(k1)) ++ text(" ") ++ op("->") ++ text(" ") ++ kind(k2)
    case Kind.Arrow(k1, k2) => kind(k1) ++ text(" ") ++ op("->") ++ text(" ") ++ kind(k2)
    case Kind.Error => ty("Error")
  }

  /**
    * Returns the given constant `c` as a tree, written as a literal, e.g. `42i32`.
    *
    * A literal has the token type the LSP gives it in source, which for `()`, `null`, the Booleans,
    * `{}` and `Static` is the type token.
    */
  def constant(c: Constant): Fmt = c match {
    case Constant.Unit => ty("()")
    case Constant.Null => ty("null")
    case Constant.Bool(lit) => ty(lit.toString)
    case Constant.Char(lit) => Token(SemanticTokenType.String, "'" + lit.toString + "'")
    case Constant.Float32(lit) => Token(SemanticTokenType.Number, s"${lit}f32")
    case Constant.Float64(lit) => Token(SemanticTokenType.Number, s"${lit}f64")
    case Constant.BigDecimal(lit) => Token(SemanticTokenType.Number, s"${lit}ff")
    case Constant.Int8(lit) => Token(SemanticTokenType.Number, s"${lit}i8")
    case Constant.Int16(lit) => Token(SemanticTokenType.Number, s"${lit}i16")
    case Constant.Int32(lit) => Token(SemanticTokenType.Number, s"${lit}i32")
    case Constant.Int64(lit) => Token(SemanticTokenType.Number, s"${lit}i64")
    case Constant.BigInt(lit) => Token(SemanticTokenType.Number, s"${lit}ii")
    case Constant.Str(lit) => Token(SemanticTokenType.String, "\"" + lit + "\"")
    case Constant.Regex(lit) => Token(SemanticTokenType.Regexp, "regex\"" + lit.pattern() + "\"")
    case Constant.RecordEmpty => ty("{}")
    case Constant.Static => ty("Static")
  }

  /**
    * Returns the given type view `d` as a tree.
    */
  private def visit(d: DisplayType)(implicit o: FormatOptions): Fmt = d match {
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
    case DisplayType.Pure => ef("{}")
    case DisplayType.Univ => ef("Univ")
    case DisplayType.Region(sym) => tparam(sym.text)
    case DisplayType.RegionToStar => ty("Region")
    case DisplayType.RegionWithoutRegion => ty("RegionWithoutRegion")
    case DisplayType.Record(labels) =>
      text("{ ") ++ sep(", ", labels.map(label)) ++ text(" }")
    case DisplayType.RecordExtend(labels, rest) =>
      text("{ ") ++ sep(", ", labels.map(label)) ++ text(" | ") ++ visit(rest) ++ text(" }")
    case DisplayType.RecordRow(labels) =>
      text("( ") ++ sep(", ", labels.map(label)) ++ text(" )")
    case DisplayType.RecordRowExtend(labels, rest) =>
      text("( ") ++ sep(", ", labels.map(label)) ++ text(" | ") ++ visit(rest) ++ text(" )")
    case DisplayType.RecordConstructor(arg) =>
      text("{ ") ++ visit(arg) ++ text(" }")
    case DisplayType.Schema(fields) =>
      text("#{ ") ++ sep(", ", fields.map(field)) ++ text(" }")
    case DisplayType.SchemaExtend(fields, rest) =>
      text("#{ ") ++ sep(", ", fields.map(field)) ++ text(" | ") ++ visit(rest) ++ text(" }")
    case DisplayType.SchemaRow(fields) =>
      text("#( ") ++ sep(", ", fields.map(field)) ++ text(" )")
    case DisplayType.SchemaRowExtend(fields, rest) =>
      text("#( ") ++ sep(", ", fields.map(field)) ++ text(" | ") ++ visit(rest) ++ text(" )")
    case DisplayType.SchemaConstructor(arg) =>
      text("#{ ") ++ visit(arg) ++ text(" }")
    case DisplayType.Extensible(fields) =>
      text("#| ") ++ sep(", ", fields.map(field)) ++ text(" |#")
    case DisplayType.ExtensibleExtend(fields, rest) =>
      text("#| ") ++ sep(", ", fields.map(field)) ++ text(" | ") ++ visit(rest) ++ text(" |#")
    case DisplayType.ExtensibleUnknown(arg) =>
      text("#| ") ++ visit(arg) ++ text(" |#")
    case DisplayType.Not(tpe) => kw("not") ++ text(" ") ++ delimit(tpe)
    case DisplayType.And(tpes) => sep(text(" ") ++ kw("and") ++ text(" "), tpes.map(delimit))
    case DisplayType.Or(tpes) => sep(text(" ") ++ kw("or") ++ text(" "), tpes.map(delimit))
    case DisplayType.Complement(tpe) => op("~") ++ delimit(tpe)
    case DisplayType.Union(tpes) => text("{") ++ sep(", ", tpes.map(visit)) ++ text("}")
    case DisplayType.Plus(tpes) => sep(text(" ") ++ op("+") ++ text(" "), tpes.map(delimit))
    case DisplayType.Intersection(tpes) => sep(text(" ") ++ op("&") ++ text(" "), tpes.map(delimit))
    case DisplayType.SymmetricDiff(tpes) => sep(text(" ") ++ op("⊕") ++ text(" "), tpes.map(delimit))
    case DisplayType.Difference(tpes) => sep(text(" ") ++ op("-") ++ text(" "), tpes.map(delimit))
    case DisplayType.RelationConstructor => ty("Relation")
    case DisplayType.Relation(tpes) =>
      ty("Relation") ++ parens(sep(", ", tpes.map(visit)))
    case DisplayType.LatticeConstructor => ty("Lattice")
    case DisplayType.Lattice(tpes, lat) =>
      ty("Lattice") ++ parens(sep(", ", tpes.map(visit)) ++ text("; ") ++ visit(lat))
    case DisplayType.PureArrow(arg, ret) =>
      delimitFunctionArg(arg) ++ text(" ") ++ op("->") ++ text(" ") ++ delimit(ret)
    case DisplayType.PolyArrow(arg, eff, ret) =>
      delimitFunctionArg(arg) ++ text(" ") ++ op("->") ++ text(" ") ++ delimit(ret) ++ text(" ") ++ op("\\") ++ text(" ") ++ visit(eff)
    case DisplayType.ArrowWithoutEffect(arg, ret) =>
      delimitFunctionArg(arg) ++ text(" ") ++ op("-->") ++ text(" ") ++ delimit(ret)
    case DisplayType.Enum(sym) => SymUse(sym, SemanticTokenType.Enum, qualify(sym))
    case DisplayType.Struct(sym) => SymUse(sym, SemanticTokenType.Struct, qualify(sym))
    case DisplayType.RestrictableEnum(sym) => SymUse(sym, SemanticTokenType.Enum, qualify(sym))
    case DisplayType.Case(sym) => SymUse(sym, SemanticTokenType.EnumMember, sym.name)
    case DisplayType.Alias(sym) => SymUse(sym, SemanticTokenType.Type, qualify(sym))
    case DisplayType.Assoc(sym) => assocType(sym)
    case DisplayType.Effect(sym) => SymUse(sym, SemanticTokenType.Effect, sym.name)
    case DisplayType.Native(desc) => ty(ClassDescs.canonicalNameOf(desc))
    case DisplayType.Apply(tpe, tpes) =>
      delimit(tpe) ++ text("[") ++ sep(", ", tpes.map(visit)) ++ text("]")
    case DisplayType.Var(sym) => typeVar(sym)
    case DisplayType.Tuple(elms) =>
      parens(sep(", ", elms.map(visit)))
    case DisplayType.JvmToType(tpe) =>
      ty("JvmToType") ++ parens(visit(tpe))
    case DisplayType.JvmToEff(tpe) =>
      ty("JvmToEff") ++ parens(visit(tpe))
    case DisplayType.JvmUnresolvedConstructor(name, tpes) =>
      ty("JvmUnresolvedConstructor") ++ parens(ty(name) ++ text(", ") ++ sep(", ", tpes.map(visit)))
    case DisplayType.JvmUnresolvedField(tpe, name) =>
      ty("JvmUnresolvedField") ++ parens(visit(tpe) ++ text(", ") ++ text(name))
    case DisplayType.JvmUnresolvedMethod(tpe, name, tpes) =>
      ty("JvmUnresolvedMethod") ++ parens(visit(tpe) ++ text(", ") ++ text(name) ++ text(", ") ++ sep(", ", tpes.map(visit)))
    case DisplayType.JvmUnresolvedStaticMethod(clazz, name, tpes) =>
      ty("JvmUnresolvedStaticMethod") ++ parens(ty(clazz) ++ text(", ") ++ text(name) ++ text(", ") ++ sep(", ", tpes.map(visit)))
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
    case DisplayType.RecordLabelType(name, tpe) => prop(name) ++ text(" = ") ++ visit(tpe)
  }

  /**
    * Returns the given schema `fieldType` pair as a tree, e.g. `P(Int32, String)`.
    */
  private def field(fieldType: DisplayType.PredicateFieldType)(implicit o: FormatOptions): Fmt = fieldType match {
    case DisplayType.RelationFieldType(name, tpes) =>
      prop(name) ++ parens(sep(", ", tpes.map(visit)))
    case DisplayType.LatticeFieldType(name, tpes, lat) =>
      prop(name) ++ parens(sep(", ", tpes.map(visit)) ++ text("; ") ++ visit(lat))
    case DisplayType.NonPredFieldType(name, tpe) =>
      prop(name) ++ parens(text("<") ++ visit(tpe) ++ text(">"))
  }

  /**
    * Returns the given type view `arg` as a tree, delimited as a function argument: a tuple gets an
    * extra set of parentheses, so that `((a, b)) -> c` is told from `(a, b) -> c`.
    */
  private def delimitFunctionArg(arg: DisplayType)(implicit o: FormatOptions): Fmt = arg match {
    case tuple: DisplayType.Tuple => parens(visit(tuple))
    case d => delimit(d)
  }

  /**
    * Returns the given associated type `sym` as a tree, written with its trait as in source, e.g.
    * `Collection.Elm`.
    */
  private def assocType(sym: Symbol.AssocTypeSym)(implicit o: FormatOptions): Fmt =
    SymUse(sym, SemanticTokenType.Type, qualify(sym.trt) + "." + sym.name)

  /**
    * Returns the name of the given symbol `sym`, qualified by its namespace if the options say so.
    */
  private def qualify(sym: QualifiedSym)(implicit o: FormatOptions): String = o.qualify.qualify(sym)

  /**
    * Returns the given type view `d` as a tree, parenthesized if it is an operator, so that it can
    * stand as an operand or in an argument position.
    */
  private def delimit(d: DisplayType)(implicit o: FormatOptions): Fmt = d match {
    case _: DisplayType.Operator => parens(visit(d))
    case _ => visit(d)
  }

}

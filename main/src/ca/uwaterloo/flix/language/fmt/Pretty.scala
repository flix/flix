/*
 * Copyright 2026 Magnus Madsen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.language.fmt

import ca.uwaterloo.flix.api.lsp.SemanticTokenType
import ca.uwaterloo.flix.language.ast.shared.{Constant, EqualityConstraint, QualifiedSym, RegionScope, TraitConstraint, VarText}
import ca.uwaterloo.flix.language.ast.{Kind, RigidityEnv, Scheme, SourceLocation, Symbol, Type, TypeConstructor, TypedAst}
import ca.uwaterloo.flix.language.fmt.Fmt.{Ref, Token, ef, kw, parens, prop, sep, text, tparam, ty}
import ca.uwaterloo.flix.language.jvm.ClassDescs
import ca.uwaterloo.flix.language.phase.TypeSimplifier
import ca.uwaterloo.flix.language.phase.unification.Substitution
import ca.uwaterloo.flix.util.InternalCompilerException
import ca.uwaterloo.flix.util.collection.Nel

/**
  * The printer: builds a [[Fmt]] tree from the typed AST.
  *
  * A type is normalized by [[DisplayType]] first, which curries arrows, collects record and schema
  * rows, flattens operator chains and fills partial applications with holes. The walk from the view
  * to the tree puts a node where a string would be: `Int32` is a type token, `List` is a reference
  * to its enum, and ` -> ` is an operator token with spacing around it.
  *
  * Every entry point takes the [[FormatOptions]] implicitly; [[FormatOptions.Default]] is found
  * without an import.
  */
object Pretty {

  /////////////////////
  // Types and effects
  /////////////////////

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
    case d => text(" ") ++ Fmt.op("\\") ++ text(" ") ++ visit(d, effPos = true)
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
    * Returns the given type constructor `tc` by its internal name, e.g. `Arrow2`, for debugging.
    *
    * A constructor that stands for a declaration is a reference to it.
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
    case TypeConstructor.Enum(sym, _) => Ref(sym, SemanticTokenType.Enum, sym.name)
    case TypeConstructor.Struct(sym, _) => Ref(sym, SemanticTokenType.Struct, sym.name)
    case TypeConstructor.RestrictableEnum(sym, _) => Ref(sym, SemanticTokenType.Enum, sym.name)

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
    case TypeConstructor.Effect(sym, _) => Ref(sym, SemanticTokenType.Effect, sym.name)

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
    case TypeConstructor.CaseSet(syms, _) => ty("CaseSet") ++ parens(sep(", ", syms.toList.map(sym => Ref(sym, SemanticTokenType.EnumMember, sym.name))))
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

  /////////////////////////
  // Schemes and constraints
  /////////////////////////

  /**
    * Returns the given scheme `sc` as a tree, e.g. `∀(a, b). a -> b with Order[a] where T[a] ~ b`.
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
      Ref(head.sym, SemanticTokenType.Interface, qualify(head.sym)) ++ text("[") ++ tpe(arg) ++ text("]")
  }

  /**
    * Returns the given equality constraint `ec` as a tree, e.g. `Collection.Elm[a] ~ Int32`.
    */
  def equalityConstraint(ec: EqualityConstraint)(implicit o: FormatOptions): Fmt = ec match {
    case EqualityConstraint(symUse, tpe1, tpe2, _) =>
      assocType(symUse.sym) ++ text("[") ++ tpe(tpe1) ++ text("] ") ++ Fmt.op("~") ++ text(" ") ++ tpe(tpe2)
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

  /////////////
  // Signatures
  /////////////

  /**
    * Returns the signature of the def, sig or op `sym` with the given `spec` as a tree, e.g.
    * `def sort(l: List[a]): List[a] with Order[a]`.
    *
    * The constraints are taken from the declared scheme: `Spec` holds the equality constraints as
    * `TypedAst.EqualityConstraint`, which does not name the associated type, so there is not enough
    * in one to write it out.
    */
  def spec(sym: QualifiedSym, spec: TypedAst.Spec)(implicit o: FormatOptions): Fmt = {
    val name = sym match {
      case s: Symbol.DefnSym => Ref(s, SemanticTokenType.Function, s.name)
      case s: Symbol.SigSym => Ref(s, SemanticTokenType.Method, s.name)
      case s: Symbol.OpSym => Ref(s, SemanticTokenType.Function, s.name)
      case s => throw InternalCompilerException(s"Unexpected symbol of a spec: '$s'.", SourceLocation.Unknown)
    }
    val sc = spec.declaredScheme
    kw("def") ++ text(" ") ++ name ++ fparams(spec.fparams) ++ text(": ") ++ typeAndEff(spec.retTpe, spec.eff) ++ constraints(sc.tconstrs, sc.econstrs)
  }

  /**
    * Returns the signature of the given def `d` as a tree.
    */
  def defn(d: TypedAst.Def)(implicit o: FormatOptions): Fmt = spec(d.sym, d.spec)

  /**
    * Returns the signature of the given sig `s` as a tree.
    */
  def sig(s: TypedAst.Sig)(implicit o: FormatOptions): Fmt = spec(s.sym, s.spec)

  /**
    * Returns the signature of the given op `o` as a tree.
    */
  def op(decl: TypedAst.Op)(implicit o: FormatOptions): Fmt = spec(decl.sym, decl.spec)

  /**
    * Returns the given formal parameters `ps` as a tree, in parentheses, e.g. `(x: Int32, y: a)`.
    *
    * A function declared with no parameters gets a single parameter of the unit type, which is
    * written as it was declared: `()`.
    */
  def fparams(ps: Nel[TypedAst.FormalParam])(implicit o: FormatOptions): Fmt = ps match {
    case Nel(fparam, Nil) if fparam.tpe == Type.Unit => text("()")
    case _ =>
      val params = ps.toList.map {
        case TypedAst.FormalParam(bnd, t, _, _, _) =>
          Token(SemanticTokenType.Parameter, bnd.sym.text) ++ text(": ") ++ tpe(t)
      }
      parens(sep(", ", params))
  }

  /////////////////////
  // Kinds and constants
  /////////////////////

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
    case Kind.CaseSet(sym) => ty("CaseSet") ++ text("[") ++ Ref(sym, SemanticTokenType.Enum, sym.name) ++ text("]")
    // parenthesize the left because `->` is right-associative
    case Kind.Arrow(k1: Kind.Arrow, k2) => parens(kind(k1)) ++ text(" ") ++ Fmt.op("->") ++ text(" ") ++ kind(k2)
    case Kind.Arrow(k1, k2) => kind(k1) ++ text(" ") ++ Fmt.op("->") ++ text(" ") ++ kind(k2)
    case Kind.Error => ty("Error")
  }

  /**
    * Returns the given constant `c` as a tree, written as a literal, e.g. `42i32`.
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
    // In effect position, the universal effect is written as a user writes it, `IO`, unless the
    // options are the internal ones, where it is what the solver sees.
    case DisplayType.Univ => if (effPos && o.varNames == FormatOptions.VarName.NameBased) ef("IO") else ef("Univ")
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
    case DisplayType.Complement(tpe) => Fmt.op("~") ++ delimit(tpe, effPos)
    case DisplayType.Union(tpes) => text("{") ++ sep(", ", tpes.map(visit(_, effPos))) ++ text("}")
    case DisplayType.Plus(tpes) => sep(text(" ") ++ Fmt.op("+") ++ text(" "), tpes.map(delimit(_, effPos)))
    case DisplayType.Intersection(tpes) => sep(text(" ") ++ Fmt.op("&") ++ text(" "), tpes.map(delimit(_, effPos)))
    case DisplayType.SymmetricDiff(tpes) => sep(text(" ") ++ Fmt.op("⊕") ++ text(" "), tpes.map(delimit(_, effPos)))
    case DisplayType.Difference(tpes) => sep(text(" ") ++ Fmt.op("-") ++ text(" "), tpes.map(delimit(_, effPos)))
    case DisplayType.RelationConstructor => ty("Relation")
    case DisplayType.Relation(tpes) =>
      ty("Relation") ++ parens(sep(", ", tpes.map(visit(_, effPos = false))))
    case DisplayType.LatticeConstructor => ty("Lattice")
    case DisplayType.Lattice(tpes, lat) =>
      ty("Lattice") ++ parens(sep(", ", tpes.map(visit(_, effPos = false))) ++ text("; ") ++ visit(lat, effPos = false))
    case DisplayType.PureArrow(arg, ret) =>
      delimitFunctionArg(arg) ++ text(" ") ++ Fmt.op("->") ++ text(" ") ++ delimit(ret, effPos = false)
    case DisplayType.PolyArrow(arg, eff, ret) =>
      delimitFunctionArg(arg) ++ text(" ") ++ Fmt.op("->") ++ text(" ") ++ delimit(ret, effPos = false) ++ text(" ") ++ Fmt.op("\\") ++ text(" ") ++ visit(eff, effPos = true)
    case DisplayType.ArrowWithoutEffect(arg, ret) =>
      delimitFunctionArg(arg) ++ text(" ") ++ Fmt.op("-->") ++ text(" ") ++ delimit(ret, effPos = false)
    case DisplayType.Enum(sym) => Ref(sym, SemanticTokenType.Enum, qualify(sym))
    case DisplayType.Struct(sym) => Ref(sym, SemanticTokenType.Struct, qualify(sym))
    case DisplayType.RestrictableEnum(sym) => Ref(sym, SemanticTokenType.Enum, qualify(sym))
    case DisplayType.Alias(sym) => Ref(sym, SemanticTokenType.Type, qualify(sym))
    case DisplayType.Assoc(sym) => assocType(sym)
    case DisplayType.Effect(sym) => Ref(sym, SemanticTokenType.Effect, qualify(sym))
    case DisplayType.Native(desc) => ty(ClassDescs.canonicalNameOf(desc))
    case DisplayType.Case(sym) => Ref(sym, SemanticTokenType.EnumMember, sym.name)
    case DisplayType.Apply(tpe, tpes) =>
      visit(tpe, effPos = false) ++ text("[") ++ sep(", ", tpes.map(visit(_, effPos = false))) ++ text("]")
    case DisplayType.Var(sym) => typeVar(sym)
    case DisplayType.Tuple(elms) =>
      parens(sep(", ", elms.map(visit(_, effPos = false))))
    case DisplayType.JvmToType(tpe) =>
      ty("JvmToType") ++ parens(visit(tpe, effPos = false))
    case DisplayType.JvmToEff(tpe) =>
      ty("JvmToEff") ++ parens(visit(tpe, effPos = false))
    case DisplayType.JvmUnresolvedConstructor(name, tpes) =>
      ty("JvmUnresolvedConstructor") ++ parens(sep(", ", ty(name) :: tpes.map(visit(_, effPos = false))))
    case DisplayType.JvmUnresolvedField(tpe, name) =>
      ty("JvmUnresolvedField") ++ parens(visit(tpe, effPos = false) ++ text(", ") ++ text(name))
    case DisplayType.JvmUnresolvedMethod(tpe, name, tpes) =>
      ty("JvmUnresolvedMethod") ++ parens(sep(", ", visit(tpe, effPos = false) :: text(name) :: tpes.map(visit(_, effPos = false))))
    case DisplayType.JvmUnresolvedStaticMethod(clazz, name, tpes) =>
      ty("JvmUnresolvedStaticMethod") ++ parens(sep(", ", ty(clazz) :: text(name) :: tpes.map(visit(_, effPos = false))))
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
    * Returns the given associated type `sym` as a tree, with its trait, as in source:
    * `Collection.Elm`.
    */
  private def assocType(sym: Symbol.AssocTypeSym)(implicit o: FormatOptions): Fmt =
    Ref(sym, SemanticTokenType.Type, qualify(sym.trt) + "." + sym.name)

  /**
    * Returns the name of the given symbol `sym`, qualified by its namespace if the options say so.
    */
  private def qualify(sym: QualifiedSym)(implicit o: FormatOptions): String = o.qualify.qualify(sym)

}

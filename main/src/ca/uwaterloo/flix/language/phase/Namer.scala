/*
 * Copyright 2015-2016 Magnus Madsen
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

package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.Ast.{BoundBy, Source}
import ca.uwaterloo.flix.language.ast.DesugaredAst.Pattern.Record
import ca.uwaterloo.flix.language.ast.DesugaredAst.RestrictableChoosePattern
import ca.uwaterloo.flix.language.ast.{NamedAst, _}
import ca.uwaterloo.flix.language.dbg.AstPrinter._
import ca.uwaterloo.flix.language.errors.NameError
import ca.uwaterloo.flix.util.Validation._
import ca.uwaterloo.flix.util.collection.ListMap
import ca.uwaterloo.flix.util.{InternalCompilerException, ParOps, Validation}

import java.util.concurrent.ConcurrentLinkedQueue
import scala.jdk.CollectionConverters._

/**
  * The Namer phase introduces unique symbols for each syntactic entity in the program.
  */
object Namer {

  /**
    * Introduces unique names for each syntactic entity in the given `program`.
    * */
  def run(program: DesugaredAst.Root)(implicit flix: Flix): Validation[NamedAst.Root, NameError] =
    flix.phase("Namer") {

      // Construct a new shared context.
      implicit val sctx: SharedContext = SharedContext.mk()

      // compute all the source locations
      val locations = program.units.values.foldLeft(Map.empty[Source, SourceLocation]) {
        case (macc, root) => macc + (root.loc.source -> root.loc)
      }

      val unitsVal = ParOps.parTraverseValues(program.units)(visitUnit)

      flatMapN(unitsVal) {
        case units =>
          val SymbolTable(symbols0, instances0, uses0) = units.values.foldLeft(SymbolTable.empty)(tableUnit)
          // TODO NS-REFACTOR remove use of NName
          val symbols = symbols0.map {
            case (k, v) => Name.mkUnlocatedNName(k) -> v.m
          }
          val instances = instances0.map {
            case (k, v) => Name.mkUnlocatedNName(k) -> v
          }
          val uses = uses0.map {
            case (k, v) => Name.mkUnlocatedNName(k) -> v
          }
          Validation.toSuccessOrSoftFailure(NamedAst.Root(symbols, instances, uses, units, program.entryPoint, locations, program.names), sctx.errors.asScala)
      }
    }(DebugValidation())

  /**
    * Performs naming on the given compilation unit `unit` under the given (partial) program `prog0`.
    */
  private def visitUnit(unit: DesugaredAst.CompilationUnit)(implicit sctx: SharedContext, flix: Flix): Validation[NamedAst.CompilationUnit, NameError] = unit match {
    case DesugaredAst.CompilationUnit(usesAndImports0, decls0, loc) =>
      val usesAndImports = usesAndImports0.map(visitUseOrImport)
      val declsVal = traverse(decls0)(visitDecl(_, Name.RootNS))
      mapN(declsVal) {
        case decls => NamedAst.CompilationUnit(usesAndImports, decls, loc)
      }
  }

  /**
    * Performs naming on the given declaration.
    */
  private def visitDecl(decl0: DesugaredAst.Declaration, ns0: Name.NName)(implicit sctx: SharedContext, flix: Flix): Validation[NamedAst.Declaration, NameError] = decl0 match {
    case decl: DesugaredAst.Declaration.Namespace => visitNamespace(decl, ns0)
    case decl: DesugaredAst.Declaration.Trait => visitTrait(decl, ns0)
    case decl: DesugaredAst.Declaration.Instance => visitInstance(decl, ns0)
    case decl: DesugaredAst.Declaration.Def => visitDef(decl, ns0, DefKind.NonMember)
    case decl: DesugaredAst.Declaration.Enum => Validation.success(visitEnum(decl, ns0))
    case decl: DesugaredAst.Declaration.Struct => Validation.success(visitStruct(decl, ns0))
    case decl: DesugaredAst.Declaration.RestrictableEnum => Validation.success(visitRestrictableEnum(decl, ns0))
    case decl: DesugaredAst.Declaration.TypeAlias => Validation.success(visitTypeAlias(decl, ns0))
    case decl: DesugaredAst.Declaration.Effect => visitEffect(decl, ns0)
    case decl: DesugaredAst.Declaration.Law => throw InternalCompilerException("unexpected law", decl.loc)
  }

  /**
    * Performs naming on the given namespace.
    */
  private def visitNamespace(decl: DesugaredAst.Declaration.Namespace, ns0: Name.NName)(implicit sctx: SharedContext, flix: Flix): Validation[NamedAst.Declaration.Namespace, NameError] = decl match {
    case DesugaredAst.Declaration.Namespace(ident, usesAndImports0, decls0, loc) =>
      val ns = Name.NName(ns0.idents :+ ident, ident.loc)
      val usesAndImports = usesAndImports0.map(visitUseOrImport)
      val declsVal = traverse(decls0)(visitDecl(_, ns))
      val sym = new Symbol.ModuleSym(ns.parts)
      mapN(declsVal) {
        case decls => NamedAst.Declaration.Namespace(sym, usesAndImports, decls, loc)
      }
  }

  /**
    * Adds symbols from the compilation unit to the table.
    */
  private def tableUnit(table0: SymbolTable, unit: NamedAst.CompilationUnit)(implicit sctx: SharedContext): SymbolTable = unit match {
    case NamedAst.CompilationUnit(_, decls, _) => decls.foldLeft(table0)(tableDecl)
  }

  private def tableDecl(table0: SymbolTable, decl: NamedAst.Declaration)(implicit sctx: SharedContext): SymbolTable = decl match {
    case NamedAst.Declaration.Namespace(sym, usesAndImports, decls, _) =>
      // Add the namespace to the table (no validation needed)
      val table1 = addDeclToTable(table0, sym.ns.init, sym.ns.last, decl)
      val table2 = decls.foldLeft(table1)(tableDecl)
      addUsesToTable(table2, sym.ns, usesAndImports)

    case NamedAst.Declaration.Trait(_, _, _, sym, _, _, assocs, sigs, _, _) =>
      val table1 = tryAddToTable(table0, sym.namespace, sym.name, decl)
      val assocsAndSigs = assocs ++ sigs
      assocsAndSigs.foldLeft(table1)(tableDecl)

    case inst@NamedAst.Declaration.Instance(_, _, _, clazz, _, _, _, _, _, ns, _) =>
      addInstanceToTable(table0, ns, clazz.ident.name, inst)

    case NamedAst.Declaration.Sig(sym, _, _) =>
      tryAddToTable(table0, sym.namespace, sym.name, decl)

    case NamedAst.Declaration.Def(sym, _, _) =>
      tryAddToTable(table0, sym.namespace, sym.name, decl)

    case NamedAst.Declaration.Enum(_, _, _, sym, _, _, cases, _) =>
      val table1 = tryAddToTable(table0, sym.namespace, sym.name, decl)
      cases.foldLeft(table1)(tableDecl)

    case NamedAst.Declaration.Struct(_, _, _, sym, _, fields, _) =>
      val table1 = tryAddToTable(table0, sym.namespace, sym.name, decl)
      fields.foldLeft(table1)(tableDecl)

    case NamedAst.Declaration.RestrictableEnum(_, _, _, sym, _, _, _, cases, _) =>
      val table1 = tryAddToTable(table0, sym.namespace, sym.name, decl)
      cases.foldLeft(table1)(tableDecl)

    case NamedAst.Declaration.TypeAlias(_, _, _, sym, _, _, _) =>
      tryAddToTable(table0, sym.namespace, sym.name, decl)

    case NamedAst.Declaration.AssocTypeSig(_, _, sym, _, _, _, _) =>
      tryAddToTable(table0, sym.namespace, sym.name, decl)

    case NamedAst.Declaration.Effect(_, _, _, sym, ops, _) =>
      val table1 = tryAddToTable(table0, sym.namespace, sym.name, decl)
      ops.foldLeft(table1)(tableDecl)

    case NamedAst.Declaration.Op(sym, _) =>
      tryAddToTable(table0, sym.namespace, sym.name, decl)

    case caze@NamedAst.Declaration.Case(sym, _, _) =>
      tryAddToTable(table0, sym.namespace, sym.name, caze)

    case field@NamedAst.Declaration.StructField(sym, _, _) =>
      tryAddToTable(table0, sym.namespace, sym.name, field)

    case caze@NamedAst.Declaration.RestrictableCase(sym, _, _) =>
      tryAddToTable(table0, sym.namespace, sym.name, caze)

    case NamedAst.Declaration.AssocTypeDef(_, _, _, _, _, loc) =>
      throw InternalCompilerException("unexpected tabling of associated type definition", loc)

  }

  /**
    * Tries to add the given declaration to the table.
    */
  private def tryAddToTable(table: SymbolTable, ns: List[String], name: String, decl: NamedAst.Declaration)(implicit sctx: SharedContext): SymbolTable = {
    lookupName(name, ns, table) match {
      case LookupResult.NotDefined => addDeclToTable(table, ns, name, decl)
      case LookupResult.AlreadyDefined(loc) =>
        mkDuplicateNamePair(name, getSymLocation(decl), loc)
        SymbolTable.empty
    }
  }

  /**
    * Adds the given declaration to the table.
    */
  private def addDeclToTable(table: SymbolTable, ns: List[String], name: String, decl: NamedAst.Declaration): SymbolTable = table match {
    case SymbolTable(symbols0, instances, uses) =>
      val oldMap = symbols0.getOrElse(ns, ListMap.empty)
      val newMap = oldMap + (name -> decl)
      val symbols = symbols0 + (ns -> newMap)
      SymbolTable(symbols, instances, uses)
  }

  /**
    * Adds the given instance to the table.
    */
  private def addInstanceToTable(table: SymbolTable, ns: List[String], name: String, decl: NamedAst.Declaration.Instance): SymbolTable = table match {
    case SymbolTable(symbols, instances0, uses) =>
      val oldMap = instances0.getOrElse(ns, Map.empty)
      val newMap = oldMap.updatedWith(name) {
        case None => Some(List(decl))
        case Some(insts) => Some(decl :: insts)
      }
      val instances = instances0 + (ns -> newMap)
      SymbolTable(symbols, instances, uses)
  }

  /**
    * Adds the given uses to the table.
    */
  private def addUsesToTable(table: SymbolTable, ns: List[String], usesAndImports: List[NamedAst.UseOrImport]): SymbolTable = table match {
    case SymbolTable(symbols, instances, uses0) =>
      val oldList = uses0.getOrElse(ns, Nil)
      val newList = usesAndImports ::: oldList
      val uses = uses0 + (ns -> newList)
      SymbolTable(symbols, instances, uses)
  }

  /**
    * Creates a pair of errors reporting a duplicate type declaration at each location.
    */
  private def mkDuplicateNamePair(name: String, loc1: SourceLocation, loc2: SourceLocation)(implicit sctx: SharedContext): Unit = {
    // NB: We report an error at both source locations.
    if (name.charAt(0).isUpper) {
      // Case 1: uppercase name
      sctx.errors.add(NameError.DuplicateUpperName(name, loc1, loc2))
      sctx.errors.add(NameError.DuplicateUpperName(name, loc2, loc1))
    } else {
      // Case 2: lowercase name
      sctx.errors.add(NameError.DuplicateLowerName(name, loc1, loc2))
      sctx.errors.add(NameError.DuplicateLowerName(name, loc2, loc1))
    }
  }

  /**
    * The result of looking up a type or trait name in an ast root.
    */
  private sealed trait NameLookupResult

  private object LookupResult {

    case object NotDefined extends NameLookupResult

    case class AlreadyDefined(loc: SourceLocation) extends NameLookupResult

  }

  /**
    * Looks up the uppercase name in the given namespace and root.
    */
  private def lookupName(name: String, ns0: List[String], table: SymbolTable): NameLookupResult = {
    val symbols0 = table.symbols.getOrElse(ns0, ListMap.empty)
    // ignore namespaces
    symbols0(name).flatMap {
      case _: NamedAst.Declaration.Namespace => None
      case decl => Some(decl)
    }.headOption match {
      // Case 1: The name is unused.
      case None => LookupResult.NotDefined
      // Case 2: An symbol with the name already exists.
      case Some(decl) => LookupResult.AlreadyDefined(getSymLocation(decl))
    }
  }

  /**
    * Performs naming on the given constraint `c0`.
    */
  private def visitConstraint(c0: DesugaredAst.Constraint, ns0: Name.NName)(implicit flix: Flix, sctx: SharedContext): Validation[NamedAst.Constraint, NameError] = c0 match {
    case DesugaredAst.Constraint(h, bs, loc) =>

      // Introduce a symbol for every unique ident in the body, removing wildcards
      val idents = bs.flatMap {
        case DesugaredAst.Predicate.Body.Atom(_, _, _, _, terms, _) => terms.flatMap(freeVars)
        case DesugaredAst.Predicate.Body.Functional(idents1, _, _) => idents1
        case DesugaredAst.Predicate.Body.Guard(_, _) => Nil
      }.distinct.filterNot(_.isWild)

      val cparams = idents.map {
        case i =>
          val sym = Symbol.freshVarSym(i, BoundBy.Constraint)
          NamedAst.ConstraintParam(sym, loc)
      }

      // Perform naming on the head and body predicates.
      mapN(visitHeadPredicate(h, ns0), traverse(bs)(b => visitBodyPredicate(b, ns0))) {
        case (head, body) =>
          NamedAst.Constraint(cparams, head, body, loc)
      }
  }


  /**
    * Performs naming on the given enum `enum0`.
    */
  private def visitEnum(enum0: DesugaredAst.Declaration.Enum, ns0: Name.NName)(implicit flix: Flix, sctx: SharedContext): NamedAst.Declaration.Enum = enum0 match {
    case DesugaredAst.Declaration.Enum(doc, ann, mod0, ident, tparams0, derives0, cases0, loc) =>
      val sym = Symbol.mkEnumSym(ns0, ident)

      // Compute the type parameters.
      val tparams = visitTypeParams(tparams0)

      val mod = visitModifiers(mod0, ns0)
      val derives = visitDerivations(derives0)
      val cases = cases0.map(visitCase(_, sym))

      NamedAst.Declaration.Enum(doc, ann, mod, sym, tparams, derives, cases, loc)
  }

  /**
    * Performs the naming on the given struct `struct0`.
    */
  private def visitStruct(struct0: DesugaredAst.Declaration.Struct, ns0: Name.NName)(implicit flix: Flix, sctx: SharedContext): NamedAst.Declaration.Struct = struct0 match {
    case DesugaredAst.Declaration.Struct(doc, ann, mod0, ident, tparams0, fields0, loc) =>
      val sym = Symbol.mkStructSym(ns0, ident)

      // Compute the type parameters.
      val tparams = visitTypeParams(tparams0)

      val mod = visitModifiers(mod0, ns0)
      val fields = fields0.map(visitField(_, sym))

      NamedAst.Declaration.Struct(doc, ann, mod, sym, tparams, fields, loc)
  }

  /**
    * Performs naming on the given enum `enum0`.
    */
  private def visitRestrictableEnum(enum0: DesugaredAst.Declaration.RestrictableEnum, ns0: Name.NName)(implicit flix: Flix, sctx: SharedContext): NamedAst.Declaration.RestrictableEnum = enum0 match {
    case DesugaredAst.Declaration.RestrictableEnum(doc, ann, mod0, ident, index0, tparams0, derives0, cases, loc) =>
      val caseIdents = cases.map(_.ident)
      val sym = Symbol.mkRestrictableEnumSym(ns0, ident, caseIdents)

      // Compute the type parameters.
      val index = visitTypeParam(index0)
      val tparams = visitTypeParams(tparams0)

      val mod = visitModifiers(mod0, ns0)
      val derives = visitDerivations(derives0)
      val cs = cases.map(visitRestrictableCase(_, sym))

      NamedAst.Declaration.RestrictableEnum(doc, ann, mod, sym, index, tparams, derives, cs, loc)
  }

  /**
    * Performs naming on the given enum derivations.
    */
  private def visitDerivations(derives0: DesugaredAst.Derivations): NamedAst.Derivations =
    NamedAst.Derivations(derives0.classes, derives0.loc)

  /**
    * Performs naming on the given enum case.
    */
  private def visitCase(case0: DesugaredAst.Case, enumSym: Symbol.EnumSym)(implicit flix: Flix, sctx: SharedContext): NamedAst.Declaration.Case = case0 match {
    case DesugaredAst.Case(ident, tpe, loc) =>
      val t = visitType(tpe)
      val caseSym = Symbol.mkCaseSym(enumSym, ident)
      NamedAst.Declaration.Case(caseSym, t, loc)
  }

  /**
    * Performs naming on the given field.
    */
  private def visitField(field0: DesugaredAst.StructField, structSym: Symbol.StructSym)(implicit flix: Flix, sctx: SharedContext): NamedAst.Declaration.StructField = field0 match {
    case DesugaredAst.StructField(ident, tpe, loc) =>
      val t = visitType(tpe)
      val fieldSym = Symbol.mkStructFieldSym(structSym, ident)
      NamedAst.Declaration.StructField(fieldSym, t, loc)
  }

  /**
    * Performs naming on the given enum case.
    */
  private def visitRestrictableCase(case0: DesugaredAst.RestrictableCase, enumSym: Symbol.RestrictableEnumSym)(implicit flix: Flix, sctx: SharedContext): NamedAst.Declaration.RestrictableCase = case0 match {
    case DesugaredAst.RestrictableCase(ident, tpe, loc) =>
      val t = visitType(tpe)
      val caseSym = Symbol.mkRestrictableCaseSym(enumSym, ident)
      NamedAst.Declaration.RestrictableCase(caseSym, t, loc)
  }

  /**
    * Performs naming on the given type alias `alias0`.
    */
  private def visitTypeAlias(alias0: DesugaredAst.Declaration.TypeAlias, ns0: Name.NName)(implicit flix: Flix, sctx: SharedContext): NamedAst.Declaration.TypeAlias = alias0 match {
    case DesugaredAst.Declaration.TypeAlias(doc, ann, mod0, ident, tparams0, tpe, loc) =>
      val mod = visitModifiers(mod0, ns0)
      val tparams = visitTypeParams(tparams0)
      val t = visitType(tpe)
      val sym = Symbol.mkTypeAliasSym(ns0, ident)
      NamedAst.Declaration.TypeAlias(doc, ann, mod, sym, tparams, t, loc)
  }

  /**
    * Performs naming on the given associated type signature `s0`.
    */
  private def visitAssocTypeSig(s0: DesugaredAst.Declaration.AssocTypeSig, trt: Symbol.TraitSym)(implicit flix: Flix, sctx: SharedContext): Validation[NamedAst.Declaration.AssocTypeSig, NameError] = s0 match {
    case DesugaredAst.Declaration.AssocTypeSig(doc, mod, ident, tparams0, kind0, tpe, loc) =>
      val sym = Symbol.mkAssocTypeSym(trt, ident)
      val tparam = visitTypeParam(tparams0)
      val kind = visitKind(kind0)
      val t = tpe.map(visitType)
      Validation.success(NamedAst.Declaration.AssocTypeSig(doc, mod, sym, tparam, kind, t, loc))
  }

  /**
    * Performs naming on the given associated type definition `d0`.
    */
  private def visitAssocTypeDef(d0: DesugaredAst.Declaration.AssocTypeDef)(implicit flix: Flix, sctx: SharedContext): Validation[NamedAst.Declaration.AssocTypeDef, NameError] = d0 match {
    case DesugaredAst.Declaration.AssocTypeDef(doc, mod, ident, arg, tpe, loc) =>
      val t1 = visitType(arg)
      val t2 = visitType(tpe)
      Validation.success(NamedAst.Declaration.AssocTypeDef(doc, mod, ident, t1, t2, loc))
  }

  /**
    * Performs naming on the given trait `trt`.
    */
  private def visitTrait(trt: DesugaredAst.Declaration.Trait, ns0: Name.NName)(implicit flix: Flix, sctx: SharedContext): Validation[NamedAst.Declaration.Trait, NameError] = trt match {
    case DesugaredAst.Declaration.Trait(doc, ann, mod0, ident, tparams0, superTraits, assocs0, signatures, laws0, loc) =>
      val sym = Symbol.mkTraitSym(ns0, ident)
      val mod = visitModifiers(mod0, ns0)
      val tparam = visitTypeParam(tparams0)

      val sts = visitTypeConstraints(superTraits)
      val assocsVal = traverse(assocs0)(visitAssocTypeSig(_, sym)) // TODO switch param order to match visitSig
      val sigsVal = traverse(signatures)(visitSig(_, ns0, sym))
      val lawsVal = traverse(laws0)(visitDef(_, ns0, DefKind.Member))

      mapN(assocsVal, sigsVal, lawsVal) {
        case (assocs, sigs, laws) =>
          NamedAst.Declaration.Trait(doc, ann, mod, sym, tparam, sts, assocs, sigs, laws, loc)
      }
  }

  /**
    * Performs naming on the given instance `instance`.
    */
  private def visitInstance(instance: DesugaredAst.Declaration.Instance, ns0: Name.NName)(implicit flix: Flix, sctx: SharedContext): Validation[NamedAst.Declaration.Instance, NameError] = instance match {
    case DesugaredAst.Declaration.Instance(doc, ann, mod, clazz, tpe, tconstrs, assocs0, defs0, loc) =>
      val tparams = getImplicitTypeParamsFromTypes(List(tpe))
      val t = visitType(tpe)
      val tcsts = visitTypeConstraints(tconstrs)
      val assocsVal = traverse(assocs0)(visitAssocTypeDef)
      flatMapN(assocsVal) {
        case assocs =>
          val defsVal = traverse(defs0)(visitDef(_, ns0, DefKind.Member))
          mapN(defsVal) {
            defs => NamedAst.Declaration.Instance(doc, ann, mod, clazz, tparams, t, tcsts, assocs, defs, ns0.parts, loc)
          }
      }
  }

  /**
    * Performs naming on the given type constraint `tconstr`.
    */
  private def visitTypeConstraint(tconstr: DesugaredAst.TypeConstraint)(implicit flix: Flix, sctx: SharedContext): NamedAst.TypeConstraint = tconstr match {
    case DesugaredAst.TypeConstraint(trt, tparam, loc) =>
      val t = visitType(tparam)
      NamedAst.TypeConstraint(trt, t, loc)
  }

  /**
    * Performs naming on the given type constraints `tconstrs`.
    */
  private def visitTypeConstraints(tconstrs: List[DesugaredAst.TypeConstraint])(implicit flix: Flix, sctx: SharedContext): List[NamedAst.TypeConstraint] = {
    tconstrs.map(visitTypeConstraint)
  }

  /**
    * Performs naming on the given equality constraint `econstr`.
    */
  private def visitEqualityConstraint(econstr: DesugaredAst.EqualityConstraint)(implicit flix: Flix, sctx: SharedContext): NamedAst.EqualityConstraint = econstr match {
    case DesugaredAst.EqualityConstraint(qname, tpe1, tpe2, loc) =>
      val t1 = visitType(tpe1)
      val t2 = visitType(tpe2)
      NamedAst.EqualityConstraint(qname, t1, t2, loc)
  }

  /**
    * Performs naming on the given equality constraints `econstrs`.
    */
  private def visitEqualityConstraints(econstrs: List[DesugaredAst.EqualityConstraint])(implicit flix: Flix, sctx: SharedContext): List[NamedAst.EqualityConstraint] = {
    econstrs.map(visitEqualityConstraint)
  }

  /**
    * Performs naming on the given signature declaration `sig`.
    */
  private def visitSig(sig: DesugaredAst.Declaration.Sig, ns0: Name.NName, traitSym: Symbol.TraitSym)(implicit flix: Flix, sctx: SharedContext): Validation[NamedAst.Declaration.Sig, NameError] = sig match {
    case DesugaredAst.Declaration.Sig(doc, ann, mod0, ident, tparams0, fparams, exp0, tpe, eff, tconstrs, econstrs, loc) =>
      val tparams = getTypeParamsFromFormalParams(tparams0, fparams, tpe, eff, econstrs)

      // First visit all the top-level information
      val mod = visitModifiers(mod0, ns0)
      val fps = visitFormalParams(fparams)
      val t = visitType(tpe)
      val ef = eff.map(visitType)
      val tcsts = visitTypeConstraints(tconstrs)
      val ecsts = visitEqualityConstraints(econstrs)

      // Then visit the parts depending on the parameters
      val expVal = traverseOpt(exp0)(visitExp(_, ns0))

      mapN(expVal) {
        case e =>
          val sym = Symbol.mkSigSym(traitSym, ident)
          val spec = NamedAst.Spec(doc, ann, mod, tparams, fps, t, ef, tcsts, ecsts, loc)
          NamedAst.Declaration.Sig(sym, spec, e)
      }
  }

  /**
    * Performs naming on the given definition declaration `decl0`.
    */
  private def visitDef(decl0: DesugaredAst.Declaration.Def, ns0: Name.NName, defKind: DefKind)(implicit flix: Flix, sctx: SharedContext): Validation[NamedAst.Declaration.Def, NameError] = decl0 match {
    case DesugaredAst.Declaration.Def(doc, ann, mod0, ident, tparams0, fparams, exp, tpe, eff, tconstrs, econstrs, loc) =>
      flix.subtask(ident.name, sample = true)

      val tparams = getTypeParamsFromFormalParams(tparams0, fparams, tpe, eff, econstrs)

      // First visit all the top-level information
      val mod = visitModifiers(mod0, ns0)
      val fps = visitFormalParams(fparams)
      val t = visitType(tpe)
      val ef = eff.map(visitType)
      val tcsts = visitTypeConstraints(tconstrs)
      val ecsts = visitEqualityConstraints(econstrs)

      // Then visit the parts depending on the parameters
      val expVal = visitExp(exp, ns0)

      mapN(expVal) {
        case e =>
          // Give the def an id only if it is an instance def.
          // This distinguishes instance defs that could share a namespace.
          val id = defKind match {
            case DefKind.Member => Some(flix.genSym.freshId())
            case DefKind.NonMember => None
          }
          val sym = Symbol.mkDefnSym(ns0, ident, id)
          val spec = NamedAst.Spec(doc, ann, mod, tparams, fps, t, ef, tcsts, ecsts, loc)
          NamedAst.Declaration.Def(sym, spec, e)
      }
  }

  /**
    * Performs naming on the given effect `eff0`.
    */
  private def visitEffect(eff0: DesugaredAst.Declaration.Effect, ns0: Name.NName)(implicit flix: Flix, sctx: SharedContext): Validation[NamedAst.Declaration.Effect, NameError] = eff0 match {
    case DesugaredAst.Declaration.Effect(doc, ann, mod0, ident, ops0, loc) =>
      val sym = Symbol.mkEffectSym(ns0, ident)

      val mod = visitModifiers(mod0, ns0)
      val opsVal = traverse(ops0)(visitOp(_, ns0, sym))

      mapN(opsVal) {
        case ops => NamedAst.Declaration.Effect(doc, ann, mod, sym, ops, loc)
      }
  }

  /**
    * Performs naming on the given effect operation `op0`.
    */
  private def visitOp(op0: DesugaredAst.Declaration.Op, ns0: Name.NName, effSym: Symbol.EffectSym)(implicit flix: Flix, sctx: SharedContext): Validation[NamedAst.Declaration.Op, NameError] = op0 match {
    case DesugaredAst.Declaration.Op(doc, ann, mod0, ident, fparams, tpe, tconstrs, loc) =>
      // First visit all the top-level information
      val mod = visitModifiers(mod0, ns0)
      val fps = visitFormalParams(fparams)
      val t = visitType(tpe)
      val tcsts = visitTypeConstraints(tconstrs)

      val tparams = NamedAst.TypeParams.Kinded(Nil) // operations are monomorphic
      val eff = None // operations are pure
      val econstrs = Nil // TODO ASSOC-TYPES allow econstrs here

      val sym = Symbol.mkOpSym(effSym, ident)
      val spec = NamedAst.Spec(doc, ann, mod, tparams, fps, t, eff, tcsts, econstrs, loc)
      Validation.success(NamedAst.Declaration.Op(sym, spec))
  }

  /**
    * Performs naming on the given expression `exp0`.
    */
  // TODO NS-REFACTOR can remove ns0 too?
  private def visitExp(exp0: DesugaredAst.Expr, ns0: Name.NName)(implicit flix: Flix, sctx: SharedContext): Validation[NamedAst.Expr, NameError] = exp0 match {

    case DesugaredAst.Expr.Ambiguous(name, loc) =>
      Validation.success(NamedAst.Expr.Ambiguous(name, loc))

    case DesugaredAst.Expr.OpenAs(name, exp, loc) =>
      mapN(visitExp(exp, ns0)) {
        case e => NamedAst.Expr.OpenAs(name, e, loc)
      }

    case DesugaredAst.Expr.Open(name, loc) =>
      Validation.success(NamedAst.Expr.Open(name, loc))

    case DesugaredAst.Expr.Hole(name, loc) =>
      Validation.success(NamedAst.Expr.Hole(name, loc))

    case DesugaredAst.Expr.HoleWithExp(exp, loc) =>
      mapN(visitExp(exp, ns0)) {
        case e => NamedAst.Expr.HoleWithExp(e, loc)
      }

    case DesugaredAst.Expr.Use(uses0, exp, loc) =>
      val uses = uses0.map(visitUseOrImport)

      mapN(visitExp(exp, ns0)) {
        case e => uses.foldRight(e) {
          case (use, acc) => NamedAst.Expr.Use(use, acc, loc)
        }
      }

    case DesugaredAst.Expr.Cst(cst, loc) =>
      Validation.success(NamedAst.Expr.Cst(cst, loc))

    case DesugaredAst.Expr.Apply(exp, exps, loc) =>
      mapN(visitExp(exp, ns0), traverse(exps)(visitExp(_, ns0))) {
        case (e, es) => NamedAst.Expr.Apply(e, es, loc)
      }

    case DesugaredAst.Expr.Lambda(fparam, exp, loc) =>
      val fp = visitFormalParam(fparam)
      mapN(visitExp(exp, ns0)) {
        case e => NamedAst.Expr.Lambda(fp, e, loc)
      }

    case DesugaredAst.Expr.Unary(sop, exp, loc) =>
      mapN(visitExp(exp, ns0)) {
        case e => NamedAst.Expr.Unary(sop, e, loc)
      }

    case DesugaredAst.Expr.Binary(sop, exp1, exp2, loc) =>
      mapN(visitExp(exp1, ns0), visitExp(exp2, ns0)) {
        case (e1, e2) => NamedAst.Expr.Binary(sop, e1, e2, loc)
      }

    case DesugaredAst.Expr.IfThenElse(exp1, exp2, exp3, loc) =>
      val e1 = visitExp(exp1, ns0)
      val e2 = visitExp(exp2, ns0)
      val e3 = visitExp(exp3, ns0)
      mapN(e1, e2, e3) {
        NamedAst.Expr.IfThenElse(_, _, _, loc)
      }

    case DesugaredAst.Expr.Stm(exp1, exp2, loc) =>
      val e1 = visitExp(exp1, ns0)
      val e2 = visitExp(exp2, ns0)
      mapN(e1, e2) {
        NamedAst.Expr.Stm(_, _, loc)
      }

    case DesugaredAst.Expr.Discard(exp, loc) =>
      mapN(visitExp(exp, ns0)) {
        case e => NamedAst.Expr.Discard(e, loc)
      }

    case DesugaredAst.Expr.Let(ident, mod, exp1, exp2, loc) =>
      // make a fresh variable symbol for the local variable.
      val sym = Symbol.freshVarSym(ident, BoundBy.Let)
      mapN(visitExp(exp1, ns0), visitExp(exp2, ns0)) {
        case (e1, e2) => NamedAst.Expr.Let(sym, mod, e1, e2, loc)
      }

    case DesugaredAst.Expr.LetRec(ident, ann, mod, exp1, exp2, loc) =>
      val sym = Symbol.freshVarSym(ident, BoundBy.Let)
      mapN(visitExp(exp1, ns0), visitExp(exp2, ns0)) {
        case (e1, e2) => NamedAst.Expr.LetRec(sym, ann, mod, e1, e2, loc)
      }

    case DesugaredAst.Expr.Region(tpe, loc) =>
      Validation.success(NamedAst.Expr.Region(tpe, loc))

    case DesugaredAst.Expr.Scope(ident, exp, loc) =>
      // Introduce a fresh variable symbol for the region.
      val sym = Symbol.freshVarSym(ident, BoundBy.Let)

      // Introduce a rigid region variable for the region.
      val regionVar = Symbol.freshUnkindedTypeVarSym(Ast.VarText.SourceText(sym.text), isRegion = true, loc)

      // We must increase the level because we go under a new region scope.
      mapN(visitExp(exp, ns0)) {
        case e => NamedAst.Expr.Scope(sym, regionVar, e, loc)
      }

    case DesugaredAst.Expr.Match(exp, rules, loc) =>
      val expVal = visitExp(exp, ns0)
      val rulesVal = traverse(rules) {
        case DesugaredAst.MatchRule(pat, exp1, exp2) =>
          val p = visitPattern(pat)
          val e1Val = traverseOpt(exp1)(visitExp(_, ns0))
          val e2Val = visitExp(exp2, ns0)
          mapN(e1Val, e2Val) {
            case (e1, e2) => NamedAst.MatchRule(p, e1, e2)
          }
      }
      mapN(expVal, rulesVal) {
        case (e, rs) => NamedAst.Expr.Match(e, rs, loc)
      }

    case DesugaredAst.Expr.TypeMatch(exp, rules, loc) =>
      val expVal = visitExp(exp, ns0)
      val rulesVal = traverse(rules) {
        case DesugaredAst.TypeMatchRule(ident, tpe, body) =>
          val sym = Symbol.freshVarSym(ident, BoundBy.Pattern)
          val t = visitType(tpe)
          mapN(visitExp(body, ns0)) {
            case b => NamedAst.TypeMatchRule(sym, t, b)
          }
      }
      mapN(expVal, rulesVal) {
        case (e, rs) => NamedAst.Expr.TypeMatch(e, rs, loc)
      }

    case DesugaredAst.Expr.RestrictableChoose(star, exp, rules, loc) =>
      val expVal = visitExp(exp, ns0)
      val rulesVal = traverse(rules) {
        case DesugaredAst.RestrictableChooseRule(pat0, exp1) =>
          val p = visitRestrictablePattern(pat0)
          val eVal = visitExp(exp1, ns0)
          mapN(eVal) {
            case e => NamedAst.RestrictableChooseRule(p, e)
          }
      }
      mapN(expVal, rulesVal) {
        case (es, rs) => NamedAst.Expr.RestrictableChoose(star, es, rs, loc)
      }

    case DesugaredAst.Expr.Tuple(exps, loc) =>
      mapN(traverse(exps)(e => visitExp(e, ns0))) {
        case es => NamedAst.Expr.Tuple(es, loc)
      }

    case DesugaredAst.Expr.RecordEmpty(loc) =>
      Validation.success(NamedAst.Expr.RecordEmpty(loc))

    case DesugaredAst.Expr.RecordSelect(exp, label, loc) =>
      mapN(visitExp(exp, ns0)) {
        case e => NamedAst.Expr.RecordSelect(e, label, loc)
      }

    case DesugaredAst.Expr.RecordExtend(label, exp1, exp2, loc) =>
      mapN(visitExp(exp1, ns0), visitExp(exp2, ns0)) {
        case (v, r) => NamedAst.Expr.RecordExtend(label, v, r, loc)
      }

    case DesugaredAst.Expr.RecordRestrict(label, exp, loc) =>
      mapN(visitExp(exp, ns0)) {
        case r => NamedAst.Expr.RecordRestrict(label, r, loc)
      }

    case DesugaredAst.Expr.ArrayLit(exps, exp, loc) =>
      mapN(traverse(exps)(visitExp(_, ns0)), visitExp(exp, ns0)) {
        case (es, e) => NamedAst.Expr.ArrayLit(es, e, loc)
      }

    case DesugaredAst.Expr.ArrayNew(exp1, exp2, exp3, loc) =>
      mapN(visitExp(exp1, ns0), visitExp(exp2, ns0), visitExp(exp3, ns0)) {
        case (e1, e2, e3) => NamedAst.Expr.ArrayNew(e1, e2, e3, loc)
      }

    case DesugaredAst.Expr.ArrayLoad(exp1, exp2, loc) =>
      mapN(visitExp(exp1, ns0), visitExp(exp2, ns0)) {
        case (e1, e2) => NamedAst.Expr.ArrayLoad(e1, e2, loc)
      }

    case DesugaredAst.Expr.ArrayStore(exp1, exp2, exp3, loc) =>
      mapN(visitExp(exp1, ns0), visitExp(exp2, ns0), visitExp(exp3, ns0)) {
        case (e1, e2, e3) => NamedAst.Expr.ArrayStore(e1, e2, e3, loc)
      }

    case DesugaredAst.Expr.ArrayLength(exp, loc) =>
      mapN(visitExp(exp, ns0)) {
        case e => NamedAst.Expr.ArrayLength(e, loc)
      }

    case DesugaredAst.Expr.StructNew(name, exps0, region0, loc) =>
      val structsym = Symbol.mkStructSym(name.namespace, name.ident)
      val expsVal = traverse(exps0) {
        case (n, e) => mapN(visitExp(e, ns0)) {
          case e => (Symbol.mkStructFieldSym(structsym, n), e)
        }
      }
      val regionVal = visitExp(region0, ns0)
      mapN(expsVal, regionVal) {
        case (exps, region) => NamedAst.Expr.StructNew(structsym, exps, region, loc)
      }

    case DesugaredAst.Expr.StructGet(e, name, loc) =>
      val structsym = Symbol.mkStructSym(ns0, ns0.idents.last)
      mapN(visitExp(e, ns0)) {
        case e => NamedAst.Expr.StructGet(structsym, e, name, loc)
      }

    case DesugaredAst.Expr.StructPut(e1, name, e2, loc) =>
      val structsym = Symbol.mkStructSym(ns0, ns0.idents.last)
      mapN(visitExp(e1, ns0), visitExp(e2, ns0)) {
        case (e1, e2) => NamedAst.Expr.StructPut(structsym, e1, name, e2, loc)
      }

    case DesugaredAst.Expr.VectorLit(exps, loc) =>
      mapN(traverse(exps)(visitExp(_, ns0))) {
        case es => NamedAst.Expr.VectorLit(es, loc)
      }

    case DesugaredAst.Expr.VectorLoad(exp1, exp2, loc) =>
      mapN(visitExp(exp1, ns0), visitExp(exp2, ns0)) {
        case (e1, e2) => NamedAst.Expr.VectorLoad(e1, e2, loc)
      }

    case DesugaredAst.Expr.VectorLength(exp, loc) =>
      mapN(visitExp(exp, ns0)) {
        case e => NamedAst.Expr.VectorLength(e, loc)
      }

    case DesugaredAst.Expr.Ref(exp1, exp2, loc) =>
      mapN(visitExp(exp1, ns0), visitExp(exp2, ns0)) {
        case (e1, e2) =>
          NamedAst.Expr.Ref(e1, e2, loc)
      }

    case DesugaredAst.Expr.Deref(exp, loc) =>
      mapN(visitExp(exp, ns0)) {
        case e =>
          NamedAst.Expr.Deref(e, loc)
      }

    case DesugaredAst.Expr.Assign(exp1, exp2, loc) =>
      mapN(visitExp(exp1, ns0), visitExp(exp2, ns0)) {
        case (e1, e2) =>
          NamedAst.Expr.Assign(e1, e2, loc)
      }

    case DesugaredAst.Expr.Ascribe(exp, tpe, eff, loc) =>
      val expVal = visitExp(exp, ns0)
      val t = tpe.map(visitType)
      val ef = eff.map(visitType)
      mapN(expVal) {
        case e => NamedAst.Expr.Ascribe(e, t, ef, loc)
      }

    case DesugaredAst.Expr.InstanceOf(exp, className, loc) =>
      mapN(visitExp(exp, ns0)) {
        case e => NamedAst.Expr.InstanceOf(e, className, loc)
      }

    case DesugaredAst.Expr.CheckedCast(c, exp, loc) =>
      mapN(visitExp(exp, ns0)) {
        case e => NamedAst.Expr.CheckedCast(c, e, loc)
      }

    case DesugaredAst.Expr.UncheckedCast(exp, tpe, eff, loc) =>
      val expVal = visitExp(exp, ns0)
      val t = tpe.map(visitType)
      val ef = eff.map(visitType)

      mapN(expVal) {
        case e => NamedAst.Expr.UncheckedCast(e, t, ef, loc)
      }

    case DesugaredAst.Expr.UncheckedMaskingCast(exp, loc) =>
      mapN(visitExp(exp, ns0)) {
        case e => NamedAst.Expr.UncheckedMaskingCast(e, loc)
      }

    case DesugaredAst.Expr.Without(exp, eff, loc) =>
      val expVal = visitExp(exp, ns0)
      mapN(expVal) {
        e => NamedAst.Expr.Without(e, eff, loc)
      }

    case DesugaredAst.Expr.TryCatch(exp, rules, loc) =>
      val expVal = visitExp(exp, ns0)
      val rulesVal = traverse(rules) {
        case DesugaredAst.CatchRule(ident, className, body) =>
          val sym = Symbol.freshVarSym(ident, BoundBy.CatchRule)
          val bodyVal = visitExp(body, ns0)
          mapN(bodyVal) {
            b => NamedAst.CatchRule(sym, className, b)
          }
      }

      mapN(expVal, rulesVal) {
        case (e, rs) => NamedAst.Expr.TryCatch(e, rs, loc)
      }

    case DesugaredAst.Expr.TryWith(e0, eff, rules0, loc) =>
      val eVal = visitExp(e0, ns0)
      val rulesVal = traverse(rules0) {
        case DesugaredAst.HandlerRule(op, fparams, body0) =>
          val fps = visitFormalParams(fparams)
          val bodyVal = visitExp(body0, ns0)
          mapN(bodyVal) {
            body => NamedAst.HandlerRule(op, fps, body)
          }
      }
      mapN(eVal, rulesVal) {
        case (e, rules) => NamedAst.Expr.TryWith(e, eff, rules, loc)
      }

    case DesugaredAst.Expr.Do(op, exps0, loc) =>
      val expsVal = traverse(exps0)(visitExp(_, ns0))
      mapN(expsVal) {
        exps => NamedAst.Expr.Do(op, exps, loc)
      }

    case DesugaredAst.Expr.InvokeConstructor2(className, exps, loc) =>
      val expsVal = traverse(exps)(visitExp(_, ns0))
      mapN(expsVal) {
        exps => NamedAst.Expr.InvokeConstructor2(className, exps, loc)
      }

    case DesugaredAst.Expr.InvokeMethod2(exp, name, exps, loc) =>
      mapN(visitExp(exp, ns0), traverse(exps)(visitExp(_, ns0))) {
        case (e, es) => NamedAst.Expr.InvokeMethod2(e, name, es, loc)
      }

    case DesugaredAst.Expr.InvokeStaticMethod2(className, methodName, exps, loc) =>
      val expsVal = traverse(exps)(visitExp(_, ns0))
      mapN(expsVal) {
        exps => NamedAst.Expr.InvokeStaticMethod2(className, methodName, exps, loc)
      }

    case DesugaredAst.Expr.InvokeConstructor(className, exps, sig, loc) =>
      val argsVal = traverse(exps)(visitExp(_, ns0))
      val ts = sig.map(visitType)
      mapN(argsVal) {
        case as => NamedAst.Expr.InvokeConstructor(className, as, ts, loc)
      }

    case DesugaredAst.Expr.InvokeMethod(className, methodName, exp, exps, sig, tpe, loc) =>
      val expVal = visitExp(exp, ns0)
      val argsVal = traverse(exps)(visitExp(_, ns0))
      val ts = sig.map(visitType)
      val t = visitType(tpe)
      mapN(expVal, argsVal) {
        case (e, as) => NamedAst.Expr.InvokeMethod(className, methodName, e, as, ts, t, loc)
      }

    case DesugaredAst.Expr.InvokeStaticMethod(className, methodName, exps, sig, tpe, loc) =>
      val argsVal = traverse(exps)(visitExp(_, ns0))
      val ts = sig.map(visitType)
      val t = visitType(tpe)
      mapN(argsVal) {
        case as => NamedAst.Expr.InvokeStaticMethod(className, methodName, as, ts, t, loc)
      }

    case DesugaredAst.Expr.GetField(className, fieldName, exp, loc) =>
      mapN(visitExp(exp, ns0)) {
        case e => NamedAst.Expr.GetField(className, fieldName, e, loc)
      }

    case DesugaredAst.Expr.PutField(className, fieldName, exp1, exp2, loc) =>
      mapN(visitExp(exp1, ns0), visitExp(exp2, ns0)) {
        case (e1, e2) => NamedAst.Expr.PutField(className, fieldName, e1, e2, loc)
      }

    case DesugaredAst.Expr.GetStaticField(className, fieldName, loc) =>
      Validation.success(NamedAst.Expr.GetStaticField(className, fieldName, loc))

    case DesugaredAst.Expr.PutStaticField(className, fieldName, exp, loc) =>
      mapN(visitExp(exp, ns0)) {
        case e => NamedAst.Expr.PutStaticField(className, fieldName, e, loc)
      }

    case DesugaredAst.Expr.NewObject(tpe, methods, loc) =>
      val t = visitType(tpe)
      mapN(traverse(methods)(visitJvmMethod(_, ns0))) {
        case ms =>
          val name = s"Anon$$${flix.genSym.freshId()}"
          NamedAst.Expr.NewObject(name, t, ms, loc)
      }

    case DesugaredAst.Expr.NewChannel(exp1, exp2, loc) =>
      mapN(visitExp(exp1, ns0), visitExp(exp2, ns0)) {
        case (e1, e2) => NamedAst.Expr.NewChannel(e1, e2, loc)
      }

    case DesugaredAst.Expr.GetChannel(exp, loc) =>
      mapN(visitExp(exp, ns0)) {
        case e => NamedAst.Expr.GetChannel(e, loc)
      }

    case DesugaredAst.Expr.PutChannel(exp1, exp2, loc) =>
      mapN(visitExp(exp1, ns0), visitExp(exp2, ns0)) {
        case (e1, e2) => NamedAst.Expr.PutChannel(e1, e2, loc)
      }

    case DesugaredAst.Expr.SelectChannel(rules, exp, loc) =>
      val rulesVal = traverse(rules) {
        case DesugaredAst.SelectChannelRule(ident, exp1, exp2) =>
          // make a fresh variable symbol for the local recursive variable.
          val sym = Symbol.freshVarSym(ident, BoundBy.SelectRule)
          mapN(visitExp(exp1, ns0), visitExp(exp2, ns0)) {
            case (e1, e2) => NamedAst.SelectChannelRule(sym, e1, e2)
          }
      }

      val defaultVal = exp match {
        case Some(exp1) => mapN(visitExp(exp1, ns0)) {
          case e => Some(e)
        }
        case None => Validation.success(None)
      }

      mapN(rulesVal, defaultVal) {
        case (rs, d) => NamedAst.Expr.SelectChannel(rs, d, loc)
      }

    case DesugaredAst.Expr.Spawn(exp1, exp2, loc) =>
      mapN(visitExp(exp1, ns0), visitExp(exp2, ns0)) {
        case (e1, e2) =>
          NamedAst.Expr.Spawn(e1, e2, loc)
      }

    case DesugaredAst.Expr.ParYield(frags, exp, loc) =>
      val fragsVal = traverse(frags) {
        case DesugaredAst.ParYieldFragment(pat, e, l) =>
          val p = visitPattern(pat)
          mapN(visitExp(e, ns0)) {
            case e1 => NamedAst.ParYieldFragment(p, e1, l)
          }
      }

      // Combine everything
      mapN(fragsVal, visitExp(exp, ns0)) {
        case (fs, e) => NamedAst.Expr.ParYield(fs, e, loc)
      }

    case DesugaredAst.Expr.Lazy(exp, loc) =>
      mapN(visitExp(exp, ns0)) {
        case e => NamedAst.Expr.Lazy(e, loc)
      }

    case DesugaredAst.Expr.Force(exp, loc) =>
      mapN(visitExp(exp, ns0)) {
        case e => NamedAst.Expr.Force(e, loc)
      }

    case DesugaredAst.Expr.FixpointConstraintSet(cs0, loc) =>
      mapN(traverse(cs0)(visitConstraint(_, ns0))) {
        case cs =>
          NamedAst.Expr.FixpointConstraintSet(cs, loc)
      }

    case DesugaredAst.Expr.FixpointLambda(pparams, exp, loc) =>
      val ps = pparams.map(visitPredicateParam)
      val expVal = visitExp(exp, ns0)
      mapN(expVal) {
        case e => NamedAst.Expr.FixpointLambda(ps, e, loc)
      }

    case DesugaredAst.Expr.FixpointMerge(exp1, exp2, loc) =>
      mapN(visitExp(exp1, ns0), visitExp(exp2, ns0)) {
        case (e1, e2) => NamedAst.Expr.FixpointMerge(e1, e2, loc)
      }

    case DesugaredAst.Expr.FixpointSolve(exp, loc) =>
      mapN(visitExp(exp, ns0)) {
        case e => NamedAst.Expr.FixpointSolve(e, loc)
      }

    case DesugaredAst.Expr.FixpointFilter(ident, exp, loc) =>
      mapN(visitExp(exp, ns0)) {
        case e => NamedAst.Expr.FixpointFilter(ident, e, loc)
      }

    case DesugaredAst.Expr.FixpointInject(exp, pred, loc) =>
      mapN(visitExp(exp, ns0)) {
        case e => NamedAst.Expr.FixpointInject(e, pred, loc)
      }

    case DesugaredAst.Expr.FixpointProject(pred, exp1, exp2, loc) =>
      mapN(visitExp(exp1, ns0), visitExp(exp2, ns0)) {
        case (e1, e2) => NamedAst.Expr.FixpointProject(pred, e1, e2, loc)
      }

    case DesugaredAst.Expr.Error(m) =>
      // Note: We must NOT use [[Validation.toSoftFailure]] because
      // that would duplicate the error inside the Validation.
      Validation.success(NamedAst.Expr.Error(m))

  }

  /**
    * Names the given pattern `pat0`.
    */
  private def visitPattern(pat0: DesugaredAst.Pattern)(implicit flix: Flix, sctx: SharedContext): NamedAst.Pattern = pat0 match {
    case DesugaredAst.Pattern.Wild(loc) => NamedAst.Pattern.Wild(loc)
    case DesugaredAst.Pattern.Var(ident, loc) =>
      // make a fresh variable symbol for the local variable.
      val sym = Symbol.freshVarSym(ident, BoundBy.Pattern)
      NamedAst.Pattern.Var(sym, loc)

    case DesugaredAst.Pattern.Cst(cst, loc) => NamedAst.Pattern.Cst(cst, loc)

    case DesugaredAst.Pattern.Tag(qname, pat, loc) =>
      NamedAst.Pattern.Tag(qname, visitPattern(pat), loc)

    case DesugaredAst.Pattern.Tuple(elms, loc) =>
      NamedAst.Pattern.Tuple(elms.map(visitPattern), loc)

    case DesugaredAst.Pattern.Record(pats, pat, loc) =>
      val psVal = pats.map {
        case DesugaredAst.Pattern.Record.RecordLabelPattern(label, pat1, loc1) =>
          val p = pat1 match {
            case Some(p1) => visitPattern(p1)
            case None =>
              // Introduce new symbols if there is no pattern
              val sym = Symbol.freshVarSym(label.name, BoundBy.Pattern, label.loc)
              NamedAst.Pattern.Var(sym, label.loc)
          }
          NamedAst.Pattern.Record.RecordLabelPattern(label, p, loc1)
      }
      val pVal = visitPattern(pat)
      NamedAst.Pattern.Record(psVal, pVal, loc)

    case DesugaredAst.Pattern.RecordEmpty(loc) => NamedAst.Pattern.RecordEmpty(loc)

    case DesugaredAst.Pattern.Error(loc) => NamedAst.Pattern.Error(loc)
  }

  /**
    * Names the given pattern `pat0`
    */
  private def visitRestrictablePattern(pat0: DesugaredAst.RestrictableChoosePattern)(implicit flix: Flix, sctx: SharedContext): NamedAst.RestrictableChoosePattern = {
    def visitVarPlace(vp: DesugaredAst.RestrictableChoosePattern.VarOrWild): NamedAst.RestrictableChoosePattern.VarOrWild = vp match {
      case RestrictableChoosePattern.Wild(loc) => NamedAst.RestrictableChoosePattern.Wild(loc)
      case RestrictableChoosePattern.Var(ident, loc) =>
        // make a fresh variable symbol for the local variable.
        val sym = Symbol.freshVarSym(ident, BoundBy.Pattern)
        NamedAst.RestrictableChoosePattern.Var(sym, loc)
    }

    pat0 match {
      case DesugaredAst.RestrictableChoosePattern.Tag(qname, pat, loc) =>
        NamedAst.RestrictableChoosePattern.Tag(qname, pat.map(visitVarPlace), loc)
    }
  }

  /**
    * Names the given head predicate `head`.
    */
  private def visitHeadPredicate(head: DesugaredAst.Predicate.Head, ns0: Name.NName)(implicit flix: Flix, sctx: SharedContext): Validation[NamedAst.Predicate.Head, NameError] = head match {
    case DesugaredAst.Predicate.Head.Atom(pred, den, exps, loc) =>
      val expsVal = traverse(exps)(t => visitExp(t, ns0))
      mapN(expsVal) {
        case es => NamedAst.Predicate.Head.Atom(pred, den, es, loc)
      }
  }

  /**
    * Names the given body predicate `body`.
    */
  private def visitBodyPredicate(body: DesugaredAst.Predicate.Body, ns0: Name.NName)(implicit flix: Flix, sctx: SharedContext): Validation[NamedAst.Predicate.Body, NameError] = body match {
    case DesugaredAst.Predicate.Body.Atom(pred, den, polarity, fixity, terms, loc) =>
      val ts = terms.map(visitPattern)
      Validation.success(NamedAst.Predicate.Body.Atom(pred, den, polarity, fixity, ts, loc))

    case DesugaredAst.Predicate.Body.Functional(idents, exp, loc) =>
      val expVal = visitExp(exp, ns0)
      mapN(expVal) {
        case e => NamedAst.Predicate.Body.Functional(idents, e, loc)
      }

    case DesugaredAst.Predicate.Body.Guard(exp, loc) =>
      val expVal = visitExp(exp, ns0)
      mapN(expVal) {
        case e => NamedAst.Predicate.Body.Guard(e, loc)
      }
  }

  /**
    * Names the given type `tpe0`.
    */
  private def visitType(tpe0: DesugaredAst.Type)(implicit flix: Flix, sctx: SharedContext): NamedAst.Type = tpe0 match {
    case DesugaredAst.Type.Unit(loc) =>
      NamedAst.Type.Unit(loc)

    case DesugaredAst.Type.Var(ident, loc) =>
      //
      // Check for [[NameError.SuspiciousTypeVarName]].
      //
      if (isSuspiciousTypeVarName(ident.name)) {
        // TODO NS-REFACTOR maybe check this at declaration site instead of use site
        sctx.errors.add(NameError.SuspiciousTypeVarName(ident.name, loc))
      }
      NamedAst.Type.Var(ident, loc)

    case DesugaredAst.Type.Ambiguous(qname, loc) =>
      NamedAst.Type.Ambiguous(qname, loc)

    case DesugaredAst.Type.Tuple(tpes, loc) =>
      val ts = tpes.map(visitType)
      NamedAst.Type.Tuple(ts, loc)

    case DesugaredAst.Type.RecordRowEmpty(loc) =>
      NamedAst.Type.RecordRowEmpty(loc)

    case DesugaredAst.Type.RecordRowExtend(label, tpe1, tpe2, loc) =>
      val t1 = visitType(tpe1)
      val t2 = visitType(tpe2)
      NamedAst.Type.RecordRowExtend(label, t1, t2, loc)

    case DesugaredAst.Type.Record(tpe, loc) =>
      val t = visitType(tpe)
      NamedAst.Type.Record(t, loc)

    case DesugaredAst.Type.SchemaRowEmpty(loc) =>
      NamedAst.Type.SchemaRowEmpty(loc)

    case DesugaredAst.Type.SchemaRowExtendByAlias(qname, targs, tpe, loc) =>
      val ts = targs.map(visitType)
      val t = visitType(tpe)
      NamedAst.Type.SchemaRowExtendWithAlias(qname, ts, t, loc)

    case DesugaredAst.Type.SchemaRowExtendByTypes(ident, den, tpes, tpe, loc) =>
      val ts = tpes.map(visitType)
      val t = visitType(tpe)
      NamedAst.Type.SchemaRowExtendWithTypes(ident, den, ts, t, loc)

    case DesugaredAst.Type.Schema(tpe, loc) =>
      val t = visitType(tpe)
      NamedAst.Type.Schema(t, loc)

    case DesugaredAst.Type.Native(fqn, loc) =>
      NamedAst.Type.Native(fqn, loc)

    case DesugaredAst.Type.Arrow(tparams, eff, tpe, loc) =>
      val ts = tparams.map(visitType)
      val ef = eff.map(visitType)
      val t = visitType(tpe)
      NamedAst.Type.Arrow(ts, ef, t, loc)

    case DesugaredAst.Type.Apply(tpe1, tpe2, loc) =>
      val t1 = visitType(tpe1)
      val t2 = visitType(tpe2)
      NamedAst.Type.Apply(t1, t2, loc)

    case DesugaredAst.Type.True(loc) =>
      NamedAst.Type.True(loc)

    case DesugaredAst.Type.False(loc) =>
      NamedAst.Type.False(loc)

    case DesugaredAst.Type.Not(tpe, loc) =>
      val t = visitType(tpe)
      NamedAst.Type.Not(t, loc)

    case DesugaredAst.Type.And(tpe1, tpe2, loc) =>
      val t1 = visitType(tpe1)
      val t2 = visitType(tpe2)
      NamedAst.Type.And(t1, t2, loc)

    case DesugaredAst.Type.Or(tpe1, tpe2, loc) =>
      val t1 = visitType(tpe1)
      val t2 = visitType(tpe2)
      NamedAst.Type.Or(t1, t2, loc)

    case DesugaredAst.Type.Complement(tpe, loc) =>
      val t = visitType(tpe)
      NamedAst.Type.Complement(t, loc)

    case DesugaredAst.Type.Union(tpe1, tpe2, loc) =>
      val t1 = visitType(tpe1)
      val t2 = visitType(tpe2)
      NamedAst.Type.Union(t1, t2, loc)

    case DesugaredAst.Type.Intersection(tpe1, tpe2, loc) =>
      val t1 = visitType(tpe1)
      val t2 = visitType(tpe2)
      NamedAst.Type.Intersection(t1, t2, loc)

    case DesugaredAst.Type.Pure(loc) =>
      NamedAst.Type.Pure(loc)

    case DesugaredAst.Type.CaseSet(cases, loc) =>
      NamedAst.Type.CaseSet(cases, loc)

    case DesugaredAst.Type.CaseComplement(tpe, loc) =>
      val t = visitType(tpe)
      NamedAst.Type.CaseComplement(t, loc)

    case DesugaredAst.Type.CaseUnion(tpe1, tpe2, loc) =>
      val t1 = visitType(tpe1)
      val t2 = visitType(tpe2)
      NamedAst.Type.CaseUnion(t1, t2, loc)

    case DesugaredAst.Type.CaseIntersection(tpe1, tpe2, loc) =>
      val t1 = visitType(tpe1)
      val t2 = visitType(tpe2)
      NamedAst.Type.CaseIntersection(t1, t2, loc)

    case DesugaredAst.Type.Ascribe(tpe, kind, loc) =>
      val t = visitType(tpe)
      val k = visitKind(kind)
      NamedAst.Type.Ascribe(t, k, loc)

    case DesugaredAst.Type.Error(loc) =>
      NamedAst.Type.Error(loc)
  }

  /**
    * Performs naming on the given kind.
    */
  private def visitKind(k0: DesugaredAst.Kind): NamedAst.Kind = k0 match {
    case DesugaredAst.Kind.Ambiguous(qname, loc) => NamedAst.Kind.Ambiguous(qname, loc)
    case DesugaredAst.Kind.Arrow(k10, k20, loc) =>
      val k1 = visitKind(k10)
      val k2 = visitKind(k20)
      NamedAst.Kind.Arrow(k1, k2, loc)
  }

  /**
    * Returns `true` if the given string `s` is a suspicious type variable name.
    */
  private def isSuspiciousTypeVarName(s: String): Boolean = s match {
    case "void" => true
    case "unit" => true
    case "bool" => true
    case "char" => true
    case "float" => true
    case "float32" => true
    case "float64" => true
    case "int8" => true
    case "int16" => true
    case "int32" => true
    case "int64" => true
    case "bigint" => true
    case "string" => true
    case "array" => true
    case "vector" => true
    case "ref" => true
    case "pure" => true
    case "univ" => true
    case _ => false
  }

  /**
    * Returns all the free variables in the given pattern `pat0`.
    */
  private def freeVars(pat0: DesugaredAst.Pattern): List[Name.Ident] = pat0 match {
    case DesugaredAst.Pattern.Var(ident, _) => List(ident)
    case DesugaredAst.Pattern.Wild(_) => Nil
    case DesugaredAst.Pattern.Cst(Ast.Constant.Unit, _) => Nil
    case DesugaredAst.Pattern.Cst(Ast.Constant.Bool(true), _) => Nil
    case DesugaredAst.Pattern.Cst(Ast.Constant.Bool(false), _) => Nil
    case DesugaredAst.Pattern.Cst(Ast.Constant.Char(_), _) => Nil
    case DesugaredAst.Pattern.Cst(Ast.Constant.Float32(_), _) => Nil
    case DesugaredAst.Pattern.Cst(Ast.Constant.Float64(_), _) => Nil
    case DesugaredAst.Pattern.Cst(Ast.Constant.BigDecimal(_), _) => Nil
    case DesugaredAst.Pattern.Cst(Ast.Constant.Int8(_), _) => Nil
    case DesugaredAst.Pattern.Cst(Ast.Constant.Int16(_), _) => Nil
    case DesugaredAst.Pattern.Cst(Ast.Constant.Int32(_), _) => Nil
    case DesugaredAst.Pattern.Cst(Ast.Constant.Int64(_), _) => Nil
    case DesugaredAst.Pattern.Cst(Ast.Constant.BigInt(_), _) => Nil
    case DesugaredAst.Pattern.Cst(Ast.Constant.Str(_), _) => Nil
    case DesugaredAst.Pattern.Cst(Ast.Constant.Regex(_), _) => Nil
    case DesugaredAst.Pattern.Cst(Ast.Constant.Null, loc) => throw InternalCompilerException("unexpected null pattern", loc)
    case DesugaredAst.Pattern.Tag(_, p, _) => freeVars(p)
    case DesugaredAst.Pattern.Tuple(elms, _) => elms.flatMap(freeVars)
    case DesugaredAst.Pattern.Record(pats, pat, _) => recordPatternFreeVars(pats) ++ freeVars(pat)
    case DesugaredAst.Pattern.RecordEmpty(_) => Nil
    case DesugaredAst.Pattern.Error(_) => Nil
  }

  /**
    * Returns the free variables in the list of [[Record.RecordLabelPattern]] `pats`.
    */
  private def recordPatternFreeVars(pats: List[Record.RecordLabelPattern]): List[Name.Ident] = {
    def optFreeVars(rfp: Record.RecordLabelPattern): List[Name.Ident] = rfp.pat.map(freeVars).getOrElse(Nil)

    pats.flatMap(optFreeVars)
  }

  /**
    * Returns the free variables in the given type `tpe0`.
    */
  private def freeTypeVars(tpe0: DesugaredAst.Type): List[Name.Ident] = tpe0 match {
    case DesugaredAst.Type.Var(ident, _) => ident :: Nil
    case DesugaredAst.Type.Ambiguous(_, _) => Nil
    case DesugaredAst.Type.Unit(_) => Nil
    case DesugaredAst.Type.Tuple(elms, _) => elms.flatMap(freeTypeVars)
    case DesugaredAst.Type.RecordRowEmpty(_) => Nil
    case DesugaredAst.Type.RecordRowExtend(_, t, r, _) => freeTypeVars(t) ::: freeTypeVars(r)
    case DesugaredAst.Type.Record(row, _) => freeTypeVars(row)
    case DesugaredAst.Type.SchemaRowEmpty(_) => Nil
    case DesugaredAst.Type.SchemaRowExtendByTypes(_, _, ts, r, _) => ts.flatMap(freeTypeVars) ::: freeTypeVars(r)
    case DesugaredAst.Type.SchemaRowExtendByAlias(_, ts, r, _) => ts.flatMap(freeTypeVars) ::: freeTypeVars(r)
    case DesugaredAst.Type.Schema(row, _) => freeTypeVars(row)
    case DesugaredAst.Type.Native(_, _) => Nil
    case DesugaredAst.Type.Arrow(tparams, eff, tresult, _) => tparams.flatMap(freeTypeVars) ::: eff.toList.flatMap(freeTypeVars) ::: freeTypeVars(tresult)
    case DesugaredAst.Type.Apply(tpe1, tpe2, _) => freeTypeVars(tpe1) ++ freeTypeVars(tpe2)
    case DesugaredAst.Type.True(_) => Nil
    case DesugaredAst.Type.False(_) => Nil
    case DesugaredAst.Type.Not(tpe, _) => freeTypeVars(tpe)
    case DesugaredAst.Type.And(tpe1, tpe2, _) => freeTypeVars(tpe1) ++ freeTypeVars(tpe2)
    case DesugaredAst.Type.Or(tpe1, tpe2, _) => freeTypeVars(tpe1) ++ freeTypeVars(tpe2)
    case DesugaredAst.Type.Complement(tpe, _) => freeTypeVars(tpe)
    case DesugaredAst.Type.Union(tpe1, tpe2, _) => freeTypeVars(tpe1) ++ freeTypeVars(tpe2)
    case DesugaredAst.Type.Intersection(tpe1, tpe2, _) => freeTypeVars(tpe1) ++ freeTypeVars(tpe2)
    case DesugaredAst.Type.Pure(_) => Nil
    case DesugaredAst.Type.CaseSet(_, _) => Nil
    case DesugaredAst.Type.CaseComplement(tpe, _) => freeTypeVars(tpe)
    case DesugaredAst.Type.CaseUnion(tpe1, tpe2, _) => freeTypeVars(tpe1) ++ freeTypeVars(tpe2)
    case DesugaredAst.Type.CaseIntersection(tpe1, tpe2, _) => freeTypeVars(tpe1) ++ freeTypeVars(tpe2)
    case DesugaredAst.Type.Ascribe(tpe, _, _) => freeTypeVars(tpe)
    case DesugaredAst.Type.Error(_) => Nil
  }

  /**
    * Performs naming on the given modifiers.
    *
    * Adds the `pub` modifier if in the root namespace.
    */
  private def visitModifiers(mod: Ast.Modifiers, ns0: Name.NName): Ast.Modifiers = {
    if (ns0.isRoot) {
      mod.asPublic
    } else {
      mod
    }
  }

  /**
    * Translates the given weeded formal parameter to a named formal parameter.
    */
  private def visitFormalParam(fparam: DesugaredAst.FormalParam)(implicit flix: Flix, sctx: SharedContext): NamedAst.FormalParam = fparam match {
    case DesugaredAst.FormalParam(ident, mod, tpe, loc) =>
      // Generate a fresh variable symbol for the identifier.
      val freshSym = Symbol.freshVarSym(ident, BoundBy.FormalParam)

      // Compute the type of the formal parameter or use the type variable of the symbol.
      val t = tpe.map(visitType)

      // Construct the formal parameter.
      NamedAst.FormalParam(freshSym, mod, t, loc)
  }

  /**
    * Translates the given weeded predicate parameter to a named predicate parameter.
    */
  private def visitPredicateParam(pparam: DesugaredAst.PredicateParam)(implicit flix: Flix, sctx: SharedContext): NamedAst.PredicateParam = pparam match {
    case DesugaredAst.PredicateParam.PredicateParamUntyped(pred, loc) =>
      NamedAst.PredicateParam.PredicateParamUntyped(pred, loc)

    case DesugaredAst.PredicateParam.PredicateParamWithType(pred, den, tpes, loc) =>
      val ts = tpes.map(visitType)
      NamedAst.PredicateParam.PredicateParamWithType(pred, den, ts, loc)
  }

  /**
    * Translates the given weeded JvmMethod to a named JvmMethod.
    */
  private def visitJvmMethod(method: DesugaredAst.JvmMethod, ns0: Name.NName)(implicit flix: Flix, sctx: SharedContext): Validation[NamedAst.JvmMethod, NameError] = method match {
    case DesugaredAst.JvmMethod(ident, fparams, exp0, tpe, eff, loc) =>
      val fps = visitFormalParams(fparams)
      val t = visitType(tpe)
      val ef = eff.map(visitType)
      val expVal = visitExp(exp0, ns0)
      mapN(expVal) {
        case e => NamedAst.JvmMethod(ident, fps, e, t, ef, loc)
      }
  }

  /**
    * Performs naming on the given formal parameters `fparam0`.
    */
  private def visitFormalParams(fparams0: List[DesugaredAst.FormalParam])(implicit flix: Flix, sctx: SharedContext): List[NamedAst.FormalParam] = {
    fparams0.map(visitFormalParam)
  }


  /**
    * Performs naming on the given type parameter.
    */
  private def visitTypeParam(tparam0: DesugaredAst.TypeParam)(implicit flix: Flix, sctx: SharedContext): NamedAst.TypeParam = tparam0 match {
    case DesugaredAst.TypeParam.Kinded(ident, kind) =>
      NamedAst.TypeParam.Kinded(ident, mkTypeVarSym(ident), visitKind(kind), ident.loc)
    case DesugaredAst.TypeParam.Unkinded(ident) =>
      NamedAst.TypeParam.Unkinded(ident, mkTypeVarSym(ident), ident.loc)
  }

  /**
    * Performs naming on the given type parameters `tparam0` from the given cases `cases`.
    */
  private def visitTypeParams(tparams0: DesugaredAst.TypeParams)(implicit flix: Flix, sctx: SharedContext): NamedAst.TypeParams = {
    tparams0 match {
      case DesugaredAst.TypeParams.Elided => NamedAst.TypeParams.Kinded(Nil)
      case DesugaredAst.TypeParams.Unkinded(tparams) => visitExplicitTypeParams(tparams)
      case DesugaredAst.TypeParams.Kinded(tparams) => visitExplicitKindedTypeParams(tparams)
    }
  }


  /**
    * Performs naming on the given type parameters `tparams0` from the given formal params `fparams` and overall type `tpe`.
    */
  private def getTypeParamsFromFormalParams(tparams0: DesugaredAst.TypeParams, fparams: List[DesugaredAst.FormalParam], tpe: DesugaredAst.Type, eff: Option[DesugaredAst.Type], econstrs: List[DesugaredAst.EqualityConstraint])(implicit flix: Flix, sctx: SharedContext): NamedAst.TypeParams = {
    tparams0 match {
      case DesugaredAst.TypeParams.Elided => visitImplicitTypeParamsFromFormalParams(fparams, tpe, eff, econstrs)
      case DesugaredAst.TypeParams.Unkinded(tparams) => visitExplicitTypeParams(tparams)
      case DesugaredAst.TypeParams.Kinded(tparams) => visitExplicitKindedTypeParams(tparams)

    }
  }

  /**
    * Names the explicit kinded type params.
    */
  private def visitExplicitKindedTypeParams(tparams0: List[DesugaredAst.TypeParam.Kinded])(implicit flix: Flix, sctx: SharedContext): NamedAst.TypeParams.Kinded = {
    val tparams = tparams0.map {
      case DesugaredAst.TypeParam.Kinded(ident, kind) =>
        NamedAst.TypeParam.Kinded(ident, mkTypeVarSym(ident), visitKind(kind), ident.loc)
    }
    NamedAst.TypeParams.Kinded(tparams)
  }

  /**
    * Returns the explicit unkinded type parameters from the given type parameter names and implicit type parameters.
    */
  private def visitExplicitTypeParams(tparams0: List[DesugaredAst.TypeParam.Unkinded])(implicit flix: Flix, sctx: SharedContext): NamedAst.TypeParams.Unkinded = {
    val tparams = tparams0.map {
      case DesugaredAst.TypeParam.Unkinded(ident) =>
        NamedAst.TypeParam.Unkinded(ident, mkTypeVarSym(ident), ident.loc)
    }
    NamedAst.TypeParams.Unkinded(tparams)
  }

  /**
    * Returns the implicit type parameters constructed from the given types.
    */
  private def getImplicitTypeParamsFromTypes(types: List[DesugaredAst.Type])(implicit flix: Flix, sctx: SharedContext): NamedAst.TypeParams.Implicit = {
    val tvars = types.flatMap(freeTypeVars).distinct
    val tparams = tvars.map {
      ident => NamedAst.TypeParam.Implicit(ident, mkTypeVarSym(ident), ident.loc)
    }
    NamedAst.TypeParams.Implicit(tparams)
  }

  /**
    * Returns the implicit type parameters constructed from the given formal parameters and type.
    */
  private def visitImplicitTypeParamsFromFormalParams(fparams: List[DesugaredAst.FormalParam], tpe: DesugaredAst.Type, eff: Option[DesugaredAst.Type], econstrs: List[DesugaredAst.EqualityConstraint])(implicit flix: Flix, sctx: SharedContext): NamedAst.TypeParams = {
    // Compute the type variables that occur in the formal parameters.
    val fparamTvars = fparams.flatMap {
      case DesugaredAst.FormalParam(_, _, Some(tpe1), _) => freeTypeVars(tpe1)
      case DesugaredAst.FormalParam(_, _, None, _) => Nil
    }

    val tpeTvars = freeTypeVars(tpe)

    val effTvars = eff.toList.flatMap(freeTypeVars)

    val econstrTvars = econstrs.flatMap {
      // We only infer vars from the right-hand-side of the constraint.
      case DesugaredAst.EqualityConstraint(_, _, tpe2, _) => freeTypeVars(tpe2)
    }

    val tparams = (fparamTvars ::: tpeTvars ::: effTvars ::: econstrTvars).distinct.map {
      ident => NamedAst.TypeParam.Implicit(ident, mkTypeVarSym(ident), ident.loc)
    }

    NamedAst.TypeParams.Implicit(tparams)
  }

  /**
    * Gets the location of the symbol of the declaration.
    */
  private def getSymLocation(f: NamedAst.Declaration): SourceLocation = f match {
    case NamedAst.Declaration.Trait(_, _, _, sym, _, _, _, _, _, _) => sym.loc
    case NamedAst.Declaration.Sig(sym, _, _) => sym.loc
    case NamedAst.Declaration.Def(sym, _, _) => sym.loc
    case NamedAst.Declaration.Enum(_, _, _, sym, _, _, _, _) => sym.loc
    case NamedAst.Declaration.Struct(_, _, _, sym, _, _, _) => sym.loc
    case NamedAst.Declaration.RestrictableEnum(_, _, _, sym, _, _, _, _, _) => sym.loc
    case NamedAst.Declaration.TypeAlias(_, _, _, sym, _, _, _) => sym.loc
    case NamedAst.Declaration.Effect(_, _, _, sym, _, _) => sym.loc
    case NamedAst.Declaration.Op(sym, _) => sym.loc
    case NamedAst.Declaration.Case(sym, _, _) => sym.loc
    case NamedAst.Declaration.StructField(sym, _, _) => sym.loc
    case NamedAst.Declaration.RestrictableCase(sym, _, _) => sym.loc
    case NamedAst.Declaration.AssocTypeSig(_, _, sym, _, _, _, _) => sym.loc
    case NamedAst.Declaration.AssocTypeDef(_, _, _, _, _, loc) => throw InternalCompilerException("Unexpected associated type definition", loc)
    case NamedAst.Declaration.Instance(_, _, _, _, _, _, _, _, _, _, loc) => throw InternalCompilerException("Unexpected instance", loc)
    case NamedAst.Declaration.Namespace(_, _, _, loc) => throw InternalCompilerException("Unexpected namespace", loc)
  }

  /**
    * Creates a flexible unkinded type variable symbol from the given ident.
    */
  private def mkTypeVarSym(ident: Name.Ident)(implicit flix: Flix, sctx: SharedContext): Symbol.UnkindedTypeVarSym = {
    Symbol.freshUnkindedTypeVarSym(Ast.VarText.SourceText(ident.name), isRegion = false, ident.loc)
  }

  /**
    * Performs naming on the given `use`.
    */
  private def visitUseOrImport(use: DesugaredAst.UseOrImport): NamedAst.UseOrImport = use match {
    case DesugaredAst.UseOrImport.Use(qname, alias, loc) => NamedAst.UseOrImport.Use(qname, alias, loc)
    case DesugaredAst.UseOrImport.Import(name, alias, loc) => NamedAst.UseOrImport.Import(name, alias, loc)
  }

  /**
    * A structure holding the symbols and instances in the program.
    */
  case class SymbolTable(symbols: Map[List[String], ListMap[String, NamedAst.Declaration]], instances: Map[List[String], Map[String, List[NamedAst.Declaration.Instance]]], uses: Map[List[String], List[NamedAst.UseOrImport]])

  object SymbolTable {

    def empty: SymbolTable = SymbolTable(Map.empty, Map.empty, Map.empty)

  }

  /**
    * An enumeration of the kinds of defs.
    */
  private sealed trait DefKind

  private object DefKind {
    /**
      * A def that is a member of an instance or trait.
      */
    case object Member extends DefKind

    /**
      * A def that is not a member of an instance or trait.
      */
    case object NonMember extends DefKind
  }

  /**
    * Companion object for [[SharedContext]]
    */
  private object SharedContext {
    /**
      * Returns a fresh shared context.
      */
    def mk(): SharedContext = new SharedContext(new ConcurrentLinkedQueue())
  }

  /**
    * A global shared context. Must be thread-safe.
    *
    * @param errors the [[NameError]]s if the AST, if any.
    */
  private case class SharedContext(errors: ConcurrentLinkedQueue[NameError])

}

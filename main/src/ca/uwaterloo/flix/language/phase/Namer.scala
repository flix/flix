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
import ca.uwaterloo.flix.language.errors.NameError
import ca.uwaterloo.flix.util.Validation._
import ca.uwaterloo.flix.util.collection.{Chain, ListMap}
import ca.uwaterloo.flix.util.{InternalCompilerException, ParOps, Validation}

/**
  * The Namer phase introduces unique symbols for each syntactic entity in the program.
  */
object Namer {

  /**
    * Introduces unique names for each syntactic entity in the given `program`.
    * */
  def run(program: DesugaredAst.Root)(implicit flix: Flix): Validation[NamedAst.Root, NameError] = flix.phase("Namer") {
    // compute all the source locations
    val locations = program.units.values.foldLeft(Map.empty[Source, SourceLocation]) {
      case (macc, root) => macc + (root.loc.source -> root.loc)
    }

    val unitsVal = ParOps.parTraverseValues(program.units)(visitUnit)

    flatMapN(unitsVal) {
      case units =>
        val tableVal = fold(units.values, SymbolTable(Map.empty, Map.empty, Map.empty)) {
          case (table, unit) => tableUnit(unit, table)
        }

        mapN(tableVal) {
          case SymbolTable(symbols0, instances0, uses0) =>
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
            NamedAst.Root(symbols, instances, uses, units, program.entryPoint, locations, program.names)
        }
    }
  }

  /**
    * Performs naming on the given compilation unit `unit` under the given (partial) program `prog0`.
    */
  private def visitUnit(unit: DesugaredAst.CompilationUnit)(implicit flix: Flix): Validation[NamedAst.CompilationUnit, NameError] = unit match {
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
  private def visitDecl(decl0: DesugaredAst.Declaration, ns0: Name.NName)(implicit flix: Flix): Validation[NamedAst.Declaration, NameError] = decl0 match {
    case decl: DesugaredAst.Declaration.Namespace => visitNamespace(decl, ns0)
    case decl: DesugaredAst.Declaration.Trait => visitTrait(decl, ns0)
    case decl: DesugaredAst.Declaration.Instance => visitInstance(decl, ns0)
    case decl: DesugaredAst.Declaration.Def => visitDef(decl, ns0, DefKind.NonMember)
    case decl: DesugaredAst.Declaration.Enum => visitEnum(decl, ns0)
    case decl: DesugaredAst.Declaration.RestrictableEnum => visitRestrictableEnum(decl, ns0)
    case decl: DesugaredAst.Declaration.TypeAlias => visitTypeAlias(decl, ns0)
    case decl: DesugaredAst.Declaration.Effect => visitEffect(decl, ns0)
    case decl: DesugaredAst.Declaration.Law => throw InternalCompilerException("unexpected law", decl.loc)
  }

  /**
    * Performs naming on the given namespace.
    */
  private def visitNamespace(decl: DesugaredAst.Declaration.Namespace, ns0: Name.NName)(implicit flix: Flix): Validation[NamedAst.Declaration.Namespace, NameError] = decl match {
    case DesugaredAst.Declaration.Namespace(ident, usesAndImports0, decls0, loc) =>
      val ns = Name.NName(ident.sp1, ns0.idents :+ ident, ident.sp2)
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
  private def tableUnit(unit: NamedAst.CompilationUnit, table0: SymbolTable): Validation[SymbolTable, NameError] = unit match {
    case NamedAst.CompilationUnit(usesAndImports, decls, loc) =>
      fold(decls, table0) {
        case (acc, decl) => tableDecl(decl, acc)
      }
  }

  private def tableDecl(decl: NamedAst.Declaration, table0: SymbolTable): Validation[SymbolTable, NameError] = decl match {
    case NamedAst.Declaration.Namespace(sym, usesAndImports, decls, _) =>
      // Add the namespace to the table (no validation needed)
      val table1 = addDeclToTable(table0, sym.ns.init, sym.ns.last, decl)
      val table2Val = fold(decls, table1) {
        case (table, d) => tableDecl(d, table)
      }
      mapN(table2Val)(addUsesToTable(_, sym.ns, usesAndImports))

    case NamedAst.Declaration.Trait(doc, ann, mod, sym, tparam, superTraits, assocs, sigs, laws, loc) =>
      val table1Val = tryAddToTable(table0, sym.namespace, sym.name, decl)
      flatMapN(table1Val) {
        case table1 => fold(assocs ++ sigs, table1) {
          case (table, d) => tableDecl(d, table)
        }
      }

    case inst@NamedAst.Declaration.Instance(doc, ann, mod, clazz, tparams, tpe, tconstrs, assocs, defs, ns, loc) =>
      Validation.success(addInstanceToTable(table0, ns, clazz.ident.name, inst))

    case NamedAst.Declaration.Sig(sym, spec, exp) =>
      tryAddToTable(table0, sym.namespace, sym.name, decl)

    case NamedAst.Declaration.Def(sym, spec, exp) =>
      tryAddToTable(table0, sym.namespace, sym.name, decl)

    case NamedAst.Declaration.Enum(doc, ann, mod, sym, tparams, derives, cases, loc) =>
      val table1Val = tryAddToTable(table0, sym.namespace, sym.name, decl)
      flatMapN(table1Val) {
        case table1 => fold(cases, table1) {
          case (table, d) => tableDecl(d, table)
        }
      }

    case NamedAst.Declaration.RestrictableEnum(doc, ann, mod, sym, index, tparams, derives, cases, loc) =>
      val table1Val = tryAddToTable(table0, sym.namespace, sym.name, decl)
      flatMapN(table1Val) {
        case table1 => fold(cases, table1) {
          case (table, d) => tableDecl(d, table)
        }
      }

    case NamedAst.Declaration.TypeAlias(doc, _, mod, sym, tparams, tpe, loc) =>
      tryAddToTable(table0, sym.namespace, sym.name, decl)

    case NamedAst.Declaration.AssocTypeSig(doc, mod, sym, tparams, kind, tpe, loc) =>
      tryAddToTable(table0, sym.namespace, sym.name, decl)

    case NamedAst.Declaration.Effect(doc, ann, mod, sym, ops, loc) =>
      val table1Val = tryAddToTable(table0, sym.namespace, sym.name, decl)
      flatMapN(table1Val) {
        case table1 => fold(ops, table1) {
          case (table, d) => tableDecl(d, table)
        }
      }

    case NamedAst.Declaration.Op(sym, spec) =>
      tryAddToTable(table0, sym.namespace, sym.name, decl)

    case caze@NamedAst.Declaration.Case(sym, _, _) =>
      tryAddToTable(table0, sym.namespace, sym.name, caze)

    case caze@NamedAst.Declaration.RestrictableCase(sym, _, _) =>
      tryAddToTable(table0, sym.namespace, sym.name, caze)

    case NamedAst.Declaration.AssocTypeDef(doc, mod, ident, args, tpe, loc) =>
      throw InternalCompilerException("unexpected tabling of associated type definition", loc)

  }

  /**
    * Tries to add the given declaration to the table.
    */
  private def tryAddToTable(table: SymbolTable, ns: List[String], name: String, decl: NamedAst.Declaration): Validation[SymbolTable, NameError] = {
    lookupName(name, ns, table) match {
      case LookupResult.NotDefined => Validation.success(addDeclToTable(table, ns, name, decl))
      case LookupResult.AlreadyDefined(loc) => mkDuplicateNamePair(name, getSymLocation(decl), loc)
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
  private def mkDuplicateNamePair[T](name: String, loc1: SourceLocation, loc2: SourceLocation): Validation.HardFailure[T, NameError] = {
    // NB: We report an error at both source locations.
    if (name.charAt(0).isUpper) {
      // Case 1: uppercase name
      HardFailure(Chain(
        NameError.DuplicateUpperName(name, loc1, loc2),
        NameError.DuplicateUpperName(name, loc2, loc1)
      ))
    } else {
      // Case 2: lowercase name
      HardFailure(Chain(
        NameError.DuplicateLowerName(name, loc1, loc2),
        NameError.DuplicateLowerName(name, loc2, loc1)
      ))
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
  private def visitConstraint(c0: DesugaredAst.Constraint, ns0: Name.NName)(implicit flix: Flix): Validation[NamedAst.Constraint, NameError] = c0 match {
    case DesugaredAst.Constraint(h, bs, loc) =>

      // Introduce a symbol for every unique ident in the body, removing wildcards
      val idents = bs.flatMap {
        case DesugaredAst.Predicate.Body.Atom(_, _, _, _, terms, _) => terms.flatMap(freeVars)
        case DesugaredAst.Predicate.Body.Functional(idents, _, _) => idents
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
  private def visitEnum(enum0: DesugaredAst.Declaration.Enum, ns0: Name.NName)(implicit flix: Flix): Validation[NamedAst.Declaration.Enum, NameError] = enum0 match {
    case DesugaredAst.Declaration.Enum(doc, ann, mod0, ident, tparams0, derives0, cases0, loc) =>
      val sym = Symbol.mkEnumSym(ns0, ident)

      // Compute the type parameters.
      val tparams = getTypeParams(tparams0)

      val mod = visitModifiers(mod0, ns0)
      val derives = visitDerivations(derives0)
      val casesVal = traverse(cases0)(visitCase(_, sym))

      mapN(casesVal) {
        case cases =>
          NamedAst.Declaration.Enum(doc, ann, mod, sym, tparams, derives, cases, loc)
      }
  }

  /**
    * Performs naming on the given enum `enum0`.
    */
  private def visitRestrictableEnum(enum0: DesugaredAst.Declaration.RestrictableEnum, ns0: Name.NName)(implicit flix: Flix): Validation[NamedAst.Declaration.RestrictableEnum, NameError] = enum0 match {
    case DesugaredAst.Declaration.RestrictableEnum(doc, ann, mod0, ident, index0, tparams0, derives0, cases0, loc) =>
      val caseIdents = cases0.map(_.ident)
      val sym = Symbol.mkRestrictableEnumSym(ns0, ident, caseIdents)

      // Compute the type parameters.
      val index = getTypeParam(index0)
      val tparams = getTypeParams(tparams0)

      val mod = visitModifiers(mod0, ns0)
      val derives = visitDerivations(derives0)
      val casesVal = traverse(cases0)(visitRestrictableCase(_, sym))

      mapN(casesVal) {
        case cases =>
          NamedAst.Declaration.RestrictableEnum(doc, ann, mod, sym, index, tparams, derives, cases, loc)
      }
  }

  /**
    * Performs naming on the given enum derivations.
    */
  private def visitDerivations(derives0: DesugaredAst.Derivations): NamedAst.Derivations =
    NamedAst.Derivations(derives0.classes, derives0.loc)

  /**
    * Performs naming on the given enum case.
    */
  private def visitCase(case0: DesugaredAst.Case, enumSym: Symbol.EnumSym)(implicit flix: Flix): Validation[NamedAst.Declaration.Case, NameError] = case0 match {
    case DesugaredAst.Case(ident, tpe0, loc) =>
      mapN(visitType(tpe0)) {
        case tpe =>
          val caseSym = Symbol.mkCaseSym(enumSym, ident)
          NamedAst.Declaration.Case(caseSym, tpe, loc)
      }
  }

  /**
    * Performs naming on the given enum case.
    */
  private def visitRestrictableCase(case0: DesugaredAst.RestrictableCase, enumSym: Symbol.RestrictableEnumSym)(implicit flix: Flix): Validation[NamedAst.Declaration.RestrictableCase, NameError] = case0 match {
    case DesugaredAst.RestrictableCase(ident, tpe0, loc) =>
      mapN(visitType(tpe0)) {
        case tpe =>
          val caseSym = Symbol.mkRestrictableCaseSym(enumSym, ident)
          NamedAst.Declaration.RestrictableCase(caseSym, tpe, loc)
      }
  }

  /**
    * Performs naming on the given type alias `alias0`.
    */
  private def visitTypeAlias(alias0: DesugaredAst.Declaration.TypeAlias, ns0: Name.NName)(implicit flix: Flix): Validation[NamedAst.Declaration.TypeAlias, NameError] = alias0 match {
    case DesugaredAst.Declaration.TypeAlias(doc, ann, mod0, ident, tparams0, tpe0, loc) =>
      val mod = visitModifiers(mod0, ns0)
      val tparams = getTypeParams(tparams0)
      mapN(visitType(tpe0)) {
        tpe =>
          val sym = Symbol.mkTypeAliasSym(ns0, ident)
          NamedAst.Declaration.TypeAlias(doc, ann, mod, sym, tparams, tpe, loc)
      }
  }

  /**
    * Performs naming on the given associated type signature `s0`.
    */
  private def visitAssocTypeSig(s0: DesugaredAst.Declaration.AssocTypeSig, clazz: Symbol.TraitSym, ns0: Name.NName)(implicit flix: Flix): Validation[NamedAst.Declaration.AssocTypeSig, NameError] = s0 match {
    case DesugaredAst.Declaration.AssocTypeSig(doc, mod, ident, tparams0, kind0, tpe0, loc) =>
      val sym = Symbol.mkAssocTypeSym(clazz, ident)
      val tparam = getTypeParam(tparams0)
      val kind = visitKind(kind0)
      val tpeVal = traverseOpt(tpe0)(visitType)
      mapN(tpeVal) {
        case tpe =>
          NamedAst.Declaration.AssocTypeSig(doc, mod, sym, tparam, kind, tpe, loc)
      }
  }

  /**
    * Performs naming on the given associated type definition `d0`.
    */
  private def visitAssocTypeDef(d0: DesugaredAst.Declaration.AssocTypeDef, ns0: Name.NName)(implicit flix: Flix): Validation[NamedAst.Declaration.AssocTypeDef, NameError] = d0 match {
    case DesugaredAst.Declaration.AssocTypeDef(doc, mod, ident, arg0, tpe0, loc) =>
      val argVal = visitType(arg0)
      val tpeVal = visitType(tpe0)
      mapN(argVal, tpeVal) {
        case (arg, tpe) => NamedAst.Declaration.AssocTypeDef(doc, mod, ident, arg, tpe, loc)
      }
  }

  /**
    * Performs naming on the given trait `trt`.
    */
  private def visitTrait(trt: DesugaredAst.Declaration.Trait, ns0: Name.NName)(implicit flix: Flix): Validation[NamedAst.Declaration.Trait, NameError] = trt match {
    case DesugaredAst.Declaration.Trait(doc, ann, mod0, ident, tparams0, superTraits0, assocs0, signatures, laws0, loc) =>
      val sym = Symbol.mkTraitSym(ns0, ident)
      val mod = visitModifiers(mod0, ns0)
      val tparam = getTypeParam(tparams0)

      val superTraitsVal = traverse(superTraits0)(visitTypeConstraint(_, ns0))
      val assocsVal = traverse(assocs0)(visitAssocTypeSig(_, sym, ns0)) // TODO switch param order to match visitSig
      val sigsVal = traverse(signatures)(visitSig(_, ns0, sym))
      val lawsVal = traverse(laws0)(visitDef(_, ns0, DefKind.Member))

      mapN(superTraitsVal, assocsVal, sigsVal, lawsVal) {
        case (superTraits, assocs, sigs, laws) =>
          NamedAst.Declaration.Trait(doc, ann, mod, sym, tparam, superTraits, assocs, sigs, laws, loc)
      }
  }

  /**
    * Performs naming on the given instance `instance`.
    */
  private def visitInstance(instance: DesugaredAst.Declaration.Instance, ns0: Name.NName)(implicit flix: Flix): Validation[NamedAst.Declaration.Instance, NameError] = instance match {
    case DesugaredAst.Declaration.Instance(doc, ann, mod, clazz, tpe0, tconstrs0, assocs0, defs0, loc) =>
      val tparams = getImplicitTypeParamsFromTypes(List(tpe0))

      val tpeVal = visitType(tpe0)
      val tconstrsVal = traverse(tconstrs0)(visitTypeConstraint(_, ns0))
      val assocsVal = traverse(assocs0)(visitAssocTypeDef(_, ns0))
      flatMapN(tpeVal, tconstrsVal, assocsVal) {
        case (tpe, tconstrs, assocs) =>
          val defsVal = traverse(defs0)(visitDef(_, ns0, DefKind.Member))
          mapN(defsVal) {
            defs => NamedAst.Declaration.Instance(doc, ann, mod, clazz, tparams, tpe, tconstrs, assocs, defs, ns0.parts, loc)
          }
      }
  }


  /**
    * Performs naming on the given type constraint `tconstr`.
    */
  private def visitTypeConstraint(tconstr: DesugaredAst.TypeConstraint, ns0: Name.NName)(implicit flix: Flix): Validation[NamedAst.TypeConstraint, NameError] = tconstr match {
    case DesugaredAst.TypeConstraint(clazz, tparam0, loc) =>
      mapN(visitType(tparam0)) {
        tparam => NamedAst.TypeConstraint(clazz, tparam, loc)
      }
  }

  /**
    * Performs naming on the given equality constraint `econstr`.
    */
  private def visitEqualityConstraint(econstr: DesugaredAst.EqualityConstraint, ns0: Name.NName)(implicit flix: Flix): Validation[NamedAst.EqualityConstraint, NameError] = econstr match {
    case DesugaredAst.EqualityConstraint(qname, tpe1, tpe2, loc) =>
      val t1Val = visitType(tpe1)
      val t2Val = visitType(tpe2)
      mapN(t1Val, t2Val) {
        case (t1, t2) => NamedAst.EqualityConstraint(qname, t1, t2, loc)
      }
  }

  /**
    * Performs naming on the given signature declaration `sig`.
    */
  private def visitSig(sig: DesugaredAst.Declaration.Sig, ns0: Name.NName, traitSym: Symbol.TraitSym)(implicit flix: Flix): Validation[NamedAst.Declaration.Sig, NameError] = sig match {
    case DesugaredAst.Declaration.Sig(doc, ann, mod0, ident, tparams0, fparams0, exp0, tpe0, eff0, tconstrs0, econstrs0, loc) =>
      val tparams = getTypeParamsFromFormalParams(tparams0, fparams0, tpe0, eff0, econstrs0)

      // First visit all the top-level information
      val mod = visitModifiers(mod0, ns0)
      val fparamsVal = getFormalParams(fparams0)
      val tpeVal = visitType(tpe0)
      val effVal = traverseOpt(eff0)(visitType)
      val tconstrsVal = traverse(tconstrs0)(visitTypeConstraint(_, ns0))
      val econstrsVal = traverse(econstrs0)(visitEqualityConstraint(_, ns0))

      flatMapN(fparamsVal, tpeVal, effVal, tconstrsVal, econstrsVal) {
        case (fparams, tpe, eff, tconstrs, econstrs) =>

          // Then visit the parts depending on the parameters
          val expVal = traverseOpt(exp0)(visitExp(_, ns0))

          mapN(expVal) {
            case exp =>

              val sym = Symbol.mkSigSym(traitSym, ident)
              val spec = NamedAst.Spec(doc, ann, mod, tparams, fparams, tpe, eff, tconstrs, econstrs, loc)
              NamedAst.Declaration.Sig(sym, spec, exp)
          }
      }
  }

  /**
    * Performs naming on the given definition declaration `decl0`.
    */
  private def visitDef(decl0: DesugaredAst.Declaration.Def, ns0: Name.NName, defKind: DefKind)(implicit flix: Flix): Validation[NamedAst.Declaration.Def, NameError] = decl0 match {
    case DesugaredAst.Declaration.Def(doc, ann, mod0, ident, tparams0, fparams0, exp, tpe0, eff0, tconstrs0, econstrs0, loc) =>
      flix.subtask(ident.name, sample = true)

      val tparams = getTypeParamsFromFormalParams(tparams0, fparams0, tpe0, eff0, econstrs0)

      // First visit all the top-level information
      val mod = visitModifiers(mod0, ns0)
      val fparamsVal = getFormalParams(fparams0)
      val tpeVal = visitType(tpe0)
      val effVal = traverseOpt(eff0)(visitType)
      val tconstrsVal = traverse(tconstrs0)(visitTypeConstraint(_, ns0))
      val econstrsVal = traverse(econstrs0)(visitEqualityConstraint(_, ns0))

      flatMapN(fparamsVal, tpeVal, effVal, tconstrsVal, econstrsVal) {
        case (fparams, tpe, eff, tconstrs, econstrs) =>

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
              val spec = NamedAst.Spec(doc, ann, mod, tparams, fparams, tpe, eff, tconstrs, econstrs, loc)
              NamedAst.Declaration.Def(sym, spec, e)
          }
      }
  }

  /**
    * Performs naming on the given effect `eff0`.
    */
  private def visitEffect(eff0: DesugaredAst.Declaration.Effect, ns0: Name.NName)(implicit flix: Flix): Validation[NamedAst.Declaration.Effect, NameError] = eff0 match {
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
  private def visitOp(op0: DesugaredAst.Declaration.Op, ns0: Name.NName, effSym: Symbol.EffectSym)(implicit flix: Flix): Validation[NamedAst.Declaration.Op, NameError] = op0 match {
    case DesugaredAst.Declaration.Op(doc, ann, mod0, ident, fparams0, tpe0, tconstrs0, loc) =>
      // First visit all the top-level information
      val mod = visitModifiers(mod0, ns0)
      val fparamsVal = getFormalParams(fparams0)
      val tpeVal = visitType(tpe0)
      val tconstrsVal = traverse(tconstrs0)(visitTypeConstraint(_, ns0))

      mapN(fparamsVal, tpeVal, tconstrsVal) {
        case (fparams, tpe, tconstrs) =>
          val tparams = NamedAst.TypeParams.Kinded(Nil) // operations are monomorphic
          val eff = None // operations are pure
          val econstrs = Nil // TODO ASSOC-TYPES allow econstrs here

          val sym = Symbol.mkOpSym(effSym, ident)
          val spec = NamedAst.Spec(doc, ann, mod, tparams, fparams, tpe, eff, tconstrs, econstrs, loc)
          NamedAst.Declaration.Op(sym, spec)
      }
  }

  /**
    * Performs naming on the given expression `exp0`.
    */
  // TODO NS-REFACTOR can remove ns0 too?
  private def visitExp(exp0: DesugaredAst.Expr, ns0: Name.NName)(implicit flix: Flix): Validation[NamedAst.Expr, NameError] = exp0 match {

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

    case DesugaredAst.Expr.Lambda(fparam0, exp, loc) =>
      mapN(visitFormalParam(fparam0): Validation[NamedAst.FormalParam, NameError], visitExp(exp, ns0)) {
        case (p, e) => NamedAst.Expr.Lambda(p, e, loc)
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
          mapN(visitType(tpe): Validation[NamedAst.Type, NameError], visitExp(body, ns0)) {
            case (t, b) => NamedAst.TypeMatchRule(sym, t, b)
          }
      }
      mapN(expVal, rulesVal) {
        case (e, rs) => NamedAst.Expr.TypeMatch(e, rs, loc)
      }

    case DesugaredAst.Expr.RestrictableChoose(star, exp, rules, loc) =>
      val expVal = visitExp(exp, ns0)
      val rulesVal = traverse(rules) {
        case DesugaredAst.RestrictableChooseRule(pat0, exp0) =>
          val p = visitRestrictablePattern(pat0)
          val eVal = visitExp(exp0, ns0)
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

    case DesugaredAst.Expr.Ascribe(exp, expectedType, expectedEff, loc) =>
      val expVal = visitExp(exp, ns0)
      val expectedTypVal = traverseOpt(expectedType)(visitType)
      val expectedEffVal = traverseOpt(expectedEff)(visitType)

      mapN(expVal, expectedTypVal, expectedEffVal) {
        case (e, t, f) => NamedAst.Expr.Ascribe(e, t, f, loc)
      }

    case DesugaredAst.Expr.InstanceOf(exp, className, loc) =>
      mapN(visitExp(exp, ns0)) {
        case e => NamedAst.Expr.InstanceOf(e, className, loc)
      }

    case DesugaredAst.Expr.CheckedCast(c, exp, loc) =>
      mapN(visitExp(exp, ns0)) {
        case e => NamedAst.Expr.CheckedCast(c, e, loc)
      }

    case DesugaredAst.Expr.UncheckedCast(exp, declaredType, declaredEff, loc) =>
      val expVal = visitExp(exp, ns0)
      val declaredTypVal = traverseOpt(declaredType)(visitType)
      val declaredEffVal = traverseOpt(declaredEff)(visitType)

      mapN(expVal, declaredTypVal, declaredEffVal) {
        case (e, t, f) => NamedAst.Expr.UncheckedCast(e, t, f, loc)
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
        case DesugaredAst.HandlerRule(op, fparams0, body0) =>
          val fparamsVal = traverse(fparams0)(visitFormalParam): Validation[List[NamedAst.FormalParam], NameError]
          val bodyVal = visitExp(body0, ns0)
          mapN(fparamsVal, bodyVal) {
            (fparams, body) => NamedAst.HandlerRule(op, fparams, body)
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

    case DesugaredAst.Expr.InvokeConstructor(className, exps, sig, loc) =>
      val argsVal = traverse(exps)(visitExp(_, ns0))
      val sigVal = traverse(sig)(visitType): Validation[List[NamedAst.Type], NameError]
      mapN(argsVal, sigVal) {
        case (as, sig) => NamedAst.Expr.InvokeConstructor(className, as, sig, loc)
      }

    case DesugaredAst.Expr.InvokeMethod(className, methodName, exp, exps, sig, retTpe, loc) =>
      val expVal = visitExp(exp, ns0)
      val argsVal = traverse(exps)(visitExp(_, ns0))
      val sigVal = traverse(sig)(visitType): Validation[List[NamedAst.Type], NameError]
      val retVal = visitType(retTpe): Validation[NamedAst.Type, NameError]
      mapN(expVal, argsVal, sigVal, retVal) {
        case (e, as, sig, ret) => NamedAst.Expr.InvokeMethod(className, methodName, e, as, sig, ret, loc)
      }

    case DesugaredAst.Expr.InvokeStaticMethod(className, methodName, exps, sig, retTpe, loc) =>
      val argsVal = traverse(exps)(visitExp(_, ns0))
      val sigVal = traverse(sig)(visitType): Validation[List[NamedAst.Type], NameError]
      val retVal = visitType(retTpe): Validation[NamedAst.Type, NameError]
      mapN(argsVal, sigVal, retVal) {
        case (as, sig, ret) => NamedAst.Expr.InvokeStaticMethod(className, methodName, as, sig, ret, loc)
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
      mapN(visitType(tpe): Validation[NamedAst.Type, NameError], traverse(methods)(visitJvmMethod(_, ns0))) {
        case (tpe, ms) =>
          val name = s"Anon$$${flix.genSym.freshId()}"
          NamedAst.Expr.NewObject(name, tpe, ms, loc)
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
        case Some(exp) => mapN(visitExp(exp, ns0)) {
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
      val psVal = traverse(pparams)(visitPredicateParam): Validation[List[NamedAst.PredicateParam], NameError]
      val expVal = visitExp(exp, ns0)
      mapN(psVal, expVal) {
        case (ps, e) => NamedAst.Expr.FixpointLambda(ps, e, loc)
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
  private def visitPattern(pat0: DesugaredAst.Pattern)(implicit flix: Flix): NamedAst.Pattern = pat0 match {
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
  private def visitRestrictablePattern(pat0: DesugaredAst.RestrictableChoosePattern)(implicit flix: Flix): NamedAst.RestrictableChoosePattern = {
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
  private def visitHeadPredicate(head: DesugaredAst.Predicate.Head, ns0: Name.NName)(implicit flix: Flix): Validation[NamedAst.Predicate.Head, NameError] = head match {
    case DesugaredAst.Predicate.Head.Atom(pred, den, exps, loc) =>
      val expsVal = traverse(exps)(t => visitExp(t, ns0))
      mapN(expsVal) {
        case es => NamedAst.Predicate.Head.Atom(pred, den, es, loc)
      }
  }

  /**
    * Names the given body predicate `body`.
    */
  private def visitBodyPredicate(body: DesugaredAst.Predicate.Body, ns0: Name.NName)(implicit flix: Flix): Validation[NamedAst.Predicate.Body, NameError] = body match {
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
    * Names the given type `tpe`.
    */
  private def visitType(t0: DesugaredAst.Type)(implicit flix: Flix): Validation[NamedAst.Type, NameError] = {
    def visit(tpe0: DesugaredAst.Type): Validation[NamedAst.Type, NameError] = tpe0 match {
      case DesugaredAst.Type.Unit(loc) =>
        Validation.success(NamedAst.Type.Unit(loc))

      case DesugaredAst.Type.Var(ident, loc) =>
        //
        // Check for [[NameError.SuspiciousTypeVarName]].
        //
        if (isSuspiciousTypeVarName(ident.name)) {
          // TODO NS-REFACTOR maybe check this at declaration site instead of use site
          Validation.toSoftFailure(NamedAst.Type.Var(ident, loc), NameError.SuspiciousTypeVarName(ident.name, loc))
        } else {
          Validation.success(NamedAst.Type.Var(ident, loc))
        }

      case DesugaredAst.Type.Ambiguous(qname, loc) =>
        Validation.success(NamedAst.Type.Ambiguous(qname, loc))

      case DesugaredAst.Type.Tuple(elms, loc) =>
        mapN(traverse(elms)(visit)) {
          case ts => NamedAst.Type.Tuple(ts, loc)
        }

      case DesugaredAst.Type.RecordRowEmpty(loc) =>
        Validation.success(NamedAst.Type.RecordRowEmpty(loc))

      case DesugaredAst.Type.RecordRowExtend(label, value, rest, loc) =>
        mapN(visit(value), visit(rest)) {
          case (t, r) => NamedAst.Type.RecordRowExtend(label, t, r, loc)
        }

      case DesugaredAst.Type.Record(row, loc) =>
        mapN(visit(row)) {
          r => NamedAst.Type.Record(r, loc)
        }

      case DesugaredAst.Type.SchemaRowEmpty(loc) =>
        Validation.success(NamedAst.Type.SchemaRowEmpty(loc))

      case DesugaredAst.Type.SchemaRowExtendByAlias(qname, targs, rest, loc) =>
        mapN(traverse(targs)(visit), visit(rest)) {
          case (ts, r) => NamedAst.Type.SchemaRowExtendWithAlias(qname, ts, r, loc)
        }

      case DesugaredAst.Type.SchemaRowExtendByTypes(ident, den, tpes, rest, loc) =>
        mapN(traverse(tpes)(visit), visit(rest)) {
          case (ts, r) => NamedAst.Type.SchemaRowExtendWithTypes(ident, den, ts, r, loc)
        }

      case DesugaredAst.Type.Schema(row, loc) =>
        mapN(visit(row)) {
          r => NamedAst.Type.Schema(r, loc)
        }

      case DesugaredAst.Type.Native(fqn, loc) =>
        Validation.success(NamedAst.Type.Native(fqn, loc))

      case DesugaredAst.Type.Arrow(tparams0, eff0, tresult0, loc) =>
        val tparamsVal = traverse(tparams0)(visit)
        val effVal = traverseOpt(eff0)(visitType)
        val tresultVal = visit(tresult0)
        mapN(tparamsVal, effVal, tresultVal) {
          case (tparams, eff, tresult) => NamedAst.Type.Arrow(tparams, eff, tresult, loc)
        }

      case DesugaredAst.Type.Apply(tpe1, tpe2, loc) =>
        mapN(visit(tpe1), visit(tpe2)) {
          case (t1, t2) => NamedAst.Type.Apply(t1, t2, loc)
        }

      case DesugaredAst.Type.True(loc) =>
        Validation.success(NamedAst.Type.True(loc))

      case DesugaredAst.Type.False(loc) =>
        Validation.success(NamedAst.Type.False(loc))

      case DesugaredAst.Type.Not(tpe, loc) =>
        mapN(visit(tpe)) {
          case t => NamedAst.Type.Not(t, loc)
        }

      case DesugaredAst.Type.And(tpe1, tpe2, loc) =>
        mapN(visit(tpe1), visit(tpe2)) {
          case (t1, t2) => NamedAst.Type.And(t1, t2, loc)
        }

      case DesugaredAst.Type.Or(tpe1, tpe2, loc) =>
        mapN(visit(tpe1), visit(tpe2)) {
          case (t1, t2) => NamedAst.Type.Or(t1, t2, loc)
        }

      case DesugaredAst.Type.Complement(tpe, loc) =>
        mapN(visit(tpe)) {
          case t => NamedAst.Type.Complement(t, loc)
        }

      case DesugaredAst.Type.Union(tpe1, tpe2, loc) =>
        mapN(visit(tpe1), visit(tpe2)) {
          case (t1, t2) => NamedAst.Type.Union(t1, t2, loc)
        }

      case DesugaredAst.Type.Intersection(tpe1, tpe2, loc) =>
        mapN(visit(tpe1), visit(tpe2)) {
          case (t1, t2) => NamedAst.Type.Intersection(t1, t2, loc)
        }

      case DesugaredAst.Type.Pure(loc) =>
        Validation.success(NamedAst.Type.Pure(loc))

      case DesugaredAst.Type.CaseSet(cases, loc) =>
        Validation.success(NamedAst.Type.CaseSet(cases, loc))

      case DesugaredAst.Type.CaseComplement(tpe, loc) =>
        mapN(visitType(tpe)) {
          case t => NamedAst.Type.CaseComplement(t, loc)
        }

      case DesugaredAst.Type.CaseUnion(tpe1, tpe2, loc) =>
        mapN(visit(tpe1), visit(tpe2)) {
          case (t1, t2) => NamedAst.Type.CaseUnion(t1, t2, loc)
        }

      case DesugaredAst.Type.CaseIntersection(tpe1, tpe2, loc) =>
        mapN(visit(tpe1), visit(tpe2)) {
          case (t1, t2) => NamedAst.Type.CaseIntersection(t1, t2, loc)
        }

      case DesugaredAst.Type.Ascribe(tpe, kind0, loc) =>
        val kind = visitKind(kind0)
        mapN(visit(tpe)) {
          t => NamedAst.Type.Ascribe(t, kind, loc)
        }

      case DesugaredAst.Type.Error(loc) =>
        Validation.success(NamedAst.Type.Error(loc))
    }

    visit(t0)
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
    case DesugaredAst.Pattern.Var(ident, loc) => List(ident)
    case DesugaredAst.Pattern.Wild(loc) => Nil
    case DesugaredAst.Pattern.Cst(Ast.Constant.Unit, loc) => Nil
    case DesugaredAst.Pattern.Cst(Ast.Constant.Bool(true), loc) => Nil
    case DesugaredAst.Pattern.Cst(Ast.Constant.Bool(false), loc) => Nil
    case DesugaredAst.Pattern.Cst(Ast.Constant.Char(lit), loc) => Nil
    case DesugaredAst.Pattern.Cst(Ast.Constant.Float32(lit), loc) => Nil
    case DesugaredAst.Pattern.Cst(Ast.Constant.Float64(lit), loc) => Nil
    case DesugaredAst.Pattern.Cst(Ast.Constant.BigDecimal(lit), loc) => Nil
    case DesugaredAst.Pattern.Cst(Ast.Constant.Int8(lit), loc) => Nil
    case DesugaredAst.Pattern.Cst(Ast.Constant.Int16(lit), loc) => Nil
    case DesugaredAst.Pattern.Cst(Ast.Constant.Int32(lit), loc) => Nil
    case DesugaredAst.Pattern.Cst(Ast.Constant.Int64(lit), loc) => Nil
    case DesugaredAst.Pattern.Cst(Ast.Constant.BigInt(lit), loc) => Nil
    case DesugaredAst.Pattern.Cst(Ast.Constant.Str(lit), loc) => Nil
    case DesugaredAst.Pattern.Cst(Ast.Constant.Regex(lit), loc) => Nil
    case DesugaredAst.Pattern.Cst(Ast.Constant.Null, loc) => throw InternalCompilerException("unexpected null pattern", loc)
    case DesugaredAst.Pattern.Tag(qname, p, loc) => freeVars(p)
    case DesugaredAst.Pattern.Tuple(elms, loc) => elms.flatMap(freeVars)
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
    case DesugaredAst.Type.Var(ident, loc) => ident :: Nil
    case DesugaredAst.Type.Ambiguous(qname, loc) => Nil
    case DesugaredAst.Type.Unit(loc) => Nil
    case DesugaredAst.Type.Tuple(elms, loc) => elms.flatMap(freeTypeVars)
    case DesugaredAst.Type.RecordRowEmpty(loc) => Nil
    case DesugaredAst.Type.RecordRowExtend(l, t, r, loc) => freeTypeVars(t) ::: freeTypeVars(r)
    case DesugaredAst.Type.Record(row, loc) => freeTypeVars(row)
    case DesugaredAst.Type.SchemaRowEmpty(loc) => Nil
    case DesugaredAst.Type.SchemaRowExtendByTypes(_, _, ts, r, loc) => ts.flatMap(freeTypeVars) ::: freeTypeVars(r)
    case DesugaredAst.Type.SchemaRowExtendByAlias(_, ts, r, _) => ts.flatMap(freeTypeVars) ::: freeTypeVars(r)
    case DesugaredAst.Type.Schema(row, loc) => freeTypeVars(row)
    case DesugaredAst.Type.Native(fqm, loc) => Nil
    case DesugaredAst.Type.Arrow(tparams, eff, tresult, loc) => tparams.flatMap(freeTypeVars) ::: eff.toList.flatMap(freeTypeVars) ::: freeTypeVars(tresult)
    case DesugaredAst.Type.Apply(tpe1, tpe2, loc) => freeTypeVars(tpe1) ++ freeTypeVars(tpe2)
    case DesugaredAst.Type.True(loc) => Nil
    case DesugaredAst.Type.False(loc) => Nil
    case DesugaredAst.Type.Not(tpe, loc) => freeTypeVars(tpe)
    case DesugaredAst.Type.And(tpe1, tpe2, loc) => freeTypeVars(tpe1) ++ freeTypeVars(tpe2)
    case DesugaredAst.Type.Or(tpe1, tpe2, loc) => freeTypeVars(tpe1) ++ freeTypeVars(tpe2)
    case DesugaredAst.Type.Complement(tpe, loc) => freeTypeVars(tpe)
    case DesugaredAst.Type.Union(tpe1, tpe2, loc) => freeTypeVars(tpe1) ++ freeTypeVars(tpe2)
    case DesugaredAst.Type.Intersection(tpe1, tpe2, loc) => freeTypeVars(tpe1) ++ freeTypeVars(tpe2)
    case DesugaredAst.Type.Pure(_) => Nil
    case DesugaredAst.Type.CaseSet(_, _) => Nil
    case DesugaredAst.Type.CaseComplement(tpe, loc) => freeTypeVars(tpe)
    case DesugaredAst.Type.CaseUnion(tpe1, tpe2, loc) => freeTypeVars(tpe1) ++ freeTypeVars(tpe2)
    case DesugaredAst.Type.CaseIntersection(tpe1, tpe2, loc) => freeTypeVars(tpe1) ++ freeTypeVars(tpe2)
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
  private def visitFormalParam(fparam: DesugaredAst.FormalParam)(implicit flix: Flix): Validation[NamedAst.FormalParam, NameError] = fparam match {
    case DesugaredAst.FormalParam(ident, mod, optType, loc) =>
      // Generate a fresh variable symbol for the identifier.
      val freshSym = Symbol.freshVarSym(ident, BoundBy.FormalParam)

      // Compute the type of the formal parameter or use the type variable of the symbol.
      val tpeVal = traverseOpt(optType)(visitType)

      // Construct the formal parameter.
      mapN(tpeVal) {
        case tpe => NamedAst.FormalParam(freshSym, mod, tpe, loc)
      }
  }

  /**
    * Translates the given weeded predicate parameter to a named predicate parameter.
    */
  private def visitPredicateParam(pparam: DesugaredAst.PredicateParam)(implicit flix: Flix): Validation[NamedAst.PredicateParam, NameError] = pparam match {
    case DesugaredAst.PredicateParam.PredicateParamUntyped(pred, loc) =>
      Validation.success(NamedAst.PredicateParam.PredicateParamUntyped(pred, loc))

    case DesugaredAst.PredicateParam.PredicateParamWithType(pred, den, tpes, loc) =>
      mapN(traverse(tpes)(visitType)) {
        case ts => NamedAst.PredicateParam.PredicateParamWithType(pred, den, ts, loc)
      }
  }

  /**
    * Translates the given weeded JvmMethod to a named JvmMethod.
    */
  private def visitJvmMethod(method: DesugaredAst.JvmMethod, ns0: Name.NName)(implicit flix: Flix): Validation[NamedAst.JvmMethod, NameError] = method match {
    case DesugaredAst.JvmMethod(ident, fparams0, exp0, tpe0, eff0, loc) =>
      flatMapN(traverse(fparams0)(visitFormalParam): Validation[List[NamedAst.FormalParam], NameError]) {
        case fparams =>
          val exp = visitExp(exp0, ns0)
          val tpe = visitType(tpe0)
          val eff = traverseOpt(eff0)(visitType)
          mapN(exp, tpe, eff) {
            case (e, t, p) => NamedAst.JvmMethod(ident, fparams, e, t, p, loc)
          }
      }: Validation[NamedAst.JvmMethod, NameError]
  }

  /**
    * Performs naming on the given formal parameters `fparam0`.
    */
  private def getFormalParams(fparams0: List[DesugaredAst.FormalParam])(implicit flix: Flix): Validation[List[NamedAst.FormalParam], NameError] = {
    traverse(fparams0)(visitFormalParam)
  }


  /**
    * Performs naming on the given type parameter.
    */
  private def getTypeParam(tparam0: DesugaredAst.TypeParam)(implicit flix: Flix): NamedAst.TypeParam = tparam0 match {
    case DesugaredAst.TypeParam.Kinded(ident, kind) =>
      NamedAst.TypeParam.Kinded(ident, mkTypeVarSym(ident), visitKind(kind), ident.loc)
    case DesugaredAst.TypeParam.Unkinded(ident) =>
      NamedAst.TypeParam.Unkinded(ident, mkTypeVarSym(ident), ident.loc)
  }

  /**
    * Performs naming on the given type parameters `tparam0` from the given cases `cases`.
    */
  private def getTypeParams(tparams0: DesugaredAst.TypeParams)(implicit flix: Flix): NamedAst.TypeParams = {
    tparams0 match {
      case DesugaredAst.TypeParams.Elided => NamedAst.TypeParams.Kinded(Nil)
      case DesugaredAst.TypeParams.Unkinded(tparams) => getExplicitTypeParams(tparams)
      case DesugaredAst.TypeParams.Kinded(tparams) => getExplicitKindedTypeParams(tparams)
    }
  }


  /**
    * Performs naming on the given type parameters `tparams0` from the given formal params `fparams` and overall type `tpe`.
    */
  private def getTypeParamsFromFormalParams(tparams0: DesugaredAst.TypeParams, fparams: List[DesugaredAst.FormalParam], tpe: DesugaredAst.Type, eff: Option[DesugaredAst.Type], econstrs: List[DesugaredAst.EqualityConstraint])(implicit flix: Flix): NamedAst.TypeParams = {
    tparams0 match {
      case DesugaredAst.TypeParams.Elided => getImplicitTypeParamsFromFormalParams(fparams, tpe, eff, econstrs)
      case DesugaredAst.TypeParams.Unkinded(tparams) => getExplicitTypeParams(tparams)
      case DesugaredAst.TypeParams.Kinded(tparams) => getExplicitKindedTypeParams(tparams)

    }
  }

  /**
    * Names the explicit kinded type params.
    */
  private def getExplicitKindedTypeParams(tparams0: List[DesugaredAst.TypeParam.Kinded])(implicit flix: Flix): NamedAst.TypeParams.Kinded = {
    val tparams = tparams0.map {
      case DesugaredAst.TypeParam.Kinded(ident, kind) =>
        NamedAst.TypeParam.Kinded(ident, mkTypeVarSym(ident), visitKind(kind), ident.loc)
    }
    NamedAst.TypeParams.Kinded(tparams)
  }

  /**
    * Returns the explicit unkinded type parameters from the given type parameter names and implicit type parameters.
    */
  private def getExplicitTypeParams(tparams0: List[DesugaredAst.TypeParam.Unkinded])(implicit flix: Flix): NamedAst.TypeParams.Unkinded = {
    val tparams = tparams0.map {
      case DesugaredAst.TypeParam.Unkinded(ident) =>
        NamedAst.TypeParam.Unkinded(ident, mkTypeVarSym(ident), ident.loc)
    }
    NamedAst.TypeParams.Unkinded(tparams)
  }

  /**
    * Returns the implicit type parameters constructed from the given types.
    */
  private def getImplicitTypeParamsFromTypes(types: List[DesugaredAst.Type])(implicit flix: Flix): NamedAst.TypeParams.Implicit = {
    val tvars = types.flatMap(freeTypeVars).distinct
    val tparams = tvars.map {
      ident => NamedAst.TypeParam.Implicit(ident, mkTypeVarSym(ident), ident.loc)
    }
    NamedAst.TypeParams.Implicit(tparams)
  }

  /**
    * Returns the implicit type parameters constructed from the given formal parameters and type.
    */
  private def getImplicitTypeParamsFromFormalParams(fparams: List[DesugaredAst.FormalParam], tpe: DesugaredAst.Type, eff: Option[DesugaredAst.Type], econstrs: List[DesugaredAst.EqualityConstraint])(implicit flix: Flix): NamedAst.TypeParams = {
    // Compute the type variables that occur in the formal parameters.
    val fparamTvars = fparams.flatMap {
      case DesugaredAst.FormalParam(_, _, Some(tpe), _) => freeTypeVars(tpe)
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
    case NamedAst.Declaration.Trait(doc, ann, mod, sym, tparam, superTraits, assocs, sigs, laws, loc) => sym.loc
    case NamedAst.Declaration.Sig(sym, spec, exp) => sym.loc
    case NamedAst.Declaration.Def(sym, spec, exp) => sym.loc
    case NamedAst.Declaration.Enum(doc, ann, mod, sym, tparams, derives, cases, loc) => sym.loc
    case NamedAst.Declaration.RestrictableEnum(doc, ann, mod, sym, ident, tparams, derives, cases, loc) => sym.loc
    case NamedAst.Declaration.TypeAlias(doc, _, mod, sym, tparams, tpe, loc) => sym.loc
    case NamedAst.Declaration.Effect(doc, ann, mod, sym, ops, loc) => sym.loc
    case NamedAst.Declaration.Op(sym, spec) => sym.loc
    case NamedAst.Declaration.Case(sym, tpe, _) => sym.loc
    case NamedAst.Declaration.RestrictableCase(sym, tpe, _) => sym.loc
    case NamedAst.Declaration.AssocTypeSig(doc, mod, sym, tparams, kind, tpe, loc) => sym.loc
    case NamedAst.Declaration.AssocTypeDef(doc, mod, ident, args, tpe, loc) => throw InternalCompilerException("Unexpected associated type definition", loc)
    case NamedAst.Declaration.Instance(doc, ann, mod, clazz, tparams, tpe, tconstrs, assocs, defs, ns, loc) => throw InternalCompilerException("Unexpected instance", loc)
    case NamedAst.Declaration.Namespace(sym, usesAndImports, decls, loc) => throw InternalCompilerException("Unexpected namespace", loc)
  }

  /**
    * Creates a flexible unkinded type variable symbol from the given ident.
    */
  private def mkTypeVarSym(ident: Name.Ident)(implicit flix: Flix): Symbol.UnkindedTypeVarSym = {
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
}

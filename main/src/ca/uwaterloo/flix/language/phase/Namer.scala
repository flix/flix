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
import ca.uwaterloo.flix.language.ast.WeededAst.RestrictableChoicePattern
import ca.uwaterloo.flix.language.ast.{NamedAst, _}
import ca.uwaterloo.flix.language.errors.NameError
import ca.uwaterloo.flix.util.Validation._
import ca.uwaterloo.flix.util.collection.ListMap
import ca.uwaterloo.flix.util.{InternalCompilerException, Validation}

/**
  * The Namer phase introduces unique symbols for each syntactic entity in the program.
  */
object Namer {

  /**
    * Introduces unique names for each syntactic entity in the given `program`.
    * */
  def run(program: WeededAst.Root)(implicit flix: Flix): Validation[NamedAst.Root, NameError] = flix.phase("Namer") {
    // compute all the source locations
    val locations = program.units.values.foldLeft(Map.empty[Source, SourceLocation]) {
      case (macc, root) => macc + (root.loc.source -> root.loc)
    }

    val unitsVal = traverseValues(program.units)(visitUnit)

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
  private def visitUnit(unit: WeededAst.CompilationUnit)(implicit flix: Flix): Validation[NamedAst.CompilationUnit, NameError] = unit match {
    case WeededAst.CompilationUnit(usesAndImports0, decls0, loc) =>
      val usesAndImports = usesAndImports0.map(visitUseOrImport)
      val declsVal = traverse(decls0)(visitDecl(_, Name.RootNS))
      mapN(declsVal) {
        case decls => NamedAst.CompilationUnit(usesAndImports, decls, loc)
      }
  }

  /**
    * Performs naming on the given declaration.
    */
  private def visitDecl(decl0: WeededAst.Declaration, ns0: Name.NName)(implicit flix: Flix): Validation[NamedAst.Declaration, NameError] = decl0 match {
    case decl: WeededAst.Declaration.Namespace => visitNamespace(decl, ns0)
    case decl: WeededAst.Declaration.Class => visitClass(decl, ns0)
    case decl: WeededAst.Declaration.Instance => visitInstance(decl, ns0)
    case decl: WeededAst.Declaration.Def => visitDef(decl, ns0)
    case decl: WeededAst.Declaration.Enum => visitEnum(decl, ns0)
    case decl: WeededAst.Declaration.RestrictableEnum => visitRestrictableEnum(decl, ns0)
    case decl: WeededAst.Declaration.TypeAlias => visitTypeAlias(decl, ns0)
    case decl: WeededAst.Declaration.Effect => visitEffect(decl, ns0)
    case decl: WeededAst.Declaration.Law => throw InternalCompilerException("unexpected law", decl.loc)
  }

  /**
    * Performs naming on the given namespace.
    */
  private def visitNamespace(decl: WeededAst.Declaration.Namespace, ns0: Name.NName)(implicit flix: Flix): Validation[NamedAst.Declaration.Namespace, NameError] = decl match {
    case WeededAst.Declaration.Namespace(ident, usesAndImports0, decls0, loc) =>
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

    case NamedAst.Declaration.Class(doc, ann, mod, sym, tparam, superClasses, assocs, sigs, laws, loc) =>
      val table1Val = tryAddToTable(table0, sym.namespace, sym.name, decl)
      flatMapN(table1Val) {
        case table1 => fold(assocs ++ sigs, table1) {
          case (table, d) => tableDecl(d, table)
        }
      }

    case inst@NamedAst.Declaration.Instance(doc, ann, mod, clazz, tparams, tpe, tconstrs, assocs, defs, ns, loc) =>
      addInstanceToTable(table0, ns, clazz.ident.name, inst).toSuccess

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

    case NamedAst.Declaration.TypeAlias(doc, mod, sym, tparams, tpe, loc) =>
      tryAddToTable(table0, sym.namespace, sym.name, decl)

    case NamedAst.Declaration.AssocTypeSig(doc, mod, sym, tparams, kind, loc) =>
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
      // TODO RESTR-VARS add to case table?
      tryAddToTable(table0, sym.namespace, sym.name, caze)

    case NamedAst.Declaration.AssocTypeDef(doc, mod, ident, args, tpe, loc) =>
      throw InternalCompilerException("unexpected tabling of associated type definition", loc)

  }

  /**
    * Tries to add the given declaration to the table.
    */
  private def tryAddToTable(table: SymbolTable, ns: List[String], name: String, decl: NamedAst.Declaration): Validation[SymbolTable, NameError] = {
    lookupName(name, ns, table) match {
      case LookupResult.NotDefined => addDeclToTable(table, ns, name, decl).toSuccess
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
  private def mkDuplicateNamePair[T](name: String, loc1: SourceLocation, loc2: SourceLocation): Validation.Failure[T, NameError] = {
    // NB: We report an error at both source locations.
    if (name.charAt(0).isUpper) {
      // Case 1: uppercase name
      Failure(LazyList(
        NameError.DuplicateUpperName(name, loc1, loc2),
        NameError.DuplicateUpperName(name, loc2, loc1)
      ))
    } else {
      // Case 2: lowercase name
      Failure(LazyList(
        NameError.DuplicateLowerName(name, loc1, loc2),
        NameError.DuplicateLowerName(name, loc2, loc1)
      ))
    }
  }

  /**
    * The result of looking up a type or class name in an ast root.
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
  private def visitConstraint(c0: WeededAst.Constraint, ns0: Name.NName)(implicit flix: Flix): Validation[NamedAst.Constraint, NameError] = c0 match {
    case WeededAst.Constraint(h, bs, loc) =>

      // Introduce a symbol for every unique ident in the body, removing wildcards
      val idents = bs.flatMap {
        case WeededAst.Predicate.Body.Atom(_, _, _, _, terms, _) => terms.flatMap(freeVars)
        case WeededAst.Predicate.Body.Functional(idents, _, _) => idents
        case WeededAst.Predicate.Body.Guard(_, _) => Nil
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
  private def visitEnum(enum0: WeededAst.Declaration.Enum, ns0: Name.NName)(implicit flix: Flix): Validation[NamedAst.Declaration.Enum, NameError] = enum0 match {
    case WeededAst.Declaration.Enum(doc, ann, mod0, ident, tparams0, derives, cases0, loc) =>
      val sym = Symbol.mkEnumSym(ns0, ident)

      // Compute the type parameters.
      val tparams = getTypeParams(tparams0)

      val mod = visitModifiers(mod0, ns0)
      val casesVal = traverse(cases0)(visitCase(_, sym))

      mapN(casesVal) {
        case cases =>
          NamedAst.Declaration.Enum(doc, ann, mod, sym, tparams, derives, cases, loc)
      }
  }

  /**
    * Performs naming on the given enum `enum0`.
    */
  private def visitRestrictableEnum(enum0: WeededAst.Declaration.RestrictableEnum, ns0: Name.NName)(implicit flix: Flix): Validation[NamedAst.Declaration.RestrictableEnum, NameError] = enum0 match {
    case WeededAst.Declaration.RestrictableEnum(doc, ann, mod0, ident, index0, tparams0, derives, cases0, loc) =>
      val caseIdents = cases0.map(_.ident)
      val sym = Symbol.mkRestrictableEnumSym(ns0, ident, caseIdents)

      // Compute the type parameters.
      val index = getTypeParam(index0)
      val tparams = getTypeParams(tparams0)

      val mod = visitModifiers(mod0, ns0)
      val casesVal = traverse(cases0)(visitRestrictableCase(_, sym))

      mapN(casesVal) {
        case cases =>
          NamedAst.Declaration.RestrictableEnum(doc, ann, mod, sym, index, tparams, derives, cases, loc)
      }
  }

  /**
    * Performs naming on the given enum case.
    */
  private def visitCase(case0: WeededAst.Case, enumSym: Symbol.EnumSym)(implicit flix: Flix): Validation[NamedAst.Declaration.Case, NameError] = case0 match {
    case WeededAst.Case(ident, tpe0, loc) =>
      mapN(visitType(tpe0)) {
        case tpe =>
          val caseSym = Symbol.mkCaseSym(enumSym, ident)
          NamedAst.Declaration.Case(caseSym, tpe, loc)
      }
  }

  /**
    * Performs naming on the given enum case.
    */
  private def visitRestrictableCase(case0: WeededAst.RestrictableCase, enumSym: Symbol.RestrictableEnumSym)(implicit flix: Flix): Validation[NamedAst.Declaration.RestrictableCase, NameError] = case0 match {
    case WeededAst.RestrictableCase(ident, tpe0, loc) =>
      mapN(visitType(tpe0)) {
        case tpe =>
          val caseSym = Symbol.mkRestrictableCaseSym(enumSym, ident)
          NamedAst.Declaration.RestrictableCase(caseSym, tpe, loc)
      }
  }

  /**
    * Performs naming on the given type alias `alias0`.
    */
  private def visitTypeAlias(alias0: WeededAst.Declaration.TypeAlias, ns0: Name.NName)(implicit flix: Flix): Validation[NamedAst.Declaration.TypeAlias, NameError] = alias0 match {
    case WeededAst.Declaration.TypeAlias(doc, mod0, ident, tparams0, tpe0, loc) =>
      val mod = visitModifiers(mod0, ns0)
      val tparams = getTypeParams(tparams0)
      mapN(visitType(tpe0)) {
        tpe =>
          val sym = Symbol.mkTypeAliasSym(ns0, ident)
          NamedAst.Declaration.TypeAlias(doc, mod, sym, tparams, tpe, loc)
      }
  }

  /**
    * Performs naming on the given associated type signature `s0`.
    */
  private def visitAssocTypeSig(s0: WeededAst.Declaration.AssocTypeSig, clazz: Symbol.ClassSym, ns0: Name.NName)(implicit flix: Flix): Validation[NamedAst.Declaration.AssocTypeSig, NameError] = s0 match {
    case WeededAst.Declaration.AssocTypeSig(doc, mod, ident, tparams0, kind0, loc) =>
      val sym = Symbol.mkAssocTypeSym(clazz, ident)
      val tparam = getTypeParam(tparams0)
      val kind = visitKind(kind0)
      NamedAst.Declaration.AssocTypeSig(doc, mod, sym, tparam, kind, loc).toSuccess
  }

  /**
    * Performs naming on the given associated type definition `d0`.
    */
  private def visitAssocTypeDef(d0: WeededAst.Declaration.AssocTypeDef, ns0: Name.NName)(implicit flix: Flix): Validation[NamedAst.Declaration.AssocTypeDef, NameError] = d0 match {
    case WeededAst.Declaration.AssocTypeDef(doc, mod, ident, arg0, tpe0, loc) =>
      val argVal = visitType(arg0)
      val tpeVal = visitType(tpe0)
      mapN(argVal, tpeVal) {
        case (arg, tpe) => NamedAst.Declaration.AssocTypeDef(doc, mod, ident, arg, tpe, loc)
      }
  }

  /**
    * Performs naming on the given class `clazz`.
    */
  private def visitClass(clazz: WeededAst.Declaration.Class, ns0: Name.NName)(implicit flix: Flix): Validation[NamedAst.Declaration.Class, NameError] = clazz match {
    case WeededAst.Declaration.Class(doc, ann, mod0, ident, tparams0, superClasses0, assocs0, signatures, laws0, loc) =>
      val sym = Symbol.mkClassSym(ns0, ident)
      val mod = visitModifiers(mod0, ns0)
      val tparam = getTypeParam(tparams0)

      val superClassesVal = traverse(superClasses0)(visitTypeConstraint(_, ns0))
      val assocsVal = traverse(assocs0)(visitAssocTypeSig(_, sym, ns0)) // TODO switch param order to match visitSig
      val sigsVal = traverse(signatures)(visitSig(_, ns0, sym))
      val lawsVal = traverse(laws0)(visitDef(_, ns0))

      mapN(superClassesVal, assocsVal, sigsVal, lawsVal) {
        case (superClasses, assocs, sigs, laws) =>
          NamedAst.Declaration.Class(doc, ann, mod, sym, tparam, superClasses, assocs, sigs, laws, loc)
      }
  }

  /**
    * Performs naming on the given instance `instance`.
    */
  private def visitInstance(instance: WeededAst.Declaration.Instance, ns0: Name.NName)(implicit flix: Flix): Validation[NamedAst.Declaration.Instance, NameError] = instance match {
    case WeededAst.Declaration.Instance(doc, ann, mod, clazz, tpe0, tconstrs0, assocs0, defs0, loc) =>
      val tparams = getImplicitTypeParamsFromTypes(List(tpe0))

      val tpeVal = visitType(tpe0)
      val tconstrsVal = traverse(tconstrs0)(visitTypeConstraint(_, ns0))
      val assocsVal = traverse(assocs0)(visitAssocTypeDef(_, ns0))
      flatMapN(tpeVal, tconstrsVal, assocsVal) {
        case (tpe, tconstrs, assocs) =>
          val defsVal = traverse(defs0)(visitDef(_, ns0))
          mapN(defsVal) {
            defs => NamedAst.Declaration.Instance(doc, ann, mod, clazz, tparams, tpe, tconstrs, assocs, defs, ns0.parts, loc)
          }
      }
  }


  /**
    * Performs naming on the given type constraint `tconstr`.
    */
  private def visitTypeConstraint(tconstr: WeededAst.TypeConstraint, ns0: Name.NName)(implicit flix: Flix): Validation[NamedAst.TypeConstraint, NameError] = tconstr match {
    case WeededAst.TypeConstraint(clazz, tparam0, loc) =>
      mapN(visitType(tparam0)) {
        tparam => NamedAst.TypeConstraint(clazz, tparam, loc)
      }
  }

  /**
    * Performs naming on the given equality constraint `econstr`.
    */
  private def visitEqualityConstraint(econstr: WeededAst.EqualityConstraint, ns0: Name.NName)(implicit flix: Flix): Validation[NamedAst.EqualityConstraint, NameError] = econstr match {
    case WeededAst.EqualityConstraint(qname, tpe1, tpe2, loc) =>
      val t1Val = visitType(tpe1)
      val t2Val = visitType(tpe2)
      mapN(t1Val, t2Val) {
        case (t1, t2) => NamedAst.EqualityConstraint(qname, t1, t2, loc)
      }
  }

  /**
    * Performs naming on the given signature declaration `sig`.
    */
  private def visitSig(sig: WeededAst.Declaration.Sig, ns0: Name.NName, classSym: Symbol.ClassSym)(implicit flix: Flix): Validation[NamedAst.Declaration.Sig, NameError] = sig match {
    case WeededAst.Declaration.Sig(doc, ann, mod0, ident, tparams0, fparams0, exp0, tpe0, purAndEff0, tconstrs0, loc) =>
      val tparams = getTypeParamsFromFormalParams(tparams0, fparams0, tpe0, purAndEff0)

      // First visit all the top-level information
      val mod = visitModifiers(mod0, ns0)
      val fparamsVal = getFormalParams(fparams0)
      val tpeVal = visitType(tpe0)
      val purAndEffVal = visitPurityAndEffect(purAndEff0)
      val tconstrsVal = traverse(tconstrs0)(visitTypeConstraint(_, ns0))

      flatMapN(fparamsVal, tpeVal, purAndEffVal, tconstrsVal) {
        case (fparams, tpe, purAndEff, tconstrs) =>
          val econstrs = Nil // TODO ASSOC-TYPES allow eq-constrs here

          // Then visit the parts depending on the parameters
          val expVal = traverseOpt(exp0)(visitExp(_, ns0))

          mapN(expVal) {
            case exp =>

              val sym = Symbol.mkSigSym(classSym, ident)
              val spec = NamedAst.Spec(doc, ann, mod, tparams, fparams, tpe, purAndEff, tconstrs, econstrs, loc)
              NamedAst.Declaration.Sig(sym, spec, exp)
          }
      }
  }

  /**
    * Performs naming on the given definition declaration `decl0`.
    */
  private def visitDef(decl0: WeededAst.Declaration.Def, ns0: Name.NName)(implicit flix: Flix): Validation[NamedAst.Declaration.Def, NameError] = decl0 match {
    case WeededAst.Declaration.Def(doc, ann, mod0, ident, tparams0, fparams0, exp, tpe0, purAndEff0, tconstrs0, econstrs0, loc) =>
      flix.subtask(ident.name, sample = true)

      val tparams = getTypeParamsFromFormalParams(tparams0, fparams0, tpe0, purAndEff0)

      // First visit all the top-level information
      val mod = visitModifiers(mod0, ns0)
      val fparamsVal = getFormalParams(fparams0)
      val tpeVal = visitType(tpe0)
      val purAndEffVal = visitPurityAndEffect(purAndEff0)
      val tconstrsVal = traverse(tconstrs0)(visitTypeConstraint(_, ns0))
      val econstrsVal = traverse(econstrs0)(visitEqualityConstraint(_, ns0))

      flatMapN(fparamsVal, tpeVal, purAndEffVal, tconstrsVal, econstrsVal) {
        case (fparams, tpe, purAndEff, tconstrs, econstrs) =>

          // Then visit the parts depending on the parameters
          val expVal = visitExp(exp, ns0)

          mapN(expVal) {
            case e =>

              val sym = Symbol.mkDefnSym(ns0, ident)
              val spec = NamedAst.Spec(doc, ann, mod, tparams, fparams, tpe, purAndEff, tconstrs, econstrs, loc)
              NamedAst.Declaration.Def(sym, spec, e)
          }
      }
  }

  /**
    * Performs naming on the given effect `eff0`.
    */
  private def visitEffect(eff0: WeededAst.Declaration.Effect, ns0: Name.NName)(implicit flix: Flix): Validation[NamedAst.Declaration.Effect, NameError] = eff0 match {
    case WeededAst.Declaration.Effect(doc, ann, mod0, ident, ops0, loc) =>
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
  private def visitOp(op0: WeededAst.Declaration.Op, ns0: Name.NName, effSym: Symbol.EffectSym)(implicit flix: Flix): Validation[NamedAst.Declaration.Op, NameError] = op0 match {
    case WeededAst.Declaration.Op(doc, ann, mod0, ident, fparams0, tpe0, tconstrs0, loc) =>
      // First visit all the top-level information
      val mod = visitModifiers(mod0, ns0)
      val fparamsVal = getFormalParams(fparams0)
      val tpeVal = visitType(tpe0)
      val tconstrsVal = traverse(tconstrs0)(visitTypeConstraint(_, ns0))

      mapN(fparamsVal, tpeVal, tconstrsVal) {
        case (fparams, tpe, tconstrs) =>
          val tparams = NamedAst.TypeParams.Kinded(Nil) // operations are monomorphic
          val purAndEff = NamedAst.PurityAndEffect(None, None) // operations are pure
          val econstrs = Nil // TODO ASSOC-TYPES allow econstrs here

          val sym = Symbol.mkOpSym(effSym, ident)
          val spec = NamedAst.Spec(doc, ann, mod, tparams, fparams, tpe, purAndEff, tconstrs, econstrs, loc)
          NamedAst.Declaration.Op(sym, spec)
      }
  }

  /**
    * Performs naming on the given expression `exp0`.
    */
  // TODO NS-REFACTOR can remove ns0 too?
  private def visitExp(exp0: WeededAst.Expression, ns0: Name.NName)(implicit flix: Flix): Validation[NamedAst.Expression, NameError] = exp0 match {

    case WeededAst.Expression.Wild(loc) =>
      NamedAst.Expression.Wild(loc).toSuccess

    case WeededAst.Expression.Ambiguous(name, loc) =>
      NamedAst.Expression.Ambiguous(name, loc).toSuccess

    case WeededAst.Expression.OpenAs(name, exp, loc) =>
      mapN(visitExp(exp, ns0)) {
        case e => NamedAst.Expression.OpenAs(name, e, loc)
      }

    case WeededAst.Expression.Open(name, loc) =>
      NamedAst.Expression.Open(name, loc).toSuccess

    case WeededAst.Expression.Hole(name, loc) =>
      NamedAst.Expression.Hole(name, loc).toSuccess

    case WeededAst.Expression.HoleWithExp(exp, loc) =>
      mapN(visitExp(exp, ns0)) {
        case e => NamedAst.Expression.HoleWithExp(e, loc)
      }

    case WeededAst.Expression.Use(uses0, exp, loc) =>
      val uses = uses0.map(visitUseOrImport)

      mapN(visitExp(exp, ns0)) {
        case e => uses.foldRight(e) {
          case (use, acc) => NamedAst.Expression.Use(use, acc, loc)
        }
      }

    case WeededAst.Expression.Cst(cst, loc) => NamedAst.Expression.Cst(cst, loc).toSuccess

    case WeededAst.Expression.Apply(exp, exps, loc) =>
      mapN(visitExp(exp, ns0), traverse(exps)(visitExp(_, ns0))) {
        case (e, es) => NamedAst.Expression.Apply(e, es, loc)
      }

    case WeededAst.Expression.Lambda(fparam0, exp, loc) =>
      mapN(visitFormalParam(fparam0): Validation[NamedAst.FormalParam, NameError], visitExp(exp, ns0)) {
        case (p, e) => NamedAst.Expression.Lambda(p, e, loc)
      }.recoverOne {
        case err: NameError.TypeNameError => NamedAst.Expression.Error(err)
      }

    case WeededAst.Expression.Unary(sop, exp, loc) =>
      visitExp(exp, ns0) map {
        case e => NamedAst.Expression.Unary(sop, e, loc)
      }

    case WeededAst.Expression.Binary(sop, exp1, exp2, loc) =>
      mapN(visitExp(exp1, ns0), visitExp(exp2, ns0)) {
        case (e1, e2) => NamedAst.Expression.Binary(sop, e1, e2, loc)
      }

    case WeededAst.Expression.IfThenElse(exp1, exp2, exp3, loc) =>
      val e1 = visitExp(exp1, ns0)
      val e2 = visitExp(exp2, ns0)
      val e3 = visitExp(exp3, ns0)
      mapN(e1, e2, e3) {
        NamedAst.Expression.IfThenElse(_, _, _, loc)
      }

    case WeededAst.Expression.Stm(exp1, exp2, loc) =>
      val e1 = visitExp(exp1, ns0)
      val e2 = visitExp(exp2, ns0)
      mapN(e1, e2) {
        NamedAst.Expression.Stm(_, _, loc)
      }

    case WeededAst.Expression.Discard(exp, loc) =>
      visitExp(exp, ns0) map {
        case e => NamedAst.Expression.Discard(e, loc)
      }

    case WeededAst.Expression.Let(ident, mod, exp1, exp2, loc) =>
      // make a fresh variable symbol for the local variable.
      val sym = Symbol.freshVarSym(ident, BoundBy.Let)
      mapN(visitExp(exp1, ns0), visitExp(exp2, ns0)) {
        case (e1, e2) => NamedAst.Expression.Let(sym, mod, e1, e2, loc)
      }

    case WeededAst.Expression.LetRec(ident, mod, exp1, exp2, loc) =>
      val sym = Symbol.freshVarSym(ident, BoundBy.Let)
      mapN(visitExp(exp1, ns0), visitExp(exp2, ns0)) {
        case (e1, e2) => NamedAst.Expression.LetRec(sym, mod, e1, e2, loc)
      }

    case WeededAst.Expression.Region(tpe, loc) =>
      NamedAst.Expression.Region(tpe, loc).toSuccess

    case WeededAst.Expression.Scope(ident, exp, loc) =>
      // Introduce a fresh variable symbol for the region.
      val sym = Symbol.freshVarSym(ident, BoundBy.Let)

      // Introduce a rigid region variable for the region.
      val regionVar = Symbol.freshUnkindedTypeVarSym(Ast.VarText.SourceText(sym.text), isRegion = true, loc)

      mapN(visitExp(exp, ns0)) {
        case e => NamedAst.Expression.Scope(sym, regionVar, e, loc)
      }

    case WeededAst.Expression.ScopeExit(exp1, exp2, loc) =>
      mapN(visitExp(exp1, ns0), visitExp(exp2, ns0)) {
        case (e1, e2) => NamedAst.Expression.ScopeExit(e1, e2, loc)
      }

    case WeededAst.Expression.Match(exp, rules, loc) =>
      val expVal = visitExp(exp, ns0)
      val rulesVal = traverse(rules) {
        case WeededAst.MatchRule(pat, exp1, exp2) =>
          val p = visitPattern(pat)
          val e1Val = traverseOpt(exp1)(visitExp(_, ns0))
          val e2Val = visitExp(exp2, ns0)
          mapN(e1Val, e2Val) {
            case (e1, e2) => NamedAst.MatchRule(p, e1, e2)
          }
      }
      mapN(expVal, rulesVal) {
        case (e, rs) => NamedAst.Expression.Match(e, rs, loc)
      }

    case WeededAst.Expression.TypeMatch(exp, rules, loc) =>
      val expVal = visitExp(exp, ns0)
      val rulesVal = traverse(rules) {
        case WeededAst.MatchTypeRule(ident, tpe, body) =>
          val sym = Symbol.freshVarSym(ident, BoundBy.Pattern)
          mapN(visitType(tpe): Validation[NamedAst.Type, NameError], visitExp(body, ns0)) {
            case (t, b) => NamedAst.MatchTypeRule(sym, t, b)
          }
      }
      mapN(expVal, rulesVal) {
        case (e, rs) => NamedAst.Expression.TypeMatch(e, rs, loc)
      }.recoverOne {
        case err: NameError.TypeNameError => NamedAst.Expression.Error(err)
      }

    case WeededAst.Expression.RelationalChoose(star, exps, rules, loc) =>
      val expsVal = traverse(exps)(visitExp(_, ns0))
      val rulesVal = traverse(rules) {
        case WeededAst.RelationalChoiceRule(pat0, exp0) =>
          val p = pat0.map {
            case WeededAst.RelationalChoicePattern.Wild(loc) => NamedAst.RelationalChoicePattern.Wild(loc)
            case WeededAst.RelationalChoicePattern.Absent(loc) => NamedAst.RelationalChoicePattern.Absent(loc)
            case WeededAst.RelationalChoicePattern.Present(ident, loc) =>
              val sym = Symbol.freshVarSym(ident, BoundBy.Pattern)
              NamedAst.RelationalChoicePattern.Present(sym, loc)
          }
          mapN(visitExp(exp0, ns0)) {
            case e => NamedAst.RelationalChoiceRule(p, e)
          }
      }
      mapN(expsVal, rulesVal) {
        case (es, rs) => NamedAst.Expression.RelationalChoose(star, es, rs, loc)
      }

    case WeededAst.Expression.RestrictableChoose(star, exp, rules, loc) =>
      val expVal = visitExp(exp, ns0)
      val rulesVal = traverse(rules) {
        case WeededAst.RestrictableChoiceRule(pat0, exp0) =>
          val p = visitRestrictablePattern(pat0)
          val eVal = visitExp(exp0, ns0)
          mapN(eVal) {
            case e => NamedAst.RestrictableChoiceRule(p, e)
          }
      }
      mapN(expVal, rulesVal) {
        case (es, rs) => NamedAst.Expression.RestrictableChoose(star, es, rs, loc)
      }

    case WeededAst.Expression.Tuple(exps, loc) =>
      traverse(exps)(e => visitExp(e, ns0)) map {
        case es => NamedAst.Expression.Tuple(es, loc)
      }

    case WeededAst.Expression.RecordEmpty(loc) =>
      NamedAst.Expression.RecordEmpty(loc).toSuccess

    case WeededAst.Expression.RecordSelect(exp, field, loc) =>
      mapN(visitExp(exp, ns0)) {
        case e => NamedAst.Expression.RecordSelect(e, field, loc)
      }

    case WeededAst.Expression.RecordExtend(field, exp1, exp2, loc) =>
      mapN(visitExp(exp1, ns0), visitExp(exp2, ns0)) {
        case (v, r) => NamedAst.Expression.RecordExtend(field, v, r, loc)
      }

    case WeededAst.Expression.RecordRestrict(field, exp, loc) =>
      mapN(visitExp(exp, ns0)) {
        case r => NamedAst.Expression.RecordRestrict(field, r, loc)
      }

    case WeededAst.Expression.ArrayLit(exps, exp, loc) =>
      mapN(traverse(exps)(visitExp(_, ns0)), visitExp(exp, ns0)) {
        case (es, e) => NamedAst.Expression.ArrayLit(es, e, loc)
      }

    case WeededAst.Expression.ArrayNew(exp1, exp2, exp3, loc) =>
      mapN(visitExp(exp1, ns0), visitExp(exp2, ns0), visitExp(exp3, ns0)) {
        case (e1, e2, e3) => NamedAst.Expression.ArrayNew(e1, e2, e3, loc)
      }

    case WeededAst.Expression.ArrayLoad(exp1, exp2, loc) =>
      mapN(visitExp(exp1, ns0), visitExp(exp2, ns0)) {
        case (e1, e2) => NamedAst.Expression.ArrayLoad(e1, e2, loc)
      }

    case WeededAst.Expression.ArrayStore(exp1, exp2, exp3, loc) =>
      mapN(visitExp(exp1, ns0), visitExp(exp2, ns0), visitExp(exp3, ns0)) {
        case (e1, e2, e3) => NamedAst.Expression.ArrayStore(e1, e2, e3, loc)
      }

    case WeededAst.Expression.ArrayLength(exp, loc) =>
      visitExp(exp, ns0) map {
        case e => NamedAst.Expression.ArrayLength(e, loc)
      }

    case WeededAst.Expression.VectorLit(exps, loc) =>
      mapN(traverse(exps)(visitExp(_, ns0))) {
        case es => NamedAst.Expression.VectorLit(es, loc)
      }

    case WeededAst.Expression.VectorLoad(exp1, exp2, loc) =>
      mapN(visitExp(exp1, ns0), visitExp(exp2, ns0)) {
        case (e1, e2) => NamedAst.Expression.VectorLoad(e1, e2, loc)
      }

    case WeededAst.Expression.VectorLength(exp, loc) =>
      visitExp(exp, ns0) map {
        case e => NamedAst.Expression.VectorLength(e, loc)
      }

    case WeededAst.Expression.Ref(exp1, exp2, loc) =>
      mapN(visitExp(exp1, ns0), visitExp(exp2, ns0)) {
        case (e1, e2) =>
          NamedAst.Expression.Ref(e1, e2, loc)
      }

    case WeededAst.Expression.Deref(exp, loc) =>
      visitExp(exp, ns0) map {
        case e =>
          NamedAst.Expression.Deref(e, loc)
      }

    case WeededAst.Expression.Assign(exp1, exp2, loc) =>
      mapN(visitExp(exp1, ns0), visitExp(exp2, ns0)) {
        case (e1, e2) =>
          NamedAst.Expression.Assign(e1, e2, loc)
      }

    case WeededAst.Expression.Ascribe(exp, expectedType, expectedEff, loc) =>
      val expVal = visitExp(exp, ns0)
      val expectedTypVal = traverseOpt(expectedType)(visitType)
      val expectedEffVal = visitPurityAndEffect(expectedEff): Validation[NamedAst.PurityAndEffect, NameError]

      mapN(expVal, expectedTypVal, expectedEffVal) {
        case (e, t, f) => NamedAst.Expression.Ascribe(e, t, f, loc)
      }.recoverOne {
        case err: NameError.TypeNameError => NamedAst.Expression.Error(err)
      }

    case WeededAst.Expression.InstanceOf(exp, className, loc) =>
      visitExp(exp, ns0) map {
        case e => NamedAst.Expression.InstanceOf(e, className, loc)
      }

    case WeededAst.Expression.CheckedCast(c, exp, loc) =>
      mapN(visitExp(exp, ns0)) {
        case e => NamedAst.Expression.CheckedCast(c, e, loc)
      }

    case WeededAst.Expression.UncheckedCast(exp, declaredType, declaredEff, loc) =>
      val expVal = visitExp(exp, ns0)
      val declaredTypVal = traverseOpt(declaredType)(visitType)
      val declaredEffVal = visitPurityAndEffect(declaredEff): Validation[NamedAst.PurityAndEffect, NameError]

      mapN(expVal, declaredTypVal, declaredEffVal) {
        case (e, t, f) => NamedAst.Expression.UncheckedCast(e, t, f, loc)
      }.recoverOne {
        case err: NameError.TypeNameError => NamedAst.Expression.Error(err)
      }

    case WeededAst.Expression.UncheckedMaskingCast(exp, loc) =>
      mapN(visitExp(exp, ns0)) {
        case e => NamedAst.Expression.UncheckedMaskingCast(e, loc)
      }

    case WeededAst.Expression.Without(exp, eff, loc) =>
      val expVal = visitExp(exp, ns0)
      mapN(expVal) {
        e => NamedAst.Expression.Without(e, eff, loc)
      }

    case WeededAst.Expression.TryCatch(exp, rules, loc) =>
      val expVal = visitExp(exp, ns0)
      val rulesVal = traverse(rules) {
        case WeededAst.CatchRule(ident, className, body) =>
          val sym = Symbol.freshVarSym(ident, BoundBy.CatchRule)
          val bodyVal = visitExp(body, ns0)
          mapN(bodyVal) {
            b => NamedAst.CatchRule(sym, className, b)
          }
      }

      mapN(expVal, rulesVal) {
        case (e, rs) => NamedAst.Expression.TryCatch(e, rs, loc)
      }

    case WeededAst.Expression.TryWith(e0, eff, rules0, loc) =>
      val eVal = visitExp(e0, ns0)
      val rulesVal = traverse(rules0) {
        case WeededAst.HandlerRule(op, fparams0, body0) =>
          val fparamsVal = traverse(fparams0)(visitFormalParam): Validation[List[NamedAst.FormalParam], NameError]
          val bodyVal = visitExp(body0, ns0)
          mapN(fparamsVal, bodyVal) {
            (fparams, body) => NamedAst.HandlerRule(op, fparams, body)
          }
      }
      mapN(eVal, rulesVal) {
        case (e, rules) => NamedAst.Expression.TryWith(e, eff, rules, loc)
      }.recoverOne {
        case err: NameError.TypeNameError => NamedAst.Expression.Error(err)
      }

    case WeededAst.Expression.Do(op, exps0, loc) =>
      val expsVal = traverse(exps0)(visitExp(_, ns0))
      mapN(expsVal) {
        exps => NamedAst.Expression.Do(op, exps, loc)
      }

    case WeededAst.Expression.Resume(exp, loc) =>
      val expVal = visitExp(exp, ns0)
      mapN(expVal) {
        e => NamedAst.Expression.Resume(e, loc)
      }

    case WeededAst.Expression.InvokeConstructor(className, exps, sig, loc) =>
      val argsVal = traverse(exps)(visitExp(_, ns0))
      val sigVal = traverse(sig)(visitType): Validation[List[NamedAst.Type], NameError]
      mapN(argsVal, sigVal) {
        case (as, sig) => NamedAst.Expression.InvokeConstructor(className, as, sig, loc)
      }.recoverOne {
        case err: NameError.TypeNameError => NamedAst.Expression.Error(err)
      }

    case WeededAst.Expression.InvokeMethod(className, methodName, exp, exps, sig, retTpe, loc) =>
      val expVal = visitExp(exp, ns0)
      val argsVal = traverse(exps)(visitExp(_, ns0))
      val sigVal = traverse(sig)(visitType): Validation[List[NamedAst.Type], NameError]
      val retVal = visitType(retTpe): Validation[NamedAst.Type, NameError]
      mapN(expVal, argsVal, sigVal, retVal) {
        case (e, as, sig, ret) => NamedAst.Expression.InvokeMethod(className, methodName, e, as, sig, ret, loc)
      }.recoverOne {
        case err: NameError.TypeNameError => NamedAst.Expression.Error(err)
      }

    case WeededAst.Expression.InvokeStaticMethod(className, methodName, exps, sig, retTpe, loc) =>
      val argsVal = traverse(exps)(visitExp(_, ns0))
      val sigVal = traverse(sig)(visitType): Validation[List[NamedAst.Type], NameError]
      val retVal = visitType(retTpe): Validation[NamedAst.Type, NameError]
      mapN(argsVal, sigVal, retVal) {
        case (as, sig, ret) => NamedAst.Expression.InvokeStaticMethod(className, methodName, as, sig, ret, loc)
      }.recoverOne {
        case err: NameError.TypeNameError => NamedAst.Expression.Error(err)
      }


    case WeededAst.Expression.GetField(className, fieldName, exp, loc) =>
      mapN(visitExp(exp, ns0)) {
        case e => NamedAst.Expression.GetField(className, fieldName, e, loc)
      }

    case WeededAst.Expression.PutField(className, fieldName, exp1, exp2, loc) =>
      mapN(visitExp(exp1, ns0), visitExp(exp2, ns0)) {
        case (e1, e2) => NamedAst.Expression.PutField(className, fieldName, e1, e2, loc)
      }

    case WeededAst.Expression.GetStaticField(className, fieldName, loc) =>
      NamedAst.Expression.GetStaticField(className, fieldName, loc).toSuccess

    case WeededAst.Expression.PutStaticField(className, fieldName, exp, loc) =>
      mapN(visitExp(exp, ns0)) {
        case e => NamedAst.Expression.PutStaticField(className, fieldName, e, loc)
      }

    case WeededAst.Expression.NewObject(tpe, methods, loc) =>
      mapN(visitType(tpe): Validation[NamedAst.Type, NameError], traverse(methods)(visitJvmMethod(_, ns0))) {
        case (tpe, ms) =>
          val name = s"Anon$$${flix.genSym.freshId()}"
          NamedAst.Expression.NewObject(name, tpe, ms, loc)
      }.recoverOne {
        case err: NameError.TypeNameError => NamedAst.Expression.Error(err)
      }

    case WeededAst.Expression.NewChannel(exp1, exp2, loc) =>
      mapN(visitExp(exp1, ns0), visitExp(exp2, ns0)) {
        case (e1, e2) => NamedAst.Expression.NewChannel(e1, e2, loc)
      }

    case WeededAst.Expression.GetChannel(exp, loc) =>
      visitExp(exp, ns0) map {
        case e => NamedAst.Expression.GetChannel(e, loc)
      }

    case WeededAst.Expression.PutChannel(exp1, exp2, loc) =>
      mapN(visitExp(exp1, ns0), visitExp(exp2, ns0)) {
        case (e1, e2) => NamedAst.Expression.PutChannel(e1, e2, loc)
      }

    case WeededAst.Expression.SelectChannel(rules, exp, loc) =>
      val rulesVal = traverse(rules) {
        case WeededAst.SelectChannelRule(ident, exp1, exp2) =>
          // make a fresh variable symbol for the local recursive variable.
          val sym = Symbol.freshVarSym(ident, BoundBy.SelectRule)
          mapN(visitExp(exp1, ns0), visitExp(exp2, ns0)) {
            case (e1, e2) => NamedAst.SelectChannelRule(sym, e1, e2)
          }
      }

      val defaultVal = exp match {
        case Some(exp) => visitExp(exp, ns0) map {
          case e => Some(e)
        }
        case None => None.toSuccess
      }

      mapN(rulesVal, defaultVal) {
        case (rs, d) => NamedAst.Expression.SelectChannel(rs, d, loc)
      }

    case WeededAst.Expression.Spawn(exp1, exp2, loc) =>
      mapN(visitExp(exp1, ns0), visitExp(exp2, ns0)) {
        case (e1, e2) =>
          NamedAst.Expression.Spawn(e1, e2, loc)
      }

    case WeededAst.Expression.Par(exp, loc) =>
      mapN(visitExp(exp, ns0)) {
        case e => NamedAst.Expression.Par(e, loc)
      }

    case WeededAst.Expression.ParYield(frags, exp, loc) =>
      val fragsVal = traverse(frags) {
        case WeededAst.ParYieldFragment(pat, e, l) =>
          val p = visitPattern(pat)
          mapN(visitExp(e, ns0)) {
            case e1 => NamedAst.ParYieldFragment(p, e1, l)
          }
      }

      // Combine everything
      mapN(fragsVal, visitExp(exp, ns0)) {
        case (fs, e) => NamedAst.Expression.ParYield(fs, e, loc)
      }

    case WeededAst.Expression.Lazy(exp, loc) =>
      visitExp(exp, ns0) map {
        case e => NamedAst.Expression.Lazy(e, loc)
      }

    case WeededAst.Expression.Force(exp, loc) =>
      visitExp(exp, ns0) map {
        case e => NamedAst.Expression.Force(e, loc)
      }

    case WeededAst.Expression.FixpointConstraintSet(cs0, loc) =>
      mapN(traverse(cs0)(visitConstraint(_, ns0))) {
        case cs =>
          NamedAst.Expression.FixpointConstraintSet(cs, loc)
      }

    case WeededAst.Expression.FixpointLambda(pparams, exp, loc) =>
      val psVal = traverse(pparams)(visitPredicateParam): Validation[List[NamedAst.PredicateParam], NameError]
      val expVal = visitExp(exp, ns0)
      mapN(psVal, expVal) {
        case (ps, e) => NamedAst.Expression.FixpointLambda(ps, e, loc)
      }.recoverOne {
        case err: NameError.TypeNameError => NamedAst.Expression.Error(err)
      }

    case WeededAst.Expression.FixpointMerge(exp1, exp2, loc) =>
      mapN(visitExp(exp1, ns0), visitExp(exp2, ns0)) {
        case (e1, e2) => NamedAst.Expression.FixpointMerge(e1, e2, loc)
      }

    case WeededAst.Expression.FixpointSolve(exp, loc) =>
      visitExp(exp, ns0) map {
        case e => NamedAst.Expression.FixpointSolve(e, loc)
      }

    case WeededAst.Expression.FixpointFilter(ident, exp, loc) =>
      mapN(visitExp(exp, ns0)) {
        case e => NamedAst.Expression.FixpointFilter(ident, e, loc)
      }

    case WeededAst.Expression.FixpointInject(exp, pred, loc) =>
      mapN(visitExp(exp, ns0)) {
        case e => NamedAst.Expression.FixpointInject(e, pred, loc)
      }

    case WeededAst.Expression.FixpointProject(pred, exp1, exp2, loc) =>
      mapN(visitExp(exp1, ns0), visitExp(exp2, ns0)) {
        case (e1, e2) => NamedAst.Expression.FixpointProject(pred, e1, e2, loc)
      }

    case WeededAst.Expression.Error(m) =>
      // Note: We must NOT use [[Validation.toSoftFailure]] because
      // that would duplicate the error inside the Validation.
      Validation.SoftFailure(NamedAst.Expression.Error(m), LazyList.empty)

  }

  /**
    * Names the given pattern `pat0`.
    */
  private def visitPattern(pat0: WeededAst.Pattern)(implicit flix: Flix): NamedAst.Pattern = pat0 match {
    case WeededAst.Pattern.Wild(loc) => NamedAst.Pattern.Wild(loc)
    case WeededAst.Pattern.Var(ident, loc) =>
      // make a fresh variable symbol for the local variable.
      val sym = Symbol.freshVarSym(ident, BoundBy.Pattern)
      NamedAst.Pattern.Var(sym, loc)

    case WeededAst.Pattern.Cst(cst, loc) => NamedAst.Pattern.Cst(cst, loc)

    case WeededAst.Pattern.Tag(qname, pat, loc) =>
      NamedAst.Pattern.Tag(qname, visitPattern(pat), loc)

    case WeededAst.Pattern.Tuple(elms, loc) => NamedAst.Pattern.Tuple(elms map visitPattern, loc)

  }

  /**
    * Names the given pattern `pat0`
    */
  private def visitRestrictablePattern(pat0: WeededAst.RestrictableChoicePattern)(implicit flix: Flix): NamedAst.RestrictableChoicePattern = {
    def visitVarPlace(vp: WeededAst.RestrictableChoicePattern.VarOrWild): NamedAst.RestrictableChoicePattern.VarOrWild = vp match {
      case RestrictableChoicePattern.Wild(loc) => NamedAst.RestrictableChoicePattern.Wild(loc)
      case RestrictableChoicePattern.Var(ident, loc) =>
        // make a fresh variable symbol for the local variable.
        val sym = Symbol.freshVarSym(ident, BoundBy.Pattern)
        NamedAst.RestrictableChoicePattern.Var(sym, loc)
    }

    pat0 match {
      case WeededAst.RestrictableChoicePattern.Tag(qname, pat, loc) =>
        NamedAst.RestrictableChoicePattern.Tag(qname, pat.map(visitVarPlace), loc)
    }
  }

  /**
    * Names the given head predicate `head`.
    */
  private def visitHeadPredicate(head: WeededAst.Predicate.Head, ns0: Name.NName)(implicit flix: Flix): Validation[NamedAst.Predicate.Head, NameError] = head match {
    case WeededAst.Predicate.Head.Atom(pred, den, exps, loc) =>
      for {
        es <- traverse(exps)(t => visitExp(t, ns0))
      } yield NamedAst.Predicate.Head.Atom(pred, den, es, loc)
  }

  /**
    * Names the given body predicate `body`.
    */
  private def visitBodyPredicate(body: WeededAst.Predicate.Body, ns0: Name.NName)(implicit flix: Flix): Validation[NamedAst.Predicate.Body, NameError] = body match {
    case WeededAst.Predicate.Body.Atom(pred, den, polarity, fixity, terms, loc) =>
      val ts = terms.map(visitPattern)
      NamedAst.Predicate.Body.Atom(pred, den, polarity, fixity, ts, loc).toSuccess

    case WeededAst.Predicate.Body.Functional(idents, exp, loc) =>
      for {
        e <- visitExp(exp, ns0)
      } yield NamedAst.Predicate.Body.Functional(idents, e, loc)

    case WeededAst.Predicate.Body.Guard(exp, loc) =>
      for {
        e <- visitExp(exp, ns0)
      } yield NamedAst.Predicate.Body.Guard(e, loc)

  }

  /**
    * Names the given type `tpe`.
    */
  private def visitType(t0: WeededAst.Type)(implicit flix: Flix): Validation[NamedAst.Type, NameError.TypeNameError] = {
    // TODO NS-REFACTOR seems like this is no longer failable. Use non-validation?
    // TODO NS-REFACTOR Can we merge WeededAst.Type and NamedAst.Type and avoid this whole function?
    def visit(tpe0: WeededAst.Type): Validation[NamedAst.Type, NameError.TypeNameError] = tpe0 match {
      case WeededAst.Type.Unit(loc) => NamedAst.Type.Unit(loc).toSuccess

      case WeededAst.Type.Var(ident, loc) =>
        //
        // Check for [[NameError.SuspiciousTypeVarName]].
        //
        if (isSuspiciousTypeVarName(ident.name)) {
          // TODO NS-REFACTOR maybe check this at declaration site instead of use site
          NameError.SuspiciousTypeVarName(ident.name, loc).toFailure
        } else {
          NamedAst.Type.Var(ident, loc).toSuccess
        }

      case WeededAst.Type.Ambiguous(qname, loc) =>
        NamedAst.Type.Ambiguous(qname, loc).toSuccess

      case WeededAst.Type.Tuple(elms, loc) =>
        mapN(traverse(elms)(visit)) {
          case ts => NamedAst.Type.Tuple(ts, loc)
        }

      case WeededAst.Type.RecordRowEmpty(loc) =>
        NamedAst.Type.RecordRowEmpty(loc).toSuccess

      case WeededAst.Type.RecordRowExtend(field, value, rest, loc) =>
        mapN(visit(value), visit(rest)) {
          case (t, r) => NamedAst.Type.RecordRowExtend(field, t, r, loc)
        }

      case WeededAst.Type.Record(row, loc) =>
        mapN(visit(row)) {
          r => NamedAst.Type.Record(r, loc)
        }

      case WeededAst.Type.SchemaRowEmpty(loc) =>
        NamedAst.Type.SchemaRowEmpty(loc).toSuccess

      case WeededAst.Type.SchemaRowExtendByAlias(qname, targs, rest, loc) =>
        mapN(traverse(targs)(visit), visit(rest)) {
          case (ts, r) => NamedAst.Type.SchemaRowExtendWithAlias(qname, ts, r, loc)
        }

      case WeededAst.Type.SchemaRowExtendByTypes(ident, den, tpes, rest, loc) =>
        mapN(traverse(tpes)(visit), visit(rest)) {
          case (ts, r) => NamedAst.Type.SchemaRowExtendWithTypes(ident, den, ts, r, loc)
        }

      case WeededAst.Type.Schema(row, loc) =>
        mapN(visit(row)) {
          r => NamedAst.Type.Schema(r, loc)
        }

      case WeededAst.Type.Relation(tpes, loc) =>
        mapN(traverse(tpes)(visit)) {
          case ts => NamedAst.Type.Relation(ts, loc)
        }

      case WeededAst.Type.Lattice(tpes, loc) =>
        mapN(traverse(tpes)(visit)) {
          case ts => NamedAst.Type.Lattice(ts, loc)
        }

      case WeededAst.Type.Native(fqn, loc) =>
        NamedAst.Type.Native(fqn, loc).toSuccess

      case WeededAst.Type.Arrow(tparams0, purAndEff0, tresult0, loc) =>
        val tparamsVal = traverse(tparams0)(visit)
        val purAndEffVal = visitPurityAndEffect(purAndEff0)
        val tresultVal = visit(tresult0)
        mapN(tparamsVal, purAndEffVal, tresultVal) {
          case (tparams, purAndEff, tresult) => NamedAst.Type.Arrow(tparams, purAndEff, tresult, loc)
        }

      case WeededAst.Type.Apply(tpe1, tpe2, loc) =>
        mapN(visit(tpe1), visit(tpe2)) {
          case (t1, t2) => NamedAst.Type.Apply(t1, t2, loc)
        }

      case WeededAst.Type.True(loc) =>
        NamedAst.Type.True(loc).toSuccess

      case WeededAst.Type.False(loc) =>
        NamedAst.Type.False(loc).toSuccess

      case WeededAst.Type.Not(tpe, loc) =>
        mapN(visit(tpe)) {
          case t => NamedAst.Type.Not(t, loc)
        }

      case WeededAst.Type.And(tpe1, tpe2, loc) =>
        mapN(visit(tpe1), visit(tpe2)) {
          case (t1, t2) => NamedAst.Type.And(t1, t2, loc)
        }

      case WeededAst.Type.Or(tpe1, tpe2, loc) =>
        mapN(visit(tpe1), visit(tpe2)) {
          case (t1, t2) => NamedAst.Type.Or(t1, t2, loc)
        }

      case WeededAst.Type.Complement(tpe, loc) =>
        mapN(visit(tpe)) {
          case t => NamedAst.Type.Complement(t, loc)
        }

      case WeededAst.Type.Union(tpe1, tpe2, loc) =>
        mapN(visit(tpe1), visit(tpe2)) {
          case (t1, t2) => NamedAst.Type.Union(t1, t2, loc)
        }

      case WeededAst.Type.Intersection(tpe1, tpe2, loc) =>
        mapN(visit(tpe1), visit(tpe2)) {
          case (t1, t2) => NamedAst.Type.Intersection(t1, t2, loc)
        }

      case WeededAst.Type.Read(tpe, loc) =>
        mapN(visit(tpe)) {
          case t => NamedAst.Type.Read(t, loc)
        }

      case WeededAst.Type.Write(tpe, loc) =>
        mapN(visit(tpe)) {
          case t => NamedAst.Type.Write(t, loc)
        }

      case WeededAst.Type.Empty(loc) => NamedAst.Type.Empty(loc).toSuccess

      case WeededAst.Type.CaseSet(cases, loc) => NamedAst.Type.CaseSet(cases, loc).toSuccess

      case WeededAst.Type.CaseComplement(tpe, loc) =>
        mapN(visitType(tpe)) {
          case t => NamedAst.Type.CaseComplement(t, loc)
        }

      case WeededAst.Type.CaseUnion(tpe1, tpe2, loc) =>
        mapN(visit(tpe1), visit(tpe2)) {
          case (t1, t2) => NamedAst.Type.CaseUnion(t1, t2, loc)
        }

      case WeededAst.Type.CaseIntersection(tpe1, tpe2, loc) =>
        mapN(visit(tpe1), visit(tpe2)) {
          case (t1, t2) => NamedAst.Type.CaseIntersection(t1, t2, loc)
        }

      case WeededAst.Type.Ascribe(tpe, kind0, loc) =>
        val kind = visitKind(kind0)
        mapN(visit(tpe)) {
          t => NamedAst.Type.Ascribe(t, kind, loc)
        }
    }

    visit(t0)
  }

  /**
    * Performs naming on the given kind.
    */
  private def visitKind(k0: WeededAst.Kind): NamedAst.Kind = k0 match {
    case WeededAst.Kind.Ambiguous(qname, loc) => NamedAst.Kind.Ambiguous(qname, loc)
    case WeededAst.Kind.Arrow(k10, k20, loc) =>
      val k1 = visitKind(k10)
      val k2 = visitKind(k20)
      NamedAst.Kind.Arrow(k1, k2, loc)
  }

  /**
    * Returns `true` if the given string `s` is a suspicious type variable name.
    */
  private def isSuspiciousTypeVarName(s: String): Boolean = s match {
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
    case "impure" => true
    case _ => false
  }

  /**
    * Performs naming on the given purity and effect.
    */
  private def visitPurityAndEffect(purAndEff: WeededAst.PurityAndEffect)(implicit flix: Flix): Validation[NamedAst.PurityAndEffect, NameError.TypeNameError] = purAndEff match {
    case WeededAst.PurityAndEffect(pur0, eff0) =>
      val purVal = traverseOpt(pur0)(visitType)
      val effVal = traverseOpt(eff0)(effs => traverse(effs)(visitType))
      mapN(purVal, effVal) {
        case (pur, eff) => NamedAst.PurityAndEffect(pur, eff)
      }
  }

  /**
    * Returns all the free variables in the given pattern `pat0`.
    */
  private def freeVars(pat0: WeededAst.Pattern): List[Name.Ident] = pat0 match {
    case WeededAst.Pattern.Var(ident, loc) => List(ident)
    case WeededAst.Pattern.Wild(loc) => Nil
    case WeededAst.Pattern.Cst(Ast.Constant.Unit, loc) => Nil
    case WeededAst.Pattern.Cst(Ast.Constant.Bool(true), loc) => Nil
    case WeededAst.Pattern.Cst(Ast.Constant.Bool(false), loc) => Nil
    case WeededAst.Pattern.Cst(Ast.Constant.Char(lit), loc) => Nil
    case WeededAst.Pattern.Cst(Ast.Constant.Float32(lit), loc) => Nil
    case WeededAst.Pattern.Cst(Ast.Constant.Float64(lit), loc) => Nil
    case WeededAst.Pattern.Cst(Ast.Constant.BigDecimal(lit), loc) => Nil
    case WeededAst.Pattern.Cst(Ast.Constant.Int8(lit), loc) => Nil
    case WeededAst.Pattern.Cst(Ast.Constant.Int16(lit), loc) => Nil
    case WeededAst.Pattern.Cst(Ast.Constant.Int32(lit), loc) => Nil
    case WeededAst.Pattern.Cst(Ast.Constant.Int64(lit), loc) => Nil
    case WeededAst.Pattern.Cst(Ast.Constant.BigInt(lit), loc) => Nil
    case WeededAst.Pattern.Cst(Ast.Constant.Str(lit), loc) => Nil
    case WeededAst.Pattern.Cst(Ast.Constant.Regex(lit), loc) => Nil
    case WeededAst.Pattern.Cst(Ast.Constant.Null, loc) => throw InternalCompilerException("unexpected null pattern", loc)
    case WeededAst.Pattern.Tag(qname, p, loc) => freeVars(p)
    case WeededAst.Pattern.Tuple(elms, loc) => elms flatMap freeVars
  }

  /**
    * Returns the free variables in the given type `tpe0`.
    */
  private def freeTypeVars(tpe0: WeededAst.Type): List[Name.Ident] = tpe0 match {
    case WeededAst.Type.Var(ident, loc) => ident :: Nil
    case WeededAst.Type.Ambiguous(qname, loc) => Nil
    case WeededAst.Type.Unit(loc) => Nil
    case WeededAst.Type.Tuple(elms, loc) => elms.flatMap(freeTypeVars)
    case WeededAst.Type.RecordRowEmpty(loc) => Nil
    case WeededAst.Type.RecordRowExtend(l, t, r, loc) => freeTypeVars(t) ::: freeTypeVars(r)
    case WeededAst.Type.Record(row, loc) => freeTypeVars(row)
    case WeededAst.Type.SchemaRowEmpty(loc) => Nil
    case WeededAst.Type.SchemaRowExtendByTypes(_, _, ts, r, loc) => ts.flatMap(freeTypeVars) ::: freeTypeVars(r)
    case WeededAst.Type.SchemaRowExtendByAlias(_, ts, r, _) => ts.flatMap(freeTypeVars) ::: freeTypeVars(r)
    case WeededAst.Type.Schema(row, loc) => freeTypeVars(row)
    case WeededAst.Type.Relation(ts, loc) => ts.flatMap(freeTypeVars)
    case WeededAst.Type.Lattice(ts, loc) => ts.flatMap(freeTypeVars)
    case WeededAst.Type.Native(fqm, loc) => Nil
    case WeededAst.Type.Arrow(tparams, WeededAst.PurityAndEffect(pur, eff), tresult, loc) => tparams.flatMap(freeTypeVars) ::: pur.toList.flatMap(freeTypeVars) ::: eff.toList.flatMap(_.flatMap(freeTypeVars)) ::: freeTypeVars(tresult)
    case WeededAst.Type.Apply(tpe1, tpe2, loc) => freeTypeVars(tpe1) ++ freeTypeVars(tpe2)
    case WeededAst.Type.True(loc) => Nil
    case WeededAst.Type.False(loc) => Nil
    case WeededAst.Type.Not(tpe, loc) => freeTypeVars(tpe)
    case WeededAst.Type.And(tpe1, tpe2, loc) => freeTypeVars(tpe1) ++ freeTypeVars(tpe2)
    case WeededAst.Type.Or(tpe1, tpe2, loc) => freeTypeVars(tpe1) ++ freeTypeVars(tpe2)
    case WeededAst.Type.Complement(tpe, loc) => freeTypeVars(tpe)
    case WeededAst.Type.Union(tpe1, tpe2, loc) => freeTypeVars(tpe1) ++ freeTypeVars(tpe2)
    case WeededAst.Type.Intersection(tpe1, tpe2, loc) => freeTypeVars(tpe1) ++ freeTypeVars(tpe2)
    case WeededAst.Type.Read(tpe, loc) => freeTypeVars(tpe)
    case WeededAst.Type.Write(tpe, loc) => freeTypeVars(tpe)
    case WeededAst.Type.Empty(_) => Nil
    case WeededAst.Type.CaseSet(_, _) => Nil
    case WeededAst.Type.CaseComplement(tpe, loc) => freeTypeVars(tpe)
    case WeededAst.Type.CaseUnion(tpe1, tpe2, loc) => freeTypeVars(tpe1) ++ freeTypeVars(tpe2)
    case WeededAst.Type.CaseIntersection(tpe1, tpe2, loc) => freeTypeVars(tpe1) ++ freeTypeVars(tpe2)
    case WeededAst.Type.Ascribe(tpe, _, _) => freeTypeVars(tpe)
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
  private def visitFormalParam(fparam: WeededAst.FormalParam)(implicit flix: Flix): Validation[NamedAst.FormalParam, NameError.TypeNameError] = fparam match {
    case WeededAst.FormalParam(ident, mod, optType, loc) =>
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
  private def visitPredicateParam(pparam: WeededAst.PredicateParam)(implicit flix: Flix): Validation[NamedAst.PredicateParam, NameError.TypeNameError] = pparam match {
    case WeededAst.PredicateParam.PredicateParamUntyped(pred, loc) =>
      NamedAst.PredicateParam.PredicateParamUntyped(pred, loc).toSuccess

    case WeededAst.PredicateParam.PredicateParamWithType(pred, den, tpes, loc) =>
      mapN(traverse(tpes)(visitType)) {
        case ts => NamedAst.PredicateParam.PredicateParamWithType(pred, den, ts, loc)
      }
  }

  /**
    * Translates the given weeded JvmMethod to a named JvmMethod.
    */
  private def visitJvmMethod(method: WeededAst.JvmMethod, ns0: Name.NName)(implicit flix: Flix): Validation[NamedAst.JvmMethod, NameError] = method match {
    case WeededAst.JvmMethod(ident, fparams0, exp0, tpe0, purAndEff0, loc) =>
      flatMapN(traverse(fparams0)(visitFormalParam): Validation[List[NamedAst.FormalParam], NameError]) {
        case fparams =>
          val exp = visitExp(exp0, ns0)
          val tpe = visitType(tpe0)
          val purAndEff = visitPurityAndEffect(purAndEff0)
          mapN(exp, tpe, purAndEff) {
            case (e, t, p) => NamedAst.JvmMethod(ident, fparams, e, t, p, loc)
          }
      }: Validation[NamedAst.JvmMethod, NameError]
  }

  /**
    * Performs naming on the given formal parameters `fparam0`.
    */
  private def getFormalParams(fparams0: List[WeededAst.FormalParam])(implicit flix: Flix): Validation[List[NamedAst.FormalParam], NameError] = {
    traverse(fparams0)(visitFormalParam)
  }


  /**
    * Performs naming on the given type parameter.
    */
  private def getTypeParam(tparam0: WeededAst.TypeParam)(implicit flix: Flix): NamedAst.TypeParam = tparam0 match {
    case WeededAst.TypeParam.Kinded(ident, kind) =>
      NamedAst.TypeParam.Kinded(ident, mkTypeVarSym(ident), visitKind(kind), ident.loc)
    case WeededAst.TypeParam.Unkinded(ident) =>
      NamedAst.TypeParam.Unkinded(ident, mkTypeVarSym(ident), ident.loc)
  }

  /**
    * Performs naming on the given type parameters `tparam0` from the given cases `cases`.
    */
  private def getTypeParams(tparams0: WeededAst.TypeParams)(implicit flix: Flix): NamedAst.TypeParams = {
    tparams0 match {
      case WeededAst.TypeParams.Elided => NamedAst.TypeParams.Kinded(Nil)
      case WeededAst.TypeParams.Unkinded(tparams) => getExplicitTypeParams(tparams)
      case WeededAst.TypeParams.Kinded(tparams) => getExplicitKindedTypeParams(tparams)
    }
  }


  /**
    * Performs naming on the given type parameters `tparams0` from the given formal params `fparams` and overall type `tpe`.
    */
  private def getTypeParamsFromFormalParams(tparams0: WeededAst.TypeParams, fparams: List[WeededAst.FormalParam], tpe: WeededAst.Type, purAndEff: WeededAst.PurityAndEffect)(implicit flix: Flix): NamedAst.TypeParams = {
    tparams0 match {
      case WeededAst.TypeParams.Elided => getImplicitTypeParamsFromFormalParams(fparams, tpe, purAndEff)
      case WeededAst.TypeParams.Unkinded(tparams) => getExplicitTypeParams(tparams)
      case WeededAst.TypeParams.Kinded(tparams) => getExplicitKindedTypeParams(tparams)

    }
  }

  /**
    * Names the explicit kinded type params.
    */
  private def getExplicitKindedTypeParams(tparams0: List[WeededAst.TypeParam.Kinded])(implicit flix: Flix): NamedAst.TypeParams.Kinded = {
    val tparams = tparams0.map {
      case WeededAst.TypeParam.Kinded(ident, kind) =>
        NamedAst.TypeParam.Kinded(ident, mkTypeVarSym(ident), visitKind(kind), ident.loc)
    }
    NamedAst.TypeParams.Kinded(tparams)
  }

  /**
    * Returns the explicit unkinded type parameters from the given type parameter names and implicit type parameters.
    */
  private def getExplicitTypeParams(tparams0: List[WeededAst.TypeParam.Unkinded])(implicit flix: Flix): NamedAst.TypeParams.Unkinded = {
    val tparams = tparams0.map {
      case WeededAst.TypeParam.Unkinded(ident) =>
        NamedAst.TypeParam.Unkinded(ident, mkTypeVarSym(ident), ident.loc)
    }
    NamedAst.TypeParams.Unkinded(tparams)
  }

  /**
    * Returns the implicit type parameters constructed from the given types.
    */
  private def getImplicitTypeParamsFromTypes(types: List[WeededAst.Type])(implicit flix: Flix): NamedAst.TypeParams.Implicit = {
    val tvars = types.flatMap(freeTypeVars).distinct
    val tparams = tvars.map {
      ident => NamedAst.TypeParam.Implicit(ident, mkTypeVarSym(ident), ident.loc)
    }
    NamedAst.TypeParams.Implicit(tparams)
  }

  /**
    * Returns the implicit type parameters constructed from the given formal parameters and type.
    */
  private def getImplicitTypeParamsFromFormalParams(fparams: List[WeededAst.FormalParam], tpe: WeededAst.Type, purAndEff: WeededAst.PurityAndEffect)(implicit flix: Flix): NamedAst.TypeParams = {
    // Compute the type variables that occur in the formal parameters.
    val fparamTvars = fparams.flatMap {
      case WeededAst.FormalParam(_, _, Some(tpe), _) => freeTypeVars(tpe)
      case WeededAst.FormalParam(_, _, None, _) => Nil
    }

    val tpeTvars = freeTypeVars(tpe)

    val WeededAst.PurityAndEffect(pur, eff) = purAndEff
    val purTvars = pur.toList.flatMap(freeTypeVars)
    val effTvars = eff.getOrElse(Nil).flatMap(freeTypeVars)

    val tparams = (fparamTvars ::: tpeTvars ::: purTvars ::: effTvars).distinct.map {
      ident => NamedAst.TypeParam.Implicit(ident, mkTypeVarSym(ident), ident.loc)
    }

    NamedAst.TypeParams.Implicit(tparams)
  }

  /**
    * Gets the location of the symbol of the declaration.
    */
  private def getSymLocation(f: NamedAst.Declaration): SourceLocation = f match {
    case NamedAst.Declaration.Class(doc, ann, mod, sym, tparam, superClasses, assocs, sigs, laws, loc) => sym.loc
    case NamedAst.Declaration.Sig(sym, spec, exp) => sym.loc
    case NamedAst.Declaration.Def(sym, spec, exp) => sym.loc
    case NamedAst.Declaration.Enum(doc, ann, mod, sym, tparams, derives, cases, loc) => sym.loc
    case NamedAst.Declaration.RestrictableEnum(doc, ann, mod, sym, ident, tparams, derives, cases, loc) => sym.loc
    case NamedAst.Declaration.TypeAlias(doc, mod, sym, tparams, tpe, loc) => sym.loc
    case NamedAst.Declaration.Effect(doc, ann, mod, sym, ops, loc) => sym.loc
    case NamedAst.Declaration.Op(sym, spec) => sym.loc
    case NamedAst.Declaration.Case(sym, tpe, _) => sym.loc
    case NamedAst.Declaration.RestrictableCase(sym, tpe, _) => sym.loc
    case NamedAst.Declaration.AssocTypeSig(doc, mod, sym, tparams, kind, loc) => sym.loc
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
  private def visitUseOrImport(use: WeededAst.UseOrImport): NamedAst.UseOrImport = use match {
    case WeededAst.UseOrImport.Use(qname, alias, loc) => NamedAst.UseOrImport.Use(qname, alias, loc)
    case WeededAst.UseOrImport.Import(name, alias, loc) => NamedAst.UseOrImport.Import(name, alias, loc)
  }

  /**
    * A structure holding the symbols and instances in the program.
    */
  case class SymbolTable(symbols: Map[List[String], ListMap[String, NamedAst.Declaration]], instances: Map[List[String], Map[String, List[NamedAst.Declaration.Instance]]], uses: Map[List[String], List[NamedAst.UseOrImport]])
}

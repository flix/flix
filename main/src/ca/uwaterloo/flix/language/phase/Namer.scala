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
import ca.uwaterloo.flix.language.ast.WeededAst.ChoicePattern
import ca.uwaterloo.flix.language.ast.{NamedAst, _}
import ca.uwaterloo.flix.language.errors.NameError
import ca.uwaterloo.flix.util.Validation._
import ca.uwaterloo.flix.util.{InternalCompilerException, Validation}

import scala.collection.mutable

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
              case (k, v) => Name.mkUnlocatedNName(k) -> v
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
    // TODO NS-REFACTOR remove needless tenvs from visitClass, visitInstance, visitEffect
    case decl: WeededAst.Declaration.Class => visitClass(decl, Map.empty, ns0)
    case decl: WeededAst.Declaration.Instance => visitInstance(decl, Map.empty, ns0)
    case decl: WeededAst.Declaration.Def => visitDef(decl, Map.empty, ns0, Nil)
    case decl: WeededAst.Declaration.Enum => visitEnum(decl, ns0)
    case decl: WeededAst.Declaration.TypeAlias => visitTypeAlias(decl, ns0)
    case decl: WeededAst.Declaration.Effect => visitEffect(decl, Map.empty, ns0)
    case decl: WeededAst.Declaration.Law => throw InternalCompilerException("unexpected law", decl.loc)
  }

  /**
    * Performs naming on the given namespace.
    */
  private def visitNamespace(decl: WeededAst.Declaration.Namespace, ns0: Name.NName)(implicit flix: Flix): Validation[NamedAst.Declaration.Namespace, NameError] = decl match {
    case WeededAst.Declaration.Namespace(name, usesAndImports0, decls0, loc) =>
      val ns = Name.NName(name.sp1, ns0.idents ::: name.idents, name.sp2)
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
    case NamedAst.Declaration.Namespace(sym, usesAndImports, decls, loc) =>
      // reset the uenv
      val table1 = fold(decls, table0) {
        case (table, d) => tableDecl(d, table)
      }
      mapN(table1)(addUsesToTable(_, sym.ns, usesAndImports))

    case NamedAst.Declaration.Class(doc, ann, mod, sym, tparam, superClasses, sigs, laws, loc) =>
      val table1Val = tryAddToTable(table0, sym.namespace, sym.name, decl)
      // reset the uenv inside the class
      flatMapN(table1Val) {
        case table1 => fold(sigs, table1) {
          case (table, d) => tableDecl(d, table)
        }
      }

    case inst@NamedAst.Declaration.Instance(doc, ann, mod, clazz, tpe, tconstrs, defs, ns, loc) =>
      addInstanceToTable(table0, ns, clazz.ident.name, inst).toSuccess

    case NamedAst.Declaration.Sig(sym, spec, exp) =>
      tryAddToTable(table0, sym.namespace, sym.name, decl)

    case NamedAst.Declaration.Def(sym, spec, exp) =>
      tryAddToTable(table0, sym.namespace, sym.name, decl)

    case NamedAst.Declaration.Enum(doc, ann, mod, sym, tparams, derives, cases, tpe, loc) =>
      tryAddToTable(table0, sym.namespace, sym.name, decl)

    case NamedAst.Declaration.TypeAlias(doc, mod, sym, tparams, tpe, loc) =>
      tryAddToTable(table0, sym.namespace, sym.name, decl)

    case NamedAst.Declaration.Effect(doc, ann, mod, sym, ops, loc) =>
      val table1Val = tryAddToTable(table0, sym.namespace, sym.name, decl)
      // reset the uenv inside the effect
      flatMapN(table1Val) {
        case table1 => fold(ops, table1) {
          case (table, d) => tableDecl(d, table)
        }
      }

    case NamedAst.Declaration.Op(sym, spec) =>
      tryAddToTable(table0, sym.namespace, sym.name, decl)

      // skip cases for now
    case NamedAst.Declaration.Case(_, _) => table0.toSuccess
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
      val oldMap = symbols0.getOrElse(ns, Map.empty)
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
    val symbols0 = table.symbols.getOrElse(ns0, Map.empty)
    symbols0.get(name) match {
      // Case 1: The name is unused.
      case None => LookupResult.NotDefined
      // Case 2: An symbol with the name already exists.
      case Some(decl) => LookupResult.AlreadyDefined(getSymLocation(decl))
    }
  }

  /**
    * Performs naming on the given constraint `c0` under the given environments `env0`, `uenv0`, and `tenv0`.
    */
  private def visitConstraint(c0: WeededAst.Constraint, outerEnv: Map[String, Symbol.VarSym], tenv0: Map[String, Symbol.UnkindedTypeVarSym], ns0: Name.NName)(implicit flix: Flix): Validation[NamedAst.Constraint, NameError] = c0 match {
    case WeededAst.Constraint(h, bs, loc) =>
      // Find the variables visible in the head and rule scope of the constraint.
      // Remove any variables already in the outer environment.
      val headVars = bs.flatMap(visibleInHeadScope).filterNot(ident => outerEnv.contains(ident.name))
      val ruleVars = bs.flatMap(visibleInRuleScope).filterNot(ident => outerEnv.contains(ident.name))

      // Introduce a symbol for each variable that is visible in the head scope of the constraint (excluding those visible by the rule scope).
      val headEnv = headVars.foldLeft(Map.empty[String, Symbol.VarSym]) {
        case (macc, ident) => macc.get(ident.name) match {
          // Check if the identifier is bound by the rule scope.
          case None if !ruleVars.exists(_.name == ident.name) =>
            macc + (ident.name -> Symbol.freshVarSym(ident, BoundBy.Constraint))
          case _ => macc
        }
      }

      // Introduce a symbol for each variable that is visible in the rule scope of the constraint.
      val ruleEnv = ruleVars.foldLeft(Map.empty[String, Symbol.VarSym]) {
        case (macc, ident) => macc.get(ident.name) match {
          case None => macc + (ident.name -> Symbol.freshVarSym(ident, BoundBy.Constraint))
          case Some(sym) => macc
        }
      }

      // Perform naming on the head and body predicates.
      mapN(visitHeadPredicate(h, outerEnv, headEnv, ruleEnv, tenv0, ns0), traverse(bs)(b => visitBodyPredicate(b, outerEnv, headEnv, ruleEnv, tenv0, ns0))) {
        case (head, body) =>
          val headParams = headEnv.map {
            case (_, sym) => NamedAst.ConstraintParam(sym, NamedAst.Type.Var(sym.tvar.sym.withoutKind, loc), sym.loc)
          }
          val ruleParam = ruleEnv.map {
            case (_, sym) => NamedAst.ConstraintParam(sym, NamedAst.Type.Var(sym.tvar.sym.withoutKind, loc), sym.loc)
          }
          val cparams = (headParams ++ ruleParam).toList
          NamedAst.Constraint(cparams, head, body, loc)
      }
  }


  /**
    * Performs naming on the given enum `enum0`.
    */
  private def visitEnum(enum0: WeededAst.Declaration.Enum, ns0: Name.NName)(implicit flix: Flix): Validation[NamedAst.Declaration.Enum, NameError] = enum0 match {
    case WeededAst.Declaration.Enum(doc, ann0, mod0, ident, tparams0, derives, cases0, loc) =>
      val sym = Symbol.mkEnumSym(ns0, ident)

      // Compute the type parameters.
      val tparams = getTypeParams(tparams0)

      val tenv = tparams.tparams.map(kv => kv.name.name -> kv.sym).toMap
      val quantifiers = tparams.tparams.map(_.sym).map(sym => NamedAst.Type.Var(sym, sym.loc))
      val base = NamedAst.Type.Enum(sym, ident.loc)
      val enumType = quantifiers.foldLeft(base: NamedAst.Type) {
        case (tacc, tvar) => NamedAst.Type.Apply(tacc, tvar, tvar.loc)
      }

      val annVal = traverse(ann0)(visitAnnotation(_, Map.empty, tenv, ns0))
      val mod = visitModifiers(mod0, ns0)
      val casesVal = traverse(cases0)(visitCase(_, sym, tenv))

      mapN(annVal, casesVal) {
        case (ann, cases) =>
          val caseMap = cases.foldLeft(Map.empty[String, NamedAst.Declaration.Case]) {
            case (acc, caze) => acc + (caze.sym.name -> caze)
          }
          NamedAst.Declaration.Enum(doc, ann, mod, sym, tparams, derives, caseMap, enumType, loc)
      }
  }


  /**
    * Performs naming on the given enum case.
    */
  private def visitCase(case0: WeededAst.Case, enumSym: Symbol.EnumSym, tenv0: Map[String, Symbol.UnkindedTypeVarSym])(implicit flix: Flix): Validation[NamedAst.Declaration.Case, NameError] = case0 match {
    case WeededAst.Case(ident, tpe0) =>
      mapN(visitType(tpe0, allowWild = false, tenv0)) {
        case tpe =>
          val caseSym = Symbol.mkCaseSym(enumSym, ident)
          NamedAst.Declaration.Case(caseSym, tpe)
      }
  }

  /**
    * Performs naming on the given type alias `alias0`.
    */
  private def visitTypeAlias(alias0: WeededAst.Declaration.TypeAlias, ns0: Name.NName)(implicit flix: Flix): Validation[NamedAst.Declaration.TypeAlias, NameError] = alias0 match {
    case WeededAst.Declaration.TypeAlias(doc, mod0, ident, tparams0, tpe0, loc) =>
      val mod = visitModifiers(mod0, ns0)
      val tparams = getTypeParams(tparams0)
      val tenv = getTypeEnv(tparams.tparams)
      mapN(visitType(tpe0, allowWild = false, tenv)) {
        tpe =>
          val sym = Symbol.mkTypeAliasSym(ns0, ident)
          NamedAst.Declaration.TypeAlias(doc, mod, sym, tparams, tpe, loc)
      }
  }

  /**
    * Performs naming on the given class `clazz`.
    */
  private def visitClass(clazz: WeededAst.Declaration.Class, tenv0: Map[String, Symbol.UnkindedTypeVarSym], ns0: Name.NName)(implicit flix: Flix): Validation[NamedAst.Declaration.Class, NameError] = clazz match {
    case WeededAst.Declaration.Class(doc, ann0, mod0, ident, tparams0, superClasses0, signatures, laws0, loc) =>
      val sym = Symbol.mkClassSym(ns0, ident)
      val mod = visitModifiers(mod0, ns0)
      val tparam = getTypeParam(tparams0)
      val tenv = getTypeEnv(List(tparam))
      val tconstr = NamedAst.TypeConstraint(Name.mkQName(ident), NamedAst.Type.Var(tparam.sym, tparam.loc.asSynthetic), sym.loc.asSynthetic)

      val annVal = traverse(ann0)(visitAnnotation(_, Map.empty, tenv, ns0))
      val superClassesVal = traverse(superClasses0)(visitTypeConstraint(_, tenv, ns0))
      val sigsVal = traverse(signatures)(visitSig(_, tenv, ns0, ident, sym, tparam))
      val lawsVal = traverse(laws0)(visitDef(_, tenv, ns0, List(tconstr)))

      mapN(annVal, superClassesVal, sigsVal, lawsVal) {
        case (ann, superClasses, sigs, laws) =>
          NamedAst.Declaration.Class(doc, ann, mod, sym, tparam, superClasses, sigs, laws, loc)
      }
  }

  /**
    * Performs naming on the given instance `instance`.
    */
  private def visitInstance(instance: WeededAst.Declaration.Instance, tenv0: Map[String, Symbol.UnkindedTypeVarSym], ns0: Name.NName)(implicit flix: Flix): Validation[NamedAst.Declaration.Instance, NameError] = instance match {
    case WeededAst.Declaration.Instance(doc, ann0, mod, clazz, tpe0, tconstrs0, defs0, loc) =>
      val tparams = getImplicitTypeParamsFromTypes(List(tpe0))
      val tenv = tenv0 ++ getTypeEnv(tparams.tparams)

      val annVal = traverse(ann0)(visitAnnotation(_, Map.empty, tenv, ns0))
      val tpeVal = visitType(tpe0, allowWild = true, tenv)
      val tconstrsVal = traverse(tconstrs0)(visitTypeConstraint(_, tenv, ns0))
      flatMapN(annVal, tpeVal, tconstrsVal) {
        case (ann, tpe, tconstrs) =>
          val instTconstr = NamedAst.TypeConstraint(clazz, tpe, clazz.loc)
          val defsVal = traverse(defs0)(visitDef(_, tenv, ns0, List(instTconstr)))
          mapN(defsVal) {
            defs => NamedAst.Declaration.Instance(doc, ann, mod, clazz, tpe, tconstrs, defs, ns0.parts, loc)
          }
      }
  }


  /**
    * Performs naming on the given type constraint `tconstr`.
    */
  private def visitTypeConstraint(tconstr: WeededAst.TypeConstraint, tenv0: Map[String, Symbol.UnkindedTypeVarSym], ns0: Name.NName)(implicit flix: Flix): Validation[NamedAst.TypeConstraint, NameError] = tconstr match {
    case WeededAst.TypeConstraint(clazz, tparam0, loc) =>
      mapN(visitType(tparam0, allowWild = false, tenv0)) {
        tparam => NamedAst.TypeConstraint(clazz, tparam, loc)
      }
  }

  /**
    * Performs naming on the given signature declaration `sig` under the given environments `env0`, `uenv0`, and `tenv0`.
    */
  private def visitSig(sig: WeededAst.Declaration.Sig, tenv0: Map[String, Symbol.UnkindedTypeVarSym], ns0: Name.NName, classIdent: Name.Ident, classSym: Symbol.ClassSym, classTparam: NamedAst.TypeParam)(implicit flix: Flix): Validation[NamedAst.Declaration.Sig, NameError] = sig match {
    case WeededAst.Declaration.Sig(doc, ann, mod0, ident, tparams0, fparams0, exp0, tpe0, purAndEff0, tconstrs0, loc) =>
      val tparams = getTypeParamsFromFormalParams(tparams0, fparams0, tpe0, purAndEff0, tenv0)
      val tenv = tenv0 ++ getTypeEnv(tparams.tparams)

      // First visit all the top-level information
      val sigTypeCheckVal = checkSigType(ident, classTparam, fparams0, tpe0, purAndEff0, ident.loc)
      val mod = visitModifiers(mod0, ns0)
      val fparamsVal = getFormalParams(fparams0, tenv)
      val tpeVal = visitType(tpe0, allowWild = true, tenv)
      val purAndEffVal = visitPurityAndEffect(purAndEff0, allowWild = true, tenv)
      val tconstrsVal = traverse(tconstrs0)(visitTypeConstraint(_, tenv, ns0))

      flatMapN(sigTypeCheckVal, fparamsVal, tpeVal, purAndEffVal, tconstrsVal) {
        case (_, fparams, tpe, purAndEff, tconstrs) =>

          // Then visit the parts depending on the parameters
          val env0 = getVarEnv(fparams)
          val annVal = traverse(ann)(visitAnnotation(_, env0, tenv, ns0))
          val expVal = traverseOpt(exp0)(visitExp(_, env0, tenv, ns0))

          mapN(annVal, expVal) {
            case (as, exp) =>

              // Add the class tconstr to the list
              val classTconstr = NamedAst.TypeConstraint(Name.mkQName(classIdent), NamedAst.Type.Var(classTparam.sym, classTparam.loc), classSym.loc)
              val allTconstrs = classTconstr :: tconstrs

              val sym = Symbol.mkSigSym(classSym, ident)
              val spec = NamedAst.Spec(doc, as, mod, tparams, fparams, tpe, purAndEff, allTconstrs, loc)
              NamedAst.Declaration.Sig(sym, spec, exp)
          }
      }
  }

  /**
    * Checks the type `tpe` of the signature to ensure that it contains the class type parameter `classTparam`.
    */
  private def checkSigType(sigIdent: Name.Ident, classTparam: NamedAst.TypeParam, fparams: List[WeededAst.FormalParam], tpe: WeededAst.Type, purAndEff: WeededAst.PurityAndEffect, loc: SourceLocation): Validation[Unit, NameError] = {
    val WeededAst.PurityAndEffect(pur, eff) = purAndEff
    val tpes = fparams.flatMap(_.tpe) ::: tpe :: pur.toList ::: eff.getOrElse(Nil)
    val tvars = tpes.flatMap(freeVars)
    if (tvars.exists(tvar => tvar.name == classTparam.name.name)) {
      ().toSuccess
    } else {
      NameError.IllegalSignature(sigIdent, loc).toFailure
    }
  }

  /**
    * Performs naming on the given definition declaration `decl0` under the given environments `env0`, `uenv0`, and `tenv0`, with type constraints `tconstrs`.
    */
  private def visitDef(decl0: WeededAst.Declaration.Def, tenv0: Map[String, Symbol.UnkindedTypeVarSym], ns0: Name.NName, addedTconstrs: List[NamedAst.TypeConstraint])(implicit flix: Flix): Validation[NamedAst.Declaration.Def, NameError] = decl0 match {
    case WeededAst.Declaration.Def(doc, ann, mod0, ident, tparams0, fparams0, exp, tpe0, purAndEff0, tconstrs0, loc) =>
      flix.subtask(ident.name, sample = true)

      val tparams = getTypeParamsFromFormalParams(tparams0, fparams0, tpe0, purAndEff0, tenv0)
      val tenv = tenv0 ++ getTypeEnv(tparams.tparams)

      // First visit all the top-level information
      val mod = visitModifiers(mod0, ns0)
      val fparamsVal = getFormalParams(fparams0, tenv)
      val tpeVal = visitType(tpe0, allowWild = true, tenv)
      val purAndEffVal = visitPurityAndEffect(purAndEff0, allowWild = true, tenv)
      val tconstrsVal = traverse(tconstrs0)(visitTypeConstraint(_, tenv, ns0))

      flatMapN(fparamsVal, tpeVal, purAndEffVal, tconstrsVal) {
        case (fparams, tpe, purAndEff, tconstrs) =>

          // Then visit the parts depending on the parameters
          val env0 = getVarEnv(fparams)
          val annVal = traverse(ann)(visitAnnotation(_, env0, tenv, ns0))
          val expVal = visitExp(exp, env0, tenv, ns0)

          mapN(annVal, expVal) {
            case (as, e) =>

              // add the extra tconstrs
              val allTconstrs = addedTconstrs ::: tconstrs

              val sym = Symbol.mkDefnSym(ns0, ident)
              val spec = NamedAst.Spec(doc, as, mod, tparams, fparams, tpe, purAndEff, allTconstrs, loc)
              NamedAst.Declaration.Def(sym, spec, e)
          }
      }
  }

  /**
    * Performs naming on the given effect `eff0` under the given environments `env0`, `uenv0`, and `tenv0`.
    */
  private def visitEffect(eff0: WeededAst.Declaration.Effect, tenv0: Map[String, Symbol.UnkindedTypeVarSym], ns0: Name.NName)(implicit flix: Flix): Validation[NamedAst.Declaration.Effect, NameError] = eff0 match {
    case WeededAst.Declaration.Effect(doc, ann0, mod0, ident, ops0, loc) =>
      val sym = Symbol.mkEffectSym(ns0, ident)

      val annVal = traverse(ann0)(visitAnnotation(_, Map.empty, tenv0, ns0))
      val mod = visitModifiers(mod0, ns0)
      val opsVal = traverse(ops0)(visitOp(_, tenv0, ns0, sym))

      mapN(annVal, opsVal) {
        case (ann, ops) => NamedAst.Declaration.Effect(doc, ann, mod, sym, ops, loc)
      }
  }

  /**
    * Performs naming on the given effect operation `op0` under the given environments `env0`, `uenv0`, and `tenv0`.
    */
  private def visitOp(op0: WeededAst.Declaration.Op, tenv: Map[String, Symbol.UnkindedTypeVarSym], ns0: Name.NName, effSym: Symbol.EffectSym)(implicit flix: Flix): Validation[NamedAst.Declaration.Op, NameError] = op0 match {
    case WeededAst.Declaration.Op(doc, ann0, mod0, ident, fparams0, tpe0, tconstrs0, loc) =>
      // First visit all the top-level information
      val mod = visitModifiers(mod0, ns0)
      val fparamsVal = getFormalParams(fparams0, tenv)
      val tpeVal = visitType(tpe0, allowWild = true, tenv)
      val tconstrsVal = traverse(tconstrs0)(visitTypeConstraint(_, tenv, ns0))

      flatMapN(fparamsVal, tpeVal, tconstrsVal) {
        case (fparams, tpe, tconstrs) =>

          // Then visit the parts depending on the parameters
          val env0 = getVarEnv(fparams)
          val annVal = traverse(ann0)(visitAnnotation(_, env0, tenv, ns0))

          mapN(annVal) {
            ann =>

              val tparams = NamedAst.TypeParams.Kinded(Nil) // operations are monomorphic
              val purAndEff = NamedAst.PurityAndEffect(None, None) // operations are pure

              val sym = Symbol.mkOpSym(effSym, ident)
              val spec = NamedAst.Spec(doc, ann, mod, tparams, fparams, tpe, purAndEff, tconstrs, loc)
              NamedAst.Declaration.Op(sym, spec)
          }
      }
  }

  /**
    * Performs naming on the given expression `exp0` under the given environments `env0`, `uenv0`, and `tenv0`.
    */
  private def visitExp(exp0: WeededAst.Expression, env0: Map[String, Symbol.VarSym], tenv0: Map[String, Symbol.UnkindedTypeVarSym], ns0: Name.NName)(implicit flix: Flix): Validation[NamedAst.Expression, NameError] = exp0 match {

    case WeededAst.Expression.Wild(loc) =>
      NamedAst.Expression.Wild(loc).toSuccess

    case WeededAst.Expression.DefOrSig(qname, loc) =>
      NamedAst.Expression.DefOrSig(qname, env0, loc).toSuccess

    case WeededAst.Expression.VarOrDefOrSig(ident, loc) =>
      // the ident name.
      val name = ident.name

      // TODO NS-REFACTOR move this lookup to Resolver
      // lookup the name in the var environment
      env0.get(name) match {
        // Case 1: not found, assume it is a function
        case None => NamedAst.Expression.UnqualifiedDefOrSig(ident, env0, loc).toSuccess
        // Case 2: found a var
        case Some(sym) => NamedAst.Expression.Var(sym, loc).toSuccess
      }

    case WeededAst.Expression.Hole(name, loc) =>
      NamedAst.Expression.Hole(name, loc).toSuccess

    case WeededAst.Expression.HoleWithExp(exp, loc) =>
      mapN(visitExp(exp, env0, tenv0, ns0)) {
        case e => NamedAst.Expression.HoleWithExp(e, loc)
      }

    case WeededAst.Expression.Use(uses0, exp, loc) =>
      val uses = uses0.map(visitUseOrImport)

      mapN(visitExp(exp, env0, tenv0, ns0)) {
        case e => uses.foldRight(e) {
          case (use, acc) => NamedAst.Expression.Use(use, acc, loc)
        }
      }

    case WeededAst.Expression.Cst(cst, loc) => NamedAst.Expression.Cst(cst, loc).toSuccess

    case WeededAst.Expression.Apply(exp, exps, loc) =>
      mapN(visitExp(exp, env0, tenv0, ns0), traverse(exps)(visitExp(_, env0, tenv0, ns0))) {
        case (e, es) => NamedAst.Expression.Apply(e, es, loc)
      }

    case WeededAst.Expression.Lambda(fparam0, exp, loc) =>
      flatMapN(visitFormalParam(fparam0, tenv0)) {
        case p =>
          val env1 = env0 + (p.sym.text -> p.sym)
          mapN(visitExp(exp, env1, tenv0, ns0)) {
            case e => NamedAst.Expression.Lambda(p, e, loc)
          }
      }

    case WeededAst.Expression.Unary(sop, exp, loc) =>
      visitExp(exp, env0, tenv0, ns0) map {
        case e => NamedAst.Expression.Unary(sop, e, loc)
      }

    case WeededAst.Expression.Binary(sop, exp1, exp2, loc) =>
      mapN(visitExp(exp1, env0, tenv0, ns0), visitExp(exp2, env0, tenv0, ns0)) {
        case (e1, e2) => NamedAst.Expression.Binary(sop, e1, e2, loc)
      }

    case WeededAst.Expression.IfThenElse(exp1, exp2, exp3, loc) =>
      val e1 = visitExp(exp1, env0, tenv0, ns0)
      val e2 = visitExp(exp2, env0, tenv0, ns0)
      val e3 = visitExp(exp3, env0, tenv0, ns0)
      mapN(e1, e2, e3) {
        NamedAst.Expression.IfThenElse(_, _, _, loc)
      }

    case WeededAst.Expression.Stm(exp1, exp2, loc) =>
      val e1 = visitExp(exp1, env0, tenv0, ns0)
      val e2 = visitExp(exp2, env0, tenv0, ns0)
      mapN(e1, e2) {
        NamedAst.Expression.Stm(_, _, loc)
      }

    case WeededAst.Expression.Discard(exp, loc) =>
      visitExp(exp, env0, tenv0, ns0) map {
        case e => NamedAst.Expression.Discard(e, loc)
      }

    case WeededAst.Expression.Let(ident, mod, exp1, exp2, loc) =>
      // make a fresh variable symbol for the local variable.
      val sym = Symbol.freshVarSym(ident, BoundBy.Let)
      mapN(visitExp(exp1, env0, tenv0, ns0), visitExp(exp2, env0 + (ident.name -> sym), tenv0, ns0)) {
        case (e1, e2) => NamedAst.Expression.Let(sym, mod, e1, e2, loc)
      }

    case WeededAst.Expression.LetRec(ident, mod, exp1, exp2, loc) =>
      val sym = Symbol.freshVarSym(ident, BoundBy.Let)
      val env1 = env0 + (ident.name -> sym)
      mapN(visitExp(exp1, env1, tenv0, ns0), visitExp(exp2, env1, tenv0, ns0)) {
        case (e1, e2) => NamedAst.Expression.LetRec(sym, mod, e1, e2, loc)
      }

    case WeededAst.Expression.Region(tpe, loc) =>
      NamedAst.Expression.Region(tpe, loc).toSuccess

    case WeededAst.Expression.Scope(ident, exp, loc) =>
      // Introduce a fresh variable symbol for the region.
      val sym = Symbol.freshVarSym(ident, BoundBy.Let)

      // Introduce a rigid region variable for the region.
      val regionVar = Symbol.freshUnkindedTypeVarSym(Ast.VarText.SourceText(sym.text), isRegion = true, loc)

      val env1 = env0 + (ident.name -> sym)
      val tenv1 = tenv0 + (ident.name -> regionVar)
      mapN(visitExp(exp, env1, tenv1, ns0)) {
        case e => NamedAst.Expression.Scope(sym, regionVar, e, loc)
      }

    case WeededAst.Expression.Match(exp, rules, loc) =>
      val expVal = visitExp(exp, env0, tenv0, ns0)
      val rulesVal = traverse(rules) {
        case WeededAst.MatchRule(pat, guard, body) =>
          // extend the environment with every variable occurring in the pattern
          // and perform naming on the rule guard and body under the extended environment.
          val (p, env1) = visitPattern(pat)
          val extendedEnv = env0 ++ env1
          val gVal = traverseOpt(guard)(visitExp(_, extendedEnv, tenv0, ns0))
          val bVal = visitExp(body, extendedEnv, tenv0, ns0)
          mapN(gVal, bVal) {
            case (g, b) => NamedAst.MatchRule(p, g, b)
          }
      }
      mapN(expVal, rulesVal) {
        case (e, rs) => NamedAst.Expression.Match(e, rs, loc)
      }

    case WeededAst.Expression.TypeMatch(exp, rules, loc) =>
      val expVal = visitExp(exp, env0, tenv0, ns0)
      val rulesVal = traverse(rules) {
        case WeededAst.MatchTypeRule(ident, tpe, body) =>
          // extend the environment with the variable
          // and perform naming on the rule body under the extended environment.
          val sym = Symbol.freshVarSym(ident, BoundBy.Pattern)
          val env1 = Map(ident.name -> sym)
          val extendedEnv = env0 ++ env1
          mapN(visitType(tpe, allowWild = true, tenv0), visitExp(body, extendedEnv, tenv0, ns0)) {
            case (t, b) => NamedAst.MatchTypeRule(sym, t, b)
          }
      }
      mapN(expVal, rulesVal) {
        case (e, rs) => NamedAst.Expression.TypeMatch(e, rs, loc)
      }

    case WeededAst.Expression.Choose(star, exps, rules, loc) =>
      val expsVal = traverse(exps)(visitExp(_, env0, tenv0, ns0))
      val rulesVal = traverse(rules) {
        case WeededAst.ChoiceRule(pat0, exp0) =>
          val env1 = pat0.foldLeft(Map.empty[String, Symbol.VarSym]) {
            case (acc, WeededAst.ChoicePattern.Wild(loc)) => acc
            case (acc, WeededAst.ChoicePattern.Absent(loc)) => acc
            case (acc, WeededAst.ChoicePattern.Present(ident, loc)) => acc + (ident.name -> Symbol.freshVarSym(ident, BoundBy.Pattern))
          }
          val p = pat0.map {
            case WeededAst.ChoicePattern.Wild(loc) => NamedAst.ChoicePattern.Wild(loc)
            case WeededAst.ChoicePattern.Absent(loc) => NamedAst.ChoicePattern.Absent(loc)
            case WeededAst.ChoicePattern.Present(ident, loc) => NamedAst.ChoicePattern.Present(env1(ident.name), loc)
          }
          mapN(visitExp(exp0, env0 ++ env1, tenv0, ns0)) {
            case e => NamedAst.ChoiceRule(p, e)
          }
      }
      mapN(expsVal, rulesVal) {
        case (es, rs) => NamedAst.Expression.Choose(star, es, rs, loc)
      }

    case WeededAst.Expression.Tag(enumOpt, tag, expOpt, loc) =>

      expOpt match {
        case None =>
          // Case 1: The tag does not have an expression. Nothing more to be done.
          NamedAst.Expression.Tag(enumOpt, tag, None, loc).toSuccess
        case Some(exp) =>
          // Case 2: The tag has an expression. Perform naming on it.
          visitExp(exp, env0, tenv0, ns0) map {
            case e => NamedAst.Expression.Tag(enumOpt, tag, Some(e), loc)
          }
      }

    case WeededAst.Expression.Tuple(elms, loc) =>
      traverse(elms)(e => visitExp(e, env0, tenv0, ns0)) map {
        case es => NamedAst.Expression.Tuple(es, loc)
      }

    case WeededAst.Expression.RecordEmpty(loc) =>
      NamedAst.Expression.RecordEmpty(loc).toSuccess

    case WeededAst.Expression.RecordSelect(exp, field, loc) =>
      mapN(visitExp(exp, env0, tenv0, ns0)) {
        case e => NamedAst.Expression.RecordSelect(e, field, loc)
      }

    case WeededAst.Expression.RecordExtend(field, value, rest, loc) =>
      mapN(visitExp(value, env0, tenv0, ns0), visitExp(rest, env0, tenv0, ns0)) {
        case (v, r) => NamedAst.Expression.RecordExtend(field, v, r, loc)
      }

    case WeededAst.Expression.RecordRestrict(field, rest, loc) =>
      mapN(visitExp(rest, env0, tenv0, ns0)) {
        case r => NamedAst.Expression.RecordRestrict(field, r, loc)
      }

    case WeededAst.Expression.New(qname, exp, loc) =>
      mapN(traverseOpt(exp)(visitExp(_, env0, tenv0, ns0))) {
        case e => NamedAst.Expression.New(qname, e, loc)
      }

    case WeededAst.Expression.ArrayLit(exps, exp, loc) =>
      mapN(traverse(exps)(visitExp(_, env0, tenv0, ns0)), traverseOpt(exp)(visitExp(_, env0, tenv0, ns0))) {
        case (es, e) => NamedAst.Expression.ArrayLit(es, e, loc)
      }

    case WeededAst.Expression.ArrayNew(exp1, exp2, exp3, loc) =>
      mapN(visitExp(exp1, env0, tenv0, ns0), visitExp(exp2, env0, tenv0, ns0), traverseOpt(exp3)(visitExp(_, env0, tenv0, ns0))) {
        case (e1, e2, e3) => NamedAst.Expression.ArrayNew(e1, e2, e3, loc)
      }

    case WeededAst.Expression.ArrayLoad(base, index, loc) =>
      mapN(visitExp(base, env0, tenv0, ns0), visitExp(index, env0, tenv0, ns0)) {
        case (b, i) => NamedAst.Expression.ArrayLoad(b, i, loc)
      }

    case WeededAst.Expression.ArrayStore(base, index, elm, loc) =>
      mapN(visitExp(base, env0, tenv0, ns0), visitExp(index, env0, tenv0, ns0), visitExp(elm, env0, tenv0, ns0)) {
        case (b, i, e) => NamedAst.Expression.ArrayStore(b, i, e, loc)
      }

    case WeededAst.Expression.ArrayLength(base, loc) =>
      visitExp(base, env0, tenv0, ns0) map {
        case b => NamedAst.Expression.ArrayLength(b, loc)
      }

    case WeededAst.Expression.ArraySlice(base, startIndex, endIndex, loc) =>
      mapN(visitExp(base, env0, tenv0, ns0), visitExp(startIndex, env0, tenv0, ns0), visitExp(endIndex, env0, tenv0, ns0)) {
        case (b, i1, i2) => NamedAst.Expression.ArraySlice(b, i1, i2, loc)
      }

    case WeededAst.Expression.Ref(exp1, exp2, loc) =>
      mapN(visitExp(exp1, env0, tenv0, ns0), traverseOpt(exp2)(visitExp(_, env0, tenv0, ns0))) {
        case (e1, e2) =>
          NamedAst.Expression.Ref(e1, e2, loc)
      }

    case WeededAst.Expression.Deref(exp, loc) =>
      visitExp(exp, env0, tenv0, ns0) map {
        case e =>
          NamedAst.Expression.Deref(e, loc)
      }

    case WeededAst.Expression.Assign(exp1, exp2, loc) =>
      mapN(visitExp(exp1, env0, tenv0, ns0), visitExp(exp2, env0, tenv0, ns0)) {
        case (e1, e2) =>
          NamedAst.Expression.Assign(e1, e2, loc)
      }

    case WeededAst.Expression.Ascribe(exp, expectedType, expectedEff, loc) =>
      val expVal = visitExp(exp, env0, tenv0, ns0)
      val expectedTypVal = expectedType match {
        case None => (None: Option[NamedAst.Type]).toSuccess
        case Some(t) => mapN(visitType(t, allowWild = true, tenv0))(x => Some(x))
      }
      val expectedEffVal = visitPurityAndEffect(expectedEff, allowWild = true, tenv0)

      mapN(expVal, expectedTypVal, expectedEffVal) {
        case (e, t, f) => NamedAst.Expression.Ascribe(e, t, f, loc)
      }

    case WeededAst.Expression.Cast(exp, declaredType, declaredEff, loc) =>
      val expVal = visitExp(exp, env0, tenv0, ns0)
      val declaredTypVal = declaredType match {
        case None => (None: Option[NamedAst.Type]).toSuccess
        case Some(t) => mapN(visitType(t, allowWild = false, tenv0))(x => Some(x))
      }
      val declaredEffVal = visitPurityAndEffect(declaredEff, allowWild = false, tenv0)

      mapN(expVal, declaredTypVal, declaredEffVal) {
        case (e, t, f) => NamedAst.Expression.Cast(e, t, f, loc)
      }

    case WeededAst.Expression.Mask(exp, loc) =>
      mapN(visitExp(exp, env0, tenv0, ns0)) {
        case e => NamedAst.Expression.Mask(e, loc)
      }

    case WeededAst.Expression.Upcast(exp, loc) =>
      mapN(visitExp(exp, env0, tenv0, ns0)) {
        case e => NamedAst.Expression.Upcast(e, loc)
      }

    case WeededAst.Expression.Supercast(exp, loc) =>
      mapN(visitExp(exp, env0, tenv0, ns0)) {
        case e => NamedAst.Expression.Supercast(e, loc)
      }

    case WeededAst.Expression.Without(exp, eff, loc) =>
      val expVal = visitExp(exp, env0, tenv0, ns0)
      mapN(expVal) {
        e => NamedAst.Expression.Without(e, eff, loc)
      }

    case WeededAst.Expression.TryCatch(exp, rules, loc) =>
      val expVal = visitExp(exp, env0, tenv0, ns0)
      val rulesVal = traverse(rules) {
        case WeededAst.CatchRule(ident, className, body) =>
          val sym = Symbol.freshVarSym(ident, BoundBy.CatchRule)
          val bodyVal = visitExp(body, env0 + (ident.name -> sym), tenv0, ns0)
          mapN(bodyVal) {
            b => NamedAst.CatchRule(sym, className, b)
          }
      }

      mapN(expVal, rulesVal) {
        case (e, rs) => NamedAst.Expression.TryCatch(e, rs, loc)
      }

    case WeededAst.Expression.TryWith(e0, eff, rules0, loc) =>
      val eVal = visitExp(e0, env0, tenv0, ns0)
      val rulesVal = traverse(rules0) {
        case WeededAst.HandlerRule(op, fparams0, body0) =>
          val fparamsVal = traverse(fparams0)(visitFormalParam(_, tenv0))
          flatMapN(fparamsVal) {
            fparams =>
              // visit the body with the fparams in the env
              val env = env0 ++ getVarEnv(fparams)
              val bodyVal = visitExp(body0, env, tenv0, ns0)
              mapN(bodyVal)(NamedAst.HandlerRule(op, fparams, _))
          }
      }
      mapN(eVal, rulesVal) {
        case (e, rules) => NamedAst.Expression.TryWith(e, eff, rules, loc)
      }

    case WeededAst.Expression.Do(op, exps0, loc) =>
      val expsVal = traverse(exps0)(visitExp(_, env0, tenv0, ns0))
      mapN(expsVal) {
        exps => NamedAst.Expression.Do(op, exps, loc)
      }

    case WeededAst.Expression.Resume(exp, loc) =>
      val expVal = visitExp(exp, env0, tenv0, ns0)
      mapN(expVal) {
        e => NamedAst.Expression.Resume(e, loc)
      }

    case WeededAst.Expression.InvokeConstructor(className, args, sig, loc) =>
      val argsVal = traverse(args)(visitExp(_, env0, tenv0, ns0))
      val sigVal = traverse(sig)(visitType(_, allowWild = false, tenv0))
      mapN(argsVal, sigVal) {
        case (as, sig) => NamedAst.Expression.InvokeConstructor(className, as, sig, loc)
      }

    case WeededAst.Expression.InvokeMethod(className, methodName, exp, args, sig, retTpe, loc) =>
      val expVal = visitExp(exp, env0, tenv0, ns0)
      val argsVal = traverse(args)(visitExp(_, env0, tenv0, ns0))
      val sigVal = traverse(sig)(visitType(_, allowWild = false, tenv0))
      val retVal = visitType(retTpe, allowWild = false, tenv0)
      mapN(expVal, argsVal, sigVal, retVal) {
        case (e, as, sig, ret) => NamedAst.Expression.InvokeMethod(className, methodName, e, as, sig, ret, loc)
      }

    case WeededAst.Expression.InvokeStaticMethod(className, methodName, args, sig, retTpe, loc) =>
      val argsVal = traverse(args)(visitExp(_, env0, tenv0, ns0))
      val sigVal = traverse(sig)(visitType(_, allowWild = false, tenv0))
      val retVal = visitType(retTpe, allowWild = false, tenv0)
      mapN(argsVal, sigVal, retVal) {
        case (as, sig, ret) => NamedAst.Expression.InvokeStaticMethod(className, methodName, as, sig, ret, loc)
      }

    case WeededAst.Expression.GetField(className, fieldName, exp, loc) =>
      mapN(visitExp(exp, env0, tenv0, ns0)) {
        case e => NamedAst.Expression.GetField(className, fieldName, e, loc)
      }

    case WeededAst.Expression.PutField(className, fieldName, exp1, exp2, loc) =>
      mapN(visitExp(exp1, env0, tenv0, ns0), visitExp(exp2, env0, tenv0, ns0)) {
        case (e1, e2) => NamedAst.Expression.PutField(className, fieldName, e1, e2, loc)
      }

    case WeededAst.Expression.GetStaticField(className, fieldName, loc) =>
      NamedAst.Expression.GetStaticField(className, fieldName, loc).toSuccess

    case WeededAst.Expression.PutStaticField(className, fieldName, exp, loc) =>
      mapN(visitExp(exp, env0, tenv0, ns0)) {
        case e => NamedAst.Expression.PutStaticField(className, fieldName, e, loc)
      }

    case WeededAst.Expression.NewObject(tpe, methods, loc) =>
      mapN(visitType(tpe, allowWild = false, tenv0), traverse(methods)(visitJvmMethod(_, env0, tenv0, ns0))) {
        case (tpe, ms) =>
          val name = s"Anon$$${flix.genSym.freshId()}"
          NamedAst.Expression.NewObject(name, tpe, ms, loc)
      }

    case WeededAst.Expression.NewChannel(exp1, exp2, loc) =>
      mapN(visitExp(exp1, env0, tenv0, ns0), visitExp(exp2, env0, tenv0, ns0)) {
        case (e1, e2) => NamedAst.Expression.NewChannel(e1, e2, loc)
      }

    case WeededAst.Expression.GetChannel(exp, loc) =>
      visitExp(exp, env0, tenv0, ns0) map {
        case e => NamedAst.Expression.GetChannel(e, loc)
      }

    case WeededAst.Expression.PutChannel(exp1, exp2, loc) =>
      mapN(visitExp(exp1, env0, tenv0, ns0), visitExp(exp2, env0, tenv0, ns0)) {
        case (e1, e2) => NamedAst.Expression.PutChannel(e1, e2, loc)
      }

    case WeededAst.Expression.SelectChannel(rules, default, loc) =>
      val rulesVal = traverse(rules) {
        case WeededAst.SelectChannelRule(ident, chan, body) =>
          // make a fresh variable symbol for the local recursive variable.
          val sym = Symbol.freshVarSym(ident, BoundBy.SelectRule)
          val env1 = env0 + (ident.name -> sym)
          mapN(visitExp(chan, env0, tenv0, ns0), visitExp(body, env1, tenv0, ns0)) {
            case (c, b) => NamedAst.SelectChannelRule(sym, c, b)
          }
      }

      val defaultVal = default match {
        case Some(exp) => visitExp(exp, env0, tenv0, ns0) map {
          case e => Some(e)
        }
        case None => None.toSuccess
      }

      mapN(rulesVal, defaultVal) {
        case (rs, d) => NamedAst.Expression.SelectChannel(rs, d, loc)
      }

    case WeededAst.Expression.Spawn(exp1, exp2, loc) =>
      mapN(visitExp(exp1, env0, tenv0, ns0), visitExp(exp2, env0, tenv0, ns0)) {
        case (e1, e2) =>
          NamedAst.Expression.Spawn(e1, e2, loc)
      }

    case WeededAst.Expression.Par(exp, loc) =>
      mapN(visitExp(exp, env0, tenv0, ns0)) {
        case e => NamedAst.Expression.Par(e, loc)
      }

    case WeededAst.Expression.ParYield(frags, exp, loc) =>
      // Create a mutable env that will be updated during traverse
      var finalEnv = env0

      val fragsVal = traverse(frags) {
        case WeededAst.ParYieldFragment(pat, e, l) =>
          val (p, env1) = visitPattern(pat)
          finalEnv = finalEnv ++ env1
          mapN(visitExp(e, env0, tenv0, ns0)) {
            case e1 => NamedAst.ParYieldFragment(p, e1, l)
          }
      }

      // Combine everything
      mapN(fragsVal, visitExp(exp, finalEnv, tenv0, ns0)) {
        case (fs, e) => NamedAst.Expression.ParYield(fs, e, loc)
      }

    case WeededAst.Expression.Lazy(exp, loc) =>
      visitExp(exp, env0, tenv0, ns0) map {
        case e => NamedAst.Expression.Lazy(e, loc)
      }

    case WeededAst.Expression.Force(exp, loc) =>
      visitExp(exp, env0, tenv0, ns0) map {
        case e => NamedAst.Expression.Force(e, loc)
      }

    case WeededAst.Expression.FixpointConstraintSet(cs0, loc) =>
      mapN(traverse(cs0)(visitConstraint(_, env0, tenv0, ns0))) {
        case cs =>
          NamedAst.Expression.FixpointConstraintSet(cs, loc)
      }

    case WeededAst.Expression.FixpointLambda(pparams, exp, loc) =>
      val psVal = traverse(pparams)(visitPredicateParam(_, tenv0))
      val expVal = visitExp(exp, env0, tenv0, ns0)
      mapN(psVal, expVal) {
        case (ps, e) => NamedAst.Expression.FixpointLambda(ps, e, loc)
      }

    case WeededAst.Expression.FixpointMerge(exp1, exp2, loc) =>
      mapN(visitExp(exp1, env0, tenv0, ns0), visitExp(exp2, env0, tenv0, ns0)) {
        case (e1, e2) => NamedAst.Expression.FixpointMerge(e1, e2, loc)
      }

    case WeededAst.Expression.FixpointSolve(exp, loc) =>
      visitExp(exp, env0, tenv0, ns0) map {
        case e => NamedAst.Expression.FixpointSolve(e, loc)
      }

    case WeededAst.Expression.FixpointFilter(ident, exp, loc) =>
      mapN(visitExp(exp, env0, tenv0, ns0)) {
        case e => NamedAst.Expression.FixpointFilter(ident, e, loc)
      }

    case WeededAst.Expression.FixpointInject(exp, pred, loc) =>
      mapN(visitExp(exp, env0, tenv0, ns0)) {
        case e => NamedAst.Expression.FixpointInject(e, pred, loc)
      }

    case WeededAst.Expression.FixpointProject(pred, exp1, exp2, loc) =>
      mapN(visitExp(exp1, env0, tenv0, ns0), visitExp(exp2, env0, tenv0, ns0)) {
        case (e1, e2) => NamedAst.Expression.FixpointProject(pred, e1, e2, loc)
      }
  }

  /**
    * Names the given pattern `pat0` and returns map from variable names to variable symbols.
    */
  private def visitPattern(pat0: WeededAst.Pattern)(implicit flix: Flix): (NamedAst.Pattern, Map[String, Symbol.VarSym]) = {
    val m = mutable.Map.empty[String, Symbol.VarSym]

    def visit(p: WeededAst.Pattern): NamedAst.Pattern = p match {
      case WeededAst.Pattern.Wild(loc) => NamedAst.Pattern.Wild(loc)
      case WeededAst.Pattern.Var(ident, loc) =>
        // make a fresh variable symbol for the local variable.
        val sym = Symbol.freshVarSym(ident, BoundBy.Pattern)
        m += (ident.name -> sym)
        NamedAst.Pattern.Var(sym, loc)

      case WeededAst.Pattern.Cst(cst, loc) => NamedAst.Pattern.Cst(cst, loc)

      case WeededAst.Pattern.Tag(enumOpt, tag, pat, loc) =>
        NamedAst.Pattern.Tag(enumOpt, tag, visit(pat), loc)

      case WeededAst.Pattern.Tuple(elms, loc) => NamedAst.Pattern.Tuple(elms map visit, loc)

      case WeededAst.Pattern.Array(elms, loc) => NamedAst.Pattern.Array(elms map visit, loc)

      case WeededAst.Pattern.ArrayTailSpread(elms, ident, loc) => ident match {
        case None =>
          val sym = Symbol.freshVarSym("_", BoundBy.Pattern, loc)
          NamedAst.Pattern.ArrayTailSpread(elms map visit, sym, loc)
        case Some(id) =>
          val sym = Symbol.freshVarSym(id, BoundBy.Pattern)
          m += (id.name -> sym)
          NamedAst.Pattern.ArrayTailSpread(elms map visit, sym, loc)
      }
      case WeededAst.Pattern.ArrayHeadSpread(ident, elms, loc) => ident match {
        case None =>
          val sym = Symbol.freshVarSym("_", BoundBy.Pattern, loc)
          NamedAst.Pattern.ArrayTailSpread(elms map visit, sym, loc)
        case Some(id) =>
          val sym = Symbol.freshVarSym(id, BoundBy.Pattern)
          m += (id.name -> sym)
          NamedAst.Pattern.ArrayHeadSpread(sym, elms map visit, loc)
      }
    }

    (visit(pat0), m.toMap)
  }

  /**
    * Names the given pattern `pat0` under the given environments `env0` and `uenv0`.
    *
    * Every variable in the pattern must be bound by the environment.
    */
  private def visitPattern(pat0: WeededAst.Pattern, env0: Map[String, Symbol.VarSym])(implicit flix: Flix): NamedAst.Pattern = {
    def visit(p: WeededAst.Pattern): NamedAst.Pattern = p match {
      case WeededAst.Pattern.Wild(loc) => NamedAst.Pattern.Wild(loc)
      case WeededAst.Pattern.Var(ident, loc) =>
        val sym = env0(ident.name)
        NamedAst.Pattern.Var(sym, loc)
      case WeededAst.Pattern.Cst(cst, loc) => NamedAst.Pattern.Cst(cst, loc)

      case WeededAst.Pattern.Tag(enumOpt, tag, pat, loc) =>
        NamedAst.Pattern.Tag(enumOpt, tag, visit(pat), loc)

      case WeededAst.Pattern.Tuple(elms, loc) => NamedAst.Pattern.Tuple(elms map visit, loc)

      case WeededAst.Pattern.Array(elms, loc) => NamedAst.Pattern.Array(elms map visit, loc)
      case WeededAst.Pattern.ArrayTailSpread(elms, ident, loc) => ident match {
        case None =>
          val sym = Symbol.freshVarSym("_", BoundBy.Pattern, loc)
          NamedAst.Pattern.ArrayTailSpread(elms map visit, sym, loc)
        case Some(value) =>
          val sym = env0(value.name)
          NamedAst.Pattern.ArrayTailSpread(elms map visit, sym, loc)
      }
      case WeededAst.Pattern.ArrayHeadSpread(ident, elms, loc) => ident match {
        case None =>
          val sym = Symbol.freshVarSym("_", BoundBy.Pattern, loc)
          NamedAst.Pattern.ArrayHeadSpread(sym, elms map visit, loc)
        case Some(value) =>
          val sym = env0(value.name)
          NamedAst.Pattern.ArrayHeadSpread(sym, elms map visit, loc)
      }
    }

    visit(pat0)
  }

  /**
    * Names the given head predicate `head` under the given environments `env0`, `uenv0`, and `tenv0`.
    */
  private def visitHeadPredicate(head: WeededAst.Predicate.Head, outerEnv: Map[String, Symbol.VarSym], headEnv0: Map[String, Symbol.VarSym], ruleEnv0: Map[String, Symbol.VarSym], tenv0: Map[String, Symbol.UnkindedTypeVarSym], ns0: Name.NName)(implicit flix: Flix): Validation[NamedAst.Predicate.Head, NameError] = head match {
    case WeededAst.Predicate.Head.Atom(pred, den, terms, loc) =>
      for {
        ts <- traverse(terms)(t => visitExp(t, outerEnv ++ headEnv0 ++ ruleEnv0, tenv0, ns0))
      } yield NamedAst.Predicate.Head.Atom(pred, den, ts, loc)
  }

  /**
    * Names the given body predicate `body` under the given environments `env0`, `uenv0`, and `tenv0`.
    */
  private def visitBodyPredicate(body: WeededAst.Predicate.Body, outerEnv: Map[String, Symbol.VarSym], headEnv0: Map[String, Symbol.VarSym], ruleEnv0: Map[String, Symbol.VarSym], tenv0: Map[String, Symbol.UnkindedTypeVarSym], ns0: Name.NName)(implicit flix: Flix): Validation[NamedAst.Predicate.Body, NameError] = body match {
    case WeededAst.Predicate.Body.Atom(pred, den, polarity, fixity, terms, loc) =>
      val ts = terms.map(t => visitPattern(t, outerEnv ++ ruleEnv0))
      NamedAst.Predicate.Body.Atom(pred, den, polarity, fixity, ts, loc).toSuccess

    case WeededAst.Predicate.Body.Guard(exp, loc) =>
      for {
        e <- visitExp(exp, outerEnv ++ headEnv0 ++ ruleEnv0, tenv0, ns0)
      } yield NamedAst.Predicate.Body.Guard(e, loc)

    case WeededAst.Predicate.Body.Loop(idents, exp, loc) =>
      val varSyms = idents.map(ident => headEnv0(ident.name))
      for {
        e <- visitExp(exp, outerEnv ++ headEnv0 ++ ruleEnv0, tenv0, ns0)
      } yield NamedAst.Predicate.Body.Loop(varSyms, e, loc)

  }

  /**
    * Returns the identifiers that are visible in the head scope by the given body predicate `p0`.
    */
  private def visibleInHeadScope(p0: WeededAst.Predicate.Body): List[Name.Ident] = p0 match {
    case WeededAst.Predicate.Body.Atom(_, _, _, _, terms, _) => terms.flatMap(freeVars)
    case WeededAst.Predicate.Body.Guard(exp, _) => Nil
    case WeededAst.Predicate.Body.Loop(idents, _, _) => idents
  }

  /**
    * Returns the identifiers that are visible in the rule scope by the given body predicate `p0`.
    */
  private def visibleInRuleScope(p0: WeededAst.Predicate.Body): List[Name.Ident] = p0 match {
    case WeededAst.Predicate.Body.Atom(_, _, _, _, terms, _) => terms.flatMap(freeVars)
    case WeededAst.Predicate.Body.Guard(_, _) => Nil
    case WeededAst.Predicate.Body.Loop(_, _, _) => Nil
  }

  /**
    * Names the given type `tpe` under the given environments `uenv0` and `tenv0`.
    */
  private def visitType(t0: WeededAst.Type, allowWild: Boolean, tenv0: Map[String, Symbol.UnkindedTypeVarSym])(implicit flix: Flix): Validation[NamedAst.Type, NameError] = {
    def visit(tpe0: WeededAst.Type): Validation[NamedAst.Type, NameError] = tpe0 match {
      case WeededAst.Type.Unit(loc) => NamedAst.Type.Unit(loc).toSuccess

      case WeededAst.Type.Var(ident, loc) =>
        //
        // Check for [[NameError.SuspiciousTypeVarName]].
        //
        if (isSuspiciousTypeVarName(ident.name)) {
          NameError.SuspiciousTypeVarName(ident.name, loc).toFailure
        } else if (ident.isWild) {
          if (allowWild) {
            // Wild idents will not be in the environment. Create a tvar instead.
            NamedAst.Type.Var(Symbol.freshUnkindedTypeVarSym(Ast.VarText.Absent, isRegion = false, loc), loc).toSuccess
          } else {
            // Wild not allowed here. Error.
            NameError.IllegalWildType(ident, ident.loc).toFailure
          }
        } else {
          tenv0.get(ident.name) match {
            case None => NameError.UndefinedTypeVar(ident.name, loc).toFailure
            case Some(tvar) => NamedAst.Type.Var(tvar, loc).toSuccess
          }
        }

      case WeededAst.Type.Ambiguous(qname, loc) =>
        if (qname.isUnqualified) {
          val name = qname.ident.name

          tenv0.get(name) match {
            case None =>
              // Case 1: the name is not a variable, resolve it later
              NamedAst.Type.Ambiguous(qname, loc).toSuccess
            case Some(tvar) =>
              // TODO NS-REFACTOR resolve this in Resolver instead
              // Case 2: the name is a type variable.
              NamedAst.Type.Var(tvar, loc).toSuccess
          }
        }
        else
          NamedAst.Type.Ambiguous(qname, loc).toSuccess

      case WeededAst.Type.Tuple(elms, loc) =>
        mapN(traverse(elms)(visit(_))) {
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
        // Disambiguate the qname.

        mapN(traverse(targs)(visit(_)), visit(rest)) {
          case (ts, r) => NamedAst.Type.SchemaRowExtendWithAlias(qname, ts, r, loc)
        }

      case WeededAst.Type.SchemaRowExtendByTypes(ident, den, tpes, rest, loc) =>
        mapN(traverse(tpes)(visit(_)), visit(rest)) {
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
        val purAndEffVal = visitPurityAndEffect(purAndEff0, allowWild, tenv0)
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

      case WeededAst.Type.Ascribe(tpe, kind, loc) =>
        mapN(visit(tpe)) {
          t => NamedAst.Type.Ascribe(t, kind, loc)
        }
    }

    visit(t0)
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
    case "ref" => true
    case "pure" => true
    case "impure" => true
    case _ => false
  }

  /**
    * Performs naming on the given purity and effect.
    */
  private def visitPurityAndEffect(purAndEff: WeededAst.PurityAndEffect, allowWild: Boolean, tenv: Map[String, Symbol.UnkindedTypeVarSym])(implicit flix: Flix): Validation[NamedAst.PurityAndEffect, NameError] = purAndEff match {
    case WeededAst.PurityAndEffect(pur0, eff0) =>
      val purVal = traverseOpt(pur0)(visitType(_, allowWild, tenv))
      val effVal = traverseOpt(eff0)(effs => traverse(effs)(visitType(_, allowWild, tenv)))
      mapN(purVal, effVal) {
        case (pur, eff) => NamedAst.PurityAndEffect(pur, eff)
      }
  }

  /**
    * Returns all the free variables in the given expression `exp0`.
    */
  private def freeVars(exp0: WeededAst.Expression): List[Name.Ident] = exp0 match {
    case WeededAst.Expression.Wild(_) => Nil
    case WeededAst.Expression.VarOrDefOrSig(ident, _) => List(ident)
    case WeededAst.Expression.DefOrSig(_, _) => Nil
    case WeededAst.Expression.Hole(_, _) => Nil
    case WeededAst.Expression.HoleWithExp(exp, _) => freeVars(exp)
    case WeededAst.Expression.Use(_, exp, _) => freeVars(exp)
    case WeededAst.Expression.Cst(Ast.Constant.Unit, _) => Nil
    case WeededAst.Expression.Cst(Ast.Constant.Null, _) => Nil
    case WeededAst.Expression.Cst(Ast.Constant.Bool(true), _) => Nil
    case WeededAst.Expression.Cst(Ast.Constant.Bool(false), _) => Nil
    case WeededAst.Expression.Cst(Ast.Constant.Char(_), _) => Nil
    case WeededAst.Expression.Cst(Ast.Constant.Float32(_), _) => Nil
    case WeededAst.Expression.Cst(Ast.Constant.Float64(_), _) => Nil
    case WeededAst.Expression.Cst(Ast.Constant.BigDecimal(_), _) => Nil
    case WeededAst.Expression.Cst(Ast.Constant.Int8(_), _) => Nil
    case WeededAst.Expression.Cst(Ast.Constant.Int16(_), _) => Nil
    case WeededAst.Expression.Cst(Ast.Constant.Int32(_), _) => Nil
    case WeededAst.Expression.Cst(Ast.Constant.Int64(_), _) => Nil
    case WeededAst.Expression.Cst(Ast.Constant.BigInt(_), _) => Nil
    case WeededAst.Expression.Cst(Ast.Constant.Str(_), _) => Nil
    case WeededAst.Expression.Apply(exp, exps, _) => freeVars(exp) ++ exps.flatMap(freeVars)
    case WeededAst.Expression.Lambda(fparam, exp, _) => filterBoundVars(freeVars(exp), List(fparam.ident))
    case WeededAst.Expression.Unary(_, exp, _) => freeVars(exp)
    case WeededAst.Expression.Binary(_, exp1, exp2, _) => freeVars(exp1) ++ freeVars(exp2)
    case WeededAst.Expression.IfThenElse(exp1, exp2, exp3, _) => freeVars(exp1) ++ freeVars(exp2) ++ freeVars(exp3)
    case WeededAst.Expression.Stm(exp1, exp2, _) => freeVars(exp1) ++ freeVars(exp2)
    case WeededAst.Expression.Discard(exp, _) => freeVars(exp)
    case WeededAst.Expression.Let(ident, _, exp1, exp2, _) => freeVars(exp1) ++ filterBoundVars(freeVars(exp2), List(ident))
    case WeededAst.Expression.LetRec(ident, _, exp1, exp2, _) => filterBoundVars(freeVars(exp1) ++ freeVars(exp2), List(ident))
    case WeededAst.Expression.Region(_, _) => Nil
    case WeededAst.Expression.Scope(ident, exp, _) => filterBoundVars(freeVars(exp), List(ident))
    case WeededAst.Expression.Match(exp, rules, _) => freeVars(exp) ++ rules.flatMap {
      case WeededAst.MatchRule(pat, guard, body) => filterBoundVars(guard.toList.flatMap(freeVars) ++ freeVars(body), freeVars(pat))
    }
    case WeededAst.Expression.TypeMatch(exp, rules, _) => freeVars(exp) ++ rules.flatMap {
      case WeededAst.MatchTypeRule(ident, _, body) => filterBoundVars(freeVars(body), List(ident))
    }
    case WeededAst.Expression.Choose(_, exps, rules, _) => exps.flatMap(freeVars) ++ rules.flatMap {
      case WeededAst.ChoiceRule(pat, exp) => filterBoundVars(freeVars(exp), pat.flatMap(freeVars))
    }
    case WeededAst.Expression.Tag(_, _, expOpt, _) => expOpt.map(freeVars).getOrElse(Nil)
    case WeededAst.Expression.Tuple(elms, _) => elms.flatMap(freeVars)
    case WeededAst.Expression.RecordEmpty(_) => Nil
    case WeededAst.Expression.RecordSelect(exp, _, _) => freeVars(exp)
    case WeededAst.Expression.RecordExtend(_, exp, rest, _) => freeVars(exp) ++ freeVars(rest)
    case WeededAst.Expression.RecordRestrict(_, rest, _) => freeVars(rest)
    case WeededAst.Expression.New(_, exp, _) => exp.map(freeVars).getOrElse(Nil)
    case WeededAst.Expression.ArrayLit(exps, exp, _) => exps.flatMap(freeVars) ++ exp.map(freeVars).getOrElse(Nil)
    case WeededAst.Expression.ArrayNew(exp1, exp2, exp3, _) => freeVars(exp1) ++ freeVars(exp2) ++ exp3.map(freeVars).getOrElse(Nil)
    case WeededAst.Expression.ArrayLoad(base, index, _) => freeVars(base) ++ freeVars(index)
    case WeededAst.Expression.ArrayStore(base, index, elm, _) => freeVars(base) ++ freeVars(index) ++ freeVars(elm)
    case WeededAst.Expression.ArrayLength(base, _) => freeVars(base)
    case WeededAst.Expression.ArraySlice(base, startIndex, endIndex, _) => freeVars(base) ++ freeVars(startIndex) ++ freeVars(endIndex)
    case WeededAst.Expression.Ref(exp1, exp2, _) => freeVars(exp1) ++ exp2.map(freeVars).getOrElse(Nil)
    case WeededAst.Expression.Deref(exp, _) => freeVars(exp)
    case WeededAst.Expression.Assign(exp1, exp2, _) => freeVars(exp1) ++ freeVars(exp2)
    case WeededAst.Expression.Ascribe(exp, _, _, _) => freeVars(exp)
    case WeededAst.Expression.Cast(exp, _, _, _) => freeVars(exp)
    case WeededAst.Expression.Mask(exp, _) => freeVars(exp)
    case WeededAst.Expression.Upcast(exp, _) => freeVars(exp)
    case WeededAst.Expression.Supercast(exp, _) => freeVars(exp)
    case WeededAst.Expression.Without(exp, _, _) => freeVars(exp)
    case WeededAst.Expression.Do(_, exps, _) => exps.flatMap(freeVars)
    case WeededAst.Expression.Resume(exp, _) => freeVars(exp)
    case WeededAst.Expression.TryWith(exp, _, rules, _) =>
      rules.foldLeft(freeVars(exp)) {
        case (fvs, WeededAst.HandlerRule(_, fparams, body)) => fvs ++ filterBoundVars(freeVars(body), fparams.map(_.ident))
      }
    case WeededAst.Expression.TryCatch(exp, rules, _) =>
      rules.foldLeft(freeVars(exp)) {
        case (fvs, WeededAst.CatchRule(ident, _, body)) => fvs ++ filterBoundVars(freeVars(body), List(ident))
      }
    case WeededAst.Expression.InvokeConstructor(_, args, _, _) => args.flatMap(freeVars)
    case WeededAst.Expression.InvokeMethod(_, _, exp, args, _, _, _) => freeVars(exp) ++ args.flatMap(freeVars)
    case WeededAst.Expression.InvokeStaticMethod(_, _, args, _, _, _) => args.flatMap(freeVars)
    case WeededAst.Expression.GetField(_, _, exp, _) => freeVars(exp)
    case WeededAst.Expression.PutField(_, _, exp1, exp2, _) => freeVars(exp1) ++ freeVars(exp2)
    case WeededAst.Expression.GetStaticField(_, _, _) => Nil
    case WeededAst.Expression.PutStaticField(_, _, exp, _) => freeVars(exp)
    case WeededAst.Expression.NewObject(_, methods, _) => methods.flatMap(m => freeVars(m.exp))
    case WeededAst.Expression.NewChannel(exp1, exp2, _) => freeVars(exp1) ++ freeVars(exp2)
    case WeededAst.Expression.GetChannel(exp, _) => freeVars(exp)
    case WeededAst.Expression.PutChannel(exp1, exp2, _) => freeVars(exp1) ++ freeVars(exp2)
    case WeededAst.Expression.SelectChannel(rules, default, _) =>
      val rulesFreeVars = rules.flatMap {
        case WeededAst.SelectChannelRule(ident, chan, exp) =>
          freeVars(chan) ++ filterBoundVars(freeVars(exp), List(ident))
      }
      val defaultFreeVars = default.map(freeVars).getOrElse(Nil)
      rulesFreeVars ++ defaultFreeVars
    case WeededAst.Expression.Spawn(exp1, exp2, _) => freeVars(exp1) ++ freeVars(exp2)
    case WeededAst.Expression.Par(exp, _) => freeVars(exp)
    case WeededAst.Expression.ParYield(frags, exp, _) => frags.flatMap(f => freeVars(f.exp)) ::: freeVars(exp)
    case WeededAst.Expression.Lazy(exp, _) => freeVars(exp)
    case WeededAst.Expression.Force(exp, _) => freeVars(exp)
    case WeededAst.Expression.FixpointConstraintSet(cs, _) => cs.flatMap(freeVarsConstraint)
    case WeededAst.Expression.FixpointLambda(_, exp, _) => freeVars(exp)
    case WeededAst.Expression.FixpointMerge(exp1, exp2, _) => freeVars(exp1) ++ freeVars(exp2)
    case WeededAst.Expression.FixpointSolve(exp, _) => freeVars(exp)
    case WeededAst.Expression.FixpointFilter(_, exp, _) => freeVars(exp)
    case WeededAst.Expression.FixpointInject(exp, _, _) => freeVars(exp)
    case WeededAst.Expression.FixpointProject(_, exp1, exp2, _) => freeVars(exp1) ++ freeVars(exp2)
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
    case WeededAst.Pattern.Cst(Ast.Constant.Null, loc) => throw InternalCompilerException("unexpected null pattern", loc)
    case WeededAst.Pattern.Tag(enumName, tagName, p, loc) => freeVars(p)
    case WeededAst.Pattern.Tuple(elms, loc) => elms flatMap freeVars
    case WeededAst.Pattern.Array(elms, loc) => elms flatMap freeVars
    case WeededAst.Pattern.ArrayTailSpread(elms, ident, loc) =>
      val freeElms = elms flatMap freeVars
      ident match {
        case None => freeElms
        case Some(value) => freeElms.appended(value)
      }
    case WeededAst.Pattern.ArrayHeadSpread(ident, elms, loc) =>
      val freeElms = elms flatMap freeVars
      ident match {
        case None => freeElms
        case Some(value) => freeElms.appended(value)
      }
  }

  /**
    * Returns all free variables in the given null pattern `pat0`.
    */
  private def freeVars(pat0: WeededAst.ChoicePattern): List[Name.Ident] = pat0 match {
    case ChoicePattern.Wild(_) => Nil
    case ChoicePattern.Present(ident, _) => ident :: Nil
    case ChoicePattern.Absent(_) => Nil
  }

  /**
    * Returns the free variables in the given type `tpe0`.
    */
  private def freeVars(tpe0: WeededAst.Type): List[Name.Ident] = tpe0 match {
    case WeededAst.Type.Var(ident, loc) => ident :: Nil
    case WeededAst.Type.Ambiguous(qname, loc) => Nil
    case WeededAst.Type.Unit(loc) => Nil
    case WeededAst.Type.Tuple(elms, loc) => elms.flatMap(freeVars)
    case WeededAst.Type.RecordRowEmpty(loc) => Nil
    case WeededAst.Type.RecordRowExtend(l, t, r, loc) => freeVars(t) ::: freeVars(r)
    case WeededAst.Type.Record(row, loc) => freeVars(row)
    case WeededAst.Type.SchemaRowEmpty(loc) => Nil
    case WeededAst.Type.SchemaRowExtendByTypes(_, _, ts, r, loc) => ts.flatMap(freeVars) ::: freeVars(r)
    case WeededAst.Type.SchemaRowExtendByAlias(_, ts, r, _) => ts.flatMap(freeVars) ::: freeVars(r)
    case WeededAst.Type.Schema(row, loc) => freeVars(row)
    case WeededAst.Type.Relation(ts, loc) => ts.flatMap(freeVars)
    case WeededAst.Type.Lattice(ts, loc) => ts.flatMap(freeVars)
    case WeededAst.Type.Native(fqm, loc) => Nil
    case WeededAst.Type.Arrow(tparams, WeededAst.PurityAndEffect(pur, eff), tresult, loc) => tparams.flatMap(freeVars) ::: pur.toList.flatMap(freeVars) ::: eff.toList.flatMap(_.flatMap(freeVars)) ::: freeVars(tresult)
    case WeededAst.Type.Apply(tpe1, tpe2, loc) => freeVars(tpe1) ++ freeVars(tpe2)
    case WeededAst.Type.True(loc) => Nil
    case WeededAst.Type.False(loc) => Nil
    case WeededAst.Type.Not(tpe, loc) => freeVars(tpe)
    case WeededAst.Type.And(tpe1, tpe2, loc) => freeVars(tpe1) ++ freeVars(tpe2)
    case WeededAst.Type.Or(tpe1, tpe2, loc) => freeVars(tpe1) ++ freeVars(tpe2)
    case WeededAst.Type.Complement(tpe, loc) => freeVars(tpe)
    case WeededAst.Type.Union(tpe1, tpe2, loc) => freeVars(tpe1) ++ freeVars(tpe2)
    case WeededAst.Type.Intersection(tpe1, tpe2, loc) => freeVars(tpe1) ++ freeVars(tpe2)
    case WeededAst.Type.Read(tpe, loc) => freeVars(tpe)
    case WeededAst.Type.Write(tpe, loc) => freeVars(tpe)
    case WeededAst.Type.Empty(_) => Nil
    case WeededAst.Type.Ascribe(tpe, _, _) => freeVars(tpe)
  }

  /**
    * Returns the free variables under the type environment `tenv`.
    */
  private def freeVarsInTenv(tpe0: WeededAst.Type, tenv: Map[String, Symbol.UnkindedTypeVarSym]): List[Name.Ident] = {
    def visit(tpe0: WeededAst.Type): List[Name.Ident] = tpe0 match {
      case WeededAst.Type.Var(ident, loc) if tenv.contains(ident.name) => Nil
      case WeededAst.Type.Var(ident, loc) => ident :: Nil
      case WeededAst.Type.Ambiguous(qname, loc) => Nil
      case WeededAst.Type.Unit(loc) => Nil
      case WeededAst.Type.Tuple(elms, loc) => elms.flatMap(visit)
      case WeededAst.Type.RecordRowEmpty(loc) => Nil
      case WeededAst.Type.RecordRowExtend(l, t, r, loc) => visit(t) ::: visit(r)
      case WeededAst.Type.Record(row, loc) => visit(row)
      case WeededAst.Type.SchemaRowEmpty(loc) => Nil
      case WeededAst.Type.SchemaRowExtendByTypes(_, _, ts, r, loc) => ts.flatMap(visit) ::: visit(r)
      case WeededAst.Type.SchemaRowExtendByAlias(_, ts, r, _) => ts.flatMap(visit) ::: visit(r)
      case WeededAst.Type.Schema(row, loc) => visit(row)
      case WeededAst.Type.Relation(ts, loc) => ts.flatMap(visit)
      case WeededAst.Type.Lattice(ts, loc) => ts.flatMap(visit)
      case WeededAst.Type.Native(fqm, loc) => Nil
      case WeededAst.Type.Arrow(tparams, WeededAst.PurityAndEffect(pur, eff), tresult, loc) => tparams.flatMap(visit) ::: pur.toList.flatMap(visit) ::: eff.toList.flatMap(_.flatMap(visit)) ::: visit(tresult)
      case WeededAst.Type.Apply(tpe1, tpe2, loc) => visit(tpe1) ++ visit(tpe2)
      case WeededAst.Type.True(loc) => Nil
      case WeededAst.Type.False(loc) => Nil
      case WeededAst.Type.Not(tpe, loc) => visit(tpe)
      case WeededAst.Type.And(tpe1, tpe2, loc) => visit(tpe1) ++ visit(tpe2)
      case WeededAst.Type.Or(tpe1, tpe2, loc) => visit(tpe1) ++ visit(tpe2)
      case WeededAst.Type.Complement(tpe, loc) => visit(tpe)
      case WeededAst.Type.Union(tpe1, tpe2, loc) => visit(tpe1) ++ visit(tpe2)
      case WeededAst.Type.Intersection(tpe1, tpe2, loc) => visit(tpe1) ++ visit(tpe2)
      case WeededAst.Type.Read(tpe, loc) => visit(tpe)
      case WeededAst.Type.Write(tpe, loc) => visit(tpe)
      case WeededAst.Type.Empty(_) => Nil
      case WeededAst.Type.Ascribe(tpe, _, _) => visit(tpe)
    }

    visit(tpe0)
  }

  /**
    * Returns the free variables in the given constraint `c0`.
    */
  private def freeVarsConstraint(c0: WeededAst.Constraint): List[Name.Ident] = c0 match {
    case WeededAst.Constraint(head, body, loc) => freeVarsHeadPred(head) ::: body.flatMap(freeVarsBodyPred)
  }

  /**
    * Returns the free variables in the given head predicate `h0`.
    */
  private def freeVarsHeadPred(h0: WeededAst.Predicate.Head): List[Name.Ident] = h0 match {
    case WeededAst.Predicate.Head.Atom(_, den, terms, loc) => terms.flatMap(freeVars)
  }

  /**
    * Returns the free variables in the given body predicate `b0`.
    */
  private def freeVarsBodyPred(b0: WeededAst.Predicate.Body): List[Name.Ident] = b0 match {
    case WeededAst.Predicate.Body.Atom(_, _, _, _, terms, _) => terms.flatMap(freeVars)
    case WeededAst.Predicate.Body.Guard(exp, _) => freeVars(exp)
    case WeededAst.Predicate.Body.Loop(_, exp, _) => freeVars(exp)
  }

  /**
    * Translates the given weeded annotation to a named annotation.
    */
  private def visitAnnotation(ann: WeededAst.Annotation, env0: Map[String, Symbol.VarSym], tenv0: Map[String, Symbol.UnkindedTypeVarSym], ns0: Name.NName)(implicit flix: Flix): Validation[NamedAst.Annotation, NameError] = ann match {
    case WeededAst.Annotation(name, args, loc) =>
      mapN(traverse(args)(visitExp(_, env0, tenv0, ns0))) {
        case as => NamedAst.Annotation(name, as, loc)
      }
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
  private def visitFormalParam(fparam: WeededAst.FormalParam, tenv0: Map[String, Symbol.UnkindedTypeVarSym])(implicit flix: Flix): Validation[NamedAst.FormalParam, NameError] = fparam match {
    case WeededAst.FormalParam(ident, mod, optType, loc) =>
      // Generate a fresh variable symbol for the identifier.
      val freshSym = Symbol.freshVarSym(ident, BoundBy.FormalParam)

      // Compute the type of the formal parameter or use the type variable of the symbol.
      val tpeVal = optType match {
        case None => NamedAst.Type.Var(freshSym.tvar.sym.withoutKind, loc).toSuccess
        case Some(t) => visitType(t, allowWild = true, tenv0)
      }

      val src = optType match {
        case None => Ast.TypeSource.Inferred
        case Some(_) => Ast.TypeSource.Ascribed
      }

      // Construct the formal parameter.
      mapN(tpeVal) {
        case tpe => NamedAst.FormalParam(freshSym, mod, tpe, src, loc)
      }
  }

  /**
    * Translates the given weeded predicate parameter to a named predicate parameter.
    */
  private def visitPredicateParam(pparam: WeededAst.PredicateParam, tenv0: Map[String, Symbol.UnkindedTypeVarSym])(implicit flix: Flix): Validation[NamedAst.PredicateParam, NameError] = pparam match {
    case WeededAst.PredicateParam.PredicateParamUntyped(pred, loc) =>
      NamedAst.PredicateParam.PredicateParamUntyped(pred, loc).toSuccess

    case WeededAst.PredicateParam.PredicateParamWithType(pred, den, tpes, loc) =>
      mapN(traverse(tpes)(visitType(_, allowWild = false, tenv0))) {
        case ts => NamedAst.PredicateParam.PredicateParamWithType(pred, den, ts, loc)
      }
  }

  /**
    * Translates the given weeded JvmMethod to a named JvmMethod.
    */
  private def visitJvmMethod(method: WeededAst.JvmMethod, env: Map[String, Symbol.VarSym], tenv: Map[String, Symbol.UnkindedTypeVarSym], ns0: Name.NName)(implicit flix: Flix): Validation[NamedAst.JvmMethod, NameError] = method match {
    case WeededAst.JvmMethod(ident, fparams0, exp0, tpe0, purAndEff0, loc) =>
      flatMapN(traverse(fparams0)(visitFormalParam(_, tenv))) {
        case fparams =>
          val exp = visitExp(exp0, env ++ getVarEnv(fparams), tenv, ns0)
          val tpe = visitType(tpe0, allowWild = false, tenv)
          val purAndEff = visitPurityAndEffect(purAndEff0, allowWild = false, tenv)
          mapN(exp, tpe, purAndEff) {
            case (e, t, p) => NamedAst.JvmMethod(ident, fparams, e, t, p, loc)
          }
      }
  }

  /**
    * Returns the given `freeVars` less the `boundVars`.
    */
  private def filterBoundVars(freeVars: List[Name.Ident], boundVars: List[Name.Ident]): List[Name.Ident] = {
    freeVars.filter(n1 => !boundVars.exists(n2 => n1.name == n2.name))
  }

  /**
    * Returns `true` if the class types present in `expected` equals those in `actual`.
    */
  private def parameterTypeMatch(expected: List[Option[Class[_]]], actual: List[Class[_]]): Boolean =
    (expected zip actual) forall {
      case (None, _) => true
      case (Some(clazz1), clazz2) => clazz1 == clazz2
    }

  /**
    * Performs naming on the given formal parameters `fparam0` under the given environments `uenv0` and `tenv0`.
    */
  private def getFormalParams(fparams0: List[WeededAst.FormalParam], tenv0: Map[String, Symbol.UnkindedTypeVarSym])(implicit flix: Flix): Validation[List[NamedAst.FormalParam], NameError] = {
    traverse(fparams0)(visitFormalParam(_, tenv0))
  }


  /**
    * Performs naming on the given type parameter.
    */
  private def getTypeParam(tparam0: WeededAst.TypeParam)(implicit flix: Flix): NamedAst.TypeParam = tparam0 match {
    case WeededAst.TypeParam.Kinded(ident, kind) =>
      NamedAst.TypeParam.Kinded(ident, mkTypeVarSym(ident), kind, ident.loc)
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
  private def getTypeParamsFromFormalParams(tparams0: WeededAst.TypeParams, fparams: List[WeededAst.FormalParam], tpe: WeededAst.Type, purAndEff: WeededAst.PurityAndEffect, tenv: Map[String, Symbol.UnkindedTypeVarSym])(implicit flix: Flix): NamedAst.TypeParams = {
    tparams0 match {
      case WeededAst.TypeParams.Elided => getImplicitTypeParamsFromFormalParams(fparams, tpe, purAndEff, tenv)
      case WeededAst.TypeParams.Unkinded(tparams0) => getExplicitTypeParams(tparams0)
      case WeededAst.TypeParams.Kinded(tparams0) => getExplicitKindedTypeParams(tparams0)

    }
  }

  /**
    * Names the explicit kinded type params.
    */
  private def getExplicitKindedTypeParams(tparams0: List[WeededAst.TypeParam.Kinded])(implicit flix: Flix): NamedAst.TypeParams.Kinded = {
    val tparams = tparams0.map {
      case WeededAst.TypeParam.Kinded(ident, kind) =>
        NamedAst.TypeParam.Kinded(ident, mkTypeVarSym(ident), kind, ident.loc)
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
  private def getImplicitTypeParamsFromTypes(types: List[WeededAst.Type])(implicit flix: Flix): NamedAst.TypeParams.Unkinded = {
    val tvars = types.flatMap(freeVars).distinct
    val tparams = tvars.map {
      ident => NamedAst.TypeParam.Unkinded(ident, mkTypeVarSym(ident), ident.loc)
    }
    NamedAst.TypeParams.Unkinded(tparams)
  }

  /**
    * Returns the implicit type parameters constructed from the given formal parameters and type.
    */
  private def getImplicitTypeParamsFromFormalParams(fparams: List[WeededAst.FormalParam], tpe: WeededAst.Type, purAndEff: WeededAst.PurityAndEffect, tenv: Map[String, Symbol.UnkindedTypeVarSym])(implicit flix: Flix): NamedAst.TypeParams = {
    // Compute the type variables that occur in the formal parameters.
    val fparamTvars = fparams.flatMap {
      case WeededAst.FormalParam(_, _, Some(tpe), _) => freeVarsInTenv(tpe, tenv)
      case WeededAst.FormalParam(_, _, None, _) => Nil
    }

    val tpeTvars = freeVarsInTenv(tpe, tenv)

    val WeededAst.PurityAndEffect(pur, eff) = purAndEff
    val purTvars = pur.toList.flatMap(freeVarsInTenv(_, tenv))
    val effTvars = eff.getOrElse(Nil).flatMap(freeVarsInTenv(_, tenv))

    val tparams = (fparamTvars ::: tpeTvars ::: purTvars ::: effTvars).distinct.map {
      ident => NamedAst.TypeParam.Unkinded(ident, mkTypeVarSym(ident), ident.loc)
    }

    NamedAst.TypeParams.Unkinded(tparams)
  }

  /**
    * Returns a variable environment constructed from the given formal parameters `fparams0`.
    */
  private def getVarEnv(fparams0: List[NamedAst.FormalParam]): Map[String, Symbol.VarSym] = {
    fparams0.foldLeft(Map.empty[String, Symbol.VarSym]) {
      case (macc, NamedAst.FormalParam(sym, _, _, _, _)) =>
        if (sym.isWild) macc else macc + (sym.text -> sym)
    }
  }

  /**
    * Returns a type environment constructed from the given type parameters `tparams0`.
    */
  private def getTypeEnv(tparams0: List[NamedAst.TypeParam]): Map[String, Symbol.UnkindedTypeVarSym] = {
    tparams0.map(p => p.name.name -> p.sym).toMap
  }

  /**
    * Gets the location of the symbol of the declaration.
    */
  private def getSymLocation(f: NamedAst.Declaration): SourceLocation = f match {
    case NamedAst.Declaration.Class(doc, ann, mod, sym, tparam, superClasses, sigs, laws, loc) => sym.loc
    case NamedAst.Declaration.Sig(sym, spec, exp) => sym.loc
    case NamedAst.Declaration.Def(sym, spec, exp) => sym.loc
    case NamedAst.Declaration.Enum(doc, ann, mod, sym, tparams, derives, cases, tpe, loc) => sym.loc
    case NamedAst.Declaration.TypeAlias(doc, mod, sym, tparams, tpe, loc) => sym.loc
    case NamedAst.Declaration.Effect(doc, ann, mod, sym, ops, loc) => sym.loc
    case NamedAst.Declaration.Op(sym, spec) => sym.loc
    case NamedAst.Declaration.Case(sym, tpe) => sym.loc
    case NamedAst.Declaration.Instance(doc, ann, mod, clazz, tpe, tconstrs, defs, ns, loc) => throw InternalCompilerException("Unexpected instance", loc)
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
    case WeededAst.UseOrImport.UseLower(qname, alias, loc) => NamedAst.UseOrImport.UseDefOrSig(qname, alias, loc)
    case WeededAst.UseOrImport.UseUpper(qname, alias, loc) => NamedAst.UseOrImport.UseTypeOrClass(qname, alias, loc)
    case WeededAst.UseOrImport.UseTag(qname, tag, alias, loc) => NamedAst.UseOrImport.UseTag(qname, tag, alias, loc)
    case WeededAst.UseOrImport.Import(name, alias, loc) => NamedAst.UseOrImport.Import(name, alias, loc)
  }

  /**
    * A structure holding the symbols and instances in the program.
    */
  case class SymbolTable(symbols: Map[List[String], Map[String, NamedAst.Declaration]], instances: Map[List[String], Map[String, List[NamedAst.Declaration.Instance]]], uses: Map[List[String], List[NamedAst.UseOrImport]])
}

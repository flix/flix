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
import ca.uwaterloo.flix.language.ast.Ast.Source
import ca.uwaterloo.flix.language.ast.WeededAst.ChoicePattern
import ca.uwaterloo.flix.language.ast.{NamedAst, _}
import ca.uwaterloo.flix.language.errors.NameError
import ca.uwaterloo.flix.util.Validation._
import ca.uwaterloo.flix.util.{InternalCompilerException, Validation}

import scala.collection.mutable

/**
  * The Namer phase introduces unique symbols for each syntactic entity in the program.
  */
object Namer extends Phase[WeededAst.Program, NamedAst.Root] {

  /**
    * Introduces unique names for each syntactic entity in the given `program`.
    * */
  def run(program: WeededAst.Program)(implicit flix: Flix): Validation[NamedAst.Root, NameError] = flix.phase("Namer") {
    // compute all the source locations
    val locations = program.roots.foldLeft(Map.empty[Source, SourceLocation]) {
      case (macc, root) => macc + (root.loc.source -> root.loc)
    }

    // make an empty program to fold over.
    val prog0 = NamedAst.Root(
      classes = Map.empty,
      instances = Map.empty,
      defsAndSigs = Map.empty,
      enums = Map.empty,
      typealiases = Map.empty,
      reachable = program.reachable,
      sources = locations
    )

    // collect all the declarations.
    val declarations = mapN(traverse(program.roots) {
      case root => mapN(mergeUseEnvs(root.uses, UseEnv.empty)) {
        case uenv0 => root.decls.map(d => (uenv0, d))
      }
    })(_.flatten)

    // fold over the top-level declarations.
    flatMapN(declarations) {
      case decls => Validation.fold(decls, prog0) {
        case (pacc, (uenv0, decl)) => visitDecl(decl, Name.RootNS, uenv0, pacc)
      }
    }
  }

  /**
    * Performs naming on the given declaration `decl0` in the given namespace `ns0` under the given (partial) program `prog0`.
    */
  private def visitDecl(decl0: WeededAst.Declaration, ns0: Name.NName, uenv0: UseEnv, prog0: NamedAst.Root)(implicit flix: Flix): Validation[NamedAst.Root, NameError] = {

    decl0 match {
      /*
       * Namespace.
       */
      case WeededAst.Declaration.Namespace(ns, uses, decls, loc) =>
        mergeUseEnvs(uses, uenv0) flatMap {
          newEnv =>
            Validation.fold(decls, prog0) {
              case (pacc, decl) =>
                val namespace = Name.NName(ns.sp1, ns0.idents ::: ns.idents, ns.sp2)
                visitDecl(decl, namespace, newEnv, pacc)
            }
        }

      case decl@WeededAst.Declaration.Class(doc, mod, ident, tparam, superClasses, sigs, laws, loc) =>
        // Check if the class already exists.
        val sigNs = Name.extendNName(ns0, ident)
        val defsAndSigs0 = prog0.defsAndSigs.getOrElse(sigNs, Map.empty)
        val classes0 = prog0.classes.getOrElse(ns0, Map.empty)
        lookupTypeOrClass(ident, ns0, prog0) match {
          case LookupResult.NotDefined =>
            // Case 1: The class does not already exist. Update it.
            visitClass(decl, uenv0, Map.empty, ns0) flatMap {
              case clazz@NamedAst.Class(_, _, _, _, _, sigs, _, _) =>
                // add each signature to the namespace
                // TODO add laws
                val defsAndSigsVal = Validation.fold(sigs, defsAndSigs0) {
                  case (defsAndSigs, sig) => defsAndSigs.get(sig.sym.name) match {
                    case Some(otherSig) =>
                      val name = sig.sym.name
                      val loc1 = sig.spec.loc
                      val loc2 = otherSig.spec.loc
                      Failure(LazyList(
                        // NB: We report an error at both source locations.
                        NameError.DuplicateDefOrSig(name, loc1, loc2),
                        NameError.DuplicateDefOrSig(name, loc2, loc1)
                      ))
                    case None => (defsAndSigs + (sig.sym.name -> sig)).toSuccess
                  }
                }
                defsAndSigsVal.map {
                  defsAndSigs =>
                    prog0.copy(
                      classes = prog0.classes + (ns0 -> (classes0 + (ident.name -> clazz))),
                      defsAndSigs = prog0.defsAndSigs + (sigNs -> defsAndSigs))
                }
            }

          // Case 2: The name is in use.
          case LookupResult.AlreadyDefined(otherLoc) => mkDuplicateNamePair(ident.name, ident.loc, otherLoc)
        }

      case decl@WeededAst.Declaration.Instance(doc, mod, clazz, tpe0, tconstrs, defs, loc) =>
        // duplication check must come after name resolution
        val instances = prog0.instances.getOrElse(ns0, Map.empty)
        visitInstance(decl, uenv0, Map.empty, ns0) map {
          instance =>
            val newInstanceList = instance :: instances.getOrElse(clazz.ident.name, Nil)
            prog0.copy(instances = prog0.instances + (ns0 -> (instances + (clazz.ident.name -> newInstanceList))))
        }

      /*
     * Definition.
     */
      case decl@WeededAst.Declaration.Def(doc, ann, mod, ident, tparams0, fparams0, exp, tpe, retTpe, eff0, tconstrs, loc) =>
        // Check if the definition already exists.
        val defsAndSigs = prog0.defsAndSigs.getOrElse(ns0, Map.empty)
        defsAndSigs.get(ident.name) match {
          case None =>
            // Case 1: The definition does not already exist. Update it.
            visitDef(decl, uenv0, Map.empty, ns0, Nil, Nil) map {
              defn => prog0.copy(defsAndSigs = prog0.defsAndSigs + (ns0 -> (defsAndSigs + (ident.name -> defn))))
            }
          case Some(defOrSig) =>
            // Case 2: Duplicate definition.
            val name = ident.name
            val loc1 = defOrSig.spec.loc
            val loc2 = ident.loc
            Failure(LazyList(
              // NB: We report an error at both source locations.
              NameError.DuplicateDefOrSig(name, loc1, loc2),
              NameError.DuplicateDefOrSig(name, loc2, loc1),
            ))
        }

      /*
     * Law.
     */
      case WeededAst.Declaration.Law(doc, ann, mod, ident, tparams0, fparams0, exp, tpe, retTpe, eff0, tconstrs, loc) => ??? // TODO

      /*
     * Enum.
     */
      case WeededAst.Declaration.Enum(doc, mod, ident, tparams0, cases, loc) =>
        val enums0 = prog0.enums.getOrElse(ns0, Map.empty)
        lookupTypeOrClass(ident, ns0, prog0) match {
          case LookupResult.NotDefined =>
            // Case 1: The enum does not exist in the namespace. Update it.
            val sym = Symbol.mkEnumSym(ns0, ident)

            // Compute the type parameters.
            val tparams = getTypeParams(tparams0, uenv0)

            val tenv = tparams.tparams.map(kv => kv.name.name -> kv.tpe).toMap
            val quantifiers = tparams.tparams.map(_.tpe).map(x => NamedAst.Type.Var(x, loc))
            val enumType = if (quantifiers.isEmpty)
              NamedAst.Type.Enum(sym, loc)
            else {
              val base = NamedAst.Type.Enum(sym, loc)
              quantifiers.foldLeft(base: NamedAst.Type) {
                case (tacc, tvar) => NamedAst.Type.Apply(tacc, tvar, loc)
              }
            }

            mapN(casesOf(cases, uenv0, tenv)) {
              case cases =>
                val enum = NamedAst.Enum(doc, mod, sym, tparams, cases, enumType, loc)
                val enums = enums0 + (ident.name -> enum)
                prog0.copy(enums = prog0.enums + (ns0 -> enums))
            }

          // Case 2: The name is in use.
          case LookupResult.AlreadyDefined(otherLoc) => mkDuplicateNamePair(ident.name, ident.loc, otherLoc)
        }

      /*
     * Type Alias.
     */
      case WeededAst.Declaration.TypeAlias(doc, mod, ident, tparams0, tpe0, loc) =>
        val typealiases0 = prog0.typealiases.getOrElse(ns0, Map.empty)
        lookupTypeOrClass(ident, ns0, prog0) match {
          case LookupResult.NotDefined =>
            // Case 1: The type alias does not exist in the namespace. Add it.
            val tparams = getTypeParamsFromFormalParams(tparams0, List.empty, tpe0, allowElision = false, uenv0, Map.empty)
            val tenv = getTypeEnv(tparams.tparams)
            mapN(visitType(tpe0, uenv0, tenv)) {
              case tpe =>
                val sym = Symbol.mkTypeAliasSym(ns0, ident)
                val typealias = NamedAst.TypeAlias(doc, mod, sym, tparams, tpe, loc)
                val typealiases = typealiases0 + (ident.name -> typealias)
                prog0.copy(typealiases = prog0.typealiases + (ns0 -> typealiases))
            }
          // Case 2: The name is in use.
          case LookupResult.AlreadyDefined(otherLoc) => mkDuplicateNamePair(ident.name, ident.loc, otherLoc)
        }

      case _: WeededAst.Declaration.Sig =>
        throw InternalCompilerException("Unexpected signature declaration.") // signatures should not be at the top level
    }
  }

  /**
    * Creates a pair of errors reporting a duplicate type declaration at each location.
    */
  private def mkDuplicateNamePair[T](name: String, loc1: SourceLocation, loc2: SourceLocation): Validation.Failure[T, NameError] = {
    Failure(LazyList(
      // NB: We report an error at both source locations.
      NameError.DuplicateTypeOrClass(name, loc1, loc2),
      NameError.DuplicateTypeOrClass(name, loc2, loc1)
    ))
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
    * Looks up the type or class in the given namespace and root.
    */
  private def lookupTypeOrClass(ident: Name.Ident, ns0: Name.NName, prog0: NamedAst.Root): NameLookupResult = {
    val classes0 = prog0.classes.getOrElse(ns0, Map.empty)
    val enums0 = prog0.enums.getOrElse(ns0, Map.empty)
    val typealiases0 = prog0.typealiases.getOrElse(ns0, Map.empty)
    (classes0.get(ident.name), enums0.get(ident.name), typealiases0.get(ident.name)) match {
      // Case 1: The name is unused.
      case (None, None, None) => LookupResult.NotDefined
      // Case 2: A class with the name already exists.
      case (Some(clazz), None, None) => LookupResult.AlreadyDefined(clazz.sym.loc)
      // Case 3: An enum with the name already exists.
      case (None, Some(enum), None) => LookupResult.AlreadyDefined(enum.sym.loc)
      // Case 4: A type alias with the name already exists.
      case (None, None, Some(typealias)) => LookupResult.AlreadyDefined(typealias.sym.loc)
      // Impossible.
      case _ => throw InternalCompilerException("Unexpected duplicate enum, type alias, or class found.")
    }
  }

  /**
    * Performs naming on the given constraint `c0` under the given environments `env0`, `uenv0`, and `tenv0`.
    */
  private def visitConstraint(c0: WeededAst.Constraint, outerEnv: Map[String, Symbol.VarSym], uenv0: UseEnv, tenv0: Map[String, UnkindedType.Var])(implicit flix: Flix): Validation[NamedAst.Constraint, NameError] = c0 match {
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
            macc + (ident.name -> Symbol.freshVarSym(ident))
          case _ => macc
        }
      }

      // Introduce a symbol for each variable that is visible in the rule scope of the constraint.
      val ruleEnv = ruleVars.foldLeft(Map.empty[String, Symbol.VarSym]) {
        case (macc, ident) => macc.get(ident.name) match {
          case None => macc + (ident.name -> Symbol.freshVarSym(ident))
          case Some(sym) => macc
        }
      }

      // Perform naming on the head and body predicates.
      mapN(visitHeadPredicate(h, outerEnv, headEnv, ruleEnv, uenv0, tenv0), traverse(bs)(b => visitBodyPredicate(b, outerEnv, headEnv, ruleEnv, uenv0, tenv0))) {
        case (head, body) =>
          val headParams = headEnv.map {
            case (_, sym) => NamedAst.ConstraintParam.HeadParam(sym, sym.tvar, sym.loc)
          }
          val ruleParam = ruleEnv.map {
            case (_, sym) => NamedAst.ConstraintParam.RuleParam(sym, sym.tvar, sym.loc)
          }
          val cparams = (headParams ++ ruleParam).toList
          NamedAst.Constraint(cparams, head, body, loc)
      }
  }

  /**
    * Performs naming on the given `cases` map.
    */
  private def casesOf(cases: Map[Name.Tag, WeededAst.Case], uenv0: UseEnv, tenv0: Map[String, UnkindedType.Var])(implicit flix: Flix): Validation[Map[Name.Tag, NamedAst.Case], NameError] = {
    val casesVal = cases map {
      case (name, WeededAst.Case(enum, tag, tpe)) =>
        mapN(visitType(tpe, uenv0, tenv0)) {
          case t => (name, NamedAst.Case(enum, tag, t))
        }
    }
    mapN(sequence(casesVal))(_.toMap)
  }

  /**
    * Performs naming on the given class `clazz`.
    */
  private def visitClass(clazz: WeededAst.Declaration.Class, uenv0: UseEnv, tenv0: Map[String, UnkindedType.Var], ns0: Name.NName)(implicit flix: Flix): Validation[NamedAst.Class, NameError] = clazz match {
    case WeededAst.Declaration.Class(doc, mod, ident, tparams0, superClasses0, signatures, laws0, loc) =>
      val sym = Symbol.mkClassSym(ns0, ident)
      val tparam = getTypeParam(tparams0)
      val tenv = tenv0 ++ getTypeEnv(List(tparam))
      val tconstr = NamedAst.TypeConstraint(Name.mkQName(ident), NamedAst.Type.Var(tparam.tpe, tparam.loc), loc)
      for {
        superClasses <- traverse(superClasses0)(visitTypeConstraint(_, uenv0, tenv, ns0))
        sigs <- traverse(signatures)(visitSig(_, uenv0, tenv, ns0, ident, sym, tparam))
        laws <- traverse(laws0)(visitDef(_, uenv0, tenv, ns0, List(tconstr), List(tparam.tpe)))
      } yield NamedAst.Class(doc, mod, sym, tparam, superClasses, sigs, laws, loc)
  }

  /**
    * Performs naming on the given instance `instance`.
    */
  private def visitInstance(instance: WeededAst.Declaration.Instance, uenv0: UseEnv, tenv0: Map[String, UnkindedType.Var], ns0: Name.NName)(implicit flix: Flix): Validation[NamedAst.Instance, NameError] = instance match {
    case WeededAst.Declaration.Instance(doc, mod, clazz, tpe0, tconstrs, defs0, loc) =>
      val tparams = getImplicitTypeParamsFromTypes(List(tpe0))
      val tenv = tenv0 ++ getTypeEnv(tparams.tparams)
      for {
        tpe <- visitType(tpe0, uenv0, tenv)
        tconstrs <- traverse(tconstrs)(visitTypeConstraint(_, uenv0, tenv, ns0))
        qualifiedClass = getClass(clazz, uenv0)
        instTconstr = NamedAst.TypeConstraint(qualifiedClass, tpe, loc)
        defs <- traverse(defs0)(visitDef(_, uenv0, tenv, ns0, List(instTconstr), tparams.tparams.map(_.tpe)))
      } yield NamedAst.Instance(doc, mod, qualifiedClass, tpe, tconstrs, defs, loc)
  }


  /**
    * Performs naming on the given type constraint `tconstr`.
    */
  private def visitTypeConstraint(tconstr: WeededAst.TypeConstraint, uenv0: UseEnv, tenv0: Map[String, UnkindedType.Var], ns0: Name.NName)(implicit flix: Flix): Validation[NamedAst.TypeConstraint, NameError] = tconstr match {
    case WeededAst.TypeConstraint(clazz0, tparam0, loc) =>
      val clazz = getClass(clazz0, uenv0)
      mapN(visitType(tparam0, uenv0, tenv0)) {
        tparam => NamedAst.TypeConstraint(clazz, tparam, loc)
      }
  }

  /**
    * Performs naming on the given signature declaration `sig` under the given environments `env0`, `uenv0`, and `tenv0`.
    */
  private def visitSig(sig: WeededAst.Declaration.Sig, uenv0: UseEnv, tenv0: Map[String, UnkindedType.Var], ns0: Name.NName, classIdent: Name.Ident, classSym: Symbol.ClassSym, classTparam: NamedAst.TypeParam)(implicit flix: Flix): Validation[NamedAst.Sig, NameError] = sig match {
    case WeededAst.Declaration.Sig(doc, ann, mod, ident, tparams0, fparams0, exp0, tpe0, retTpe0, eff0, tconstrs0, loc) =>
      val tparams = getTypeParamsFromFormalParams(tparams0, fparams0, tpe0, allowElision = true, uenv0, tenv0)
      val tenv = tenv0 ++ getTypeEnv(tparams.tparams)
      val sigTypeCheckVal = checkSigType(ident, classTparam, tpe0, loc)
      val fparamsVal = getFormalParams(fparams0, uenv0, tenv)
      val tconstrsVal = traverse(tconstrs0)(visitTypeConstraint(_, uenv0, tenv, ns0))
      flatMapN(sigTypeCheckVal, fparamsVal, tconstrsVal) {
        case (_, fparams, tconstrs) =>
          val env0 = getVarEnv(fparams)
          val annVal = traverse(ann)(visitAnnotation(_, env0, uenv0, tenv))
          val classTconstr = NamedAst.TypeConstraint(Name.mkQName(classIdent), NamedAst.Type.Var(classTparam.tpe, classTparam.loc), classSym.loc)
          val schemeVal = getDefOrSigScheme(tparams.tparams, tpe0, uenv0, tenv, classTconstr :: tconstrs, List(classTparam.tpe))
          val expVal = traverse(exp0)(visitExp(_, env0, uenv0, tenv))
          val retTpeVal = visitType(retTpe0, uenv0, tenv)
          val effVal = visitType(eff0, uenv0, tenv)
          mapN(annVal, schemeVal, retTpeVal, effVal, expVal) {
            case (as, sc, retTpe, eff, exp) =>
              val sym = Symbol.mkSigSym(classSym, ident)
              val spec = NamedAst.Spec(doc, as, mod, tparams, fparams, sc, retTpe, eff, loc)
              NamedAst.Sig(sym, spec, exp.headOption)
          }
      }
  }

  /**
    * Checks the type `tpe` of the signature to ensure that it contains the class type parameter `classTparam`.
    */
  private def checkSigType(sigIdent: Name.Ident, classTparam: NamedAst.TypeParam, tpe: WeededAst.Type, loc: SourceLocation): Validation[Unit, NameError] = {
    if (freeVars(tpe).exists(_.name == classTparam.name.name)) {
      ().toSuccess
    } else {
      NameError.IllegalSignature(sigIdent, loc).toFailure
    }
  }

  /**
    * Performs naming on the given definition declaration `decl0` under the given environments `env0`, `uenv0`, and `tenv0`, with type constraints `tconstrs`.
    */
  private def visitDef(decl0: WeededAst.Declaration.Def, uenv0: UseEnv, tenv0: Map[String, UnkindedType.Var], ns0: Name.NName, addedTconstrs: List[NamedAst.TypeConstraint], addedQuantifiers: List[UnkindedType.Var])(implicit flix: Flix): Validation[NamedAst.Def, NameError] = decl0 match {
    case WeededAst.Declaration.Def(doc, ann, mod, ident, tparams0, fparams0, exp, tpe0, retTpe0, eff0, tconstrs, loc) =>
      // TODO: we use tenv when getting the types from formal params first, before the explicit tparams have a chance to modify it
      // This means that if an explicit type variable is shadowing, the outer scope variable will be used for some parts, and inner for others
      // Resulting in a type error rather than a redundancy error (as redundancy checking happens later)
      // To fix: require explicit kind annotations (getting rid of the formal-param-first logic)
      // Or delay using the tenv until evaluating explicit tparams (could become complex)
      val tparams = getTypeParamsFromFormalParams(tparams0, fparams0, tpe0, allowElision = true, uenv0, tenv0)
      val tenv = tenv0 ++ getTypeEnv(tparams.tparams)
      flatMapN(getFormalParams(fparams0, uenv0, tenv), traverse(tconstrs)(visitTypeConstraint(_, uenv0, tenv, ns0))) {
        case (fparams, tconstrs) =>
          val env0 = getVarEnv(fparams)
          val annVal = traverse(ann)(visitAnnotation(_, env0, uenv0, tenv))
          val expVal = visitExp(exp, env0, uenv0, tenv)
          val schemeVal = getDefOrSigScheme(tparams.tparams, tpe0, uenv0, tenv, tconstrs ++ addedTconstrs, addedQuantifiers)
          val retTpeVal = visitType(retTpe0, uenv0, tenv)
          val effVal = visitType(eff0, uenv0, tenv)
          mapN(annVal, expVal, schemeVal, retTpeVal, effVal) {
            case (as, e, sc, retTpe, eff) =>
              val sym = Symbol.mkDefnSym(ns0, ident)
              val spec = NamedAst.Spec(doc, as, mod, tparams, fparams, sc, retTpe, eff, loc)
              NamedAst.Def(sym, spec, e)
          }
      }
  }

  /**
    * Performs naming on the given expression `exp0` under the given environments `env0`, `uenv0`, and `tenv0`.
    */
  private def visitExp(exp0: WeededAst.Expression, env0: Map[String, Symbol.VarSym], uenv0: UseEnv, tenv0: Map[String, UnkindedType.Var])(implicit flix: Flix): Validation[NamedAst.Expression, NameError] = exp0 match {

    case WeededAst.Expression.Wild(loc) =>
      NamedAst.Expression.Wild(loc).toSuccess

    case WeededAst.Expression.DefOrSig(qname, loc) =>
      NamedAst.Expression.DefOrSig(qname, loc).toSuccess

    case WeededAst.Expression.VarOrDefOrSig(ident, loc) =>
      // the ident name.
      val name = ident.name

      // lookup the name in the var and use environments.
      (env0.get(name), uenv0.defsAndSigs.get(name)) match {
        case (None, None) =>
          // Case 1: the name is a top-level function.
          NamedAst.Expression.DefOrSig(Name.mkQName(ident), loc).toSuccess
        case (None, Some(actualQName)) =>
          // Case 2: the name is a use def.
          NamedAst.Expression.DefOrSig(actualQName, loc).toSuccess
        case (Some(sym), None) =>
          // Case 4: the name is a variable.
          NamedAst.Expression.Var(sym, loc).toSuccess
        case (Some(sym), Some(qname)) =>
          // Case 5.1: the name is ambiguous: var-def
          NameError.AmbiguousVarOrUse(name, loc, sym.loc, qname.loc).toFailure
      }

    case WeededAst.Expression.Hole(name, loc) =>
      NamedAst.Expression.Hole(name, loc).toSuccess

    case WeededAst.Expression.Use(uses0, exp, loc) =>
      val uses = uses0.map {
        case WeededAst.Use.UseDefOrSig(qname, alias, loc) => NamedAst.Use.UseDefOrSig(qname, alias, loc)
        case WeededAst.Use.UseTypeOrClass(qname, alias, loc) => NamedAst.Use.UseTypeOrClass(qname, alias, loc)
        case WeededAst.Use.UseTag(qname, tag, alias, loc) => NamedAst.Use.UseTag(qname, tag, alias, loc)
      }

      flatMapN(mergeUseEnvs(uses0, uenv0)) {
        case uenv1 => mapN(visitExp(exp, env0, uenv1, tenv0)) {
          case e => uses.foldRight(e) {
            case (use, acc) => NamedAst.Expression.Use(use, acc, loc)
          }
        }
      }

    case WeededAst.Expression.Unit(loc) => NamedAst.Expression.Unit(loc).toSuccess

    case WeededAst.Expression.Null(loc) => NamedAst.Expression.Null(loc).toSuccess

    case WeededAst.Expression.True(loc) => NamedAst.Expression.True(loc).toSuccess

    case WeededAst.Expression.False(loc) => NamedAst.Expression.False(loc).toSuccess

    case WeededAst.Expression.Char(lit, loc) => NamedAst.Expression.Char(lit, loc).toSuccess

    case WeededAst.Expression.Float32(lit, loc) => NamedAst.Expression.Float32(lit, loc).toSuccess

    case WeededAst.Expression.Float64(lit, loc) => NamedAst.Expression.Float64(lit, loc).toSuccess

    case WeededAst.Expression.Int8(lit, loc) => NamedAst.Expression.Int8(lit, loc).toSuccess

    case WeededAst.Expression.Int16(lit, loc) => NamedAst.Expression.Int16(lit, loc).toSuccess

    case WeededAst.Expression.Int32(lit, loc) => NamedAst.Expression.Int32(lit, loc).toSuccess

    case WeededAst.Expression.Int64(lit, loc) => NamedAst.Expression.Int64(lit, loc).toSuccess

    case WeededAst.Expression.BigInt(lit, loc) => NamedAst.Expression.BigInt(lit, loc).toSuccess

    case WeededAst.Expression.Str(lit, loc) => NamedAst.Expression.Str(lit, loc).toSuccess

    case WeededAst.Expression.Default(loc) => NamedAst.Expression.Default(loc).toSuccess

    case WeededAst.Expression.Apply(exp, exps, loc) =>
      mapN(visitExp(exp, env0, uenv0, tenv0), traverse(exps)(visitExp(_, env0, uenv0, tenv0))) {
        case (e, es) => NamedAst.Expression.Apply(e, es, loc)
      }

    case WeededAst.Expression.Lambda(fparam0, exp, loc) =>
      flatMapN(visitFormalParam(fparam0, uenv0, tenv0)) {
        case p =>
          val env1 = env0 + (p.sym.text -> p.sym)
          mapN(visitExp(exp, env1, uenv0, tenv0)) {
            case e => NamedAst.Expression.Lambda(p, e, loc)
          }
      }

    case WeededAst.Expression.Unary(sop, exp, loc) =>
      visitExp(exp, env0, uenv0, tenv0) map {
        case e => NamedAst.Expression.Unary(sop, e, loc)
      }

    case WeededAst.Expression.Binary(sop, exp1, exp2, loc) =>
      mapN(visitExp(exp1, env0, uenv0, tenv0), visitExp(exp2, env0, uenv0, tenv0)) {
        case (e1, e2) => NamedAst.Expression.Binary(sop, e1, e2, loc)
      }

    case WeededAst.Expression.IfThenElse(exp1, exp2, exp3, loc) =>
      val e1 = visitExp(exp1, env0, uenv0, tenv0)
      val e2 = visitExp(exp2, env0, uenv0, tenv0)
      val e3 = visitExp(exp3, env0, uenv0, tenv0)
      mapN(e1, e2, e3) {
        NamedAst.Expression.IfThenElse(_, _, _, loc)
      }

    case WeededAst.Expression.Stm(exp1, exp2, loc) =>
      val e1 = visitExp(exp1, env0, uenv0, tenv0)
      val e2 = visitExp(exp2, env0, uenv0, tenv0)
      mapN(e1, e2) {
        NamedAst.Expression.Stm(_, _, loc)
      }

    case WeededAst.Expression.Let(ident, mod, exp1, exp2, loc) =>
      // make a fresh variable symbol for the local variable.
      val scopedness = if (mod.isScoped)
        Scopedness.Scoped
      else
        Scopedness.Unscoped

      val sym = Symbol.freshVarSym(ident, scopedness)
      mapN(visitExp(exp1, env0, uenv0, tenv0), visitExp(exp2, env0 + (ident.name -> sym), uenv0, tenv0)) {
        case (e1, e2) => NamedAst.Expression.Let(sym, mod, e1, e2, loc)
      }

    case WeededAst.Expression.LetRegion(ident, exp, loc) =>
      // make a fresh variable symbol for the local variable.
      val sym = Symbol.freshVarSym(ident)
      mapN(visitExp(exp, env0 + (ident.name -> sym), uenv0, tenv0)) {
        case e =>
          NamedAst.Expression.LetRegion(sym, e, loc)
      }

    case WeededAst.Expression.Match(exp, rules, loc) =>
      val expVal = visitExp(exp, env0, uenv0, tenv0)
      val rulesVal = traverse(rules) {
        case WeededAst.MatchRule(pat, guard, body) =>
          // extend the environment with every variable occurring in the pattern
          // and perform naming on the rule guard and body under the extended environment.
          val (p, env1) = visitPattern(pat, uenv0)
          val extendedEnv = env0 ++ env1
          mapN(visitExp(guard, extendedEnv, uenv0, tenv0), visitExp(body, extendedEnv, uenv0, tenv0)) {
            case (g, b) => NamedAst.MatchRule(p, g, b)
          }
      }
      mapN(expVal, rulesVal) {
        case (e, rs) => NamedAst.Expression.Match(e, rs, loc)
      }

    case WeededAst.Expression.Choose(star, exps, rules, loc) =>
      val expsVal = traverse(exps)(visitExp(_, env0, uenv0, tenv0))
      val rulesVal = traverse(rules) {
        case WeededAst.ChoiceRule(pat0, exp0) =>
          val env1 = pat0.foldLeft(Map.empty[String, Symbol.VarSym]) {
            case (acc, WeededAst.ChoicePattern.Wild(loc)) => acc
            case (acc, WeededAst.ChoicePattern.Absent(loc)) => acc
            case (acc, WeededAst.ChoicePattern.Present(ident, loc)) => acc + (ident.name -> Symbol.freshVarSym(ident))
          }
          val p = pat0.map {
            case WeededAst.ChoicePattern.Wild(loc) => NamedAst.ChoicePattern.Wild(loc)
            case WeededAst.ChoicePattern.Absent(loc) => NamedAst.ChoicePattern.Absent(loc)
            case WeededAst.ChoicePattern.Present(ident, loc) => NamedAst.ChoicePattern.Present(env1(ident.name), loc)
          }
          mapN(visitExp(exp0, env0 ++ env1, uenv0, tenv0)) {
            case e => NamedAst.ChoiceRule(p, e)
          }
      }
      mapN(expsVal, rulesVal) {
        case (es, rs) => NamedAst.Expression.Choose(star, es, rs, loc)
      }

    case WeededAst.Expression.Tag(enumOpt0, tag0, expOpt, loc) =>
      val (enumOpt, tag) = getDisambiguatedTag(enumOpt0, tag0, uenv0)

      expOpt match {
        case None =>
          // Case 1: The tag does not have an expression. Nothing more to be done.
          NamedAst.Expression.Tag(enumOpt, tag, None, loc).toSuccess
        case Some(exp) =>
          // Case 2: The tag has an expression. Perform naming on it.
          visitExp(exp, env0, uenv0, tenv0) map {
            case e => NamedAst.Expression.Tag(enumOpt, tag, Some(e), loc)
          }
      }

    case WeededAst.Expression.Tuple(elms, loc) =>
      traverse(elms)(e => visitExp(e, env0, uenv0, tenv0)) map {
        case es => NamedAst.Expression.Tuple(es, loc)
      }

    case WeededAst.Expression.RecordEmpty(loc) =>
      NamedAst.Expression.RecordEmpty(loc).toSuccess

    case WeededAst.Expression.RecordSelect(exp, field, loc) =>
      mapN(visitExp(exp, env0, uenv0, tenv0)) {
        case e => NamedAst.Expression.RecordSelect(e, field, loc)
      }

    case WeededAst.Expression.RecordExtend(field, value, rest, loc) =>
      mapN(visitExp(value, env0, uenv0, tenv0), visitExp(rest, env0, uenv0, tenv0)) {
        case (v, r) => NamedAst.Expression.RecordExtend(field, v, r, loc)
      }

    case WeededAst.Expression.RecordRestrict(field, rest, loc) =>
      mapN(visitExp(rest, env0, uenv0, tenv0)) {
        case r => NamedAst.Expression.RecordRestrict(field, r, loc)
      }

    case WeededAst.Expression.ArrayLit(elms, loc) =>
      traverse(elms)(e => visitExp(e, env0, uenv0, tenv0)) map {
        case es => NamedAst.Expression.ArrayLit(es, loc)
      }

    case WeededAst.Expression.ArrayNew(elm, len, loc) =>
      mapN(visitExp(elm, env0, uenv0, tenv0), visitExp(len, env0, uenv0, tenv0)) {
        case (es, ln) => NamedAst.Expression.ArrayNew(es, ln, loc)
      }

    case WeededAst.Expression.ArrayLoad(base, index, loc) =>
      mapN(visitExp(base, env0, uenv0, tenv0), visitExp(index, env0, uenv0, tenv0)) {
        case (b, i) => NamedAst.Expression.ArrayLoad(b, i, loc)
      }

    case WeededAst.Expression.ArrayStore(base, index, elm, loc) =>
      mapN(visitExp(base, env0, uenv0, tenv0), visitExp(index, env0, uenv0, tenv0), visitExp(elm, env0, uenv0, tenv0)) {
        case (b, i, e) => NamedAst.Expression.ArrayStore(b, i, e, loc)
      }

    case WeededAst.Expression.ArrayLength(base, loc) =>
      visitExp(base, env0, uenv0, tenv0) map {
        case b => NamedAst.Expression.ArrayLength(b, loc)
      }

    case WeededAst.Expression.ArraySlice(base, startIndex, endIndex, loc) =>
      mapN(visitExp(base, env0, uenv0, tenv0), visitExp(startIndex, env0, uenv0, tenv0), visitExp(endIndex, env0, uenv0, tenv0)) {
        case (b, i1, i2) => NamedAst.Expression.ArraySlice(b, i1, i2, loc)
      }

    case WeededAst.Expression.Ref(exp, loc) =>
      visitExp(exp, env0, uenv0, tenv0) map {
        case e =>
          NamedAst.Expression.Ref(e, loc)
      }

    case WeededAst.Expression.RefWithRegion(exp1, exp2, loc) =>
      mapN(visitExp(exp1, env0, uenv0, tenv0), visitExp(exp2, env0, uenv0, tenv0)) {
        case (e1, e2) =>
          NamedAst.Expression.RefWithRegion(e1, e2, loc)
      }

    case WeededAst.Expression.Deref(exp, loc) =>
      visitExp(exp, env0, uenv0, tenv0) map {
        case e =>
          NamedAst.Expression.Deref(e, loc)
      }

    case WeededAst.Expression.Assign(exp1, exp2, loc) =>
      mapN(visitExp(exp1, env0, uenv0, tenv0), visitExp(exp2, env0, uenv0, tenv0)) {
        case (e1, e2) =>
          NamedAst.Expression.Assign(e1, e2, loc)
      }

    case WeededAst.Expression.Existential(tparams0, fparam, exp, loc) =>
      val tparams = getTypeParamsFromFormalParams(tparams0, List(fparam), WeededAst.Type.Ambiguous(Name.mkQName("Bool"), loc), allowElision = true, uenv0, tenv0)
      for {
        p <- visitFormalParam(fparam, uenv0, tenv0 ++ getTypeEnv(tparams.tparams))
        e <- visitExp(exp, env0 + (p.sym.text -> p.sym), uenv0, tenv0 ++ getTypeEnv(tparams.tparams))
      } yield NamedAst.Expression.Existential(p, e, loc) // TODO: Preserve type parameters in NamedAst?

    case WeededAst.Expression.Universal(tparams0, fparam, exp, loc) =>
      val tparams = getTypeParamsFromFormalParams(tparams0, List(fparam), WeededAst.Type.Ambiguous(Name.mkQName("Bool"), loc), allowElision = true, uenv0, tenv0)
      for {
        p <- visitFormalParam(fparam, uenv0, tenv0 ++ getTypeEnv(tparams.tparams))
        e <- visitExp(exp, env0 + (p.sym.text -> p.sym), uenv0, tenv0 ++ getTypeEnv(tparams.tparams))
      } yield NamedAst.Expression.Universal(p, e, loc) // TODO: Preserve type parameters in NamedAst?

    case WeededAst.Expression.Ascribe(exp, expectedType, expectedEff, loc) =>
      val expVal = visitExp(exp, env0, uenv0, tenv0)
      val expectedTypVal = expectedType match {
        case None => (None: Option[NamedAst.Type]).toSuccess
        case Some(t) => mapN(visitType(t, uenv0, tenv0))(x => Some(x))
      }
      val expectedEffVal = expectedEff match {
        case None => (None: Option[NamedAst.Type]).toSuccess
        case Some(f) => mapN(visitType(f, uenv0, tenv0))(x => Some(x))
      }

      mapN(expVal, expectedTypVal, expectedEffVal) {
        case (e, t, f) => NamedAst.Expression.Ascribe(e, t, f, loc)
      }

    case WeededAst.Expression.Cast(exp, declaredType, declaredEff, loc) =>
      val expVal = visitExp(exp, env0, uenv0, tenv0)
      val declaredTypVal = declaredType match {
        case None => (None: Option[NamedAst.Type]).toSuccess
        case Some(t) => mapN(visitType(t, uenv0, tenv0))(x => Some(x))
      }
      val declaredEffVal = declaredEff match {
        case None => (None: Option[NamedAst.Type]).toSuccess
        case Some(f) => mapN(visitType(f, uenv0, tenv0))(x => Some(x))
      }

      mapN(expVal, declaredTypVal, declaredEffVal) {
        case (e, t, f) => NamedAst.Expression.Cast(e, t, f, loc)
      }

    case WeededAst.Expression.TryCatch(exp, rules, loc) =>
      val expVal = visitExp(exp, env0, uenv0, tenv0)
      val rulesVal = traverse(rules) {
        case WeededAst.CatchRule(ident, className, body) =>
          val sym = Symbol.freshVarSym(ident)
          val classVal = lookupClass(className, loc)
          // TODO: Currently the bound name is not available due to bug in code gen.
          // val bodyVal = namer(body, env0 + (ident.name -> sym), tenv0)
          val bodyVal = visitExp(body, env0, uenv0, tenv0)
          mapN(classVal, bodyVal) {
            case (c, b) => NamedAst.CatchRule(sym, c, b)
          }
      }

      mapN(expVal, rulesVal) {
        case (e, rs) => NamedAst.Expression.TryCatch(e, rs, loc)
      }

    case WeededAst.Expression.InvokeConstructor(className, args, sig, loc) =>
      val argsVal = traverse(args)(visitExp(_, env0, uenv0, tenv0))
      val sigVal = traverse(sig)(visitType(_, uenv0, tenv0))
      mapN(argsVal, sigVal) {
        case (as, sig) => NamedAst.Expression.InvokeConstructor(className, as, sig, loc)
      }

    case WeededAst.Expression.InvokeMethod(className, methodName, exp, args, sig, loc) =>
      val expVal = visitExp(exp, env0, uenv0, tenv0)
      val argsVal = traverse(args)(visitExp(_, env0, uenv0, tenv0))
      val sigVal = traverse(sig)(visitType(_, uenv0, tenv0))
      mapN(expVal, argsVal, sigVal) {
        case (e, as, sig) => NamedAst.Expression.InvokeMethod(className, methodName, e, as, sig, loc)
      }

    case WeededAst.Expression.InvokeStaticMethod(className, methodName, args, sig, loc) =>
      val argsVal = traverse(args)(visitExp(_, env0, uenv0, tenv0))
      val sigVal = traverse(sig)(visitType(_, uenv0, tenv0))
      mapN(argsVal, sigVal) {
        case (as, sig) => NamedAst.Expression.InvokeStaticMethod(className, methodName, as, sig, loc)
      }

    case WeededAst.Expression.GetField(className, fieldName, exp, loc) =>
      mapN(visitExp(exp, env0, uenv0, tenv0)) {
        case e => NamedAst.Expression.GetField(className, fieldName, e, loc)
      }

    case WeededAst.Expression.PutField(className, fieldName, exp1, exp2, loc) =>
      mapN(visitExp(exp1, env0, uenv0, tenv0), visitExp(exp2, env0, uenv0, tenv0)) {
        case (e1, e2) => NamedAst.Expression.PutField(className, fieldName, e1, e2, loc)
      }

    case WeededAst.Expression.GetStaticField(className, fieldName, loc) =>
      NamedAst.Expression.GetStaticField(className, fieldName, loc).toSuccess

    case WeededAst.Expression.PutStaticField(className, fieldName, exp, loc) =>
      mapN(visitExp(exp, env0, uenv0, tenv0)) {
        case e => NamedAst.Expression.PutStaticField(className, fieldName, e, loc)
      }

    case WeededAst.Expression.NewChannel(exp, tpe, loc) =>
      mapN(visitExp(exp, env0, uenv0, tenv0), visitType(tpe, uenv0, tenv0)) {
        case (e, t) => NamedAst.Expression.NewChannel(e, t, loc)
      }

    case WeededAst.Expression.GetChannel(exp, loc) =>
      visitExp(exp, env0, uenv0, tenv0) map {
        case e => NamedAst.Expression.GetChannel(e, loc)
      }

    case WeededAst.Expression.PutChannel(exp1, exp2, loc) =>
      mapN(visitExp(exp1, env0, uenv0, tenv0), visitExp(exp2, env0, uenv0, tenv0)) {
        case (e1, e2) => NamedAst.Expression.PutChannel(e1, e2, loc)
      }

    case WeededAst.Expression.SelectChannel(rules, default, loc) =>
      val rulesVal = traverse(rules) {
        case WeededAst.SelectChannelRule(ident, chan, body) =>
          // make a fresh variable symbol for the local recursive variable.
          val sym = Symbol.freshVarSym(ident)
          val env1 = env0 + (ident.name -> sym)
          mapN(visitExp(chan, env0, uenv0, tenv0), visitExp(body, env1, uenv0, tenv0)) {
            case (c, b) => NamedAst.SelectChannelRule(sym, c, b)
          }
      }

      val defaultVal = default match {
        case Some(exp) => visitExp(exp, env0, uenv0, tenv0) map {
          case e => Some(e)
        }
        case None => None.toSuccess
      }

      mapN(rulesVal, defaultVal) {
        case (rs, d) => NamedAst.Expression.SelectChannel(rs, d, loc)
      }

    case WeededAst.Expression.Spawn(exp, loc) =>
      visitExp(exp, env0, uenv0, tenv0) map {
        case e => NamedAst.Expression.Spawn(e, loc)
      }

    case WeededAst.Expression.Lazy(exp, loc) =>
      visitExp(exp, env0, uenv0, tenv0) map {
        case e => NamedAst.Expression.Lazy(e, loc)
      }

    case WeededAst.Expression.Force(exp, loc) =>
      visitExp(exp, env0, uenv0, tenv0) map {
        case e => NamedAst.Expression.Force(e, loc)
      }

    case WeededAst.Expression.FixpointConstraintSet(cs0, loc) =>
      mapN(traverse(cs0)(visitConstraint(_, env0, uenv0, tenv0))) {
        case cs =>
          NamedAst.Expression.FixpointConstraintSet(cs, loc)
      }

    case WeededAst.Expression.FixpointMerge(exp1, exp2, loc) =>
      mapN(visitExp(exp1, env0, uenv0, tenv0), visitExp(exp2, env0, uenv0, tenv0)) {
        case (e1, e2) => NamedAst.Expression.FixpointMerge(e1, e2, loc)
      }

    case WeededAst.Expression.FixpointSolve(exp, loc) =>
      visitExp(exp, env0, uenv0, tenv0) map {
        case e => NamedAst.Expression.FixpointSolve(e, loc)
      }

    case WeededAst.Expression.FixpointFilter(ident, exp, loc) =>
      mapN(visitExp(exp, env0, uenv0, tenv0)) {
        case e => NamedAst.Expression.FixpointFilter(ident, e, loc)
      }

    case WeededAst.Expression.FixpointProjectIn(exp, pred, loc) =>
      mapN(visitExp(exp, env0, uenv0, tenv0)) {
        case e => NamedAst.Expression.FixpointProjectIn(e, pred, loc)
      }

    case WeededAst.Expression.FixpointProjectOut(pred, exp1, exp2, loc) =>
      mapN(visitExp(exp1, env0, uenv0, tenv0), visitExp(exp2, env0, uenv0, tenv0)) {
        case (e1, e2) => NamedAst.Expression.FixpointProjectOut(pred, e1, e2, loc)
      }

    case WeededAst.Expression.MatchEff(exp1, exp2, exp3, loc) =>
      mapN(visitExp(exp1, env0, uenv0, tenv0), visitExp(exp2, env0, uenv0, tenv0), visitExp(exp3, env0, uenv0, tenv0)) {
        case (e1, e2, e3) => NamedAst.Expression.MatchEff(e1, e2, e3, loc)
      }

  }

  /**
    * Names the given pattern `pat0` and returns map from variable names to variable symbols.
    */
  private def visitPattern(pat0: WeededAst.Pattern, uenv0: UseEnv)(implicit flix: Flix): (NamedAst.Pattern, Map[String, Symbol.VarSym]) = {
    val m = mutable.Map.empty[String, Symbol.VarSym]

    def visit(p: WeededAst.Pattern): NamedAst.Pattern = p match {
      case WeededAst.Pattern.Wild(loc) => NamedAst.Pattern.Wild(loc)
      case WeededAst.Pattern.Var(ident, loc) =>
        // make a fresh variable symbol for the local variable.
        val sym = Symbol.freshVarSym(ident)
        m += (ident.name -> sym)
        NamedAst.Pattern.Var(sym, loc)
      case WeededAst.Pattern.Unit(loc) => NamedAst.Pattern.Unit(loc)
      case WeededAst.Pattern.True(loc) => NamedAst.Pattern.True(loc)
      case WeededAst.Pattern.False(loc) => NamedAst.Pattern.False(loc)
      case WeededAst.Pattern.Char(lit, loc) => NamedAst.Pattern.Char(lit, loc)
      case WeededAst.Pattern.Float32(lit, loc) => NamedAst.Pattern.Float32(lit, loc)
      case WeededAst.Pattern.Float64(lit, loc) => NamedAst.Pattern.Float64(lit, loc)
      case WeededAst.Pattern.Int8(lit, loc) => NamedAst.Pattern.Int8(lit, loc)
      case WeededAst.Pattern.Int16(lit, loc) => NamedAst.Pattern.Int16(lit, loc)
      case WeededAst.Pattern.Int32(lit, loc) => NamedAst.Pattern.Int32(lit, loc)
      case WeededAst.Pattern.Int64(lit, loc) => NamedAst.Pattern.Int64(lit, loc)
      case WeededAst.Pattern.BigInt(lit, loc) => NamedAst.Pattern.BigInt(lit, loc)
      case WeededAst.Pattern.Str(lit, loc) => NamedAst.Pattern.Str(lit, loc)

      case WeededAst.Pattern.Tag(enumOpt0, tag0, pat, loc) =>
        val (enumOpt, tag) = getDisambiguatedTag(enumOpt0, tag0, uenv0)
        NamedAst.Pattern.Tag(enumOpt, tag, visit(pat), loc)

      case WeededAst.Pattern.Tuple(elms, loc) => NamedAst.Pattern.Tuple(elms map visit, loc)

      case WeededAst.Pattern.Array(elms, loc) => NamedAst.Pattern.Array(elms map visit, loc)

      case WeededAst.Pattern.ArrayTailSpread(elms, ident, loc) => ident match {
        case None =>
          val sym = Symbol.freshVarSym("_", loc)
          NamedAst.Pattern.ArrayTailSpread(elms map visit, sym, loc)
        case Some(id) =>
          val sym = Symbol.freshVarSym(id)
          m += (id.name -> sym)
          NamedAst.Pattern.ArrayTailSpread(elms map visit, sym, loc)
      }
      case WeededAst.Pattern.ArrayHeadSpread(ident, elms, loc) => ident match {
        case None =>
          val sym = Symbol.freshVarSym("_", loc)
          NamedAst.Pattern.ArrayTailSpread(elms map visit, sym, loc)
        case Some(id) =>
          val sym = Symbol.freshVarSym(id)
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
  private def visitPattern(pat0: WeededAst.Pattern, env0: Map[String, Symbol.VarSym], uenv0: UseEnv)(implicit flix: Flix): NamedAst.Pattern = {
    def visit(p: WeededAst.Pattern): NamedAst.Pattern = p match {
      case WeededAst.Pattern.Wild(loc) => NamedAst.Pattern.Wild(loc)
      case WeededAst.Pattern.Var(ident, loc) =>
        val sym = env0(ident.name)
        NamedAst.Pattern.Var(sym, loc)
      case WeededAst.Pattern.Unit(loc) => NamedAst.Pattern.Unit(loc)
      case WeededAst.Pattern.True(loc) => NamedAst.Pattern.True(loc)
      case WeededAst.Pattern.False(loc) => NamedAst.Pattern.False(loc)
      case WeededAst.Pattern.Char(lit, loc) => NamedAst.Pattern.Char(lit, loc)
      case WeededAst.Pattern.Float32(lit, loc) => NamedAst.Pattern.Float32(lit, loc)
      case WeededAst.Pattern.Float64(lit, loc) => NamedAst.Pattern.Float64(lit, loc)
      case WeededAst.Pattern.Int8(lit, loc) => NamedAst.Pattern.Int8(lit, loc)
      case WeededAst.Pattern.Int16(lit, loc) => NamedAst.Pattern.Int16(lit, loc)
      case WeededAst.Pattern.Int32(lit, loc) => NamedAst.Pattern.Int32(lit, loc)
      case WeededAst.Pattern.Int64(lit, loc) => NamedAst.Pattern.Int64(lit, loc)
      case WeededAst.Pattern.BigInt(lit, loc) => NamedAst.Pattern.BigInt(lit, loc)
      case WeededAst.Pattern.Str(lit, loc) => NamedAst.Pattern.Str(lit, loc)

      case WeededAst.Pattern.Tag(enumOpt0, tag0, pat, loc) =>
        val (enumOpt, tag) = getDisambiguatedTag(enumOpt0, tag0, uenv0)
        NamedAst.Pattern.Tag(enumOpt, tag, visit(pat), loc)

      case WeededAst.Pattern.Tuple(elms, loc) => NamedAst.Pattern.Tuple(elms map visit, loc)

      case WeededAst.Pattern.Array(elms, loc) => NamedAst.Pattern.Array(elms map visit, loc)
      case WeededAst.Pattern.ArrayTailSpread(elms, ident, loc) => ident match {
        case None =>
          NamedAst.Pattern.ArrayTailSpread(elms map visit, Symbol.freshVarSym("_", loc), loc)
        case Some(value) =>
          val sym = env0(value.name)
          NamedAst.Pattern.ArrayTailSpread(elms map visit, sym, loc)
      }
      case WeededAst.Pattern.ArrayHeadSpread(ident, elms, loc) => ident match {
        case None =>
          NamedAst.Pattern.ArrayHeadSpread(Symbol.freshVarSym("_", loc), elms map visit, loc)
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
  private def visitHeadPredicate(head: WeededAst.Predicate.Head, outerEnv: Map[String, Symbol.VarSym], headEnv0: Map[String, Symbol.VarSym], ruleEnv0: Map[String, Symbol.VarSym], uenv0: UseEnv, tenv0: Map[String, UnkindedType.Var])(implicit flix: Flix): Validation[NamedAst.Predicate.Head, NameError] = head match {
    case WeededAst.Predicate.Head.Atom(pred, den, terms, loc) =>
      for {
        ts <- traverse(terms)(t => visitExp(t, outerEnv ++ headEnv0 ++ ruleEnv0, uenv0, tenv0))
      } yield NamedAst.Predicate.Head.Atom(pred, den, ts, loc)
  }

  /**
    * Names the given body predicate `body` under the given environments `env0`, `uenv0`, and `tenv0`.
    */
  private def visitBodyPredicate(body: WeededAst.Predicate.Body, outerEnv: Map[String, Symbol.VarSym], headEnv0: Map[String, Symbol.VarSym], ruleEnv0: Map[String, Symbol.VarSym], uenv0: UseEnv, tenv0: Map[String, UnkindedType.Var])(implicit flix: Flix): Validation[NamedAst.Predicate.Body, NameError] = body match {
    case WeededAst.Predicate.Body.Atom(pred, den, polarity, terms, loc) =>
      val ts = terms.map(t => visitPattern(t, outerEnv ++ ruleEnv0, uenv0))
      NamedAst.Predicate.Body.Atom(pred, den, polarity, ts, loc).toSuccess

    case WeededAst.Predicate.Body.Guard(exp, loc) =>
      for {
        e <- visitExp(exp, outerEnv ++ headEnv0 ++ ruleEnv0, uenv0, tenv0)
      } yield NamedAst.Predicate.Body.Guard(e, loc)
  }

  /**
    * Returns the identifiers that are visible in the head scope by the given body predicate `p0`.
    */
  private def visibleInHeadScope(p0: WeededAst.Predicate.Body): List[Name.Ident] = p0 match {
    case WeededAst.Predicate.Body.Atom(_, den, polarity, terms, loc) => terms.flatMap(freeVars)
    case WeededAst.Predicate.Body.Guard(exp, loc) => Nil
  }

  /**
    * Returns the identifiers that are visible in the rule scope by the given body predicate `p0`.
    */
  private def visibleInRuleScope(p0: WeededAst.Predicate.Body): List[Name.Ident] = p0 match {
    case WeededAst.Predicate.Body.Atom(_, den, polarity, terms, loc) => terms.flatMap(freeVars)
    case WeededAst.Predicate.Body.Guard(exp, loc) => Nil
  }

  /**
    * Names the given type `tpe` under the given environments `uenv0` and `tenv0`.
    */
  private def visitType(tpe0: WeededAst.Type, uenv0: UseEnv, tenv0: Map[String, UnkindedType.Var])(implicit flix: Flix): Validation[NamedAst.Type, NameError] = tpe0 match {
    case WeededAst.Type.Unit(loc) => NamedAst.Type.Unit(loc).toSuccess

    case WeededAst.Type.Var(ident, loc) =>
      //
      // Check for [[NameError.SuspiciousTypeVarName]].
      //
      if (isSuspiciousTypeVarName(ident.name)) {
        NameError.SuspiciousTypeVarName(ident.name, loc).toFailure
      } else if (ident.isWild) {
        // Wild idents will not be in the environment. Create a tvar instead.
        NamedAst.Type.Var(UnkindedType.freshVar(loc = loc), loc).toSuccess
      } else {
        tenv0.get(ident.name) match {
          case None => NameError.UndefinedTypeVar(ident.name, loc).toFailure
          case Some(tvar) => NamedAst.Type.Var(tvar, loc).toSuccess
        }
      }

    case WeededAst.Type.Ambiguous(qname, loc) =>
      if (qname.isUnqualified) {
        val name = qname.ident.name
        // Disambiguate the qname.
        (tenv0.get(name), uenv0.typesAndClasses.get(name)) match {
          case (None, None) =>
            // Case 1: the name is top-level type.
            NamedAst.Type.Ambiguous(qname, loc).toSuccess

          case (Some(tvar), None) =>
            // Case 2: the name is a type variable.
            NamedAst.Type.Var(tvar, loc).toSuccess

          case (None, Some(actualQName)) =>
            // Case 3: the name is a use.
            NamedAst.Type.Ambiguous(actualQName, loc).toSuccess

          case (Some(tvar), Some(qname)) =>
            // Case 4: the name is ambiguous.
            throw InternalCompilerException(s"Unexpected ambiguous type.")
        }
      }
      else
        NamedAst.Type.Ambiguous(qname, loc).toSuccess

    case WeededAst.Type.Tuple(elms, loc) =>
      mapN(traverse(elms)(visitType(_, uenv0, tenv0))) {
        case ts => NamedAst.Type.Tuple(ts, loc)
      }

    case WeededAst.Type.RecordEmpty(loc) =>
      NamedAst.Type.RecordEmpty(loc).toSuccess

    case WeededAst.Type.RecordExtend(field, value, rest, loc) =>
      mapN(visitType(value, uenv0, tenv0), visitType(rest, uenv0, tenv0)) {
        case (t, r) => NamedAst.Type.RecordExtend(field, t, r, loc)
      }

    case WeededAst.Type.SchemaEmpty(loc) =>
      NamedAst.Type.SchemaEmpty(loc).toSuccess

    case WeededAst.Type.SchemaExtendByAlias(qname, targs, rest, loc) =>
      // Disambiguate the qname.
      val name = if (qname.isUnqualified) {
        uenv0.typesAndClasses.getOrElse(qname.ident.name, qname)
      } else {
        qname
      }

      mapN(traverse(targs)(visitType(_, uenv0, tenv0)), visitType(rest, uenv0, tenv0)) {
        case (ts, r) => NamedAst.Type.SchemaExtendWithAlias(name, ts, r, loc)
      }

    case WeededAst.Type.SchemaExtendByTypes(ident, den, tpes, rest, loc) =>
      mapN(traverse(tpes)(visitType(_, uenv0, tenv0)), visitType(rest, uenv0, tenv0)) {
        case (ts, r) => NamedAst.Type.SchemaExtendWithTypes(ident, den, ts, r, loc)
      }

    case WeededAst.Type.Relation(tpes, loc) =>
      mapN(traverse(tpes)(visitType(_, uenv0, tenv0))) {
        case ts => NamedAst.Type.Relation(ts, loc)
      }

    case WeededAst.Type.Lattice(tpes, loc) =>
      mapN(traverse(tpes)(visitType(_, uenv0, tenv0))) {
        case ts => NamedAst.Type.Lattice(ts, loc)
      }

    case WeededAst.Type.Native(fqn, loc) =>
      NamedAst.Type.Native(fqn, loc).toSuccess

    case WeededAst.Type.Arrow(tparams, eff, tresult, loc) =>
      mapN(traverse(tparams)(visitType(_, uenv0, tenv0)), visitType(eff, uenv0, tenv0), visitType(tresult, uenv0, tenv0)) {
        case (ts, f, t) => NamedAst.Type.Arrow(ts, f, t, loc)
      }

    case WeededAst.Type.Apply(tpe1, tpe2, loc) =>
      mapN(visitType(tpe1, uenv0, tenv0), visitType(tpe2, uenv0, tenv0)) {
        case (t1, t2) => NamedAst.Type.Apply(t1, t2, loc)
      }

    case WeededAst.Type.True(loc) =>
      NamedAst.Type.True(loc).toSuccess

    case WeededAst.Type.False(loc) =>
      NamedAst.Type.False(loc).toSuccess

    case WeededAst.Type.Not(tpe, loc) =>
      mapN(visitType(tpe, uenv0, tenv0)) {
        case t => NamedAst.Type.Not(t, loc)
      }

    case WeededAst.Type.And(tpe1, tpe2, loc) =>
      mapN(visitType(tpe1, uenv0, tenv0), visitType(tpe2, uenv0, tenv0)) {
        case (t1, t2) => NamedAst.Type.And(t1, t2, loc)
      }

    case WeededAst.Type.Or(tpe1, tpe2, loc) =>
      mapN(visitType(tpe1, uenv0, tenv0), visitType(tpe2, uenv0, tenv0)) {
        case (t1, t2) => NamedAst.Type.Or(t1, t2, loc)
      }

    case WeededAst.Type.Ascribe(tpe, kind, loc) =>
      mapN(visitType(tpe, uenv0, tenv0)) {
        t => NamedAst.Type.Ascribe(t, kind, loc)
      }
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
    case "int" => true
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
    * Returns all the free variables in the given expression `exp0`.
    */
  private def freeVars(exp0: WeededAst.Expression): List[Name.Ident] = exp0 match {
    case WeededAst.Expression.Wild(loc) => Nil
    case WeededAst.Expression.VarOrDefOrSig(ident, loc) => List(ident)
    case WeededAst.Expression.DefOrSig(_, _) => Nil
    case WeededAst.Expression.Hole(name, loc) => Nil
    case WeededAst.Expression.Use(_, exp, _) => freeVars(exp)
    case WeededAst.Expression.Unit(loc) => Nil
    case WeededAst.Expression.Null(loc) => Nil
    case WeededAst.Expression.True(loc) => Nil
    case WeededAst.Expression.False(loc) => Nil
    case WeededAst.Expression.Char(lit, loc) => Nil
    case WeededAst.Expression.Float32(lit, loc) => Nil
    case WeededAst.Expression.Float64(lit, loc) => Nil
    case WeededAst.Expression.Int8(lit, loc) => Nil
    case WeededAst.Expression.Int16(lit, loc) => Nil
    case WeededAst.Expression.Int32(lit, loc) => Nil
    case WeededAst.Expression.Int64(lit, loc) => Nil
    case WeededAst.Expression.BigInt(lit, loc) => Nil
    case WeededAst.Expression.Str(lit, loc) => Nil
    case WeededAst.Expression.Default(loc) => Nil
    case WeededAst.Expression.Apply(exp, exps, loc) => freeVars(exp) ++ exps.flatMap(freeVars)
    case WeededAst.Expression.Lambda(fparam, exp, loc) => filterBoundVars(freeVars(exp), List(fparam.ident))
    case WeededAst.Expression.Unary(sop, exp, loc) => freeVars(exp)
    case WeededAst.Expression.Binary(op, exp1, exp2, loc) => freeVars(exp1) ++ freeVars(exp2)
    case WeededAst.Expression.IfThenElse(exp1, exp2, exp3, loc) => freeVars(exp1) ++ freeVars(exp2) ++ freeVars(exp3)
    case WeededAst.Expression.Stm(exp1, exp2, loc) => freeVars(exp1) ++ freeVars(exp2)
    case WeededAst.Expression.Let(ident, mod, exp1, exp2, loc) => freeVars(exp1) ++ filterBoundVars(freeVars(exp2), List(ident))
    case WeededAst.Expression.LetRegion(ident, exp, loc) => filterBoundVars(freeVars(exp), List(ident))
    case WeededAst.Expression.Match(exp, rules, loc) => freeVars(exp) ++ rules.flatMap {
      case WeededAst.MatchRule(pat, guard, body) => filterBoundVars(freeVars(guard) ++ freeVars(body), freeVars(pat))
    }
    case WeededAst.Expression.Choose(_, exps, rules, loc) => exps.flatMap(freeVars) ++ rules.flatMap {
      case WeededAst.ChoiceRule(pat, exp) => filterBoundVars(freeVars(exp), pat.flatMap(freeVars))
    }
    case WeededAst.Expression.Tag(enum, tag, expOpt, loc) => expOpt.map(freeVars).getOrElse(Nil)
    case WeededAst.Expression.Tuple(elms, loc) => elms.flatMap(freeVars)
    case WeededAst.Expression.RecordEmpty(loc) => Nil
    case WeededAst.Expression.RecordSelect(exp, _, loc) => freeVars(exp)
    case WeededAst.Expression.RecordExtend(_, exp, rest, loc) => freeVars(exp) ++ freeVars(rest)
    case WeededAst.Expression.RecordRestrict(_, rest, loc) => freeVars(rest)
    case WeededAst.Expression.ArrayLit(elms, loc) => elms.flatMap(freeVars)
    case WeededAst.Expression.ArrayNew(elm, len, loc) => freeVars(elm) ++ freeVars(len)
    case WeededAst.Expression.ArrayLoad(base, index, loc) => freeVars(base) ++ freeVars(index)
    case WeededAst.Expression.ArrayStore(base, index, elm, loc) => freeVars(base) ++ freeVars(index) ++ freeVars(elm)
    case WeededAst.Expression.ArrayLength(base, loc) => freeVars(base)
    case WeededAst.Expression.ArraySlice(base, startIndex, endIndex, loc) => freeVars(base) ++ freeVars(startIndex) ++ freeVars(endIndex)
    case WeededAst.Expression.Ref(exp, loc) => freeVars(exp)
    case WeededAst.Expression.RefWithRegion(exp1, exp2, loc) => freeVars(exp1) ++ freeVars(exp2)
    case WeededAst.Expression.Deref(exp, loc) => freeVars(exp)
    case WeededAst.Expression.Assign(exp1, exp2, loc) => freeVars(exp1) ++ freeVars(exp2)
    case WeededAst.Expression.Existential(tparams, fparam, exp, loc) => filterBoundVars(freeVars(exp), List(fparam.ident))
    case WeededAst.Expression.Universal(tparams, fparam, exp, loc) => filterBoundVars(freeVars(exp), List(fparam.ident))
    case WeededAst.Expression.Ascribe(exp, tpe, eff, loc) => freeVars(exp)
    case WeededAst.Expression.Cast(exp, tpe, eff, loc) => freeVars(exp)
    case WeededAst.Expression.TryCatch(exp, rules, loc) =>
      rules.foldLeft(freeVars(exp)) {
        case (fvs, WeededAst.CatchRule(ident, className, body)) => filterBoundVars(freeVars(body), List(ident))
      }
    case WeededAst.Expression.InvokeConstructor(className, args, sig, loc) => args.flatMap(freeVars)
    case WeededAst.Expression.InvokeMethod(className, methodName, exp, args, sig, loc) => freeVars(exp) ++ args.flatMap(freeVars)
    case WeededAst.Expression.InvokeStaticMethod(className, methodName, args, sig, loc) => args.flatMap(freeVars)
    case WeededAst.Expression.GetField(className, fieldName, exp, loc) => freeVars(exp)
    case WeededAst.Expression.PutField(className, fieldName, exp1, exp2, loc) => freeVars(exp1) ++ freeVars(exp2)
    case WeededAst.Expression.GetStaticField(className, fieldName, loc) => Nil
    case WeededAst.Expression.PutStaticField(className, fieldName, exp, loc) => freeVars(exp)
    case WeededAst.Expression.NewChannel(tpe, exp, loc) => freeVars(exp) // TODO exp is a Type. is this a bug?
    case WeededAst.Expression.GetChannel(exp, loc) => freeVars(exp)
    case WeededAst.Expression.PutChannel(exp1, exp2, loc) => freeVars(exp1) ++ freeVars(exp2)
    case WeededAst.Expression.SelectChannel(rules, default, loc) =>
      val rulesFreeVars = rules.flatMap {
        case WeededAst.SelectChannelRule(ident, chan, exp) =>
          freeVars(chan) ++ filterBoundVars(freeVars(exp), List(ident))
      }
      val defaultFreeVars = default.map(freeVars).getOrElse(Nil)
      rulesFreeVars ++ defaultFreeVars
    case WeededAst.Expression.Spawn(exp, loc) => freeVars(exp)
    case WeededAst.Expression.Lazy(exp, loc) => freeVars(exp)
    case WeededAst.Expression.Force(exp, loc) => freeVars(exp)
    case WeededAst.Expression.FixpointConstraintSet(cs, loc) => cs.flatMap(freeVarsConstraint)
    case WeededAst.Expression.FixpointMerge(exp1, exp2, loc) => freeVars(exp1) ++ freeVars(exp2)
    case WeededAst.Expression.FixpointSolve(exp, loc) => freeVars(exp)
    case WeededAst.Expression.FixpointFilter(qname, exp, loc) => freeVars(exp)
    case WeededAst.Expression.FixpointProjectIn(exp, pred, loc) => freeVars(exp)
    case WeededAst.Expression.FixpointProjectOut(pred, exp1, exp2, loc) => freeVars(exp1) ++ freeVars(exp2)
    case WeededAst.Expression.MatchEff(exp1, exp2, exp3, loc) => freeVars(exp1) ++ freeVars(exp2) ++ freeVars(exp3)
  }

  /**
    * Returns all the free variables in the given pattern `pat0`.
    */
  private def freeVars(pat0: WeededAst.Pattern): List[Name.Ident] = pat0 match {
    case WeededAst.Pattern.Var(ident, loc) => List(ident)
    case WeededAst.Pattern.Wild(loc) => Nil
    case WeededAst.Pattern.Unit(loc) => Nil
    case WeededAst.Pattern.True(loc) => Nil
    case WeededAst.Pattern.False(loc) => Nil
    case WeededAst.Pattern.Char(lit, loc) => Nil
    case WeededAst.Pattern.Float32(lit, loc) => Nil
    case WeededAst.Pattern.Float64(lit, loc) => Nil
    case WeededAst.Pattern.Int8(lit, loc) => Nil
    case WeededAst.Pattern.Int16(lit, loc) => Nil
    case WeededAst.Pattern.Int32(lit, loc) => Nil
    case WeededAst.Pattern.Int64(lit, loc) => Nil
    case WeededAst.Pattern.BigInt(lit, loc) => Nil
    case WeededAst.Pattern.Str(lit, loc) => Nil
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
    case WeededAst.Type.RecordEmpty(loc) => Nil
    case WeededAst.Type.RecordExtend(l, t, r, loc) => freeVars(t) ::: freeVars(r)
    case WeededAst.Type.SchemaEmpty(loc) => Nil
    case WeededAst.Type.SchemaExtendByTypes(_, _, ts, r, loc) => ts.flatMap(freeVars) ::: freeVars(r)
    case WeededAst.Type.SchemaExtendByAlias(_, ts, r, _) => ts.flatMap(freeVars) ::: freeVars(r)
    case WeededAst.Type.Relation(ts, loc) => ts.flatMap(freeVars)
    case WeededAst.Type.Lattice(ts, loc) => ts.flatMap(freeVars)
    case WeededAst.Type.Native(fqm, loc) => Nil
    case WeededAst.Type.Arrow(tparams, eff, tresult, loc) => tparams.flatMap(freeVars) ::: freeVars(eff) ::: freeVars(tresult)
    case WeededAst.Type.Apply(tpe1, tpe2, loc) => freeVars(tpe1) ++ freeVars(tpe2)
    case WeededAst.Type.True(loc) => Nil
    case WeededAst.Type.False(loc) => Nil
    case WeededAst.Type.Not(tpe, loc) => freeVars(tpe)
    case WeededAst.Type.And(tpe1, tpe2, loc) => freeVars(tpe1) ++ freeVars(tpe2)
    case WeededAst.Type.Or(tpe1, tpe2, loc) => freeVars(tpe1) ++ freeVars(tpe2)
    case WeededAst.Type.Ascribe(tpe, _, _) => freeVars(tpe)
  }

  /**
    * Returns the free variables under the type environment `tenv`.
    */
  private def freeVarsInTenv(tpe0: WeededAst.Type, tenv: Map[String, UnkindedType.Var]): List[Name.Ident] = {
    def visit(tpe0: WeededAst.Type): List[Name.Ident] = tpe0 match {
      case WeededAst.Type.Var(ident, loc) if tenv.contains(ident.name) => Nil
      case WeededAst.Type.Var(ident, loc) => ident :: Nil
      case WeededAst.Type.Ambiguous(qname, loc) => Nil
      case WeededAst.Type.Unit(loc) => Nil
      case WeededAst.Type.Tuple(elms, loc) => elms.flatMap(visit)
      case WeededAst.Type.RecordEmpty(loc) => Nil
      case WeededAst.Type.RecordExtend(l, t, r, loc) => visit(t) ::: visit(r)
      case WeededAst.Type.SchemaEmpty(loc) => Nil
      case WeededAst.Type.SchemaExtendByTypes(_, _, ts, r, loc) => ts.flatMap(visit) ::: visit(r)
      case WeededAst.Type.SchemaExtendByAlias(_, ts, r, _) => ts.flatMap(visit) ::: visit(r)
      case WeededAst.Type.Relation(ts, loc) => ts.flatMap(visit)
      case WeededAst.Type.Lattice(ts, loc) => ts.flatMap(visit)
      case WeededAst.Type.Native(fqm, loc) => Nil
      case WeededAst.Type.Arrow(tparams, eff, tresult, loc) => tparams.flatMap(visit) ::: visit(eff) ::: visit(tresult)
      case WeededAst.Type.Apply(tpe1, tpe2, loc) => visit(tpe1) ++ visit(tpe2)
      case WeededAst.Type.True(loc) => Nil
      case WeededAst.Type.False(loc) => Nil
      case WeededAst.Type.Not(tpe, loc) => visit(tpe)
      case WeededAst.Type.And(tpe1, tpe2, loc) => visit(tpe1) ++ visit(tpe2)
      case WeededAst.Type.Or(tpe1, tpe2, loc) => visit(tpe1) ++ visit(tpe2)
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
    case WeededAst.Predicate.Body.Atom(_, den, polarity, terms, loc) => terms.flatMap(freeVars)
    case WeededAst.Predicate.Body.Guard(exp, loc) => freeVars(exp)
  }

  /**
    * Translates the given weeded annotation to a named annotation.
    */
  private def visitAnnotation(ann: WeededAst.Annotation, env0: Map[String, Symbol.VarSym], uenv0: UseEnv, tenv0: Map[String, UnkindedType.Var])(implicit flix: Flix): Validation[NamedAst.Annotation, NameError] = ann match {
    case WeededAst.Annotation(name, args, loc) =>
      mapN(traverse(args)(visitExp(_, env0, uenv0, tenv0))) {
        case as => NamedAst.Annotation(name, as, loc)
      }
  }

  /**
    * Translates the given weeded attribute to a named attribute.
    */
  private def visitAttribute(attr: WeededAst.Attribute, uenv0: UseEnv, tenv0: Map[String, UnkindedType.Var])(implicit flix: Flix): Validation[NamedAst.Attribute, NameError] = attr match {
    case WeededAst.Attribute(ident, tpe0, loc) =>
      mapN(visitType(tpe0, uenv0, tenv0)) {
        case tpe => NamedAst.Attribute(ident, tpe, loc)
      }
  }

  /**
    * Translates the given weeded formal parameter to a named formal parameter.
    */
  private def visitFormalParam(fparam: WeededAst.FormalParam, uenv0: UseEnv, tenv0: Map[String, UnkindedType.Var])(implicit flix: Flix): Validation[NamedAst.FormalParam, NameError] = fparam match {
    case WeededAst.FormalParam(ident, mod, optType, loc) =>
      // Generate a fresh variable symbol for the identifier.
      val scopedness = if (mod.isScoped)
        Scopedness.Scoped
      else
        Scopedness.Unscoped

      val freshSym = if (ident.name == "_")
        Symbol.freshVarSym("_", fparam.loc)
      else
        Symbol.freshVarSym(ident, scopedness)

      // Compute the type of the formal parameter or use the type variable of the symbol.
      val tpeVal = optType match {
        case None => NamedAst.Type.Var(freshSym.tvar, loc).toSuccess
        case Some(t) => visitType(t, uenv0, tenv0)
      }

      // Construct the formal parameter.
      mapN(tpeVal) {
        case tpe => NamedAst.FormalParam(freshSym, mod, tpe, loc)
      }
  }

  /**
    * Returns the given `freeVars` less the `boundVars`.
    */
  private def filterBoundVars(freeVars: List[Name.Ident], boundVars: List[Name.Ident]): List[Name.Ident] = {
    freeVars.filter(n1 => !boundVars.exists(n2 => n1.name == n2.name))
  }

  /**
    * Returns the class reflection object for the given `className`.
    */
  // TODO: Deprecated should be moved to resolver.
  private def lookupClass(className: String, loc: SourceLocation): Validation[Class[_], NameError] = try {
    Class.forName(className).toSuccess
  } catch {
    case ex: ClassNotFoundException => NameError.UndefinedNativeClass(className, loc).toFailure
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
  private def getFormalParams(fparams0: List[WeededAst.FormalParam], uenv0: UseEnv, tenv0: Map[String, UnkindedType.Var])(implicit flix: Flix): Validation[List[NamedAst.FormalParam], NameError] = {
    traverse(fparams0)(visitFormalParam(_, uenv0, tenv0))
  }


  /**
    * Performs naming on the given type parameter.
    */
  private def getTypeParam(tparam0: WeededAst.TypeParam)(implicit flix: Flix): NamedAst.TypeParam = tparam0 match {
    case WeededAst.TypeParam.Kinded(ident, kind) =>
      NamedAst.TypeParam.Kinded(ident, UnkindedType.freshVar(text = Some(ident.name), ident.loc), kind, ident.loc)
    case WeededAst.TypeParam.Unkinded(ident) =>
      NamedAst.TypeParam.Unkinded(ident, UnkindedType.freshVar(text = Some(ident.name), ident.loc), ident.loc)
  }

  /**
    * Performs naming on the given type parameters `tparam0` from the given cases `cases`.
    */
  private def getTypeParams(tparams0: WeededAst.TypeParams, uenv0: UseEnv)(implicit flix: Flix): NamedAst.TypeParams = {
    tparams0 match {
      case WeededAst.TypeParams.Elided => NamedAst.TypeParams.Kinded(Nil)
      case WeededAst.TypeParams.Unkinded(tparams) => getExplicitUnkindedTypeParams(tparams, uenv0)
      case WeededAst.TypeParams.Kinded(tparams) => getExplicitKindedTypeParams(tparams)
    }
  }


  /**
    * Performs naming on the given type parameters `tparams0` from the given formal params `fparams` and overall type `tpe`.
    */
  private def getTypeParamsFromFormalParams(tparams0: WeededAst.TypeParams, fparams: List[WeededAst.FormalParam], tpe: WeededAst.Type, allowElision: Boolean, uenv: UseEnv, tenv: Map[String, UnkindedType.Var])(implicit flix: Flix): NamedAst.TypeParams = {
    tparams0 match {
      case WeededAst.TypeParams.Elided =>
        if (allowElision)
          getImplicitTypeParamsFromFormalParams(fparams, tpe, tenv)
        else
          NamedAst.TypeParams.Kinded(Nil)
      case WeededAst.TypeParams.Unkinded(tparams0) => getExplicitUnkindedTypeParams(tparams0, uenv)
      case WeededAst.TypeParams.Kinded(tparams0) => getExplicitKindedTypeParams(tparams0)

    }
  }

  /**
    * Names the explicit kinded type params.
    */
  private def getExplicitKindedTypeParams(tparams0: List[WeededAst.TypeParam.Kinded])(implicit flix: Flix): NamedAst.TypeParams.Kinded = {
    val tparams = tparams0.map {
      case WeededAst.TypeParam.Kinded(ident, kind) =>
        val tvar = UnkindedType.freshVar(text = Some(ident.name), ident.loc)
        NamedAst.TypeParam.Kinded(ident, tvar, kind, ident.loc)
    }
    NamedAst.TypeParams.Kinded(tparams)
  }

  /**
    * Returns the explicit unkinded type parameters from the given type parameter names and implicit type parameters.
    */
  private def getExplicitUnkindedTypeParams(tparams0: List[WeededAst.TypeParam.Unkinded], uenv0: UseEnv)(implicit flix: Flix): NamedAst.TypeParams.Unkinded = {
    val tparams = tparams0.map {
      case WeededAst.TypeParam.Unkinded(ident) =>
        val tvar = UnkindedType.freshVar(text = Some(ident.name), ident.loc)
        NamedAst.TypeParam.Unkinded(ident, tvar, ident.loc)
    }
    NamedAst.TypeParams.Unkinded(tparams)
  }

  /**
    * Returns the implicit type parameters constructed from the given types.
    */
  private def getImplicitTypeParamsFromTypes(types: List[WeededAst.Type])(implicit flix: Flix): NamedAst.TypeParams.Unkinded = {
    val tvars = types.flatMap(freeVars).distinct
    val tparams = tvars.map {
      tvar => NamedAst.TypeParam.Unkinded(tvar, UnkindedType.freshVar(text = Some(tvar.name), tvar.loc), tvar.loc)
    }
    NamedAst.TypeParams.Unkinded(tparams)
  }

  /**
    * Returns the implicit type parameters constructed from the given formal parameters and type.
    */
  private def getImplicitTypeParamsFromFormalParams(fparams: List[WeededAst.FormalParam], tpe: WeededAst.Type, tenv: Map[String, UnkindedType.Var])(implicit flix: Flix): NamedAst.TypeParams = {
    // Compute the type variables that occur in the formal parameters.
    val fparamTvars = fparams.flatMap {
      case WeededAst.FormalParam(_, _, Some(tpe), _) => freeVarsInTenv(tpe, tenv)
      case WeededAst.FormalParam(_, _, None, _) => Nil
    }

    val returnTvars = freeVarsInTenv(tpe, tenv)

    val tparams = (fparamTvars ++ returnTvars).distinct.map {
      tvar => NamedAst.TypeParam.Unkinded(tvar, UnkindedType.freshVar(text = Some(tvar.name), tvar.loc), tvar.loc)
    }
    // MATT maybe make a helper for ident -> tparam

    NamedAst.TypeParams.Unkinded(tparams)
  }

  /**
    * Returns a variable environment constructed from the given formal parameters `fparams0`.
    */
  private def getVarEnv(fparams0: List[NamedAst.FormalParam]): Map[String, Symbol.VarSym] = {
    fparams0.foldLeft(Map.empty[String, Symbol.VarSym]) {
      case (macc, NamedAst.FormalParam(sym, mod, tpe, loc)) =>
        if (sym.isWild()) macc else macc + (sym.text -> sym)
    }
  }

  /**
    * Returns a type environment constructed from the given type parameters `tparams0`.
    */
  private def getTypeEnv(tparams0: List[NamedAst.TypeParam]): Map[String, UnkindedType.Var] = {
    tparams0.map(p => p.name.name -> p.tpe).toMap
  }

  /**
    * Returns the type scheme for the given type parameters `tparams0` and type `tpe` under the given environments `uenv0` and `tenv0`, with the added type constraints `tconstrs0`.
    */
  private def getDefOrSigScheme(tparams0: List[NamedAst.TypeParam], tpe: WeededAst.Type, uenv0: UseEnv, tenv0: Map[String, UnkindedType.Var], tconstrs0: List[NamedAst.TypeConstraint], addedQuantifiers: List[UnkindedType.Var])(implicit flix: Flix): Validation[NamedAst.Scheme, NameError] = {
    for {
      t <- visitType(tpe, uenv0, tenv0)
      tparams = tparams0.map(_.tpe)
    } yield NamedAst.Scheme(tparams ++ addedQuantifiers, tconstrs0, t)
  }

  /**
    * Disambiguate the given tag `tag0` with the given optional enum name `enumOpt0` under the given use environment `uenv0`.
    */
  private def getDisambiguatedTag(enumOpt0: Option[Name.QName], tag0: Name.Tag, uenv0: UseEnv): (Option[Name.QName], Name.Tag) = {
    enumOpt0 match {
      case None =>
        // Case 1: The tag is unqualified. Look it up in the use environment.
        uenv0.tags.get(tag0.name) match {
          case None =>
            // Case 1.1: The tag is unqualified and does not appear in the use environment. Leave it as is.
            (None, tag0)
          case Some((actualQName, actualTag)) =>
            // Case 1.2: The tag is unqualified and appears in the use environment. Use the actual qualified name and actual tag.
            (Some(actualQName), actualTag)
        }
      case Some(qname) =>
        // Case 2: The tag is qualified. Check if it fully-qualified.
        if (qname.isUnqualified) {
          // Case 2.1: The tag is only qualified by one name. Look it up in the use environment.
          uenv0.typesAndClasses.get(qname.ident.name) match {
            case None =>
              // Case 2.1.1: The qualified name is not in the use environment. Do not touch it.
              (Some(qname), tag0)
            case Some(actualQName) =>
              // Case 2.1.2: The qualified name is in the use environment. Use it instead.
              (Some(actualQName), tag0)
          }
        } else {
          // Case 2.2: The tag is fully-qualified. Do not touch it.
          (Some(qname), tag0)
        }
    }
  }

  /**
    * Looks up the class in the given UseEnv.
    */
  private def getClass(qname: Name.QName, uenv0: UseEnv): Name.QName = {
    if (qname.isQualified) {
      qname
    } else {
      uenv0.typesAndClasses.getOrElse(qname.ident.name, qname)
    }
  }


  /**
    * Merges the given `uses` into the given use environment `uenv0`.
    */
  private def mergeUseEnvs(uses: List[WeededAst.Use], uenv0: UseEnv): Validation[UseEnv, NameError] = {

    Validation.fold(uses, uenv0) {
      case (uenv1, WeededAst.Use.UseDefOrSig(qname, alias, _)) =>
        val name = alias.name
        uenv1.defsAndSigs.get(name) match {
          case None => uenv1.addDefOrSig(name, qname).toSuccess
          case Some(otherQName) =>
            val loc1 = otherQName.loc
            val loc2 = qname.loc
            // NB: We report an error at both source locations.
            Failure(LazyList(
              NameError.DuplicateUseDefOrSig(name, loc1, loc2),
              NameError.DuplicateUseDefOrSig(name, loc2, loc1)
            ))
        }
      case (uenv1, WeededAst.Use.UseTypeOrClass(qname, alias, _)) =>
        val name = alias.name
        uenv1.typesAndClasses.get(name) match {
          case None => uenv1.addTypeOrClass(name, qname).toSuccess
          case Some(otherQName) =>
            val loc1 = otherQName.loc
            val loc2 = qname.loc
            Failure(LazyList(
              // NB: We report an error at both source locations.
              NameError.DuplicateUseTypeOrClass(name, loc1, loc2),
              NameError.DuplicateUseTypeOrClass(name, loc2, loc1)
            ))
        }
      case (uenv1, WeededAst.Use.UseTag(qname, tag, alias, loc)) =>
        val name = alias.name
        uenv1.tags.get(name) match {
          case None => uenv1.addTag(name, qname, tag).toSuccess
          case Some((otherQName, otherTag)) =>
            val loc1 = otherTag.loc
            val loc2 = tag.loc
            Failure(LazyList(
              // NB: We report an error at both source locations.
              NameError.DuplicateUseTag(name, loc1, loc2),
              NameError.DuplicateUseTag(name, loc2, loc1)
            ))
        }
    }
  }

  /**
    * Companion object for the [[UseEnv]] class.
    */
  private object UseEnv {
    val empty: UseEnv = UseEnv(Map.empty, Map.empty, Map.empty)
  }

  /**
    * Represents an environment of "imported" names, including defs, types, and tags.
    */
  private case class UseEnv(defsAndSigs: Map[String, Name.QName], typesAndClasses: Map[String, Name.QName], tags: Map[String, (Name.QName, Name.Tag)]) {
    /**
      * Binds the def or sig name `s` to the qualified name `n`.
      */
    def addDefOrSig(s: String, n: Name.QName): UseEnv = copy(defsAndSigs = defsAndSigs + (s -> n))

    /**
      * Binds the type or class name `s` to the qualified name `n`.
      */
    def addTypeOrClass(s: String, n: Name.QName): UseEnv = copy(typesAndClasses = typesAndClasses + (s -> n))

    /**
      * Binds the tag name `s` to the qualified name `n` and tag `t`.
      */
    def addTag(s: String, n: Name.QName, t: Name.Tag): UseEnv = copy(tags = tags + (s -> (n, t)))
  }

}

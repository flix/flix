/*
 *  Copyright 2016 Magnus Madsen
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.language.ast.NamedAst.Program
import ca.uwaterloo.flix.language.ast._
import ca.uwaterloo.flix.language.errors.TypeError
import ca.uwaterloo.flix.language.errors.TypeError.UnresolvedRef
import ca.uwaterloo.flix.util.Result._
import ca.uwaterloo.flix.util.{InternalCompilerException, Result}

import scala.collection.mutable

object Disambiguation {

  /**
    * The result of a reference lookup.
    */
  sealed trait RefTarget

  object RefTarget {

    case class Defn(ns: Name.NName, defn: NamedAst.Declaration.Definition) extends RefTarget

    case class Hook(hook: Ast.Hook) extends RefTarget

  }

  /**
    * Finds the definition with the qualified name `qname` in the namespace `ns0`.
    */
  def lookupRef(qname: Name.QName, ns0: Name.NName, program: Program): Result[RefTarget, TypeError] = {
    // check whether the reference is fully-qualified.
    if (qname.isUnqualified) {
      // Case 1: Unqualified reference. Lookup both the definition and the hook.
      val defnOpt = program.definitions.getOrElse(ns0, Map.empty).get(qname.ident.name)
      val hookOpt = program.hooks.getOrElse(ns0, Map.empty).get(qname.ident.name)

      (defnOpt, hookOpt) match {
        case (Some(defn), None) => Ok(RefTarget.Defn(ns0, defn))
        case (None, Some(hook)) => Ok(RefTarget.Hook(hook))
        case (None, None) => Err(UnresolvedRef(qname, ns0, qname.loc))
        case (Some(defn), Some(hook)) => Err(TypeError.AmbiguousRef(qname, ns0, qname.loc))
      }
    } else {
      // Case 2: Qualified. Lookup both the definition and the hook.
      val defnOpt = program.definitions.getOrElse(qname.namespace, Map.empty).get(qname.ident.name)
      val hookOpt = program.hooks.getOrElse(qname.namespace, Map.empty).get(qname.ident.name)

      (defnOpt, hookOpt) match {
        case (Some(defn), None) => Ok(RefTarget.Defn(qname.namespace, defn))
        case (None, Some(hook)) => Ok(RefTarget.Hook(hook))
        case (None, None) => Err(UnresolvedRef(qname, ns0, qname.loc))
        case (Some(defn), Some(hook)) => Err(TypeError.AmbiguousRef(qname, ns0, qname.loc))
      }
    }
  }

  /**
    * Finds the enum definition matching the given qualified name and tag.
    */
  def lookupEnumByTag(qname: Option[Name.QName], tag: Name.Ident, ns: Name.NName, program: Program): Result[NamedAst.Declaration.Enum, TypeError] = {
    /*
     * Lookup the tag name in all enums across all namespaces.
     */
    val globalMatches = mutable.Set.empty[NamedAst.Declaration.Enum]
    for ((_, decls) <- program.enums) {
      for ((enumName, decl) <- decls) {
        for ((tagName, caze) <- decl.cases) {
          if (tag.name == tagName) {
            globalMatches += decl
          }
        }
      }
    }

    // Case 1: Exact match found. Simply return it.
    if (globalMatches.size == 1) {
      return Ok(globalMatches.head)
    }

    // Case 2: No or multiple matches found.
    // Lookup the tag in either the fully qualified namespace or the current namespace.
    val namespace = if (qname.exists(_.isQualified)) qname.get.namespace else ns

    /*
     * Lookup the tag name in all enums in the current namespace.
     */
    val namespaceMatches = mutable.Set.empty[NamedAst.Declaration.Enum]
    for ((enumName, decl) <- program.enums.getOrElse(namespace, Map.empty[String, NamedAst.Declaration.Enum])) {
      for ((tagName, caze) <- decl.cases) {
        if (tag.name == tagName) {
          namespaceMatches += decl
        }
      }
    }

    // Case 2.1: Exact match found in namespace. Simply return it.
    if (namespaceMatches.size == 1) {
      return Ok(namespaceMatches.head)
    }

    // Case 2.2: No matches found in namespace.
    if (namespaceMatches.isEmpty) {
      return Err(TypeError.UnresolvedTag(tag, ns, tag.loc))
    }

    // Case 2.3: Multiple matches found in namespace and no enum name.
    if (qname.isEmpty) {
      return Err(TypeError.UnresolvedTag(tag, ns, tag.loc))
    }

    // Case 2.4: Multiple matches found in namespace and an enum name is available.
    val filteredMatches = namespaceMatches.filter(_.sym.name == qname.get.ident.name)
    if (filteredMatches.size == 1) {
      return Ok(filteredMatches.head)
    }

    Err(TypeError.UnresolvedTag(tag, ns, tag.loc))
  }

  /**
    * Finds the table of the given `qname` in the namespace `ns`.
    *
    * Returns [[Err]] of [[TypeError.UnresolvedTable]] if the table does not exist.
    */
  def lookupTable(qname: Name.QName, ns: Name.NName, program: Program): Result[NamedAst.Table, TypeError] = {
    if (qname.isUnqualified) {
      // Lookup in the current namespace.
      val tables = program.tables.getOrElse(ns, Map.empty)
      tables.get(qname.ident.name) match {
        case None => Err(TypeError.UnresolvedTable(qname, ns, qname.loc))
        case Some(table) => Ok(table)
      }
    } else {
      // Lookup in the qualified namespace.
      val tables = program.tables.getOrElse(qname.namespace, Map.empty)
      tables.get(qname.ident.name) match {
        case None => Err(TypeError.UnresolvedTable(qname, qname.namespace, qname.loc))
        case Some(table) => Ok(table)
      }
    }
  }

  /**
    * Resolves the given type `tpe0` in the given namespace `ns0`.
    */
  def resolve(tpe0: NamedAst.Type, ns0: Name.NName, program: Program): Result[Type, TypeError] = tpe0 match {
    case NamedAst.Type.Var(tvar, loc) => Ok(tvar)
    case NamedAst.Type.Unit(loc) => Ok(Type.Unit)
    case NamedAst.Type.Ref(qname, loc) if qname.isUnqualified => qname.ident.name match {
      // Basic Types
      case "Unit" => Ok(Type.Unit)
      case "Bool" => Ok(Type.Bool)
      case "Char" => Ok(Type.Char)
      case "Float" => Ok(Type.Float64)
      case "Float32" => Ok(Type.Float32)
      case "Float64" => Ok(Type.Float64)
      case "Int" => Ok(Type.Int32)
      case "Int8" => Ok(Type.Int8)
      case "Int16" => Ok(Type.Int16)
      case "Int32" => Ok(Type.Int32)
      case "Int64" => Ok(Type.Int64)
      case "BigInt" => Ok(Type.BigInt)
      case "Str" => Ok(Type.Str)
      case "Native" => Ok(Type.Native)

      // Higher-Kinded Types.
      case "List" => Ok(Type.FList)
      case "Vec" => Ok(Type.FVec)
      case "Set" => Ok(Type.FSet)
      case "Map" => Ok(Type.FMap)

      // Enum Types.
      case typeName =>
        // Lookup the enum in the current namespace.
        // If the namespace doesn't even exist, just use an empty map.
        val decls = program.enums.getOrElse(ns0, Map.empty)
        decls.get(typeName) match {
          case None => Err(TypeError.UnresolvedType(qname, ns0, loc))
          case Some(enum) => enum.sc.base match {
            case t: NamedAst.Type.Enum => resolve(t, ns0, program)
            case NamedAst.Type.Apply(t: NamedAst.Type.Enum, _, _) => resolve(t, ns0, program)
            case tpe => throw InternalCompilerException(s"Unexpected type `$tpe'.")
          }
        }
    }
    case NamedAst.Type.Ref(qname, loc) if qname.isQualified =>
      // Lookup the enum using the namespace.
      val decls = program.enums.getOrElse(qname.namespace, Map.empty)
      decls.get(qname.ident.name) match {
        case None => Err(TypeError.UnresolvedType(qname, ns0, loc))
        case Some(enum) => resolve(enum.sc.base, qname.namespace, program)
      }
    case NamedAst.Type.Enum(sym, tparams, cases) =>
      val asList = cases.toList
      val tags = asList.map(_._1)
      val tpes = asList.map(_._2)
      val kind = if (tparams.isEmpty) Kind.Star else Kind.Arrow(tparams.map(_ => Kind.Star), Kind.Star)
      seqM(tpes.map(tpe => resolve(tpe, ns0, program))) map {
        case rtpes => Type.Enum(sym, (tags zip rtpes).toMap, kind)
      }
    case NamedAst.Type.Tuple(elms0, loc) =>
      for (
        elms <- seqM(elms0.map(tpe => resolve(tpe, ns0, program)))
      ) yield Type.mkFTuple(elms)
    case NamedAst.Type.Arrow(tparams0, tresult0, loc) =>
      for (
        tparams <- seqM(tparams0.map(tpe => resolve(tpe, ns0, program)));
        tresult <- resolve(tresult0, ns0, program)
      ) yield Type.mkArrow(tparams, tresult)
    case NamedAst.Type.Apply(base0, tparams0, loc) =>
      for (
        baseType <- resolve(base0, ns0, program);
        argTypes <- seqM(tparams0.map(tpe => resolve(tpe, ns0, program)))
      ) yield Type.Apply(baseType, argTypes)

  }

  /**
    * Resolves the given scheme `sc0` in the given namespace `ns0`.
    */
  def resolve(sc0: NamedAst.Scheme, ns0: Name.NName, program: Program): Result[Scheme, TypeError] = {
    resolve(sc0.base, ns0, program) map {
      case base => Scheme(sc0.quantifiers, base)
    }
  }

  /**
    * Resolves the given type `tpe0` in the given namespace `ns0`.
    */
  def resolve(tpes0: List[NamedAst.Type], ns0: Name.NName, program: Program): Result[List[Type], TypeError] = {
    seqM(tpes0.map(tpe => resolve(tpe, ns0, program)))
  }

}

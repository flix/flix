/*
 * Copyright 2021 Magnus Madsen
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

import ca.uwaterloo.flix.api.{Flix, Version}
import ca.uwaterloo.flix.language.ast.Ast.{Modifier, TypeConstraint}
import ca.uwaterloo.flix.language.ast.TypedAst._
import ca.uwaterloo.flix.language.ast.{Ast, Kind, SourceLocation, Symbol, Type, TypeConstructor, TypedAst}
import ca.uwaterloo.flix.language.fmt.FormatType
import org.json4s.JsonAST._
import org.json4s.JsonDSL._
import org.json4s.native.JsonMethods

import java.io.IOException
import java.nio.file.{Files, Path, Paths}

/**
  * A phase that emits a JSON file for library documentation.
  */
object Documentor {

  /**
    * The "Pseudo-name" of the root namespace.
    */
  val RootNS: String = "Prelude"

  /**
    * The directory where to write the ouput.
    */
  val OutputDirectory: Path = Paths.get("./build/api")

  def run(root: TypedAst.Root)(implicit flix: Flix): TypedAst.Root = flix.phase("Documentor") {
    //
    // Determine whether to generate documentation.
    //
    if (!flix.options.documentor) {
      return root
    }

    //
    // Classes.
    //
    val classesByNS = root.classes.values.groupBy(getNameSpace).flatMap {
      case (ns, decls) =>
        def isInternal(clazz: TypedAst.Class): Boolean = clazz.ann.isInternal

        val filtered = decls.filter(clazz => clazz.mod.isPublic && !isInternal(clazz)).toList
        val sorted = filtered.sortBy(_.sym.name)
        if (sorted.isEmpty)
          None
        else
          Some(ns -> JArray(sorted.map(visitClass(_)(root, flix))))
    }

    //
    // Instances (for use in Enum documentation)
    //
    val instancesByEnum = root.instances.values.flatten.groupBy(getEnum).collect {
      case (Some(enum), insts) => (enum, insts.toList)
    }

    //
    // Enums.
    //
    val enumsByNS = root.enums.values.groupBy(getNameSpace).flatMap {
      case (ns, decls) =>
        def isInternal(enum0: TypedAst.Enum): Boolean = enum0.ann.isInternal

        val filtered = decls.filter(enum => enum.mod.isPublic && !isInternal(enum)).toList
        val sorted = filtered.sortBy(_.sym.name)
        if (sorted.isEmpty)
          None
        else
          Some(ns -> JArray(sorted.map(visitEnum(_, instancesByEnum))))
    }

    //
    // Instances (for use in RestrictableEnum documentation)
    //
    val instancesByRestrictableEnum = root.instances.values.flatten.groupBy(getRestrictableEnum).collect {
      case (Some(enum), insts) => (enum, insts.toList)
    }

    //
    // Restrictable Enums.
    //
    val restrictableEnumsByNS = root.restrictableEnums.values.groupBy(getNameSpace).flatMap {
      case (ns, decls) =>
        def isInternal(enum0: TypedAst.RestrictableEnum): Boolean = enum0.ann.isInternal

        val filtered = decls.filter(enum => enum.mod.isPublic && !isInternal(enum)).toList
        val sorted = filtered.sortBy(_.sym.name)
        if (sorted.isEmpty)
          None
        else
          Some(ns -> JArray(sorted.map(visitRestrictableEnum(_, instancesByRestrictableEnum))))
    }

    //
    // Type Aliases.
    //
    val typeAliasesByNS = root.typeAliases.values.groupBy(getNameSpace).flatMap {
      case (ns, decls) =>
        val filtered = decls.filter(_.mod.isPublic).toList
        val sorted = filtered.sortBy(_.sym.name)
        if (sorted.isEmpty)
          None
        else
          Some(ns -> JArray(sorted.map(visitTypeAlias)))
    }

    //
    // Defs.
    //
    val defsByNS = root.defs.values.groupBy(getNameSpace).flatMap {
      case (ns, decls) =>
        def isPublic(decl: TypedAst.Def): Boolean =
          decl.spec.mod.isPublic

        def isInternal(decl: TypedAst.Def): Boolean = decl.spec.ann.isInternal

        val filtered = decls.filter(decl => isPublic(decl) && !isInternal(decl)).toList
        val sorted = filtered.sortBy(_.sym.name)
        if (sorted.isEmpty)
          None
        else
          Some(ns -> JArray(sorted.map(visitDef)))
    }

    //
    // Compute all namespaces.
    //
    val namespaces = classesByNS.keySet ++ enumsByNS.keySet ++ typeAliasesByNS.keySet ++ defsByNS.keySet
    val namespacesSorted = RootNS :: (namespaces - RootNS).toList.sorted

    // Construct the JSON object.
    val json =
      ("version" -> Version.CurrentVersion.toString) ~
        ("namespaces" -> namespacesSorted) ~
        ("classes" -> classesByNS) ~
        ("enums" -> enumsByNS) ~
        ("restrictableEnums" -> restrictableEnumsByNS) ~
        ("typeAliases" -> typeAliasesByNS) ~
        ("defs" -> defsByNS)

    // Serialize the JSON object to a string.
    val s = JsonMethods.pretty(JsonMethods.render(json))

    // The path to the file to write.
    val p = OutputDirectory.resolve("api.json")

    // Write the string to the path.
    writeString(s, p)

    root
  }

  /**
    * Returns the enum that is in the head position of the instance's type, if it exists.
    * Returns `None` if the type is not an `enum` type.
    */
  def getEnum(inst: TypedAst.Instance): Option[Symbol.EnumSym] = inst.tpe.baseType match {
    case Type.Cst(TypeConstructor.Enum(sym, _), _) => Some(sym)
    case _ => None
  }

  /**
    * Returns the restrictable enum that is in the head position of the instance's type, if it exists.
    * Returns `None` if the type is not an `enum` type.
    */
  def getRestrictableEnum(inst: TypedAst.Instance): Option[Symbol.RestrictableEnumSym] = inst.tpe.baseType match {
    case Type.Cst(TypeConstructor.RestrictableEnum(sym, _), _) => Some(sym)
    case _ => None
  }


  /**
    * Returns the namespace of the given class `decl`.
    */
  private def getNameSpace(decl: TypedAst.Class): String = {
    val namespace = decl.sym.namespace
    if (namespace == Nil)
      RootNS
    else
      namespace.mkString(".")
  }

  /**
    * Returns the namespace of the given enum `decl`.
    */
  private def getNameSpace(decl: TypedAst.Enum): String =
    if (decl.sym.namespace == Nil)
      RootNS
    else
      decl.sym.namespace.mkString(".")

  /**
    * Returns the namespace of the given enum `decl`.
    */
  private def getNameSpace(decl: TypedAst.RestrictableEnum): String =
    if (decl.sym.namespace == Nil)
      RootNS
    else
      decl.sym.namespace.mkString(".")

  /**
    * Returns the namespace of the given definition `decl`.
    */
  private def getNameSpace(decl: TypedAst.Def): String =
    if (decl.sym.namespace == Nil)
      RootNS
    else
      decl.sym.namespace.mkString(".")

  /**
    * Returns the namespace of the given type alias `decl`.
    */
  private def getNameSpace(decl: TypedAst.TypeAlias): String =
    if (decl.sym.namespace == Nil)
      RootNS
    else
      decl.sym.namespace.mkString(".")

  /**
    * Returns the given definition `defn0` as a JSON object.
    */
  private def visitDef(defn0: Def)(implicit flix: Flix): JObject = {
    ("sym" -> visitDefnSym(defn0.sym)) ~
      ("ann" -> visitAnnotations(defn0.spec.ann)) ~
      ("doc" -> visitDoc(defn0.spec.doc)) ~
      ("name" -> defn0.sym.name) ~
      ("tparams" -> defn0.spec.tparams.map(visitTypeParam)) ~
      ("fparams" -> defn0.spec.fparams.map(visitFormalParam)) ~
      ("tpe" -> FormatType.formatType(defn0.spec.retTpe)) ~
      ("eff" -> FormatType.formatType(defn0.spec.pur)) ~ // TODO change JSON name to `pur`
      ("tcs" -> defn0.spec.declaredScheme.tconstrs.map(visitTypeConstraint)) ~
      ("loc" -> visitSourceLocation(defn0.spec.loc))
  }

  /**
    * Returns the given instance `inst` as a JSON value.
    */
  private def visitInstance(sym: Symbol.ClassSym, inst: Instance)(implicit flix: Flix): JObject = inst match {
    case Instance(_, ann, _, _, tpe, tcs, _, _, _, loc) => // TODO ASSOC-TYPES visit assocs
      ("sym" -> visitClassSym(sym)) ~
        ("ann" -> visitAnnotations(ann)) ~
        ("tpe" -> visitType(tpe)) ~
        ("tcs" -> tcs.map(visitTypeConstraint)) ~
        ("loc" -> visitSourceLocation(loc))
  }

  /**
    * Returns the given type `tpe` as a JSON value.
    */
  private def visitType(tpe: Type)(implicit flix: Flix): JString = JString(FormatType.formatType(tpe))

  /**
    * Returns the given type constraint `tc` as a JSON value.
    */
  private def visitTypeConstraint(tc: TypeConstraint)(implicit flix: Flix): JObject = tc match {
    case TypeConstraint(head, tpe, _) =>
      ("sym" -> visitClassSym(head.sym)) ~ ("tpe" -> visitType(tpe))
  }

  /**
    * Returns the given class symbol `sym` as a JSON value.
    */
  private def visitClassSym(sym: Symbol.ClassSym): JObject =
    ("namespace" -> sym.namespace) ~
      ("name" -> sym.name) ~
      ("loc" -> visitSourceLocation(sym.loc))

  /**
    * Returns the given class symbol `sym` as a JSON value.
    */
  private def visitTypeAliasSym(sym: Symbol.TypeAliasSym): JObject =
    ("namespace" -> sym.namespace) ~
      ("name" -> sym.name) ~
      ("loc" -> visitSourceLocation(sym.loc))

  /**
    * Returns the given defn symbol `sym` as a JSON value.
    */
  private def visitDefnSym(sym: Symbol.DefnSym): JObject =
    ("namespace" -> sym.namespace) ~
      ("name" -> sym.text) ~
      ("loc" -> visitSourceLocation(sym.loc))

  /**
    * Returns the given enum symbol `sym` as a JSON value.
    */
  private def visitEnumSym(sym: Symbol.EnumSym): JObject =
    ("namespace" -> sym.namespace) ~
      ("name" -> sym.name) ~
      ("loc" -> visitSourceLocation(sym.loc))

  /**
    * Returns the given enum symbol `sym` as a JSON value.
    */
  private def visitRestrictableEnumSym(sym: Symbol.RestrictableEnumSym): JObject =
    ("namespace" -> sym.namespace) ~
      ("name" -> sym.name) ~
      ("loc" -> visitSourceLocation(sym.loc))

  /**
    * Returns the given sig symbol `sym` as a JSON value.
    */
  private def visitSigSym(sym: Symbol.SigSym): JObject =
    ("classSym" -> visitClassSym(sym.clazz)) ~
      ("name" -> sym.name) ~
      ("loc" -> visitSourceLocation(sym.loc))


  /**
    * Returns the given Kind `kind` as a JSON value.
    */
  def visitKind(kind: Kind): String = kind match {
    case Kind.Wild => ""
    case Kind.WildCaseSet => ""
    case Kind.Star => "Type"
    case Kind.Eff => "Bool"
    case Kind.RecordRow => "Record"
    case Kind.SchemaRow => "Schema"
    case Kind.Predicate => ""
    case Kind.CaseSet(sym) => s"CaseSet[${sym.toString}]"
    case Kind.Arrow(k1, k2) => visitKind(k1) + " -> " + visitKind(k2)
  }

  /**
    * Returns the given annotations `ann` as a JSON value.
    */
  private def visitAnnotations(ann: Ast.Annotations): JArray = {
    JArray(ann.annotations.map(_.toString))
  }

  /**
    * Returns the given Doc `doc` as a JSON value.
    */
  private def visitDoc(doc: Ast.Doc): JArray =
    JArray(doc.lines.map(JString))

  /**
    * Returns the given Modifier `mod` as a JSON value.
    */
  private def visitModifier(mod: Ast.Modifiers): JArray = JArray(mod.mod.map {
    case Modifier.Lawful => "lawful"
    case Modifier.Override => "override"
    case Modifier.Opaque => "opaque"
    case Modifier.Public => "public"
    case Modifier.Sealed => "sealed"
    case Modifier.Synthetic => "synthetic"
  })

  /**
    * Returns the given Type Alias `talias` as a JSON value.
    */
  private def visitTypeAlias(talias: TypeAlias)(implicit flix: Flix): JObject = talias match {
    case TypeAlias(doc, _, sym, tparams, tpe, loc) =>
      ("doc" -> visitDoc(doc)) ~
        ("sym" -> visitTypeAliasSym(sym)) ~
        ("tparams" -> tparams.map(visitTypeParam)) ~
        ("tpe" -> FormatType.formatType(tpe)) ~
        ("loc" -> visitSourceLocation(loc))
  }

  /**
    * Returns the given Type Parameter `tparam` as a JSON value.
    */
  private def visitTypeParam(tparam: TypeParam): JObject = tparam match {
    case TypeParam(ident, tpe, _) =>
      ("name" -> ident.name) ~ ("kind" -> visitKind(tpe.kind))
  }

  /**
    * Returns the given formal parameter `fparam` as a JSON value.
    */
  private def visitFormalParam(fparam: FormalParam)(implicit flix: Flix): JObject = fparam match {
    case FormalParam(sym, _, tpe, _, _) =>
      ("name" -> sym.text) ~ ("tpe" -> visitType(tpe))
  }

  /**
    * Returns the given Sig `sig` as a JSON value.
    */
  private def visitSig(sig: Sig)(implicit flix: Flix): JObject = sig match {
    case Sig(sym, spec, _) =>
      ("sym" -> visitSigSym(sym)) ~
        ("doc" -> visitDoc(spec.doc)) ~
        ("mod" -> visitModifier(spec.mod)) ~
        ("tparams" -> spec.tparams.map(visitTypeParam)) ~
        ("fparams" -> spec.fparams.map(visitFormalParam)) ~
        ("tpe" -> visitType(spec.retTpe)) ~
        ("eff" -> visitType(spec.pur)) ~ // TODO change JSON to `pur`
        ("tcs" -> spec.declaredScheme.tconstrs.map(visitTypeConstraint)) ~
        ("loc" -> visitSourceLocation(spec.loc))
  }

  /**
    * Returns the given Enum `enum` as a JSON value.
    */
  private def visitEnum(enum0: Enum, instances: Map[Symbol.EnumSym, List[Instance]])(implicit flix: Flix): JObject = enum0 match {
    case Enum(doc, ann, _, sym, tparams, derives, cases, _, loc) =>
      ("doc" -> visitDoc(doc)) ~
        ("ann" -> visitAnnotations(ann)) ~
        ("sym" -> visitEnumSym(sym)) ~
        ("tparams" -> tparams.map(visitTypeParam)) ~
        ("cases" -> cases.values.toList.sortBy(_.loc).map(visitCase)) ~
        ("derives" -> derives.map { d => visitClassSym(d.clazz) }) ~
        ("instances" -> instances.getOrElse(sym, Nil).map { i => visitClassSym(i.clazz.sym) }) ~
        ("loc" -> visitSourceLocation(loc))
  }

  /**
    * Returns the given RestrictableEnum `enum` as a JSON value.
    */
  private def visitRestrictableEnum(enum0: RestrictableEnum, instances: Map[Symbol.RestrictableEnumSym, List[Instance]])(implicit flix: Flix): JObject = enum0 match {
    case RestrictableEnum(doc, ann, _, sym, index, tparams, derives, cases, _, loc) =>
      ("doc" -> visitDoc(doc)) ~
        ("ann" -> visitAnnotations(ann)) ~
        ("sym" -> visitRestrictableEnumSym(sym)) ~
        ("index" -> visitTypeParam(index)) ~
        ("tparams" -> tparams.map(visitTypeParam)) ~
        ("cases" -> cases.values.toList.sortBy(_.loc).map(visitRestrictableCase)) ~
        ("derives" -> derives.map { d => visitClassSym(d.clazz) }) ~
        ("instances" -> instances.getOrElse(sym, Nil).map { i => visitClassSym(i.clazz.sym) }) ~
        ("loc" -> visitSourceLocation(loc))
  }

  /**
    * Returns the given case `caze` as a JSON value.
    */
  private def visitCase(caze: Case)(implicit flix: Flix): JObject = caze match {
    case Case(sym, _, _, _) =>
      val tpe = FormatType.formatType(caze.tpe)
      ("tag" -> sym.name) ~ ("tpe" -> tpe)
  }

  /**
    * Returns the given case `caze` as a JSON value.
    */
  private def visitRestrictableCase(caze: RestrictableCase)(implicit flix: Flix): JObject = caze match {
    case RestrictableCase(sym, _, _, _) =>
      val tpe = FormatType.formatType(caze.tpe)
      ("tag" -> sym.name) ~ ("tpe" -> tpe)
  }

  /**
    * Return the given class `clazz` as a JSON value.
    */
  private def visitClass(cla: Class)(implicit root: Root, flix: Flix): JObject = cla match {
    case Class(doc, ann, mod, sym, tparam, superClasses, _, signatures0, _, loc) => // TODO ASSOC-TYPES visit assocs
      val (sigs0, defs0) = signatures0.partition(_.impl.isEmpty)

      val sigs = sigs0.sortBy(_.sym.name).map(visitSig)
      val defs = defs0.sortBy(_.sym.name).map(visitSig)
      val instances = root.instances.getOrElse(sym, Nil).sortBy(_.loc).map(inst => visitInstance(sym, inst))

      ("sym" -> visitClassSym(sym)) ~
        ("doc" -> visitDoc(doc)) ~
        ("ann" -> visitAnnotations(ann)) ~
        ("mod" -> visitModifier(mod)) ~
        ("tparam" -> visitTypeParam(tparam)) ~
        ("superClasses" -> superClasses.map(visitTypeConstraint)) ~
        ("sigs" -> sigs) ~
        ("defs" -> defs) ~
        ("instances" -> instances) ~
        ("loc" -> visitSourceLocation(loc))
  }

  /**
    * Returns the given source location `loc` as a JSON value.
    */
  private def visitSourceLocation(loc: SourceLocation): JObject = loc match {
    case SourceLocation(_, source, _, beginLine, _, endLine, _) =>
      ("name" -> source.name) ~ ("beginLine" -> beginLine) ~ ("endLine" -> endLine)
  }

  /**
    * Writes the given string `s` to the given path `p`.
    */
  private def writeString(s: String, p: Path): Unit = try {
    Files.createDirectories(OutputDirectory)
    val writer = Files.newBufferedWriter(p)
    writer.write(s)
    writer.close()
  } catch {
    case ex: IOException => throw new RuntimeException(s"Unable to write to path '$p'.", ex)
  }

}

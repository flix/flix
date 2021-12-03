/*
 * Copyright 2019 Magnus Madsen
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
import ca.uwaterloo.flix.language.CompilationMessage
import ca.uwaterloo.flix.language.ast.Ast.TypeConstraint
import ca.uwaterloo.flix.language.ast.TypedAst._
import ca.uwaterloo.flix.language.ast.ops.TypedAstOps._
import ca.uwaterloo.flix.language.ast.{Ast, Kind, Name, Scheme, SourceLocation, Symbol, Type, TypeConstructor, TypedAst}
import ca.uwaterloo.flix.language.debug.{Audience, FormatType, PrettyExpression}
import ca.uwaterloo.flix.util.Validation
import ca.uwaterloo.flix.util.Validation._
import org.json4s.JsonAST._
import org.json4s.JsonDSL._
import org.json4s.native.JsonMethods

import java.io.IOException
import java.nio.file.{Files, Path, Paths}
import scala.collection.immutable.List

object Documentor extends Phase[TypedAst.Root, TypedAst.Root] {

  /**
    * The title of the generated API.
    */
  val ApiTitle = "Flix Standard Library"

  /**
    * The directory where to write the ouput.
    */
  val OutputDirectory: Path = Paths.get("./target/api")

  private implicit val audience: Audience = Audience.External

  /**
    * Emits a JSON file with information about the definitions of the program.
    */
  def run(root: TypedAst.Root)(implicit flix: Flix): Validation[TypedAst.Root, CompilationMessage] = flix.phase("Documentor") {
    // Check whether to generate documentation.
    if (flix.options.documentor) {

      // Compute all namespaces, remove duplicates, and sort them.
      // TODO: Compute at the end instead.
      val namespaces = root.defs.foldLeft(Set.empty[String]) {
        case (acc, (sym, _)) => acc + getNameSpace(sym)
      }.toList.sorted

      //
      // Classes
      //
      val classesByNS = root.classes.values.groupBy(decl => getNameSpace(decl.sym)).map {
        case (ns, decls) =>
          val sorted = decls.toList.sortBy(_.sym.name)
          ns -> JArray(sorted.map(visitClass))
      }

      //
      // Defs
      //
      val defsByNS = root.defs.values.groupBy(decl => getNameSpace(decl.sym)).map {
        case (ns, decls) =>
          val filtered = decls.filter(_.spec.mod.isPublic).toList
          val sorted = filtered.sortBy(_.sym.name)
          ns -> JArray(sorted.map(visitDef))
      }

      //
      // Enums
      //
      val enumsByNS = root.enums.values.groupBy(decl => getNameSpace(decl.sym)).map {
        case (ns, decls) =>
          val filtered = decls.toList.filter(_.mod.isPublic)
          val sorted = filtered.sortBy(_.sym.name)
          ns -> JArray(sorted.map(visitEnum))
      }

      //
      // Type Aliases
      //
      val typeAliasesByNS = root.typealiases.values.groupBy(getNameSpace).map {
        case (ns, decls) =>
          val filtered = decls.toList
          val sorted = filtered.sortBy(_.sym.name)
          ns -> JArray(sorted.map(visitTypeAlias))
      }

      // Construct the JSON object.
      val json = JObject(
        ("namespaces", namespaces),
        ("classes", classesByNS),
        ("enums", enumsByNS),
        ("typeAliases", typeAliasesByNS),
        ("defs", defsByNS)
      )


      // Serialize the JSON object to a string.
      val s = JsonMethods.pretty(JsonMethods.render(json))

      // The path to the file to write.
      val p = OutputDirectory.resolve("api.json")

      // Write the string to the path.
      writeString(s, p)
    }

    root.toSuccess
  }

  // TODO: DOC
  private def getNameSpace(sym: Symbol.EnumSym): String =
    if (sym.namespace == Nil)
      "Prelude"
    else
      sym.namespace.mkString(".")

  // TODO: DOC
  private def getNameSpace(sym: Symbol.ClassSym): String =
    if (sym.namespace == Nil)
      "Prelude"
    else
      sym.namespace.mkString(".")

  // TODO: DOC
  private def getNameSpace(sym: Symbol.DefnSym): String =
    if (sym.namespace == Nil)
      "Prelude"
    else
      sym.namespace.mkString(".")


  // TODO: DOC
  private def getNameSpace(decl: TypedAst.TypeAlias): String =
    if (decl.sym.namespace == Nil)
      "Prelude"
    else
      decl.sym.namespace.mkString(".")


  /**
    * Returns the given definition `defn0` as a JSON object.
    */
  private def visitDef(defn0: Def): JObject = {
    // Compute the type parameters.
    val tparams = defn0.spec.tparams.map {
      case TypeParam(ident, tpe, loc) => JObject(List(
        JField("name", JString(ident.name))
      ))
    }

    // Compute the formal parameters.
    val fparams = defn0.spec.fparams.collect {
      case FormalParam(psym, mod, tpe, loc) if tpe != Type.Unit => JObject(
        JField("name", JString(psym.text)),
        JField("type", JString("type"))
      )
    }

    // Compute return type and effect.
    val result = defn0.spec.retTpe
    val effect = defn0.spec.eff

    // Construct the JSON object.
    ("name" -> defn0.sym.name) ~
      ("tparams" -> tparams) ~
      ("fparams" -> fparams) ~
      ("result" -> FormatType.formatType(result)) ~
      ("effect" -> FormatType.formatType(effect)) ~
      ("time" -> getTime(defn0)) ~
      ("space" -> getSpace(defn0)) ~
      ("comment" -> defn0.spec.doc.text.trim)
  }

  /**
    * Optionally returns the time complexity of the given definition `defn0`.
    */
  private def getTime(defn0: Def): Option[String] = defn0.spec.ann.collectFirst {
    case Annotation(Ast.Annotation.Time(_), exp :: _, _) =>
      PrettyExpression.pretty(exp)
  }

  /**
    * Optionally returns the space complexity of the given definition `defn0`.
    */
  private def getSpace(defn0: Def): Option[String] = defn0.spec.ann.collectFirst {
    case Annotation(Ast.Annotation.Space(_), exp :: _, _) =>
      PrettyExpression.pretty(exp)
  }

  /**
    * Returns the given instance `inst` as a JSON value.
    */
  private def visitInstance(inst: Instance): JObject = inst match {
    case Instance(_, _, sym, tpe, tconstrs, _, _, _) =>
      ("sym" -> visitInstanceSym(sym)) ~
        //("sym" -> visitClassSym(sym.clazz)) ~
        ("tpe" -> visitType(tpe)) ~
        ("tconstrs" -> tconstrs.map(visitTypeConstraint)) ~
        ("loc" -> visitSourceLocation(sym.loc))
  }

  private def visitInstanceSym(sym: Symbol.InstanceSym): JObject = ("placeholder" -> "placeholder") // TODO

  /**
    * Returns the given type `tpe` as a JSON value.
    */
  private def visitType(tpe: Type): JObject = tpe match {
    case value: Type.Var =>
      val name = value.text match {
        case Some(value) => value
        case None => "placeholder"
      }
      ("tag" -> "Var") ~
        ("name" -> name) ~
        ("kind" -> visitKind(tpe.kind))
    case Type.Cst(tc, loc) =>
      val typeConst = tc match {
        case TypeConstructor.Bool => ("tag" -> "Bool")
        case TypeConstructor.Int32 => ("tag" -> "Int32")
        case _ => ("placeholder" -> "placeholder")
      }
      ("tag" -> "Cst") ~
        ("tc" -> typeConst) ~
        ("kind" -> visitKind(tpe.kind))
    case Type.Apply(tpe1, tpe2, loc) =>
      ("tag" -> "Apply") ~
        ("tpe1" -> visitType(tpe1)) ~
        ("tpe2" -> visitType(tpe2)) ~
        ("kind" -> visitKind(tpe.kind))
    case _ => ("placeholder" -> "placeholder")
  }

  /**
    * Returns the given type constraint `tc` as a JSON value.
    */
  private def visitTypeConstraint(tc: TypeConstraint): JObject = tc match {
    case TypeConstraint(sym, arg, _) =>
      ("sym" -> visitClassSym(sym)) ~
        ("arg" -> visitType(arg))
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
    * Returns the given sig symbol `sym` as a JSON value.
    */
  private def visitSigSym(sym: Symbol.SigSym): JObject =
    ("classSym" -> visitClassSym(sym.clazz)) ~
      ("name" -> sym.name) ~
      ("loc" -> visitSourceLocation(sym.loc))

  // TODO: Visit the other symbols.

  /**
    * Returns the given source location `loc` as a JSON value.
    */
  private def visitSourceLocation(loc: SourceLocation): JObject = loc match {
    case SourceLocation(_, source, _, beginLine, beginCol, endLine, endCol) =>
      ("name" -> source.name) ~
        ("beginLine" -> beginLine) ~
        ("beginCol" -> beginCol) ~
        ("endLine" -> endLine) ~
        ("endCol" -> endCol)
  }

  /**
    * Returns the given Kind `kind` as a JSON value.
    */
  def visitKind(kind: Kind): String = kind match {
    case Kind.Wild => "placeholder"
    case Kind.Star => "Star"
    case Kind.Bool => "Bool"
    case Kind.RecordRow => "Record"
    case Kind.SchemaRow => "Schema"
    case Kind.Predicate => "placeholder"
    case Kind.Arrow(k1, k2) => "placeholder"
  }

  /**
    * Returns the given Type Parameter `tparam` as a JSON value.
    */
  private def visitTypeParam(tparam: TypeParam): JObject = tparam match {
    case TypeParam(ident, tpe, loc) =>
      ("name" -> ident.name) ~
        ("kind" -> visitKind(tpe.kind))
  }

  /**
    * Returns the given Doc `doc` as a JSON value.
    */
  private def visitDoc(doc: Ast.Doc): JArray =
    JArray(doc.lines.map(JString))

  /**
    * Returns the given Modifier `mod` as a JSON value.
    */
  private def visitModifier(mod: Ast.Modifiers): String = "public"

  /**
    * Returns the given Type Alias `talias` as a JSON value.
    */
  private def visitTypeAlias(talias: TypeAlias): JObject = talias match {
    case TypeAlias(doc, _, sym, tparams, tpe, loc) =>
      ("doc" -> visitDoc(doc)) ~
        ("sym" -> visitTypeAliasSym(sym)) ~
        ("tparams" -> tparams.map(visitTypeParam)) ~
        ("tpe" -> FormatType.formatType(tpe)) ~
        ("loc" -> visitSourceLocation(loc))
  }

  /**
    *
    */
  private def visitFormalParam(f: FormalParam): JObject = f match {
    case FormalParam(sym, mod, tpe, loc) =>
      ("name" -> sym.text) ~
        ("tpe" -> visitType(tpe))
  }

  /**
    * Returns the given Sig `sig` as a JSON value.
    */
  private def visitSig(sig: Sig): JObject = sig match {
    case Sig(sym, spec, impl) =>
      // Compute the type parameters.
      val computedtparams = spec.tparams.map {
        t => visitTypeParam(t)
      }

      // Compute the formal parameters.
      val computedfparams = spec.fparams.map {
        f => visitFormalParam(f)
      }

      ("sym" -> visitSigSym(sym)) ~
        ("doc" -> visitDoc(spec.doc)) ~
        ("mod" -> visitModifier(spec.mod)) ~
        ("tparams" -> computedtparams) ~
        ("fparams" -> computedfparams) ~
        ("retTpe" -> visitType(spec.retTpe)) ~
        ("eff" -> visitType(spec.eff)) ~
        ("loc" -> visitSourceLocation(spec.loc))

    // TODO: missing implemented as last type
  }

  /**
    * Returns the given Enum `enum` as a JSON value.
    */
  private def visitEnum(enum: Enum): JObject = enum match {
    case Enum(doc, _, sym, tparams, cases, _, _, loc) =>
      val computedCases = "placeholder" // TODO: convert cases (Map) to [Case]

      ("doc" -> visitDoc(doc)) ~
        ("sym" -> visitEnumSym(sym)) ~
        ("tparams" -> tparams.map(visitTypeParam)) ~
        ("cases" -> computedCases) ~
        ("loc" -> visitSourceLocation(loc))
  }

  /**
    * Return the given Class `cla` as a JSON value.
    */
  private def visitClass(cla: Class): JObject = cla match {
    case Class(doc, mod, sym, tparam, superClasses, signatures, laws, loc) =>
      // Compute the type constraints.
      val computedTypeConstraints = superClasses.map {
        tc => visitTypeConstraint(tc)
      }

      // Compute the signatures.
      val computedSig = signatures.map {
        sig => visitSig(sig)
      }

      ("sym" -> visitClassSym(sym)) ~
        ("doc" -> visitDoc(doc)) ~
        ("mod" -> visitModifier(mod)) ~
        ("tparams" -> visitTypeParam(tparam)) ~
        ("superClasses" -> computedTypeConstraints) ~
        ("signatures" -> computedSig) ~
        ("loc" -> visitSourceLocation(loc))

    // TODO: missing instances (second last)
  }

  // TODO: visitAPI

  /**
    * Writes the given string `s` to the given path `p`.
    */
  private def writeString(s: String, p: Path): Unit = try {
    val writer = Files.newBufferedWriter(p)
    writer.write(s)
    writer.close()
  } catch {
    case ex: IOException => throw new RuntimeException(s"Unable to write to path '$p'.", ex)
  }

}

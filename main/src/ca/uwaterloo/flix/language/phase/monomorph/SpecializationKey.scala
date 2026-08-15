/*
 * Copyright 2026 Werner Stein
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
package ca.uwaterloo.flix.language.phase.monomorph

import ca.uwaterloo.flix.language.ast.{Symbol, Type, TypeConstructor}

import scala.collection.mutable

/**
  * Renders the identity of a specialization as a string.
  *
  * A specialization is determined by the definition being specialized and the type it is
  * specialized at, which is exactly the pair the specialization cache is already keyed on
  * in [[Specialization]]. Hashing that string gives a name that depends on what the
  * specialization *is* rather than on how many symbols preceded it.
  *
  * Three things are deliberately excluded, because each would reintroduce the instability
  * the key exists to remove:
  *
  *   - The definition's own id. Instance defs carry a [[ca.uwaterloo.flix.language.GenSym]]
  *     counter assigned in the namer; two instances of one trait method are told apart by
  *     the type they are specialized at, which is in the key already.
  *   - Region symbols, which carry a counter and are erased before code generation.
  *   - Error types, which carry a counter and only occur in programs that do not compile.
  *
  * The renderer does not use `Type.toString`, which formats types for humans through
  * [[ca.uwaterloo.flix.language.fmt.FormatType]]. Keying on that would mean improving an
  * error message renames every class in every Flix program. Nor does it use the derived
  * `toString` of [[TypeConstructor]], which would tie those names to Scala case class names.
  */
object SpecializationKey {

  /**
    * Returns the key identifying the specialization of `sym` at `tpe`.
    *
    * `tpe` is expected to be normalized, as everything reaching
    * [[Specialization.specializeDefCallsite]] is: alias-free, associated types reduced,
    * record and schema rows sorted, and effects in canonical form. The key is only as
    * stable as that normalization.
    */
  def of(sym: Symbol.DefnSym, tpe: Type): String = {
    val sb = new mutable.StringBuilder()
    sb.append(sym.namespace.mkString("."))
    if (sym.namespace.nonEmpty) sb.append('.')
    sb.append(sym.text)
    sb.append('|')
    render(tpe, mutable.Map.empty, sb)
    sb.toString()
  }

  /**
    * Appends the rendering of `tpe` to `sb`.
    *
    * Type variables are numbered by first appearance rather than by their symbol, so that
    * two types differing only in which variables the inference happened to allocate render
    * identically. A normalized specialization type is ground, so this is a safety net
    * rather than a common path.
    */
  private def render(tpe: Type, vars: mutable.Map[Symbol.KindedTypeVarSym, Int], sb: mutable.StringBuilder): Unit = tpe match {
    case Type.Var(sym, _) =>
      sb.append('\'').append(vars.getOrElseUpdate(sym, vars.size))

    case Type.Cst(tc, _) =>
      sb.append(constructor(tc))

    case Type.Apply(tpe1, tpe2, _) =>
      sb.append('(')
      render(tpe1, vars, sb)
      sb.append(' ')
      render(tpe2, vars, sb)
      sb.append(')')

    case Type.Alias(_, _, t, _) =>
      // Normalization removes aliases; follow it if one survives rather than naming the alias.
      render(t, vars, sb)

    case Type.AssocType(symUse, arg, _, _) =>
      sb.append("Assoc(").append(symUse.sym).append(' ')
      render(arg, vars, sb)
      sb.append(')')

    case Type.JvmToType(t, _) =>
      sb.append("JvmToType(")
      render(t, vars, sb)
      sb.append(')')

    case Type.JvmToEff(t, _) =>
      sb.append("JvmToEff(")
      render(t, vars, sb)
      sb.append(')')

    case Type.UnresolvedJvmType(member, _) =>
      sb.append("UnresolvedJvm(").append(member).append(')')
  }

  /**
    * Returns the rendering of `tc`.
    *
    * Constructors carrying a symbol name it explicitly. The kind is omitted where a symbol
    * already determines it. Everything else is a nullary constructor and is named by its
    * case, which is a fixed vocabulary rather than a formatting decision.
    */
  private def constructor(tc: TypeConstructor): String = tc match {
    case TypeConstructor.Enum(sym, _) => s"Enum($sym)"
    case TypeConstructor.Struct(sym, _) => s"Struct($sym)"
    case TypeConstructor.RestrictableEnum(sym, _) => s"RestrictableEnum($sym)"
    case TypeConstructor.Effect(sym, _) => s"Effect($sym)"

    case TypeConstructor.Arrow(arity) => s"Arrow($arity)"
    case TypeConstructor.ArrowWithoutEffect(arity) => s"ArrowWithoutEffect($arity)"
    case TypeConstructor.Tuple(arity) => s"Tuple($arity)"
    case TypeConstructor.Relation(arity) => s"Relation($arity)"
    case TypeConstructor.Lattice(arity) => s"Lattice($arity)"

    case TypeConstructor.RecordRowExtend(label) => s"RecordRowExtend(${label.name})"
    case TypeConstructor.SchemaRowExtend(pred) => s"SchemaRowExtend(${pred.name})"

    case TypeConstructor.CaseComplement(sym) => s"CaseComplement($sym)"
    case TypeConstructor.CaseUnion(sym) => s"CaseUnion($sym)"
    case TypeConstructor.CaseIntersection(sym) => s"CaseIntersection($sym)"
    case TypeConstructor.CaseSymmetricDiff(sym) => s"CaseSymmetricDiff($sym)"
    case TypeConstructor.CaseSet(syms, enumSym) => s"CaseSet($enumSym:${syms.mkString(",")})"

    case TypeConstructor.Native(clazz) => s"Native(${clazz.getName})"
    case TypeConstructor.JvmConstructor(constructor) => s"JvmConstructor($constructor)"
    case TypeConstructor.JvmMethod(method) => s"JvmMethod($method)"
    case TypeConstructor.JvmField(field) => s"JvmField($field)"

    // Regions carry a counter and are erased before code generation, so all regions are
    // one region as far as a generated name is concerned.
    case TypeConstructor.Region(_) => "Region"

    // Error types carry a counter, and a program containing one does not reach code
    // generation, so distinguishing them would only make the key unstable.
    case TypeConstructor.Error(_, _) => "Error"

    case nullary: Product => nullary.productPrefix
    case other => other.getClass.getSimpleName.stripSuffix("$")
  }

}

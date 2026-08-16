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
import ca.uwaterloo.flix.util.{InternalCompilerException, StableName}

import java.lang.reflect.{Constructor, Field, Method}
import scala.collection.mutable

/**
  * Renders the identity of a specialization as a string.
  *
  * A specialization is determined by the definition being specialized and the type it is
  * specialized at, which is exactly the pair the specialization cache is already keyed on
  * in [[Specialization]]. Hashing that string gives a name that depends on what the
  * specialization *is* rather than on how many symbols preceded it.
  *
  * Two things are deliberately excluded, because each would reintroduce the instability
  * the key exists to remove:
  *
  *   - Region symbols, which carry a counter and are erased before code generation.
  *   - Error types, which carry a counter and only occur in programs that do not compile.
  *
  * The definition's own id is *included*, and cannot be dropped: a trait's default
  * implementation and an instance's implementation share a qualified name and can be
  * specialized at the identical type, so the id is the only thing separating them. That
  * id is itself content-addressed, derived in the namer from the instance the definition
  * belongs to, so it carries no counter into the key. The rendered form is appended rather
  * than the raw value, so the key does not move if the id changes representation.
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
    *
    * An alias surviving into `tpe` is deliberately tolerated rather than rejected: see
    * `render`'s [[Type.Alias]] case and `TestSpecializationKey.followsAlias.01`, which pins
    * that behavior. Associated types and `JvmToType`/`JvmToEff` are likewise rendered
    * rather than rejected, each under its own tag.
    *
    * An unresolved JVM member is different: it is not tolerated, because there is no
    * legitimate way for one to reach here. `TypeReduction2.reduce` resolves every JVM
    * member into a concrete `TypeConstructor.Jvm*` during type checking, or the constraint
    * solver reports it as a `TypeError` (`ConstraintSolverInterface.mkTypeError`) and
    * compilation stops before `Specialization` ever runs. `render`'s [[Type.UnresolvedJvmType]]
    * case throws rather than rendering it, on that basis.
    */
  def of(sym: Symbol.DefnSym, tpe: Type): String = {
    val sb = new mutable.StringBuilder()
    sb.append(sym.namespace.mkString("."))
    if (sym.namespace.nonEmpty) sb.append('.')
    sb.append(sym.text)
    // The id distinguishes a trait's default implementation from an instance's, which can
    // be specialized at the very same type. Leaving it out merges them.
    sym.id.foreach(id => sb.append(0x24.toChar).append(StableName.render(id)))
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

    case Type.UnresolvedJvmType(member, loc) =>
      // Unlike Alias, this is not tolerated: TypeReduction2 resolves every JVM member
      // into a concrete TypeConstructor.Jvm* during type checking, or the constraint
      // solver reports it as a TypeError (ConstraintSolverInterface.mkTypeError) and
      // compilation stops before Specialization runs. A well-typed program can never
      // reach this case, so reaching it here means a real compiler bug, not a case to
      // render around.
      throw InternalCompilerException(s"Unresolved JVM member reached SpecializationKey.of: $member", loc)
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
    case TypeConstructor.JvmConstructor(ctor) => s"JvmConstructor(${jvmConstructorDescriptor(ctor)})"
    case TypeConstructor.JvmMethod(method) => s"JvmMethod(${jvmMethodDescriptor(method)})"
    case TypeConstructor.JvmField(field) => s"JvmField(${jvmFieldDescriptor(field)})"

    // Regions carry a counter and are erased before code generation, so all regions are
    // one region as far as a generated name is concerned.
    case TypeConstructor.Region(_) => "Region"

    // Error types carry a counter, and a program containing one does not reach code
    // generation, so distinguishing them would only make the key unstable.
    case TypeConstructor.Error(_, _) => "Error"

    case nullary: Product => nullary.productPrefix
    case other => other.getClass.getSimpleName.stripSuffix("$")
  }

  /**
    * Returns the JVM field descriptor (JVMS §4.3.2) for `clazz` — e.g. `Ljava/lang/String;`
    * for a reference type, `I` for `int`, `[I` for `int[]`. `Class.descriptorString` is the
    * JDK's own encoding, not a formatting decision made here, and it is specified: unlike
    * [[Constructor]]/[[Method]]/[[Field]]'s `toString`, which the JDK documents only as an
    * aid to readability with no format guarantee across versions.
    */
  private def classDescriptor(clazz: Class[?]): String = clazz.descriptorString()

  /**
    * Returns `name` prefixed with its length, so it cannot be confused with the descriptor
    * text appended after it regardless of what characters `name` contains.
    */
  private def lengthPrefixed(name: String): String = s"${name.length}:$name"

  /**
    * Returns a JVM-descriptor-based identity for `ctor`: its declaring class plus its
    * parameter types.
    */
  private def jvmConstructorDescriptor(ctor: Constructor[?]): String =
    s"${classDescriptor(ctor.getDeclaringClass)}(${ctor.getParameterTypes.map(classDescriptor).mkString})"

  /**
    * Returns a JVM-descriptor-based identity for `method`: its declaring class, its
    * length-prefixed name, its parameter types, and its return type.
    *
    * The return type is included, even though a Java *source* compiler never emits two
    * overloads differing only by return type: the JVM itself does not enforce that rule,
    * and javac's own output routinely breaks it for exactly this reason. A covariant
    * return override (`Dog reproduce()` overriding `Animal reproduce()`) compiles to two
    * real, differently-behaving methods in the same class -- the actual override plus a
    * synthetic `ACC_BRIDGE` method with the original erased signature, both named
    * `reproduce`, both taking no arguments, differing only in return type. Excluding it
    * would have rendered both to the identical key.
    */
  private def jvmMethodDescriptor(method: Method): String =
    s"${classDescriptor(method.getDeclaringClass)}${lengthPrefixed(method.getName)}" +
      s"(${method.getParameterTypes.map(classDescriptor).mkString})${classDescriptor(method.getReturnType)}"

  /**
    * Returns a JVM-descriptor-based identity for `field`: its declaring class, its
    * length-prefixed name, and its own type.
    */
  private def jvmFieldDescriptor(field: Field): String =
    s"${classDescriptor(field.getDeclaringClass)}${lengthPrefixed(field.getName)}${classDescriptor(field.getType)}"

}

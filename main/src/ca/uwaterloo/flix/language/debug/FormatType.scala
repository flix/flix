/*
 * Copyright 2020 Matthew Lutze
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


package ca.uwaterloo.flix.language.debug

import ca.uwaterloo.flix.language.ast.Kind.Effect
import ca.uwaterloo.flix.language.ast.{Type, TypeConstructor}
import ca.uwaterloo.flix.util.InternalCompilerException
import ca.uwaterloo.flix.util.vt.{VirtualString, VirtualTerminal}

object FormatType {

  def formatType(tpe: Type)(implicit audience: Audience): String = {

    val renameMap = alphaRenameVars(tpe)

    def formatWellFormedRecord(record: Type): String = flattenRecord(record) match {
      case FlatNestable(fields, Type.Cst(TypeConstructor.RecordEmpty)) =>
        fields.map { case (label, tpe) => formatRecordField(label, tpe) }.mkString("{ ", ", ", " }")
      case FlatNestable(fields, rest) =>
        val fieldString = fields.map { case (label, tpe) => formatRecordField(label, tpe) }.mkString(", ")
        s"{ $fieldString | ${visit(rest)} }"
    }

    def formatWellFormedSchema(schema: Type): String = flattenSchema(schema) match {
      case FlatNestable(fields, Type.Cst(TypeConstructor.SchemaEmpty)) =>
        fields.map { case (label, tpe) => formatSchemaField(label, tpe) }.mkString("#{ ", ", ", " }")
      case FlatNestable(fields, rest) =>
        val fieldString = fields.map { case (label, tpe) => formatSchemaField(label, tpe) }.mkString(", ")
        s"#{ $fieldString | ${visit(rest)} }"
    }

    def formatRecordField(label: String, tpe: Type): String = {
      s"$label: ${visit(tpe)}"
    }

    def formatSchemaField(name: String, tpe: Type): String = {
      val typeConstructor = tpe.typeConstructor
      val fullName = typeConstructor match {
        case Type.Cst(TypeConstructor.Relation) => name
        case Type.Cst(TypeConstructor.Lattice) => s"$name<>"
        case _ => s"$name?"
      }
      val arg = typeConstructor match {
        case Type.Cst(TypeConstructor.Relation) | Type.Cst(TypeConstructor.Lattice) =>
          tpe.typeArguments match {
            case Nil => "(???)"
            case arg :: tail => arg.typeConstructor match {
              case Type.Cst(TypeConstructor.Unit) => formatApply("()", tail)
              case Type.Cst(TypeConstructor.Tuple(_)) => formatApply(formatType(arg), tail)
              case _ => formatApply(s"(${formatType(arg)})", tail)
            }
          }
        case Type.Cst(TypeConstructor.Unit) => "()"
        case Type.Cst(TypeConstructor.Tuple(_)) => formatType(tpe)
        case _ => s"(${formatType(tpe)})"

      }
      s"$fullName$arg"
    }

    def formatApply(name: String, args: List[Type]): String = {
      if (args.isEmpty)
        name
      else
        name + "[" + args.map(visit).mkString(", ") + "]"
    }


    def visit(tpe: Type)(implicit audience: Audience): String = {
      val base = tpe.typeConstructor
      val args = tpe.typeArguments

      base match {
        case tvar@Type.Var(id, kind, _) => audience match {
          case Audience.Internal => kind match {
            case Effect => s"''$id"
            case _ => s"'$id"
          }
          case Audience.External => tvar.getText.getOrElse(renameMap(tvar.id))
        }

        case Type.Cst(TypeConstructor.Unit) => formatApply("Unit", args)
        case Type.Cst(TypeConstructor.Bool) => formatApply("Bool", args)
        case Type.Cst(TypeConstructor.Char) => formatApply("Char", args)
        case Type.Cst(TypeConstructor.Float32) => formatApply("Float32", args)
        case Type.Cst(TypeConstructor.Float64) => formatApply("Float64", args)
        case Type.Cst(TypeConstructor.Int8) => formatApply("Int8", args)
        case Type.Cst(TypeConstructor.Int16) => formatApply("Int16", args)
        case Type.Cst(TypeConstructor.Int32) => formatApply("Int32", args)
        case Type.Cst(TypeConstructor.Int64) => formatApply("Int64", args)
        case Type.Cst(TypeConstructor.BigInt) => formatApply("BigInt", args)
        case Type.Cst(TypeConstructor.Str) => formatApply("String", args)
        case Type.Cst(TypeConstructor.RecordEmpty) => formatApply("{ }", args)
        case Type.Cst(TypeConstructor.SchemaEmpty) => formatApply("#{ }", args)
        case Type.Cst(TypeConstructor.Pure) => formatApply("Pure", args)
        case Type.Cst(TypeConstructor.Impure) => formatApply("Impure", args)

        case Type.Cst(TypeConstructor.Array) => formatApply("Array", args)
        case Type.Cst(TypeConstructor.Channel) => formatApply("Channel", args)
        case Type.Cst(TypeConstructor.Enum(sym, _)) => formatApply(sym.toString, args)
        case Type.Cst(TypeConstructor.Lattice) => formatApply("Lattice", args)
        case Type.Cst(TypeConstructor.Relation) => formatApply("Relation", args)
        case Type.Cst(TypeConstructor.Ref) => formatApply("Ref", args)

        case Type.Cst(TypeConstructor.RecordExtend(label)) => args.length match {
          case 0 => s"{ $label: ??? }"
          case 1 => s"{ $label: ${visit(args.head)} | ??? }"
          case 2 => formatWellFormedRecord(tpe)
          case _ => formatApply(s"RecordExtend($label)", args)
        }

        case Type.Cst(TypeConstructor.SchemaExtend(name)) => args.length match {
          case 0 => s"#{ $name?(???) }"
          case 1 => s"#{ ${formatSchemaField(name, args.head)} | ??? }"
          case 2 => formatWellFormedSchema(tpe)
          case _ => formatApply(s"SchemaExtend($name)", args)
        }

        case Type.Cst(TypeConstructor.Tuple(length)) =>
          val elements = args.take(length).map(visit)
          val applyParams = args.drop(length) // excess elements
          val tuple = elements.padTo(length, "???").mkString("(", ", ", ")")
          formatApply(tuple, applyParams)

        case Type.Cst(TypeConstructor.Tag(sym, tag)) => // TODO better unhappy case handling
          if (args.lengthIs == 2)
            s"$tag${args.head}"
          else
            formatApply(tag, args)

        case Type.Cst(TypeConstructor.Not) => args match {
          case (t1: Type.Var) :: Nil => s"¬${visit(t1)}"
          case t1 :: Nil => s"¬(${visit(t1)})"
          case Nil => "¬???"
          case _ => formatApply("¬", args)
        }

        case Type.Cst(TypeConstructor.And) => args match {
          case (t1: Type.Var) :: (t2: Type.Var) :: Nil => s"${visit(t1)} ∧ ${visit(t2)}"
          case (t1: Type.Var) :: t2 :: Nil => s"${visit(t1)} ∧ (${visit(t2)})"
          case t1 :: (t2: Type.Var) :: Nil => s"(${visit(t1)}) ∧ ${visit(t2)}"
          case t1 :: t2 :: Nil => s"(${visit(t1)}) ∧ (${visit(t2)})"
          case (t1: Type.Var) :: Nil => s"${visit(t1)} ∧ ???"
          case t1 :: Nil => s"(${visit(t1)}) ∧ ???"
          case Nil => s"??? ∧ ???"
          case _ => formatApply("∧", args)
        }

        case Type.Cst(TypeConstructor.Or) => args match {
          case (t1: Type.Var) :: (t2: Type.Var) :: Nil => s"${visit(t1)} ∨ ${visit(t2)}"
          case (t1: Type.Var) :: t2 :: Nil => s"${visit(t1)} ∨ (${visit(t2)})"
          case t1 :: (t2: Type.Var) :: Nil => s"(${visit(t1)}) ∨ ${visit(t2)}"
          case t1 :: t2 :: Nil => s"(${visit(t1)}) ∨ (${visit(t2)})"
          case (t1: Type.Var) :: Nil => s"${visit(t1)} ∨ ???"
          case t1 :: Nil => s"(${visit(t1)}) ∨ ???"
          case Nil => s"??? ∨ ???"
          case _ => formatApply("∨", args)
        }

        case Type.Arrow(arity, eff) =>
          if (arity < 2) {
            formatApply(s"Arrow$arity", args)
          } else {

            // Retrieve and result type.
            val types = args.take(arity).map(visit)
            val applyParams = args.drop(arity) // excess args
            val typeStrings = types.padTo(arity, "???")


            // Format the arguments.
            val argPart = typeStrings.init.mkString(" -> ")
            // Format the arrow.
            val arrowPart = eff match {
              case Type.Cst(TypeConstructor.Impure) => " ~> "
              case _ => " -> "
            }
            // Format the effect.
            val effPart = eff match {
              case Type.Cst(TypeConstructor.Pure) => ""
              case Type.Cst(TypeConstructor.Impure) => ""
              case _: Type.Var => s" & ${visit(eff)}"
              case _ => " & (" + visit(eff) + ")"
            }
            // Format the result type.
            val resultPart = typeStrings.last

            // Put everything together.
            val applyPart = argPart + arrowPart + resultPart + effPart
            if (applyParams.isEmpty) {
              applyPart
            } else {
              formatApply(s"($applyPart)", applyParams)
            }
          }

        case Type.Lambda(tvar, tpe) => audience match {
          case Audience.Internal => s"${tvar.id.toString} => ${visit(tpe)}"
          case Audience.External => s"${tvar.getText.getOrElse(renameMap(tvar.id))} => ${visit(tpe)}"
        }

        case Type.Cst(TypeConstructor.Native(clazz)) => s"${clazz.getSimpleName}"

        case Type.Apply(_, _) => throw InternalCompilerException("Unexpected type: Apply") // Should be impossible

      }
    }

    visit(tpe)

  }

  /**
    * Returns a human readable representation of the given type difference.
    */
  def formatTypeDiff(td: TypeDiff, color: String => VirtualString)(implicit audience: Audience): VirtualTerminal = {
    val vt = new VirtualTerminal()

    def visit(d: TypeDiff): Unit = {
      val base = d.typeConstructor
      val args = d.typeArguments

      base match {
          case TypeDiff.Arrow =>
            intercalate(args, visit, vt, before = "", separator = " -> ", after = "")
          case TypeDiff.Enum =>
            vt << "*"
            intercalate(args, visit, vt, before = "[", separator = ", ", after = "]")
          case TypeDiff.Tuple =>
            intercalate(args, visit, vt, before = "(", separator = ", ", after = ")")
          case TypeDiff.Other =>
            vt << "*"
            intercalate(args, visit, vt, before = "[", separator = ", ", after = "]")
        case TypeDiff.Mismatch(tpe1, _) => vt << color(formatType(tpe1))
        case _ => throw InternalCompilerException(s"Unexpected base type: '$base'.")
      }
    }

    visit(td)

    vt
  }

  /**
    * Helper function to generate text before, in the middle of, and after a list of items.
    */
  private def intercalate[A](xs: List[A], f: A => Unit, vt: VirtualTerminal, before: String, separator: String, after: String): Unit = {
    if (xs.isEmpty) return
    vt << before
    var first: Boolean = true
    for (x <- xs) {
      if (first) {
        f(x)
      } else {
        vt << separator
        f(x)
      }
      first = false
    }
    vt << after
  }


  /**
    * A flat representation of a schema or record.
    *
    * Contains the fields and their types as a list at the top level.
    * This better mirrors the structure of records and schemas as they are displayed (e.g. `{ x: Int8, y: Bool | r }`)
    * rather than their true underlying shape (e.g. `{ x: Int8 | { y: Bool | r } }`).
    */
  private case class FlatNestable(fields: List[(String, Type)], rest: Type) {
    def ::(head: (String, Type)): FlatNestable = {
      copy(fields = head :: fields)
    }
  }

  /**
    * Convert a record to a [[FlatNestable]].
    */
  private def flattenRecord(record: Type): FlatNestable = record match {
    case Type.Apply(Type.Apply(Type.Cst(TypeConstructor.RecordExtend(label)), tpe), rest) =>
      (label, tpe) :: flattenRecord(rest)
    case _ => FlatNestable(Nil, record)
  }

  /**
    * Convert a schema to a [[FlatNestable]].
    */
  private def flattenSchema(schema: Type): FlatNestable = schema match {
    case Type.Apply(Type.Apply(Type.Cst(TypeConstructor.SchemaExtend(label)), tpe), rest) =>
      (label, tpe) :: flattenSchema(rest)
    case _ => FlatNestable(Nil, schema)
  }


  /**
    * Get the var name for the given index.
    * Maps `0-25` to `a-z`,
    * then `26-51` to `a1-z1`,
    * then `52-77` to `a2-z2`,
    * etc.
    */
  private def getVarName(index: Int): String = {
    if (index / 26 <= 0)
      (index + 'a').toChar.toString
    else
      (index + 'a').toChar.toString + (index / 26).toString
  }

  /**
    * Rename the variables in the given type.
    */
  private def alphaRenameVars(tpe0: Type): Map[Int, String] = {
    tpe0.typeVars.toList.sortBy(_.id).zipWithIndex.map {
      case (tvar, index) => tvar.id -> getVarName(index)
    }.toMap
  }
}

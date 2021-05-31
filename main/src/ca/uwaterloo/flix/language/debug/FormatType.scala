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

import ca.uwaterloo.flix.language.ast.Kind.Bool
import ca.uwaterloo.flix.language.ast.{Kind, Rigidity, Type, TypeConstructor}
import ca.uwaterloo.flix.util.InternalCompilerException
import ca.uwaterloo.flix.util.vt.{VirtualString, VirtualTerminal}

object FormatType {

  def formatType(tpe: Type)(implicit audience: Audience): String = {

    val renameMap = alphaRenameVars(tpe)

    def formatWellFormedRecord(record: Type): String = flattenRecord(record) match {
      case FlatNestable(fields, Type.Cst(TypeConstructor.RecordEmpty, _)) =>
        fields.map { case (field, tpe) => formatRecordField(field, tpe) }.mkString("{ ", ", ", " }")
      case FlatNestable(fields, rest) =>
        val fieldString = fields.map { case (field, tpe) => formatRecordField(field, tpe) }.mkString(", ")
        s"{ $fieldString | ${visit(rest)} }"
    }

    def formatWellFormedSchema(schema: Type): String = flattenSchema(schema) match {
      case FlatNestable(fields, Type.Cst(TypeConstructor.SchemaEmpty, _)) =>
        fields.map { case (field, tpe) => formatSchemaField(field, tpe) }.mkString("#{ ", ", ", " }")
      case FlatNestable(fields, rest) =>
        val fieldString = fields.map { case (field, tpe) => formatSchemaField(field, tpe) }.mkString(", ")
        s"#{ $fieldString | ${visit(rest)} }"
    }

    def formatRecordField(field: String, tpe: Type): String = {
      s"$field: ${visit(tpe)}"
    }

    def formatSchemaField(name: String, tpe: Type): String = {
      val typeConstructor = tpe.typeConstructor
      val fullName = typeConstructor match {
        case Some(TypeConstructor.Relation) => name
        case Some(TypeConstructor.Lattice) => s"$name<>"
        case _ => s"$name?"
      }
      val arg = typeConstructor match {
        case Some(TypeConstructor.Relation) | Some(TypeConstructor.Lattice) =>
          tpe.typeArguments match {
            case Nil => "(???)"
            case arg :: tail => arg.typeConstructor match {
              case Some(TypeConstructor.Unit) => formatApply("()", tail)
              case Some(TypeConstructor.Tuple(_)) => formatApply(formatType(arg), tail)
              case _ => formatApply(s"(${formatType(arg)})", tail)
            }
          }
        case Some(TypeConstructor.Unit) => "()"
        case Some(TypeConstructor.Tuple(_)) => formatType(tpe)
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
        case None => tpe match {
          case tvar@Type.Var(id, kind, rigidity, _) => audience match {
            case Audience.Internal => kind match {
              // TODO: We need a systematic way to print type variables, their kind, and rigidity.
              case Bool => s"''$id"
              case _ => s"'$id"
            }
            case Audience.External => tvar.text.getOrElse(renameMap(tvar.id))
          }
          case Type.Lambda(tvar, tpe) => audience match {
            case Audience.Internal => s"${tvar.id.toString} => ${visit(tpe)}"
            case Audience.External => s"${tvar.text.getOrElse(renameMap(tvar.id))} => ${visit(tpe)}"
          }
          case Type.Apply(tpe1, tpe2) => s"${visit(tpe1)}[${visit(tpe2)}]"
          case _ => throw InternalCompilerException(s"Unexpected type: '${tpe.getClass}'.") // TODO: This can lead to infinite recursion.
        }

        case Some(tc) => tc match {
          case TypeConstructor.Unit => formatApply("Unit", args)

          case TypeConstructor.Null => formatApply("Null", args)

          case TypeConstructor.Bool => formatApply("Bool", args)

          case TypeConstructor.Char => formatApply("Char", args)

          case TypeConstructor.Float32 => formatApply("Float32", args)

          case TypeConstructor.Float64 => formatApply("Float64", args)

          case TypeConstructor.Int8 => formatApply("Int8", args)

          case TypeConstructor.Int16 => formatApply("Int16", args)

          case TypeConstructor.Int32 => formatApply("Int32", args)

          case TypeConstructor.Int64 => formatApply("Int64", args)

          case TypeConstructor.BigInt => formatApply("BigInt", args)

          case TypeConstructor.Str => formatApply("String", args)

          case TypeConstructor.RecordEmpty => formatApply("{ }", args)

          case TypeConstructor.SchemaEmpty => formatApply("#{ }", args)

          case TypeConstructor.True => formatApply("true", args)

          case TypeConstructor.False => formatApply("false", args)

          case TypeConstructor.Array => formatApply("Array", args)

          case TypeConstructor.Channel => formatApply("Channel", args)

          case TypeConstructor.Enum(sym, _) => formatApply(sym.toString, args)

          case TypeConstructor.Lattice => formatApply("Lattice", args)

          case TypeConstructor.Relation => formatApply("Relation", args)

          case TypeConstructor.Lazy => formatApply("Lazy", args)

          case TypeConstructor.Ref => formatApply("Ref", args)

          case TypeConstructor.ScopedRef => formatApply("ScopedRef", args)

          case TypeConstructor.RecordExtend(field) => args.length match {
            case 0 => s"{ $field: ??? }"
            case 1 => s"{ $field: ${visit(args.head)} | ??? }"
            case 2 => formatWellFormedRecord(tpe)
            case _ => formatApply(s"RecordExtend($field)", args)
          }

          case TypeConstructor.SchemaExtend(pred) => args.length match {
            case 0 => s"#{ ${pred.name}?(???) }"
            case 1 => s"#{ ${formatSchemaField(pred.name, args.head)} | ??? }"
            case 2 => formatWellFormedSchema(tpe)
            case _ => formatApply(s"SchemaExtend($pred)", args)
          }

          case TypeConstructor.Tuple(length) =>
            val elements = args.take(length).map(visit)
            val applyParams = args.drop(length) // excess elements
            val tuple = elements.padTo(length, "???").mkString("(", ", ", ")")
            formatApply(tuple, applyParams)

          case TypeConstructor.Tag(sym, tag) => // TODO better unhappy case handling
            if (args.lengthIs == 2)
              s"${tag.name}${args.head}"
            else
              formatApply(tag.name, args)

          case TypeConstructor.Not => args match {
            case (t1: Type.Var) :: Nil => s"¬${visit(t1)}"
            case t1 :: Nil => s"¬(${visit(t1)})"
            case Nil => "¬???"
            case _ => formatApply("¬", args)
          }

          case TypeConstructor.And => args match {
            case (t1: Type.Var) :: (t2: Type.Var) :: Nil => s"${visit(t1)} ∧ ${visit(t2)}"
            case (t1: Type.Var) :: t2 :: Nil => s"${visit(t1)} ∧ (${visit(t2)})"
            case t1 :: (t2: Type.Var) :: Nil => s"(${visit(t1)}) ∧ ${visit(t2)}"
            case t1 :: t2 :: Nil => s"(${visit(t1)}) ∧ (${visit(t2)})"
            case (t1: Type.Var) :: Nil => s"${visit(t1)} ∧ ???"
            case t1 :: Nil => s"(${visit(t1)}) ∧ ???"
            case Nil => s"??? ∧ ???"
            case _ => formatApply("∧", args)
          }

          case TypeConstructor.Or => args match {
            case (t1: Type.Var) :: (t2: Type.Var) :: Nil => s"${visit(t1)} ∨ ${visit(t2)}"
            case (t1: Type.Var) :: t2 :: Nil => s"${visit(t1)} ∨ (${visit(t2)})"
            case t1 :: (t2: Type.Var) :: Nil => s"(${visit(t1)}) ∨ ${visit(t2)}"
            case t1 :: t2 :: Nil => s"(${visit(t1)}) ∨ (${visit(t2)})"
            case (t1: Type.Var) :: Nil => s"${visit(t1)} ∨ ???"
            case t1 :: Nil => s"(${visit(t1)}) ∨ ???"
            case Nil => s"??? ∨ ???"
            case _ => formatApply("∨", args)
          }

          case TypeConstructor.Arrow(arity) =>
            if (arity < 2) {
              formatApply(s"Arrow$arity", args)
            } else {

              // Retrieve and result type.
              val eff = args.head
              val typeArgs = args.slice(1, arity + 1)
              val types = typeArgs.map(visit)
              val applyParams = typeArgs.drop(arity + 1) // excess args
              val typeStrings = types.padTo(arity, "???")

              // Format the arguments.
              val argPart = typeStrings.init.mkString(" -> ")
              // Format the arrow.
              val arrowPart = eff match {
                case Type.Cst(TypeConstructor.False, _) => " ~> "
                case _ => " -> "
              }
              // Format the effect.
              val effPart = eff match {
                case Type.Cst(TypeConstructor.True, _) => ""
                case Type.Cst(TypeConstructor.False, _) => ""
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

          case TypeConstructor.Native(clazz) => s"${clazz.getSimpleName}"
        }
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
          vt << "..."
          intercalate(args, visit, vt, before = "[", separator = ", ", after = "]")
        case TypeDiff.Tuple =>
          intercalate(args, visit, vt, before = "(", separator = ", ", after = ")")
        case TypeDiff.Other =>
          vt << "..."
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
    case Type.Apply(Type.Apply(Type.Cst(TypeConstructor.RecordExtend(field), _), tpe), rest) =>
      (field.name, tpe) :: flattenRecord(rest)
    case _ => FlatNestable(Nil, record)
  }

  /**
    * Convert a schema to a [[FlatNestable]].
    */
  private def flattenSchema(schema: Type): FlatNestable = schema match {
    case Type.Apply(Type.Apply(Type.Cst(TypeConstructor.SchemaExtend(pred), _), tpe), rest) =>
      (pred.name, tpe) :: flattenSchema(rest)
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
      "'" + (index + 'a').toChar.toString
    else
      "'" + (index + 'a').toChar.toString + (index / 26).toString
  }

  /**
    * Rename the variables in the given type.
    */
  private def alphaRenameVars(tpe0: Type): Map[Int, String] = {
    val tvars = typeVars(tpe0)
    val starTypeVars = tvars.filter(_.kind == Kind.Star)
    val boolTypeVars = tvars.filter(_.kind == Kind.Bool)
    val otherTypeVars = tvars.filter(k => k.kind != Kind.Star && k.kind != Kind.Bool)
    val orderedTypeVars = starTypeVars ::: boolTypeVars ::: otherTypeVars

    orderedTypeVars.zipWithIndex.map {
      case (tvar, index) => tvar.id -> getVarName(index)
    }.toMap
  }

  /**
    * Returns all type variables in the type in the order in which they appear.
    */
  private def typeVars(tpe0: Type): List[Type.Var] = {
    def visit(t: Type): List[Type.Var] = t match {
      case tvar: Type.Var => tvar :: Nil
      case Type.Cst(tc, loc) => Nil
      case Type.Lambda(tvar, tpe) => tvar :: visit(tpe)
      case Type.Apply(tpe1, tpe2) => visit(tpe1) ::: visit(tpe2)
    }

    visit(tpe0).distinct
  }

}

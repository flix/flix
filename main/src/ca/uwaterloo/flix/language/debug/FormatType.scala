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

import ca.uwaterloo.flix.language.ast.{Kind, Rigidity, Type, TypeConstructor}
import ca.uwaterloo.flix.util.InternalCompilerException

object FormatType {

  /**
    * Formats the given type.
    * The type is assumed to be well-kinded, though not necessarily a proper type (e.g. it may be partially applied).
    */
  def formatType(tpe: Type)(implicit audience: Audience): String = {

    return FormatSimpleType.formatWellKindedType(tpe, Map.empty) // MATT hacking for testing

    val renameMap = alphaRenameVars(tpe)

    def formatWellFormedRecordRow(row: Type, sep: String): String = flattenRecordRow(row) match {
      case FlatNestable(fields, Type.Cst(TypeConstructor.RecordRowEmpty, _)) =>
        fields.map { case (field, tpe) => formatRecordField(field, tpe) }.mkString(", ")
      case FlatNestable(fields, rest) =>
        val fieldString = fields.map { case (field, tpe) => formatRecordField(field, tpe) }.mkString(", ")
        s"$fieldString | ${visit(rest)}"
    }

    def formatWellFormedSchemaRow(row: Type): String = flattenSchemaRow(row) match {
      case FlatNestable(fields, Type.Cst(TypeConstructor.SchemaRowEmpty, _)) =>
        fields.map { case (field, tpe) => formatSchemaField(field, tpe) }.mkString(", ")
      case FlatNestable(fields, rest) =>
        val fieldString = fields.map { case (field, tpe) => formatSchemaField(field, tpe) }.mkString(", ")
        s"$fieldString | ${visit(rest)}"
    }

    def formatRecordField(field: String, tpe: Type): String = {
      s"$field :: ${visit(tpe)}"
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
          case Type.KindedVar(id, kind, _, rigidity, text) =>
            val prefix: String = kind match {
              case Kind.Wild => "_" + id.toString
              case Kind.Star => "t" + id
              case Kind.Bool => "b" + id
              case Kind.RecordRow => "r" + id
              case Kind.SchemaRow => "s" + id
              case Kind.Predicate => "'" + id.toString
              case Kind.Arrow(_, _) => "'" + id.toString
            }
            val suffix = rigidity match {
              case Rigidity.Flexible => ""
              case Rigidity.Rigid => "!"
            }
            val s = prefix + suffix
            audience match {
              case Audience.Internal => s
              case Audience.External => text.getOrElse(s)
            }

          case Type.Apply(tpe1, tpe2, loc) => s"${visit(tpe1)}[${visit(tpe2)}]"

          case Type.Alias(cst, aliasArgs, _, _) => formatApply(cst.sym.name, aliasArgs ++ args)

          case _: Type.Cst => throw InternalCompilerException("Unexpected type.")

          case _: Type.Ascribe => throw InternalCompilerException("Unexpected type.")

          case _: Type.UnkindedVar => throw InternalCompilerException("Unexpected type.")
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

          case TypeConstructor.RecordRowEmpty => formatApply("()", args)

          case TypeConstructor.SchemaRowEmpty => formatApply("#()", args)

          case TypeConstructor.True => formatApply("true", args)

          case TypeConstructor.False => formatApply("false", args)

          case TypeConstructor.Channel => formatApply("Channel", args)

          case TypeConstructor.KindedEnum(sym, _) => formatApply(sym.toString, args)

          case TypeConstructor.UnkindedEnum(sym) => formatApply(sym.toString, args)

          case TypeConstructor.UnappliedAlias(sym) => formatApply(sym.toString, args)

          case TypeConstructor.Lattice => formatApply("Lattice", args)

          case TypeConstructor.Relation => formatApply("Relation", args)

          case TypeConstructor.Lazy => formatApply("Lazy", args)

          case TypeConstructor.ScopedArray => formatApply("ScopedArray", args)

          case TypeConstructor.ScopedRef => formatApply("ScopedRef", args)

          case TypeConstructor.RecordRowExtend(field) => args.length match {
            case 0 => s"( $field :: ??? )"
            case 1 => s"( $field :: ${visit(args.head)} | ??? )"
            case 2 => s"( ${formatWellFormedRecordRow(tpe, " :: ")} )"
            case _ => formatApply(s"RecordExtend($field)", args)
          }

          case TypeConstructor.SchemaRowExtend(pred) => args.length match {
            case 0 => s"#( ${pred.name}?(???) )"
            case 1 => s"#( ${formatSchemaField(pred.name, args.head)} | ??? )"
            case 2 => s"#( ${formatWellFormedSchemaRow(tpe)} )"
            case _ => throw InternalCompilerException("unexpected overapplication")
          }

          case TypeConstructor.Record => args.length match {
            case 0 => s"{ ??? }"
            case 1 =>
              val contents = formatWellFormedRecordRow(args.head, ": ")
              s"{ $contents }"
            case _ => throw InternalCompilerException("unexpected overapplication")
          }

          case TypeConstructor.Schema => args.length match {
            case 0 => s"#{ ??? }"
            case 1 =>
              val contents = formatWellFormedSchemaRow(args.head)
              s"#{ $contents }"
            case _ => throw InternalCompilerException("unexpected overapplication")
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
            case (t1: Type.KindedVar) :: Nil => s"¬${visit(t1)}"
            case t1 :: Nil => s"¬(${visit(t1)})"
            case Nil => "¬???"
            case _ => formatApply("¬", args)
          }

          case TypeConstructor.And => args match {
            case (t1: Type.KindedVar) :: (t2: Type.KindedVar) :: Nil => s"${visit(t1)} ∧ ${visit(t2)}"
            case (t1: Type.KindedVar) :: t2 :: Nil => s"${visit(t1)} ∧ (${visit(t2)})"
            case t1 :: (t2: Type.KindedVar) :: Nil => s"(${visit(t1)}) ∧ ${visit(t2)}"
            case t1 :: t2 :: Nil => s"(${visit(t1)}) ∧ (${visit(t2)})"
            case (t1: Type.KindedVar) :: Nil => s"${visit(t1)} ∧ ???"
            case t1 :: Nil => s"(${visit(t1)}) ∧ ???"
            case Nil => s"??? ∧ ???"
            case _ => formatApply("∧", args)
          }

          case TypeConstructor.Or => args match {
            case (t1: Type.KindedVar) :: (t2: Type.KindedVar) :: Nil => s"${visit(t1)} ∨ ${visit(t2)}"
            case (t1: Type.KindedVar) :: t2 :: Nil => s"${visit(t1)} ∨ (${visit(t2)})"
            case t1 :: (t2: Type.KindedVar) :: Nil => s"(${visit(t1)}) ∨ ${visit(t2)}"
            case t1 :: t2 :: Nil => s"(${visit(t1)}) ∨ (${visit(t2)})"
            case (t1: Type.KindedVar) :: Nil => s"${visit(t1)} ∨ ???"
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
                case _: Type.KindedVar => s" & ${visit(eff)}"
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

          case TypeConstructor.Region => formatApply("Region", args)

          case TypeConstructor.Native(clazz) => s"${clazz.getSimpleName}"
        }
      }
    }

    // TODO: Until the new formatter arrives we do this:
    try {
      visit(tpe)
    } catch {
      case _: Throwable => "ERR_UNABLE_TO_FORMAT_TYPE"
    }
  }

  /**
    * A flat representation of a schema or record.
    *
    * Contains the fields and their types as a list at the top level.
    * This better mirrors the structure of records and schemas as they are displayed (e.g. `{ x :: Int8, y :: Bool | r }`)
    * rather than their true underlying shape (e.g. `{ x :: Int8 | { y :: Bool | r } }`).
    */
  private case class FlatNestable(fields: List[(String, Type)], rest: Type) {
    def ::(head: (String, Type)): FlatNestable = {
      copy(fields = head :: fields)
    }
  }

  /**
    * Convert a record to a [[FlatNestable]].
    */
  private def flattenRecordRow(record: Type): FlatNestable = record match {
    case Type.Apply(Type.Apply(Type.Cst(TypeConstructor.RecordRowExtend(field), _), tpe, _), rest, _) =>
      (field.name, tpe) :: flattenRecordRow(rest)
    case _ => FlatNestable(Nil, record)
  }

  /**
    * Convert a schema to a [[FlatNestable]].
    */
  private def flattenSchemaRow(schema: Type): FlatNestable = schema match {
    case Type.Apply(Type.Apply(Type.Cst(TypeConstructor.SchemaRowExtend(pred), _), tpe, _), rest, _) =>
      (pred.name, tpe) :: flattenSchemaRow(rest)
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
      case tvar: Type.KindedVar => tvar :: Nil
      case tvar: Type.UnkindedVar => tvar :: Nil
      case Type.Cst(tc, loc) => Nil
      case Type.Apply(tpe1, tpe2, _) => visit(tpe1) ::: visit(tpe2)
      case Type.Ascribe(tpe, _, _) => typeVars(tpe)
      case Type.Alias(_, _, tpe, _) => typeVars(tpe)
    }

    visit(tpe0).distinct
  }

}

/*
 * Copyright 2026 Magnus Madsen
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

import ca.uwaterloo.flix.language.ast.{SimpleType, Symbol, Type, TypeConstructor}
import ca.uwaterloo.flix.util.Result

import scala.annotation.tailrec

/**
  * Portable export ABI classification shared by the LLVM-native and LLVM-wasm embedding surfaces.
  *
  * v1 exposes a deliberately small but actually useful recursive type universe:
  *   - Unit, Bool, Int8/16/32/64, Float32/64, String, Bytes
  *   - List[T]
  *   - Array[T] (from source-level `Array[T, Static]`)
  *   - Tuple[...]
  *   - Option[T]
  *   - Result[Ok, Err]
  *   - closed structural records with distinct labels
  *
  * Arbitrary ADTs are intentionally excluded for now.
  */
object ExportAbi {

  case class Signature(params: List[AbiType], result: AbiType)

  sealed trait AbiType {
    def displayName: String

    def stableId: String

    def isPointerLike: Boolean
  }

  object AbiType {
    case object Unit extends AbiType {
      val displayName = "Unit"
      val stableId = "unit"
      val isPointerLike = false
    }

    case object Bool extends AbiType {
      val displayName = "Bool"
      val stableId = "bool"
      val isPointerLike = false
    }

    case object Int8 extends AbiType {
      val displayName = "Int8"
      val stableId = "int8"
      val isPointerLike = false
    }

    case object Int16 extends AbiType {
      val displayName = "Int16"
      val stableId = "int16"
      val isPointerLike = false
    }

    case object Int32 extends AbiType {
      val displayName = "Int32"
      val stableId = "int32"
      val isPointerLike = false
    }

    case object Int64 extends AbiType {
      val displayName = "Int64"
      val stableId = "int64"
      val isPointerLike = false
    }

    case object Float32 extends AbiType {
      val displayName = "Float32"
      val stableId = "float32"
      val isPointerLike = false
    }

    case object Float64 extends AbiType {
      val displayName = "Float64"
      val stableId = "float64"
      val isPointerLike = false
    }

    case object String extends AbiType {
      val displayName = "String"
      val stableId = "string"
      val isPointerLike = true
    }

    case object Bytes extends AbiType {
      val displayName = "Bytes"
      val stableId = "bytes"
      val isPointerLike = true
    }

    case class List(elm: AbiType) extends AbiType {
      val displayName: String = s"List[${elm.displayName}]"
      val stableId: String = s"list_${elm.stableId}"
      val isPointerLike: Boolean = true
    }

    /**
      * Host-facing ABI for source-level `Array[t, Static]`.
      *
      * The public representation is still a length+elements sequence, but we keep array/list
      * distinct in the ABI model so typed exports and typed resumptions stay semantically precise.
      */
    case class Array(elm: AbiType) extends AbiType {
      val displayName: String = s"Array[${elm.displayName}]"
      val stableId: String = s"array_${elm.stableId}"
      val isPointerLike: Boolean = true
    }

    case class Tuple(elms: scala.List[AbiType]) extends AbiType {
      val displayName: String = s"Tuple[${elms.map(_.displayName).mkString(", ")}]"
      val stableId: String = s"tuple${elms.length}_${elms.map(_.stableId).mkString("_")}"
      val isPointerLike: Boolean = true
    }

    case class Option(elm: AbiType) extends AbiType {
      val displayName: String = s"Option[${elm.displayName}]"
      val stableId: String = s"option_${elm.stableId}"
      val isPointerLike: Boolean = true
    }

    /**
      * Portable export results are host-facing, so `ok` comes first even though Flix source
      * `Result[e, t]` is parameterized as error-then-success.
      */
    case class Result(ok: AbiType, err: AbiType) extends AbiType {
      val displayName: String = s"Result[${ok.displayName}, ${err.displayName}]"
      val stableId: String = s"result_${ok.stableId}_${err.stableId}"
      val isPointerLike: Boolean = true
    }

    case class Record(fields: scala.List[(String, AbiType)]) extends AbiType {
      val displayName: String = s"{${fields.map { case (label, tpe) => s"$label = ${tpe.displayName}" }.mkString(", ")}}"
      val stableId: String = s"record_${fields.map { case (label, tpe) => s"${stableFieldId(label)}_${tpe.stableId}" }.mkString("_")}"
      val isPointerLike: Boolean = true
    }
  }

  /**
    * Returns the portable export ABI type for the given source-level type.
    *
    * `Ok(None)` means "well-formed but not exportable".
    * `Err(())` means "malformed / unresolved".
    */
  def portableFromType(tpe: Type): Result[Option[AbiType], Unit] =
    portableFromType0(Type.eraseAliases(tpe))

  /**
    * Returns the portable export ABI type for the given lowered/simple type.
    */
  def portableFromSimpleType(tpe: SimpleType): Option[AbiType] = tpe match {
    case SimpleType.Unit => Some(AbiType.Unit)
    case SimpleType.Bool => Some(AbiType.Bool)
    case SimpleType.Int8 => Some(AbiType.Int8)
    case SimpleType.Int16 => Some(AbiType.Int16)
    case SimpleType.Int32 => Some(AbiType.Int32)
    case SimpleType.Int64 => Some(AbiType.Int64)
    case SimpleType.Float32 => Some(AbiType.Float32)
    case SimpleType.Float64 => Some(AbiType.Float64)
    case SimpleType.String => Some(AbiType.String)
    case SimpleType.Array(SimpleType.Int8) => Some(AbiType.Bytes)
    case SimpleType.Array(elm) =>
      portableFromSimpleType(elm).map(t => AbiType.Array(t))
    case SimpleType.Tuple(elms) =>
      traverse(elms)(portableFromSimpleType).map(ts => AbiType.Tuple(ts))
    case tpe =>
      portableRecordFromSimpleType(tpe).orElse(tpe match {
        case SimpleType.Enum(sym, targs) if isPortableListSym(sym) && targs.length == 1 =>
          portableFromSimpleType(targs.head).map(t => AbiType.List(t))
        case SimpleType.Enum(sym, targs) if isPortableOptionSym(sym) && targs.length == 1 =>
          portableFromSimpleType(targs.head).map(t => AbiType.Option(t))
        case SimpleType.Enum(sym, targs) if isPortableResultSym(sym) && targs.length == 2 =>
          for {
            ok <- portableFromSimpleType(targs(1))
            err <- portableFromSimpleType(targs.head)
          } yield AbiType.Result(ok, err)
        case _ => None
      })
  }

  def portableSignature(params: List[SimpleType], result: SimpleType): Option[Signature] =
    for {
      ps <- traverse(params)(portableFromSimpleType)
      r <- portableFromSimpleType(result)
    } yield Signature(ps, r)

  /**
    * Exported Flix defs with no user-visible parameters are represented internally as a
    * single `Unit` parameter. Collapse that here so public embedding surfaces use a genuine
    * zero-argument ABI instead of `tuple<>` / `Unit` placeholder parameters.
    */
  def portableExportSignature(params: List[SimpleType], result: SimpleType): Option[Signature] =
    portableSignature(normalizeExportParams(params), result)

  def aggregateTypes(sig: Signature): List[AbiType] =
    (sig.params :+ sig.result).flatMap(flattenAbiType).filter(isAggregate).distinct

  def flattenAbiType(tpe: AbiType): List[AbiType] =
    tpe :: (tpe match {
      case AbiType.List(elm) => flattenAbiType(elm)
      case AbiType.Array(elm) => flattenAbiType(elm)
      case AbiType.Tuple(elms) => elms.flatMap(flattenAbiType)
      case AbiType.Option(elm) => flattenAbiType(elm)
      case AbiType.Result(ok, err) => flattenAbiType(ok) ::: flattenAbiType(err)
      case AbiType.Record(fields) => fields.flatMap { case (_, fieldTpe) => flattenAbiType(fieldTpe) }
      case _ => Nil
    })

  def isAggregate(tpe: AbiType): Boolean = tpe match {
    case AbiType.List(_) | AbiType.Array(_) | AbiType.Tuple(_) | AbiType.Option(_) | AbiType.Result(_, _) | AbiType.Record(_) => true
    case _ => false
  }

  private def normalizeExportParams(params: List[SimpleType]): List[SimpleType] = params match {
    case SimpleType.Unit :: Nil => Nil
    case _ => params
  }

  private def portableFromType0(tpe: Type): Result[Option[AbiType], Unit] = tpe match {
    case Type.Cst(TypeConstructor.Unit, _) => Result.Ok(Some(AbiType.Unit))
    case Type.Cst(TypeConstructor.Bool, _) => Result.Ok(Some(AbiType.Bool))
    case Type.Cst(TypeConstructor.Int8, _) => Result.Ok(Some(AbiType.Int8))
    case Type.Cst(TypeConstructor.Int16, _) => Result.Ok(Some(AbiType.Int16))
    case Type.Cst(TypeConstructor.Int32, _) => Result.Ok(Some(AbiType.Int32))
    case Type.Cst(TypeConstructor.Int64, _) => Result.Ok(Some(AbiType.Int64))
    case Type.Cst(TypeConstructor.Float32, _) => Result.Ok(Some(AbiType.Float32))
    case Type.Cst(TypeConstructor.Float64, _) => Result.Ok(Some(AbiType.Float64))
    case Type.Cst(TypeConstructor.Str, _) => Result.Ok(Some(AbiType.String))

    case _ =>
      val (base, args) = decomposeApplyChain(tpe)
      base match {
        case Type.Cst(TypeConstructor.Array, _) if args.length == 2 =>
          val elm = args.head
          val reg = args(1)
          (isInt8Type(elm), isStaticRegion(reg), portableFromType0(elm)) match {
            case (Result.Ok(true), Result.Ok(true), _) => Result.Ok(Some(AbiType.Bytes))
            case (Result.Ok(false), Result.Ok(true), Result.Ok(Some(elmTpe))) => Result.Ok(Some(AbiType.Array(elmTpe)))
            case (Result.Ok(false), Result.Ok(true), Result.Ok(None)) => Result.Ok(None)
            case (Result.Ok(_), Result.Ok(true), Result.Err(_)) => Result.Err(())
            case (Result.Ok(_), Result.Ok(false), _) => Result.Ok(None)
            case (Result.Ok(_), Result.Err(_), _) => Result.Err(())
            case (Result.Err(_), _, _) => Result.Err(())
          }

        case Type.Cst(TypeConstructor.Tuple(arity), _) if args.length == arity =>
          traverseResult(args)(portableFromType0) match {
            case Result.Ok(ts) => Result.Ok(Some(AbiType.Tuple(ts)))
            case Result.Err(_) => Result.Err(())
          }

        case Type.Cst(TypeConstructor.Record, _) if args.length == 1 =>
          portableRecordFromTypeRow(args.head) match {
            case Result.Ok(Some(fields)) => Result.Ok(Some(AbiType.Record(fields)))
            case Result.Ok(None) => Result.Ok(None)
            case Result.Err(_) => Result.Err(())
          }

        case Type.Cst(TypeConstructor.Enum(sym, _), _) if isPortableListSym(sym) && args.length == 1 =>
          portableFromType0(args.head) match {
            case Result.Ok(Some(tpe0)) => Result.Ok(Some(AbiType.List(tpe0)))
            case Result.Ok(None) => Result.Ok(None)
            case Result.Err(_) => Result.Err(())
          }

        case Type.Cst(TypeConstructor.Enum(sym, _), _) if isPortableOptionSym(sym) && args.length == 1 =>
          portableFromType0(args.head) match {
            case Result.Ok(Some(tpe0)) => Result.Ok(Some(AbiType.Option(tpe0)))
            case Result.Ok(None) => Result.Ok(None)
            case Result.Err(_) => Result.Err(())
          }

        case Type.Cst(TypeConstructor.Enum(sym, _), _) if isPortableResultSym(sym) && args.length == 2 =>
          (portableFromType0(args(1)), portableFromType0(args.head)) match {
            case (Result.Ok(Some(ok)), Result.Ok(Some(err))) => Result.Ok(Some(AbiType.Result(ok, err)))
            case (Result.Ok(None), _) | (_, Result.Ok(None)) => Result.Ok(None)
            case _ => Result.Err(())
          }

        case Type.Cst(_, _) => Result.Ok(None)
        case Type.Apply(_, _, _) => Result.Ok(None)
        case Type.Var(_, _) => Result.Err(())
        case Type.AssocType(_, _, _, _) => Result.Err(())
        case Type.JvmToType(_, _) => Result.Err(())
        case Type.JvmToEff(_, _) => Result.Err(())
        case Type.UnresolvedJvmType(_, _) => Result.Err(())
        case Type.Alias(_, _, _, _) => throw new IllegalStateException("aliases must be erased")
      }
  }

  private def isInt8Type(tpe: Type): Result[Boolean, Unit] = tpe match {
    case Type.Cst(TypeConstructor.Int8, _) => Result.Ok(true)
    case Type.Cst(_, _) => Result.Ok(false)
    case Type.Apply(_, _, _) => Result.Ok(false)
    case Type.Var(_, _) => Result.Err(())
    case Type.AssocType(_, _, _, _) => Result.Err(())
    case Type.JvmToType(_, _) => Result.Err(())
    case Type.JvmToEff(_, _) => Result.Err(())
    case Type.UnresolvedJvmType(_, _) => Result.Err(())
    case Type.Alias(_, _, _, _) => throw new IllegalStateException("aliases must be erased")
  }

  private def isStaticRegion(tpe: Type): Result[Boolean, Unit] = tpe match {
    case Type.Cst(TypeConstructor.Effect(sym, _), _) if sym == Symbol.IO => Result.Ok(true)
    case Type.Cst(_, _) => Result.Ok(false)
    case Type.Apply(_, _, _) => Result.Ok(false)
    case Type.Var(_, _) => Result.Err(())
    case Type.AssocType(_, _, _, _) => Result.Err(())
    case Type.JvmToType(_, _) => Result.Err(())
    case Type.JvmToEff(_, _) => Result.Err(())
    case Type.UnresolvedJvmType(_, _) => Result.Err(())
    case Type.Alias(_, _, _, _) => throw new IllegalStateException("aliases must be erased")
  }

  private def traverse[A, B](xs: List[A])(f: A => Option[B]): Option[List[B]] =
    xs.foldRight(Option(List.empty[B])) {
      case (x, Some(acc)) => f(x).map(_ :: acc)
      case (_, None) => None
    }

  private def traverseResult[A, B](xs: List[A])(f: A => Result[Option[B], Unit]): Result[List[B], Unit] =
    xs.foldRight(Result.Ok(List.empty[B]): Result[List[B], Unit]) {
      case (x, Result.Ok(acc)) =>
        f(x) match {
          case Result.Ok(Some(v)) => Result.Ok(v :: acc)
          case Result.Ok(None) => Result.Err(())
          case Result.Err(_) => Result.Err(())
        }
      case (_, err@Result.Err(_)) => err
    }

  private def decomposeApplyChain(tpe: Type): (Type, List[Type]) = {
    @tailrec
    def loop(t: Type, acc: List[Type]): (Type, List[Type]) = t match {
      case Type.Apply(t1, t2, _) => loop(t1, t2 :: acc)
      case other => (other, acc)
    }
    loop(tpe, Nil)
  }

  private def portableRecordFromSimpleType(tpe: SimpleType): Option[AbiType] =
    recordFieldsOfSimpleType(tpe).flatMap {
      case Nil => None
      case fields => Some(AbiType.Record(fields.sortBy(_._1)))
    }

  private def recordFieldsOfSimpleType(tpe: SimpleType): Option[List[(String, AbiType)]] = {
    @tailrec
    def loop(t: SimpleType, seen: Set[String], acc: List[(String, AbiType)]): Option[List[(String, AbiType)]] = t match {
      case SimpleType.RecordEmpty => Some(acc.reverse)
      case SimpleType.RecordExtend(label, value, rest) =>
        if (seen.contains(label) || !isPortableRecordLabel(label)) None
        else portableFromSimpleType(value) match {
          case Some(valueTpe) => loop(rest, seen + label, (label -> valueTpe) :: acc)
          case None => None
        }
      case _ => None
    }
    loop(tpe, Set.empty, Nil)
  }

  private def portableRecordFromTypeRow(row: Type): Result[Option[List[(String, AbiType)]], Unit] = {
    def loop(t: Type, seen: Set[String], acc: List[(String, AbiType)]): Result[Option[List[(String, AbiType)]], Unit] = t match {
      case Type.Cst(TypeConstructor.RecordRowEmpty, _) =>
        if (acc.isEmpty) Result.Ok(None) else Result.Ok(Some(acc.reverse.sortBy(_._1)))
      case Type.Apply(Type.Apply(Type.Cst(TypeConstructor.RecordRowExtend(label), _), value, _), rest, _) =>
        if (seen.contains(label.name) || !isPortableRecordLabel(label.name)) Result.Ok(None)
        else portableFromType0(value) match {
          case Result.Ok(Some(valueTpe)) => loop(rest, seen + label.name, (label.name -> valueTpe) :: acc)
          case Result.Ok(None) => Result.Ok(None)
          case Result.Err(_) => Result.Err(())
        }
      case Type.Var(_, _) => Result.Err(())
      case Type.AssocType(_, _, _, _) => Result.Err(())
      case Type.JvmToType(_, _) => Result.Err(())
      case Type.JvmToEff(_, _) => Result.Err(())
      case Type.UnresolvedJvmType(_, _) => Result.Err(())
      case Type.Alias(_, _, _, _) => throw new IllegalStateException("aliases must be erased")
      case _ => Result.Ok(None)
    }

    loop(row, Set.empty, Nil)
  }

  private def stableFieldId(label: String): String = {
    val cleaned = label.map {
      case c if c.isLetterOrDigit || c == '_' => c
      case _ => '_'
    }.mkString
    if (cleaned.isEmpty) "field"
    else if (cleaned.head.isDigit) s"_$cleaned"
    else cleaned
  }

  private def isPortableRecordLabel(label: String): Boolean =
    label.matches("[A-Za-z_][A-Za-z0-9_]*")

  private def isPortableOptionSym(sym: Symbol.EnumSym): Boolean =
    sym.namespace.isEmpty && sym.text == "Option"

  private def isPortableResultSym(sym: Symbol.EnumSym): Boolean =
    sym.namespace.isEmpty && sym.text == "Result"

  private def isPortableListSym(sym: Symbol.EnumSym): Boolean =
    sym.namespace.isEmpty && sym.text == "List"
}

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

package ca.uwaterloo.flix.tools

import ca.uwaterloo.flix.util.{FileOps, Result, ZigToolchain}
import org.tomlj.{Toml, TomlTable}

import java.io.IOException
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import scala.collection.mutable
import scala.jdk.CollectionConverters.{ListHasAsScala, SetHasAsScala}

/**
 * Generates Flix `extern native` declarations from a curated C header by using
 * `zig translate-c` as the parsing/normalization substrate plus an optional
 * sidecar TOML spec for ownership/effect/callback semantics.
 *
 * This generator slice is intentionally conservative:
 *   - curated headers only,
 *   - direct synchronous native imports only,
 *   - direct scalar/unit C ABI signatures,
 *   - shim-backed borrowed `const char*` and byte-slice inputs,
 *   - explicit synchronous export-backed callback trampolines over scalar/unit callback signatures,
 *   - explicit borrowed/owned string, byte, and opaque-handle result annotations from a sidecar spec,
 *   - anything richer is reported as skipped rather than guessed.
 */
object NativeBindingsTool {

  case class Config(header: Path,
                    outDir: Path,
                    rootModule: String = "Native",
                    spec: Option[Path] = None,
                    includePaths: List[Path] = Nil,
                    defines: List[String] = Nil,
                    cflags: List[String] = Nil)

  case class Skipped(symbol: String, reason: String)

  case class Generated(flixFile: Path,
                       shimFile: Option[Path],
                       shimHeaderFile: Option[Path],
                       totalDecls: Int,
                       generatedDecls: Int,
                       skipped: List[Skipped])

  private case class HandleFamily(opaqueBase: String,
                                  ownedType: Option[String],
                                  borrowedType: Option[String])

  private case class SourceDecl(params: List[String],
                                resultAnnotation: Option[ResultAnnotation],
                                callback: Option[CallbackAnnotation],
                                borrowedFromParam: Option[String],
                                destroyParam: Option[String],
                                retainParam: Option[String],
                                effect: Option[String],
                                annotationError: Option[String])

  private case class SourceAnnotation(resultAnnotation: Option[ResultAnnotation],
                                      callback: Option[CallbackAnnotation],
                                      borrowedFromParam: Option[String],
                                      destroyParam: Option[String],
                                      retainParam: Option[String],
                                      effect: Option[String])

  private case class ZigExtern(symbol: String,
                               params: List[(String, String)],
                               result: String)

  private case class CallbackAnnotation(paramName: String,
                                        exportDef: String)

  private case class CallbackType(alias: String,
                                  params: List[String],
                                  result: String)

  private sealed trait LoweredParam {
    def publicParams: List[(String, String)]
  }

  private case class ScalarParam(name: String, flixType: String, zigType: String) extends LoweredParam {
    def publicParams: List[(String, String)] = List((name, flixType))
  }

  private case class CStringParam(name: String) extends LoweredParam {
    def publicParams: List[(String, String)] = List((name, "String"))
  }

  private case class BytesParam(name: String, ptrCType: String, lenCType: String) extends LoweredParam {
    def publicParams: List[(String, String)] = List((name, "Array[Int8, Static]"))
  }

  private case class HandleParam(name: String, flixType: String, cType: String) extends LoweredParam {
    def publicParams: List[(String, String)] = List((name, flixType))
  }

  private sealed trait LoweredResult {
    def publicResult: String
  }

  private case class ScalarResult(flixType: String, cType: String) extends LoweredResult {
    def publicResult: String = flixType
  }

  private case class StringResult(cType: String, ownership: StringResultOwnership) extends LoweredResult {
    def publicResult: String = "String"
  }

  private case class BytesResult(lenOut: HiddenResultLenParam, ownership: BytesResultOwnership) extends LoweredResult {
    def publicResult: String = "Array[Int8, Static]"
  }

  private case class HandleResult(flixType: String, cType: String) extends LoweredResult {
    def publicResult: String = s"Option[$flixType]"
  }

  private case class HandleStatusResult(statusFlixType: String,
                                        statusCType: String,
                                        handleFlixType: String,
                                        outHandle: HiddenOutHandleParam,
                                        okValue: String) extends LoweredResult {
    def publicResult: String = s"Result[$statusFlixType, $handleFlixType]"
  }

  private sealed trait SourceParamShape

  private case object SourceParamOther extends SourceParamShape

  private case object SourceCStringBorrowed extends SourceParamShape

  private case class SourceBytesBorrowed(ptrCType: String) extends SourceParamShape

  private case class SourceLength(cType: String) extends SourceParamShape

  private case class SourceLengthOut(cType: String) extends SourceParamShape

  private sealed trait ResultAnnotation

  private case object BorrowedStringResultAnnotation extends ResultAnnotation

  private case class OwnedStringResultAnnotation(freeFn: String) extends ResultAnnotation

  private case class BorrowedBytesResultAnnotation(lenParam: String) extends ResultAnnotation

  private case class OwnedBytesResultAnnotation(lenParam: String, freeFn: String) extends ResultAnnotation

  private case class OwnedHandleResultAnnotation(flixType: String) extends ResultAnnotation

  private case class BorrowedHandleResultAnnotation(flixType: String) extends ResultAnnotation

  private case class StatusOwnedHandleResultAnnotation(flixType: String, outParam: String, okValue: String) extends ResultAnnotation

  private sealed trait StringResultOwnership

  private case object BorrowedStringOwnership extends StringResultOwnership

  private case class OwnedStringOwnership(freeFn: String) extends StringResultOwnership

  private sealed trait BytesResultOwnership

  private case object BorrowedBytesOwnership extends BytesResultOwnership

  private case class OwnedBytesOwnership(freeFn: String) extends BytesResultOwnership

  private case class HiddenResultLenParam(name: String, cType: String)

  private case class HiddenOutHandleParam(name: String, cType: String, flixType: String)

  private sealed trait ShimCallArg

  private case class ScalarCallArg(name: String) extends ShimCallArg

  private case class CStringCallArg(name: String) extends ShimCallArg

  private case class BytesCallArg(name: String, ptrCType: String, lenCType: String) extends ShimCallArg

  private case class ResultLenOutCallArg(param: HiddenResultLenParam) extends ShimCallArg

  private case class ResultHandleOutCallArg(param: HiddenOutHandleParam) extends ShimCallArg

  private case class HandleCallArg(name: String, cType: String) extends ShimCallArg

  private case class CallbackCallArg(name: String,
                                     exportSymbol: String,
                                     callbackType: CallbackType) extends ShimCallArg

  private case class LoweredParams(publicParams: List[LoweredParam],
                                   callArgs: List[ShimCallArg],
                                   hiddenResultLen: Option[HiddenResultLenParam],
                                   hiddenOutHandle: Option[HiddenOutHandleParam])

  private case class NativeShim(symbol: String,
                                originalSymbol: String,
                                result: LoweredResult,
                                params: List[LoweredParam],
                                callArgs: List[ShimCallArg])

  private case class HandleBorrowHelper(defName: String,
                                        ownedType: String,
                                        borrowedType: String)

  private case class ResourceCloseHelper(defName: String,
                                         targetDefName: String,
                                         handleType: String)

  private case class ResourceRetainHelper(defName: String,
                                          targetDefName: String,
                                          handleType: String,
                                          targetParamType: String)

  private case class BorrowedOwnerHelper(defName: String,
                                         targetDefName: String,
                                         publicParams: List[(String, String)],
                                         targetArgs: List[String],
                                         publicResult: String,
                                         effect: Option[String])

  private case class FlixBinding(symbol: String,
                                 targetSymbol: String,
                                 defName: String,
                                 rawName: String,
                                 publicParams: List[(String, String)],
                                 rawParams: List[(String, String)],
                                 publicResult: String,
                                 rawResult: String,
                                 effect: Option[String],
                                 wrapperExpr: String,
                                 handleTypes: Set[String],
                                 shim: Option[NativeShim])

  private case class TranslatedOpaquePtr(baseName: String, cType: String, isConst: Boolean)

  private val FlixIdentifierRegex = "^[A-Za-z_][A-Za-z0-9_]*$".r

  private val ReservedIdentifiers: Set[String] = Set(
    "alias", "and", "as", "case", "catch", "checked_cast", "checked_ecast", "choose",
    "def", "discard", "eff", "else", "ematch", "enum", "extern", "false", "fix",
    "forA", "forM", "forall", "force", "foreach", "from", "handler", "if", "import",
    "inject", "instance", "instanceof", "into", "law", "lawful", "lazy", "let",
    "match", "mod", "mut", "new", "not", "null", "open_variant", "open_variant_as",
    "or", "override", "par", "pquery", "project", "psolve", "pub", "query", "redef",
    "region", "restrictable", "run", "sealed", "select", "solve", "spawn", "static",
    "struct", "throw", "trait", "true", "try", "type", "typematch", "unchecked_cast",
    "unsafe", "use", "where", "with", "without", "xor", "yield",
  )

  def run(config: Config): Result[Generated, String] = {
    validateConfig(config).flatMap { _ =>
      val headerSource = Files.readString(config.header.toAbsolutePath.normalize(), StandardCharsets.UTF_8)
      val headerDecls = parseHeaderDecls(headerSource)
      parseBindingSpec(config.spec, headerDecls.keySet).flatMap { specAnnotations =>
        val sourceDecls = mergeSourceDecls(headerDecls, specAnnotations)
        val translated = runTranslateC(config)
        translated.map { zigSource =>
        val decls = parseExternDecls(zigSource)
        val callbackTypes = parseCallbackTypes(zigSource)
        val lowered = lowerDecls(decls, sourceDecls, callbackTypes)
        val flixFile = config.outDir.toAbsolutePath.normalize().resolve("flix").resolve(s"${config.rootModule}.flix")
        FileOps.writeString(flixFile, renderFlix(config, lowered._1, lowered._2, lowered._3, lowered._4, lowered._5, lowered._6))
        val shims = lowered._1.flatMap(_.shim)
        val (shimFile, shimHeaderFile) =
          if (shims.isEmpty) {
            (None, None)
          } else {
            val nativeDir = config.outDir.toAbsolutePath.normalize().resolve("native")
            val includeDir = nativeDir.resolve("include")
            Files.createDirectories(includeDir)
            val shimHeaderFile = includeDir.resolve(config.header.getFileName.toString)
            Files.copy(config.header.toAbsolutePath.normalize(), shimHeaderFile, java.nio.file.StandardCopyOption.REPLACE_EXISTING)
            val shimFile = nativeDir.resolve(s"${config.rootModule}_shim.c")
            FileOps.writeString(shimFile, renderShim(config, shims, shimHeaderFile))
            (Some(shimFile), Some(shimHeaderFile))
          }
        Generated(
          flixFile = flixFile,
          shimFile = shimFile,
          shimHeaderFile = shimHeaderFile,
          totalDecls = decls.length,
          generatedDecls = lowered._1.length,
          skipped = lowered._2,
        )
        }
      }
    }
  }

  private def validateConfig(config: Config): Result[Unit, String] = {
    val header = config.header.toAbsolutePath.normalize()
    if (!Files.isRegularFile(header)) {
      return Result.Err(s"Header file does not exist: $header")
    }
    config.spec.map(_.toAbsolutePath.normalize()).foreach { spec =>
      if (!Files.isRegularFile(spec)) {
        return Result.Err(s"Binding spec file does not exist: $spec")
      }
    }
    if (!config.rootModule.matches("[A-Z][A-Za-z0-9_]*")) {
      return Result.Err(s"Invalid root module '${config.rootModule}'. Expected an uppercase Flix identifier.")
    }
    Result.Ok(())
  }

  private def runTranslateC(config: Config): Result[String, String] = {
    val zigCmd = ZigToolchain.usableCommand.getOrElse {
      return Result.Err("A usable Zig command is required for `bind native`. Set FLIX_ZIG_CMD if Zig is managed through a wrapper such as anyzig.")
    }
    val header = config.header.toAbsolutePath.normalize()
    val cflags = nativeCFlags(config)
    val cmd =
      if (cflags.isEmpty) zigCmd ::: List("translate-c", header.toString)
      else zigCmd ::: List("translate-c", "-cflags") ::: cflags ::: List("--", header.toString)
    val (exit, output) = exec(cmd, header.getParent)
    if (exit != 0) {
      Result.Err(
        s"""Failed to translate curated C header '${header}'.
           |
           |Command:
           |  ${cmd.mkString(" ")}
           |
           |Output:
           |$output
           |""".stripMargin
      )
    } else Result.Ok(output)
  }

  private def nativeCFlags(config: Config): List[String] = {
    val includeFlags = config.includePaths.map(_.toAbsolutePath.normalize()).map(p => s"-I${p.toString}")
    val defineFlags = config.defines.map(d => s"-D$d")
    includeFlags ::: defineFlags ::: config.cflags
  }

  private def parseExternDecls(translated: String): List[ZigExtern] = {
    collectExternDeclStrings(translated).flatMap(parseExternDecl)
  }

  private def parseCallbackTypes(translated: String): Map[String, CallbackType] = {
    val Pattern = """(?m)pub const ([A-Za-z_][A-Za-z0-9_]*) = \?\*const fn \((.*?)\) callconv\(\.c\) ([^;]+);""".r
    Pattern.findAllMatchIn(translated).flatMap { m =>
      parseCallbackParamTypes(m.group(2)).map(params => m.group(1) -> CallbackType(m.group(1), params, m.group(3).trim))
    }.toMap
  }

  private def parseCallbackParamTypes(rawParams: String): Option[List[String]] = {
    val trimmed = rawParams.trim
    if (trimmed.isEmpty) Some(Nil)
    else Some(trimmed.split(",").toList.map(_.trim).filter(_.nonEmpty))
  }

  private def collectExternDeclStrings(translated: String): List[String] = {
    val decls = mutable.ListBuffer.empty[String]
    var searchIdx = 0
    val marker = "pub extern fn"
    while (searchIdx >= 0 && searchIdx < translated.length) {
      val start = translated.indexOf(marker, searchIdx)
      if (start < 0) {
        searchIdx = -1
      } else {
        val end = translated.indexOf(';', start)
        if (end < 0) {
          searchIdx = -1
        } else {
          decls += translated.substring(start, end + 1)
          searchIdx = end + 1
        }
      }
    }
    decls.toList
  }

  private def parseExternDecl(decl: String): Option[ZigExtern] = {
    val FnPattern = """(?s)pub extern fn\s+([A-Za-z_][A-Za-z0-9_]*)\s*\((.*?)\)\s*([^;]+);""".r
    decl.trim match {
      case FnPattern(symbol, rawParams, rawResult) =>
        parseParams(rawParams).map(ps => ZigExtern(symbol, ps, rawResult.trim))
      case _ => None
    }
  }

  private def parseParams(rawParams: String): Option[List[(String, String)]] = {
    val trimmed = rawParams.trim
    if (trimmed.isEmpty) {
      Some(Nil)
    } else {
      val parts = trimmed.split(",").toList.map(_.trim).filter(_.nonEmpty)
      Result.traverse(parts.zipWithIndex) {
        case (part, idx) => parseParam(part, idx)
      } match {
        case Result.Ok(params) => Some(params)
        case Result.Err(_) => None
      }
    }
  }

  private def parseParam(part: String, idx: Int): Result[(String, String), Unit] = {
    val ParamPattern = """([A-Za-z_][A-Za-z0-9_]*)\s*:\s*(.+)""".r
    part match {
      case ParamPattern(name, tpe) =>
        Result.Ok((sanitizeParamName(name, idx), tpe.trim))
      case _ =>
        Result.Err(())
    }
  }

  private def lowerDecls(decls: List[ZigExtern], sourceDecls: Map[String, SourceDecl], callbackTypes: Map[String, CallbackType]): (List[FlixBinding], List[Skipped], List[HandleBorrowHelper], List[ResourceCloseHelper], List[ResourceRetainHelper], List[BorrowedOwnerHelper]) = {
    val generated = mutable.ListBuffer.empty[FlixBinding]
    val skipped = mutable.ListBuffer.empty[Skipped]
    val usedNames = mutable.Set.empty[String]
    val (knownHandleTypes, handleSetupSkips) = collectKnownHandleTypes(decls, sourceDecls)
    val skippedSymbols = handleSetupSkips.iterator.map(_.symbol).toSet
    skipped ++= handleSetupSkips

    decls.foreach { decl =>
      if (!skippedSymbols.contains(decl.symbol)) {
        lowerDecl(decl, sourceDecls, callbackTypes, knownHandleTypes, usedNames) match {
          case Left(binding) => generated += binding
          case Right(skip) => skipped += skip
        }
      }
    }

    val borrowHelpers = knownHandleTypes.values.toList.sortBy(_.opaqueBase).flatMap {
      case HandleFamily(_, Some(ownedType), Some(borrowedType)) =>
        val helperName = dedupeName(s"borrow${ownedType}", usedNames)
        Some(HandleBorrowHelper(helperName, ownedType, borrowedType))
      case _ => None
    }

    val ownedHandleTypes = knownHandleTypes.values.flatMap(_.ownedType).toSet
    val closeHelpers = mutable.ListBuffer.empty[ResourceCloseHelper]
    val retainHelpers = mutable.ListBuffer.empty[ResourceRetainHelper]
    val ownerHelpers = mutable.ListBuffer.empty[BorrowedOwnerHelper]
    val closeByHandle = mutable.Map.empty[String, String]
    val retainByHandle = mutable.Map.empty[String, String]
    generated.foreach { binding =>
      sourceDecls.get(binding.symbol).foreach { sourceDecl =>
        sourceDecl.borrowedFromParam.foreach { paramName =>
          val supportsBorrowedFrom = sourceDecl.resultAnnotation.exists {
            case BorrowedHandleResultAnnotation(_) => true
            case BorrowedStringResultAnnotation => true
            case BorrowedBytesResultAnnotation(_) => true
            case _ => false
          }
          if (!supportsBorrowedFrom) {
            skipped += Skipped(binding.symbol, s"`borrowed-from=$paramName` requires a borrowed result annotation")
          } else {
            binding.publicParams.indexWhere(_._1 == paramName) match {
              case -1 =>
                skipped += Skipped(binding.symbol, s"`borrowed-from=$paramName` requires a generated public parameter named '$paramName'")
              case idx =>
                val (_, paramType) = binding.publicParams(idx)
                val family = knownHandleTypes.values.find(f => f.ownedType.contains(paramType) || f.borrowedType.contains(paramType))
                family match {
                  case Some(HandleFamily(_, Some(ownedType), Some(borrowedType))) if paramType == borrowedType =>
                    val helperParams = binding.publicParams.updated(idx, (paramName, ownedType))
                    val targetArgs = binding.publicParams.indices.map { j =>
                      val (name, _) = binding.publicParams(j)
                      if (j == idx) s"""match $paramName { case ${renderHandleCtor(ownedType)}(id) => ${renderHandleCtor(borrowedType)}(id) }"""
                      else name
                    }.toList
                    ownerHelpers += BorrowedOwnerHelper(
                      defName = dedupeName(s"${binding.defName}Owned", usedNames),
                      targetDefName = binding.defName,
                      publicParams = helperParams,
                      targetArgs = targetArgs,
                      publicResult = binding.publicResult,
                      effect = binding.effect,
                    )
                  case Some(_) =>
                    ()
                  case None =>
                    skipped += Skipped(binding.symbol, s"`borrowed-from=$paramName` requires '$paramName' to lower to an owned or borrowed handle parameter")
                }
            }
          }
        }

        sourceDecl.destroyParam match {
          case Some(paramName) =>
            binding.publicParams match {
              case List((onlyName, onlyType)) if onlyName == paramName && ownedHandleTypes.contains(onlyType) && binding.publicResult == "Unit" && binding.effect.contains("IO") =>
                closeByHandle.get(onlyType) match {
                  case Some(existing) if existing != binding.defName =>
                    skipped += Skipped(binding.symbol, s"owned handle type '$onlyType' already has a generated close helper for '${existing}'")
                  case _ =>
                    closeByHandle(onlyType) = binding.defName
                    closeHelpers += ResourceCloseHelper(dedupeName(s"close${onlyType}", usedNames), binding.defName, onlyType)
                }
              case _ =>
                skipped += Skipped(binding.symbol, s"`destroy=$paramName` requires the generated public wrapper to take exactly one owned handle parameter named '$paramName' and return Unit \\ IO")
            }
          case None => ()
        }

        sourceDecl.retainParam match {
          case Some(paramName) =>
            binding.publicParams match {
              case List((onlyName, onlyType)) if onlyName == paramName && binding.effect.contains("IO") =>
                val maybeOwnedType = knownHandleTypes.values.find(_.ownedType.exists(ot => binding.publicResult == s"Option[$ot]")).flatMap(_.ownedType)
                maybeOwnedType match {
                  case Some(ownedType) if onlyType == ownedType || knownHandleTypes.values.exists(f => f.ownedType.contains(ownedType) && f.borrowedType.contains(onlyType)) =>
                    retainByHandle.get(ownedType) match {
                      case Some(existing) if existing != binding.defName =>
                        skipped += Skipped(binding.symbol, s"owned handle type '$ownedType' already has a generated retain helper for '${existing}'")
                      case _ =>
                        retainByHandle(ownedType) = binding.defName
                        retainHelpers += ResourceRetainHelper(dedupeName(s"retain${ownedType}", usedNames), binding.defName, ownedType, onlyType)
                    }
                  case _ =>
                    skipped += Skipped(binding.symbol, s"`retain=$paramName` requires the generated public wrapper to take one owned-or-borrowed handle parameter named '$paramName', return Option[OwnedHandle], and be marked \\ IO")
                }
              case _ =>
                skipped += Skipped(binding.symbol, s"`retain=$paramName` requires the generated public wrapper to take one owned-or-borrowed handle parameter named '$paramName', return Option[OwnedHandle], and be marked \\ IO")
            }
          case None => ()
        }
      }
    }

    (generated.toList, skipped.toList, borrowHelpers, closeHelpers.toList, retainHelpers.toList, ownerHelpers.toList)
  }

  private def collectKnownHandleTypes(decls: List[ZigExtern], sourceDecls: Map[String, SourceDecl]): (Map[String, HandleFamily], List[Skipped]) = {
    val byOpaque = mutable.Map.empty[String, HandleFamily]
    val byFlix = mutable.Map.empty[String, String]
    val skipped = mutable.ListBuffer.empty[Skipped]

    decls.foreach { decl =>
      sourceDecls.get(decl.symbol).flatMap(_.resultAnnotation) match {
        case Some(OwnedHandleResultAnnotation(flixType)) =>
          parseTranslatedOpaquePtrType(decl.result) match {
            case Some(ptr) if ptr.isConst =>
              skipped += Skipped(decl.symbol, s"owned handle result requires a mutable opaque pointer, found '${decl.result}'")
            case Some(ptr) =>
              registerHandleFamilyMember(byOpaque, byFlix, skipped, decl.symbol, ptr.baseName, flixType, isBorrowed = false)
            case None =>
              skipped += Skipped(decl.symbol, s"owned handle result requires an opaque pointer result, found '${decl.result}'")
          }
        case Some(BorrowedHandleResultAnnotation(flixType)) =>
          parseTranslatedOpaquePtrType(decl.result) match {
            case Some(ptr) if !ptr.isConst =>
              skipped += Skipped(decl.symbol, s"borrowed handle result requires a const opaque pointer, found '${decl.result}'")
            case Some(ptr) =>
              registerHandleFamilyMember(byOpaque, byFlix, skipped, decl.symbol, ptr.baseName, flixType, isBorrowed = true)
            case None =>
              skipped += Skipped(decl.symbol, s"borrowed handle result requires an opaque pointer result, found '${decl.result}'")
          }
        case Some(StatusOwnedHandleResultAnnotation(flixType, outParam, _)) =>
          val sourceDecl = sourceDecls(decl.symbol)
          val outIdx = sourceDecl.params.indexWhere(raw => sourceParamName(raw).contains(outParam))
          if (outIdx < 0 || outIdx >= decl.params.length) {
            skipped += Skipped(decl.symbol, s"status-owned-handle result requires a named opaque out-parameter '$outParam'")
          } else {
            parseTranslatedOpaqueOutPtrType(decl.params(outIdx)._2) match {
              case Some(ptr) if ptr.isConst =>
                skipped += Skipped(decl.symbol, s"status-owned-handle result requires a mutable opaque out-parameter, found '${decl.params(outIdx)._2}'")
              case Some(ptr) =>
                registerHandleFamilyMember(byOpaque, byFlix, skipped, decl.symbol, ptr.baseName, flixType, isBorrowed = false)
              case None =>
                skipped += Skipped(decl.symbol, s"status-owned-handle result requires an opaque out-parameter, found '${decl.params(outIdx)._2}'")
            }
          }
        case _ => ()
      }
    }

    (byOpaque.toMap, skipped.toList)
  }

  private def registerHandleFamilyMember(byOpaque: mutable.Map[String, HandleFamily],
                                         byFlix: mutable.Map[String, String],
                                         skipped: mutable.ListBuffer[Skipped],
                                         symbol: String,
                                         opaqueBase: String,
                                         flixType: String,
                                         isBorrowed: Boolean): Unit = {
    byFlix.get(flixType) match {
      case Some(existingOpaque) if existingOpaque != opaqueBase =>
        skipped += Skipped(symbol, s"Flix handle type '$flixType' is already used for opaque C type '$existingOpaque'")
        return
      case _ => ()
    }

    val current = byOpaque.getOrElse(opaqueBase, HandleFamily(opaqueBase, None, None))
    val conflictingSameBase =
      if (isBorrowed) current.ownedType.contains(flixType) || current.borrowedType.exists(_ != flixType)
      else current.borrowedType.contains(flixType) || current.ownedType.exists(_ != flixType)
    if (conflictingSameBase) {
      val existing = if (isBorrowed) current.borrowedType.orElse(current.ownedType) else current.ownedType.orElse(current.borrowedType)
      skipped += Skipped(symbol, s"opaque handle type '$opaqueBase' already has an incompatible Flix handle mapping '${existing.getOrElse("?")}'")
      return
    }

    val updated =
      if (isBorrowed) current.copy(borrowedType = Some(flixType))
      else current.copy(ownedType = Some(flixType))
    byOpaque(opaqueBase) = updated
    byFlix(flixType) = opaqueBase
  }

  private def lowerDecl(decl: ZigExtern, sourceDecls: Map[String, SourceDecl], callbackTypes: Map[String, CallbackType], knownHandleTypes: Map[String, HandleFamily], usedNames: mutable.Set[String]): Either[FlixBinding, Skipped] = {
    if (decl.params.exists(_._2.contains("..."))) {
      return Right(Skipped(decl.symbol, "variadic declarations are not supported"))
    }

    val sourceDecl = sourceDecls.getOrElse(decl.symbol, SourceDecl(List.fill(decl.params.length)(""), None, None, None, None, None, None, None))
    sourceDecl.annotationError.foreach(msg => return Right(Skipped(decl.symbol, msg)))
    val sourceParams = sourceDecl.params
    val loweredParams = lowerParams(decl.params, sourceParams, sourceDecl.resultAnnotation, sourceDecl.callback, callbackTypes, knownHandleTypes)
    val loweredResult = loweredParams.flatMap(lp => lowerResult(decl.result, sourceDecl.resultAnnotation, lp.hiddenResultLen, lp.hiddenOutHandle))

    (loweredParams, loweredResult) match {
      case (Result.Ok(params), Result.Ok(result)) =>
        val baseName = toFlixDefBaseName(decl.symbol)
        val defName = dedupeName(baseName, usedNames)
        val rawName = dedupeName(s"ffiRaw${upperFirst(defName)}", usedNames)
        val usesShim = params.publicParams.exists {
          case _: CStringParam => true
          case _: BytesParam => true
          case _: HandleParam => true
          case _ => false
        } || params.callArgs.exists {
          case _: CallbackCallArg => true
          case _: ResultHandleOutCallArg => true
          case _ => false
        } || result.isInstanceOf[StringResult] || result.isInstanceOf[BytesResult] || result.isInstanceOf[HandleResult] || result.isInstanceOf[HandleStatusResult]
        val targetSymbol =
          if (usesShim) s"flix_bind_native_shim_${decl.symbol}"
          else decl.symbol
        val publicParams = params.publicParams.flatMap(_.publicParams)
        val rawParams = params.publicParams.flatMap {
          case ScalarParam(name, flixType, _) => List((name, flixType))
          case CStringParam(name) => List((name, "String"))
          case BytesParam(name, _, _) => List((name, "Array[Int8, Static]"))
          case HandleParam(name, _, _) => List((name, "Int64"))
        }
        val rawCallExpr = s"${rawName}(${params.publicParams.map(renderWrapperArgExpr).mkString(", ")})"
        val wrapperExpr = result match {
          case ScalarResult(_, _) => rawCallExpr
          case StringResult(_, _) => rawCallExpr
          case BytesResult(_, _) => rawCallExpr
          case HandleResult(flixType, _) =>
            s"""let rawResult = ${rawCallExpr};
               |if (rawResult == 0i64) None else Some(${renderHandleCtor(flixType)}(rawResult))""".stripMargin
          case HandleStatusResult(_, _, flixType, _, _) =>
            s"""match ${rawCallExpr} {
               |    case Ok(rawHandle) => Ok(${renderHandleCtor(flixType)}(rawHandle))
               |    case Err(status) => Err(status)
               |}""".stripMargin
        }
        val handleTypes =
          params.publicParams.collect { case HandleParam(_, flixType, _) => flixType }.toSet ++
            (result match {
              case HandleResult(flixType, _) => Set(flixType)
              case HandleStatusResult(_, _, flixType, _, _) => Set(flixType)
              case _ => Set.empty[String]
            })
        val binding = FlixBinding(
          symbol = decl.symbol,
          targetSymbol = targetSymbol,
          defName = defName,
          rawName = rawName,
          publicParams = publicParams,
          rawParams = rawParams,
          publicResult = result.publicResult,
          rawResult = result match {
            case ScalarResult(flixType, _) => flixType
            case StringResult(_, _) => "String"
            case BytesResult(_, _) => "Array[Int8, Static]"
            case HandleResult(_, _) => "Int64"
            case HandleStatusResult(statusFlixType, _, _, _, _) => s"Result[$statusFlixType, Int64]"
          },
          effect = sourceDecl.effect,
          wrapperExpr = wrapperExpr,
          handleTypes = handleTypes,
          shim = if (usesShim) Some(NativeShim(targetSymbol, decl.symbol, result, params.publicParams, params.callArgs)) else None,
        )
        Left(binding)
      case _ =>
        val firstUnsupportedParam = loweredParams match {
          case Result.Err(reason) => Some(reason)
          case _ => None
        }
        val reason = firstUnsupportedParam.getOrElse(loweredResult match {
          case Result.Err(msg) => msg
          case _ => s"unsupported result type '${decl.result}'"
        })
        Right(Skipped(decl.symbol, reason))
    }
  }

  private def lowerParams(params: List[(String, String)],
                          sourceParams: List[String],
                          resultAnnotation: Option[ResultAnnotation],
                          callback: Option[CallbackAnnotation],
                          callbackTypes: Map[String, CallbackType],
                          knownHandleTypes: Map[String, HandleFamily]): Result[LoweredParams, String] = {
    val sourceShapes = sourceParams.map(classifySourceParam)
    val publicParams = mutable.ListBuffer.empty[LoweredParam]
    val callArgs = mutable.ListBuffer.empty[ShimCallArg]
    val hiddenResultLenParamName = resultAnnotation.flatMap {
      case BorrowedBytesResultAnnotation(lenParam) => Some(lenParam)
      case OwnedBytesResultAnnotation(lenParam, _) => Some(lenParam)
      case _ => None
    }
    val hiddenOutHandleParamName = resultAnnotation.flatMap {
      case StatusOwnedHandleResultAnnotation(_, outParam, _) => Some(outParam)
      case _ => None
    }
    var hiddenResultLen: Option[HiddenResultLenParam] = None
    var hiddenOutHandle: Option[HiddenOutHandleParam] = None
    var idx = 0
    while (idx < params.length) {
      val (name, zigType) = params(idx)
      val sourceParam = sourceParams.lift(idx).getOrElse("")
      val sourceName = sourceParamName(sourceParam)
      val matchesAnnotatedCallback = callback.exists(ann => ann.paramName == name || sourceName.contains(ann.paramName))
      if (matchesAnnotatedCallback) {
        callback.flatMap { ann =>
          callbackTypes.get(zigType.trim).map(cb => (ann, cb))
        } match {
          case Some((ann, cbType)) =>
            validateCallbackType(cbType) match {
              case Result.Ok(()) =>
                callArgs += CallbackCallArg(name, renderExportSymbol(ann.exportDef), cbType)
                idx += 1
              case Result.Err(reason) =>
                return Result.Err(reason)
            }
          case None =>
            return Result.Err(s"annotated callback parameter '$name' requires a supported translated C callback typedef, found '$zigType'")
        }
      } else
      sourceShapes.lift(idx).getOrElse(SourceParamOther) match {
        case SourceLengthOut(lenCType) if hiddenResultLenParamName.contains(sourceName.getOrElse("")) =>
          if (!isTranslatedLengthOutPtr(zigType, lenCType)) {
            return Result.Err(s"annotated bytes result length parameter '$name' requires translated type for `${lenCType}*`, found '$zigType'")
          }
          val hidden = HiddenResultLenParam(name, lenCType)
          hiddenResultLen = Some(hidden)
          callArgs += ResultLenOutCallArg(hidden)
          idx += 1
        case _ if hiddenOutHandleParamName.contains(sourceName.getOrElse("")) =>
          resultAnnotation match {
            case Some(StatusOwnedHandleResultAnnotation(flixType, _, _)) =>
              parseTranslatedOpaqueOutPtrType(zigType) match {
                case Some(ptr) =>
                  knownHandleTypes.get(ptr.baseName).flatMap(_.ownedType) match {
                    case Some(`flixType`) =>
                      val hidden = HiddenOutHandleParam(name, ptr.cType, flixType)
                      hiddenOutHandle = Some(hidden)
                      callArgs += ResultHandleOutCallArg(hidden)
                      idx += 1
                    case Some(otherType) =>
                      return Result.Err(s"annotated out-handle parameter '$name' expects owned handle type '$flixType', but opaque C type '${ptr.baseName}' is already mapped to '$otherType'")
                    case None =>
                      return Result.Err(s"annotated out-handle parameter '$name' requires a declared owned handle type '$flixType' for opaque C type '${ptr.baseName}'")
                  }
                case None =>
                  return Result.Err(s"annotated out-handle parameter '$name' requires translated type for `${flixType}**`-style opaque out-parameter, found '$zigType'")
              }
            case _ =>
              return Result.Err(s"annotated out-handle parameter '$name' requires `result=status-owned-handle ...`")
          }
        case SourceCStringBorrowed =>
          if (!isTranslatedConstBytePtr(zigType)) {
            return Result.Err(s"unsupported string parameter type '$zigType'")
          }
          publicParams += CStringParam(name)
          callArgs += CStringCallArg(name)
          idx += 1
        case SourceBytesBorrowed(ptrCType) =>
          if (!isTranslatedConstBytePtr(zigType)) {
            return Result.Err(s"unsupported byte-slice pointer parameter type '$zigType'")
          }
          if (idx + 1 >= params.length) {
            return Result.Err(s"byte-slice pointer parameter '$name' is missing a following length parameter")
          }
          sourceShapes(idx + 1) match {
            case SourceLength(lenCType) =>
              publicParams += BytesParam(name, ptrCType, lenCType)
              callArgs += BytesCallArg(name, ptrCType, lenCType)
              idx += 2
            case _ =>
              return Result.Err(s"byte-slice pointer parameter '$name' is missing a supported following length parameter")
          }
        case _ =>
          parseTranslatedOpaquePtrType(zigType) match {
            case Some(ptr) =>
              knownHandleTypes.get(ptr.baseName) match {
                case Some(family) =>
                  val resolvedType =
                    if (ptr.isConst) family.borrowedType.orElse(family.ownedType)
                    else family.ownedType
                  resolvedType match {
                    case Some(flixType) =>
                      publicParams += HandleParam(name, flixType, ptr.cType)
                      callArgs += HandleCallArg(name, ptr.cType)
                      idx += 1
                    case None =>
                      return Result.Err(s"mutable opaque pointer parameter '$zigType' requires an owned handle type annotation for opaque C type '${ptr.baseName}'")
                  }
                case None =>
                  mapZigTypeToFlixParamType(zigType) match {
                    case Result.Ok(ft) =>
                      publicParams += ScalarParam(name, ft, zigType)
                      callArgs += ScalarCallArg(name)
                      idx += 1
                    case Result.Err(_) =>
                      return Result.Err(pointerParamReason(zigType).getOrElse(s"unsupported parameter type '$zigType'"))
                  }
              }
            case None =>
              mapZigTypeToFlixParamType(zigType) match {
                case Result.Ok(ft) =>
                  publicParams += ScalarParam(name, ft, zigType)
                  callArgs += ScalarCallArg(name)
                  idx += 1
                case Result.Err(_) =>
                  return Result.Err(pointerParamReason(zigType).getOrElse(s"unsupported parameter type '$zigType'"))
              }
          }
      }
    }
    callback.foreach { ann =>
      val matched = params.indices.exists { i =>
        val translatedName = params(i)._1
        val sourceName = sourceParams.lift(i).flatMap(sourceParamName)
        ann.paramName == translatedName || sourceName.contains(ann.paramName)
      }
      if (!matched) {
        return Result.Err(s"annotated callback parameter '${ann.paramName}' was not found in the declaration")
      }
    }
    if (hiddenResultLenParamName.nonEmpty && hiddenResultLen.isEmpty) {
      Result.Err(s"annotated bytes result requires a named supported length out-parameter `${hiddenResultLenParamName.get}`")
    } else if (hiddenOutHandleParamName.nonEmpty && hiddenOutHandle.isEmpty) {
      Result.Err(s"annotated status-owned-handle result requires a named supported opaque out-parameter `${hiddenOutHandleParamName.get}`")
    } else {
      Result.Ok(LoweredParams(publicParams.toList, callArgs.toList, hiddenResultLen, hiddenOutHandle))
    }
  }

  private def mapZigTypeToFlixParamType(tpe0: String): Result[String, Unit] = {
    mapZigTypeToFlixType(tpe0).filter(_ != "Unit").map(Result.Ok(_)).getOrElse(Result.Err(()))
  }

  private def mapZigTypeToFlixResultType(tpe0: String): Option[String] = {
    mapZigTypeToFlixType(tpe0)
  }

  private def lowerResult(tpe0: String, annotation: Option[ResultAnnotation], hiddenResultLen: Option[HiddenResultLenParam], hiddenOutHandle: Option[HiddenOutHandleParam]): Result[LoweredResult, String] = annotation match {
    case Some(BorrowedStringResultAnnotation) =>
      mapZigStringResultType(tpe0, allowConst = true) match {
        case Some(cType) => Result.Ok(StringResult(cType, BorrowedStringOwnership))
        case None => Result.Err(s"annotated borrowed string result requires `char*` or `const char*`, found '$tpe0'")
      }
    case Some(OwnedStringResultAnnotation(freeFn)) =>
      mapZigStringResultType(tpe0, allowConst = false) match {
        case Some(cType) => Result.Ok(StringResult(cType, OwnedStringOwnership(freeFn)))
        case None => Result.Err(s"annotated owned string result requires mutable `char*`, found '$tpe0'")
      }
    case Some(BorrowedBytesResultAnnotation(_)) =>
      hiddenResultLen match {
        case Some(lenParam) =>
          mapZigBytesResultType(tpe0, allowConst = true) match {
            case Some(_) => Result.Ok(BytesResult(lenParam, BorrowedBytesOwnership))
            case None => Result.Err(s"annotated borrowed bytes result requires `uint8_t*` / `int8_t*` / `unsigned char*` pointer shape, found '$tpe0'")
          }
        case None =>
          Result.Err("annotated bytes result is missing a supported length out-parameter")
      }
    case Some(OwnedBytesResultAnnotation(_, freeFn)) =>
      hiddenResultLen match {
        case Some(lenParam) =>
          mapZigBytesResultType(tpe0, allowConst = false) match {
            case Some(_) => Result.Ok(BytesResult(lenParam, OwnedBytesOwnership(freeFn)))
            case None => Result.Err(s"annotated owned bytes result requires mutable `uint8_t*` / `int8_t*` / `unsigned char*` pointer shape, found '$tpe0'")
          }
        case None =>
          Result.Err("annotated bytes result is missing a supported length out-parameter")
      }
    case Some(OwnedHandleResultAnnotation(flixType)) =>
      parseTranslatedOpaquePtrType(tpe0) match {
        case Some(ptr) if ptr.isConst =>
          Result.Err(s"annotated owned handle result requires a mutable opaque pointer, found '$tpe0'")
        case Some(ptr) =>
          Result.Ok(HandleResult(flixType, ptr.cType))
        case None =>
          Result.Err(s"annotated owned handle result requires an opaque pointer result, found '$tpe0'")
      }
    case Some(BorrowedHandleResultAnnotation(flixType)) =>
      parseTranslatedOpaquePtrType(tpe0) match {
        case Some(ptr) if !ptr.isConst =>
          Result.Err(s"annotated borrowed handle result requires a const opaque pointer, found '$tpe0'")
        case Some(ptr) =>
          Result.Ok(HandleResult(flixType, ptr.cType))
        case None =>
          Result.Err(s"annotated borrowed handle result requires an opaque pointer result, found '$tpe0'")
      }
    case Some(StatusOwnedHandleResultAnnotation(flixType, _, okValue)) =>
      hiddenOutHandle match {
        case Some(outHandle) if outHandle.flixType == flixType =>
          (mapZigTypeToFlixResultType(tpe0), mapZigTypeToCType(tpe0)) match {
            case (Some(statusFlixType @ ("Bool" | "Int8" | "Int16" | "Int32" | "Int64")), Some(statusCType)) =>
              Result.Ok(HandleStatusResult(statusFlixType, statusCType, flixType, outHandle, okValue))
            case (Some(otherType), _) =>
              Result.Err(s"annotated status-owned-handle result requires a Bool/Int8/Int16/Int32/Int64 status result, found '$otherType'")
            case _ =>
              Result.Err(s"annotated status-owned-handle result requires a scalar status result, found '$tpe0'")
          }
        case Some(otherOut) =>
          Result.Err(s"annotated status-owned-handle result expects out-handle type '$flixType', found '${otherOut.flixType}'")
        case None =>
          Result.Err("annotated status-owned-handle result is missing a supported opaque out-parameter")
      }
    case None =>
      (mapZigTypeToFlixResultType(tpe0), mapZigTypeToCType(tpe0)) match {
        case (Some(flixType), Some(cType)) => Result.Ok(ScalarResult(flixType, cType))
        case _ => Result.Err(pointerResultReason(tpe0).getOrElse(s"unsupported result type '$tpe0'"))
      }
  }

  private def isUnsupportedParamType(tpe: String): Boolean = mapZigTypeToFlixParamType(tpe) match {
    case Result.Err(_) => true
    case _ => false
  }

  private def mapZigTypeToFlixType(tpe0: String): Option[String] = {
    val tpe = tpe0.trim
    tpe match {
      case "void" => Some("Unit")
      case "bool" => Some("Bool")
      case "i8" | "c_char" => Some("Int8")
      case "i16" | "c_short" => Some("Int16")
      case "i32" | "c_int" => Some("Int32")
      case "i64" | "c_longlong" => Some("Int64")
      case "f32" => Some("Float32")
      case "f64" => Some("Float64")
      case _ => None
    }
  }

  private def mapZigTypeToCType(tpe0: String): Option[String] = {
    val tpe = tpe0.trim
    tpe match {
      case "void" => Some("void")
      case "bool" => Some("bool")
      case "i8" | "c_char" => Some("int8_t")
      case "i16" | "c_short" => Some("int16_t")
      case "i32" | "c_int" => Some("int32_t")
      case "i64" | "c_longlong" => Some("int64_t")
      case "f32" => Some("float")
      case "f64" => Some("double")
      case _ => None
    }
  }

  private def validateCallbackType(callbackType: CallbackType): Result[Unit, String] = {
    val unsupportedParam = callbackType.params.find { tpe =>
      mapZigTypeToFlixParamType(tpe) match {
        case Result.Ok(_) => false
        case Result.Err(_) => true
      }
    }
    unsupportedParam match {
      case Some(tpe) =>
        Result.Err(s"callback typedef '${callbackType.alias}' uses unsupported parameter type '$tpe'; synchronous callback bindings currently support only scalar/unit signatures")
      case None =>
        mapZigTypeToFlixResultType(callbackType.result) match {
          case Some(_) => Result.Ok(())
          case None => Result.Err(s"callback typedef '${callbackType.alias}' uses unsupported result type '${callbackType.result}'; synchronous callback bindings currently support only scalar/unit signatures")
        }
    }
  }

  private def pointerParamReason(tpe0: String): Option[String] = {
    val tpe = tpe0.trim
    if (isTranslatedConstBytePtr(tpe)) Some(s"raw pointer parameter '$tpe' requires an explicit borrowed string/byte pattern")
    else if (parseTranslatedOpaquePtrType(tpe).nonEmpty) Some(s"opaque pointer parameter '$tpe' requires a declared handle type from an annotated owned or borrowed handle result")
    else None
  }

  private def pointerResultReason(tpe0: String): Option[String] = {
    val tpe = tpe0.trim
    if (isTranslatedStringPtr(tpe) || isTranslatedConstBytePtr(tpe)) Some(s"raw pointer result '$tpe' requires an explicit ownership policy")
    else if (parseTranslatedOpaquePtrType(tpe).nonEmpty) Some(s"opaque pointer result '$tpe' requires an explicit handle ownership annotation")
    else None
  }

  private def isTranslatedConstBytePtr(tpe0: String): Boolean = {
    val tpe = tpe0.trim.replace(" ", "")
    tpe == "[*c]constu8" || tpe == "[*c]consti8"
  }

  private def isTranslatedStringPtr(tpe0: String): Boolean = {
    val tpe = tpe0.trim.replace(" ", "")
    tpe == "[*c]u8" || tpe == "[*c]constu8"
  }

  private def isTranslatedLengthOutPtr(tpe0: String, cType: String): Boolean = {
    val tpe = tpe0.trim.replace(" ", "")
    cType match {
      case "size_t" | "uintptr_t" => tpe == "[*c]usize"
      case "intptr_t" | "ssize_t" => tpe == "[*c]isize"
      case "int64_t" => tpe == "[*c]i64"
      case "uint64_t" => tpe == "[*c]u64"
      case _ => false
    }
  }

  private def parseTranslatedOpaquePtrType(tpe0: String): Option[TranslatedOpaquePtr] = {
    val tpe = tpe0.trim.replace(" ", "")
    val ConstPattern = """\?\*const([A-Za-z_][A-Za-z0-9_]*)""".r
    val MutPattern = """\?\*([A-Za-z_][A-Za-z0-9_]*)""".r
    tpe match {
      case ConstPattern(baseName) => Some(TranslatedOpaquePtr(baseName, s"const ${baseName}*", isConst = true))
      case MutPattern(baseName) => Some(TranslatedOpaquePtr(baseName, s"${baseName}*", isConst = false))
      case _ => None
    }
  }

  private def parseTranslatedOpaqueOutPtrType(tpe0: String): Option[TranslatedOpaquePtr] = {
    val tpe = tpe0.trim.replace(" ", "")
    val OutPattern = """\[\*c\](\?\*const[A-Za-z_][A-Za-z0-9_]*|\?\*[A-Za-z_][A-Za-z0-9_]*)""".r
    tpe match {
      case OutPattern(inner) => parseTranslatedOpaquePtrType(inner)
      case _ => None
    }
  }

  private def mapZigStringResultType(tpe0: String, allowConst: Boolean): Option[String] = {
    val tpe = tpe0.trim.replace(" ", "")
    tpe match {
      case "[*c]u8" => Some("char*")
      case "[*c]constu8" if allowConst => Some("const char*")
      case _ => None
    }
  }

  private def mapZigBytesResultType(tpe0: String, allowConst: Boolean): Option[String] = {
    val tpe = tpe0.trim.replace(" ", "")
    tpe match {
      case "[*c]u8" | "[*c]i8" => Some("void*")
      case "[*c]constu8" | "[*c]consti8" if allowConst => Some("const void*")
      case _ => None
    }
  }

  private def splitSourceParams(rawParams: String): List[String] = {
    val trimmed = rawParams.trim
    if (trimmed.isEmpty || trimmed == "void") Nil
    else trimmed.split(",").toList.map(_.trim).filter(_.nonEmpty)
  }

  private def parseHeaderDecls(headerSource: String): Map[String, SourceDecl] = {
    val result = mutable.Map.empty[String, SourceDecl]
    val currentDecl = new StringBuilder
    var collectingDecl = false

    def flushDecl(): Unit = {
      val decl = currentDecl.toString()
      currentDecl.clear()
      collectingDecl = false
      val fnPattern = """(?s)\b([A-Za-z_][A-Za-z0-9_]*)\s*\((.*?)\)\s*;""".r
      fnPattern.findFirstMatchIn(decl) match {
        case Some(m) =>
          val symbol = m.group(1)
          val rawParams = m.group(2)
          result(symbol) = SourceDecl(splitSourceParams(rawParams), None, None, None, None, None, None, None)
        case _ => // ignore non-function declarations in curated headers
      }
    }

    headerSource.linesIterator.foreach { line =>
      val trimmed = line.trim
      if (collectingDecl) {
        currentDecl.append(line).append('\n')
        if (trimmed.contains(";")) flushDecl()
      } else if (trimmed.isEmpty || trimmed.startsWith("//")) {
        ()
      } else if (trimmed.contains("(")) {
        currentDecl.append(line).append('\n')
        if (trimmed.contains(";")) flushDecl() else collectingDecl = true
      }
    }

    result.toMap
  }

  private def parseBindingSpec(spec: Option[Path], headerSymbols: Set[String]): Result[Map[String, SourceAnnotation], String] = spec match {
    case None => Result.Ok(Map.empty)
    case Some(specPath0) =>
      val specPath = specPath0.toAbsolutePath.normalize()
      val parser =
        try Toml.parse(specPath)
        catch {
          case e: IOException => return Result.Err(s"Failed to read binding spec '$specPath': ${e.getMessage}")
        }

      if (!parser.errors().isEmpty) {
        val msg = parser.errors().asScala.map(_.toString).mkString("; ")
        return Result.Err(s"Failed to parse binding spec '$specPath': $msg")
      }

      val bindings = Option(parser.getArray("binding")) match {
        case Some(array) => List.tabulate(array.size())(array.get)
        case None => Nil
      }
      Result.traverse(bindings.zipWithIndex) {
        case (rawTable, idx) =>
          rawTable match {
            case table: TomlTable => parseBindingSpecTable(table, idx, specPath)
            case _ => Result.Err(s"Binding spec '$specPath' entry #${idx + 1} must be a TOML table")
          }
      }.flatMap { entries =>
        val duplicates = entries.groupBy(_._1).collectFirst { case (symbol, xs) if xs.size > 1 => symbol }
        duplicates match {
          case Some(symbol) => Result.Err(s"Binding spec '$specPath' declares symbol '$symbol' more than once")
          case None =>
            val unknown = entries.collectFirst { case (symbol, _) if !headerSymbols.contains(symbol) => symbol }
            unknown match {
              case Some(symbol) => Result.Err(s"Binding spec '$specPath' declares symbol '$symbol' which does not exist in the curated header")
              case None => Result.Ok(entries.toMap)
            }
        }
      }
  }

  private def parseBindingSpecTable(table: TomlTable, idx: Int, specPath: Path): Result[(String, SourceAnnotation), String] = {
    val allowedKeys = Set("symbol", "result", "free", "len", "type", "out", "ok", "callback", "callback-export", "borrowed-from", "destroy", "retain", "effect")
    val unknownKeys = table.keySet().asScala.toSet.diff(allowedKeys)
    if (unknownKeys.nonEmpty) {
      return Result.Err(s"Binding spec '$specPath' entry #${idx + 1} contains unsupported keys: ${unknownKeys.toList.sorted.mkString(", ")}")
    }

    for {
      symbol <- getRequiredBindingSpecString(table, "symbol", specPath, idx)
      fieldPairs <- Result.traverse(allowedKeys.toList.sorted.filterNot(_ == "symbol")) { key =>
        getOptionalBindingSpecString(table, key, specPath, idx).map(_.map(value => key -> value))
      }
      annotation <- parseSourceAnnotationFields(fieldPairs.flatten.toMap)
    } yield symbol -> annotation
  }

  private def getRequiredBindingSpecString(table: TomlTable, key: String, specPath: Path, idx: Int): Result[String, String] =
    getOptionalBindingSpecString(table, key, specPath, idx).flatMap {
      case Some(value) => Result.Ok(value)
      case None => Result.Err(s"Binding spec '$specPath' entry #${idx + 1} is missing required key '$key'")
    }

  private def getOptionalBindingSpecString(table: TomlTable, key: String, specPath: Path, idx: Int): Result[Option[String], String] =
    try {
      Result.Ok(Option(table.getString(key)))
    } catch {
      case _: IllegalArgumentException => Result.Ok(None)
      case e: Exception => Result.Err(s"Binding spec '$specPath' entry #${idx + 1} key '$key' must be a string: ${e.getMessage}")
    }

  private def mergeSourceDecls(headerDecls: Map[String, SourceDecl], annotations: Map[String, SourceAnnotation]): Map[String, SourceDecl] =
    headerDecls.map {
      case (symbol, decl) =>
        val merged = annotations.get(symbol) match {
          case Some(ann) =>
            decl.copy(
              resultAnnotation = ann.resultAnnotation,
              callback = ann.callback,
              borrowedFromParam = ann.borrowedFromParam,
              destroyParam = ann.destroyParam,
              retainParam = ann.retainParam,
              effect = ann.effect,
              annotationError = None,
            )
          case None => decl
        }
        symbol -> merged
    }

  private def parseSourceAnnotationFields(map: Map[String, String]): Result[SourceAnnotation, String] = {
    val resultAnn = map.get("result") match {
        case Some("borrowed-string") =>
          Result.Ok(Some(BorrowedStringResultAnnotation))
        case Some("owned-string") =>
          map.get("free") match {
            case Some(freeFn) if freeFn.nonEmpty => Result.Ok(Some(OwnedStringResultAnnotation(freeFn)))
            case _ => Result.Err("`result=owned-string` requires `free=<symbol>`")
          }
        case Some("borrowed-bytes") =>
          map.get("len") match {
            case Some(lenParam) if lenParam.nonEmpty => Result.Ok(Some(BorrowedBytesResultAnnotation(lenParam)))
            case _ => Result.Err("`result=borrowed-bytes` requires `len=<param>`")
          }
        case Some("owned-bytes") =>
          (map.get("len"), map.get("free")) match {
            case (Some(lenParam), Some(freeFn)) if lenParam.nonEmpty && freeFn.nonEmpty =>
              Result.Ok(Some(OwnedBytesResultAnnotation(lenParam, freeFn)))
            case _ =>
              Result.Err("`result=owned-bytes` requires `len=<param>` and `free=<symbol>`")
          }
        case Some("owned-handle") =>
          map.get("type") match {
            case Some(flixType) if flixType.matches("[A-Z][A-Za-z0-9_]*") =>
              Result.Ok(Some(OwnedHandleResultAnnotation(flixType)))
            case Some(flixType) =>
              Result.Err(s"`result=owned-handle` requires `type=<UpperCamelCase>`; found '$flixType'")
            case None =>
              Result.Err("`result=owned-handle` requires `type=<FlixHandleType>`")
          }
        case Some("borrowed-handle") =>
          map.get("type") match {
            case Some(flixType) if flixType.matches("[A-Z][A-Za-z0-9_]*") =>
              Result.Ok(Some(BorrowedHandleResultAnnotation(flixType)))
            case Some(flixType) =>
              Result.Err(s"`result=borrowed-handle` requires `type=<UpperCamelCase>`; found '$flixType'")
            case None =>
              Result.Err("`result=borrowed-handle` requires `type=<FlixHandleType>`")
          }
        case Some("status-owned-handle") =>
          (map.get("type"), map.get("out")) match {
            case (Some(flixType), Some(outParam)) if flixType.matches("[A-Z][A-Za-z0-9_]*") && outParam.nonEmpty =>
              Result.Ok(Some(StatusOwnedHandleResultAnnotation(flixType, outParam, map.getOrElse("ok", "0"))))
            case (Some(flixType), Some(_)) =>
              Result.Err(s"`result=status-owned-handle` requires `type=<UpperCamelCase>`; found '$flixType'")
            case _ =>
              Result.Err("`result=status-owned-handle` requires `type=<FlixHandleType>` and `out=<param>`")
          }
        case Some(other) =>
          Result.Err(s"unsupported binding spec result annotation '$other'")
        case None =>
          Result.Ok(None)
      }

      val effect = map.get("effect") match {
        case Some("IO") => Result.Ok(Some("IO"))
        case Some(other) => Result.Err(s"unsupported binding spec effect annotation '$other'")
        case None => Result.Ok(None)
      }

      val callback = (map.get("callback"), map.get("callback-export")) match {
        case (None, None) => Result.Ok(None)
        case (Some(paramName), Some(exportDef)) if paramName.nonEmpty && isCallbackExportName(exportDef) =>
          Result.Ok(Some(CallbackAnnotation(paramName, exportDef)))
        case (Some(_), Some(exportDef)) =>
          Result.Err(s"`callback-export` requires a dotted exported def name such as `Api.onStep`; found '$exportDef'")
        case _ =>
          Result.Err("callback bindings require both `callback=<param>` and `callback-export=<Module.def>`")
      }

      val borrowedFrom = map.get("borrowed-from") match {
        case Some(paramName) if paramName.nonEmpty => Result.Ok(Some(paramName))
        case Some(_) => Result.Err("`borrowed-from=<param>` requires a non-empty parameter name")
        case None => Result.Ok(None)
      }

      val destroyParam = map.get("destroy") match {
        case Some(paramName) if paramName.nonEmpty => Result.Ok(Some(paramName))
        case Some(_) => Result.Err("`destroy=<param>` requires a non-empty parameter name")
        case None => Result.Ok(None)
      }

      val retainParam = map.get("retain") match {
        case Some(paramName) if paramName.nonEmpty => Result.Ok(Some(paramName))
        case Some(_) => Result.Err("`retain=<param>` requires a non-empty parameter name")
        case None => Result.Ok(None)
      }

      for {
        resultAnnotation <- resultAnn
        callbackAnnotation <- callback
        borrowedFromAnnotation <- borrowedFrom
        destroyAnnotation <- destroyParam
        retainAnnotation <- retainParam
        effectRow <- effect
        _ <- if (resultAnnotation.nonEmpty || callbackAnnotation.nonEmpty || borrowedFromAnnotation.nonEmpty || destroyAnnotation.nonEmpty || retainAnnotation.nonEmpty || effectRow.nonEmpty) Result.Ok(()) else Result.Err("binding spec entry requires at least one supported field such as `result`, `callback`, `borrowed-from`, `destroy`, `retain`, or `effect`")
      } yield SourceAnnotation(resultAnnotation, callbackAnnotation, borrowedFromAnnotation, destroyAnnotation, retainAnnotation, effectRow)
  }

  private def classifySourceParam(raw: String): SourceParamShape = {
    val withoutName = raw.replaceAll("""\b[A-Za-z_][A-Za-z0-9_]*\s*$""", "").trim
    val compact = withoutName.replaceAll("\\s+", "")
    compact match {
      case "constchar*" | "charconst*" => SourceCStringBorrowed
      case "constuint8_t*" | "uint8_tconst*" => SourceBytesBorrowed("const uint8_t*")
      case "constint8_t*" | "int8_tconst*" => SourceBytesBorrowed("const int8_t*")
      case "constunsignedchar*" | "unsignedcharconst*" => SourceBytesBorrowed("const unsigned char*")
      case "size_t*" => SourceLengthOut("size_t")
      case "int64_t*" => SourceLengthOut("int64_t")
      case "uint64_t*" => SourceLengthOut("uint64_t")
      case "intptr_t*" => SourceLengthOut("intptr_t")
      case "uintptr_t*" => SourceLengthOut("uintptr_t")
      case "ssize_t*" => SourceLengthOut("ssize_t")
      case "size_t" => SourceLength("size_t")
      case "int64_t" => SourceLength("int64_t")
      case "uint64_t" => SourceLength("uint64_t")
      case "intptr_t" => SourceLength("intptr_t")
      case "uintptr_t" => SourceLength("uintptr_t")
      case "ssize_t" => SourceLength("ssize_t")
      case _ => SourceParamOther
    }
  }

  private def sourceParamName(raw: String): Option[String] = {
    val NamePattern = """.*\b([A-Za-z_][A-Za-z0-9_]*)\s*$""".r
    raw.trim match {
      case NamePattern(name) => Some(name)
      case _ => None
    }
  }

  private def toFlixDefBaseName(symbol: String): String = {
    val parts = symbol.split("[^A-Za-z0-9]+").toList.filter(_.nonEmpty)
    val raw =
      if (parts.isEmpty) "ffiBinding"
      else {
        val head = lowerFirst(parts.head)
        val tail = parts.tail.map(upperFirst)
        (head :: tail).mkString
      }
    val prefixed =
      if (raw.headOption.exists(_.isDigit) || ReservedIdentifiers.contains(raw)) s"ffi${upperFirst(raw)}"
      else raw
    prefixed match {
      case FlixIdentifierRegex() => prefixed
      case _ => "ffiBinding"
    }
  }

  private def sanitizeParamName(name: String, idx: Int): String = {
    val cleaned =
      if (FlixIdentifierRegex.pattern.matcher(name).matches() && !ReservedIdentifiers.contains(name)) name
      else s"arg$idx"
    if (cleaned.headOption.exists(_.isDigit)) s"arg$idx" else cleaned
  }

  private def dedupeName(base: String, usedNames: mutable.Set[String]): String = {
    var candidate = base
    var n = 2
    while (usedNames.contains(candidate)) {
      candidate = s"${base}${n}"
      n = n + 1
    }
    usedNames += candidate
    candidate
  }

  private def renderInvocation(config: Config): String = {
    val parts = mutable.ListBuffer("flix", "bind", "native", "--header", config.header.toAbsolutePath.normalize().toString, "--out", config.outDir.toAbsolutePath.normalize().toString)
    config.spec.foreach(path => parts ++= List("--spec", path.toAbsolutePath.normalize().toString))
    if (config.rootModule != "Native") {
      parts ++= List("--native-module", config.rootModule)
    }
    parts.mkString(" ")
  }

  private def renderFlix(config: Config, bindings: List[FlixBinding], skipped: List[Skipped], borrowHelpers: List[HandleBorrowHelper], closeHelpers: List[ResourceCloseHelper], retainHelpers: List[ResourceRetainHelper], ownerHelpers: List[BorrowedOwnerHelper]): String = {
    val sb = new StringBuilder
    sb.append(s"/// Generated by `${renderInvocation(config)}`.\n")
    sb.append("/// This generator emits direct scalar/unit imports plus shim-backed String/Bytes adapters, synchronous export-backed callback trampolines, and explicitly annotated borrowed/owned opaque handle wrappers from a curated C header plus sidecar spec.\n")
    if (skipped.nonEmpty) {
      sb.append("/// Skipped declarations:\n")
      skipped.foreach { skip =>
        sb.append(s"///   - ${skip.symbol}: ${skip.reason}\n")
      }
    }
    sb.append(s"pub mod ${config.rootModule} {\n")
    val handleTypes = (bindings.iterator.flatMap(_.handleTypes).toSet ++ borrowHelpers.iterator.flatMap(h => List(h.ownedType, h.borrowedType)).toSet).toList.sorted
    if (handleTypes.nonEmpty) {
      handleTypes.foreach { handleType =>
        sb.append('\n')
        sb.append(s"    /// Generated opaque native handle wrapper.\n")
        sb.append(s"    pub enum ${handleType}(Int64)\n")
      }
    }
    if (borrowHelpers.nonEmpty) {
      borrowHelpers.foreach { helper =>
        sb.append('\n')
        sb.append(s"    /// Generated owned-to-borrowed opaque handle view.\n")
        sb.append(s"    pub def ${helper.defName}(handle: ${helper.ownedType}): ${helper.borrowedType} = ")
        sb.append(s"match handle { case ${renderHandleCtor(helper.ownedType)}(id) => ${renderHandleCtor(helper.borrowedType)}(id) }\n")
      }
    }
    if (closeHelpers.nonEmpty) {
      closeHelpers.foreach { helper =>
        sb.append('\n')
        sb.append(s"    /// Generated canonical resource close helper.\n")
        sb.append(s"    pub def ${helper.defName}(handle: ${helper.handleType}): Unit \\ IO = ${helper.targetDefName}(handle)\n")
      }
    }
    if (retainHelpers.nonEmpty) {
      retainHelpers.foreach { helper =>
        sb.append('\n')
        sb.append(s"    /// Generated canonical resource retain helper.\n")
        val argExpr =
          if (helper.targetParamType == helper.handleType) "handle"
          else s"""match handle { case ${renderHandleCtor(helper.handleType)}(id) => ${renderHandleCtor(helper.targetParamType)}(id) }"""
        sb.append(s"    pub def ${helper.defName}(handle: ${helper.handleType}): Option[${helper.handleType}] \\ IO = ${helper.targetDefName}(${argExpr})\n")
      }
    }
    if (ownerHelpers.nonEmpty) {
      ownerHelpers.foreach { helper =>
        sb.append('\n')
        sb.append(s"    /// Generated owner-based wrapper for a borrowed result tied to a resource parameter.\n")
        val effSuffix = helper.effect.map(e => s" \\ $e").getOrElse("")
        sb.append(s"    pub def ${helper.defName}(")
        sb.append(helper.publicParams.map { case (name, tpe) => s"$name: $tpe" }.mkString(", "))
        sb.append(s"): ${helper.publicResult}${effSuffix} = ${helper.targetDefName}(")
        sb.append(helper.targetArgs.mkString(", "))
        sb.append(")\n")
      }
    }
    if (bindings.isEmpty) {
      sb.append("    /// No supported direct native imports were found in the curated header.\n")
    } else {
      bindings.foreach { binding =>
        val effSuffix = binding.effect.map(e => s" \\ $e").getOrElse("")
        sb.append('\n')
        sb.append(s"""    extern native(symbol = "${escape(binding.targetSymbol)}")\n""")
        sb.append(s"    def ${binding.rawName}(")
        sb.append(binding.rawParams.map { case (name, tpe) => s"$name: $tpe" }.mkString(", "))
        sb.append(s"): ${binding.rawResult}${effSuffix}\n")
        sb.append('\n')
        sb.append(s"    pub def ${binding.defName}(")
        sb.append(binding.publicParams.map { case (name, tpe) => s"$name: $tpe" }.mkString(", "))
        sb.append(s"): ${binding.publicResult}${effSuffix} =")
        if (binding.wrapperExpr.contains('\n')) {
          sb.append('\n')
          binding.wrapperExpr.linesIterator.foreach { line =>
            sb.append("        ").append(line).append('\n')
          }
        } else {
          sb.append(' ').append(binding.wrapperExpr).append('\n')
        }
      }
    }
    sb.append("}\n")
    sb.toString()
  }

  private def renderShim(config: Config, shims: List[NativeShim], shimHeaderFile: Path): String = {
    val sb = new StringBuilder
    sb.append("/*\n")
    sb.append(s" * Generated by `${renderInvocation(config)}`.\n")
    sb.append(" * This shim adapts curated C signatures to the Flix native bridge ABI.\n")
    sb.append(" */\n\n")
    sb.append("#include <stdbool.h>\n")
    sb.append("#include <stdint.h>\n")
    sb.append("#include <stddef.h>\n")
    sb.append("#include <stdlib.h>\n")
    sb.append("#include <string.h>\n")
    sb.append("#include \"include/")
    sb.append(shimHeaderFile.getFileName.toString)
    sb.append("\"\n\n")
    sb.append("typedef struct flix_exec {\n")
    sb.append("  int64_t tag;\n")
    sb.append("  int64_t payload;\n")
    sb.append("} flix_exec_t;\n")
    sb.append("\n")
    sb.append("enum {\n")
    sb.append("  FLIX_EXEC_OK = 1,\n")
    sb.append("  FLIX_EXEC_SUSPENDED = 3,\n")
    sb.append("  FLIX_EXEC_THROWN = 4,\n")
    sb.append("};\n")
    sb.append("\n")
    sb.append("typedef struct flix_ctx flix_ctx_t;\n")
    sb.append("typedef int64_t flix_handle_t;\n")
    sb.append("typedef flix_handle_t flix_string_t;\n")
    sb.append("typedef flix_handle_t flix_bytes_t;\n")
    sb.append("typedef flix_handle_t flix_i8_array_t;\n\n")
    sb.append("flix_string_t flix_string_from_utf8(flix_ctx_t* ctx, const uint8_t* bytes, int64_t len);\n")
    sb.append("uint8_t* flix_string_to_utf8(flix_ctx_t* ctx, flix_string_t str, int64_t* out_len);\n")
    sb.append("flix_i8_array_t flix_i8_array_from_bytes(flix_ctx_t* ctx, const uint8_t* bytes, int64_t len);\n")
    sb.append("uint8_t* flix_i8_array_to_bytes(flix_ctx_t* ctx, flix_i8_array_t arr, int64_t* out_len);\n")
    sb.append("flix_ctx_t* flix_ctx_new(void);\n")
    sb.append("void flix_ctx_free(flix_ctx_t* ctx);\n")
    sb.append("void flix_exn_report(flix_ctx_t* ctx, flix_handle_t exn);\n")
    sb.append("void flix_suspension_report(flix_ctx_t* ctx, flix_handle_t susp);\n")
    sb.append("flix_handle_t flix_handle_new_i64(flix_ctx_t* ctx, int64_t payload);\n")
    sb.append("void flix_handle_release(flix_ctx_t* ctx, flix_handle_t handle);\n")
    sb.append("flix_handle_t flix_export_tag_new(flix_ctx_t* ctx, int64_t tag_id, const flix_handle_t* handles_ptr, int32_t arity);\n")
    sb.append("void flix_free(void* ptr);\n")

    val callbacks = shims.iterator.flatMap { shim =>
      shim.callArgs.collect { case cb: CallbackCallArg => (shim, cb) }
    }.toList

    callbacks.foreach { case (shim, cb) =>
      val suffix = s"${shim.symbol}_${cb.name}"
      sb.append("\n")
      sb.append(s"static _Thread_local flix_ctx_t* flix_bind_native_tls_ctx_${suffix} = NULL;\n")
      sb.append(s"static _Thread_local bool flix_bind_native_tls_failed_${suffix} = false;\n")
      sb.append(s"static _Thread_local flix_exec_t flix_bind_native_tls_exec_${suffix};\n")
      sb.append(renderCallbackExportPrototype(cb))
      sb.append(renderCallbackTrampoline(suffix, cb))
    }

    shims.foreach { shim =>
      val requiresBridgeCtx = nativeShimRequiresBridgeCtx(shim)
      sb.append('\n')
      sb.append(s"${renderShimResultSignature(shim.result)} ${shim.symbol}(")
      if (requiresBridgeCtx) {
        sb.append("flix_ctx_t* ctx")
      }
      shim.params.zipWithIndex.foreach {
        case (param, idx) =>
          if (requiresBridgeCtx || idx > 0) sb.append(", ")
          param match {
            case ScalarParam(name, _, zigType) =>
              sb.append(s"${mapZigTypeToCType(zigType).getOrElse("int64_t")} $name")
            case CStringParam(name) =>
              sb.append(s"flix_string_t $name")
            case BytesParam(name, _, _) =>
              sb.append(s"flix_bytes_t $name")
            case HandleParam(name, _, _) =>
              sb.append(s"int64_t $name")
          }
      }
      sb.append(") {\n")

      shim.callArgs.foreach {
        case CStringCallArg(name) =>
          sb.append(s"  int64_t ${name}_len = 0;\n")
          sb.append(s"  uint8_t* ${name}_bytes = flix_string_to_utf8(ctx, $name, &${name}_len);\n")
          sb.append(s"  (void)${name}_len;\n")
        case BytesCallArg(name, _, _) =>
          sb.append(s"  int64_t ${name}_len = 0;\n")
          sb.append(s"  uint8_t* ${name}_bytes = flix_i8_array_to_bytes(ctx, (flix_i8_array_t)$name, &${name}_len);\n")
        case ResultLenOutCallArg(param) =>
          sb.append(s"  ${param.cType} ${param.name}_out = 0;\n")
        case ResultHandleOutCallArg(param) =>
          sb.append(s"  ${param.cType} ${param.name}_out = NULL;\n")
        case CallbackCallArg(_, _, _) =>
          ()
        case HandleCallArg(_, _) =>
          ()
        case _ => // nop
      }

      val callbackSuffixes = shim.callArgs.collect { case cb: CallbackCallArg => s"${shim.symbol}_${cb.name}" }
      callbackSuffixes.foreach { suffix =>
        if (requiresBridgeCtx) {
          sb.append(s"  flix_ctx_t* flix_bind_native_prev_ctx_${suffix} = flix_bind_native_tls_ctx_${suffix};\n")
          sb.append(s"  bool flix_bind_native_prev_failed_${suffix} = flix_bind_native_tls_failed_${suffix};\n")
          sb.append(s"  flix_exec_t flix_bind_native_prev_exec_${suffix} = flix_bind_native_tls_exec_${suffix};\n")
          sb.append(s"  flix_bind_native_tls_ctx_${suffix} = ctx;\n")
        } else {
          sb.append(s"  flix_ctx_t* flix_bind_native_local_ctx_${suffix} = flix_ctx_new();\n")
          sb.append(s"  flix_ctx_t* flix_bind_native_prev_ctx_${suffix} = flix_bind_native_tls_ctx_${suffix};\n")
          sb.append(s"  bool flix_bind_native_prev_failed_${suffix} = flix_bind_native_tls_failed_${suffix};\n")
          sb.append(s"  flix_exec_t flix_bind_native_prev_exec_${suffix} = flix_bind_native_tls_exec_${suffix};\n")
          sb.append(s"  flix_bind_native_tls_ctx_${suffix} = flix_bind_native_local_ctx_${suffix};\n")
        }
        sb.append(s"  flix_bind_native_tls_failed_${suffix} = false;\n")
        sb.append(s"  flix_bind_native_tls_exec_${suffix}.tag = 0;\n")
        sb.append(s"  flix_bind_native_tls_exec_${suffix}.payload = 0;\n")
      }

      val callExpr = new StringBuilder
      callExpr.append(s"${shim.originalSymbol}(")
      callExpr.append(shim.callArgs.map {
        case ScalarCallArg(name) => name
        case CStringCallArg(name) => s"(const char*)${name}_bytes"
        case BytesCallArg(name, ptrCType, lenCType) => s"(${ptrCType})${name}_bytes, (${lenCType})${name}_len"
        case ResultLenOutCallArg(param) => s"&${param.name}_out"
        case ResultHandleOutCallArg(param) => s"&${param.name}_out"
        case cb: CallbackCallArg => s"&flix_bind_native_callback_${shim.symbol}_${cb.name}"
        case HandleCallArg(name, cType) => s"(${cType})(intptr_t)${name}"
      }.mkString(", "))
      callExpr.append(")")

      shim.result match {
        case ScalarResult(_, cType) if cType == "void" =>
          sb.append("  ")
          sb.append(callExpr)
          sb.append(";\n")
        case ScalarResult(_, cType) =>
          sb.append(s"  ${cType} result = ")
          sb.append(callExpr)
          sb.append(";\n")
        case StringResult(_, ownership) =>
          sb.append(s"  ${ownershipAwareCStringType(shim.result)} raw_result = ")
          sb.append(callExpr)
          sb.append(";\n")
          sb.append("  int64_t result_len = (int64_t)strlen(raw_result);\n")
          sb.append("  flix_string_t result = flix_string_from_utf8(ctx, (const uint8_t*)raw_result, result_len);\n")
          ownership match {
            case OwnedStringOwnership(freeFn) =>
              sb.append(s"  ${freeFn}(raw_result);\n")
            case BorrowedStringOwnership =>
              ()
          }
        case BytesResult(lenOut, ownership) =>
          sb.append(s"  ${ownershipAwareBytesPtrCType(shim.result)} raw_result = ")
          sb.append(callExpr)
          sb.append(";\n")
          sb.append(s"  flix_bytes_t result = (flix_bytes_t)flix_i8_array_from_bytes(ctx, (const uint8_t*)raw_result, (int64_t)${lenOut.name}_out);\n")
          ownership match {
            case OwnedBytesOwnership(freeFn) =>
              sb.append(s"  ${freeFn}(raw_result);\n")
            case BorrowedBytesOwnership =>
              ()
          }
        case HandleResult(_, cType) =>
          sb.append(s"  ${cType} raw_result = ")
          sb.append(callExpr)
          sb.append(";\n")
          sb.append("  int64_t result = raw_result == NULL ? 0 : (int64_t)(intptr_t)raw_result;\n")
        case HandleStatusResult(_, statusCType, _, outHandle, okValue) =>
          sb.append("  int64_t result = 0;\n")
          sb.append(s"  ${statusCType} status = ")
          sb.append(callExpr)
          sb.append(";\n")
          sb.append(s"  if ((int64_t)status == (int64_t)(${okValue})) {\n")
          sb.append(s"    if (${outHandle.name}_out == NULL) {\n")
          sb.append("      abort();\n")
          sb.append("    }\n")
          sb.append(s"    flix_handle_t ok_payload = flix_handle_new_i64(ctx, (int64_t)(intptr_t)${outHandle.name}_out);\n")
          sb.append("    result = flix_export_tag_new(ctx, 1, &ok_payload, 1);\n")
          sb.append("    flix_handle_release(ctx, ok_payload);\n")
          sb.append("  } else {\n")
          sb.append("    flix_handle_t err_payload = flix_handle_new_i64(ctx, (int64_t)status);\n")
          sb.append("    result = flix_export_tag_new(ctx, 0, &err_payload, 1);\n")
          sb.append("    flix_handle_release(ctx, err_payload);\n")
          sb.append("  }\n")
      }

      shim.callArgs.foreach {
        case CStringCallArg(name) =>
          sb.append(s"  flix_free(${name}_bytes);\n")
        case BytesCallArg(name, _, _) =>
          sb.append(s"  flix_free(${name}_bytes);\n")
        case _ => // nop
      }

      callbackSuffixes.foreach { suffix =>
        sb.append(s"  if (flix_bind_native_tls_failed_${suffix}) {\n")
        sb.append(s"    if (flix_bind_native_tls_exec_${suffix}.tag == FLIX_EXEC_THROWN) {\n")
        sb.append(s"      flix_exn_report(flix_bind_native_tls_ctx_${suffix}, flix_bind_native_tls_exec_${suffix}.payload);\n")
        sb.append("    } else if (flix_bind_native_tls_exec_").append(suffix).append(".tag == FLIX_EXEC_SUSPENDED) {\n")
        sb.append(s"      flix_suspension_report(flix_bind_native_tls_ctx_${suffix}, flix_bind_native_tls_exec_${suffix}.payload);\n")
        sb.append("    }\n")
        if (!requiresBridgeCtx) {
          sb.append(s"    flix_ctx_free(flix_bind_native_tls_ctx_${suffix});\n")
        }
        sb.append("    abort();\n")
        sb.append("  }\n")
        if (!requiresBridgeCtx) {
          sb.append(s"  flix_ctx_free(flix_bind_native_tls_ctx_${suffix});\n")
        }
        sb.append(s"  flix_bind_native_tls_ctx_${suffix} = flix_bind_native_prev_ctx_${suffix};\n")
        sb.append(s"  flix_bind_native_tls_failed_${suffix} = flix_bind_native_prev_failed_${suffix};\n")
        sb.append(s"  flix_bind_native_tls_exec_${suffix} = flix_bind_native_prev_exec_${suffix};\n")
      }

      shim.result match {
        case ScalarResult(_, cType) if cType == "void" => ()
        case _ => sb.append("  return result;\n")
      }
      sb.append("}\n")
    }

    sb.toString()
  }

  private def renderShimResultSignature(result: LoweredResult): String = result match {
    case ScalarResult(_, cType) => cType
    case StringResult(_, _) => "flix_string_t"
    case BytesResult(_, _) => "flix_bytes_t"
    case HandleResult(_, _) => "int64_t"
    case HandleStatusResult(_, _, _, _, _) => "int64_t"
  }

  private def renderCallbackExportPrototype(cb: CallbackCallArg): String = {
    val params = cb.callbackType.params.zipWithIndex.map {
      case (tpe, idx) => s", ${mapZigTypeToCType(tpe).getOrElse("int64_t")} a$idx"
    }.mkString
    val outParam = mapZigTypeToCType(cb.callbackType.result) match {
      case Some("void") => ""
      case Some(cType) => s", ${cType}* out"
      case None => ""
    }
    s"flix_exec_t ${cb.exportSymbol}(flix_ctx_t* ctx$params$outParam);\n"
  }

  private def renderCallbackTrampoline(suffix: String, cb: CallbackCallArg): String = {
    val sb = new StringBuilder
    val resultCType = mapZigTypeToCType(cb.callbackType.result).getOrElse("int64_t")
    sb.append(s"static ${renderCallbackResultSignature(cb.callbackType)} flix_bind_native_callback_${suffix}(")
    sb.append(cb.callbackType.params.zipWithIndex.map {
      case (tpe, idx) => s"${mapZigTypeToCType(tpe).getOrElse("int64_t")} a$idx"
    }.mkString(", "))
    sb.append(") {\n")
    if (resultCType == "void") {
      sb.append(s"  flix_exec_t exec = ${cb.exportSymbol}(flix_bind_native_tls_ctx_${suffix}")
      if (cb.callbackType.params.nonEmpty) {
        sb.append(", ")
        sb.append(cb.callbackType.params.indices.map(idx => s"a$idx").mkString(", "))
      }
      sb.append(");\n")
      sb.append(s"  if (exec.tag != FLIX_EXEC_OK) {\n")
      sb.append(s"    flix_bind_native_tls_failed_${suffix} = true;\n")
      sb.append(s"    flix_bind_native_tls_exec_${suffix} = exec;\n")
      sb.append("    return;\n")
      sb.append("  }\n")
    } else {
      sb.append(s"  ${resultCType} out = 0;\n")
      sb.append(s"  flix_exec_t exec = ${cb.exportSymbol}(flix_bind_native_tls_ctx_${suffix}")
      if (cb.callbackType.params.nonEmpty) {
        sb.append(", ")
        sb.append(cb.callbackType.params.indices.map(idx => s"a$idx").mkString(", "))
        sb.append(", ")
      } else {
        sb.append(", ")
      }
      sb.append("&out);\n")
      sb.append(s"  if (exec.tag != FLIX_EXEC_OK) {\n")
      sb.append(s"    flix_bind_native_tls_failed_${suffix} = true;\n")
      sb.append(s"    flix_bind_native_tls_exec_${suffix} = exec;\n")
      sb.append(s"    return ${renderZeroLiteral(resultCType)};\n")
      sb.append("  }\n")
      sb.append("  return out;\n")
    }
    sb.append("}\n")
    sb.toString()
  }

  private def renderCallbackResultSignature(callbackType: CallbackType): String =
    mapZigTypeToCType(callbackType.result).getOrElse("int64_t")

  private def renderZeroLiteral(cType: String): String = cType match {
    case "float" => "0.0f"
    case "double" => "0.0"
    case "bool" => "false"
    case _ => "0"
  }

  private def ownershipAwareCStringType(result: LoweredResult): String = result match {
    case StringResult(cType, _) => cType
    case other => throw new IllegalStateException(s"Unexpected shim string result request: $other")
  }

  private def ownershipAwareBytesPtrCType(result: LoweredResult): String = result match {
    case BytesResult(_, BorrowedBytesOwnership) => "const void*"
    case BytesResult(_, OwnedBytesOwnership(_)) => "void*"
    case other => throw new IllegalStateException(s"Unexpected shim bytes result request: $other")
  }

  private def renderWrapperArgExpr(param: LoweredParam): String = param match {
    case ScalarParam(name, _, _) => name
    case CStringParam(name) => name
    case BytesParam(name, _, _) => name
    case HandleParam(name, flixType, _) => s"match $name { case ${renderHandleCtor(flixType)}(id) => id }"
  }

  private def renderHandleCtor(flixType: String): String = s"${flixType}.${flixType}"

  private def nativeShimRequiresBridgeCtx(shim: NativeShim): Boolean =
    shim.params.exists {
      case _: CStringParam => true
      case _: BytesParam => true
      case _ => false
    } || (shim.result match {
      case _: StringResult => true
      case _: BytesResult => true
      case _: HandleStatusResult => true
      case _ => false
    })

  private def isCallbackExportName(s: String): Boolean =
    s.matches("[A-Za-z_][A-Za-z0-9_]*(\\.[A-Za-z_][A-Za-z0-9_]*)+")

  private def renderExportSymbol(exportDef: String): String =
    s"flix_export_${mangleExportName(exportDef)}"

  private def mangleExportName(s: String): String = {
    val b = new StringBuilder(s.length + 8)
    s.foreach {
      case c if (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') || c == '_' =>
        b.append(c)
      case _ =>
        b.append('_')
    }
    if (b.isEmpty) "_"
    else {
      val head = b.charAt(0)
      if (head >= '0' && head <= '9') "_" + b.toString() else b.toString()
    }
  }

  private def lowerFirst(s: String): String =
    if (s.isEmpty) s else s"${s.head.toLower}${s.tail}"

  private def upperFirst(s: String): String =
    if (s.isEmpty) s else s"${s.head.toUpper}${s.tail}"

  private def escape(s: String): String =
    s.replace("\\", "\\\\").replace("\"", "\\\"")

  private def exec(cmd: List[String], cwd: Path): (Int, String) = {
    val pb = new ProcessBuilder(cmd*)
    pb.directory(cwd.toFile)
    pb.redirectErrorStream(true)
    val proc = pb.start()
    val output = new String(proc.getInputStream.readAllBytes(), StandardCharsets.UTF_8)
    val exit = proc.waitFor()
    (exit, output)
  }
}

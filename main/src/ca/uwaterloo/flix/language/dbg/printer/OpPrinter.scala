/*
 * Copyright 2023 Jonathan Lindegaard Starup
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

package ca.uwaterloo.flix.language.dbg.printer

import ca.uwaterloo.flix.language.ast.SemanticOp.*
import ca.uwaterloo.flix.language.ast.*
import ca.uwaterloo.flix.language.ast.shared.Mutability
import ca.uwaterloo.flix.language.dbg.DocAst
import ca.uwaterloo.flix.language.dbg.DocAst.Expr
import ca.uwaterloo.flix.language.dbg.DocAst.Expr.*
import ca.uwaterloo.flix.util.collection.ListOps

object OpPrinter {

  private val and = "and"
  private val div = "/"
  private val eq = "=="
  private val exp = "**"
  private val ge = ">="
  private val gt = ">"
  private val le = "<="
  private val lt = "<"
  private val minus = "-"
  private val mul = "*"
  private val neg = "-"
  private val neq = "!="
  private val not = "!"
  private val or = "or"
  private val plus = "+"
  private val rem = "rem"
  private val shl = "shl"
  private val shr = "shr"
  private val xor = "xor"

  /**
    * Returns the string representation of `so`.
    */
  def print(so: SemanticOp): String = so match {
    case op: BoolOp => printBoolOp(op)
    case op: CharOp => printCharOp(op)
    case op: Float32Op => printFloat32Op(op)
    case op: Float64Op => printFloat64Op(op)
    case op: Int8Op => printInt8Op(op)
    case op: Int16Op => printInt16Op(op)
    case op: Int32Op => printInt32Op(op)
    case op: Int64Op => printInt64Op(op)
    case op: BigIntOp => printBigIntOp(op)
    case op: BigDecimalOp => printBigDecimalOp(op)
    case op: CodePointOp => printCodePointOp(op)
    case op: StringOp => printStringOp(op)
    case op: ParseOp => printParseOp(op)
    case op: StringBuilderOp => printStringBuilderOp(op)
    case op: RegexOp => printRegexOp(op)
    case op: ToStringOp => printToStringOp(op)
    case op: ConvertOp => printConvertOp(op)
    case op: IoOp => printIoOp(op)
    case op: PlatformOp => printPlatformOp(op)
    case op: ObjectOp => printObjectOp(op)
    case op: HashOp => printHashOp(op)
    case op: ExnOp => printExnOp(op)
  }

  private def printBoolOp(op: BoolOp): String = op match {
    case BoolOp.Not => not
    case BoolOp.And => and
    case BoolOp.Or => or
    case BoolOp.Eq => eq
    case BoolOp.Neq => neq
  }

  private def printCharOp(op: CharOp): String = op match {
    case CharOp.Eq => eq
    case CharOp.Neq => neq
    case CharOp.Lt => lt
    case CharOp.Le => le
    case CharOp.Gt => gt
    case CharOp.Ge => ge
    case CharOp.IsLetter => "charIsLetter"
    case CharOp.IsDigit => "charIsDigit"
    case CharOp.IsLetterOrDigit => "charIsLetterOrDigit"
    case CharOp.IsLowerCase => "charIsLowerCase"
    case CharOp.IsUpperCase => "charIsUpperCase"
    case CharOp.IsTitleCase => "charIsTitleCase"
    case CharOp.IsWhitespace => "charIsWhitespace"
    case CharOp.IsDefined => "charIsDefined"
    case CharOp.IsISOControl => "charIsISOControl"
    case CharOp.IsMirrored => "charIsMirrored"
    case CharOp.IsSurrogate => "charIsSurrogate"
    case CharOp.IsSurrogatePair => "charIsSurrogatePair"
    case CharOp.ToLowerCase => "charToLowerCase"
    case CharOp.ToUpperCase => "charToUpperCase"
    case CharOp.ToTitleCase => "charToTitleCase"
    case CharOp.GetNumericValue => "charGetNumericValue"
    case CharOp.ToCodePoint => "charToCodePoint"
    case CharOp.Digit => "charDigit"
    case CharOp.ForDigit => "charForDigit"
  }

  private def printFloat32Op(op: Float32Op): String = op match {
    case Float32Op.Eq => eq
    case Float32Op.Neq => neq
    case Float32Op.Lt => lt
    case Float32Op.Le => le
    case Float32Op.Gt => gt
    case Float32Op.Ge => ge
    case Float32Op.Add => plus
    case Float32Op.Sub => minus
    case Float32Op.Mul => mul
    case Float32Op.Div => div
    case Float32Op.Exp => exp
    case Float32Op.Neg => neg
  }

  private def printFloat64Op(op: Float64Op): String = op match {
    case Float64Op.Eq => eq
    case Float64Op.Neq => neq
    case Float64Op.Lt => lt
    case Float64Op.Le => le
    case Float64Op.Gt => gt
    case Float64Op.Ge => ge
    case Float64Op.Add => plus
    case Float64Op.Sub => minus
    case Float64Op.Mul => mul
    case Float64Op.Div => div
    case Float64Op.Exp => exp
    case Float64Op.Neg => neg
  }

  private def printInt8Op(op: Int8Op): String = op match {
    case Int8Op.Not => not
    case Int8Op.And => and
    case Int8Op.Or => or
    case Int8Op.Eq => eq
    case Int8Op.Neq => neq
    case Int8Op.Lt => lt
    case Int8Op.Le => le
    case Int8Op.Gt => gt
    case Int8Op.Ge => ge
    case Int8Op.Add => plus
    case Int8Op.Sub => minus
    case Int8Op.Mul => mul
    case Int8Op.Div => div
    case Int8Op.Exp => exp
    case Int8Op.Neg => neg
    case Int8Op.Rem => rem
    case Int8Op.Xor => xor
    case Int8Op.Shl => shl
    case Int8Op.Shr => shr
  }

  private def printInt16Op(op: Int16Op): String = op match {
    case Int16Op.Not => not
    case Int16Op.And => and
    case Int16Op.Or => or
    case Int16Op.Eq => eq
    case Int16Op.Neq => neq
    case Int16Op.Lt => lt
    case Int16Op.Le => le
    case Int16Op.Gt => gt
    case Int16Op.Ge => ge
    case Int16Op.Add => plus
    case Int16Op.Sub => minus
    case Int16Op.Mul => mul
    case Int16Op.Div => div
    case Int16Op.Exp => exp
    case Int16Op.Neg => neg
    case Int16Op.Rem => rem
    case Int16Op.Xor => xor
    case Int16Op.Shl => shl
    case Int16Op.Shr => shr
  }

  private def printInt32Op(op: Int32Op): String = op match {
    case Int32Op.Not => not
    case Int32Op.And => and
    case Int32Op.Or => or
    case Int32Op.Eq => eq
    case Int32Op.Neq => neq
    case Int32Op.Lt => lt
    case Int32Op.Le => le
    case Int32Op.Gt => gt
    case Int32Op.Ge => ge
    case Int32Op.Add => plus
    case Int32Op.Sub => minus
    case Int32Op.Mul => mul
    case Int32Op.Div => div
    case Int32Op.Exp => exp
    case Int32Op.Neg => neg
    case Int32Op.Rem => rem
    case Int32Op.Xor => xor
    case Int32Op.Shl => shl
    case Int32Op.Shr => shr
  }

  private def printInt64Op(op: Int64Op): String = op match {
    case Int64Op.Not => not
    case Int64Op.Or => or
    case Int64Op.Eq => eq
    case Int64Op.Neq => neq
    case Int64Op.Lt => lt
    case Int64Op.Le => le
    case Int64Op.Gt => gt
    case Int64Op.Ge => ge
    case Int64Op.Add => plus
    case Int64Op.Sub => minus
    case Int64Op.Mul => mul
    case Int64Op.Div => div
    case Int64Op.Exp => exp
    case Int64Op.Neg => neg
    case Int64Op.Rem => rem
    case Int64Op.Xor => xor
    case Int64Op.Shl => shl
    case Int64Op.Shr => shr
    case Int64Op.And => plus
  }

  private def printBigIntOp(op: BigIntOp): String = op match {
    case BigIntOp.Not => not
    case BigIntOp.Or => or
    case BigIntOp.Add => plus
    case BigIntOp.Sub => minus
    case BigIntOp.Mul => mul
    case BigIntOp.Div => div
    case BigIntOp.Neg => neg
    case BigIntOp.Rem => rem
    case BigIntOp.Xor => xor
    case BigIntOp.Shl => shl
    case BigIntOp.Shr => shr
    case BigIntOp.And => and
    case BigIntOp.Cmp => "bigIntCompare"
    case BigIntOp.BitLength => "bigIntBitLength"
    case BigIntOp.FromInt64 => "bigIntFromInt64"
  }

  private def printBigDecimalOp(op: BigDecimalOp): String = op match {
    case BigDecimalOp.Neg => neg
    case BigDecimalOp.Add => plus
    case BigDecimalOp.Sub => minus
    case BigDecimalOp.Mul => mul
    case BigDecimalOp.Div => div
    case BigDecimalOp.Cmp => "bigDecimalCompare"
    case BigDecimalOp.Scale => "bigDecimalScale"
    case BigDecimalOp.Precision => "bigDecimalPrecision"
    case BigDecimalOp.Ceil => "bigDecimalCeil"
    case BigDecimalOp.Floor => "bigDecimalFloor"
    case BigDecimalOp.Round => "bigDecimalRound"
    case BigDecimalOp.ToBigInt => "bigDecimalToBigInt"
    case BigDecimalOp.ToPlainString => "bigDecimalToPlainString"
  }

  private def printCodePointOp(op: CodePointOp): String = op match {
    case CodePointOp.IsLetter => "codePointIsLetter"
    case CodePointOp.IsDigit => "codePointIsDigit"
    case CodePointOp.IsLowerCase => "codePointIsLowerCase"
    case CodePointOp.IsUpperCase => "codePointIsUpperCase"
    case CodePointOp.IsTitleCase => "codePointIsTitleCase"
    case CodePointOp.IsWhitespace => "codePointIsWhitespace"
    case CodePointOp.IsAlphabetic => "codePointIsAlphabetic"
    case CodePointOp.IsDefined => "codePointIsDefined"
    case CodePointOp.IsIdeographic => "codePointIsIdeographic"
    case CodePointOp.IsISOControl => "codePointIsISOControl"
    case CodePointOp.IsMirrored => "codePointIsMirrored"
    case CodePointOp.ToLowerCase => "codePointToLowerCase"
    case CodePointOp.ToUpperCase => "codePointToUpperCase"
    case CodePointOp.ToTitleCase => "codePointToTitleCase"
    case CodePointOp.GetName => "codePointGetName"
    case CodePointOp.GetNumericValue => "codePointGetNumericValue"
  }

  private def printStringOp(op: StringOp): String = op match {
    case StringOp.Concat => plus
    case StringOp.Length => "stringLength"
    case StringOp.CharAt => "stringCharAt"
    case StringOp.ToLowerCase => "stringToLowerCase"
    case StringOp.ToUpperCase => "stringToUpperCase"
    case StringOp.Repeat => "stringRepeat"
  }

  private def printParseOp(op: ParseOp): String = op match {
    case ParseOp.Int8FromString => "int8FromString"
    case ParseOp.Int16FromString => "int16FromString"
    case ParseOp.Int32FromString => "int32FromString"
    case ParseOp.Int64FromString => "int64FromString"
    case ParseOp.Float32FromString => "float32FromString"
    case ParseOp.Float64FromString => "float64FromString"
    case ParseOp.BigIntFromString => "bigIntFromString"
    case ParseOp.BigDecimalFromString => "bigDecimalFromString"
    case ParseOp.Int32Parse => "int32Parse"
    case ParseOp.Int64Parse => "int64Parse"
  }

  private def printStringBuilderOp(op: StringBuilderOp): String = op match {
    case StringBuilderOp.New => "stringBuilderNew"
    case StringBuilderOp.AppendString => "stringBuilderAppendString"
    case StringBuilderOp.AppendCodePoint => "stringBuilderAppendCodePoint"
    case StringBuilderOp.CharAt => "stringBuilderCharAt"
    case StringBuilderOp.Length => "stringBuilderLength"
    case StringBuilderOp.SetLength => "stringBuilderSetLength"
    case StringBuilderOp.ToString => "stringBuilderToString"
  }

  private def printRegexOp(op: RegexOp): String = op match {
    case RegexOp.FlagCanonEq => "regexFlagCanonEq"
    case RegexOp.FlagCaseInsensitive => "regexFlagCaseInsensitive"
    case RegexOp.FlagComments => "regexFlagComments"
    case RegexOp.FlagDotall => "regexFlagDotall"
    case RegexOp.FlagLiteral => "regexFlagLiteral"
    case RegexOp.FlagMultiline => "regexFlagMultiline"
    case RegexOp.FlagUnicodeCase => "regexFlagUnicodeCase"
    case RegexOp.FlagUnicodeCharacterClass => "regexFlagUnicodeCharacterClass"
    case RegexOp.FlagUnixLines => "regexFlagUnixLines"
    case RegexOp.Compile => "regexCompile"
    case RegexOp.CompileWithFlags => "regexCompileWithFlags"
    case RegexOp.TryCompile => "regexTryCompile"
    case RegexOp.TryCompileWithFlags => "regexTryCompileWithFlags"
    case RegexOp.Quote => "regexQuote"
    case RegexOp.Pattern => "regexPattern"
    case RegexOp.Flags => "regexFlags"
    case RegexOp.Split => "regexSplit"
    case RegexOp.NewMatcher => "regexNewMatcher"
    case RegexOp.MatcherMatches => "regexMatcherMatches"
    case RegexOp.MatcherFind => "regexMatcherFind"
    case RegexOp.MatcherFindFrom => "regexMatcherFindFrom"
    case RegexOp.MatcherLookingAt => "regexMatcherLookingAt"
    case RegexOp.MatcherReplaceAll => "regexMatcherReplaceAll"
    case RegexOp.MatcherReplaceFirst => "regexMatcherReplaceFirst"
    case RegexOp.MatcherSetBounds => "regexMatcherSetBounds"
    case RegexOp.MatcherStart => "regexMatcherStart"
    case RegexOp.MatcherEnd => "regexMatcherEnd"
    case RegexOp.MatcherGroup => "regexMatcherGroup"
    case RegexOp.MatcherGroupCount => "regexMatcherGroupCount"
  }

  private def printToStringOp(op: ToStringOp): String = op match {
    case ToStringOp.CharToString => "charToString"
    case ToStringOp.Float32ToString => "float32ToString"
    case ToStringOp.Float64ToString => "float64ToString"
    case ToStringOp.Int8ToString => "int8ToString"
    case ToStringOp.Int16ToString => "int16ToString"
    case ToStringOp.Int32ToString => "int32ToString"
    case ToStringOp.Int64ToString => "int64ToString"
    case ToStringOp.BigIntToString => "bigIntToString"
    case ToStringOp.BigDecimalToString => "bigDecimalToString"
  }

  private def printConvertOp(op: ConvertOp): String = op match {
    case ConvertOp.Int8ToInt16 => "int8ToInt16"
    case ConvertOp.Int8ToInt32 => "int8ToInt32"
    case ConvertOp.Int8ToInt64 => "int8ToInt64"
    case ConvertOp.Int8ToFloat32 => "int8ToFloat32"
    case ConvertOp.Int8ToFloat64 => "int8ToFloat64"
    case ConvertOp.Int16ToInt8 => "int16ToInt8"
    case ConvertOp.Int16ToInt32 => "int16ToInt32"
    case ConvertOp.Int16ToInt64 => "int16ToInt64"
    case ConvertOp.Int16ToFloat32 => "int16ToFloat32"
    case ConvertOp.Int16ToFloat64 => "int16ToFloat64"
    case ConvertOp.Int32ToInt8 => "int32ToInt8"
    case ConvertOp.Int32ToInt16 => "int32ToInt16"
    case ConvertOp.Int32ToInt64 => "int32ToInt64"
    case ConvertOp.Int32ToFloat32 => "int32ToFloat32"
    case ConvertOp.Int32ToFloat64 => "int32ToFloat64"
    case ConvertOp.Int64ToInt8 => "int64ToInt8"
    case ConvertOp.Int64ToInt16 => "int64ToInt16"
    case ConvertOp.Int64ToInt32 => "int64ToInt32"
    case ConvertOp.Int64ToFloat32 => "int64ToFloat32"
    case ConvertOp.Int64ToFloat64 => "int64ToFloat64"
    case ConvertOp.Float32ToInt8 => "float32ToInt8"
    case ConvertOp.Float32ToInt16 => "float32ToInt16"
    case ConvertOp.Float32ToInt32 => "float32ToInt32"
    case ConvertOp.Float32ToInt64 => "float32ToInt64"
    case ConvertOp.Float32ToFloat64 => "float32ToFloat64"
    case ConvertOp.Float64ToInt8 => "float64ToInt8"
    case ConvertOp.Float64ToInt16 => "float64ToInt16"
    case ConvertOp.Float64ToInt32 => "float64ToInt32"
    case ConvertOp.Float64ToInt64 => "float64ToInt64"
    case ConvertOp.Float64ToFloat32 => "float64ToFloat32"
  }

  private def printIoOp(op: IoOp): String = op match {
    case IoOp.Print => "print"
    case IoOp.EPrint => "eprint"
    case IoOp.Readln => "readln"
    case IoOp.Println => "println"
    case IoOp.EPrintln => "eprintln"
    case IoOp.SleepMillis => "sleepMillis"
    case IoOp.Exit => "exit"
    case IoOp.NewId => "newId"
    case IoOp.TimeNowMillis => "timeNowMillis"
    case IoOp.FileExists => "fileExists"
    case IoOp.FileIsDirectory => "fileIsDirectory"
    case IoOp.FileIsRegularFile => "fileIsRegularFile"
    case IoOp.FileIsReadable => "fileIsReadable"
    case IoOp.FileIsSymbolicLink => "fileIsSymbolicLink"
    case IoOp.FileIsWritable => "fileIsWritable"
    case IoOp.FileIsExecutable => "fileIsExecutable"
    case IoOp.FileAccessTime => "fileAccessTime"
    case IoOp.FileCreationTime => "fileCreationTime"
    case IoOp.FileModificationTime => "fileModificationTime"
    case IoOp.FileSize => "fileSize"
    case IoOp.FileRead => "fileRead"
    case IoOp.FileReadLines => "fileReadLines"
    case IoOp.FileReadBytes => "fileReadBytes"
    case IoOp.FileList => "fileList"
    case IoOp.FileWrite => "fileWrite"
    case IoOp.FileWriteBytes => "fileWriteBytes"
    case IoOp.FileAppend => "fileAppend"
    case IoOp.FileAppendBytes => "fileAppendBytes"
    case IoOp.FileTruncate => "fileTruncate"
    case IoOp.FileMkDir => "fileMkDir"
    case IoOp.FileMkDirs => "fileMkDirs"
    case IoOp.FileMkTempDir => "fileMkTempDir"
    case IoOp.TcpSocketRead => "tcpSocketRead"
    case IoOp.TcpSocketWrite => "tcpSocketWrite"
    case IoOp.TcpSocketConnect => "tcpSocketConnect"
    case IoOp.TcpSocketClose => "tcpSocketClose"
    case IoOp.TcpServerBind => "tcpServerBind"
    case IoOp.TcpServerAccept => "tcpServerAccept"
    case IoOp.TcpServerLocalPort => "tcpServerLocalPort"
    case IoOp.TcpServerClose => "tcpServerClose"
    case IoOp.ProcessStdinWrite => "processStdinWrite"
    case IoOp.ProcessExec => "processExec"
    case IoOp.ProcessExitValue => "processExitValue"
    case IoOp.ProcessIsAlive => "processIsAlive"
    case IoOp.ProcessPid => "processPid"
    case IoOp.ProcessStop => "processStop"
    case IoOp.ProcessWaitFor => "processWaitFor"
    case IoOp.ProcessWaitForTimeout => "processWaitForTimeout"
    case IoOp.ProcessStdoutRead => "processStdoutRead"
    case IoOp.ProcessStderrRead => "processStderrRead"
    case IoOp.ProcessRelease => "processRelease"
    case IoOp.HttpRequest => "httpRequest"
    case IoOp.EnvGetArgs => "envGetArgs"
    case IoOp.EnvGetEnvPairs => "envGetEnvPairs"
    case IoOp.EnvGetVar => "envGetVar"
    case IoOp.EnvGetProp => "envGetProp"
    case IoOp.EnvVirtualProcessors => "envVirtualProcessors"
  }

  private def printPlatformOp(op: PlatformOp): String = op match {
    case PlatformOp.FileSeparator => "fileSeparator"
    case PlatformOp.PathSeparator => "pathSeparator"
    case PlatformOp.LineSeparator => "lineSeparator"
  }

  private def printObjectOp(op: ObjectOp): String = op match {
    case ObjectOp.IsNull => "objectIsNull"
  }

  private def printHashOp(op: HashOp): String = op match {
    case HashOp.CharHash => "charHash"
    case HashOp.Float32Hash => "float32Hash"
    case HashOp.Float64Hash => "float64Hash"
    case HashOp.Int8Hash => "int8Hash"
    case HashOp.Int16Hash => "int16Hash"
    case HashOp.Int32Hash => "int32Hash"
    case HashOp.Int64Hash => "int64Hash"
    case HashOp.BigIntHash => "bigIntHash"
    case HashOp.BigDecimalHash => "bigDecimalHash"
    case HashOp.StringHash => "stringHash"
  }

  private def printExnOp(op: ExnOp): String = op match {
    case ExnOp.KindId => "exnKindId"
  }

  /**
    * Returns the [[DocAst.Expr]] representation of `op`.
    */
  def print(op: AtomicOp, ds: List[Expr], tpe: DocAst.Type, eff: DocAst.Type): Expr = (op, ds) match {
    case (AtomicOp.GetStaticField(field), Nil) => JavaGetStaticField(field)
    case (AtomicOp.HoleError(sym), Nil) => HoleError(sym)
    case (AtomicOp.MatchError, Nil) => MatchError
    case (AtomicOp.CastError(_, _), Nil) => CastError
    case (AtomicOp.Unary(sop), List(d)) => Unary(OpPrinter.print(sop), d)
    case (AtomicOp.Binary(sop), List(d1, d2)) => Binary(d1, OpPrinter.print(sop), d2)
    case (AtomicOp.Is(sym), List(d)) => Is(sym, d)
    case (AtomicOp.Tag(sym), _) => Tag(sym, ds)
    case (AtomicOp.Untag(_, idx), List(d)) => Untag(d, idx)
    case (AtomicOp.InstanceOf(clazz), List(d)) => InstanceOf(d, clazz)
    case (AtomicOp.Cast, List(d)) => UncheckedCast(d, Some(tpe), Some(eff))
    case (AtomicOp.Unbox, List(d)) => Unbox(d, tpe)
    case (AtomicOp.Box, List(d)) => Box(d)
    case (AtomicOp.Index(idx), List(d)) => Index(idx, d)
    case (AtomicOp.RecordSelect(label), List(d)) => RecordSelect(label, d)
    case (AtomicOp.RecordRestrict(label), List(d)) => RecordRestrict(label, d)
    case (AtomicOp.ArrayLength, List(d)) => ArrayLength(d)
    case (AtomicOp.StructNew(sym, Mutability.Mutable, fields), d :: rs) =>
      ListOps.zipOption(fields, rs) match {
        case None => Expr.Unknown
        case Some(fs) => Expr.StructNew(sym, fs, Some(d))
      }
    case (AtomicOp.StructNew(sym, Mutability.Immutable, fields), rs) =>
      ListOps.zipOption(fields, rs) match {
        case None => Expr.Unknown
        case Some(fs) => Expr.StructNew(sym, fs, None)
      }
    case (AtomicOp.StructGet(field), List(d)) => Expr.StructGet(d, field)
    case (AtomicOp.StructPut(field), List(d1, d2)) => Expr.StructPut(d1, field, d2)
    case (AtomicOp.Lazy, List(d)) => Lazy(d)
    case (AtomicOp.Force, List(d)) => Force(d)
    case (AtomicOp.GetField(field), List(d)) => JavaGetField(field, d)
    case (AtomicOp.PutStaticField(field), List(d)) => JavaPutStaticField(field, d)
    case (AtomicOp.Closure(sym), _) => ClosureLifted(sym, ds)
    case (AtomicOp.Tuple, _) => Tuple(ds)
    case (AtomicOp.ArrayLit, _) => ArrayLit(ds)
    case (AtomicOp.InvokeConstructor(constructor), _) => JavaInvokeConstructor(constructor, ds)
    case (AtomicOp.InvokeStaticMethod(method), _) => JavaInvokeStaticMethod(method, ds)
    case (AtomicOp.RecordExtend(label), List(d1, d2)) => RecordExtend(label, d1, d2)
    case (AtomicOp.ArrayNew, List(d1, d2)) => ArrayNew(d1, d2)
    case (AtomicOp.ArrayNew, List(_, d1, d2)) => ArrayNew(d1, d2)
    case (AtomicOp.ArrayLoad, List(d1, d2)) => ArrayLoad(d1, d2)
    case (AtomicOp.Spawn, List(d1, d2)) => Spawn(d1, d2)
    case (AtomicOp.PutField(field), List(d1, d2)) => JavaPutField(field, d1, d2)
    case (AtomicOp.ArrayStore, List(d1, d2, d3)) => ArrayStore(d1, d2, d3)
    case (AtomicOp.InvokeMethod(method), d :: rs) => JavaInvokeMethod(method, d, rs)
    // fall back if non other applies
    case (op1, ds1) => App(Meta(op1.toString), ds1)
  }

}

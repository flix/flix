/*
 * Copyright 2023 Magnus Madsen
 * Copyright 2024 Alexander Dybdahl Troelsen
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
package ca.uwaterloo.flix.verifier

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.ReducedAst.*
import ca.uwaterloo.flix.language.ast.shared.{Constant, Mutability}
import ca.uwaterloo.flix.language.ast.{AtomicOp, SemanticOp, SimpleType, SourceLocation, Symbol}
import ca.uwaterloo.flix.util.collection.ListOps
import ca.uwaterloo.flix.util.{InternalCompilerException, ParOps}

import scala.annotation.tailrec

object TypeVerifier {

  /**
    * Verifies that types in the given AST `root` are meaningful.
    *
    * Throws [[InternalCompilerException]] if they are not.
    */
  def verify(root: Root)(implicit flix: Flix): Unit = {
    ParOps.parMap(root.defs.values)(visitDef(_)(root))
  }

  private def visitDef(decl: Def)(implicit root: Root): Unit = {
    val env = (decl.cparams ++ decl.fparams).foldLeft(Map.empty[Symbol.VarSym, SimpleType]) {
      case (macc, fparam) => macc + (fparam.sym -> fparam.tpe)
    }
    val ret = visitExpr(decl.exp)(root, env, Map.empty)
    checkEq(decl.tpe, ret, decl.loc)
  }

  private def visitExpr(expr: Expr)(implicit root: Root, env: Map[Symbol.VarSym, SimpleType], lenv: Map[Symbol.LabelSym, SimpleType]): SimpleType = expr match {
    case Expr.Cst(cst, _) => cst.tpe

    case Expr.NativeImport(_, tpe, _, _) => tpe
    case Expr.WasmImport(_, tpe, _, _) => tpe

    case Expr.Var(sym, tpe1, loc) => env.get(sym) match {
      case None => throw InternalCompilerException(s"Unknown variable sym: '$sym'", sym.loc)
      case Some(tpe2) =>
        checkEq(tpe1, tpe2, loc)
    }

    case Expr.ApplyAtomic(op, exps, tpe, _, loc) =>
      val ts = exps.map(visitExpr)

      op match {
        case AtomicOp.Unary(sop) =>
          val List(t) = ts
          val (argTpe, resTpe) = sop match {
            case SemanticOp.ExnOp.KindId => (t, SimpleType.Int32)
            case SemanticOp.BoolOp.Not => (SimpleType.Bool, SimpleType.Bool)
            case SemanticOp.Float32Op.Neg => (SimpleType.Float32, SimpleType.Float32)
            case SemanticOp.Float64Op.Neg => (SimpleType.Float64, SimpleType.Float64)
            case SemanticOp.Int8Op.Neg => (SimpleType.Int8, SimpleType.Int8)
            case SemanticOp.Int8Op.Not => (SimpleType.Int8, SimpleType.Int8)
            case SemanticOp.Int16Op.Neg => (SimpleType.Int16, SimpleType.Int16)
            case SemanticOp.Int16Op.Not => (SimpleType.Int16, SimpleType.Int16)
            case SemanticOp.Int32Op.Neg => (SimpleType.Int32, SimpleType.Int32)
            case SemanticOp.Int32Op.Not => (SimpleType.Int32, SimpleType.Int32)
            case SemanticOp.Int64Op.Neg => (SimpleType.Int64, SimpleType.Int64)
            case SemanticOp.Int64Op.Not => (SimpleType.Int64, SimpleType.Int64)
            case SemanticOp.BigIntOp.Neg => (SimpleType.BigInt, SimpleType.BigInt)
            case SemanticOp.BigIntOp.Not => (SimpleType.BigInt, SimpleType.BigInt)
            case SemanticOp.BigIntOp.BitLength => (SimpleType.BigInt, SimpleType.Int32)
            case SemanticOp.BigIntOp.FromInt64 => (SimpleType.Int64, SimpleType.BigInt)
            case SemanticOp.BigDecimalOp.Neg => (SimpleType.BigDecimal, SimpleType.BigDecimal)
            case SemanticOp.BigDecimalOp.Scale => (SimpleType.BigDecimal, SimpleType.Int32)
            case SemanticOp.BigDecimalOp.Precision => (SimpleType.BigDecimal, SimpleType.Int32)
            case SemanticOp.BigDecimalOp.Ceil => (SimpleType.BigDecimal, SimpleType.BigDecimal)
            case SemanticOp.BigDecimalOp.Floor => (SimpleType.BigDecimal, SimpleType.BigDecimal)
            case SemanticOp.BigDecimalOp.Round => (SimpleType.BigDecimal, SimpleType.BigDecimal)
            case SemanticOp.BigDecimalOp.ToBigInt => (SimpleType.BigDecimal, SimpleType.BigInt)
            case SemanticOp.BigDecimalOp.ToPlainString => (SimpleType.BigDecimal, SimpleType.String)
            case SemanticOp.CodePointOp.IsLetter => (SimpleType.Int32, SimpleType.Bool)
            case SemanticOp.CodePointOp.IsDigit => (SimpleType.Int32, SimpleType.Bool)
            case SemanticOp.CodePointOp.IsLowerCase => (SimpleType.Int32, SimpleType.Bool)
            case SemanticOp.CodePointOp.IsUpperCase => (SimpleType.Int32, SimpleType.Bool)
            case SemanticOp.CodePointOp.IsTitleCase => (SimpleType.Int32, SimpleType.Bool)
            case SemanticOp.CodePointOp.IsWhitespace => (SimpleType.Int32, SimpleType.Bool)
            case SemanticOp.CodePointOp.IsAlphabetic => (SimpleType.Int32, SimpleType.Bool)
            case SemanticOp.CodePointOp.IsDefined => (SimpleType.Int32, SimpleType.Bool)
            case SemanticOp.CodePointOp.IsIdeographic => (SimpleType.Int32, SimpleType.Bool)
            case SemanticOp.CodePointOp.IsISOControl => (SimpleType.Int32, SimpleType.Bool)
            case SemanticOp.CodePointOp.IsMirrored => (SimpleType.Int32, SimpleType.Bool)
            case SemanticOp.CodePointOp.ToLowerCase => (SimpleType.Int32, SimpleType.Int32)
            case SemanticOp.CodePointOp.ToUpperCase => (SimpleType.Int32, SimpleType.Int32)
            case SemanticOp.CodePointOp.ToTitleCase => (SimpleType.Int32, SimpleType.Int32)
            case SemanticOp.CodePointOp.GetName => (SimpleType.Int32, SimpleType.String)
            case SemanticOp.CodePointOp.GetNumericValue => (SimpleType.Int32, SimpleType.Int32)

            case SemanticOp.ToStringOp.CharToString => (SimpleType.Char, SimpleType.String)
            case SemanticOp.ToStringOp.Float32ToString => (SimpleType.Float32, SimpleType.String)
            case SemanticOp.ToStringOp.Float64ToString => (SimpleType.Float64, SimpleType.String)
            case SemanticOp.ToStringOp.Int8ToString => (SimpleType.Int8, SimpleType.String)
            case SemanticOp.ToStringOp.Int16ToString => (SimpleType.Int16, SimpleType.String)
            case SemanticOp.ToStringOp.Int32ToString => (SimpleType.Int32, SimpleType.String)
            case SemanticOp.ToStringOp.Int64ToString => (SimpleType.Int64, SimpleType.String)
            case SemanticOp.ToStringOp.BigIntToString => (SimpleType.BigInt, SimpleType.String)
            case SemanticOp.ToStringOp.BigDecimalToString => (SimpleType.BigDecimal, SimpleType.String)

            case SemanticOp.ConvertOp.Int8ToInt16 => (SimpleType.Int8, SimpleType.Int16)
            case SemanticOp.ConvertOp.Int8ToInt32 => (SimpleType.Int8, SimpleType.Int32)
            case SemanticOp.ConvertOp.Int8ToInt64 => (SimpleType.Int8, SimpleType.Int64)
            case SemanticOp.ConvertOp.Int8ToFloat32 => (SimpleType.Int8, SimpleType.Float32)
            case SemanticOp.ConvertOp.Int8ToFloat64 => (SimpleType.Int8, SimpleType.Float64)
            case SemanticOp.ConvertOp.Int16ToInt8 => (SimpleType.Int16, SimpleType.Int8)
            case SemanticOp.ConvertOp.Int16ToInt32 => (SimpleType.Int16, SimpleType.Int32)
            case SemanticOp.ConvertOp.Int16ToInt64 => (SimpleType.Int16, SimpleType.Int64)
            case SemanticOp.ConvertOp.Int16ToFloat32 => (SimpleType.Int16, SimpleType.Float32)
            case SemanticOp.ConvertOp.Int16ToFloat64 => (SimpleType.Int16, SimpleType.Float64)
            case SemanticOp.ConvertOp.Int32ToInt8 => (SimpleType.Int32, SimpleType.Int8)
            case SemanticOp.ConvertOp.Int32ToInt16 => (SimpleType.Int32, SimpleType.Int16)
            case SemanticOp.ConvertOp.Int32ToInt64 => (SimpleType.Int32, SimpleType.Int64)
            case SemanticOp.ConvertOp.Int32ToFloat32 => (SimpleType.Int32, SimpleType.Float32)
            case SemanticOp.ConvertOp.Int32ToFloat64 => (SimpleType.Int32, SimpleType.Float64)
            case SemanticOp.ConvertOp.Int64ToInt8 => (SimpleType.Int64, SimpleType.Int8)
            case SemanticOp.ConvertOp.Int64ToInt16 => (SimpleType.Int64, SimpleType.Int16)
            case SemanticOp.ConvertOp.Int64ToInt32 => (SimpleType.Int64, SimpleType.Int32)
            case SemanticOp.ConvertOp.Int64ToFloat32 => (SimpleType.Int64, SimpleType.Float32)
            case SemanticOp.ConvertOp.Int64ToFloat64 => (SimpleType.Int64, SimpleType.Float64)
            case SemanticOp.ConvertOp.Float32ToInt8 => (SimpleType.Float32, SimpleType.Int8)
            case SemanticOp.ConvertOp.Float32ToInt16 => (SimpleType.Float32, SimpleType.Int16)
            case SemanticOp.ConvertOp.Float32ToInt32 => (SimpleType.Float32, SimpleType.Int32)
            case SemanticOp.ConvertOp.Float32ToInt64 => (SimpleType.Float32, SimpleType.Int64)
            case SemanticOp.ConvertOp.Float32ToFloat64 => (SimpleType.Float32, SimpleType.Float64)
            case SemanticOp.ConvertOp.Float64ToInt8 => (SimpleType.Float64, SimpleType.Int8)
            case SemanticOp.ConvertOp.Float64ToInt16 => (SimpleType.Float64, SimpleType.Int16)
            case SemanticOp.ConvertOp.Float64ToInt32 => (SimpleType.Float64, SimpleType.Int32)
            case SemanticOp.ConvertOp.Float64ToInt64 => (SimpleType.Float64, SimpleType.Int64)
            case SemanticOp.ConvertOp.Float64ToFloat32 => (SimpleType.Float64, SimpleType.Float32)

            case SemanticOp.PlatformOp.FileSeparator => (SimpleType.Unit, SimpleType.String)
            case SemanticOp.PlatformOp.PathSeparator => (SimpleType.Unit, SimpleType.String)
            case SemanticOp.PlatformOp.LineSeparator => (SimpleType.Unit, SimpleType.String)

            case SemanticOp.ObjectOp.IsNull => (t, SimpleType.Bool)

            case SemanticOp.StringOp.Length => (SimpleType.String, SimpleType.Int32)
            case SemanticOp.StringOp.ToLowerCase => (SimpleType.String, SimpleType.String)
            case SemanticOp.StringOp.ToUpperCase => (SimpleType.String, SimpleType.String)

            case SemanticOp.ParseOp.Int8FromString => (SimpleType.String, SimpleType.mkTuple(List(SimpleType.Bool, SimpleType.Int8)))
            case SemanticOp.ParseOp.Int16FromString => (SimpleType.String, SimpleType.mkTuple(List(SimpleType.Bool, SimpleType.Int16)))
            case SemanticOp.ParseOp.Int32FromString => (SimpleType.String, SimpleType.mkTuple(List(SimpleType.Bool, SimpleType.Int32)))
            case SemanticOp.ParseOp.Int64FromString => (SimpleType.String, SimpleType.mkTuple(List(SimpleType.Bool, SimpleType.Int64)))
            case SemanticOp.ParseOp.Float32FromString => (SimpleType.String, SimpleType.mkTuple(List(SimpleType.Bool, SimpleType.Float32)))
            case SemanticOp.ParseOp.Float64FromString => (SimpleType.String, SimpleType.mkTuple(List(SimpleType.Bool, SimpleType.Float64)))
            case SemanticOp.ParseOp.BigIntFromString => (SimpleType.String, SimpleType.mkTuple(List(SimpleType.Bool, SimpleType.BigInt)))
            case SemanticOp.ParseOp.BigDecimalFromString => (SimpleType.String, SimpleType.mkTuple(List(SimpleType.Bool, SimpleType.BigDecimal)))
            case SemanticOp.ParseOp.Int32Parse => (SimpleType.mkTuple(List(SimpleType.Int32, SimpleType.String)), SimpleType.mkTuple(List(SimpleType.Bool, SimpleType.Int32)))
            case SemanticOp.ParseOp.Int64Parse => (SimpleType.mkTuple(List(SimpleType.Int32, SimpleType.String)), SimpleType.mkTuple(List(SimpleType.Bool, SimpleType.Int64)))

            case SemanticOp.StringBuilderOp.New => (SimpleType.Region, SimpleType.StringBuilderHandle)
            case SemanticOp.StringBuilderOp.AppendString => (SimpleType.mkTuple(List(SimpleType.Region, SimpleType.StringBuilderHandle, SimpleType.String)), SimpleType.Unit)
            case SemanticOp.StringBuilderOp.AppendCodePoint => (SimpleType.mkTuple(List(SimpleType.Region, SimpleType.StringBuilderHandle, SimpleType.Int32)), SimpleType.Unit)
            case SemanticOp.StringBuilderOp.CharAt => (SimpleType.mkTuple(List(SimpleType.Region, SimpleType.StringBuilderHandle, SimpleType.Int32)), SimpleType.Char)
            case SemanticOp.StringBuilderOp.Length => (SimpleType.mkTuple(List(SimpleType.Region, SimpleType.StringBuilderHandle)), SimpleType.Int32)
            case SemanticOp.StringBuilderOp.SetLength => (SimpleType.mkTuple(List(SimpleType.Region, SimpleType.StringBuilderHandle, SimpleType.Int32)), SimpleType.Unit)
            case SemanticOp.StringBuilderOp.ToString => (SimpleType.mkTuple(List(SimpleType.Region, SimpleType.StringBuilderHandle)), SimpleType.String)

            case SemanticOp.RegexOp.FlagCanonEq => (SimpleType.Unit, SimpleType.Int32)
            case SemanticOp.RegexOp.FlagCaseInsensitive => (SimpleType.Unit, SimpleType.Int32)
            case SemanticOp.RegexOp.FlagComments => (SimpleType.Unit, SimpleType.Int32)
            case SemanticOp.RegexOp.FlagDotall => (SimpleType.Unit, SimpleType.Int32)
            case SemanticOp.RegexOp.FlagLiteral => (SimpleType.Unit, SimpleType.Int32)
            case SemanticOp.RegexOp.FlagMultiline => (SimpleType.Unit, SimpleType.Int32)
            case SemanticOp.RegexOp.FlagUnicodeCase => (SimpleType.Unit, SimpleType.Int32)
            case SemanticOp.RegexOp.FlagUnicodeCharacterClass => (SimpleType.Unit, SimpleType.Int32)
            case SemanticOp.RegexOp.FlagUnixLines => (SimpleType.Unit, SimpleType.Int32)
            case SemanticOp.RegexOp.Compile => (SimpleType.String, SimpleType.Regex)
            case SemanticOp.RegexOp.CompileWithFlags => (SimpleType.mkTuple(List(SimpleType.Int32, SimpleType.String)), SimpleType.Regex)
            case SemanticOp.RegexOp.TryCompile => (SimpleType.String, SimpleType.mkTuple(List(SimpleType.Bool, SimpleType.Regex, SimpleType.String)))
            case SemanticOp.RegexOp.TryCompileWithFlags => (SimpleType.mkTuple(List(SimpleType.Int32, SimpleType.String)), SimpleType.mkTuple(List(SimpleType.Bool, SimpleType.Regex, SimpleType.String)))
            case SemanticOp.RegexOp.Quote => (SimpleType.String, SimpleType.String)
            case SemanticOp.RegexOp.Pattern => (SimpleType.Regex, SimpleType.String)
            case SemanticOp.RegexOp.Flags => (SimpleType.Regex, SimpleType.Int32)
            case SemanticOp.RegexOp.Split => (SimpleType.mkTuple(List(SimpleType.Region, SimpleType.Regex, SimpleType.String)), SimpleType.Array(SimpleType.String))
            case SemanticOp.RegexOp.NewMatcher => (SimpleType.mkTuple(List(SimpleType.Region, SimpleType.Regex, SimpleType.String)), SimpleType.RegexMatcher)
            case SemanticOp.RegexOp.MatcherMatches => (SimpleType.mkTuple(List(SimpleType.Region, SimpleType.RegexMatcher)), SimpleType.Bool)
            case SemanticOp.RegexOp.MatcherFind => (SimpleType.mkTuple(List(SimpleType.Region, SimpleType.RegexMatcher)), SimpleType.Bool)
            case SemanticOp.RegexOp.MatcherFindFrom => (SimpleType.mkTuple(List(SimpleType.Region, SimpleType.RegexMatcher, SimpleType.Int32)), SimpleType.Bool)
            case SemanticOp.RegexOp.MatcherLookingAt => (SimpleType.mkTuple(List(SimpleType.Region, SimpleType.RegexMatcher)), SimpleType.Bool)
            case SemanticOp.RegexOp.MatcherReplaceAll => (SimpleType.mkTuple(List(SimpleType.Region, SimpleType.RegexMatcher, SimpleType.String)), SimpleType.String)
            case SemanticOp.RegexOp.MatcherReplaceFirst => (SimpleType.mkTuple(List(SimpleType.Region, SimpleType.RegexMatcher, SimpleType.String)), SimpleType.String)
            case SemanticOp.RegexOp.MatcherSetBounds => (SimpleType.mkTuple(List(SimpleType.Region, SimpleType.RegexMatcher, SimpleType.Int32, SimpleType.Int32)), SimpleType.Unit)
            case SemanticOp.RegexOp.MatcherStart => (SimpleType.mkTuple(List(SimpleType.Region, SimpleType.RegexMatcher)), SimpleType.Int32)
            case SemanticOp.RegexOp.MatcherEnd => (SimpleType.mkTuple(List(SimpleType.Region, SimpleType.RegexMatcher)), SimpleType.Int32)
            case SemanticOp.RegexOp.MatcherGroup => (SimpleType.mkTuple(List(SimpleType.Region, SimpleType.RegexMatcher, SimpleType.Int32)), SimpleType.String)
            case SemanticOp.RegexOp.MatcherGroupCount => (SimpleType.mkTuple(List(SimpleType.Region, SimpleType.RegexMatcher)), SimpleType.Int32)

            case SemanticOp.CharOp.IsLetter => (SimpleType.Char, SimpleType.Bool)
            case SemanticOp.CharOp.IsDigit => (SimpleType.Char, SimpleType.Bool)
            case SemanticOp.CharOp.IsLetterOrDigit => (SimpleType.Char, SimpleType.Bool)
            case SemanticOp.CharOp.IsLowerCase => (SimpleType.Char, SimpleType.Bool)
            case SemanticOp.CharOp.IsUpperCase => (SimpleType.Char, SimpleType.Bool)
            case SemanticOp.CharOp.IsTitleCase => (SimpleType.Char, SimpleType.Bool)
            case SemanticOp.CharOp.IsWhitespace => (SimpleType.Char, SimpleType.Bool)
            case SemanticOp.CharOp.IsDefined => (SimpleType.Char, SimpleType.Bool)
            case SemanticOp.CharOp.IsISOControl => (SimpleType.Char, SimpleType.Bool)
            case SemanticOp.CharOp.IsMirrored => (SimpleType.Char, SimpleType.Bool)
            case SemanticOp.CharOp.IsSurrogate => (SimpleType.Char, SimpleType.Bool)
            case SemanticOp.CharOp.ToLowerCase => (SimpleType.Char, SimpleType.Char)
            case SemanticOp.CharOp.ToUpperCase => (SimpleType.Char, SimpleType.Char)
            case SemanticOp.CharOp.ToTitleCase => (SimpleType.Char, SimpleType.Char)
            case SemanticOp.CharOp.GetNumericValue => (SimpleType.Char, SimpleType.Int32)

            case SemanticOp.HashOp.CharHash => (SimpleType.Char, SimpleType.Int32)
            case SemanticOp.HashOp.Float32Hash => (SimpleType.Float32, SimpleType.Int32)
            case SemanticOp.HashOp.Float64Hash => (SimpleType.Float64, SimpleType.Int32)
            case SemanticOp.HashOp.Int8Hash => (SimpleType.Int8, SimpleType.Int32)
            case SemanticOp.HashOp.Int16Hash => (SimpleType.Int16, SimpleType.Int32)
            case SemanticOp.HashOp.Int32Hash => (SimpleType.Int32, SimpleType.Int32)
            case SemanticOp.HashOp.Int64Hash => (SimpleType.Int64, SimpleType.Int32)
            case SemanticOp.HashOp.BigIntHash => (SimpleType.BigInt, SimpleType.Int32)
            case SemanticOp.HashOp.BigDecimalHash => (SimpleType.BigDecimal, SimpleType.Int32)
            case SemanticOp.HashOp.StringHash => (SimpleType.String, SimpleType.Int32)

            case SemanticOp.IoOp.Print => (SimpleType.String, SimpleType.Unit)
            case SemanticOp.IoOp.EPrint => (SimpleType.String, SimpleType.Unit)
            case SemanticOp.IoOp.Readln => (SimpleType.Unit, SimpleType.String)
            case SemanticOp.IoOp.Println => (SimpleType.String, SimpleType.Unit)
            case SemanticOp.IoOp.EPrintln => (SimpleType.String, SimpleType.Unit)
            case SemanticOp.IoOp.SleepMillis => (SimpleType.Int64, SimpleType.Unit)
            case SemanticOp.IoOp.Exit => (SimpleType.Int32, SimpleType.Unit)
            case SemanticOp.IoOp.NewId => (SimpleType.Unit, SimpleType.Int64)
            case SemanticOp.IoOp.TimeNowMillis => (SimpleType.Unit, SimpleType.Int64)
            case SemanticOp.IoOp.FileExists => (SimpleType.String, SimpleType.mkTuple(List(SimpleType.Bool, SimpleType.Bool, SimpleType.Int32, SimpleType.String)))
            case SemanticOp.IoOp.FileIsDirectory => (SimpleType.String, SimpleType.mkTuple(List(SimpleType.Bool, SimpleType.Bool, SimpleType.Int32, SimpleType.String)))
            case SemanticOp.IoOp.FileIsRegularFile => (SimpleType.String, SimpleType.mkTuple(List(SimpleType.Bool, SimpleType.Bool, SimpleType.Int32, SimpleType.String)))
            case SemanticOp.IoOp.FileIsReadable => (SimpleType.String, SimpleType.mkTuple(List(SimpleType.Bool, SimpleType.Bool, SimpleType.Int32, SimpleType.String)))
            case SemanticOp.IoOp.FileIsSymbolicLink => (SimpleType.String, SimpleType.mkTuple(List(SimpleType.Bool, SimpleType.Bool, SimpleType.Int32, SimpleType.String)))
            case SemanticOp.IoOp.FileIsWritable => (SimpleType.String, SimpleType.mkTuple(List(SimpleType.Bool, SimpleType.Bool, SimpleType.Int32, SimpleType.String)))
            case SemanticOp.IoOp.FileIsExecutable => (SimpleType.String, SimpleType.mkTuple(List(SimpleType.Bool, SimpleType.Bool, SimpleType.Int32, SimpleType.String)))
            case SemanticOp.IoOp.FileAccessTime => (SimpleType.String, SimpleType.mkTuple(List(SimpleType.Bool, SimpleType.Int64, SimpleType.Int32, SimpleType.String)))
            case SemanticOp.IoOp.FileCreationTime => (SimpleType.String, SimpleType.mkTuple(List(SimpleType.Bool, SimpleType.Int64, SimpleType.Int32, SimpleType.String)))
            case SemanticOp.IoOp.FileModificationTime => (SimpleType.String, SimpleType.mkTuple(List(SimpleType.Bool, SimpleType.Int64, SimpleType.Int32, SimpleType.String)))
            case SemanticOp.IoOp.FileSize => (SimpleType.String, SimpleType.mkTuple(List(SimpleType.Bool, SimpleType.Int64, SimpleType.Int32, SimpleType.String)))
            case SemanticOp.IoOp.FileRead => (SimpleType.String, SimpleType.mkTuple(List(SimpleType.Bool, SimpleType.String, SimpleType.Int32, SimpleType.String)))
            case SemanticOp.IoOp.FileReadLines => (SimpleType.mkTuple(List(SimpleType.Region, SimpleType.String)), SimpleType.mkTuple(List(SimpleType.Bool, SimpleType.Array(SimpleType.String), SimpleType.Int32, SimpleType.String)))
            case SemanticOp.IoOp.FileReadBytes => (SimpleType.mkTuple(List(SimpleType.Region, SimpleType.String)), SimpleType.mkTuple(List(SimpleType.Bool, SimpleType.Array(SimpleType.Int8), SimpleType.Int32, SimpleType.String)))
            case SemanticOp.IoOp.FileList => (SimpleType.mkTuple(List(SimpleType.Region, SimpleType.String)), SimpleType.mkTuple(List(SimpleType.Bool, SimpleType.Array(SimpleType.String), SimpleType.Int32, SimpleType.String)))
            case SemanticOp.IoOp.FileWrite => (SimpleType.mkTuple(List(SimpleType.String, SimpleType.String)), SimpleType.mkTuple(List(SimpleType.Bool, SimpleType.Unit, SimpleType.Int32, SimpleType.String)))
            case SemanticOp.IoOp.FileWriteBytes => (SimpleType.mkTuple(List(SimpleType.Array(SimpleType.Int8), SimpleType.String)), SimpleType.mkTuple(List(SimpleType.Bool, SimpleType.Unit, SimpleType.Int32, SimpleType.String)))
            case SemanticOp.IoOp.FileAppend => (SimpleType.mkTuple(List(SimpleType.String, SimpleType.String)), SimpleType.mkTuple(List(SimpleType.Bool, SimpleType.Unit, SimpleType.Int32, SimpleType.String)))
            case SemanticOp.IoOp.FileAppendBytes => (SimpleType.mkTuple(List(SimpleType.Array(SimpleType.Int8), SimpleType.String)), SimpleType.mkTuple(List(SimpleType.Bool, SimpleType.Unit, SimpleType.Int32, SimpleType.String)))
            case SemanticOp.IoOp.FileTruncate => (SimpleType.String, SimpleType.mkTuple(List(SimpleType.Bool, SimpleType.Unit, SimpleType.Int32, SimpleType.String)))
            case SemanticOp.IoOp.FileMkDir => (SimpleType.String, SimpleType.mkTuple(List(SimpleType.Bool, SimpleType.Unit, SimpleType.Int32, SimpleType.String)))
            case SemanticOp.IoOp.FileMkDirs => (SimpleType.String, SimpleType.mkTuple(List(SimpleType.Bool, SimpleType.Unit, SimpleType.Int32, SimpleType.String)))
            case SemanticOp.IoOp.FileMkTempDir => (SimpleType.String, SimpleType.mkTuple(List(SimpleType.Bool, SimpleType.String, SimpleType.Int32, SimpleType.String)))
            case SemanticOp.IoOp.TcpSocketRead => (SimpleType.mkTuple(List(SimpleType.Int64, SimpleType.Array(SimpleType.Int8))), SimpleType.mkTuple(List(SimpleType.Bool, SimpleType.Int32, SimpleType.String)))
            case SemanticOp.IoOp.TcpSocketWrite => (SimpleType.mkTuple(List(SimpleType.Int64, SimpleType.Array(SimpleType.Int8))), SimpleType.mkTuple(List(SimpleType.Bool, SimpleType.Int32, SimpleType.String)))
            case SemanticOp.IoOp.TcpSocketConnect => (SimpleType.mkTuple(List(SimpleType.Array(SimpleType.Int8), SimpleType.Int32)), SimpleType.mkTuple(List(SimpleType.Bool, SimpleType.Int64, SimpleType.Int32, SimpleType.String)))
            case SemanticOp.IoOp.TcpSocketClose => (SimpleType.Int64, SimpleType.mkTuple(List(SimpleType.Bool, SimpleType.String)))
            case SemanticOp.IoOp.TcpServerBind => (SimpleType.mkTuple(List(SimpleType.Array(SimpleType.Int8), SimpleType.Int32)), SimpleType.mkTuple(List(SimpleType.Bool, SimpleType.Int64, SimpleType.Int32, SimpleType.String)))
            case SemanticOp.IoOp.TcpServerLocalPort => (SimpleType.Int64, SimpleType.mkTuple(List(SimpleType.Bool, SimpleType.Int32, SimpleType.String)))
            case SemanticOp.IoOp.TcpServerAccept => (SimpleType.Int64, SimpleType.mkTuple(List(SimpleType.Bool, SimpleType.Int64, SimpleType.Int32, SimpleType.String)))
            case SemanticOp.IoOp.TcpServerClose => (SimpleType.Int64, SimpleType.mkTuple(List(SimpleType.Bool, SimpleType.String)))
            case SemanticOp.IoOp.ProcessStdinWrite => (SimpleType.mkTuple(List(SimpleType.Int64, SimpleType.Array(SimpleType.Int8))), SimpleType.mkTuple(List(SimpleType.Bool, SimpleType.Int32, SimpleType.Int32, SimpleType.String)))
            case SemanticOp.IoOp.ProcessExec => (SimpleType.mkTuple(List(SimpleType.Array(SimpleType.String), SimpleType.Bool, SimpleType.String, SimpleType.Array(SimpleType.String))), SimpleType.mkTuple(List(SimpleType.Bool, SimpleType.Int64, SimpleType.Int32, SimpleType.String)))
            case SemanticOp.IoOp.ProcessExitValue => (SimpleType.Int64, SimpleType.mkTuple(List(SimpleType.Bool, SimpleType.Int32, SimpleType.Int32, SimpleType.String)))
            case SemanticOp.IoOp.ProcessIsAlive => (SimpleType.Int64, SimpleType.mkTuple(List(SimpleType.Bool, SimpleType.Bool, SimpleType.Int32, SimpleType.String)))
            case SemanticOp.IoOp.ProcessPid => (SimpleType.Int64, SimpleType.mkTuple(List(SimpleType.Bool, SimpleType.Int64, SimpleType.Int32, SimpleType.String)))
            case SemanticOp.IoOp.ProcessStop => (SimpleType.Int64, SimpleType.mkTuple(List(SimpleType.Bool, SimpleType.Unit, SimpleType.Int32, SimpleType.String)))
            case SemanticOp.IoOp.ProcessWaitFor => (SimpleType.Int64, SimpleType.mkTuple(List(SimpleType.Bool, SimpleType.Int32, SimpleType.Int32, SimpleType.String)))
            case SemanticOp.IoOp.ProcessWaitForTimeout => (SimpleType.mkTuple(List(SimpleType.Int64, SimpleType.Int64)), SimpleType.mkTuple(List(SimpleType.Bool, SimpleType.Bool, SimpleType.Int32, SimpleType.String)))
            case SemanticOp.IoOp.ProcessStdoutRead => (SimpleType.mkTuple(List(SimpleType.Int64, SimpleType.Array(SimpleType.Int8))), SimpleType.mkTuple(List(SimpleType.Bool, SimpleType.Int32, SimpleType.Int32, SimpleType.String)))
            case SemanticOp.IoOp.ProcessStderrRead => (SimpleType.mkTuple(List(SimpleType.Int64, SimpleType.Array(SimpleType.Int8))), SimpleType.mkTuple(List(SimpleType.Bool, SimpleType.Int32, SimpleType.Int32, SimpleType.String)))
            case SemanticOp.IoOp.ProcessRelease => (SimpleType.Int64, SimpleType.mkTuple(List(SimpleType.Bool, SimpleType.String)))
            case SemanticOp.IoOp.HttpRequest => (SimpleType.mkTuple(List(SimpleType.String, SimpleType.String, SimpleType.Array(SimpleType.String), SimpleType.Bool, SimpleType.String)), SimpleType.mkTuple(List(SimpleType.Bool, SimpleType.Int32, SimpleType.Array(SimpleType.String), SimpleType.String, SimpleType.Int32, SimpleType.String)))
            case SemanticOp.IoOp.EnvGetArgs => (SimpleType.Region, SimpleType.Array(SimpleType.String))
            case SemanticOp.IoOp.EnvGetEnvPairs => (SimpleType.Region, SimpleType.Array(SimpleType.String))
            case SemanticOp.IoOp.EnvGetVar => (SimpleType.String, SimpleType.String)
            case SemanticOp.IoOp.EnvGetProp => (SimpleType.String, SimpleType.String)
            case SemanticOp.IoOp.EnvVirtualProcessors => (SimpleType.Unit, SimpleType.Int32)
          }
          check(expected = argTpe)(actual = t, loc)
          check(expected = tpe)(actual = resTpe, loc)

        case AtomicOp.Binary(sop) =>
          val List(t1, t2) = ts
          val (argTpe1, argTpe2, resTpe) = sop match {
            case SemanticOp.BoolOp.And => (SimpleType.Bool, SimpleType.Bool, SimpleType.Bool)
            case SemanticOp.BoolOp.Neq => (SimpleType.Bool, SimpleType.Bool, SimpleType.Bool)
            case SemanticOp.BoolOp.Eq => (SimpleType.Bool, SimpleType.Bool, SimpleType.Bool)
            case SemanticOp.BoolOp.Or => (SimpleType.Bool, SimpleType.Bool, SimpleType.Bool)

            case SemanticOp.CharOp.Eq => (SimpleType.Char, SimpleType.Char, SimpleType.Bool)
            case SemanticOp.CharOp.Neq => (SimpleType.Char, SimpleType.Char, SimpleType.Bool)
            case SemanticOp.CharOp.Ge => (SimpleType.Char, SimpleType.Char, SimpleType.Bool)
            case SemanticOp.CharOp.Gt => (SimpleType.Char, SimpleType.Char, SimpleType.Bool)
            case SemanticOp.CharOp.Le => (SimpleType.Char, SimpleType.Char, SimpleType.Bool)
            case SemanticOp.CharOp.Lt => (SimpleType.Char, SimpleType.Char, SimpleType.Bool)
            case SemanticOp.CharOp.Digit => (SimpleType.Char, SimpleType.Int32, SimpleType.Int32)
            case SemanticOp.CharOp.IsSurrogatePair => (SimpleType.Char, SimpleType.Char, SimpleType.Bool)
            case SemanticOp.CharOp.ToCodePoint => (SimpleType.Char, SimpleType.Char, SimpleType.Int32)
            case SemanticOp.CharOp.ForDigit => (SimpleType.Int32, SimpleType.Int32, SimpleType.Char)

            case SemanticOp.Float32Op.Eq => (SimpleType.Float32, SimpleType.Float32, SimpleType.Bool)
            case SemanticOp.Float32Op.Neq => (SimpleType.Float32, SimpleType.Float32, SimpleType.Bool)
            case SemanticOp.Float32Op.Ge => (SimpleType.Float32, SimpleType.Float32, SimpleType.Bool)
            case SemanticOp.Float32Op.Gt => (SimpleType.Float32, SimpleType.Float32, SimpleType.Bool)
            case SemanticOp.Float32Op.Le => (SimpleType.Float32, SimpleType.Float32, SimpleType.Bool)
            case SemanticOp.Float32Op.Lt => (SimpleType.Float32, SimpleType.Float32, SimpleType.Bool)
            case SemanticOp.Float32Op.Add => (SimpleType.Float32, SimpleType.Float32, SimpleType.Float32)
            case SemanticOp.Float32Op.Div => (SimpleType.Float32, SimpleType.Float32, SimpleType.Float32)
            case SemanticOp.Float32Op.Exp => (SimpleType.Float32, SimpleType.Float32, SimpleType.Float32)
            case SemanticOp.Float32Op.Mul => (SimpleType.Float32, SimpleType.Float32, SimpleType.Float32)
            case SemanticOp.Float32Op.Sub => (SimpleType.Float32, SimpleType.Float32, SimpleType.Float32)

            case SemanticOp.Float64Op.Eq => (SimpleType.Float64, SimpleType.Float64, SimpleType.Bool)
            case SemanticOp.Float64Op.Neq => (SimpleType.Float64, SimpleType.Float64, SimpleType.Bool)
            case SemanticOp.Float64Op.Ge => (SimpleType.Float64, SimpleType.Float64, SimpleType.Bool)
            case SemanticOp.Float64Op.Gt => (SimpleType.Float64, SimpleType.Float64, SimpleType.Bool)
            case SemanticOp.Float64Op.Le => (SimpleType.Float64, SimpleType.Float64, SimpleType.Bool)
            case SemanticOp.Float64Op.Lt => (SimpleType.Float64, SimpleType.Float64, SimpleType.Bool)
            case SemanticOp.Float64Op.Add => (SimpleType.Float64, SimpleType.Float64, SimpleType.Float64)
            case SemanticOp.Float64Op.Div => (SimpleType.Float64, SimpleType.Float64, SimpleType.Float64)
            case SemanticOp.Float64Op.Exp => (SimpleType.Float64, SimpleType.Float64, SimpleType.Float64)
            case SemanticOp.Float64Op.Mul => (SimpleType.Float64, SimpleType.Float64, SimpleType.Float64)
            case SemanticOp.Float64Op.Sub => (SimpleType.Float64, SimpleType.Float64, SimpleType.Float64)

            case SemanticOp.Int8Op.Eq => (SimpleType.Int8, SimpleType.Int8, SimpleType.Bool)
            case SemanticOp.Int8Op.Neq => (SimpleType.Int8, SimpleType.Int8, SimpleType.Bool)
            case SemanticOp.Int8Op.Ge => (SimpleType.Int8, SimpleType.Int8, SimpleType.Bool)
            case SemanticOp.Int8Op.Gt => (SimpleType.Int8, SimpleType.Int8, SimpleType.Bool)
            case SemanticOp.Int8Op.Le => (SimpleType.Int8, SimpleType.Int8, SimpleType.Bool)
            case SemanticOp.Int8Op.Lt => (SimpleType.Int8, SimpleType.Int8, SimpleType.Bool)
            case SemanticOp.Int8Op.Add => (SimpleType.Int8, SimpleType.Int8, SimpleType.Int8)
            case SemanticOp.Int8Op.Div => (SimpleType.Int8, SimpleType.Int8, SimpleType.Int8)
            case SemanticOp.Int8Op.Exp => (SimpleType.Int8, SimpleType.Int8, SimpleType.Int8)
            case SemanticOp.Int8Op.Mul => (SimpleType.Int8, SimpleType.Int8, SimpleType.Int8)
            case SemanticOp.Int8Op.Sub => (SimpleType.Int8, SimpleType.Int8, SimpleType.Int8)
            case SemanticOp.Int8Op.Rem => (SimpleType.Int8, SimpleType.Int8, SimpleType.Int8)
            case SemanticOp.Int8Op.And => (SimpleType.Int8, SimpleType.Int8, SimpleType.Int8)
            case SemanticOp.Int8Op.Or => (SimpleType.Int8, SimpleType.Int8, SimpleType.Int8)
            case SemanticOp.Int8Op.Xor => (SimpleType.Int8, SimpleType.Int8, SimpleType.Int8)
            case SemanticOp.Int8Op.Shl => (SimpleType.Int8, SimpleType.Int32, SimpleType.Int8)
            case SemanticOp.Int8Op.Shr => (SimpleType.Int8, SimpleType.Int32, SimpleType.Int8)

            case SemanticOp.Int16Op.Eq => (SimpleType.Int16, SimpleType.Int16, SimpleType.Bool)
            case SemanticOp.Int16Op.Neq => (SimpleType.Int16, SimpleType.Int16, SimpleType.Bool)
            case SemanticOp.Int16Op.Ge => (SimpleType.Int16, SimpleType.Int16, SimpleType.Bool)
            case SemanticOp.Int16Op.Gt => (SimpleType.Int16, SimpleType.Int16, SimpleType.Bool)
            case SemanticOp.Int16Op.Le => (SimpleType.Int16, SimpleType.Int16, SimpleType.Bool)
            case SemanticOp.Int16Op.Lt => (SimpleType.Int16, SimpleType.Int16, SimpleType.Bool)
            case SemanticOp.Int16Op.Add => (SimpleType.Int16, SimpleType.Int16, SimpleType.Int16)
            case SemanticOp.Int16Op.Div => (SimpleType.Int16, SimpleType.Int16, SimpleType.Int16)
            case SemanticOp.Int16Op.Exp => (SimpleType.Int16, SimpleType.Int16, SimpleType.Int16)
            case SemanticOp.Int16Op.Mul => (SimpleType.Int16, SimpleType.Int16, SimpleType.Int16)
            case SemanticOp.Int16Op.Sub => (SimpleType.Int16, SimpleType.Int16, SimpleType.Int16)
            case SemanticOp.Int16Op.Rem => (SimpleType.Int16, SimpleType.Int16, SimpleType.Int16)
            case SemanticOp.Int16Op.And => (SimpleType.Int16, SimpleType.Int16, SimpleType.Int16)
            case SemanticOp.Int16Op.Or => (SimpleType.Int16, SimpleType.Int16, SimpleType.Int16)
            case SemanticOp.Int16Op.Xor => (SimpleType.Int16, SimpleType.Int16, SimpleType.Int16)
            case SemanticOp.Int16Op.Shl => (SimpleType.Int16, SimpleType.Int32, SimpleType.Int16)
            case SemanticOp.Int16Op.Shr => (SimpleType.Int16, SimpleType.Int32, SimpleType.Int16)

            case SemanticOp.Int32Op.Eq => (SimpleType.Int32, SimpleType.Int32, SimpleType.Bool)
            case SemanticOp.Int32Op.Neq => (SimpleType.Int32, SimpleType.Int32, SimpleType.Bool)
            case SemanticOp.Int32Op.Ge => (SimpleType.Int32, SimpleType.Int32, SimpleType.Bool)
            case SemanticOp.Int32Op.Gt => (SimpleType.Int32, SimpleType.Int32, SimpleType.Bool)
            case SemanticOp.Int32Op.Le => (SimpleType.Int32, SimpleType.Int32, SimpleType.Bool)
            case SemanticOp.Int32Op.Lt => (SimpleType.Int32, SimpleType.Int32, SimpleType.Bool)
            case SemanticOp.Int32Op.Add => (SimpleType.Int32, SimpleType.Int32, SimpleType.Int32)
            case SemanticOp.Int32Op.Div => (SimpleType.Int32, SimpleType.Int32, SimpleType.Int32)
            case SemanticOp.Int32Op.Exp => (SimpleType.Int32, SimpleType.Int32, SimpleType.Int32)
            case SemanticOp.Int32Op.Mul => (SimpleType.Int32, SimpleType.Int32, SimpleType.Int32)
            case SemanticOp.Int32Op.Sub => (SimpleType.Int32, SimpleType.Int32, SimpleType.Int32)
            case SemanticOp.Int32Op.Rem => (SimpleType.Int32, SimpleType.Int32, SimpleType.Int32)
            case SemanticOp.Int32Op.And => (SimpleType.Int32, SimpleType.Int32, SimpleType.Int32)
            case SemanticOp.Int32Op.Or => (SimpleType.Int32, SimpleType.Int32, SimpleType.Int32)
            case SemanticOp.Int32Op.Xor => (SimpleType.Int32, SimpleType.Int32, SimpleType.Int32)
            case SemanticOp.Int32Op.Shl => (SimpleType.Int32, SimpleType.Int32, SimpleType.Int32)
            case SemanticOp.Int32Op.Shr => (SimpleType.Int32, SimpleType.Int32, SimpleType.Int32)

            case SemanticOp.Int64Op.Eq => (SimpleType.Int64, SimpleType.Int64, SimpleType.Bool)
            case SemanticOp.Int64Op.Neq => (SimpleType.Int64, SimpleType.Int64, SimpleType.Bool)
            case SemanticOp.Int64Op.Ge => (SimpleType.Int64, SimpleType.Int64, SimpleType.Bool)
            case SemanticOp.Int64Op.Gt => (SimpleType.Int64, SimpleType.Int64, SimpleType.Bool)
            case SemanticOp.Int64Op.Le => (SimpleType.Int64, SimpleType.Int64, SimpleType.Bool)
            case SemanticOp.Int64Op.Lt => (SimpleType.Int64, SimpleType.Int64, SimpleType.Bool)
            case SemanticOp.Int64Op.Add => (SimpleType.Int64, SimpleType.Int64, SimpleType.Int64)
            case SemanticOp.Int64Op.Div => (SimpleType.Int64, SimpleType.Int64, SimpleType.Int64)
            case SemanticOp.Int64Op.Exp => (SimpleType.Int64, SimpleType.Int64, SimpleType.Int64)
            case SemanticOp.Int64Op.Mul => (SimpleType.Int64, SimpleType.Int64, SimpleType.Int64)
            case SemanticOp.Int64Op.Sub => (SimpleType.Int64, SimpleType.Int64, SimpleType.Int64)
            case SemanticOp.Int64Op.Rem => (SimpleType.Int64, SimpleType.Int64, SimpleType.Int64)
            case SemanticOp.Int64Op.And => (SimpleType.Int64, SimpleType.Int64, SimpleType.Int64)
            case SemanticOp.Int64Op.Or => (SimpleType.Int64, SimpleType.Int64, SimpleType.Int64)
            case SemanticOp.Int64Op.Xor => (SimpleType.Int64, SimpleType.Int64, SimpleType.Int64)
            case SemanticOp.Int64Op.Shl => (SimpleType.Int64, SimpleType.Int32, SimpleType.Int64)
            case SemanticOp.Int64Op.Shr => (SimpleType.Int64, SimpleType.Int32, SimpleType.Int64)

            case SemanticOp.BigIntOp.Add => (SimpleType.BigInt, SimpleType.BigInt, SimpleType.BigInt)
            case SemanticOp.BigIntOp.Sub => (SimpleType.BigInt, SimpleType.BigInt, SimpleType.BigInt)
            case SemanticOp.BigIntOp.Mul => (SimpleType.BigInt, SimpleType.BigInt, SimpleType.BigInt)
            case SemanticOp.BigIntOp.Div => (SimpleType.BigInt, SimpleType.BigInt, SimpleType.BigInt)
            case SemanticOp.BigIntOp.Rem => (SimpleType.BigInt, SimpleType.BigInt, SimpleType.BigInt)
            case SemanticOp.BigIntOp.And => (SimpleType.BigInt, SimpleType.BigInt, SimpleType.BigInt)
            case SemanticOp.BigIntOp.Or => (SimpleType.BigInt, SimpleType.BigInt, SimpleType.BigInt)
            case SemanticOp.BigIntOp.Xor => (SimpleType.BigInt, SimpleType.BigInt, SimpleType.BigInt)
            case SemanticOp.BigIntOp.Shl => (SimpleType.BigInt, SimpleType.Int32, SimpleType.BigInt)
            case SemanticOp.BigIntOp.Shr => (SimpleType.BigInt, SimpleType.Int32, SimpleType.BigInt)
            case SemanticOp.BigIntOp.Cmp => (SimpleType.BigInt, SimpleType.BigInt, SimpleType.Int32)

            case SemanticOp.BigDecimalOp.Add => (SimpleType.BigDecimal, SimpleType.BigDecimal, SimpleType.BigDecimal)
            case SemanticOp.BigDecimalOp.Sub => (SimpleType.BigDecimal, SimpleType.BigDecimal, SimpleType.BigDecimal)
            case SemanticOp.BigDecimalOp.Mul => (SimpleType.BigDecimal, SimpleType.BigDecimal, SimpleType.BigDecimal)
            case SemanticOp.BigDecimalOp.Div => (SimpleType.BigDecimal, SimpleType.BigDecimal, SimpleType.BigDecimal)
            case SemanticOp.BigDecimalOp.Cmp => (SimpleType.BigDecimal, SimpleType.BigDecimal, SimpleType.Int32)

            case SemanticOp.StringOp.CharAt => (SimpleType.String, SimpleType.Int32, SimpleType.Char)
            case SemanticOp.StringOp.Repeat => (SimpleType.String, SimpleType.Int32, SimpleType.String)
            case SemanticOp.StringOp.Concat => (SimpleType.String, SimpleType.String, SimpleType.String)
          }
          check(expected = argTpe1)(t1, loc)
          check(expected = argTpe2)(t2, loc)
          check(expected = tpe)(actual = resTpe, loc)

        case AtomicOp.Is(sym) =>
          val List(t1) = ts
          t1 match {
            case SimpleType.Enum(enumSym, _) if enumSym == sym.enumSym => ()
            case _ => failMismatchedShape(t1, sym.enumSym.toString, loc)
          }
          check(expected = SimpleType.Bool)(actual = tpe, loc)

        case AtomicOp.Tag(sym) =>
          // Checking this requires instantiating the enum case
          tpe match {
            case SimpleType.Enum(enumSym, _) if enumSym == sym.enumSym => ()
            case _ => failMismatchedShape(tpe, sym.enumSym.toString, loc)
          }
          tpe

        case AtomicOp.Untag(sym, _) =>
          val List(t1) = ts
          // Untag(Nil): Unit
          // Checking this requires instantiating the enum case
          t1 match {
            case SimpleType.Enum(enumSym, _) if enumSym == sym.enumSym => ()
            case _ => failMismatchedShape(t1, sym.enumSym.toString, loc)
          }
          tpe

        case AtomicOp.ArrayLength =>
          val List(t1) = ts
          t1 match {
            case SimpleType.Array(_) => check(expected = SimpleType.Int32)(actual = tpe, loc)
            case _ => failMismatchedShape(t1, "Array", loc)
          }

        case AtomicOp.StructNew(sym0, Mutability.Mutable, _) =>
          ts match {
            case region :: _ =>
              checkStructType(tpe, sym0, loc)
              check(SimpleType.Region)(region, exps.head.loc)
              tpe
            case _ => throw InternalCompilerException(s"Struct $sym0 missing region tparam", loc)
          }

        case AtomicOp.StructNew(sym0, Mutability.Immutable, _) =>
          checkStructType(tpe, sym0, loc)
          tpe

        case AtomicOp.StructGet(sym0) =>
          ts match {
            case tpe1 :: Nil =>
              checkStructType(tpe1, sym0.structSym, loc)
              tpe
            case _ => failMismatchedShape(tpe, "Struct", loc)
          }

        case AtomicOp.StructPut(sym0) =>
          ts match {
            case tpe1 :: _ :: Nil =>
              checkStructType(tpe1, sym0.structSym, loc)
              tpe
            case _ => failMismatchedShape(tpe, "Struct", loc)
          }

        case AtomicOp.ArrayNew =>
          val List(t1, t2) = ts
          val arrType = SimpleType.mkArray(t1)
          checkEq(arrType, tpe, loc)
          check(expected = SimpleType.Int32)(actual = t2, loc)
          tpe

        case AtomicOp.ArrayLit =>
          tpe match {
            case SimpleType.Array(elmt) =>
              ts.foreach(t => checkEq(elmt, t, loc))
              tpe
            case _ => failMismatchedShape(tpe, "Array", loc)
          }

        case AtomicOp.ArrayLoad =>
          val List(t1, t2) = ts
          t1 match {
            case SimpleType.Array(elmt) =>
              check(expected = SimpleType.Int32)(actual = t2, loc)
              checkEq(elmt, tpe, loc)
            case _ => failMismatchedShape(t1, "Array", loc)
          }

        case AtomicOp.ArrayStore =>
          val List(t1, t2, t3) = ts
          t1 match {
            case SimpleType.Array(elmt) =>
              check(expected = SimpleType.Int32)(actual = t2, loc)
              checkEq(elmt, t3, loc)
              check(expected = SimpleType.Unit)(actual = tpe, loc)
            case _ => failMismatchedShape(t1, "Array", loc)
          }

        case AtomicOp.Lazy =>
          val List(t1) = ts
          tpe match {
            case SimpleType.Lazy(elmt) =>
              val fun = SimpleType.mkArrow(List(SimpleType.Unit), elmt)
              checkEq(t1, fun, loc)
              tpe
            case _ => failMismatchedShape(tpe, "Lazy", loc)
          }

        case AtomicOp.Force =>
          val List(t1) = ts
          t1 match {
            case SimpleType.Lazy(elm) => checkEq(elm, tpe, loc)
            case _ => failMismatchedShape(t1, "Lazy", loc)
          }

        case AtomicOp.Tuple =>
          val tup = SimpleType.mkTuple(ts)
          checkEq(tup, tpe, loc)

        case AtomicOp.Index(idx: Int) =>
          val List(t1) = ts
          t1 match {
            case SimpleType.Tuple(elms) => checkEq(elms(idx), tpe, loc)
            case _ => failMismatchedShape(t1, "Tuple", loc)
          }

        // Match- and Hole-errors match with any type
        case AtomicOp.HoleError(_) =>
          tpe

        case AtomicOp.MatchError =>
          tpe

        case AtomicOp.CastError(_, _) =>
          tpe

        case AtomicOp.RecordExtend(label) =>
          val List(t1, t2) = ts
          removeFromRecordType(tpe, label.name, loc) match {
            case (rec, Some(valtype)) =>
              checkEq(rec, t2, loc)
              checkEq(valtype, t1, loc)
              tpe
            case (_, None) => failMismatchedShape(tpe, s"Record with ${label.name}", loc)
          }

        case AtomicOp.RecordRestrict(label) =>
          val List(t1) = ts
          removeFromRecordType(t1, label.name, loc) match {
            case (rec, Some(_)) =>
              checkEq(tpe, rec, loc)
            case (_, None) => failMismatchedShape(t1, s"Record with ${label.name}", loc)
          }

        case AtomicOp.RecordSelect(label) =>
          val List(t1) = ts
          selectFromRecordType(t1, label.name, loc) match {
            case Some(elmt) =>
              checkEq(tpe, elmt, loc)
            case None => failMismatchedShape(t1, s"Record with '${label.name}'", loc)
          }

        case AtomicOp.ExtIs(label) =>
          val List(t1) = ts
          getExtensibleTagType(t1, label.name, loc) match {
            case Some(_) => ()
            case None => failMismatchedShape(t1, label.name, loc)
          }
          check(expected = SimpleType.Bool)(actual = tpe, loc)

        case AtomicOp.ExtTag(label) =>
          getExtensibleTagType(tpe, label.name, loc) match {
            case Some(ts2) if ts.length == ts2.length =>
              ListOps.zip(ts, ts2).map { case (t1, t2) => checkEq(t1, t2, loc) }
              tpe
            case _ =>
              failMismatchedShape(tpe, label.name, loc)
          }

        case AtomicOp.ExtUntag(label, idx) =>
          val List(t1) = ts
          val termTypes = SimpleType.findExtensibleTermTypes(label, t1)
          checkEq(termTypes(idx), tpe, loc)

        case AtomicOp.Closure(sym) =>
          val defn = root.defs(sym)
          val signature = SimpleType.mkArrow(defn.fparams.map(_.tpe), defn.tpe)

          val decl = SimpleType.mkArrow(defn.cparams.map(_.tpe), signature)
          val actual = SimpleType.mkArrow(ts, tpe)

          checkEq(decl, actual, loc)
          tpe

        case AtomicOp.Box =>
          // Boxing is used for casts from primitives into an erased "any" value type.
          // Depending on the cast target, the result is either `Object` (JVM erased type)
          // or `AnyType` (portable erased value type).
          tpe match {
            case SimpleType.AnyType => tpe
            case SimpleType.Native(clazz) if clazz == classOf[java.lang.Object] => tpe
            case _ => failUnexpectedType(tpe, SimpleType.Object, loc)
          }

        case AtomicOp.Unbox =>
          val List(t1) = ts
          t1 match {
            case SimpleType.AnyType => ()
            case SimpleType.Native(clazz) if clazz == classOf[java.lang.Object] => ()
            case _ => failUnexpectedType(t1, SimpleType.Object, loc)
          }
          tpe

        // cast may result in any type
        case AtomicOp.Cast =>
          tpe

        case AtomicOp.Spawn =>
          val List(t1, t2) = ts
          t1 match {
            case SimpleType.Arrow(List(SimpleType.Unit), _) => ()
            case _ => failMismatchedShape(t1, "Arrow(List(Unit), _)", loc)
          }

          check(expected = SimpleType.Region)(actual = t2, loc)
          check(expected = SimpleType.Unit)(actual = tpe, loc)

        case AtomicOp.ChannelNew =>
          val List(t1) = ts
          check(expected = SimpleType.Int32)(actual = t1, loc)
          check(expected = SimpleType.ChannelHandle)(actual = tpe, loc)

        case AtomicOp.ChannelGet =>
          val List(t1) = ts
          check(expected = SimpleType.ChannelHandle)(actual = t1, loc)
          tpe

        case AtomicOp.ChannelPut =>
          val List(t1, _) = ts
          check(expected = SimpleType.ChannelHandle)(actual = t1, loc)
          check(expected = SimpleType.Unit)(actual = tpe, loc)

        case AtomicOp.ChannelSelect =>
          if (ts.isEmpty) {
            throw InternalCompilerException("Unexpected arity for ChannelSelect", loc)
          }
          val (channels, rest) = ts.splitAt(ts.length - 1)
          channels.foreach(t => check(expected = SimpleType.ChannelHandle)(actual = t, loc))
          rest match {
            case List(blocking) =>
              check(expected = SimpleType.Bool)(actual = blocking, loc)
            case _ =>
              throw InternalCompilerException(s"Unexpected arity for ChannelSelect: ${ts.length}", loc)
          }
          check(expected = SimpleType.Int64)(actual = tpe, loc)

        case AtomicOp.ChannelSelectIndex =>
          val List(tokenTpe) = ts
          check(expected = SimpleType.Int64)(actual = tokenTpe, loc)
          check(expected = SimpleType.Int32)(actual = tpe, loc)

        case AtomicOp.ChannelSelectGet =>
          val List(tokenTpe) = ts
          check(expected = SimpleType.Int64)(actual = tokenTpe, loc)
          tpe

        case AtomicOp.ReentrantLockNew =>
          check(expected = SimpleType.ReentrantLockHandle)(actual = tpe, loc)

        case AtomicOp.ReentrantLockLock =>
          val List(lockTpe) = ts
          check(expected = SimpleType.ReentrantLockHandle)(actual = lockTpe, loc)
          check(expected = SimpleType.Unit)(actual = tpe, loc)

        case AtomicOp.ReentrantLockTryLock =>
          val List(lockTpe) = ts
          check(expected = SimpleType.ReentrantLockHandle)(actual = lockTpe, loc)
          check(expected = SimpleType.Bool)(actual = tpe, loc)

        case AtomicOp.ReentrantLockUnlock =>
          val List(lockTpe) = ts
          check(expected = SimpleType.ReentrantLockHandle)(actual = lockTpe, loc)
          check(expected = SimpleType.Bool)(actual = tpe, loc)

        case AtomicOp.ConditionNew =>
          val List(lockTpe) = ts
          check(expected = SimpleType.ReentrantLockHandle)(actual = lockTpe, loc)
          check(expected = SimpleType.ConditionHandle)(actual = tpe, loc)

        case AtomicOp.ConditionAwait =>
          val List(conditionTpe) = ts
          check(expected = SimpleType.ConditionHandle)(actual = conditionTpe, loc)
          check(expected = SimpleType.Int32)(actual = tpe, loc)

        case AtomicOp.ConditionSignal =>
          val List(conditionTpe) = ts
          check(expected = SimpleType.ConditionHandle)(actual = conditionTpe, loc)
          check(expected = SimpleType.Bool)(actual = tpe, loc)

        case AtomicOp.ConditionSignalAll =>
          val List(conditionTpe) = ts
          check(expected = SimpleType.ConditionHandle)(actual = conditionTpe, loc)
          check(expected = SimpleType.Bool)(actual = tpe, loc)

        case AtomicOp.CyclicBarrierNew =>
          val List(partiesTpe) = ts
          check(expected = SimpleType.Int32)(actual = partiesTpe, loc)
          check(expected = SimpleType.CyclicBarrierHandle)(actual = tpe, loc)

        case AtomicOp.CyclicBarrierAwait =>
          val List(barrierTpe) = ts
          check(expected = SimpleType.CyclicBarrierHandle)(actual = barrierTpe, loc)
          check(expected = SimpleType.Int32)(actual = tpe, loc)

        case AtomicOp.CountDownLatchNew =>
          val List(countTpe) = ts
          check(expected = SimpleType.Int32)(actual = countTpe, loc)
          check(expected = SimpleType.CountDownLatchHandle)(actual = tpe, loc)

        case AtomicOp.CountDownLatchAwait =>
          val List(latchTpe) = ts
          check(expected = SimpleType.CountDownLatchHandle)(actual = latchTpe, loc)
          check(expected = SimpleType.Unit)(actual = tpe, loc)

        case AtomicOp.CountDownLatchCountDown =>
          val List(latchTpe) = ts
          check(expected = SimpleType.CountDownLatchHandle)(actual = latchTpe, loc)
          check(expected = SimpleType.Unit)(actual = tpe, loc)

        case AtomicOp.SemaphoreNew =>
          val List(permitsTpe) = ts
          check(expected = SimpleType.Int32)(actual = permitsTpe, loc)
          check(expected = SimpleType.SemaphoreHandle)(actual = tpe, loc)

        case AtomicOp.SemaphoreAcquire =>
          val List(semTpe) = ts
          check(expected = SimpleType.SemaphoreHandle)(actual = semTpe, loc)
          check(expected = SimpleType.Unit)(actual = tpe, loc)

        case AtomicOp.SemaphoreTryAcquire =>
          val List(semTpe) = ts
          check(expected = SimpleType.SemaphoreHandle)(actual = semTpe, loc)
          check(expected = SimpleType.Bool)(actual = tpe, loc)

        case AtomicOp.SemaphoreRelease =>
          val List(semTpe) = ts
          check(expected = SimpleType.SemaphoreHandle)(actual = semTpe, loc)
          check(expected = SimpleType.Unit)(actual = tpe, loc)

        case AtomicOp.GetField(field) =>
          val List(t) = ts
          checkJavaSubtype(t, field.getDeclaringClass, loc)
          checkJavaSubtype(tpe, field.getType, loc)

        case AtomicOp.GetStaticField(field) =>
          checkJavaSubtype(tpe, field.getType, loc)

        case AtomicOp.PutField(field) =>
          val List(t1, t2) = ts
          checkJavaSubtype(t1, field.getDeclaringClass, loc)
          checkJavaSubtype(t2, field.getType, loc)
          check(expected = SimpleType.Unit)(actual = tpe, loc)

        case AtomicOp.PutStaticField(field) =>
          val List(t) = ts
          checkJavaSubtype(t, field.getType, loc)
          check(expected = SimpleType.Unit)(actual = tpe, loc)

        case AtomicOp.Throw =>
          val List(t) = ts
          t match {
            case SimpleType.Native(clazz) =>
              if (!classOf[Throwable].isAssignableFrom(clazz)) {
                failMismatchedTypes(t, classOf[Throwable], loc)
              }
            case SimpleType.Enum(sym, _) if sym.text == "Exn" && sym.namespace.isEmpty => ()
            case _ => failMismatchedTypes(t, classOf[Throwable], loc)
          }
          tpe

        case AtomicOp.InstanceOf(_) =>
          val List(t) = ts
          checkJavaSubtype(t, new Object().getClass, loc) // must not be primitive type
          check(expected = SimpleType.Bool)(actual = tpe, loc)

        case AtomicOp.InvokeConstructor(constructor) =>
          checkJavaParameters(ts, constructor.getParameterTypes.toList, loc)
          checkJavaSubtype(tpe, constructor.getDeclaringClass, loc)

        case AtomicOp.InvokeMethod(method) =>
          val t :: pts = ts
          checkJavaParameters(pts, method.getParameterTypes.toList, loc)
          checkJavaSubtype(t, method.getDeclaringClass, loc)
          checkJavaSubtype(tpe, method.getReturnType, loc)

        case AtomicOp.InvokeStaticMethod(method) =>
          checkJavaParameters(ts, method.getParameterTypes.toList, loc)
          checkJavaSubtype(tpe, method.getReturnType, loc)
      }

    case Expr.ApplyClo(exp1, exp2, _, tpe, _, loc) =>
      val lamType1 = visitExpr(exp1)
      val lamType2 = SimpleType.mkArrow(List(visitExpr(exp2)), tpe)
      checkEq(lamType1, lamType2, loc)
      tpe

    case Expr.ApplyDef(sym, exps, _, tpe, _, loc) =>
      val defn = root.defs(sym)
      val declared = SimpleType.mkArrow(defn.fparams.map(_.tpe), defn.tpe)
      val actual = SimpleType.mkArrow(exps.map(visitExpr), tpe)
      check(expected = declared)(actual = actual, loc)
      tpe

    case Expr.ApplyOp(sym, exps, tpe, _, loc) =>
      val ts = exps.map(visitExpr)
      val eff = root.effects.getOrElse(sym.eff,
        throw InternalCompilerException(s"Unknown effect sym: '${sym.eff}'", sym.loc))
      val op = eff.ops.find(_.sym == sym)
        .getOrElse(throw InternalCompilerException(s"Unknown operation sym: '${sym}'", sym.loc))

      val oprestype = op.tpe match {
        case SimpleType.Void => tpe // should match any return type
        case t => t
      }

      val sig = SimpleType.mkArrow(ts, tpe)
      val opsig = SimpleType.mkArrow(
        op.fparams.map(_.tpe), oprestype
      )

      checkEq(sig, opsig, loc)
      tpe

    case Expr.ApplySelfTail(sym, actuals, tpe, _, loc) =>
      val defn = root.defs(sym)
      val declared = SimpleType.mkArrow(defn.fparams.map(_.tpe), defn.tpe)
      val actual = SimpleType.mkArrow(actuals.map(visitExpr), tpe)
      check(expected = declared)(actual = actual, loc)
      tpe

    case Expr.IfThenElse(exp1, exp2, exp3, tpe, _, _) =>
      val condType = visitExpr(exp1)
      val thenType = visitExpr(exp2)
      val elseType = visitExpr(exp3)
      check(expected = SimpleType.Bool)(actual = condType, exp1.loc)
      checkEq(tpe, thenType, exp2.loc)
      checkEq(tpe, elseType, exp3.loc)

    case Expr.Branch(_, branches, tpe, _, loc) =>
      val lenv1 = branches.foldLeft(lenv) {
        case (acc, (label, _)) => acc + (label -> tpe)
      }
      branches.foreach {
        case (_, body) =>
          checkEq(tpe, visitExpr(body)(root, env, lenv1), loc)
      }
      tpe

    case Expr.JumpTo(sym, tpe1, _, loc) => lenv.get(sym) match {
      case None => throw InternalCompilerException(s"Unknown label sym: '$sym'.", loc)
      case Some(tpe2) => checkEq(tpe1, tpe2, loc)
    }

    case Expr.Let(sym, exp1, exp2, _) =>
      val letBoundType = visitExpr(exp1)
      visitExpr(exp2)(root, env + (sym -> letBoundType), lenv)

    case Expr.Stmt(exp1, exp2, _) =>
      // Visit `exp1` to check types inside.
      visitExpr(exp1)
      visitExpr(exp2)

    case Expr.Region(sym, exp, tpe, _, loc) =>
      checkEq(tpe, visitExpr(exp)(root, env + (sym -> SimpleType.Region), lenv), loc)

    case Expr.TryCatch(exp, rules, tpe, _, loc) =>
      lazy val exnTpe: SimpleType = root.enums.keys.find(sym => sym.text == "Exn" && sym.namespace.isEmpty) match {
        case Some(sym) => SimpleType.mkEnum(sym, Nil)
        case None => throw InternalCompilerException("Missing enum symbol: Exn.", loc)
      }
      for (CatchRule(sym, catchTpe, exp) <- rules) {
        val binderTpe = catchTpe match {
          case SimpleType.Native(clazz) => SimpleType.Native(clazz)
          case _ => exnTpe
        }
        checkEq(tpe, visitExpr(exp)(root, env + (sym -> binderTpe), lenv), exp.loc)
      }
      val t = visitExpr(exp)
      checkEq(tpe, t, loc)

    case Expr.RunWith(exp, effUse, rules, _, tpe, _, loc) =>
      val exptype = visitExpr(exp) match {
        case SimpleType.Arrow(List(SimpleType.Unit), t) => t
        case e => failMismatchedShape(e, "Arrow(List(Unit), _)", exp.loc)
      }

      val effect = root.effects.getOrElse(effUse.sym,
        throw InternalCompilerException(s"Unknown effect sym: '${effUse.sym}'", effUse.qname.loc))
      val ops = effect.ops.map(op => op.sym -> op).toMap

      for (rule <- rules) {
        val ruletype = visitExpr(rule.exp)
        val op = ops.getOrElse(rule.op.sym,
          throw InternalCompilerException(s"Unknown operation sym: '${rule.op.sym}'", rule.op.loc))

        val params = op.fparams.map(_.tpe)
        val resumptionType = SimpleType.mkArrow(List(op.tpe), exptype)
        val signature = SimpleType.mkArrow(params :+ resumptionType, exptype)

        checkEq(ruletype, signature, rule.exp.loc)
      }

      checkEq(tpe, exptype, loc)

    case Expr.NewObject(_, clazz, tpe, _, methods, loc) =>
      for (m <- methods) {
        val exptype = visitExpr(m.exp)
        val signature = SimpleType.mkArrow(m.fparams.map(_.tpe), m.tpe)
        checkEq(signature, exptype, m.loc)
      }
      checkEq(tpe, SimpleType.Native(clazz), loc)

  }

  /**
    * Asserts that the given type `expected` is equal to the `actual` type.
    */
  private def check(expected: SimpleType)(actual: SimpleType, loc: SourceLocation): SimpleType = {
    if (expected == actual)
      expected
    else failUnexpectedType(actual, expected, loc)
  }

  /**
    * Asserts that the two given types `tpe1` and `tpe2` are the same.
    */
  private def checkEq(tpe1: SimpleType, tpe2: SimpleType, loc: SourceLocation): SimpleType = {
    if (tpe1 == tpe2)
      tpe1
    else failMismatchedTypes(tpe1, tpe2, loc)
  }

  /**
    * Asserts that the list of types `ts` matches the list of java classes `cs`
    */
  private def checkJavaParameters(ts: List[SimpleType], cs: List[Class[?]], loc: SourceLocation): Unit = {
    ListOps.zipOption(ts, cs) match {
      case None => throw InternalCompilerException("Number of types in constructor call mismatch with parameter list", loc)
      case Some(zipped) => zipped.foreach { case (tp, klazz) => checkJavaSubtype(tp, klazz, loc) }
    }
  }

  /**
    * Asserts that the type `tpe` is a `Struct` type whose name is `sym0`
    */
  private def checkStructType(tpe: SimpleType, sym0: Symbol.StructSym, loc: SourceLocation): Unit = {
    tpe match {
      case SimpleType.Struct(sym, _) =>
        if (sym0 != sym) {
          throw InternalCompilerException(s"Expected struct type $sym0, got struct type $sym", loc)
        }
      case _ => failMismatchedShape(tpe, "Struct", loc)
    }
  }

  /**
    * Asserts that `tpe` is a subtype of the java class type `klazz`.
    */
  private def checkJavaSubtype(tpe: SimpleType, klazz: Class[?], loc: SourceLocation): SimpleType = {
    tpe match {
      case SimpleType.Array(elmt) if klazz.isArray =>
        checkJavaSubtype(elmt, klazz.getComponentType, loc)
        tpe

      case SimpleType.Native(k) if klazz.isAssignableFrom(k) =>
        tpe

      case SimpleType.Int8 if klazz == classOf[Byte] => tpe
      case SimpleType.Int16 if klazz == classOf[Short] => tpe
      case SimpleType.Int32 if klazz == classOf[Int] => tpe
      case SimpleType.Int64 if klazz == classOf[Long] => tpe
      case SimpleType.Float32 if klazz == classOf[Float] => tpe
      case SimpleType.Float64 if klazz == classOf[Double] => tpe
      case SimpleType.Bool if klazz == classOf[Boolean] => tpe
      case SimpleType.Char if klazz == classOf[Char] => tpe
      case SimpleType.Unit if klazz == classOf[Unit] => tpe
      case SimpleType.Null if !klazz.isPrimitive => tpe

      case SimpleType.String if klazz.isAssignableFrom(classOf[java.lang.String]) => tpe
      case SimpleType.BigInt if klazz.isAssignableFrom(classOf[java.math.BigInteger]) => tpe
      case SimpleType.BigDecimal if klazz.isAssignableFrom(classOf[java.math.BigDecimal]) => tpe
      case SimpleType.Regex if klazz.isAssignableFrom(classOf[java.util.regex.Pattern]) => tpe
      case SimpleType.Arrow(List(SimpleType.Object), SimpleType.Unit) if klazz.isAssignableFrom(classOf[java.util.function.Consumer[Object]]) => tpe
      case SimpleType.Arrow(List(SimpleType.Object), SimpleType.Bool) if klazz.isAssignableFrom(classOf[java.util.function.Predicate[Object]]) => tpe
      case SimpleType.Arrow(List(SimpleType.Int32), SimpleType.Unit) if klazz.isAssignableFrom(classOf[java.util.function.IntConsumer]) => tpe
      case SimpleType.Arrow(List(SimpleType.Int32), SimpleType.Object) if klazz.isAssignableFrom(classOf[java.util.function.IntFunction[Object]]) => tpe
      case SimpleType.Arrow(List(SimpleType.Int32), SimpleType.Bool) if klazz.isAssignableFrom(classOf[java.util.function.IntPredicate]) => tpe
      case SimpleType.Arrow(List(SimpleType.Int32), SimpleType.Int32) if klazz.isAssignableFrom(classOf[java.util.function.IntUnaryOperator]) => tpe
      case SimpleType.Arrow(List(SimpleType.Int32), SimpleType.Unit) if klazz.isAssignableFrom(classOf[java.util.function.IntConsumer]) => tpe
      case SimpleType.Arrow(List(SimpleType.Int64), SimpleType.Unit) if klazz.isAssignableFrom(classOf[java.util.function.LongConsumer]) => tpe
      case SimpleType.Arrow(List(SimpleType.Int64), SimpleType.Object) if klazz.isAssignableFrom(classOf[java.util.function.LongFunction[Object]]) => tpe
      case SimpleType.Arrow(List(SimpleType.Int64), SimpleType.Bool) if klazz.isAssignableFrom(classOf[java.util.function.LongPredicate]) => tpe
      case SimpleType.Arrow(List(SimpleType.Int64), SimpleType.Int64) if klazz.isAssignableFrom(classOf[java.util.function.LongUnaryOperator]) => tpe
      case SimpleType.Arrow(List(SimpleType.Float64), SimpleType.Unit) if klazz.isAssignableFrom(classOf[java.util.function.DoubleConsumer]) => tpe
      case SimpleType.Arrow(List(SimpleType.Float64), SimpleType.Object) if klazz.isAssignableFrom(classOf[java.util.function.DoubleFunction[Object]]) => tpe
      case SimpleType.Arrow(List(SimpleType.Float64), SimpleType.Bool) if klazz.isAssignableFrom(classOf[java.util.function.DoublePredicate]) => tpe
      case SimpleType.Arrow(List(SimpleType.Float64), SimpleType.Float64) if klazz.isAssignableFrom(classOf[java.util.function.DoubleUnaryOperator]) => tpe

      case SimpleType.Array(_) => tpe // TODO: Array subtyping

      case _ => failMismatchedTypes(tpe, klazz, loc)
    }
  }

  /**
    * Remove the type associated with `label` from the given record type `rec`.
    * If `rec` is not a record, return `None`.
    */
  private def removeFromRecordType(rec: SimpleType, label: String, loc: SourceLocation): (SimpleType, Option[SimpleType]) = rec match {
    case SimpleType.RecordEmpty => (rec, None)
    case SimpleType.RecordExtend(lbl, valtype, rest) =>
      if (label == lbl) (rest, Some(valtype))
      else {
        val (rec, opt) = removeFromRecordType(rest, label, loc)
        (SimpleType.RecordExtend(lbl, valtype, rec), opt)
      }
    case _ => failMismatchedShape(rec, "Record", loc)
  }

  /**
    * Remove the type associated with `label` from the given extensible tag type `tag`.
    * If `tag` is not [[SimpleType.ExtensibleExtend]], it returns `None`.
    */
  private def getExtensibleTagType(tag: SimpleType, label: String, loc: SourceLocation): Option[List[SimpleType]] = tag match {
    case SimpleType.ExtensibleEmpty => None
    case SimpleType.ExtensibleExtend(cons, tpes, rest) =>
      if (label == cons.name)
        Some(tpes)
      else {
        getExtensibleTagType(rest, label, loc)
      }
    case _ => failMismatchedShape(tag, s"ExtensibleExtend($label)", loc)
  }

  /**
    * Get the type associated with `label` in the given record type `rec`.
    * If `rec` is not a record, return `None`.
    */
  @tailrec
  private def selectFromRecordType(rec: SimpleType, label: String, loc: SourceLocation): Option[SimpleType] = rec match {
    case SimpleType.RecordExtend(lbl, valtype, rest) =>
      if (lbl == label)
        Some(valtype)
      else
        selectFromRecordType(rest, label, loc)
    case SimpleType.RecordEmpty => None
    case _ => failMismatchedShape(rec, "Record", loc)
  }

  /**
    * Throw `InternalCompilerException` because the `found` does not match the shape specified by `expected`.
    */
  private def failMismatchedShape(found: SimpleType, expected: String, loc: SourceLocation): Nothing =
    throw InternalCompilerException(
      s"Mismatched shape: expected = \'$expected\', found = $found", loc
    )

  /**
    * Throw `InternalCompilerException` because the `expected` type does not match the `found` type.
    */
  private def failUnexpectedType(found: SimpleType, expected: SimpleType, loc: SourceLocation): Nothing =
    throw InternalCompilerException(
      s"Unexpected type: expected = $expected, found = $found", loc
    )

  /**
    * Throw `InternalCompilerException` because `tpe1` is not equal to `tpe2`.
    */
  private def failMismatchedTypes(tpe1: SimpleType, tpe2: SimpleType, loc: SourceLocation): Nothing =
    throw InternalCompilerException(
      s"Mismatched types: tpe1 = $tpe1, tpe2 = $tpe2", loc
    )

  /**
    * Throw `InternalCompilerException` because `tpe` does not match `klazz`.
    */
  private def failMismatchedTypes(tpe: SimpleType, klazz: Class[?], loc: SourceLocation): Nothing =
    throw InternalCompilerException(
      s"Mismatched types: tpe1 = $tpe, class = $klazz", loc
    )
}

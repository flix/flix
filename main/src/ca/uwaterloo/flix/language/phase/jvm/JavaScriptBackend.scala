package ca.uwaterloo.flix.language.phase.jvm

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.Ast.Constant
import ca.uwaterloo.flix.language.ast.ReducedAst._
import ca.uwaterloo.flix.language.ast.SemanticOp.{BoolOp, CharOp, Float32Op, Float64Op, Int16Op, Int32Op, Int64Op, Int8Op, StringOp}
import ca.uwaterloo.flix.language.ast.{Ast, AtomicOp, SemanticOp, SourceLocation, Symbol}
import ca.uwaterloo.flix.language.dbg.AstPrinter.DebugNoOp
import ca.uwaterloo.flix.language.dbg.Doc
import ca.uwaterloo.flix.language.dbg.Doc._
import ca.uwaterloo.flix.language.phase.extra.JavaScriptSource._
import ca.uwaterloo.flix.util.InternalCompilerException

import java.nio.file.{Files, LinkOption, Path}
import scala.annotation.tailrec

object JavaScriptBackend {

  def run(root: Root)(implicit flix: Flix): Unit = flix.phase("JavaScriptBackend") {
    if (flix.options.output.nonEmpty) {
      implicit val indent: Indent = indentationLevel(2)
      val program = root.defs.values.map(compileDef).mkString("\n")
      val outputPath = flix.options.output.get.resolve("js/flix.js").toAbsolutePath
      writeProgram(outputPath, program)
    }
  }(DebugNoOp())

  private def writeProgram(path: Path, program: String): Unit = {
    // Create all parent directories (in case they don't exist).
    Files.createDirectories(path.getParent)

    // Check if the file already exists.
    if (Files.exists(path)) {
      // Check that the file is a regular file.
      if (!Files.isRegularFile(path, LinkOption.NOFOLLOW_LINKS)) {
        throw InternalCompilerException(s"Unable to write to non-regular file: '$path'.", SourceLocation.Unknown)
      }

      // Check if the file is writable.
      if (!Files.isWritable(path)) {
        throw InternalCompilerException(s"Unable to write to read-only file: '$path'.", SourceLocation.Unknown)
      }

      // Check that the file is empty or a class file.
      if (!(isEmpty(path) || isJavaScriptFile(path))) {
        throw InternalCompilerException(s"Refusing to overwrite non-empty, non-class file: '$path'.", SourceLocation.Unknown)
      }
    }

    // Write the bytecode.
    Files.write(path, program.getBytes)
  }


  private def isEmpty(path: Path): Boolean = Files.size(path) == 0L

  private def isJavaScriptFile(path: Path): Boolean = false

  private def compileDef(defn: Def)(implicit indent: Indent): String = {
    val name = compileDefnSym(defn.sym)
    val args = defn.fparams.map(compileFparam)
    val body = compileExp(defn.expr)
    val fun = function(name, args, body)
    pretty(80, fun)
  }

  private def compileFparam(fp: FormalParam): Doc = {
    compileVarSym(fp.sym)
  }

  private def compileVarSym(x: Symbol.VarSym): Doc = {
    text(s"v${x.getStackOffset(localOffset = 0)}")
  }

  private def compileDefnSym(x: Symbol.DefnSym): Doc = {
    namespacedName(x.namespace, x.name)
  }

  private def compileExp(exp: Expr)(implicit indent: Indent): Doc = exp match {
    case Expr.Cst(cst, _, loc) => compileCst(cst, loc)
    case Expr.Var(sym, _, _) => compileVarSym(sym)
    case Expr.ApplyAtomic(op, exps, tpe, purity, loc) => compileAtomic(op, exps.map(compileExp), loc)
    case Expr.ApplyClo(exp, exps, ct, tpe, purity, loc) =>
      compileExp(exp) :: tuple(exps.map(compileExp))
    case Expr.ApplyDef(sym, exps, ct, tpe, purity, loc) =>
      compileDefnSym(sym) :: tuple(exps.map(compileExp))
    case Expr.ApplySelfTail(sym, actuals, tpe, purity, loc) =>
      compileDefnSym(sym) :: tuple(actuals.map(compileExp))
    case Expr.IfThenElse(exp1, exp2, exp3, tpe, purity, loc) =>
      text("if") +: parens(compileExp(exp1)) +: curly(compileExp(exp2)) +: text("else") +: curly(compileExp(exp3))
    case Expr.Branch(exp, branches, tpe, purity, loc) => throw InternalCompilerException("Pattern matching is not supported in JS", loc)
    case Expr.JumpTo(sym, tpe, purity, loc) => throw InternalCompilerException("Pattern matching is not supported in JS", loc)
    case Expr.Let(_, _, _, _, _, _) => semiSep(compileBinders(exp, Nil))
    case Expr.LetRec(_, _, _, _, _, _, _, _) => semiSep(compileBinders(exp, Nil))
    case Expr.Stmt(_, _, _, _, _) => semiSep(compileBinders(exp, Nil))
    case Expr.Scope(sym, exp, tpe, purity, loc) => throw InternalCompilerException("Regions are not supported in JS", loc)
    case Expr.TryCatch(exp, rules, tpe, purity, loc) => throw InternalCompilerException("Try-Catch is not supported in JS", loc)
    case Expr.TryWith(exp, effUse, rules, ct, tpe, purity, loc) => throw InternalCompilerException("Algebraic effects are not supported in JS", loc)
    case Expr.Do(op, exps, tpe, purity, loc) => throw InternalCompilerException("Algebraic effects are not supported in JS", loc)
    case Expr.NewObject(name, clazz, tpe, purity, methods, loc) => throw InternalCompilerException("Java interop is not supported in JS", loc)
  }

  @tailrec
  private def compileBinders(exp: Expr, acc: List[Doc])(implicit indent: Indent): List[Doc] = exp match {
    case Expr.Let(sym, exp1, exp2, _, _, _) =>
      val doc = text("const") +: compileVarSym(sym) +: text("=") +\: compileExp(exp1)
      compileBinders(exp2, doc :: acc)
    case Expr.LetRec(_, _, _, _, _, _, _, loc) =>
      throw InternalCompilerException("Local defs are not supported in JS", loc)
    case Expr.Stmt(exp1, exp2, _, _, _) =>
      val doc = compileExp(exp1)
      compileBinders(exp2, doc :: acc)
    case _ => (compileExp(exp) :: acc).reverse
  }

  private def compileCst(cst: Constant, loc: SourceLocation): Doc = cst match {
    case Constant.Unit => string("[unit]")
    case Constant.Null => string("[null]")
    case Constant.Bool(lit) =>
      if (lit) text("true")
      else text("false")
    case Constant.Char(lit) => string(lit.toString)
    case Constant.Float32(lit) => text(lit.toString)
    case Constant.Float64(lit) => text(lit.toString)
    case Constant.BigDecimal(_) => throw InternalCompilerException("BigDecimal is not supported in JS", loc)
    case Constant.Int8(lit) => text(lit.toString)
    case Constant.Int16(lit) => text(lit.toString)
    case Constant.Int32(lit) => text(lit.toString)
    case Constant.Int64(lit) => text(lit.toString)
    case Constant.BigInt(_) => throw InternalCompilerException("BigInt is not supported in JS", loc)
    case Constant.Str(lit) => string(lit)
    case Constant.Regex(_) => throw InternalCompilerException("Regex is not supported in JS", loc)
  }

  private def compileAtomic(op: AtomicOp, exps: List[Doc], loc: SourceLocation)(implicit indent: Indent): Doc = op match {
    case AtomicOp.Closure(sym) => throw InternalCompilerException("Closures are not supported by JS", loc)
    case AtomicOp.Unary(sop) => exps match {
      case List(one) => compileUnary(sop, one, loc)
      case _ => throw InternalCompilerException("Malformed program", loc)
    }
    case AtomicOp.Binary(sop) => exps match {
      case List(one, two) => compileBinary(sop, one, two, loc)
      case _ => throw InternalCompilerException("Malformed program", loc)
    }
    case AtomicOp.Region => throw InternalCompilerException("Regions are not supported by JS", loc)
    case AtomicOp.Is(sym) => throw InternalCompilerException("Enums are not supported by JS", loc)
    case AtomicOp.Tag(sym) => throw InternalCompilerException("Enums are not supported by JS", loc)
    case AtomicOp.Untag(sym) => throw InternalCompilerException("Enums are not supported by JS", loc)
    case AtomicOp.Index(idx) => throw InternalCompilerException("Tuples are not supported by JS", loc)
    case AtomicOp.Tuple => throw InternalCompilerException("Tuples are not supported by JS", loc)
    case AtomicOp.RecordEmpty => throw InternalCompilerException("Records are not supported by JS", loc)
    case AtomicOp.RecordSelect(label) => throw InternalCompilerException("Records are not supported by JS", loc)
    case AtomicOp.RecordExtend(label) => throw InternalCompilerException("Records are not supported by JS", loc)
    case AtomicOp.RecordRestrict(label) => throw InternalCompilerException("Records are not supported by JS", loc)
    case AtomicOp.ArrayLit => throw InternalCompilerException("Arrays are not supported by JS", loc)
    case AtomicOp.ArrayNew => throw InternalCompilerException("Arrays are not supported by JS", loc)
    case AtomicOp.ArrayLoad => throw InternalCompilerException("Arrays are not supported by JS", loc)
    case AtomicOp.ArrayStore => throw InternalCompilerException("Arrays are not supported by JS", loc)
    case AtomicOp.ArrayLength => throw InternalCompilerException("Arrays are not supported by JS", loc)
    case AtomicOp.Ref => throw InternalCompilerException("References are not supported by JS", loc)
    case AtomicOp.Deref => throw InternalCompilerException("References are not supported by JS", loc)
    case AtomicOp.Assign => throw InternalCompilerException("References are not supported by JS", loc)
    case AtomicOp.InstanceOf(_) => throw InternalCompilerException("Java interop is not supported in JS", loc)
    case AtomicOp.Cast => takeOne(exps, loc)
    case AtomicOp.Unbox => takeOne(exps, loc)
    case AtomicOp.Box => takeOne(exps, loc)
    case AtomicOp.InvokeConstructor(_) => throw InternalCompilerException("Java interop is not supported in JS", loc)
    case AtomicOp.InvokeMethod(_) => throw InternalCompilerException("Java interop is not supported in JS", loc)
    case AtomicOp.InvokeStaticMethod(_) => throw InternalCompilerException("Java interop is not supported in JS", loc)
    case AtomicOp.GetField(_) => throw InternalCompilerException("Java interop is not supported in JS", loc)
    case AtomicOp.PutField(_) => throw InternalCompilerException("Java interop is not supported in JS", loc)
    case AtomicOp.GetStaticField(_) => throw InternalCompilerException("Java interop is not supported in JS", loc)
    case AtomicOp.PutStaticField(_) => throw InternalCompilerException("Java interop is not supported in JS", loc)
    case AtomicOp.Spawn => throw InternalCompilerException("Spawn is not supported in JS", loc)
    case AtomicOp.Lazy => throw InternalCompilerException("Lazy values are not supported in JS", loc)
    case AtomicOp.Force => throw InternalCompilerException("Lazy values are not supported in JS", loc)
    case AtomicOp.HoleError(sym) => text("throw") +: text("new") +: text("Error") :: parens(string(s"HoleError ${sym.name}"))
    case AtomicOp.MatchError => text("throw") +: text("new") +: text("Error") :: parens(string(s"MatchError"))
  }

  private def compileUnary(op: SemanticOp.UnaryOp, exp: Doc, loc: SourceLocation): Doc = op match {
    case BoolOp.Not => text("!") :: exp
    case Float32Op.Neg => text("-") :: exp
    case Float64Op.Neg => text("-") :: exp
    case Int8Op.Neg => text("-") :: exp
    case Int8Op.Not => throw InternalCompilerException("Bitwise operations not supported in JS", loc)
    case Int16Op.Neg => text("-") :: exp
    case Int16Op.Not => throw InternalCompilerException("Bitwise operations not supported in JS", loc)
    case Int32Op.Neg => text("-") :: exp
    case Int32Op.Not => throw InternalCompilerException("Bitwise operations not supported in JS", loc)
    case Int64Op.Neg => text("-") :: exp
    case Int64Op.Not => throw InternalCompilerException("Bitwise operations not supported in JS", loc)
  }

  private def compileBinary(op: SemanticOp.BinaryOp, exp1: Doc, exp2: Doc, loc: SourceLocation): Doc = {
    exp1 +: text(binaryString(op, loc)) +: exp2
  }

  private def binaryString(op: SemanticOp.BinaryOp, loc: SourceLocation): String = op match {
    case BoolOp.And => "&&"
    case BoolOp.Or => "||"
    case BoolOp.Eq => "==="
    case BoolOp.Neq => "!=="
    case CharOp.Eq => "==="
    case CharOp.Neq => "!=="
    case CharOp.Lt => "<"
    case CharOp.Le => "<="
    case CharOp.Gt => ">"
    case CharOp.Ge => ">="
    case Float32Op.Add => "+"
    case Float32Op.Sub => "-"
    case Float32Op.Mul => "*"
    case Float32Op.Div => throw InternalCompilerException("Division is not supported by JS", loc)
    case Float32Op.Exp => throw InternalCompilerException("Exponentiation is not supported by JS", loc)
    case Float32Op.Eq => "==="
    case Float32Op.Neq => "!=="
    case Float32Op.Lt => "<"
    case Float32Op.Le => "<="
    case Float32Op.Gt => ">"
    case Float32Op.Ge => ">="
    case Float64Op.Add => "+"
    case Float64Op.Sub => "-"
    case Float64Op.Mul => "*"
    case Float64Op.Div => throw InternalCompilerException("Division is not supported by JS", loc)
    case Float64Op.Exp => throw InternalCompilerException("Exponentiation is not supported by JS", loc)
    case Float64Op.Eq => "==="
    case Float64Op.Neq => "!=="
    case Float64Op.Lt => "<"
    case Float64Op.Le => "<="
    case Float64Op.Gt => ">"
    case Float64Op.Ge => ">="
    case Int8Op.Add => "+"
    case Int8Op.Sub => "-"
    case Int8Op.Mul => "*"
    case Int8Op.Div => throw InternalCompilerException("Division is not supported by JS", loc)
    case Int8Op.Rem => throw InternalCompilerException("Remainder is not supported by JS", loc)
    case Int8Op.Exp => throw InternalCompilerException("Exponentiation is not supported by JS", loc)
    case Int8Op.And => throw InternalCompilerException("Bitwise operations not supported in JS", loc)
    case Int8Op.Or => throw InternalCompilerException("Bitwise operations not supported in JS", loc)
    case Int8Op.Xor => throw InternalCompilerException("Bitwise operations not supported in JS", loc)
    case Int8Op.Shl => throw InternalCompilerException("Bitwise operations not supported in JS", loc)
    case Int8Op.Shr => throw InternalCompilerException("Bitwise operations not supported in JS", loc)
    case Int8Op.Eq => "==="
    case Int8Op.Neq => "!=="
    case Int8Op.Lt => "<"
    case Int8Op.Le => "<="
    case Int8Op.Gt => ">"
    case Int8Op.Ge => ">="
    case Int16Op.Add => "+"
    case Int16Op.Sub => "-"
    case Int16Op.Mul => "*"
    case Int16Op.Div => throw InternalCompilerException("Division is not supported by JS", loc)
    case Int16Op.Rem => throw InternalCompilerException("Remainder is not supported by JS", loc)
    case Int16Op.Exp => throw InternalCompilerException("Exponentiation is not supported by JS", loc)
    case Int16Op.And => throw InternalCompilerException("Bitwise operations not supported in JS", loc)
    case Int16Op.Or => throw InternalCompilerException("Bitwise operations not supported in JS", loc)
    case Int16Op.Xor => throw InternalCompilerException("Bitwise operations not supported in JS", loc)
    case Int16Op.Shl => throw InternalCompilerException("Bitwise operations not supported in JS", loc)
    case Int16Op.Shr => throw InternalCompilerException("Bitwise operations not supported in JS", loc)
    case Int16Op.Eq => "==="
    case Int16Op.Neq => "!=="
    case Int16Op.Lt => "<"
    case Int16Op.Le => "<="
    case Int16Op.Gt => ">"
    case Int16Op.Ge => ">="
    case Int32Op.Add => "+"
    case Int32Op.Sub => "-"
    case Int32Op.Mul => "*"
    case Int32Op.Div => throw InternalCompilerException("Division is not supported by JS", loc)
    case Int32Op.Rem => throw InternalCompilerException("Remainder is not supported by JS", loc)
    case Int32Op.Exp => throw InternalCompilerException("Exponentiation is not supported by JS", loc)
    case Int32Op.And => throw InternalCompilerException("Bitwise operations not supported in JS", loc)
    case Int32Op.Or => throw InternalCompilerException("Bitwise operations not supported in JS", loc)
    case Int32Op.Xor => throw InternalCompilerException("Bitwise operations not supported in JS", loc)
    case Int32Op.Shl => throw InternalCompilerException("Bitwise operations not supported in JS", loc)
    case Int32Op.Shr => throw InternalCompilerException("Bitwise operations not supported in JS", loc)
    case Int32Op.Eq => "==="
    case Int32Op.Neq => "!=="
    case Int32Op.Lt => "<"
    case Int32Op.Le => "<="
    case Int32Op.Gt => ">"
    case Int32Op.Ge => ">="
    case Int64Op.Add => "+"
    case Int64Op.Sub => "-"
    case Int64Op.Mul => "*"
    case Int64Op.Div => throw InternalCompilerException("Division is not supported by JS", loc)
    case Int64Op.Rem => throw InternalCompilerException("Remainder is not supported by JS", loc)
    case Int64Op.Exp => throw InternalCompilerException("Exponentiation is not supported by JS", loc)
    case Int64Op.And => throw InternalCompilerException("Bitwise operations not supported in JS", loc)
    case Int64Op.Or => throw InternalCompilerException("Bitwise operations not supported in JS", loc)
    case Int64Op.Xor => throw InternalCompilerException("Bitwise operations not supported in JS", loc)
    case Int64Op.Shl => throw InternalCompilerException("Bitwise operations not supported in JS", loc)
    case Int64Op.Shr => throw InternalCompilerException("Bitwise operations not supported in JS", loc)
    case Int64Op.Eq => "==="
    case Int64Op.Neq => "!=="
    case Int64Op.Lt => "<"
    case Int64Op.Le => "<="
    case Int64Op.Gt => ">"
    case Int64Op.Ge => ">="
    case StringOp.Concat => "+"
  }

  private def takeOne[T](l: List[T], loc: SourceLocation): T = {
    l match {
      case List(one) => one
      case _ => throw InternalCompilerException("Malformed program", loc)
    }
  }
}

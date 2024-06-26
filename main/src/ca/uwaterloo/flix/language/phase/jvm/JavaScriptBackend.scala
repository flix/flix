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
      val program = root.defs.values.map(compileDef).mkString("\n\n")
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
        throw InternalCompilerException(s"Refusing to overwrite non-empty, non-js file: '$path'.", SourceLocation.Unknown)
      }
    }

    // Write the bytecode.
    Files.write(path, program.getBytes)
  }


  private def isEmpty(path: Path): Boolean = Files.size(path) == 0L

  private def isJavaScriptFile(path: Path): Boolean = true

  private def compileDef(defn: Def)(implicit indent: Indent): String = {
    val name = compileDefnSym(defn.sym)
    val args = defn.fparams.map(compileFparam)
    val body = compileExp(defn.expr, inf, tail = true)
    val fun = function(name, args, body)
    pretty(80, fun)
  }

  private def compileFparam(fp: FormalParam): Doc = {
    compileVarSym(fp.sym)
  }

  private def compileVarSym(x: Symbol.VarSym): Doc = {
    text(s"v${x.getStackOffset(localOffset = 0)}")
  }

  private def compileCaseSym(x: Symbol.CaseSym): Doc = {
    text(s"v${mkSafe(x.name)}")
  }

  private def compileDefnSym(x: Symbol.DefnSym): Doc = {
    namespacedName(x.namespace, x.name)
  }

  private val crash = false

  private val inf = 99
  private val binOp = 30
  private val unOp = 7
  private val app = 5
  private val nothing = 0

  private def returnit(doc: Doc, tail: Boolean): Doc = {
    if (tail) text("return") +: doc else doc
  }

  private def compileExp(exp: Expr, level: Int, tail: Boolean)(implicit indent: Indent): Doc = exp match {
    case Expr.Cst(cst, _, loc) => returnit(compileCst(cst, loc), tail)
    case Expr.Var(sym, _, _) => returnit(compileVarSym(sym), tail)
    case Expr.ApplyAtomic(op, exps, tpe, purity, loc) =>
      compileAtomic(op, exps, level: Int, tail, loc)
    case Expr.ApplyClo(exp, exps, ct, tpe, purity, loc) =>
      returnit(compileExp(exp, app, tail = false) :: tuple(exps.map(compileExp(_, inf, tail = false))), tail)
      // no paren: inf, app, unOp, binOp
      //    paren:
    case Expr.ApplyDef(sym, exps, ct, tpe, purity, loc) =>
      returnit(compileDefnSym(sym) :: tuple(exps.map(compileExp(_, inf, tail = false))), tail)
    case Expr.ApplySelfTail(sym, actuals, tpe, purity, loc) =>
      returnit(compileDefnSym(sym) :: tuple(actuals.map(compileExp(_, inf, tail = false))), tail)
    case Expr.IfThenElse(exp1, exp2, exp3, tpe, purity, loc) =>
      text("if") +: parens(compileExp(exp1, inf, tail = false)) +: curly(compileExp(exp2, inf, tail)) +: text("else") +: curly(compileExp(exp3, inf, tail))
    case Expr.Branch(exp, branches, tpe, purity, loc) => if (crash) throw InternalCompilerException("Pattern matching is not supported in JS", loc) else text("?branch?")
    case Expr.JumpTo(sym, tpe, purity, loc) => if (crash) throw InternalCompilerException("Pattern matching is not supported in JS", loc) else text("?jumpto?")
    case Expr.Let(_, _, _, _, _, _) | Expr.LetRec(_, _, _, _, _, _, _, _) | Expr.Stmt(_, _, _, _, _) =>
      val d = semiSep(compileBinders(exp, Nil, tail))
      // no paren: inf
      //    paren: app, unOp, binOp
      curlyCond(d, level, binOp)
    case Expr.Scope(sym, exp, tpe, purity, loc) => if (crash) throw InternalCompilerException("Regions are not supported in JS", loc) else text("?scope?")
    case Expr.TryCatch(exp, rules, tpe, purity, loc) => if (crash) throw InternalCompilerException("Try-Catch is not supported in JS", loc) else text("?trycatch?")
    case Expr.TryWith(exp, effUse, rules, ct, tpe, purity, loc) => if (crash) throw InternalCompilerException("Algebraic effects are not supported in JS", loc) else text("?trywith?")
    case Expr.Do(op, exps, tpe, purity, loc) => if (crash) throw InternalCompilerException("Algebraic effects are not supported in JS", loc) else text("?do?")
    case Expr.NewObject(name, clazz, tpe, purity, methods, loc) => if (crash) throw InternalCompilerException("Java interop is not supported in JS", loc) else text("?newobject?")
  }

  @tailrec
  private def compileBinders(exp: Expr, acc: List[Doc], tail: Boolean)(implicit indent: Indent): List[Doc] = exp match {
    case Expr.Let(sym, exp1, exp2, _, _, _) =>
      val doc = text("const") +: compileVarSym(sym) +: text("=") +\: compileExp(exp1, inf, tail = false)
      compileBinders(exp2, group(doc) :: acc, tail)
    case Expr.LetRec(_, _, _, _, exp2, _, _, loc) =>
      val doc = if (crash) throw InternalCompilerException("Local defs are not supported in JS", loc) else text("?letrec?")
      compileBinders(exp2, doc :: acc, tail)
    case Expr.Stmt(exp1, exp2, _, _, _) =>
      val doc = compileExp(exp1, inf, tail = false)
      compileBinders(exp2, doc :: acc, tail)
    case _ => (compileExp(exp, inf, tail) :: acc).reverse
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
    case Constant.BigDecimal(_) => if (crash) throw InternalCompilerException("BigDecimal is not supported in JS", loc) else text("?bigdecimal?")
    case Constant.Int8(lit) => text(lit.toString)
    case Constant.Int16(lit) => text(lit.toString)
    case Constant.Int32(lit) => text(lit.toString)
    case Constant.Int64(lit) => text(lit.toString)
    case Constant.BigInt(_) => if (crash) throw InternalCompilerException("BigInt is not supported in JS", loc) else text("?bigint?")
    case Constant.Str(lit) => string(lit)
    case Constant.Regex(_) => if (crash) throw InternalCompilerException("Regex is not supported in JS", loc) else text("?regex?")
  }

  private def compileAtomic(op: AtomicOp, exps: List[Expr], level: Int, tail: Boolean, loc: SourceLocation)(implicit indent: Indent): Doc = op match {
    case AtomicOp.Closure(sym) => if (crash) throw InternalCompilerException("Closures are not supported by JS", loc) else text("?closure?")
    case AtomicOp.Unary(sop) => exps match {
      case List(one) =>
        val d = compileUnary(sop, compileExp(one, unOp, tail = false), loc)
        // no paren: inf, binOp
        //    paren: app, unOp
        returnit(parensCond(d, level, maxParen = unOp), tail)
      case _ => throw InternalCompilerException("Malformed program", loc)
    }
    case AtomicOp.Binary(sop) => exps match {
      case List(one, two) =>
        val d = compileBinary(sop, compileExp(one, binOp, tail = false), compileExp(two, binOp, tail = false), loc)
        // no paren: inf
        //    paren: unOp, app, binOp
        returnit(parensCond(d, level, maxParen = binOp), tail)
      case _ => throw InternalCompilerException("Malformed program", loc)
    }
    case AtomicOp.Region => if (crash) throw InternalCompilerException("Regions are not supported by JS", loc) else text("?region?")
    case AtomicOp.Is(sym) =>
      val d = compileExp(takeOne(exps, loc), binOp, tail = false) :: square(text("0")) +: text("===") +: compileCaseSym(sym)
      returnit(parensCond(d, level, maxParen = binOp), tail = false)
    case AtomicOp.Tag(sym) =>
      returnit(squareTuple(List(compileCaseSym(sym), compileExp(takeOne(exps, loc), inf, tail = false))), tail)
    case AtomicOp.Untag(_) =>
      returnit(compileExp(takeOne(exps, loc), app, tail = false) :: square(text("1")), tail)
    case AtomicOp.Index(idx) =>
      returnit(compileExp(takeOne(exps, loc), app, tail = false) :: square(text(idx.toString)), tail)
    case AtomicOp.Tuple =>
      returnit(squareTuple(exps.map(compileExp(_, inf, tail = false))), tail)
    case AtomicOp.RecordEmpty => if (crash) throw InternalCompilerException("Records are not supported by JS", loc) else text("?recordEmpty?")
    case AtomicOp.RecordSelect(label) => if (crash) throw InternalCompilerException("Records are not supported by JS", loc) else text("?recordselect?")
    case AtomicOp.RecordExtend(label) => if (crash) throw InternalCompilerException("Records are not supported by JS", loc) else text("?recordExtend?")
    case AtomicOp.RecordRestrict(label) => if (crash) throw InternalCompilerException("Records are not supported by JS", loc) else text("?recordRestrict?")
    case AtomicOp.ArrayLit => if (crash) throw InternalCompilerException("Arrays are not supported by JS", loc) else text("?arrayLit?")
    case AtomicOp.ArrayNew => if (crash) throw InternalCompilerException("Arrays are not supported by JS", loc) else text("?arrayNew?")
    case AtomicOp.ArrayLoad => if (crash) throw InternalCompilerException("Arrays are not supported by JS", loc) else text("?arrayload?")
    case AtomicOp.ArrayStore => if (crash) throw InternalCompilerException("Arrays are not supported by JS", loc) else text("?arrayStore?")
    case AtomicOp.ArrayLength => if (crash) throw InternalCompilerException("Arrays are not supported by JS", loc) else text("?arrayLength?")
    case AtomicOp.Ref => if (crash) throw InternalCompilerException("References are not supported by JS", loc) else text("?re?")
    case AtomicOp.Deref => if (crash) throw InternalCompilerException("References are not supported by JS", loc) else text("?deref?")
    case AtomicOp.Assign => if (crash) throw InternalCompilerException("References are not supported by JS", loc) else text("?assign?")
    case AtomicOp.InstanceOf(_) => if (crash) throw InternalCompilerException("Java interop is not supported in JS", loc) else text("?instanceof?")
    case AtomicOp.Cast => compileExp(takeOne(exps, loc), level, tail)
    case AtomicOp.Unbox => compileExp(takeOne(exps, loc), level, tail)
    case AtomicOp.Box => compileExp(takeOne(exps, loc), level, tail)
    case AtomicOp.InvokeConstructor(_) => if (crash) throw InternalCompilerException("Java interop is not supported in JS", loc) else text("?invokeCOnstructor?")
    case AtomicOp.InvokeMethod(_) => if (crash) throw InternalCompilerException("Java interop is not supported in JS", loc) else text("?invokeMethod?")
    case AtomicOp.InvokeStaticMethod(_) => if (crash) throw InternalCompilerException("Java interop is not supported in JS", loc) else text("?invokeStaticMethod?")
    case AtomicOp.GetField(_) => if (crash) throw InternalCompilerException("Java interop is not supported in JS", loc) else text("?getField?")
    case AtomicOp.PutField(_) => if (crash) throw InternalCompilerException("Java interop is not supported in JS", loc) else text("?PutField?")
    case AtomicOp.GetStaticField(_) => if (crash) throw InternalCompilerException("Java interop is not supported in JS", loc) else text("?getStaticField?")
    case AtomicOp.PutStaticField(_) => if (crash) throw InternalCompilerException("Java interop is not supported in JS", loc) else text("?putStaticField?")
    case AtomicOp.Spawn => if (crash) throw InternalCompilerException("Spawn is not supported in JS", loc) else text("?spawn?")
    case AtomicOp.Lazy => if (crash) throw InternalCompilerException("Lazy values are not supported in JS", loc) else text("?lazy?")
    case AtomicOp.Force => if (crash) throw InternalCompilerException("Lazy values are not supported in JS", loc) else text("?force?")
    case AtomicOp.HoleError(sym) => text("throw") +: text("new") +: text("Error") :: parens(string(s"HoleError ${sym.name}"))
    case AtomicOp.MatchError => text("throw") +: text("new") +: text("Error") :: parens(string(s"MatchError"))
  }

  private def parensCond(doc: Doc, found: Int, maxParen: Int)(implicit indent: Indent): Doc = {
    if (found <= maxParen) parens(doc) else doc
  }

  private def curlyCond(doc: Doc, found: Int, maxParen: Int)(implicit indent: Indent): Doc = {
    if (found <= maxParen) curly(doc) else doc
  }

  private def compileUnary(op: SemanticOp.UnaryOp, exp: Doc, loc: SourceLocation): Doc = op match {
    case BoolOp.Not => text("!") :: exp
    case Float32Op.Neg => text("-") :: exp
    case Float64Op.Neg => text("-") :: exp
    case Int8Op.Neg => text("-") :: exp
    case Int8Op.Not => if (crash) throw InternalCompilerException("Bitwise operations not supported in JS", loc) else text("?int8not?")
    case Int16Op.Neg => text("-") :: exp
    case Int16Op.Not => if (crash) throw InternalCompilerException("Bitwise operations not supported in JS", loc) else text("?int16not?")
    case Int32Op.Neg => text("-") :: exp
    case Int32Op.Not => if (crash) throw InternalCompilerException("Bitwise operations not supported in JS", loc) else text("?int32not?")
    case Int64Op.Neg => text("-") :: exp
    case Int64Op.Not => if (crash) throw InternalCompilerException("Bitwise operations not supported in JS", loc) else text("?int64not?")
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
    case Float32Op.Div => "/"
    case Float32Op.Exp => if (crash) throw InternalCompilerException("Exponentiation is not supported by JS", loc) else "?float32exp?"
    case Float32Op.Eq => "==="
    case Float32Op.Neq => "!=="
    case Float32Op.Lt => "<"
    case Float32Op.Le => "<="
    case Float32Op.Gt => ">"
    case Float32Op.Ge => ">="
    case Float64Op.Add => "+"
    case Float64Op.Sub => "-"
    case Float64Op.Mul => "*"
    case Float64Op.Div => "/"
    case Float64Op.Exp => if (crash) throw InternalCompilerException("Exponentiation is not supported by JS", loc) else "?float64exp?"
    case Float64Op.Eq => "==="
    case Float64Op.Neq => "!=="
    case Float64Op.Lt => "<"
    case Float64Op.Le => "<="
    case Float64Op.Gt => ">"
    case Float64Op.Ge => ">="
    case Int8Op.Add => "+"
    case Int8Op.Sub => "-"
    case Int8Op.Mul => "*"
    case Int8Op.Div => if (crash) throw InternalCompilerException("Integer division is not supported by JS", loc) else "?int8op?"
    case Int8Op.Rem => if (crash) throw InternalCompilerException("Remainder is not supported by JS", loc) else "?int8op?"
    case Int8Op.Exp => if (crash) throw InternalCompilerException("Exponentiation is not supported by JS", loc) else "?int8op?"
    case Int8Op.And => if (crash) throw InternalCompilerException("Bitwise operations not supported in JS", loc) else "?int8op?"
    case Int8Op.Or => if (crash) throw InternalCompilerException("Bitwise operations not supported in JS", loc) else "?int8op?"
    case Int8Op.Xor => if (crash) throw InternalCompilerException("Bitwise operations not supported in JS", loc) else "?int8op?"
    case Int8Op.Shl => if (crash) throw InternalCompilerException("Bitwise operations not supported in JS", loc) else "?int8op?"
    case Int8Op.Shr => if (crash) throw InternalCompilerException("Bitwise operations not supported in JS", loc) else "?int8op?"
    case Int8Op.Eq => "==="
    case Int8Op.Neq => "!=="
    case Int8Op.Lt => "<"
    case Int8Op.Le => "<="
    case Int8Op.Gt => ">"
    case Int8Op.Ge => ">="
    case Int16Op.Add => "+"
    case Int16Op.Sub => "-"
    case Int16Op.Mul => "*"
    case Int16Op.Div => if (crash) throw InternalCompilerException("Integer division is not supported by JS", loc) else "?int16op?"
    case Int16Op.Rem => if (crash) throw InternalCompilerException("Remainder is not supported by JS", loc) else "?int16op?"
    case Int16Op.Exp => if (crash) throw InternalCompilerException("Exponentiation is not supported by JS", loc) else "?int16op?"
    case Int16Op.And => if (crash) throw InternalCompilerException("Bitwise operations not supported in JS", loc) else "?int16op?"
    case Int16Op.Or => if (crash) throw InternalCompilerException("Bitwise operations not supported in JS", loc) else "?int16op?"
    case Int16Op.Xor => if (crash) throw InternalCompilerException("Bitwise operations not supported in JS", loc) else "?int16op?"
    case Int16Op.Shl => if (crash) throw InternalCompilerException("Bitwise operations not supported in JS", loc) else "?int16op?"
    case Int16Op.Shr => if (crash) throw InternalCompilerException("Bitwise operations not supported in JS", loc) else "?int16op?"
    case Int16Op.Eq => "==="
    case Int16Op.Neq => "!=="
    case Int16Op.Lt => "<"
    case Int16Op.Le => "<="
    case Int16Op.Gt => ">"
    case Int16Op.Ge => ">="
    case Int32Op.Add => "+"
    case Int32Op.Sub => "-"
    case Int32Op.Mul => "*"
    case Int32Op.Div => if (crash) throw InternalCompilerException("Integer division is not supported by JS", loc) else "?int32op?"
    case Int32Op.Rem => if (crash) throw InternalCompilerException("Remainder is not supported by JS", loc) else "?int32op?"
    case Int32Op.Exp => if (crash) throw InternalCompilerException("Exponentiation is not supported by JS", loc) else "?int32op?"
    case Int32Op.And => if (crash) throw InternalCompilerException("Bitwise operations not supported in JS", loc) else "?int32op?"
    case Int32Op.Or => if (crash) throw InternalCompilerException("Bitwise operations not supported in JS", loc) else "?int32op?"
    case Int32Op.Xor => if (crash) throw InternalCompilerException("Bitwise operations not supported in JS", loc) else "?int32op?"
    case Int32Op.Shl => if (crash) throw InternalCompilerException("Bitwise operations not supported in JS", loc) else "?int32op?"
    case Int32Op.Shr => if (crash) throw InternalCompilerException("Bitwise operations not supported in JS", loc) else "?int32op?"
    case Int32Op.Eq => "==="
    case Int32Op.Neq => "!=="
    case Int32Op.Lt => "<"
    case Int32Op.Le => "<="
    case Int32Op.Gt => ">"
    case Int32Op.Ge => ">="
    case Int64Op.Add => "+"
    case Int64Op.Sub => "-"
    case Int64Op.Mul => "*"
    case Int64Op.Div => if (crash) throw InternalCompilerException("Integer division is not supported by JS", loc) else "?int64op?"
    case Int64Op.Rem => if (crash) throw InternalCompilerException("Remainder is not supported by JS", loc) else "?int64op?"
    case Int64Op.Exp => if (crash) throw InternalCompilerException("Exponentiation is not supported by JS", loc) else "?int64op?"
    case Int64Op.And => if (crash) throw InternalCompilerException("Bitwise operations not supported in JS", loc) else "?int64op?"
    case Int64Op.Or => if (crash) throw InternalCompilerException("Bitwise operations not supported in JS", loc) else "?int64op?"
    case Int64Op.Xor => if (crash) throw InternalCompilerException("Bitwise operations not supported in JS", loc) else "?int64op?"
    case Int64Op.Shl => if (crash) throw InternalCompilerException("Bitwise operations not supported in JS", loc) else "?int64op?"
    case Int64Op.Shr => if (crash) throw InternalCompilerException("Bitwise operations not supported in JS", loc) else "?int64op?"
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

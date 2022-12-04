package ca.uwaterloo.flix.language.dbg.prettierPrettyPrinting

import ca.uwaterloo.flix.language.ast.{ErasedAst, MonoType}
import ca.uwaterloo.flix.language.ast.ErasedAst._
import ca.uwaterloo.flix.language.ast.Symbol._
import ca.uwaterloo.flix.language.dbg.prettierPrettyPrinting.DocUtil._
import ca.uwaterloo.flix.language.dbg.prettierPrettyPrinting.Doc._

import scala.annotation.tailrec

object ErasedPrinter {

  implicit val indent: Int = 4

  def doc(root: Root): Doc = {
    val defs = root.defs.map{case (_, defn) => doc(defn)}.toList
    fold(_ <> line <> line <> _, defs)
  }

  def doc(defn: Def): Doc = {
    defnf(
      defn.sym.toString,
      defn.formals.map(doc),
      doc(defn.tpe),
      doc(defn.exp)
    )
  }

  def doc(f: FormalParam): Doc = {
    ascf(doc(f.sym), doc(f.tpe))
  }

  def doc(tpe: MonoType): Doc = {
    def tapp(tpeS: Doc, args: List[MonoType]): Doc =
      typeAppf(tpeS, args.map(doc))
    tpe match {
      case MonoType.Unit => text("Unit")
      case MonoType.Bool => text("Bool")
      case MonoType.Char => text("Char")
      case MonoType.Float32 => text("Float32")
      case MonoType.Float64 => text("Float64")
      case MonoType.BigDecimal => text("BigDecimal")
      case MonoType.Int8 => text("Int8")
      case MonoType.Int16 => text("Int16")
      case MonoType.Int32 => text("Int32")
      case MonoType.Int64 => text("Int64")
      case MonoType.BigInt => text("BigInt")
      case MonoType.Str => text("String")
      case MonoType.Array(tpe) => tapp(text("Array"), List(tpe))
      case MonoType.Lazy(tpe) => tapp(text("Lazy"), List(tpe))
      case MonoType.Ref(tpe) => tapp(text("Ref"), List(tpe))
      case MonoType.Tuple(elms) => tuplef(elms.map(doc))
      case MonoType.Enum(sym, args) => tapp(doc(sym), args)
      case MonoType.Arrow(args, result) => arrowf(args.map(doc), doc(result))
      case MonoType.RecordEmpty() => text("{}")
      case MonoType.RecordExtend(_, _, _) =>
        @tailrec
        def recordDoc(tpe: MonoType, acc: List[(String, MonoType)]): Doc = tpe match {
          case MonoType.RecordExtend(field, value, rest) =>
            recordDoc(rest, (field, value) :: acc)
          case rest =>
            val fields = acc.reverse.map{case (field, tpe) => (text(field), doc(tpe))}
            recordExtendf(fields, doc(rest))
        }
        recordDoc(tpe, Nil)
      case MonoType.SchemaEmpty() => text("#{}")
      case MonoType.SchemaExtend(_, _, _) =>
        @tailrec
        def schemaDoc(tpe: MonoType, acc: List[(String, MonoType)]): Doc = tpe match {
          case MonoType.SchemaExtend(name, tpe, rest) =>
            schemaDoc(rest, (name, tpe) :: acc)
          case rest =>
            val fields = acc.reverse.map { case (field, tpe) => (text(field), doc(tpe)) }
            schemaExtendf(fields, doc(rest))
        }
        schemaDoc(tpe, Nil)
      case MonoType.Relation(tpes) => ???
      case MonoType.Lattice(tpes) => ???
      case MonoType.Native(clazz) =>
        val name = clazz.getCanonicalName
        val nullGuardedName = if (name == null) "<AnonClass>" else name
        text(nullGuardedName)
      case MonoType.Var(id) => text("<tvar_") <> text(id.toString) <> text(">")
    }
  }

  def doc(sym: EnumSym): Doc = text(sym.toString)

  def doc(sym: VarSym): Doc = text(sym.toString)

  def doc(exp: Expression): Doc = exp match {
    case Expression.Cst(cst, tpe, loc) => ???
    case Expression.Var(sym, tpe, loc) => ???
    case Expression.Closure(sym, closureArgs, tpe, loc) => ???
    case Expression.ApplyClo(exp, args, tpe, loc) => ???
    case Expression.ApplyDef(sym, args, tpe, loc) => ???
    case Expression.ApplyCloTail(exp, args, tpe, loc) => ???
    case Expression.ApplyDefTail(sym, args, tpe, loc) => ???
    case Expression.ApplySelfTail(sym, formals, actuals, tpe, loc) => ???
    case Expression.Unary(sop, op, exp, tpe, loc) => ???
    case Expression.Binary(sop, op, exp1, exp2, tpe, loc) => ???
    case Expression.IfThenElse(exp1, exp2, exp3, tpe, loc) => ???
    case Expression.Branch(exp, branches, tpe, loc) => ???
    case Expression.JumpTo(sym, tpe, loc) => ???
    case Expression.Let(sym, exp1, exp2, tpe, loc) => ???
    case Expression.LetRec(varSym, index, defSym, exp1, exp2, tpe, loc) => ???
    case Expression.Region(tpe, loc) => ???
    case Expression.Is(sym, exp, loc) => ???
    case Expression.Tag(sym, exp, tpe, loc) => ???
    case Expression.Untag(sym, exp, tpe, loc) => ???
    case Expression.Index(base, offset, tpe, loc) => ???
    case Expression.Tuple(elms, tpe, loc) => ???
    case Expression.RecordEmpty(tpe, loc) => ???
    case Expression.RecordSelect(exp, field, tpe, loc) => ???
    case Expression.RecordExtend(field, value, rest, tpe, loc) => ???
    case Expression.RecordRestrict(field, rest, tpe, loc) => ???
    case Expression.ArrayLit(elms, tpe, loc) => ???
    case Expression.ArrayNew(elm, len, tpe, loc) => ???
    case Expression.ArrayLoad(base, index, tpe, loc) => ???
    case Expression.ArrayStore(base, index, elm, tpe, loc) => ???
    case Expression.ArrayLength(base, tpe, loc) => ???
    case Expression.ArraySlice(base, beginIndex, endIndex, tpe, loc) => ???
    case Expression.Ref(exp, tpe, loc) => ???
    case Expression.Deref(exp, tpe, loc) => ???
    case Expression.Assign(exp1, exp2, tpe, loc) => ???
    case Expression.Cast(exp, tpe, loc) => ???
    case Expression.TryCatch(exp, rules, tpe, loc) => ???
    case Expression.InvokeConstructor(constructor, args, tpe, loc) => ???
    case Expression.InvokeMethod(method, exp, args, tpe, loc) => ???
    case Expression.InvokeStaticMethod(method, args, tpe, loc) => ???
    case Expression.GetField(field, exp, tpe, loc) => ???
    case Expression.PutField(field, exp1, exp2, tpe, loc) => ???
    case Expression.GetStaticField(field, tpe, loc) => ???
    case Expression.PutStaticField(field, exp, tpe, loc) => ???
    case Expression.NewObject(name, clazz, tpe, methods, loc) => ???
    case Expression.Spawn(exp, tpe, loc) => ???
    case Expression.Lazy(exp, tpe, loc) => ???
    case Expression.Force(exp, tpe, loc) => ???
    case Expression.HoleError(sym, tpe, loc) => ???
    case Expression.MatchError(tpe, loc) => ???
    case Expression.BoxBool(exp, loc) => ???
    case Expression.BoxInt8(exp, loc) => ???
    case Expression.BoxInt16(exp, loc) => ???
    case Expression.BoxInt32(exp, loc) => ???
    case Expression.BoxInt64(exp, loc) => ???
    case Expression.BoxChar(exp, loc) => ???
    case Expression.BoxFloat32(exp, loc) => ???
    case Expression.BoxFloat64(exp, loc) => ???
    case Expression.UnboxBool(exp, loc) => ???
    case Expression.UnboxInt8(exp, loc) => ???
    case Expression.UnboxInt16(exp, loc) => ???
    case Expression.UnboxInt32(exp, loc) => ???
    case Expression.UnboxInt64(exp, loc) => ???
    case Expression.UnboxChar(exp, loc) => ???
    case Expression.UnboxFloat32(exp, loc) => ???
    case Expression.UnboxFloat64(exp, loc) => ???
  }

}

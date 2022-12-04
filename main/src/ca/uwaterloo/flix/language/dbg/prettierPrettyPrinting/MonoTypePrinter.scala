package ca.uwaterloo.flix.language.dbg.prettierPrettyPrinting

import ca.uwaterloo.flix.language.ast.MonoType
import ca.uwaterloo.flix.language.ast.Symbol.EnumSym
import ca.uwaterloo.flix.language.dbg.prettierPrettyPrinting.Doc._
import ca.uwaterloo.flix.language.dbg.prettierPrettyPrinting.DocUtil._

import scala.annotation.tailrec

object MonoTypePrinter {

  implicit val indent: Int = INDENT

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
            val fields = acc.reverse.map { case (field, tpe) => (text(field), doc(tpe)) }
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
      case MonoType.Relation(_) => text("<Relation>")
      case MonoType.Lattice(_) => text("<Lattice>")
      case MonoType.Native(clazz) =>
        val name = clazz.getCanonicalName
        val nullGuardedName = if (name == null) "<AnonClass>" else name
        text(nullGuardedName)
      case MonoType.Var(id) => text("<tvar_") <> text(id.toString) <> text(">")
    }
  }

  def doc(sym: EnumSym): Doc = text(sym.toString)

}

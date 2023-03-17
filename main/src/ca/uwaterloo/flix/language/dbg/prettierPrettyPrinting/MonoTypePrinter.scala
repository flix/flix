package ca.uwaterloo.flix.language.dbg.prettierPrettyPrinting

import ca.uwaterloo.flix.language.ast.MonoType
import ca.uwaterloo.flix.language.ast.Symbol.EnumSym
import ca.uwaterloo.flix.language.dbg.prettierPrettyPrinting.Doc._
import ca.uwaterloo.flix.language.dbg.prettierPrettyPrinting.DocUtil.Language._

import scala.annotation.tailrec

object MonoTypePrinterrrr {

  def doc(tpe: MonoType)(implicit i: Indent): Doc = {
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
      case MonoType.Region => text("Region")
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
            recordExtendf(fields, Some(doc(rest)))
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
            schemaExtendf(fields, Some(doc(rest)))
        }

        schemaDoc(tpe, Nil)
      case MonoType.Native(clazz) => (clazz.getCanonicalName, clazz.getSuperclass) match {
        case (null, null) =>
          metaText(text("Anon class with null superclass"))
        case (null, superClass) =>
          val superName = superClass.getCanonicalName
          if (superName != null) {
            metaText(text("Anon subclass of") <+> text(superName))
          } else {
            metaText(text("Anon class"))
          }
        case (name, _) =>
          text(name)
      }
      case MonoType.Var(id) => metaText(text("tvar_") <> text(id.toString))
    }
  }

  def doc(sym: EnumSym): Doc = text(sym.toString)

}

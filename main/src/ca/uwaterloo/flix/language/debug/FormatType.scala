package ca.uwaterloo.flix.language.debug

import ca.uwaterloo.flix.language.ast.Kind.Effect
import ca.uwaterloo.flix.language.ast.{Type, TypeConstructor}
import ca.uwaterloo.flix.util.InternalCompilerException
import ca.uwaterloo.flix.util.vt.{VirtualString, VirtualTerminal}

object FormatType {

  def format(tpe: Type)(implicit audience: Audience): String = {

    val renameMap = alphaRenameVars(tpe)

    def formatWellFormedRecord(record: Type): String = flattenRecord(record) match {
      case FlatThing(fields, Type.Cst(TypeConstructor.RecordEmpty)) =>
        fields.map { case (label, tpe) => formatRecordField(label, tpe) }.mkString("{ ", ", ", " }")
      case FlatThing(fields, rest) =>
        val fieldString = fields.map { case (label, tpe) => formatRecordField(label, tpe) }.mkString(", ")
        s"{ $fieldString | ${visit(rest)} }"
    }

    def formatWellFormedSchema(schema: Type): String = flattenSchema(schema) match {
      case FlatThing(fields, Type.Cst(TypeConstructor.SchemaEmpty)) =>
        fields.map { case (label, tpe) => formatSchemaField(label, tpe) }.mkString("#{ ", ", ", " }")
      case FlatThing(fields, rest) =>
        val fieldString = fields.map { case (label, tpe) => formatSchemaField(label, tpe) }.mkString(", ")
        s"#{ $fieldString | ${visit(rest)} }"
    }

    def formatRecordField(label: String, tpe: Type): String = {
      s"$label: ${visit(tpe)}"
    }

    def formatSchemaField(name: String, tpe: Type): String = {
      tpe.typeConstructor match {
        case Type.Cst(TypeConstructor.Unit) => s"$name()"
        case Type.Cst(TypeConstructor.Tuple(_)) => s"$name${visit(tpe)}"
        case _ => s"$name(${visit(tpe)})"
      }
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
        case tvar@Type.Var(id, kind, _) => audience match {
          case Audience.Internal => kind match {
            case Effect => s"''$id"
            case _ => s"'$id"
          }
          case Audience.External => tvar.getText.getOrElse(renameMap(tvar.id))
        }

        case Type.Cst(TypeConstructor.Unit) => formatApply("Unit", args)
        case Type.Cst(TypeConstructor.Bool) => formatApply("Bool", args)
        case Type.Cst(TypeConstructor.Char) => formatApply("Char", args)
        case Type.Cst(TypeConstructor.Float32) => formatApply("Float32", args)
        case Type.Cst(TypeConstructor.Float64) => formatApply("Float64", args)
        case Type.Cst(TypeConstructor.Int8) => formatApply("Int8", args)
        case Type.Cst(TypeConstructor.Int16) => formatApply("Int16", args)
        case Type.Cst(TypeConstructor.Int32) => formatApply("Int32", args)
        case Type.Cst(TypeConstructor.Int64) => formatApply("Int64", args)
        case Type.Cst(TypeConstructor.BigInt) => formatApply("BigInt", args)
        case Type.Cst(TypeConstructor.Str) => formatApply("String", args)
        case Type.Cst(TypeConstructor.RecordEmpty) => formatApply("{ }", args)
        case Type.Cst(TypeConstructor.SchemaEmpty) => formatApply("#{ }", args)
        case Type.Cst(TypeConstructor.Pure) => formatApply("Pure", args)
        case Type.Cst(TypeConstructor.Impure) => formatApply("Impure", args)

        case Type.Cst(TypeConstructor.Array) => formatApply("Array", args)
        case Type.Cst(TypeConstructor.Vector) => formatApply("Vector", args)
        case Type.Cst(TypeConstructor.Channel) => formatApply("Channel", args)
        case Type.Cst(TypeConstructor.Enum(sym, _)) => formatApply(sym.toString, args)
        case Type.Cst(TypeConstructor.Lattice) => formatApply("Lattice", args)
        case Type.Cst(TypeConstructor.Relation) => formatApply("Relation", args)
        case Type.Cst(TypeConstructor.Ref) => formatApply("Ref", args)

        case Type.Zero => formatApply("Zero", args)
        case Type.Succ(n, tpe) => formatApply(s"Succ($n,  ${visit(tpe)})", args)

        case Type.Cst(TypeConstructor.RecordExtend(label)) => args.length match {
          case 0 => s"{ $label: ??? }"
          case 1 => s"{ $label: ${visit(args.head)} | ??? }"
          case 2 => formatWellFormedRecord(tpe)
          case _ => formatApply(s"RecordExtend($label)", args)
        }

        case Type.Cst(TypeConstructor.SchemaExtend(name)) => args.length match {
          case 0 => s"#{ $name(???) }"
          case 1 => s"#{ $name(${visit(args.head)}) | ??? }"
          case 2 => formatWellFormedSchema(tpe)
          case _ => formatApply(s"SchemaExtend($name)", args)
        }

        case Type.Cst(TypeConstructor.Tuple(length)) =>
          val elements = args.take(length).map(visit)
          val applyParams = args.drop(length) // excess elements
          val tuple = elements.padTo(length, "???").mkString("(", ", ", ")")
          formatApply(tuple, applyParams)


        case Type.Cst(TypeConstructor.Not) => args match {
          case (t1: Type.Var) :: Nil => s"¬${visit(t1)}"
          case t1 :: Nil => s"¬(${visit(t1)})"
          case Nil => "¬???"
          case _ => formatApply("¬", args)
        }

        case Type.Cst(TypeConstructor.And) => args match {
          case (t1: Type.Var) :: (t2: Type.Var) :: Nil => s"${visit(t1)} ∧ ${visit(t2)}"
          case (t1: Type.Var) :: t2 :: Nil => s"${visit(t1)} ∧ (${visit(t2)})"
          case t1 :: (t2: Type.Var) :: Nil => s"(${visit(t1)}) ∧ ${visit(t2)}"
          case t1 :: t2 :: Nil => s"(${visit(t1)}) ∧ (${visit(t2)})"
          case (t1: Type.Var) :: Nil => s"${visit(t1)} ∧ ???"
          case t1 :: Nil => s"(${visit(t1)}) ∧ ???"
          case Nil => s"??? ∧ ???"
          case _ => formatApply("∧", args)
        }

        case Type.Cst(TypeConstructor.Or) => args match {
          case (t1: Type.Var) :: (t2: Type.Var) :: Nil => s"${visit(t1)} ∨ ${visit(t2)}"
          case (t1: Type.Var) :: t2 :: Nil => s"${visit(t1)} ∨ (${visit(t2)})"
          case t1 :: (t2: Type.Var) :: Nil => s"(${visit(t1)}) ∨ ${visit(t2)}"
          case t1 :: t2 :: Nil => s"(${visit(t1)}) ∨ (${visit(t2)})"
          case (t1: Type.Var) :: Nil => s"${visit(t1)} ∨ ???"
          case t1 :: Nil => s"(${visit(t1)}) ∨ ???"
          case Nil => s"??? ∨ ???"
          case _ => formatApply("∨", args)
        }

        case Type.Arrow(arity, eff) => // MATT need to handle arity < 2

          // Retrieve and result type.
          val types = args.take(arity).map(visit)
          val applyParams = args.drop(arity)  // excess args
          val typeStrings = types.padTo(arity, "???")


          // Format the arguments.
          val argPart = typeStrings.init.mkString(" -> ")
          // Format the arrow.
          val arrowPart = eff match {
            case Type.Cst(TypeConstructor.Impure) => " ~> "
            case _ => " -> "
          }
          // Format the effect.
          val effPart = eff match {
            case Type.Cst(TypeConstructor.Pure) => ""
            case Type.Cst(TypeConstructor.Impure) => ""
            case _: Type.Var => s" & ${visit(eff)}"
            case _ => " & (" + visit(eff) + ")"
          }
          // Format the result type.
          val resultPart = typeStrings.last

          // Put everything together.
          val mainString = argPart + arrowPart + resultPart + effPart // MATT find better var name
          if (applyParams.isEmpty) {
            mainString
          } else {
            formatApply(s"($mainString)", applyParams)
          }

        case Type.Lambda(tvar, tpe) => audience match {
          case Audience.Internal => s"${tvar.id.toString} => ${visit(tpe)}"
          case Audience.External => throw InternalCompilerException(s"Unexpected type: Lambda") // Lambda should not be show externally
        }

        case Type.Cst(TypeConstructor.Native(clazz)) => s"${clazz.getSimpleName}"

        case Type.Apply(_, _) => throw InternalCompilerException("Unexpected type: Apply") // Should be impossible

      }
    }

    visit(tpe)

  }

  /**
    * Returns a human readable representation of the given type difference.
    */
  def format(td: TypeDiff, color: String => VirtualString)(implicit audience: Audience): VirtualTerminal = {
    val vt = new VirtualTerminal()

    def visit(d: TypeDiff): Unit = {
      val base = d.typeConstructor
      val args = d.typeArguments

      base match {
        case TypeDiff.Star(constructor) => constructor match {
          case TypeDiff.TyCon.Arrow =>
            intercalate(args, visit, vt, before = "", separator = " -> ", after = "")
          case TypeDiff.TyCon.Enum(name) =>
            vt << name
            intercalate(args, visit, vt, before = "[", separator = ", ", after = "]")
          case TypeDiff.TyCon.Tuple =>
            intercalate(args, visit, vt, before = "(", separator = ", ", after = ")")
          case TypeDiff.TyCon.Other =>
            vt << "*"
            intercalate(args, visit, vt, before = "[", separator = ", ", after = "]")
        }
        case TypeDiff.Mismatch(tpe1, _) => vt << color(format(tpe1))
        case _ => throw InternalCompilerException(s"Unexpected base type: '$base'.")
      }
    }

    visit(td)

    vt
  }

  /**
    * Helper function to generate text before, in the middle of, and after a list of items.
    */
  private def intercalate[A](xs: List[A], f: A => Unit, vt: VirtualTerminal, before: String, separator: String, after: String): Unit = {
    if (xs.isEmpty) return
    vt << before
    var first: Boolean = true
    for (x <- xs) {
      if (first) {
        f(x)
      } else {
        vt << separator
        f(x)
      }
      first = false
    }
    vt << after
  }



  // MATT rename if we keep this
  private case class FlatThing(fields: List[(String, Type)], rest: Type) {
    def ::(head: (String, Type)): FlatThing = {
      copy(fields = head :: fields)
    }
  }

  private def flattenRecord(record: Type): FlatThing = record match {
    case Type.Apply(Type.Apply(Type.Cst(TypeConstructor.RecordExtend(label)), tpe), rest) =>
      (label, tpe) :: flattenRecord(rest)
    case _ => FlatThing(Nil, record)
  }


  private def flattenSchema(schema: Type): FlatThing = schema match {
    case Type.Apply(Type.Apply(Type.Cst(TypeConstructor.SchemaExtend(label)), tpe), rest) =>
      (label, tpe) :: flattenSchema(rest)
    case _ => FlatThing(Nil, schema)
  }


  private def getVarName(index: Int): String = {
    if (index / 26 <= 0)
      (index + 'a').toChar.toString
    else
      (index + 'a').toChar.toString + (index / 26).toString
  }

  private def alphaRenameVars(tpe0: Type): Map[Int, String] = {
    tpe0.typeVars.toList.sortBy(_.id).zipWithIndex.map {
      case (tvar, index) => tvar.id -> getVarName(index)
    }.toMap
  }
}

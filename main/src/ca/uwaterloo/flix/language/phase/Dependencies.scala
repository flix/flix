package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.api.lsp.{Consumer, Visitor}
import ca.uwaterloo.flix.api.lsp.acceptors.AllAcceptor
import ca.uwaterloo.flix.language.ast.{SourceLocation, Symbol, Type, TypeConstructor, TypedAst}
import ca.uwaterloo.flix.language.ast.Symbol.{DefnSym, TraitSym}
import ca.uwaterloo.flix.language.ast.TypedAst.Root
import ca.uwaterloo.flix.language.ast.shared.{DependencyGraph, Input, SymUse}
import ca.uwaterloo.flix.util.collection.MultiMap

import scala.collection.mutable

object Dependencies {

  /** Checks the safety and well-formedness of `root`. */
  def run(root: Root)(implicit flix: Flix): Root = {

    object consumer extends Consumer {
      var deps: MultiMap[SourceLocation, SourceLocation] = MultiMap.empty

      override def consumeCaseSymUse(symUse: SymUse.CaseSymUse): Unit = {
        deps += (symUse.sym.loc -> symUse.loc)
      }

      override def consumeDefSymUse(symUse: SymUse.DefSymUse): Unit = {
        deps += (symUse.sym.loc -> symUse.loc)
      }

      override def consumeEffectSymUse(symUse: SymUse.EffectSymUse): Unit = {
        deps += (symUse.sym.loc -> symUse.loc)
      }

      override def consumeInstance(ins: TypedAst.Instance): Unit = {
        deps += (ins.trt.sym.loc -> ins.loc)
      }

      override def consumeOpSymUse(symUse: SymUse.OpSymUse): Unit = {
        deps += (symUse.sym.loc -> symUse.loc)
      }

      override def consumeSigSymUse(symUse: SymUse.SigSymUse): Unit = {
        deps += (symUse.sym.loc -> symUse.loc)
      }

      override def consumeStructFieldSymUse(symUse: SymUse.StructFieldSymUse): Unit = {
        deps += (symUse.sym.loc -> symUse.loc)
      }

      override def consumeTraitSymUse(symUse: SymUse.TraitSymUse): Unit = {
        deps += (symUse.sym.loc -> symUse.loc)
      }

      override def consumeType(tpe: Type): Unit = tpe match {
        case Type.Alias(cst, _, _, loc) =>
          deps += (cst.sym.loc -> loc)

        case Type.Cst(TypeConstructor.Enum(sym, _), loc) =>
          deps += (sym.loc -> loc)

        case Type.Cst(TypeConstructor.Struct(sym, _), loc) =>
          deps += (sym.loc -> loc)

        case _ => // nop
      }
    }

    Visitor.visitRoot(root, consumer, AllAcceptor)


    val dg = DependencyGraph(consumer.deps)

    println(dg)

    root
  }

}

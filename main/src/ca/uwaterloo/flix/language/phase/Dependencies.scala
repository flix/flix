package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.api.lsp.{Consumer, Visitor}
import ca.uwaterloo.flix.api.lsp.acceptors.AllAcceptor
import ca.uwaterloo.flix.language.ast.{SourceLocation, Type, TypeConstructor, TypedAst}
import ca.uwaterloo.flix.language.ast.TypedAst.Root
import ca.uwaterloo.flix.language.ast.shared.{DependencyGraph, Input, SymUse}
import ca.uwaterloo.flix.util.collection.MultiMap

object Dependencies {

  /** Checks the safety and well-formedness of `root`. */
  def run(root: Root): Root = {

    object consumer extends Consumer {
      var deps: MultiMap[Input, Input] = MultiMap.empty

      /**
        * Adds a dependency `src -> dst` signifying that if `src` changes then `dst` must be recomputed.
        */
      private def addDependency(src: SourceLocation, dst: SourceLocation): Unit = {
        deps += (src.sp1.source.input, dst.sp1.source.input)
      }

      override def consumeCaseSymUse(symUse: SymUse.CaseSymUse): Unit = {
        addDependency(symUse.sym.loc, symUse.loc)
      }

      override def consumeDefSymUse(symUse: SymUse.DefSymUse): Unit = {
        addDependency(symUse.sym.loc, symUse.loc)
      }

      override def consumeEffectSymUse(symUse: SymUse.EffectSymUse): Unit = {
        addDependency(symUse.sym.loc, symUse.loc)
      }

      override def consumeInstance(ins: TypedAst.Instance): Unit = {
        addDependency(ins.trt.sym.loc, ins.loc)
      }

      override def consumeOpSymUse(symUse: SymUse.OpSymUse): Unit = {
        addDependency(symUse.sym.loc, symUse.loc)
      }

      override def consumeSigSymUse(symUse: SymUse.SigSymUse): Unit = {
        addDependency(symUse.sym.loc, symUse.loc)
      }

      override def consumeStructFieldSymUse(symUse: SymUse.StructFieldSymUse): Unit = {
        addDependency(symUse.sym.loc, symUse.loc)
      }

      override def consumeTraitSymUse(symUse: SymUse.TraitSymUse): Unit = {
        addDependency(symUse.sym.loc, symUse.loc)
      }

      override def consumeType(tpe: Type): Unit = tpe match {
        case Type.Alias(cst, _, _, loc) =>
          addDependency(cst.sym.loc, loc)

        case Type.Cst(TypeConstructor.Enum(sym, _), loc) =>
          addDependency(sym.loc, loc)

        case Type.Cst(TypeConstructor.Struct(sym, _), loc) =>
          addDependency(sym.loc, loc)

        case _ => // nop
      }
    }

    Visitor.visitRoot(root, consumer, AllAcceptor)

    val dg = DependencyGraph(consumer.deps)
    println(dg)
    root.copy(dependencyGraph = dg)
  }

}

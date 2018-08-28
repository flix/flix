package ca.uwaterloo.flix.runtime.solver.api

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.{Ast, FinalAst, Symbol}
import ca.uwaterloo.flix.runtime.{InvocationTarget, Linker}
import ca.uwaterloo.flix.runtime.solver.LatticeOps
import ca.uwaterloo.flix.runtime.solver.api.predicate._
import ca.uwaterloo.flix.runtime.solver.api.symbol.VarSym
import ca.uwaterloo.flix.runtime.solver.api.term._

import scala.collection.mutable

object ApiBridge {

  // TODO: Temporary class.

  // Class used to ensure that the symbols share the same object by identity.
  class SymbolCache {

    val varSyms = mutable.Map.empty[Symbol.VarSym, VarSym]
    val relSyms = mutable.Map.empty[Symbol.RelSym, Relation]
    val latSyms = mutable.Map.empty[Symbol.LatSym, Lattice]

    def getVarSym(sym: Symbol.VarSym): VarSym =
      varSyms.get(sym) match {
        case None =>
          val newSym = new VarSym(sym.text)
          varSyms += (sym -> newSym)
          newSym
        case Some(res) => res
      }

    def getRelSym(sym: Symbol.RelSym, name: String, attributes: Array[Attribute]): Relation =
      relSyms.get(sym) match {
        case None =>
          val newSym = new Relation(name, attributes)
          relSyms += (sym -> newSym)
          newSym
        case Some(res) => res
      }

    def getLatSym(sym: Symbol.LatSym, name: String, keys: Array[Attribute], value: Attribute, ops: LatticeOps): Lattice =
      latSyms.get(sym) match {
        case None =>
          val newSym = new Lattice(name, keys, value, ops)
          latSyms += (sym -> newSym)
          newSym
        case Some(res) => res
      }

  }

  def translate(root: FinalAst.Root)(implicit flix: Flix): ConstraintSet = {
    implicit val _ = root
    implicit val cache = new SymbolCache

    val strata = root.strata.map(visitStratum).toArray

    new ConstraintSet(strata.flatten)
  }

  private def visitStratum(stratum: FinalAst.Stratum)(implicit root: FinalAst.Root, cache: SymbolCache, flix: Flix): Array[Constraint] = {
    stratum.constraints.map(visitConstraint).toArray
  }

  def visitConstraint(c0: FinalAst.Constraint)(implicit root: FinalAst.Root, cache: SymbolCache, flix: Flix): Constraint = {
    val cparams = c0.cparams.map(visitConstraintParam)
    val head = visitHeadPredicate(c0.head)
    val body = c0.body.map(visitBodyPredicate)
    new Constraint(cparams.toArray, head, body.toArray)
  }

  private def visitConstraintParam(c0: FinalAst.ConstraintParam)(implicit root: FinalAst.Root, cache: SymbolCache, flix: Flix): VarSym = c0 match {
    case FinalAst.ConstraintParam.HeadParam(sym, _, _) => visitVarSym(sym)
    case FinalAst.ConstraintParam.RuleParam(sym, _, _) => visitVarSym(sym)
  }

  private def visitHeadPredicate(h0: FinalAst.Predicate.Head)(implicit root: FinalAst.Root, cache: SymbolCache, flix: Flix): Predicate = h0 match {
    case FinalAst.Predicate.Head.True(_) => new TruePredicate()
    case FinalAst.Predicate.Head.False(_) => new FalsePredicate()
    case FinalAst.Predicate.Head.RelAtom(_, sym, terms, _) => new AtomPredicate(visitRelSym(sym), positive = true, terms.map(visitHeadTerm).toArray, null)
    case FinalAst.Predicate.Head.LatAtom(_, sym, terms, _) => new AtomPredicate(visitLatSym(sym), positive = true, terms.map(visitHeadTerm).toArray, null)
  }

  private def visitBodyPredicate(b0: FinalAst.Predicate.Body)(implicit root: FinalAst.Root, cache: SymbolCache, flix: Flix): Predicate = b0 match {
    case FinalAst.Predicate.Body.RelAtom(_, sym, polarity, terms, index2sym, loc) =>
      val s = visitRelSym(sym)
      val p = polarity match {
        case Ast.Polarity.Positive => true
        case Ast.Polarity.Negative => false
      }
      val ts = terms.map(visitBodyTerm)
      val i2s = index2sym map {
        case x if x != null => visitVarSym(x)
        case _ => null
      }
      new AtomPredicate(s, p, ts.toArray, i2s.toArray)

    case FinalAst.Predicate.Body.LatAtom(_, sym, polarity, terms, index2sym, loc) =>
      val s = visitLatSym(sym)
      val p = polarity match {
        case Ast.Polarity.Positive => true
        case Ast.Polarity.Negative => false
      }
      val ts = terms.map(visitBodyTerm)
      val i2s = index2sym map {
        case x if x != null => visitVarSym(x)
        case _ => null
      }
      new AtomPredicate(s, p, ts.toArray, i2s.toArray)

    case FinalAst.Predicate.Body.Filter(sym, terms, loc) =>
      val f = (as: Array[AnyRef]) => Linker.link(sym, root).invoke(as).getValue.asInstanceOf[Boolean].booleanValue()
      val ts = terms.map(visitBodyTerm)
      new FilterPredicate(f, ts.toArray)

    case FinalAst.Predicate.Body.Functional(varSym, defSym, terms, loc) =>
      val s = cache.getVarSym(varSym)
      // TODO: Problem here is that an array does not contain proxy objects.
      val f = (as: Array[AnyRef]) => Linker.link(defSym, root).invoke(as).getValue.asInstanceOf[Array[ProxyObject]]
      new FunctionalPredicate(s, f, terms.map(t => cache.getVarSym(t)).toArray)
  }

  private def visitRelSym(sym: Symbol.RelSym)(implicit root: FinalAst.Root, cache: SymbolCache, flix: Flix): Table =
    root.relations(sym) match {
      case r: FinalAst.Relation =>
        val attributes = r.attr.map(visitAttribute)
        cache.getRelSym(sym, sym.toString, attributes.toArray)
    }

  private def visitLatSym(sym: Symbol.LatSym)(implicit root: FinalAst.Root, cache: SymbolCache, flix: Flix): Table =
    root.lattices(sym) match {
      case l: FinalAst.Lattice =>
        val attributes = l.attr.map(visitAttribute)
        val keys = attributes.init
        val value = attributes.last
        val ops = getLatticeOps(l.attr.last)
        cache.getLatSym(sym, sym.toString, keys.toArray, value, ops)
    }

  private def visitHeadTerm(t0: FinalAst.Term.Head)(implicit root: FinalAst.Root, cache: SymbolCache, flix: Flix): Term = t0 match {
    case FinalAst.Term.Head.QuantVar(sym, _, _) => new VarTerm(visitVarSym(sym))
    case FinalAst.Term.Head.CapturedVar(sym, _, _) => ???
    case FinalAst.Term.Head.Lit(sym, _, _) => new LitTerm(() => Linker.link(sym, root).invoke(Array.emptyObjectArray))
    case FinalAst.Term.Head.App(sym, args, _, _) =>
      val f = (args: Array[AnyRef]) => Linker.link(sym, root).invoke(args)
      val as = args.map(visitVarSym)
      new AppTerm(f, as.toArray)
  }

  private def visitBodyTerm(t0: FinalAst.Term.Body)(implicit root: FinalAst.Root, cache: SymbolCache, flix: Flix): Term = t0 match {
    case FinalAst.Term.Body.Wild(_, _) => new WildTerm()
    case FinalAst.Term.Body.QuantVar(sym, _, _) => new VarTerm(visitVarSym(sym))
    case FinalAst.Term.Body.CapturedVar(sym, _, _) => ???
    case FinalAst.Term.Body.Lit(sym, _, _) => new LitTerm(() => Linker.link(sym, root).invoke(Array.emptyObjectArray))
  }

  private def visitVarSym(sym: Symbol.VarSym)(implicit root: FinalAst.Root, cache: SymbolCache, flix: Flix): VarSym =
    cache.getVarSym(sym)

  private def visitAttribute(a: FinalAst.Attribute)(implicit root: FinalAst.Root, cache: SymbolCache, flix: Flix): Attribute =
    new Attribute(a.name)

  private def visitLatOps(tables: Map[Symbol.LatSym, FinalAst.Lattice])(implicit root: FinalAst.Root, cache: SymbolCache, flix: Flix): Map[Table, LatticeOps] = {
    tables.foldLeft(Map.empty[Table, LatticeOps]) {
      case (macc, (sym, FinalAst.Lattice(_, _, attr, _))) =>
        // lattice
        val latticeOps = getLatticeOps(attr.last)
        macc + (visitLatSym(sym) -> latticeOps)
    }
  }

  private def getLatticeOps(value: FinalAst.Attribute)(implicit root: FinalAst.Root, cache: SymbolCache, flix: Flix): LatticeOps = {
    val lattice = root.latticeComponents(value.tpe)

    new LatticeOps {
      override def bot: ProxyObject = Linker.link(lattice.bot, root).invoke(Array.empty)

      override def equ: InvocationTarget = Linker.link(lattice.equ, root)

      override def leq: InvocationTarget = Linker.link(lattice.leq, root)

      override def lub: InvocationTarget = Linker.link(lattice.lub, root)

      override def glb: InvocationTarget = Linker.link(lattice.glb, root)
    }
  }

}

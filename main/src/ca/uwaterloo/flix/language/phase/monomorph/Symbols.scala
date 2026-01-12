/*
 * Copyright 2021 Magnus Madsen
 *                Casper Dalgaard Nielsen
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

package ca.uwaterloo.flix.language.phase.monomorph

import ca.uwaterloo.flix.language.ast.{Kind, SourceLocation, Symbol, Type, TypeConstructor}

object Symbols {
  protected[monomorph] object Defs {

    lazy val ChannelGet: Symbol.DefnSym = Symbol.mkDefnSym("Concurrent.Channel.get")
    lazy val ChannelNew: Symbol.DefnSym = Symbol.mkDefnSym("Concurrent.Channel.newChannel")
    lazy val ChannelPut: Symbol.DefnSym = Symbol.mkDefnSym("Concurrent.Channel.put")
    lazy val ChannelMpmcAdmin: Symbol.DefnSym = Symbol.mkDefnSym("Concurrent.Channel.mpmcAdmin")
    lazy val ChannelNewTuple: Symbol.DefnSym = Symbol.mkDefnSym("Concurrent.Channel.newChannelTuple")
    lazy val ChannelSelectFrom: Symbol.DefnSym = Symbol.mkDefnSym("Concurrent.Channel.selectFrom")
    lazy val ChannelUnsafeGetAndUnlock: Symbol.DefnSym = Symbol.mkDefnSym("Concurrent.Channel.unsafeGetAndUnlock")

    val version: String = "3"
    lazy val Box: Symbol.DefnSym = Symbol.mkDefnSym(s"Fixpoint$version.Boxable.box")
    lazy val Unbox: Symbol.DefnSym = Symbol.mkDefnSym(s"Fixpoint$version.Boxable.unbox")
    lazy val Solve: Symbol.DefnSym = Symbol.mkDefnSym(s"Fixpoint$version.Solver.runSolver")
    lazy val SolveWithProvenance: Symbol.DefnSym = Symbol.mkDefnSym(s"Fixpoint$version.Solver.runSolverWithProvenance")
    lazy val Merge: Symbol.DefnSym = Symbol.mkDefnSym(s"Fixpoint$version.Solver.union")
    lazy val Filter: Symbol.DefnSym = Symbol.mkDefnSym(s"Fixpoint$version.Solver.projectSym")
    lazy val Rename: Symbol.DefnSym = Symbol.mkDefnSym(s"Fixpoint$version.Solver.rename")
    lazy val ProvenanceOf: Symbol.DefnSym = Symbol.mkDefnSym(s"Fixpoint$version.Solver.provenanceOf")

    def ProjectInto(arity: Int): Symbol.DefnSym = Symbol.mkDefnSym(s"Fixpoint$version.Solver.injectInto$arity")

    def Facts(arity: Int): Symbol.DefnSym = Symbol.mkDefnSym(s"Fixpoint$version.Solver.facts$arity")

  }

  protected[monomorph] object Enums {
    lazy val ChannelMpmc: Symbol.EnumSym = Symbol.mkEnumSym("Concurrent.Channel.Mpmc")
    lazy val ChannelMpmcAdmin: Symbol.EnumSym = Symbol.mkEnumSym("Concurrent.Channel.MpmcAdmin")
    lazy val ConcurrentReentrantLock: Symbol.EnumSym = Symbol.mkEnumSym("Concurrent.ReentrantLock")
    lazy val FList: Symbol.EnumSym = Symbol.mkEnumSym("List")

    lazy val Datalog: Symbol.EnumSym = Symbol.mkEnumSym(s"Fixpoint${Defs.version}.Ast.Datalog.Datalog")
    lazy val Constraint: Symbol.EnumSym = Symbol.mkEnumSym(s"Fixpoint${Defs.version}.Ast.Datalog.Constraint")

    lazy val HeadPredicate: Symbol.EnumSym = Symbol.mkEnumSym(s"Fixpoint${Defs.version}.Ast.Datalog.HeadPredicate")
    lazy val BodyPredicate: Symbol.EnumSym = Symbol.mkEnumSym(s"Fixpoint${Defs.version}.Ast.Datalog.BodyPredicate")

    lazy val HeadTerm: Symbol.EnumSym = Symbol.mkEnumSym(s"Fixpoint${Defs.version}.Ast.Datalog.HeadTerm")
    lazy val BodyTerm: Symbol.EnumSym = Symbol.mkEnumSym(s"Fixpoint${Defs.version}.Ast.Datalog.BodyTerm")

    lazy val PredSym: Symbol.EnumSym = Symbol.mkEnumSym(s"Fixpoint${Defs.version}.Ast.Shared.PredSym")
    lazy val VarSym: Symbol.EnumSym = Symbol.mkEnumSym(s"Fixpoint${Defs.version}.Ast.Datalog.VarSym")

    lazy val Denotation: Symbol.EnumSym = Symbol.mkEnumSym(s"Fixpoint${Defs.version}.Ast.Shared.Denotation")
    lazy val Polarity: Symbol.EnumSym = Symbol.mkEnumSym(s"Fixpoint${Defs.version}.Ast.Datalog.Polarity")
    lazy val Fixity: Symbol.EnumSym = Symbol.mkEnumSym(s"Fixpoint${Defs.version}.Ast.Datalog.Fixity")

    lazy val Boxed: Symbol.EnumSym = Symbol.mkEnumSym(s"Fixpoint${Defs.version}.Boxed")
  }

  protected[monomorph] object Types {
    lazy val ChannelMpmc: Type = Type.Cst(TypeConstructor.Enum(Enums.ChannelMpmc, Kind.Star ->: Kind.Eff ->: Kind.Star), SourceLocation.Unknown)
    lazy val ChannelMpmcAdmin: Type = Type.mkEnum(Enums.ChannelMpmcAdmin, Nil, SourceLocation.Unknown)
    lazy val ConcurrentReentrantLock: Type = Type.mkEnum(Enums.ConcurrentReentrantLock, Nil, SourceLocation.Unknown)

    def mkList(t: Type, loc: SourceLocation): Type = Type.mkEnum(Enums.FList, List(t), loc)


    lazy val Datalog: Type = Type.mkEnum(Enums.Datalog, Nil, SourceLocation.Unknown)
    lazy val Constraint: Type = Type.mkEnum(Enums.Constraint, Nil, SourceLocation.Unknown)

    lazy val HeadPredicate: Type = Type.mkEnum(Enums.HeadPredicate, Nil, SourceLocation.Unknown)
    lazy val BodyPredicate: Type = Type.mkEnum(Enums.BodyPredicate, Nil, SourceLocation.Unknown)

    lazy val HeadTerm: Type = Type.mkEnum(Enums.HeadTerm, Nil, SourceLocation.Unknown)
    lazy val BodyTerm: Type = Type.mkEnum(Enums.BodyTerm, Nil, SourceLocation.Unknown)

    lazy val PredSym: Type = Type.mkEnum(Enums.PredSym, Nil, SourceLocation.Unknown)
    lazy val VarSym: Type = Type.mkEnum(Enums.VarSym, Nil, SourceLocation.Unknown)

    lazy val Denotation: Type = Type.mkEnum(Enums.Denotation, Boxed :: Nil, SourceLocation.Unknown)
    lazy val Polarity: Type = Type.mkEnum(Enums.Polarity, Nil, SourceLocation.Unknown)
    lazy val Fixity: Type = Type.mkEnum(Enums.Fixity, Nil, SourceLocation.Unknown)

    lazy val Boxed: Type = Type.mkEnum(Enums.Boxed, Nil, SourceLocation.Unknown)

    lazy val VectorOfBoxed: Type = Type.mkVector(Types.Boxed, SourceLocation.Unknown)

    //
    // Function Types.
    //
    lazy val SolveType: Type = Type.mkPureArrow(Datalog, Datalog, SourceLocation.Unknown)
    lazy val MergeType: Type = Type.mkPureUncurriedArrow(List(Datalog, Datalog), Datalog, SourceLocation.Unknown)
    lazy val FilterType: Type = Type.mkPureUncurriedArrow(List(PredSym, Datalog), Datalog, SourceLocation.Unknown)
    lazy val RenameType: Type = Type.mkPureUncurriedArrow(List(mkList(PredSym, SourceLocation.Unknown), Datalog), Datalog, SourceLocation.Unknown)

    def mkProvenanceOf(t: Type, loc: SourceLocation): Type =
      Type.mkPureUncurriedArrow(
        List(
          PredSym,
          Type.mkVector(Boxed, loc),
          Type.mkVector(PredSym, loc),
          Type.mkPureCurriedArrow(List(PredSym, Type.mkVector(Boxed, loc)), t, loc),
          Datalog
        ),
        Type.mkVector(t, loc), loc
      )
  }

}

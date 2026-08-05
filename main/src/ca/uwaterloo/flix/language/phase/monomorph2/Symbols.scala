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

package ca.uwaterloo.flix.language.phase.monomorph2

import ca.uwaterloo.flix.language.ast.{Kind, SourceLocation, Symbol, Type, TypeConstructor}

/**
  * A collection of symbols defined in the Flix Standard Library that this pipeline's Channel and
  * Datalog lowering refers to. Each entry here must match a real stdlib symbol of the same name
  * and type.
  */
private[monomorph2] object Symbols {
  val fixpointVersion: String = "3"

  object Defs {
    object Concurrent {
      object Channel {
        lazy val Get: Symbol.DefnSym = Symbol.mkDefnSym("Concurrent.Channel.get")
        lazy val NewChannel: Symbol.DefnSym = Symbol.mkDefnSym("Concurrent.Channel.newChannel")
        lazy val Put: Symbol.DefnSym = Symbol.mkDefnSym("Concurrent.Channel.put")
        lazy val MpmcAdmin: Symbol.DefnSym = Symbol.mkDefnSym("Concurrent.Channel.mpmcAdmin")
        lazy val NewChannelTuple: Symbol.DefnSym = Symbol.mkDefnSym("Concurrent.Channel.newChannelTuple")
        lazy val SelectFrom: Symbol.DefnSym = Symbol.mkDefnSym("Concurrent.Channel.selectFrom")
        lazy val UnsafeGetAndUnlock: Symbol.DefnSym = Symbol.mkDefnSym("Concurrent.Channel.unsafeGetAndUnlock")
      }
    }

    object Fixpoint {
      object Boxable {
        lazy val Box: Symbol.DefnSym = Symbol.mkDefnSym(s"Fixpoint$fixpointVersion.Boxable.box")
        lazy val Unbox: Symbol.DefnSym = Symbol.mkDefnSym(s"Fixpoint$fixpointVersion.Boxable.unbox")

        def Lift(arity: Int): Symbol.DefnSym = Symbol.mkDefnSym(s"Fixpoint$fixpointVersion.Boxable.lift$arity")
        def LiftB(arity: Int): Symbol.DefnSym = Symbol.mkDefnSym(s"Fixpoint$fixpointVersion.Boxable.lift${arity}b")
        def LiftXM(inArity: Int, outArity: Int): Symbol.DefnSym = Symbol.mkDefnSym(s"Fixpoint$fixpointVersion.Boxable.lift${inArity}X$outArity")
      }

      object Solver {
        lazy val RunSolver: Symbol.DefnSym = Symbol.mkDefnSym(s"Fixpoint$fixpointVersion.Solver.runSolver")
        lazy val RunSolverWithProvenance: Symbol.DefnSym = Symbol.mkDefnSym(s"Fixpoint$fixpointVersion.Solver.runSolverWithProvenance")
        lazy val Union: Symbol.DefnSym = Symbol.mkDefnSym(s"Fixpoint$fixpointVersion.Solver.union")
        lazy val ProjectSym: Symbol.DefnSym = Symbol.mkDefnSym(s"Fixpoint$fixpointVersion.Solver.projectSym")
        lazy val Rename: Symbol.DefnSym = Symbol.mkDefnSym(s"Fixpoint$fixpointVersion.Solver.rename")
        lazy val ProvenanceOf: Symbol.DefnSym = Symbol.mkDefnSym(s"Fixpoint$fixpointVersion.Solver.provenanceOf")

        def InjectInto(arity: Int): Symbol.DefnSym = Symbol.mkDefnSym(s"Fixpoint$fixpointVersion.Solver.injectInto$arity")
        def Facts(arity: Int): Symbol.DefnSym = Symbol.mkDefnSym(s"Fixpoint$fixpointVersion.Solver.facts$arity")
      }

      object Ast {
        object Shared {
          lazy val Lattice: Symbol.DefnSym = Symbol.mkDefnSym(s"Fixpoint$fixpointVersion.Ast.Shared.lattice")
          lazy val Box: Symbol.DefnSym = Symbol.mkDefnSym(s"Fixpoint$fixpointVersion.Ast.Shared.box")
        }
      }
    }

    object Vector {
      lazy val Get: Symbol.DefnSym = Symbol.mkDefnSym("Vector.get")
    }
  }

  object Enums {
    object Concurrent {
      object Channel {
        lazy val Mpmc: Symbol.EnumSym = Symbol.mkEnumSym("Concurrent.Channel.Mpmc")
        lazy val MpmcAdmin: Symbol.EnumSym = Symbol.mkEnumSym("Concurrent.Channel.MpmcAdmin")
      }

      object ReentrantLock {
        lazy val ReentrantLock: Symbol.EnumSym = Symbol.mkEnumSym("Concurrent.ReentrantLock")
      }
    }

    object Fixpoint {
      object Ast {
        object Datalog {
          lazy val Datalog: Symbol.EnumSym = Symbol.mkEnumSym(s"Fixpoint${fixpointVersion}.Ast.Datalog")
          lazy val Constraint: Symbol.EnumSym = Symbol.mkEnumSym(s"Fixpoint${fixpointVersion}.Ast.Datalog.Constraint")
          lazy val HeadPredicate: Symbol.EnumSym = Symbol.mkEnumSym(s"Fixpoint${fixpointVersion}.Ast.Datalog.HeadPredicate")
          lazy val BodyPredicate: Symbol.EnumSym = Symbol.mkEnumSym(s"Fixpoint${fixpointVersion}.Ast.Datalog.BodyPredicate")
          lazy val HeadTerm: Symbol.EnumSym = Symbol.mkEnumSym(s"Fixpoint${fixpointVersion}.Ast.Datalog.HeadTerm")
          lazy val BodyTerm: Symbol.EnumSym = Symbol.mkEnumSym(s"Fixpoint${fixpointVersion}.Ast.Datalog.BodyTerm")
          lazy val VarSym: Symbol.EnumSym = Symbol.mkEnumSym(s"Fixpoint${fixpointVersion}.Ast.Datalog.VarSym")
          lazy val Polarity: Symbol.EnumSym = Symbol.mkEnumSym(s"Fixpoint${fixpointVersion}.Ast.Datalog.Polarity")
          lazy val Fixity: Symbol.EnumSym = Symbol.mkEnumSym(s"Fixpoint${fixpointVersion}.Ast.Datalog.Fixity")
        }

        object Shared {
          lazy val PredSym: Symbol.EnumSym = Symbol.mkEnumSym(s"Fixpoint${fixpointVersion}.Ast.Shared.PredSym")
          lazy val Denotation: Symbol.EnumSym = Symbol.mkEnumSym(s"Fixpoint${fixpointVersion}.Ast.Shared.Denotation")
        }
      }

      lazy val Boxed: Symbol.EnumSym = Symbol.mkEnumSym(s"Fixpoint${fixpointVersion}.Boxed")
    }

    object List {
      lazy val List: Symbol.EnumSym = Symbol.mkEnumSym("List")
    }

    object Reflect {
      lazy val Purity: Symbol.EnumSym = Symbol.mkEnumSym("Reflect.Purity")
      lazy val JvmType: Symbol.EnumSym = Symbol.mkEnumSym("Reflect.JvmType")
      lazy val JvmValue: Symbol.EnumSym = Symbol.mkEnumSym("Reflect.JvmValue")
    }
  }

  object Types {
    object Concurrent {
      object Channel {
        lazy val Mpmc: Type = Type.Cst(TypeConstructor.Enum(Enums.Concurrent.Channel.Mpmc, Kind.Star ->: Kind.Eff ->: Kind.Star), SourceLocation.Unknown)
        lazy val MpmcAdmin: Type = Type.mkEnum(Enums.Concurrent.Channel.MpmcAdmin, Nil, SourceLocation.Unknown)
      }

      object ReentrantLock {
        lazy val ReentrantLock: Type = Type.mkEnum(Enums.Concurrent.ReentrantLock.ReentrantLock, Nil, SourceLocation.Unknown)
      }
    }

    object Fixpoint {
      object Solver {
        // Synthetic, these are not real stdlib declarations, just the corresponding def's arrow type.
        lazy val SolveType: Type = Type.mkPureArrow(Types.Fixpoint.Ast.Datalog.Datalog, Types.Fixpoint.Ast.Datalog.Datalog, SourceLocation.Unknown)
        lazy val MergeType: Type = Type.mkPureUncurriedArrow(scala.collection.immutable.List(Types.Fixpoint.Ast.Datalog.Datalog, Types.Fixpoint.Ast.Datalog.Datalog), Types.Fixpoint.Ast.Datalog.Datalog, SourceLocation.Unknown)
        lazy val FilterType: Type = Type.mkPureUncurriedArrow(scala.collection.immutable.List(Types.Fixpoint.Ast.Shared.PredSym, Types.Fixpoint.Ast.Datalog.Datalog), Types.Fixpoint.Ast.Datalog.Datalog, SourceLocation.Unknown)
        lazy val RenameType: Type = Type.mkPureUncurriedArrow(scala.collection.immutable.List(Types.List.mkList(Types.Fixpoint.Ast.Shared.PredSym, SourceLocation.Unknown), Types.Fixpoint.Ast.Datalog.Datalog), Types.Fixpoint.Ast.Datalog.Datalog, SourceLocation.Unknown)

        def mkProvenanceOf(t: Type, loc: SourceLocation): Type =
          Type.mkPureUncurriedArrow(
            scala.collection.immutable.List(
              Types.Fixpoint.Ast.Shared.PredSym,
              Type.mkVector(Types.Fixpoint.Boxed, loc),
              Type.mkVector(Types.Fixpoint.Ast.Shared.PredSym, loc),
              Type.mkPureCurriedArrow(scala.collection.immutable.List(Types.Fixpoint.Ast.Shared.PredSym, Type.mkVector(Boxed, loc)), t, loc),
              Types.Fixpoint.Ast.Datalog.Datalog
            ),
            Type.mkVector(t, loc), loc
          )
      }

      object Ast {
        object Datalog {
          lazy val Datalog: Type = Type.mkEnum(Enums.Fixpoint.Ast.Datalog.Datalog, Nil, SourceLocation.Unknown)
          lazy val Constraint: Type = Type.mkEnum(Enums.Fixpoint.Ast.Datalog.Constraint, Nil, SourceLocation.Unknown)
          lazy val HeadPredicate: Type = Type.mkEnum(Enums.Fixpoint.Ast.Datalog.HeadPredicate, Nil, SourceLocation.Unknown)
          lazy val BodyPredicate: Type = Type.mkEnum(Enums.Fixpoint.Ast.Datalog.BodyPredicate, Nil, SourceLocation.Unknown)
          lazy val HeadTerm: Type = Type.mkEnum(Enums.Fixpoint.Ast.Datalog.HeadTerm, Nil, SourceLocation.Unknown)
          lazy val BodyTerm: Type = Type.mkEnum(Enums.Fixpoint.Ast.Datalog.BodyTerm, Nil, SourceLocation.Unknown)
          lazy val VarSym: Type = Type.mkEnum(Enums.Fixpoint.Ast.Datalog.VarSym, Nil, SourceLocation.Unknown)
          lazy val Polarity: Type = Type.mkEnum(Enums.Fixpoint.Ast.Datalog.Polarity, Nil, SourceLocation.Unknown)
          lazy val Fixity: Type = Type.mkEnum(Enums.Fixpoint.Ast.Datalog.Fixity, Nil, SourceLocation.Unknown)
        }

        object Shared {
          lazy val PredSym: Type = Type.mkEnum(Enums.Fixpoint.Ast.Shared.PredSym, Nil, SourceLocation.Unknown)
          lazy val Denotation: Type = Type.mkEnum(Enums.Fixpoint.Ast.Shared.Denotation, Boxed :: Nil, SourceLocation.Unknown)
        }
      }

      lazy val Boxed: Type = Type.mkEnum(Enums.Fixpoint.Boxed, Nil, SourceLocation.Unknown)
      lazy val VectorOfBoxed: Type = Type.mkVector(Boxed, SourceLocation.Unknown)
    }

    object List {
      def mkList(t: Type, loc: SourceLocation): Type = Type.mkEnum(Enums.List.List, scala.collection.immutable.List(t), loc)
    }
  }
}

/*
 * Copyright 2021 Benjamin Dahse
 * Copyright 2025 Casper Dalgaard Nielsen
 *                Adam Yasser Tallouzi
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

pub mod Fixpoint3.Phase.Stratifier {
    use Fixpoint3.Ast.Datalog.BodyPredicate.BodyAtom
    use Fixpoint3.Ast.Datalog.Constraint.Constraint
    use Fixpoint3.Ast.Datalog.Datalog.{Datalog, Join, Model, Provenance}
    use Fixpoint3.Ast.Datalog.HeadPredicate.HeadAtom
    use Fixpoint3.Ast.Datalog.{BodyPredicate, Constraint, Datalog, Fixity, HeadPredicate, Polarity}
    use Fixpoint3.Ast.Shared.PredSym
    use Fixpoint3.Util.getOrCrash
    use Fixpoint3.PrecedenceGraph.{getPseudoStrata, MutGraph, PseudoStratum, Vertex}

    ///
    /// Array allowing lookup of the id-part of `PredSym` to name-part of `PredSym`.
    ///
    type alias PredIdToName[r: Region] = Array[String, r]

    ///
    /// Computes the stratification of `d`.
    ///
    /// Returns a map from `PredSym` to their position within their pseudostrata:
    /// `predSym -> (pseudoStratumIndex, stratumIndexInPseudoStratum)`
    ///
    /// A pseudostratum is a group of mutually independent, consecutive strata. For example,
    /// `predSym -> (2, 1)` means `predSym` belongs to stratum 1 within pseudostrata 2.
    ///
    pub def stratify(d: Datalog): Map[PredSym, (Int32, Int32)] = region rc {
        let depGraph = MutGraph.empty(rc);
        addDepEdges(depGraph, d);
        mkStratificationMap(rc, d, depGraph)
    }

    ///
    /// Given `d` with dependency graph `graph`, constructs a map from `PredSym`s in `d`
    /// to their stratum index within their pseudostrata.
    ///
    def mkStratificationMap(rc: Region[r], d: Datalog, graph: MutGraph[r]): Map[PredSym, (Int32, Int32)] \ r =
        let pseudoStrata = getPseudoStrata(graph);
        let numberOfPredSyms = MutGraph.numberOfVertices(graph);
        let predSyms = Array.empty(rc, numberOfPredSyms);
        addPredSymsFrom(predSyms, d);
        let result = MutMap.empty(rc);
        foreach ((pseudoStratum, list) <- ForEach.withIndex(pseudoStrata)) {
            foreach ((stratum, set) <- ForEach.withIndex(list)) {
                foreach (x <- set) {
                    let predSym = PredSym.PredSym(Array.get(x, predSyms), Int32.toInt64(x));
                    MutMap.put(predSym, (pseudoStratum, stratum), result)
                }

            }
        };
        MutMap.toMap(result)

    ///
    /// Insert edges into `graph` for the dependencies in the Datalog program `d`.
    /// `A(x) :- B(x), A(x), -C(x).` inserts the edges `B -> A` and `C -> A` in `graph`.
    ///
    def addDepEdges(graph: MutGraph[r], d: Datalog): Unit \ r = match d {
        case Datalog(_, rules) => Vector.forEach(precedenceHelper(graph), rules)
        case Model(_)          => ()
        case Join(d1, d2)      =>
            addDepEdges(graph, d1);
            addDepEdges(graph, d2)
        case Provenance(_, _)  => ()
    }

    ///
    /// Inserts edges into `graph` for the dependencies in the constraints `cnst`.
    ///
    /// See `addDepEdges`.
    ///
    def precedenceHelper(graph: MutGraph[r], cnst: Constraint): Unit \ r = match cnst {
        case Constraint(head, body) => Vector.forEach(addDepEdge(graph, head), body)
    }

    ///
    /// Inserts edges into `graph` for the found dependency between `dst` and `src`. No
    /// edge is inserted if `dst == src`.
    ///
    /// See `addDepEdges`.
    ///
    def addDepEdge(graph: MutGraph[r], dst: HeadPredicate, src: BodyPredicate): Unit \ r = match (dst, src) {
        case (HeadAtom(PredSym.PredSym(_, dstId), _, _), BodyAtom(PredSym.PredSym(_, srcId), _, _, _, _)) => {
            if (dstId != srcId) {
                MutGraph.addEdge(toInt32(srcId), toInt32(dstId), graph)
            } else (
                // We still register the vertex, as it is needed later.
                MutGraph.addVertex(toInt32(dstId), graph)
            )
        }
        case _ => ()
    }

    ///
    /// Collects the names of the `PredSym`s of `d`. A `PredSym` `(name, id)` in `d` will
    /// ensure that `predMap[id] == name`.
    ///
    def addPredSymsFrom(predMap: PredIdToName[r], d: Datalog): Unit \ r = match d {
        case Datalog(_, rules) => Vector.forEach(predSymHelper(predMap), rules)
        case Model(_)          => ()
        case Join(d1, d2)      => addPredSymsFrom(predMap, d1); addPredSymsFrom(predMap, d2)
        case Provenance(_, _)  => ()
    }

    ///
    /// Collects the names of the `PredSym`s of `cnst`.
    ///
    /// See `addPredSymsFrom`.
    ///
    def predSymHelper(predMap: PredIdToName[r], cnst: Constraint): Unit \ r = match cnst {
        case Constraint(HeadPredicate.HeadAtom(PredSym.PredSym(name, id), _, _), body) =>
            Array.put(name, toInt32(id), predMap);
            Vector.forEach(addPredSymsOf(predMap), body)
    }

    ///
    /// Collects the names of the `PredSym`s of `body`.
    ///
    /// See `addPredSymsFrom`.
    ///
    def addPredSymsOf(predMap: PredIdToName[r], body: BodyPredicate): Unit \ r = match body {
        case (BodyAtom(PredSym.PredSym(name, id), _, _, _, _)) => {
            Array.put(name, toInt32(id), predMap);
            ()
        }
        case _ => ()
    }

    ///
    /// Cast `i` to an `Int32`. Crashes if there are more than 2^32 `PredSyms`s.
    ///
    def toInt32(i: Int64): Int32 = getOrCrash(Int64.tryToInt32(i))
}

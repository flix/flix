/*
 * Copyright 2018 Magnus Madsen
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

package flix.runtime.fixpoint;

import flix.runtime.fixpoint.predicate.AtomPredicate;
import flix.runtime.fixpoint.predicate.Predicate;
import flix.runtime.fixpoint.ram.interpreter.RamInterpreter;
import flix.runtime.fixpoint.ram.stmt.Stmt;
import flix.runtime.fixpoint.symbol.PredSym;

import java.util.Arrays;
import java.util.LinkedList;

public final class Solver {

    /**
     * Returns the composition of `cs1` with `cs2`.
     */
    public static ConstraintSystem compose(ConstraintSystem cs1, ConstraintSystem cs2) {
        var constraints = concat(cs1.getConstraints(), cs2.getConstraints());
        return ConstraintSystem.of(constraints);
    }

    /**
     * Returns the projection of the given predicate symbol `sym` of the given constraint system `cs`.
     */
    public static ConstraintSystem project(PredSym sym, ConstraintSystem cs) {
        if (sym == null)
            throw new IllegalArgumentException("'sym' must be non-null.");
        if (cs == null)
            throw new IllegalArgumentException("'cs' must be non-null.");

        // Collect all facts with `sym` in its head.
        var result = new LinkedList<Constraint>();
        for (Constraint fact : cs.getFacts()) {
            Predicate head = fact.getHeadPredicate();
            if (head instanceof AtomPredicate) {
                if (((AtomPredicate) head).getSym().equals(sym)) {
                    result.add(fact);
                }
            }
        }

        Constraint[] facts = result.toArray(new Constraint[0]);
        Constraint[] rules = new Constraint[0];
        Constraint[] constraints = concat(facts, rules);
        return ConstraintSystem.of(constraints);
    }

    /**
     * Solves the given constraint system `cs` with the given stratification `stf` and options `o`.
     */
    public static ConstraintSystem solve(ConstraintSystem cs, Stratification stf, Options o) {
        ca.uwaterloo.flix.runtime.solver.Solver solver = new ca.uwaterloo.flix.runtime.solver.Solver(cs, stf, o);
        Stmt compiled = DatalogCompiler.compileProgram(cs, stf, o);
        return RamInterpreter.run(compiled);
    }

    /**
     * Returns `true` if all facts in `cs2` are included in `cs1`.
     */
    public static boolean entails(ConstraintSystem cs1, ConstraintSystem cs2) {
        var entails = true;
        for (Constraint fact2 : cs2.getFacts()) {
            var found = false;
            for (Constraint fact1 : cs1.getFacts()) {
                if (fact1.entails(fact2)) {
                    found = true;
                }
            }
            if (!found) {
                entails = false;
            }
        }
        return entails;
    }

    /**
     * Returns the concatenation of the two given arrays.
     */
    private static <T> T[] concat(T[] fst, T[] snd) {
        T[] result = Arrays.copyOf(fst, fst.length + snd.length);
        System.arraycopy(snd, 0, result, fst.length, snd.length);
        return result;
    }

}

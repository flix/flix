/*
 * Copyright 2020 Andreas Salling Heglingegård
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package flix.runtime.fixpoint.ram.exp.bool;

import flix.runtime.fixpoint.ram.exp.relation.RelationExp;
import flix.runtime.fixpoint.ram.term.RamTerm;

import java.io.PrintStream;

public final class TupleInRelBoolExp implements BoolExp {
    private final RamTerm[] terms;
    private final RelationExp exp;

    public TupleInRelBoolExp(RamTerm[] terms, RelationExp exp) {
        if (terms == null)
            throw new IllegalArgumentException("'terms' must be non-null");
        if (exp == null) throw new IllegalArgumentException("'table' must be non-null");
        this.terms = terms;
        this.exp = exp;
    }

    @Override
    public void prettyPrint(PrintStream stream, int indentLevel) {
        stream.print("(");
        for (int i = 0; i < terms.length; i++) {
            terms[i].prettyPrint(stream);
            if (i < terms.length - 1) {
                stream.print(", ");
            }
        }
        stream.print(") in ");
        exp.prettyPrint(stream, indentLevel);
    }

    public RamTerm[] getTerms() {
        return terms;
    }

    public RelationExp getExp() {
        return exp;
    }
}

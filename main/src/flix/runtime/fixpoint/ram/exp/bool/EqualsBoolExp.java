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

import flix.runtime.fixpoint.ram.term.RamTerm;

import java.io.PrintStream;

public final class EqualsBoolExp implements BoolExp {
    private final RamTerm term1;
    private final RamTerm term2;

    public EqualsBoolExp(RamTerm term1, RamTerm term2) {
        if (term1 == null) throw new IllegalArgumentException("'term1' must be non-null");
        if (term2 == null) throw new IllegalArgumentException("'term2' must be non-null");
        this.term1 = term1;
        this.term2 = term2;
    }

    @Override
    public void prettyPrint(PrintStream stream, int indentLevel) {
        term1.prettyPrint(stream);
        stream.print(" = ");
        term2.prettyPrint(stream);
    }

    public RamTerm getTerm1() {
        return term1;
    }

    public RamTerm getTerm2() {
        return term2;
    }
}

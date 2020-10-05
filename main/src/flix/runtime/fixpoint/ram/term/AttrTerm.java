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

package flix.runtime.fixpoint.ram.term;

import flix.runtime.fixpoint.ram.RowVariable;

import java.io.PrintStream;
import java.util.Objects;

public final class AttrTerm implements RamTerm {
    private final RowVariable localVar;
    private final int index;

    public AttrTerm(RowVariable localVar, int index) {
        if (localVar == null) {
            throw new IllegalArgumentException("'localVar' must be non-null");
        }
        this.localVar = localVar;
        this.index = index;
    }

    public RowVariable getLocalVar() {
        return localVar;
    }

    public int getIndex() {
        return index;
    }

    @Override
    public void prettyPrint(PrintStream stream) {
        stream.print('$' + localVar.getVarName() +
                "[" + index + ']');
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        AttrTerm attrTerm = (AttrTerm) o;
        return index == attrTerm.index &&
                localVar.equals(attrTerm.localVar);
    }

    @Override
    public int hashCode() {
        return Objects.hash(localVar, index);
    }
}

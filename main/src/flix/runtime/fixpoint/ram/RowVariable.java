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

package flix.runtime.fixpoint.ram;

import java.util.Objects;

public final class RowVariable {
    private final String varName;

    public RowVariable(String varName) {
        if (varName == null) throw new IllegalArgumentException("'varName' must be non-null");
        this.varName = varName;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        RowVariable that = (RowVariable) o;
        return varName.equals(that.varName);
    }

    @Override
    public int hashCode() {
        return Objects.hash(varName);
    }

    public String getVarName() {
        return varName;
    }
}

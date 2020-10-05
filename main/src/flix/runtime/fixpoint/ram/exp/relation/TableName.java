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

package flix.runtime.fixpoint.ram.exp.relation;

import flix.runtime.fixpoint.symbol.PredSym;

import java.io.PrintStream;
import java.util.Objects;

public final class TableName implements RelationExp {
    private final TableVersion version;
    private final PredSym name;

    public TableName(TableVersion version, PredSym name) {
        if (version == null) throw new IllegalArgumentException("'version' must be non-null");
        if (name == null) throw new IllegalArgumentException("'name' must be non-null");
        this.version = version;
        this.name = name;
    }

    public TableVersion getVersion() {
        return version;
    }

    public PredSym getName() {
        return name;
    }

    @Override
    public void prettyPrint(PrintStream stream, int indentLevel) {
        String result = "";
        if (version == TableVersion.DELTA) {
            result += "Δ" + name.getName();
        } else if (version == TableVersion.NEW) {
            result += "Δ" + name.getName() + "'";
        } else {
            result += name.getName();
        }
        stream.print(result);
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        TableName tableName = (TableName) o;
        return version == tableName.version &&
                name.equals(tableName.name);
    }

    @Override
    public int hashCode() {
        return Objects.hash(version, name);
    }
}

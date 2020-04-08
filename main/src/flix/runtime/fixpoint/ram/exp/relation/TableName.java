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

package flix.runtime.fixpoint.ram;

import flix.runtime.fixpoint.ram.exp.relation.RelationExp;
import flix.runtime.fixpoint.symbol.PredSym;

import java.io.PrintStream;

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
    public String toString() {
        String result = "";
        if (version == TableVersion.DELTA) {
            result += "Δ" + name.getName();
        } else if (version == TableVersion.NEW) {
            result += "Δ" + name.getName() + "'";
        } else {
            result += name.getName();
        }
        return result;
    }

    @Override
    public void prettyPrint(PrintStream stream) {
        stream.print(this);
    }
}

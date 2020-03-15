package flix.runtime.fixpoint.ram;

import flix.runtime.fixpoint.ram.exp.relation.RelationExp;
import flix.runtime.fixpoint.symbol.PredSym;

import java.io.PrintStream;

public final class TableName implements RelationExp {
    private final TableVersion classifier;
    private final PredSym name;

    public TableName(TableVersion classifier, PredSym name) {
        this.classifier = classifier;
        this.name = name;
    }

    public TableVersion getClassifier() {
        return classifier;
    }

    public PredSym getName() {
        return name;
    }

    @Override
    public String toString() {
        String result = "";
        if (classifier == TableVersion.DELTA) {
            result += "Δ" + name.getName();
        } else if (classifier == TableVersion.NEW) {
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

package flix.runtime.fixpoint.ram;

import flix.runtime.fixpoint.ram.exp.relation.RelationExp;
import flix.runtime.fixpoint.symbol.PredSym;

import java.io.PrintStream;

public class TableName implements RelationExp {
    private TableClassifier classifier;
    private PredSym name;

    public TableName(TableClassifier classifier, PredSym name) {
        this.classifier = classifier;
        this.name = name;
    }

    public TableClassifier getClassifier() {
        return classifier;
    }

    public PredSym getName() {
        return name;
    }

    @Override
    public String toString() {
        String result = "";
        if (classifier == TableClassifier.DELTA) {
            result += "Δ" + name.getName();
        } else if (classifier == TableClassifier.NEW) {
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

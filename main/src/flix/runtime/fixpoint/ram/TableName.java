package flix.runtime.fixpoint.ram;

import flix.runtime.fixpoint.symbol.PredSym;

public class TableName {
    private RamTableClassifier classifier;
    private PredSym name;

    public TableName(RamTableClassifier classifier, PredSym name) {
        this.classifier = classifier;
        this.name = name;
    }
}

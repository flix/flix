package flix.runtime.fixpoint.ram;

public class AssignStmt implements Stmt {
    private TableName name;
    private RamTableClassifier classifier;
    private RelationExp relationExp;

    public AssignStmt(TableName name, RamTableClassifier classifier, RelationExp relationExp) {
        this.name = name;
        this.classifier = classifier;
        this.relationExp = relationExp;
    }
}

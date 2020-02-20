package flix.runtime.fixpoint.ram;

public class LocalVariable {
    private String varName;

    public LocalVariable(String varName) {
        this.varName = varName;
    }

    public String getVarName() {
        return varName;
    }
}

package flix.runtime.fixpoint.ram;

public final class RowVariable {
    private final String varName;

    public RowVariable(String varName) {
        this.varName = varName;
    }

    public String getVarName() {
        return varName;
    }
}

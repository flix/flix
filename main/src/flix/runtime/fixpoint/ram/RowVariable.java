package flix.runtime.fixpoint.ram;

public final class RowVariable {
    private final String varName;

    public RowVariable(String varName) {
        if (varName == null) throw new IllegalArgumentException("'varName' must be non-null");
        this.varName = varName;
    }

    public String getVarName() {
        return varName;
    }
}

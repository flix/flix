package flix.runtime.fixpoint.ram;

import java.util.Objects;

public final class RowVariable {
    private final String varName;

    public RowVariable(String varName) {
        if (varName == null) throw new IllegalArgumentException("'varName' must be non-null");
        this.varName = varName;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        RowVariable that = (RowVariable) o;
        return varName.equals(that.varName);
    }

    @Override
    public int hashCode() {
        return Objects.hash(varName);
    }

    public String getVarName() {
        return varName;
    }
}

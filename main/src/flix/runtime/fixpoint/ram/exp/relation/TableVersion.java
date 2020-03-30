package flix.runtime.fixpoint.ram.exp.relation;

public enum TableVersion {
    NEW, // Delta' or new in litterature. This table holds the values generated on the previous iteration
    DELTA, // This table holds the new values generated in the current iteration
    RESULT // the cumulative result holds the combined values from all iterations
}

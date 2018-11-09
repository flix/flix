package ca.uwaterloo.flix.runtime.solver.api.symbol;

import ca.uwaterloo.flix.runtime.solver.api.Attribute;

public interface RelSym extends PredSym {

    /**
     * Returns the name of the relation symbol.
     */
    String getName();

    /**
     * Returns the attributes of the relation symbol.
     */
    Attribute[] getAttributes();

}

package ca.uwaterloo.flix.runtime.solver.api.symbol;

import ca.uwaterloo.flix.runtime.solver.api.Attribute;
import ca.uwaterloo.flix.runtime.solver.api.LatticeOps;

public interface LatSym extends PredSym {

    /**
     * Returns the name of the lattice symbol.
     */
    String getName();

    /**
     * Returns the keys of the lattice symbol.
     */
    Attribute[] getKeys();

    /**
     * Returns the value of the lattice symbol.
     */
    Attribute getValue();

    /**
     * Returns the associated lattice operations.
     */
    LatticeOps getOps();

}

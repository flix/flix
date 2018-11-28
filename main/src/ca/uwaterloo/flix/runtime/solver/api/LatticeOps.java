package ca.uwaterloo.flix.runtime.solver.api;

import flix.runtime.ProxyObject;

import java.util.function.Function;

public interface LatticeOps {

    ProxyObject bot();

    Function<Object[], ProxyObject> equ();

    Function<Object[], ProxyObject> leq();

    Function<Object[], ProxyObject> lub();

    Function<Object[], ProxyObject> glb();

}

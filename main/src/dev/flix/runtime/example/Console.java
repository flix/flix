package dev.flix.runtime.example;

import dev.flix.runtime.Result;
import dev.flix.runtime.Resumption;
import dev.flix.runtime.Unit;

/**
 * A Java interface for the `Console` effect.
 */
public interface Console {

    /**
     * Signature of the `Console.read` operation.
     */
    public Result read(Unit opArg, Resumption k);

    /**
     * Signature of the `Console.print` handler.
     */
    public Result print(String opArg, Resumption k);

}

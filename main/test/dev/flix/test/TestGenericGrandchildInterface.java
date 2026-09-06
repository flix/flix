package dev.flix.test;

/**
 * A generic interface two levels below TestGenericInterface:
 * TestGenericGrandchildInterface -> TestGenericChildInterface -> TestGenericInterface.
 *
 * Used to test that a type parameter is traced through an *intermediate* supertype,
 * i.e. that `testMethod`'s `T1` resolves to `U` rather than being erased to Object.
 */
public interface TestGenericGrandchildInterface<U> extends TestGenericChildInterface<U> {
    U identity(U x);
}

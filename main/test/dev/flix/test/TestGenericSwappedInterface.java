package dev.flix.test;

/**
 * A generic interface that reorders its supertype's type parameters:
 * TestGenericSwappedInterface<A, B> extends TestGenericInterface2<B, A>.
 *
 * Since TestGenericInterface2<X, Y> declares `Y apply(X x)`, the inherited method
 * here is `A apply(B b)`. Used to test that type parameter *positions* are permuted
 * rather than assumed to line up.
 */
public interface TestGenericSwappedInterface<A, B> extends TestGenericInterface2<B, A> {
}

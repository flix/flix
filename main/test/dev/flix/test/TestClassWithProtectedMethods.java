package dev.flix.test;

/**
 * An abstract class with protected methods (one abstract, one concrete).
 *
 * Used to test that a NewObject anonymous subclass can override protected methods. These methods
 * are not reported by Class.getMethods (which only returns public members), so before the fix for
 * issue #11415 the compiler rejected overriding them with "method not found in superclass".
 */
public abstract class TestClassWithProtectedMethods {

    public TestClassWithProtectedMethods() {
    }

    /** A protected abstract method that a subclass MUST implement. */
    protected abstract int protectedAbstractMethod(int x);

    /** A protected concrete method that a subclass MAY override. */
    protected String protectedConcreteMethod(String y) {
        return "base:" + y;
    }

    /** Public entry point so the overridden protected abstract method can be observed. */
    public int callProtectedAbstract(int x) {
        return protectedAbstractMethod(x);
    }

    /** Public entry point so the overridden protected concrete method can be observed. */
    public String callProtectedConcrete(String y) {
        return protectedConcreteMethod(y);
    }
}

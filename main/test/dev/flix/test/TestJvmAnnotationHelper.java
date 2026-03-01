package dev.flix.test;

import java.lang.reflect.Method;

/**
 * A helper class for testing JVM annotations on anonymous class methods.
 */
public class TestJvmAnnotationHelper {

    /**
     * Checks whether the named method on the given object has the TestJvmAnnotation annotation.
     */
    public static boolean hasTestJvmAnnotation(Object obj, String methodName) {
        try {
            for (Method m : obj.getClass().getMethods()) {
                if (m.getName().equals(methodName)) {
                    return m.isAnnotationPresent(TestJvmAnnotation.class);
                }
            }
            return false;
        } catch (Exception e) {
            return false;
        }
    }

    /**
     * Checks whether the named method on the given object has the TestJvmAnnotationClassRetention annotation.
     * Since it has CLASS retention, this should always return false at runtime.
     */
    public static boolean hasTestJvmAnnotationClassRetention(Object obj, String methodName) {
        try {
            for (Method m : obj.getClass().getMethods()) {
                if (m.getName().equals(methodName)) {
                    return m.isAnnotationPresent(TestJvmAnnotationClassRetention.class);
                }
            }
            return false;
        } catch (Exception e) {
            return false;
        }
    }
}

package dev.flix.test;

import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;

/**
 * A test annotation with RUNTIME retention for verifying JVM annotation emission.
 */
@Retention(RetentionPolicy.RUNTIME)
public @interface TestJvmAnnotation {
}

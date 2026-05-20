package dev.flix.test;

import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;

/**
 * A test annotation with CLASS retention (not visible at runtime).
 */
@Retention(RetentionPolicy.CLASS)
public @interface TestJvmAnnotationClassRetention {
}

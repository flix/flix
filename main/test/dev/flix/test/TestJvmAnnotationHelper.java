/*
 * Copyright 2025 Stephen Tetley
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
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

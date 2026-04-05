/*
 * Copyright 2025 Magnus Madsen
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
package ca.uwaterloo.flix.util;

import java.lang.classfile.ClassFile;
import java.lang.classfile.ClassModel;

/**
 * A thin Java helper that wraps {@code java.lang.classfile} API calls.
 * <p>
 * Scala 2.13 cannot resolve the sealed interface hierarchy of
 * {@link ClassModel} (introduced in JDK 24), causing "illegal cyclic reference"
 * errors. By calling the API from Java and returning {@link Object}, we avoid
 * exposing the sealed type to the Scala compiler.
 */
public final class JrtClassFileHelper {

    private JrtClassFileHelper() {}

    /**
     * Parses class file bytes into a {@link ClassModel}, returned as {@link Object}
     * to avoid Scala 2.13 sealed-interface issues.
     */
    public static Object parse(byte[] bytes) {
        return ClassFile.of().parse(bytes);
    }

}

/*
 * Copyright 2020 Stephen Tetley
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

package flix.runtime.library;

import java.math.BigInteger;

/**
 * A wrapper class for reading and showing primitive types that provides concrete, non-overloaded methods.
 *
 * Flix can resolve overloading for Java's `parseInt` family of functions, so these do not
 * need wrappers, but we need to supply a non-overloaded "reader" for BigInteger.
 *
 * Note - This is considered an interim solution for reading and showing primitive types.
 */
public class InterimReadShow {

    public static String byteToString(byte i) {
        return Byte.toString(i);
    }

    public static String shortToString(short i) {
        return Short.toString(i);
    }

    public static String intToString(int i) {
        return Integer.toString(i);
    }

    public static String longToString(long i) {
        return Long.toString(i);
    }

    public static String bigIntegerToString(BigInteger i) {
        return i.toString();
    }

    public static String floatToString(float d) {
        return Float.toString(d);
    }

    public static String doubleToString(double d) {
        return Double.toString(d);
    }

    public static BigInteger bigIntegerFromString(String s) throws Exception {
        return new BigInteger(s.trim());
    }

}
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
 * Java's standard library has a odd quirk where the float parsing functions `Float.parseFloat` and
 * `Double.parseDouble` trims whitespace in the input but the corresponding integer parsing functions
 * don't trim and instead throw an exception if they encounter whitespace.
 *
 * So Flix can provide a consistent API (always trim) we trim whitespace for the integer parsers here.
 *
 * Note - This module is considered an interim solution for reading and showing primitive types.
 */
public class InterimReadShow {

    public static String byteToString(byte i) {
        return Byte.toString(i);
    }

    public static byte byteFromString(String s) throws Exception {
        return Byte.parseByte(s.strip());
    }

    public static String shortToString(short i) {
        return Short.toString(i);
    }

    public static short shortFromString(String s) throws Exception {
        return Short.parseShort(s.strip());
    }

    public static String intToString(int i) {
        return Integer.toString(i);
    }

    public static int intFromString(String s) throws Exception {
        return Integer.parseInt(s.strip());
    }

    public static String longToString(long i) {
        return Long.toString(i);
    }

    public static long longFromString(String s) throws Exception {
        return Long.parseLong(s.strip());
    }

    public static String bigIntegerToString(BigInteger i) {
        return i.toString();
    }

    public static BigInteger bigIntegerFromString(String s) throws Exception {
        return new BigInteger(s.strip());
    }

    public static String floatToString(float d) {
        return Float.toString(d);
    }

    public static String doubleToString(double d) {
        return Double.toString(d);
    }


}
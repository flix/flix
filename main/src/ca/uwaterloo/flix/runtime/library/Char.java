/*
 * Copyright 2017 Magnus Madsen
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

package ca.uwaterloo.flix.runtime.library;

public class Char {

    public static boolean isAscii(char c) {
        return c <= 127;
    }

    public static boolean isLetter(char c) {
        return java.lang.Character.isLetter(c);
    }

    public static boolean isDigit(char c) {
        return java.lang.Character.isDigit(c);
    }

    public static boolean isOctDigit(char c) {
        return '0' <= c && c <= '7';
    }

    public static boolean isHexDigit(char c) {
        return ('0' <= c && c <= '7') || ('a' <= c && c <= 'f') || ('A' <= c && c <= 'F');
    }

    public static boolean isLowerCase(char c) {
        return java.lang.Character.isLowerCase(c);
    }

    public static boolean isUpperCase(char c) {
        return java.lang.Character.isUpperCase(c);
    }

    public static boolean isWhiteSpace(char c) {
        return java.lang.Character.isWhitespace(c);
    }

    public static char toLowerCase(char c) {
        return java.lang.Character.toLowerCase(c);
    }

    public static char toUpperCase(char c) {
        return java.lang.Character.toUpperCase(c);
    }

    public static String toString(char c) {
        return String.valueOf(c);
    }

}

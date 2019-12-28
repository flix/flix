/*
 * Copyright 2019 Stephen Tetley
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

import java.lang.StringBuilder;
import java.math.BigInteger;

/**
 * Wrap Java's StringBuilder, provide a small, monomorphic API.
 *
 * Note - Java's StringBuilder is imperative, appending elements
 * mutates the original StringBuilder.
 */
public class MonoStringBuilder {
    private StringBuilder sb = new StringBuilder();

    // Convert to a String
    public String toString() {
        return sb.toString();
    }

    // Append a String
    public void appendString(String s) {
        this.sb.append(s);
        return;
    }

    // Append a Char
    public void appendChar(char c) {
        this.sb.append(c);
        return;
    }

    // Append a byte (int8)
    public void appendByte(byte i) {
        this.sb.append(i);
        return;
    }

    // Append a short (int16)
    public void appendShort(short i) {
        this.sb.append(i);
        return;
    }

    // Append an int (int32)
    public void appendInt(int i) {
        this.sb.append(i);
        return;
    }

    // Append a long (int64)
    public void appendLong(long i) {
        this.sb.append(i);
        return;
    }

    // Append a BigInteger (BigInt)
    public void appendBigInteger(BigInteger i) {
        this.sb.append(i);
        return;
    }

    // Append a float (float32)
    public void appendFloat(float d) {
        this.sb.append(d);
        return;
    }

    // Append a double (float64)
    public void appendDouble(double d) {
        this.sb.append(d);
        return;
    }

    // Append a newline
    public void appendLineSeparator() {
        String s = System.lineSeparator();
        this.sb.append(s);
        return;
    }

    // Return the length of the StringBuilder
    public int length() {
        return this.sb.length();
    }

}

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

import java.util.Scanner;

public class Console {

    //
    // Standard Input Stream.
    //

    public static String readLine() {
        Scanner scanner = new Scanner(System.in);
        return scanner.nextLine();
    }

    //
    // Standard Output Stream.
    //

    public static void printStdOut(String s) {
        System.out.print(s);
    }

    public static void printLineStdOut(String s) {
        System.out.println(s);
    }

    public static void newLineStdOut() {
        System.out.println();
    }


    public static void flushStdOut() {
        System.out.flush();
    }

    //
    // Standard Error Stream.
    //

    public static void printStdErr(String s) {
        System.err.print(s);
    }

    public static void printLineStdErr(String s) {
        System.err.println(s);
    }

    public static void newLineStdErr() {
        System.err.println();
    }


    public static void flushStdErr() {
        System.err.flush();
    }

}

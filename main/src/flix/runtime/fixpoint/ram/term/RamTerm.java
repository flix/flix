/*
 * Copyright 2020 Andreas Salling Heglingegård
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package flix.runtime.fixpoint.ram.term;

import java.io.PrintStream;

/**
 * This is an Interface for terms used in our RAM language
 */
public interface RamTerm {
    /**
     * A function to print the statements as a program
     *
     * @param stream The stram to print to
     */
    void prettyPrint(PrintStream stream);
}

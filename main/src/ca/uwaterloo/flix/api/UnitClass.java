/*
 * Copyright 2017 Ramin Zarifi
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

package ca.uwaterloo.flix.api;

/**
 * UnitClass which is used to represent `Unit`
 */
public class UnitClass {
    /**
     * Instance of `UnitClass`
     */
    private static UnitClass INSTANCE = new UnitClass();

    /**
     * Getter of `INSTANCE`
     */
    public static UnitClass getInstance() {
        return INSTANCE;
    }

    /**
     * Overriding toString method to return "()"
     */
    @Override
    public String toString() {
        return "()";
    }

    /**
     * Returns true if `obj` is instance of `UnitClass'
     */
    @Override
    public boolean equals(Object obj) {
        return obj instanceof UnitClass;
    }

    /**
     * Private constructor
     */
    private UnitClass() {}
}
